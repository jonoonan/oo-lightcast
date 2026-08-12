
# Here we build a dataset of graduates for Lightcast. Rick Houston has pulled a 
# list of students from Salesforce. Jenzabar also has degrees conferred. Here, 
# we'll use the Salesforce data as the main reference, add rows from Jenzabar 
# where Salesforce didn't track a graduate, then supplement a few columns of 
# the Salesforce data with Jenzabar data. 
# Columns to supplement for all graduates: birthDate, email1, email2, email#n, 
# phone1, phone2, phone#n, programCode, cip6, (where 'Unknown Degree' or 
# 'Deciding Major') programNameOriginal, programActive, academicUnit 
# (quintiles of class rank based on final GPA), academicSubUnit 
# (firstyear student v transfer), degreeLevelAbbrev



# Library -----------------------------------------------------------------

# library(plyr)
library(tidyverse)
library(DBI)
library(odbc)
library(keyring)
library(openxlsx)

# Connect -----------------------------------------------------------------

tms <- dbConnect(odbc(), 
                 driver = "/opt/homebrew/lib/libmsodbcsql.17.dylib", 
                 server = "adminprodsql.gordon.edu", 
                 port = 1433, 
                 database = "TmsEPrd", 
                 uid = "josephine.noonan",
                 pwd = key_get("ADMINPRODSQL"))
gcw <- dbConnect(odbc(), 
                 driver = "/opt/homebrew/lib/libmsodbcsql.17.dylib", 
                 server = "adminprodsql.gordon.edu", 
                 port = 1433, 
                 database = "GCWarehouse", 
                 uid = "josephine.noonan",
                 pwd = key_get("ADMINPRODSQL"))

# Import ------------------------------------------------------------------

alum_imp <- read.xlsx("Lightcast Alumni Export CR2 8.7.26.xlsx",
                  detectDates = T)
degree_history_tms <- tbl(tms, "DEGREE_HISTORY")
dev_person_tms <- tbl(tms, "DEV_PERSON")
majmin_tms <- tbl(tms, "MAJOR_MINOR_DEF") 
degdef_tms <- tbl(tms, "DEGREE_DEFINITION")
table_detail <- tbl(tms, "TABLE_DETAIL")
universalid_xref <- tbl(gcw, "UniversalID_XREF")
namemaster <- tbl(tms, "NameMaster")
biograph_master <- tbl(tms, "BIOGRAPH_MASTER")
gord_latest_ethnicrace_detail <- tbl(tms, "GORD_Latest_ETHNICRACE_Detail")
gord_addr_all_email_addrs <- tbl(tms, "GORD_ADDR_All_Email_Addrs")
address_history <- tbl(tms, "ADDRESS_HISTORY")

# Prepare the alumni data sets ------------------------------------------------

# These Jenzabar tables will be joined to local tibbles, so collect and trim them
majmin <- majmin_tms |> 
  select(MAJOR_CDE, MAJOR_MINOR_DESC, CIP_CDE, BEGIN_DTE, END_DTE, PROG_TYPE, 
         DEGR_CDE) |> 
  collect() |> 
  mutate_all(.funs = trimws)
degdef <- degdef_tms |> 
  select(DEGREE, DEGREE_DESC) |>
  collect() |> 
  mutate_all(.funs = trimws)
dev_maj <- table_detail |>
  filter(COLUMN_NAME == "major") |>
  mutate(MAJOR_CDE = TABLE_VALUE,
         MAJOR_MINOR_DESC = TABLE_DESC) |>
  select(MAJOR_CDE, MAJOR_MINOR_DESC) |>
  collect() |>
  mutate_all(.funs = trimws)
dev_deg <- table_detail |>
  filter(COLUMN_NAME == "degree") |>
  mutate(DEGREE = TABLE_VALUE,
         DEGREE_DESC = TABLE_DESC) |>
  select(DEGREE, DEGREE_DESC) |>
  collect() |>
  mutate_all(.funs = trimws)

majdeg_j <- left_join(majmin, degdef, join_by(DEGR_CDE == DEGREE)) |>
  mutate(programActive = ifelse(is.na(END_DTE), "Y", "N"),
         programActive = ifelse(is.na(END_DTE) & is.na(DEGR_CDE), NA, programActive),
         programActive = 
           ifelse(DEGR_CDE %in% c("CERT", "AAS", "ABN", "ABT", "AS", "BBD", 
                                  "BD", "BED", "BEN", "BME", "BRE", "BSC", 
                                  "BSE", "BTH", "CER", "DIP", "GBC", "MAT", 
                                  "MBA", "MDV", "MRE", "MS", "PHD", "STB", 
                                  "TBB", "THB", "THD"), "N", programActive)) |>
  rename(programCode = MAJOR_CDE,
         cip6 = CIP_CDE,
         programNameOriginal = MAJOR_MINOR_DESC,
         degreeLevelSpelled = DEGREE_DESC,
         degreeLevelAbbrev = DEGR_CDE) |>
  select(programCode, cip6, programNameOriginal, programActive, 
         degreeLevelSpelled, degreeLevelAbbrev) |>
  group_by(programNameOriginal, degreeLevelSpelled) |>
  slice(1)

devmaj_d <- dev_maj |>
  filter(MAJOR_MINOR_DESC != "Art" | MAJOR_CDE != "ARTE",
         MAJOR_MINOR_DESC != "Biology" | MAJOR_CDE != "BIOA",
         MAJOR_MINOR_DESC != "Business Administration" | MAJOR_CDE != "ADMRS",
         MAJOR_MINOR_DESC != "Computer Science" | MAJOR_CDE != "CPSA",
         MAJOR_MINOR_DESC != "Elementary Art Education" | MAJOR_CDE != "ELAR",
         MAJOR_MINOR_DESC != "Music Education" | !(MAJOR_CDE %in% c("MUEDI", "MUEDV")),
         MAJOR_MINOR_DESC != "Music Performance" | !(MAJOR_CDE %in% c("MUPFI", "MUPFV")),
         MAJOR_MINOR_DESC != "Philosophy" | !(MAJOR_CDE %in% c("PHIL2", "PHL2")),
         MAJOR_MINOR_DESC != "Physics" | MAJOR_CDE != "PHYA",
         MAJOR_MINOR_DESC != "Pre-Thomas Jefferson Univ." | MAJOR_CDE != "TJEF",
         MAJOR_MINOR_DESC != "Recreation, Sport and Wellness" | MAJOR_CDE != "RSPAD",
         MAJOR_MINOR_DESC != "Spanish" | MAJOR_CDE != "SPAN2")


# In the Salesforce data, there are many duplicated rows. 
# For one student, their `campus` is noted as "Bachelor of Arts", but they were 
# enrolled after the Barrington-Gordon merger, so their `campus` should be 
# 'Gordon College'
# Split this into folks who have known majors and graduation dates, folks who 
# have unknown majors regardless of graduation date, and folks who have known 
# majors and unknown graduation dates so that later it will be easier to add 
# what additional information we have from Jenzabar straight in and retain the 
# known items from Salesforce for all these people.

alum_sectioned <- alum_imp |>
  mutate(campus = ifelse(campus == "Bachelor of Arts",
                         "Gordon College",
                         campus)) |>
  distinct() |>
  mutate(
    section = case_when(
      !(programNameOriginal %in% c("Unknown Degree", "Deciding Major")) & 
        degreeLevelSpelled != "Unknown Degree" &
        gradDate != ymd("1900-01-01") ~ "known",
      
      programNameOriginal %in% c("Unknown Degree", "Deciding Major") & 
        degreeLevelSpelled != "Unknown Degree" &
        gradDate != ymd("1900-01-01") ~ "unknown major",
      
      !(programNameOriginal %in% c("Unknown Degree", "Deciding Major")) & 
        degreeLevelSpelled == "Unknown Degree" &
        gradDate != ymd("1900-01-01") ~ "unknown degree",
      
      programNameOriginal %in% c("Unknown Degree", "Deciding Major") & 
        degreeLevelSpelled != "Unknown Degree" &
        gradDate == ymd("1900-01-01") ~ "unknown major and gradDate",
      
      !(programNameOriginal %in% c("Unknown Degree", "Deciding Major")) & 
        degreeLevelSpelled != "Unknown Degree" &
        gradDate == ymd("1900-01-01") ~ "unknown gradDate",
      
      programNameOriginal %in% c("Unknown Degree", "Deciding Major") & 
        degreeLevelSpelled == "Unknown Degree" &
        gradDate == ymd("1900-01-01") ~ "unknown major degree and gradDate"),
    programCode = as.character(programCode),
    programActive = as.character(programActive),
    degreeLevelAbbrev = as.character(degreeLevelAbbrev),
    programNameOriginal = ifelse(programNameOriginal %in% c("Unknown Degree", "Deciding Major"),
                                 NA,
                                 programNameOriginal),
    degreeLevelSpelled = ifelse(degreeLevelSpelled == "Unknown Degree",
                                NA,
                                degreeLevelSpelled))

alum_known <- alum_sectioned |>
  filter(section == "known") |>
  left_join(majdeg_j, join_by(programNameOriginal, degreeLevelSpelled)) |>
  mutate(programCode.x = coalesce(as.character(programCode.x), programCode.y),
         cip6.x = coalesce(as.character(cip6.x), cip6.y),
         programActive.x = coalesce(as.character(programActive.x), programActive.y),
         degreeLevelAbbrev.x = coalesce(as.character(degreeLevelAbbrev.x), degreeLevelAbbrev.y),
         .keep = "unused") |>
  rename(programCode = programCode.x,
         cip6 = cip6.x,
         programActive = programActive.x,
         degreeLevelAbbrev = degreeLevelAbbrev.x) |>
  left_join(dev_deg, join_by(degreeLevelSpelled == DEGREE_DESC)) |>
  mutate(degreeLevelAbbrev = coalesce(degreeLevelAbbrev, DEGREE),
         .keep = "unused") |>
  left_join(devmaj_d, join_by(programNameOriginal == MAJOR_MINOR_DESC)) |>
  mutate(programCode = coalesce(programCode, MAJOR_CDE),
         .keep = "unused")
alum_unknown_maj <- alum_sectioned |>
  filter(section == "unknown major")
alum_unknown_deg <- alum_sectioned |>
  filter(section == "unknown degree")
alum_unknown_maj_year <- alum_sectioned |>
  filter(section == "unknown major and gradDate")
alum_unknown_year <- alum_sectioned |>
  filter(section == "unknown gradDate")
alum_unknown_maj_deg_year <- alum_sectioned |>
  filter(section == "unknown major degree and gradDate")

# seems like all observations from alum_sectioned are represented in these 
# sections. So why are there student ids not showing up in ids?
# alum_sectioned |>
#   anti_join(alum_known) |>
#   anti_join(alum_unknown_maj) |>
#   anti_join(alum_unknown_deg) |>
#   anti_join(alum_unknown_maj_year) |>
#   anti_join(alum_unknown_year) |>
#   anti_join(alum_unknown_maj_deg_year)

# here pull the necessary basic information on all degrees conferred that we 
# have record of in Jenzabar. Eliminate information from people 
# covered in previous (complete) tables
degree_history <- degree_history_tms |>
  pivot_longer(cols = c(MAJOR_1, MAJOR_2, MAJOR_3),
               names_to = "maj_type",
               values_to = "major") |>
  filter(!is.na(DTE_DEGR_CONFERRED),
         !is.na(major)) |>
  select(ID_NUM, major, DTE_DEGR_CONFERRED, CLASS_RANK, CLASS_SIZE) |>
  collect() |>
  mutate_all(.funs = trimws) |>
  mutate(ID_NUM = as.numeric(ID_NUM),
         CLASS_RANK = as.numeric(CLASS_RANK),
         DTE_DEGR_CONFERRED = ymd(DTE_DEGR_CONFERRED)) |>
  anti_join(alum_known, 
            join_by(ID_NUM == studentID, 
                    DTE_DEGR_CONFERRED == gradDate)) |>
  left_join(majmin |> select(MAJOR_CDE, MAJOR_MINOR_DESC, CIP_CDE, BEGIN_DTE, 
                             END_DTE, DEGR_CDE), 
            join_by(major == MAJOR_CDE)) |>
  left_join(degdef, join_by(DEGR_CDE == DEGREE)) |>
  mutate(programActive = ifelse(is.na(END_DTE), "Y", "N"),
         CIP_CDE = as.numeric(CIP_CDE),
         section = "degree_history") |>
  rename(studentID = ID_NUM,
         programCode = major,
         gradDate = DTE_DEGR_CONFERRED,
         programNameOriginal = MAJOR_MINOR_DESC,
         cip6 = CIP_CDE,
         degreeLevelAbbrev = DEGR_CDE,
         degreeLevelSpelled = DEGREE_DESC) |>
  select(studentID, programCode, cip6, programNameOriginal, programActive, 
         degreeLevelSpelled, degreeLevelAbbrev, gradDate, section)
  
## I'm unsure which years indicate that we have conferred a degree to them, 
## so include them all until we hear back from CTS. 
dev_person <- dev_person_tms |>
  filter(!is.na(UNDERGRAD_GRAD_YR) | 
                  !is.na(GRAD_GRAD_YEAR)) |>
  select(ID_NUM, DEGREE_1, DEGREE_2, UNDERGRAD_GRAD_YR,
         GRAD_GRAD_YEAR, MAJOR_1, MAJOR_2) |>
  collect() |>
  mutate_all(.funs = trimws) |>
  mutate(ID_NUM = as.numeric(ID_NUM)) |>
  anti_join(alum_known, join_by(ID_NUM == studentID)) |>
  anti_join(degree_history, join_by(ID_NUM == studentID)) |>
  # only one person has two degree codes, but they only have one undergraduate 
  # major code, so I'll make DEGREE_2 == NA for them to solve my pivot_longer 
  # woes
  mutate(DEGREE_2 = ifelse(ID_NUM == 9005351, NA,  DEGREE_2)) |>
  pivot_longer(cols = c(MAJOR_1, MAJOR_2), 
               names_to = NULL, 
               values_to = "major") |>
  # can't just drop_na on major because that will drop a bunch of observations, 
  # so call distinct to get rid of any duplicate NA values, then group by ID 
  # number and count to see who has multiple rows. Then, you can filter for
  # where !is.na(major) or count == 1 to save the rows where we just don't know 
  # the major
  distinct() |>
  group_by(ID_NUM) |>
  mutate(count = n()) |>
  filter(count == 1 | !is.na(major)) |>
  select(!count) |>
  ungroup() |>
  mutate(
    degree = coalesce(DEGREE_1, DEGREE_2),
    gradDate = ymd(
      paste0(coalesce(UNDERGRAD_GRAD_YR, GRAD_GRAD_YEAR), 
             "-01-01")),
    .keep = "unused") |>
  # these are the majors and degrees specific to DEV_PERSON
  left_join(dev_maj |> rename(programNameOriginal = MAJOR_MINOR_DESC), 
            join_by(major == MAJOR_CDE)) |>
  left_join(dev_deg |> rename(degreeLevelSpelled = DEGREE_DESC), 
            join_by(degree == DEGREE)) |>
  # see what else can be added using the generic jenzabar maj and deg tables
  left_join(majmin |> select(MAJOR_CDE, MAJOR_MINOR_DESC, DEGR_CDE), 
            join_by(major == MAJOR_CDE)) |>
  left_join(degdef, join_by(DEGR_CDE == DEGREE)) |>
  mutate(programNameOriginal = coalesce(programNameOriginal, MAJOR_MINOR_DESC),
         degreeLevelAbbrev = coalesce(degree, DEGR_CDE),
         degreeLevelSpelled = coalesce(degreeLevelSpelled, DEGREE_DESC),
         .keep = "unused") |>
  left_join(majmin |> select(MAJOR_CDE, CIP_CDE, END_DTE), 
            join_by(major == MAJOR_CDE)) |>
  mutate(programActive = case_when(is.na(END_DTE) & !is.na(CIP_CDE) ~ "Y", 
                                   !is.na(END_DTE) & !is.na(CIP_CDE) ~ "N"),
         programActive = case_when(degreeLevelAbbrev %in% c("CERT", "AAS", "ABN", "ABT", "AS", "BBD", 
                                                            "BD", "BED", "BEN", "BME", "BRE", "BSC", 
                                                            "BSE", "BTH", "CER", "DIP", "GBC", "MAT", 
                                                            "MBA", "MDV", "MRE", "MS", "PHD", "STB", 
                                                            "TBB", "THB", "THD") ~ "N",
                                   .default = programActive),
         CIP_CDE = as.numeric(CIP_CDE),
         section = "dev_person") |>
  rename(studentID = ID_NUM,
         programCode = major,
         cip6 = CIP_CDE) |>
  select(studentID, programCode, cip6, programNameOriginal, programActive, 
         degreeLevelSpelled, degreeLevelAbbrev, gradDate, section)

# It will be useful below to have a list of all alumni student ids
ids <- unique(c(pull(alum_known, studentID),
                pull(alum_unknown_maj, studentID),
                pull(alum_unknown_deg, studentID),
                pull(alum_unknown_maj_year, studentID),
                pull(alum_unknown_year, studentID),
                pull(alum_unknown_maj_deg_year, studentID),
                pull(degree_history, studentID),
                pull(dev_person, studentID)))


## collect information for the rest of the columns -------------------------

# uniqueID - salesforceid in universalid_xref doesn't match how uniqueID looks in Rick's data
# uniqueID <- universalid_xref |>
#   filter(J1ID %in% ids) |>
#   select(J1ID, SalesForceID) |>
#   collect() |>
#   drop_na(SalesForceID)

# firstName, preferredName, middleName, lastName1, lastName2, lastName#n
names <- namemaster |>
  filter(ID_NUM %in% ids) |>
  select(ID_NUM, FIRST_NAME, PREFERRED_NAME, MIDDLE_NAME, LAST_NAME, BIRTH_NAME) |>
  collect() |>
  rename(studentID = ID_NUM,
         firstName = FIRST_NAME,
         preferredName = PREFERRED_NAME,
         middleName = MIDDLE_NAME,
         lastName1 = LAST_NAME,
         lastName2 = BIRTH_NAME) |>
  mutate_all(.funs = trimws) |>
  mutate(
    studentID = as.numeric(studentID),
    firstName = ifelse(lastName2 == "María Paulina De los Ríos", "Maria", firstName),
    middleName = ifelse(lastName1 == "María Paulina De los Ríos", "Paulina", middleName),
    lastName2 = case_when(
      lastName2 %in% c("Adrianna Sturgis-Massa", "Afton M Dean", 
                       "Amy Elyse O'Dowd", "Anne Elizabeth Benedict", 
                       "Annemaria Theodora van Woerden", 
                       "Bruce Robert Nelson Jr.", "Emily Sydow", "Eunbee Noh",
                       "Gabriella Victoria Mooers", 
                       "Jaqueline Gorgonio de Almeida", "Jeff Wilson", 
                       "Jennifer Lynn", "Jillian Sarah Fudge", "Jiwon Lee",
                       "Joseph Stephen Gruntkosky", "Kate Clinch", 
                       "Katherine Eileen Smith", "Kathleen Dwyer", 
                       "Keith Maxwell Enos", "Kelsey Megan Giovannacci", 
                       "Khristopher Silveria", "Krystal Jane Walsh", 
                       "Kylie Cointot", "Melissa McCarthy", "Nancy Kreusser", 
                       "Nicolette Bricknell", "Richard (first)", 
                       "Rosalie Ellen O'Melia", "Sofia Zgany", "Suzannah Hofford", "Taylor Catalano", "Timothy Joseph Lane", "Troy Gabriele", "Vittoria Monte", "Yexin Li") ~ NA,
      lastName2 == "(Fox, Debra A.)" ~ "Fox",
      lastName2 == "06/29/1996/ Gray" ~ "Gray",
      lastName2 == "Alessia Indiciani" ~ "Indiciani",
      lastName2 == "Amanda Bagatelli Schmildes" ~ "Bagatelli Schmildes",
      lastName2 == "Amanda Marie Garnier" ~ "Garnier",
      lastName2 == "Amanda Mello" ~ "Mello",
      lastName2 == "Amie Johnson" ~ "Johnson",
      lastName2 == "Amy Bullock" ~ "Bullock",
      lastName2 == "Balu Lavanya" ~ "Balu",
      lastName2 == "Bergen Hall" ~ "Hall",
      lastName2 == "Beth Ann Brain" ~ "Brain",
      lastName2 == "Caitlin Barry" ~ "Barry",
      lastName2 == "Chrystal Goldberg" ~ "Goldberg",
      lastName2 == "Corrie Granitsas" ~ "Granitsas",
      lastName2 == "Debra Davis" ~ "Davis",
      lastName2 == "Elba Dominguez Covarrubias" ~ "Dominguez Covarrubias",
      lastName2 == "Elena C. Vasilakis" ~ "Vasilakis",
      lastName2 == "Elizabeth Ann Bachman" ~ "Bachman",
      lastName2 == "Emily Jane Gottstein" ~ "Gottstein",
      lastName2 == "Emily Judd" ~ "Judd",
      lastName2 == "Emily Margaret Morse" ~ "Morse",
      lastName2 == "Gillian Corkery" ~ "Corkery",
      lastName2 == "Jean Carpenter" ~ "Carpenter",
      lastName2 == "Jennifer L Snow" ~ "Snow",
      lastName2 == "Jessica Marie Hanson" ~ "Hanson",
      lastName2 == "Joan Davis" ~ "Davis",
      lastName2 == "Joanne Bebout" ~ "Bebout",
      lastName2 == "Johanna Lynn Cronk" ~ "Cronk",
      lastName2 == "Julie Freitas" ~ "Freitas",
      lastName2 == "Kathryn Rebecca Wood" ~ "Wood",
      lastName2 == "Kayley Miskis" ~ "Miskis",
      lastName2 == "Kelly McCarthy" ~ "McCarthy",
      lastName2 == "Kelsey Allen" ~ "Allen",
      lastName2 == "Kimberlee Markham" ~ "Markham",
      lastName2 == "Kimberly Hotalen" ~ "Hotalen",
      lastName2 == "Kimberly Nguyen" ~ "Nguyen",
      lastName2 == "Kristen Sarver" ~ "Sarver",
      lastName2 == "Lauren Amy Khoyi" ~ "Khoyi",
      lastName2 == "Leesa Marie Witt" ~ "Witt",
      lastName2 == "Marcia Steuernagel" ~ "Steuernagel",
      lastName2 == "María Paulina De los Ríos" ~ "De los Ríos",
      lastName2 == "Matthew Paul Nicewinter" ~ "Nicewinter",
      lastName2 == "Melanie LaPrade" ~ "LaPrade",
      lastName2 == "Michele Lee Botts" ~ "Botts",
      lastName2 == "Morgan Ruckel" ~ "Ruckel",
      lastName2 == "Nancy Smith" ~ "Smith",
      lastName2 == "Pamela Jean Melvin" ~ "Melvin",
      lastName2 == "Rachel Sierra Kanter" ~ "Kanter",
      lastName2 == "Rebecca J Hobson" ~ "Hobson",
      lastName2 == "Rebecca Ruiz" ~ "Ruiz",
      lastName2 == "Sandra A. McShane" ~ "McShane",
      lastName2 == "Sheri Tolman" ~ "Tolman",
      lastName2 == "Sylvia Helena das Neves" ~ "das Neves",
      lastName2 == "Thaissa de Souza dos Santos" ~ "de Souza dos Santos",
      lastName2 == "Tiffany Deraedt" ~ "Deraedt",
      lastName2 == "Virginia Souza Campos" ~ "Souza Campos",
      lastName2 == "Whitney Schwartz" ~ "Schwartz",
      lastName2 == "Kavin, Laura" ~ "Kavin",
      .default = lastName2))

# gender, birthdate
# gen_bir <- biograph_master |>
#   filter(ID_NUM %in% ids) |>
#   select(ID_NUM, GENDER, BIRTH_DTE) |>
#   collect() |>
#   rename(gender = GENDER,
#          birthDate = BIRTH_DTE)

# race, ethnicity
raceethnicity <- gord_latest_ethnicrace_detail |>
  collect() 
ethrace <- raceethnicity |>
  filter(id_num %in% ids) |>
  mutate(race = case_when(White+Pacific+AfrAmer+Asian+AmerIndian == 0 ~ "Unknown Race",
                          White+Pacific+AfrAmer+Asian+AmerIndian > 1 ~ "Two or more races",
                          White == 1 ~ "White",
                          Pacific == 1 ~ "Native Hawaiian or Other Pacific Islander",
                          AfrAmer == 1 ~ "Black or African American",
                          Asian == 1 ~ "Asian",
                          AmerIndian == 1 ~ "American Indian or Alaska Native"),
         ETHNIC_RPT_DESC = ifelse(ETHNIC_RPT_DESC == "Unreported", "Unknown Ethnicity", ETHNIC_RPT_DESC)) |>
  rename(studentID = id_num, 
         ethnicity = ETHNIC_RPT_DESC) |>
  select(studentID, ethnicity, race) |>
  # 10004506 showed up twice, once with an unknown ethnicity, and once with a known ethnicity
  filter(studentID != 10004506 | ethnicity != "Unknown Ethnicity")

# emails
gaaea <- gord_addr_all_email_addrs |>
  collect()
emails <- gaaea |>
  filter(ID_NUM %in% ids) |>
  select(ID_NUM, Email, Alternate_Email, Personal_Email) |>
  rename(studentID = ID_NUM,
         email1 = Email,
         email2 = Alternate_Email,
         `email#n` = Personal_Email)

# phones addresses
ah <- address_history |>
  filter(ADDR_CDE == "*LHP") |>
  collect()
phone_address <- ah |>
  filter(ID_NUM %in% ids) |>
  group_by(ID_NUM) |>
  filter(ARCHIVE_JOB_TIM == max(ARCHIVE_JOB_TIM)) |> 
  mutate(
    addressLineTwo1 = 
      case_when(
        !is.na(ADDR_LINE_2) & !is.na(ADDR_LINE_3) ~ 
          paste(str_trim(ADDR_LINE_2),  
                str_trim(ADDR_LINE_3),  
                sep = ", "),
        !is.na(ADDR_LINE_2) & is.na(ADDR_LINE_3) ~  ADDR_LINE_2,
        is.na(ADDR_LINE_2) & !is.na(ADDR_LINE_3) ~  ADDR_LINE_3,
        ADDR_LINE_2 == "" & ADDR_LINE_3 == "" ~ NA)) |>
  select(ID_NUM, PHONE, ADDR_LINE_1, addressLineTwo1, CITY, STATE, ZIP) |>
  rename(studentID = ID_NUM,
         phone1 = PHONE,
         addressLineOne1 = ADDR_LINE_1,
         city1 = CITY,
         state1 = STATE,
         zipCode1 = ZIP)



## add unknown info to existing Salesforce rows ----------------------------
# Where Jenzabar can shore up Salesforce's unknown major, degree, and gradDate 
# information, add those bad boys in

# no additional major information
# left_join(alum_unknown_maj, dev_person, join_by(studentID)) |> filter(!is.na(programNameOriginal.y))
# left_join(alum_unknown_maj, degree_history, join_by(studentID)) |> filter(!is.na(programNameOriginal.y))
alum_unknown_maj_w_deg <- alum_unknown_maj |>
  left_join(degdef |> filter(DEGREE != "CERT"), join_by(degreeLevelSpelled == DEGREE_DESC)) |>
  mutate(degreeLevelAbbrev = coalesce(degreeLevelAbbrev, DEGREE),
         .keep = "unused") |>
  left_join(dev_deg, join_by(degreeLevelSpelled == DEGREE_DESC)) |>
  mutate(degreeLevelAbbrev = coalesce(degreeLevelAbbrev, DEGREE),
         degreeLevelAbbrev = case_when(degreeLevelSpelled == "Associate of Applied Science" ~ "AAS",
                                       degreeLevelSpelled == "Bachelor's Degree" ~ "BAD",
                                       .default = degreeLevelAbbrev),
         .keep = "unused")

# alum_unknown_deg |> left_join(degree_history, join_by(studentID, programNameOriginal)) |> filter(!is.na(degreeLevelSpelled.y))
alum_known_deg <- left_join(
  alum_unknown_deg, 
  dev_person |> 
    select(studentID, programCode, cip6, programNameOriginal, programActive, 
           degreeLevelSpelled, degreeLevelAbbrev), 
  join_by(studentID, programNameOriginal)) |> 
  mutate(programCode = coalesce(programCode.x, programCode.y),
         cip6 = coalesce(cip6.x, cip6.y),
         programActive = coalesce(programActive.x, programActive.y),
         degreeLevelSpelled = coalesce(degreeLevelSpelled.x, degreeLevelSpelled.y),
         degreeLevelAbbrev = coalesce(degreeLevelAbbrev.x, degreeLevelAbbrev.y),
         .keep = "unused") |>
  replace_na(list(degreeLevelSpelled = "Unknown Degree")) |>
  select(uniqueID, studentID, firstName, preferredName, middleName, lastName1, 
         lastName2, `lastName#n`, gender, race, ethnicity, birthDate, email1, 
         email2, `email#n`, phone1, phone2, `phone#n`, addressLineOne1, 
         addressLineTwo1, city1, state1, zipCode1, addressLineOne2, 
         addressLineTwo2, city2, state2, zipCode2, `addressLineOne#n`, 
         `addressLineTwo#n`, `city#n`, `state#n`, `zipCode#n`, campus, 
         programCode, cip6, programNameOriginal, programActive, academicUnit, 
         academicSubUnit, degreeLevelSpelled, degreeLevelAbbrev, gradDate, 
         section)

# left_join(alum_unknown_maj_year, degree_history, join_by(studentID)) |> filter(!is.na(programNameOriginal.y))
# left_join(alum_unknown_maj_year, dev_person, join_by(studentID)) |> filter(!is.na(programNameOriginal.y))

# cannot verify that anyone else with a 1900-01-01 grad date actually graduated 
# from Gordon, so filter them out here
alum_known_year <- left_join(alum_unknown_year, 
          degree_history |> select(studentID, degreeLevelSpelled, gradDate), 
          join_by(studentID, degreeLevelSpelled)) |>
  mutate(gradDate = coalesce(gradDate.y, gradDate.x),
         .keep = "unused") |>
  filter(gradDate != ymd("1900-01-01")) |>
  left_join(majdeg_j, join_by(programNameOriginal, degreeLevelSpelled)) |>
  mutate(programCode.x = coalesce(as.character(programCode.x), programCode.y),
         cip6.x = coalesce(as.character(cip6.x), cip6.y),
         programActive.x = coalesce(as.character(programActive.x), programActive.y),
         degreeLevelAbbrev.x = coalesce(as.character(degreeLevelAbbrev.x), degreeLevelAbbrev.y),
         .keep = "unused") |>
  rename(programCode = programCode.x,
         cip6 = cip6.x,
         programActive = programActive.x,
         degreeLevelAbbrev = degreeLevelAbbrev.x)
  

# left_join(alum_unknown_maj_deg_year, degree_history, join_by(studentID)) |> filter(!is.na(programNameOriginal.y))
# left_join(alum_unknown_maj_deg_year, dev_person, join_by(studentID)) |> filter(!is.na(programNameOriginal.y))


# Bind together all alum/deg/dev tibbles ----------------------------------
# get one tibble going to which I can add the missing columns
# anti_join where info was supplemented into alum sections
all_alum <- bind_rows(alum_known |> mutate(cip6 = as.numeric(cip6)), 
          alum_unknown_maj_w_deg,
          alum_known_deg,
          alum_known_year |> mutate(cip6 = as.numeric(cip6)),
          degree_history |> 
            anti_join(alum_known_year, 
                      join_by(studentID, degreeLevelSpelled)),
          dev_person |> 
            anti_join(alum_known_deg, 
                      join_by(studentID, programNameOriginal)))


# Clean existing columns and add in final columns from Jenzabar -----------

final_tibble <- all_alum |>
  left_join(names, join_by(studentID)) |>
  mutate(firstName.x = coalesce(firstName.x, firstName.y),
         preferredName.x = coalesce(preferredName.x, preferredName.y),
         middleName.x = coalesce(middleName.x, middleName.y),
         lastName1.x = coalesce(lastName1.x, lastName1.y),
         lastName2.x = coalesce(lastName2.x, lastName2.y),
         .keep = "unused") |>
  left_join(gen_bir, join_by(studentID == ID_NUM)) |>
  mutate(gender.x = coalesce(gender.x, gender.y),
         birthDate.x = coalesce(birthDate.x, birthDate.y),
         .keep = "unused") |>
  left_join(ethrace, join_by(studentID)) |>
  mutate(ethnicity.x = coalesce(as.character(ethnicity.x), ethnicity.y),
         race.x = coalesce(as.character(race.x), race.y),
         .keep = "unused") |>
  left_join(emails, join_by(studentID)) |>
  mutate(email1.x = coalesce(email1.x, email1.y),
         email2.x = coalesce(email2.x, email2.y),
         `email#n.x` = coalesce(`email#n.x`, `email#n.y`),
         .keep = "unused") |>
  left_join(phone_address, join_by(studentID)) |>
  mutate(phone1.x = coalesce(phone1.x, as.character(phone1.y)),
         addressLineOne1.x = coalesce(addressLineOne1.x, addressLineOne1.y),
         addressLineTwo1.x = coalesce(addressLineTwo1.x, addressLineTwo1.y),
         city1.x = coalesce(city1.x, city1.y),
         state1.x = coalesce(state1.x, state1.y),
         zipCode1.x = coalesce(zipCode1.x, zipCode1.y),
         campus = ifelse(gradDate >= ymd("1991-05-19"),
                         "Gordon College",
                         campus),
         .keep = "unused") 

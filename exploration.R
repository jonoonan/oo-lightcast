library(tidyverse)
library(DBI)
library(odbc)
library(keyring)
library(openxlsx)

alum <- read.xlsx("Lightcast Alumni Export CR2 8.7.26.xlsx",
                  detectDates = T) |>
  mutate(campus = ifelse(campus == "Bachelor of Arts",
                         "Gordon College",
                         campus)) |>
  # group_by(across(everything())) |>
  # mutate(count = n()) |> filter(count>1)
  distinct()

tms <- dbConnect(odbc(), driver = "/opt/homebrew/lib/libmsodbcsql.17.dylib", 
                 server = "adminprodsql.gordon.edu", port = 1433, database = "TmsEPrd", 
                 uid = "josephine.noonan", pwd = key_get("ADMINPRODSQL"))

majmin <- tbl(tms, "MAJOR_MINOR_DEF") |> collect() |> mutate_all(.funs = trimws)
degdef <- tbl(tms, "DEGREE_DEFINITION") |> collect() |> mutate_all(.funs = trimws)
ids <- pull(alum, studentID)
birthdate <- tbl(tms, "BIOGRAPH_MASTER") |>
  filter(ID_NUM %in% ids) |>
  select(ID_NUM, BIRTH_DTE) |>
  collect()
# deciding which email columns to use
# tbl(tms, "GORD_ADDR_All_Email_Addrs") |> 
#   collect() |>
#   summarise(alt = sum(!is.na(Alternate_Email))/n(),
#             bus = sum(!is.na(Business_Email))/n(),
#             ema = sum(!is.na(Email))/n(),
#             fow = sum(!is.na(Forwarding_Email))/n(),
#             gor = sum(!is.na(Gordon_Email))/n(),
#             old = sum(!is.na(Old_Email))/n(),
#             per = sum(!is.na(Personal_Email))/n())
email <- tbl(tms, "GORD_ADDR_All_Email_Addrs") |>
  filter(ID_NUM %in% ids) |>
  select(ID_NUM, Email, Alternate_Email, Personal_Email) |>
  collect()
# phone <- tbl(tms, "PhoneMaster") |>
#   filter(ID_NUM %in% ids) |>
#   select(ID_NUM, FormattedPhoneNumber) |>
#   collect()
degree_history <- tbl(tms, "DEGREE_HISTORY") |>
  # filter(ID_NUM %in% ids) |>
  collect() 
dev_person <- tbl(tms, "DEV_PERSON") |>
  # filter(ID_NUM %in% ids) |>
  collect() |>
  mutate_all(.funs = trimws)
dev_person |> 
  distinct(DEGREE_1) |>
  left_join(degdef, join_by(DEGREE_1 == DEGREE))
dev_person |> 
  distinct(MAJOR_1) |>
  left_join(majmin, join_by(MAJOR_1 == MAJOR_CDE)) |>
  print(n=95)

left_join(alum, degree_history |>
            select(ID_NUM, DTE_DEGR_CONFERRED, MAJOR_1), 
          join_by(studentID == ID_NUM, gradDate == DTE_DEGR_CONFERRED)) |>
  mutate(programCode = MAJOR_1) |>
  distinct(programCode, programNameOriginal)

# degree_history |>
#   drop_na(DTE_DEGR_CONFERRED) |>
#   group_by(ID_NUM, DTE_DEGR_CONFERRED) |>
#   mutate(count = n()) |>
#   filter(count > 1) |>
#   ungroup() |>
#   arrange(ID_NUM, CUR_DEGREE) |>
#   View()

pivoted_degr_hist <- degree_history |>
  pivot_longer(cols = c(MAJOR_1, MAJOR_2, MAJOR_3, MAJOR_4),
               names_to = NULL,
               values_to = "major") |>
  drop_na(major) |> 
  mutate(major = trimws(major),
         DTE_DEGR_CONFERRED = ymd(DTE_DEGR_CONFERRED)) |>
  select(ID_NUM, major, DIV_CDE, DEGR_CDE, DTE_DEGR_CONFERRED) |>
  left_join(majmin, join_by(major == MAJOR_CDE)) |>
  select(ID_NUM, major, MAJOR_MINOR_DESC, DTE_DEGR_CONFERRED, CIP_CDE)

with_degree_history <- left_join(alum, 
          pivoted_degr_hist,
          join_by(studentID == ID_NUM, programNameOriginal == MAJOR_MINOR_DESC, gradDate == DTE_DEGR_CONFERRED)) |>
  mutate(cip6 = CIP_CDE,
         programCode = major,
         .keep = "unused") 

majdeg <- left_join(majmin, degdef, join_by(DEGR_CDE == DEGREE)) |>
  filter(MAJOR_MINOR_DESC != "Business Administration" | MAJOR_CDE != "ADMRS",
         MAJOR_MINOR_DESC != "Physics" | !(MAJOR_CDE %in% c("PHYA", "PASP")),
         MAJOR_MINOR_DESC != "Sociology" | MAJOR_CDE != "SOCL",
         MAJOR_MINOR_DESC != "Music Education" | !(MAJOR_CDE %in% c("MUEDV", "MUEDI")),
         MAJOR_MINOR_DESC != "Philosophy" | MAJOR_CDE != "PHL2",
         MAJOR_MINOR_DESC != "Music Performance" | !(MAJOR_CDE %in% c("MUPFV", "MUPFI")),
         MAJOR_MINOR_DESC != "Biblical Studies" | MAJOR_CDE != "CMBI",
         MAJOR_MINOR_DESC != "Spanish" | MAJOR_CDE != "SPAN2",
         MAJOR_MINOR_DESC != "Art" | MAJOR_CDE != "ARTE",
         MAJOR_MINOR_DESC != "Health Professions" | 
           !(MAJOR_CDE %in% c("XXPM", "HPNR", "HPOP", "HPOT", "HPPA", "HPPH",
                              "HPPM", "HPPT", "HPVT"))) |>
  mutate(g_u = ifelse(PROG_TYPE == "UG", "U", "G"))
# ART v ARTE

from_maj_and_deg <- with_degree_history |>
  filter(is.na(programCode)) |> 
  left_join(majdeg |> select(MAJOR_CDE, CIP_CDE, MAJOR_MINOR_DESC, DEGREE_DESC), 
            join_by(programNameOriginal == MAJOR_MINOR_DESC, degreeLevelSpelled == DEGREE_DESC)) |>
  mutate(programCode = MAJOR_CDE,
         cip6 = CIP_CDE,
         .keep = "unused")

from_deghist_majdeg <- rbind(with_degree_history |> drop_na(programCode),
      from_maj_and_deg)

from_majba <- from_deghist_majdeg |>
  filter(is.na(programCode),
         degreeLevelSpelled %in% c("Bachelor of Arts",
                                   "Bachelor's Degree",
                                   "Bachelor of Science",
                                   "Bachelor of Music",
                                   "Bachelor of Religious Education",
                                   "Bachelor of Sacred Theology",
                                   "Bachelor of Theology",
                                   "Bachelor of Science in Education",
                                   "Bachelor of Music Educ",
                                   "Bachelor of Arts in Theology",
                                   "Bachelor of Education")) |>
  left_join(majdeg |> 
              filter(g_u == "U", 
                     is.na(DEGR_CDE) | DEGR_CDE %in% c("BA", "BM", "BS")) |> 
              select(MAJOR_CDE, CIP_CDE, MAJOR_MINOR_DESC), 
            join_by(programNameOriginal == MAJOR_MINOR_DESC)) |>
  mutate(programCode = MAJOR_CDE,
         cip6 = CIP_CDE,
         .keep = "unused")
from_majass <- from_deghist_majdeg |>
  filter(is.na(programCode),
         degreeLevelSpelled %in% c("Associate of Science",
                                   "Associate of Applied Science")) |>
  left_join(majdeg |> 
              filter(g_u == "U", 
                     is.na(DEGR_CDE) | !(DEGR_CDE %in% c("BA", "BM", "BS"))) |> 
              select(MAJOR_CDE, CIP_CDE, MAJOR_MINOR_DESC), 
            join_by(programNameOriginal == MAJOR_MINOR_DESC)) |>
  mutate(programCode = MAJOR_CDE,
         cip6 = CIP_CDE,
         .keep = "unused")
from_majma <- from_deghist_majdeg |>
  filter(is.na(programCode),
         degreeLevelSpelled %in% c("Master of Arts")) |>
  left_join(majdeg |> 
              filter(g_u == "G") |> 
              select(MAJOR_CDE, CIP_CDE, MAJOR_MINOR_DESC), 
            join_by(programNameOriginal == MAJOR_MINOR_DESC)) |>
  mutate(programCode = MAJOR_CDE,
         cip6 = CIP_CDE,
         .keep = "unused")
from_majunk <- from_deghist_majdeg |>
  filter(is.na(programCode),
         degreeLevelSpelled %in% c("Diploma",
                                   "Unknown Degree",
                                   "Certificate")) |>
  left_join(majdeg |> 
              filter(!(DEGR_CDE %in% c("BA", "BM", "BS", "MEDU", "EDS", "MMED", "MA", "MS", "MPH")) | is.na(DEGR_CDE)) |> 
              select(MAJOR_CDE, CIP_CDE, MAJOR_MINOR_DESC), 
            join_by(programNameOriginal == MAJOR_MINOR_DESC)) |>
  mutate(programCode = MAJOR_CDE,
         cip6 = CIP_CDE,
         .keep = "unused")
all_from_degrees <- rbind(from_deghist_majdeg |> filter(is.na(programCode)),
      from_majba,
      from_majass,
      from_majma,
      from_majunk) |>
  filter(is.na(programCode),
         programNameOriginal != "Unknown Degree")



# Exploring using Jenzabar as the backbone --------------------------------

# From the below, it looks like both Salesforce and Jenzabar begin to have 
# complete or semi-complete records starting in 1920. Salesforce has the same 
# or more grads in 1990, and 1988 and before. Jenzabar has the same or more 
# grads in 1989, and 1991 and after. Of Salesforce's 5170 grads, 2,860 are in 
# Jenzabar. Of the folks with a 1900 gradDate in Salesforce, only one is in 
# Jenzabar and graduated in 2025. 

year_from_rick <- alum |>
  mutate(year = str_extract(gradDate, "[:digit:]{4}")) |>
  distinct(studentID, year) |>
  count(year)

dev_year <- dev_person |>
  pivot_longer(cols = c(UNDERGRAD_GRAD_YR, GRAD_GRAD_YEAR),
               names_to = "type",
               values_to = "year") |>
  drop_na(year) |>
  select(ID_NUM, year) |>
  distinct()
deg_year <- degree_history |>
  mutate(year = str_extract(DTE_DEGR_CONFERRED, "[:digit:]{4}")) |>
  select(ID_NUM, year) |>
  drop_na(year) |>
  distinct()
year_from_jenzabar <- rbind(deg_year, dev_year) |>
  distinct() |>
  count(year)

full_join(year_from_rick, year_from_jenzabar, join_by(year)) |> 
  arrange(year) |> 
  filter(n.x > n.y) |>
  View()

barrington_from_rick <- alum |>
  filter(campus == "Barrington College")
rbind(deg_year, dev_year) |>
  distinct() |>
  filter(ID_NUM %in% barrington_from_rick$studentID)
century_grads <- alum |>
  filter(gradDate == ymd("1900-01-01"))
rbind(deg_year, dev_year) |>
  distinct() |> 
  filter(ID_NUM %in% century_grads$studentID)

  
no_deg_info <- dev_person |> 
  filter(is.na(UNDERGRAD_GRAD_YR),
         is.na(GRAD_GRAD_YEAR),
         !is.na(CLASS_YEAR))
inner_join(alum, no_deg_info, join_by(studentID == ID_NUM)) |> View()

alum |>
  filter(gradDate == ymd("1900-01-01")) |>
  # left_join(degree_history, join_by(studentID == ID_NUM)) |> View()
  left_join(dev_person |> mutate(ID_NUM = as.numeric(ID_NUM)), join_by(studentID == ID_NUM)) |> View()

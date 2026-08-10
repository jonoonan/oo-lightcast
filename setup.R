# Date: 2025-05-07
# Author: Sam Mason

# Instructions: source this script to setup fonts, themes, and palette. Please
# read through the code to understand how fonts should be used

library(showtext) # for font support
library(ggpath) # image rendering in ggplot2
library(ggplot2)

# Fonts -------------------------------------------------------------------

## Gotham (official sans serif) alternatives
### Montserrat
font_add_google(name = "Montserrat",
                family = "montserrat")
### Open Sans
font_add_google(name = "Open Sans",
                family = "open_sans")

## Amster (official serif) alternatives
### None identified

## Institutional Research fonts (approach modeled after Our World in Data)
### Neuton (serif; for plot titles)
font_add_google(name = "Neuton",
                family = "neuton")
### Jost (sans serif; for all other text)
font_add_google(name = "Jost",
                family = "jost")

## Setting up font rendering in ggplot2
showtext_auto()
showtext_opts(dpi = 600) # always export with ggsave() at dpi = 600

# Logo --------------------------------------------------------------------

ir_logo <- "resources/ir/ir_horizontal_blue.png"

# Theme -------------------------------------------------------------------

## See /resources/ggplot_theme.R

# Color palette -----------------------------------------------------------

source("resources/gc_palette.R")

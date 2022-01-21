library(scales)
library(reshape2)
library(ggplot2)
library(combinat)
library(readr)
library(dplyr)

source("scripts/data_functions.R")
source("scripts/plot_functions.R")
source("scripts/unit_status_module.R")

# Read in data tables
unit.data <- read_csv("data/unit_data.csv")  %>%
  filter(nchar(image_name_front) > 0) %>%
  arrange(nationality, branch, type, unit_name, desc(attack_front)) %>%
  mutate(team = if_else(nationality == "Japan", "Japan", "Allies"),
         battle_type = if_else(type == "Ground", "ground", "an"),
         id = row_number(nationality))

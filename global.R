library(scales)
library(reshape2)
library(dtplyr)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

source("scripts/data_functions.R")
source("scripts/plot_functions.R")

# Read in data tables
unit.data <- fread("data/unit_data.csv")  %>%
  arrange(nationality, type, branch, desc(attack_front)) %>%
  mutate(team = if_else(nationality == "Japan", "Japan", "Allies"),
         battle_type = if_else(type == "Ground", "ground", "an"),
         id = row_number(nationality))

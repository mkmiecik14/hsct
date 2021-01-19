# HSCT Controls (separate from DOD study) Preprocessing Script
# Matt Kmiecik
# Started 14 November 2018

# Libraries ----
library(tidyverse)
library(readxl)
library(naniar)

# Import data & clean ----
control_hsct_data <- read_excel("raw-data/hsct-data-updated.xlsx", 
                                sheet = "hsct"
                                )

control_iq_data <- read_excel("raw-data/neuropsych-data.xlsx",
                              sheet = "iq"
                              )

# Demographic data are collected in Qualtrics
# these data must first be downloaded from Qualtrics and imported from a file
# when exporting .csv from Qualtrics, use choice text
control_demo_data <- read_csv("raw-data/demo-11-April-2019.csv", 
                              na = c(common_na_strings, "Na")
                              )  %>%
  filter(Progress == 100) %>% # deletes first two rows bc they are not needed
  select(ID:SC1) %>% # selects only cols needed
  rename(Sex_Other = Sex_3_TEXT, 
         Age = Age_1, 
         Edu = Edu_1, 
         AUDIT_SCORE = SC0,
         BDI_SCORE = SC1
         )

# Save out data ----
save(control_hsct_data, file = "control-hsct-data.RData")
save(control_iq_data,   file = "control-iq-data.RData")
save(control_demo_data, file = "control-demo-data.RData")

# Cleaning up script ----
rm(control_demo_data, control_hsct_data, control_iq_data)

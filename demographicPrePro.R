# Purpose: preprocess the HSCT demographic survey exported from Qualtrics

# loading packages
library(tidyverse)

# Change the name of this to the most recent demographic file
demoFile <- 'demo-15-June-2018.csv'

# Loads the data into R
demoData <- read_csv(file = file.path('raw-data', demoFile)) %>%
  filter(Progress == 100)

as_tibble(demoData)

# Cleans the data using dplyr
demoDataClean <- demoData %>% 
  filter(Progress == 100) %>% # Deletes first two rows
  select(ID:Other_Language_Speak_3...Topics) %>% # Selects columns of interest
  mutate(auditScore = sum(as.numeric(AUDIT_1:AUDIT_10))) # Creates scores

# Writes the file out
write_csv(demoDataClean, path = file.path('clean-data/', 'demoDataCleaned.csv'))

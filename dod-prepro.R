# Preprocessing script for DOD study - neuropscyh only

# Load packages ----
library(tidyverse); library(readxl); library(Hmisc)

# Load data ----
dod_data <- 
  read_excel(
    "../data/dod-neuropsych-data.xlsx", 
    sheet = "mattDatabase_addedHSCT_SS",
    na = c("", "na")
    )

dod_hayling_tbi <- 
  read_excel(
    "../data/dod-hayling-words.xlsx",
    sheet = "TBI_T1_R",
    na = c("", "na", "(no answer)", "N/A")
    )

dod_hayling_ctl <- 
  read_excel(
    "../data/dod-hayling-words.xlsx",
    sheet = "Control_T1_R",
    na = c("", "na", "(no answer)", "N/A"),
    skip = 1
    )

# Prepro pipeline ----

# Neuropscyh data
# Dropping these participants (outdated protocol)
not_wanted <- as.character(300:399)

dod_pre <- 
  dod_data %>% 
  select(
    dodID, time, participantType,	trainingGroup,	tbiLevel,	status,	yrEd,	sex,	
    maritalStatus,	race, birthDate,	age,	dateLastInjury,	injuryType,	injuryDesc,	notes,
    audit,	awareness,	bdi,	bdiCog,	bdiAffective,	bdiSomatic,	gose,	osuNumLOC,
    osuNumTBIGT30min,	osu1stLOCAge,	osu1stTBIAge,	osuWorstInjury, sec1RawScore,	
    sec1SS,	sec2RawScore,	sec2SS,	catAErrors,	aScore,	catBErrors,	bScore,	
    abScore,	sec2ErrorsSS,	abcSS, overallSS,
    similaritiesRawScore, similaritiesTScore, matrixRawScore, matrixTScore,
    vocabRawScore, vocabTScore, wasiIQ, wtarRawScore, starts_with("wtar")
    ) %>%
  filter(time == "Pre", dodID %nin% not_wanted)

# Hayling words
dod_hayling_tbi_trim <- 
  dod_hayling_tbi %>%
  select(
    DODID = `DOD-ID`, 
    paste0("SC0", 1:9), paste0("SC", 10:15),
    paste0("UC0", 1:9), paste0("UC", 10:15)
    ) %>%
  filter(complete.cases(DODID))

dod_hayling_ctl_trim <- 
  dod_hayling_ctl %>%
  select(
    DODID = `DOD-ID`, 
    paste0("SC0", 1:9), paste0("SC", 10:15),
    paste0("UC0", 1:9), paste0("UC", 10:15)
    ) %>%
  filter(complete.cases(DODID)) %>%
  filter(DODID %nin% not_wanted)
  
# Saving out data ---- 
save(dod_pre, file = "../output/dod-pre-data.RData")
save(dod_hayling_tbi_trim, file = "../output/dod-hayling-tbi-trim.RData")
save(dod_hayling_ctl_trim, file = "../output/dod-hayling-ctl-trim.RData")

# Cleaning up script ----
rm(
  dod_data, 
  dod_pre, 
  dod_hayling_ctl, dod_hayling_ctl_trim, 
  dod_hayling_tbi, dod_hayling_tbi_trim,
  not_wanted
  )

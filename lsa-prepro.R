# Preprocessing script that calculates LSA values for the HSCT study
# Matt Kmiecik
# Started 18 April 2019
 
# Packages ----
library(Hmisc)
library(tidyverse)
library(LSAfun)
library(broom)
library(ngram)

# Source ----
# Run these scripts (if needed) for most updated data:
# source("dod-prepro.R")
# source("control-prepro.R")

# Loading data ----
load("../output/control-hsct-data.RData")     # From HSCT Control study
load("../output/dod-hayling-tbi-trim.RData")  # From DOD study
load("../output/dod-hayling-ctl-trim.RData")  # From DOD study
load("../data/EN_100k.rda")                   # Semantic space for LSA
# SUBTLEX
subtlus <- 
  read_delim('../data/SUBTLEXus74286wordstextversion.txt', delim = '\t')

# Calculating LSA data ----

# Creates a data frame of the hayling sentences
hsct_sents <- 
  tibble(
    section = rep(c("SC", "UC"), each = 15),
    num = rep(1:15, 2),
    sent = c(
      "He mailed a letter without a",
      "In the first blank enter your",
      "The old house will be torn",
      "It's hard to admit when one is",
      "The job was easy most of the",
      "When you go to bed turn off the",
      "The game was stopped when it started to",
      "He scraped the cold food from his",
      "The dispute was settled by a third",
      "Three people were killed in an interstate",
      "The baby cried and upset her",
      "George could not believe that his son had stolen a",
      "He crept into the room without a",
      "Billy hit his sister on the",
      "Too many men are out of",
      "The captain wanted to stay with the sinking",
      "They went as far as they",
      "Most cats see very well at",
      "Jean was glad the affair was",
      "The whole town came to hear the mayor",
      "Most sharks attack very close to",
      "None of the books made any",
      "The dough was put in the hot",
      "She called the husband at his",
      "All the guests had a very good",
      "He bought them in the candy",
      "His leaving home amazed all his",
      "At last the time for action had",
      "The dog chased our cat up the",
      "At night they often took a short"
      )
    )

# This calculates the LSA values

#CONTROLS FROM DOD
dod_hsct_ctl_calc <- 
  dod_hayling_ctl_trim %>%
  gather(section, resp, -DODID) %>%
  separate(section, sep = 2, into = c("section", "num")) %>%
  mutate(num = as.numeric(num)) %>%
  inner_join(., hsct_sents, by = c("section", "num")) %>%
  group_by(DODID, section, num, resp, sent) %>%
  do(lsa = costring(.$sent, .$resp, EN_100k, breakdown = TRUE)) %>%
  mutate(lsa = unlist(lsa), group = "Control") %>%
  ungroup()

# TBI FROM DOD
dod_hsct_tbi_calc <- 
  dod_hayling_tbi_trim %>%
  gather(section, resp, -DODID) %>%
  separate(section, sep = 2, into = c("section", "num")) %>%
  mutate(num = as.numeric(num)) %>%
  inner_join(., hsct_sents, by = c("section", "num")) %>%
  group_by(DODID, section, num, resp, sent) %>%
  do(lsa = costring(.$sent, .$resp, EN_100k, breakdown = TRUE)) %>%
  mutate(lsa = unlist(lsa), group = "TBI") %>%
  ungroup()

# Controls from HSCT
ctl_hsct_calc <- 
  control_hsct_data %>%
  filter(complete.cases(hsct.scaled)) %>%
  select(ssID, contains("word")) %>%
  gather(section, resp, -ssID) %>%
  separate(section, into = c("section", "drop_this", "num")) %>%
  mutate(
    num = as.numeric(num), 
    drop_this = NULL, 
    section = toupper(section)
    ) %>%
  inner_join(., hsct_sents, by = c("section", "num")) %>%
  group_by(ssID, section, num, resp, sent) %>%
  do(lsa = costring(.$sent, .$resp, EN_100k, breakdown = TRUE)) %>%
  mutate(
    lsa = unlist(lsa),
    group = "Control",
    DODID = as.character(ssID),
    ssID = NULL
    ) %>%
  ungroup()

# Combines TBI+CONTROLS from DOD and HSCT into one df
lsa_data <- bind_rows(dod_hsct_ctl_calc, dod_hsct_tbi_calc, ctl_hsct_calc)

save(lsa_data, file = "../output/lsa-data.RData") # Saves out LSA data

# Calculating WF measures ----

# DOD Controls

# First let's get the max num of multi-word responses
dod_hsct_ctl_calc_numwords <- 
  dod_hsct_ctl_calc %>%
  rowwise() %>%
  mutate(num_words = wordcount(resp)) # wordcount() is from ngram

# The max number of multi-word resp
# max(dod_hsct_ctl_calc_numwords$num_words) # answer is 2

# Calculates WF measures for each word (even if multi-word resp)
dod_hsct_ctl_subtlus <- 
  dod_hsct_ctl_calc %>% 
  separate(resp, into = c("resp1", "resp2"), sep = " ") %>% # uses above calc
  select(DODID:resp2) %>%
  gather(resp, Word, -DODID, -section, -num) %>%
  filter(complete.cases(Word)) %>% # removes the NAs for single word responses
  left_join(., subtlus)

# Takes the average for multi-word resp (if there are some)
dod_hsct_ctl_subtlus_1 <- 
  dod_hsct_ctl_subtlus %>%
  group_by(DODID, section, num) %>%
  summarise(
    subtlex.n = n(),
    subtlex.na = sum(is.na(SUBTLWF)),
    wf = mean(SUBTLWF, na.rm = FALSE), # FALSE ensures that the resp
    cd = mean(SUBTLCD, na.rm = FALSE)  # wont be counted in average
  ) %>%
  ungroup()

dod_hsct_ctl_subtlus_ss <- 
  dod_hsct_ctl_subtlus_1 %>%
  group_by(DODID, section) %>%
  summarise(
    subtlex.N = n(), 
    subtlex.NA = sum(is.na(wf)),
    m_wf = mean(wf, na.rm = TRUE), 
    m_cd = mean(cd, na.rm = TRUE)
  ) %>%
  mutate(group = "Control") %>%
  ungroup()

# DOD TBIs

# First let's get the max num of multi-word responses
dod_hsct_tbi_calc_numwords <- 
  dod_hsct_tbi_calc %>%
  rowwise() %>%
  mutate(num_words = wordcount(resp)) # wordcount() is from ngram

# The max number of multi-word resp
# max(dod_hsct_tbi_calc_numwords$num_words) # answer is 4

# Calculates WF measures for each word (even if multi-word resp)
dod_hsct_tbi_subtlus <- 
  dod_hsct_tbi_calc %>% 
  separate(
    resp, 
    into = c("resp1", "resp2", "resp3", "resp4"), 
    sep = " "
    ) %>% # uses above calc
  select(DODID:resp4) %>%
  gather(resp, Word, -DODID, -section, -num) %>%
  filter(complete.cases(Word)) %>% # removes the NAs for single word responses
  left_join(., subtlus)

# Takes the average for multi-word resp (if there are some)
dod_hsct_tbi_subtlus_1 <- 
  dod_hsct_tbi_subtlus %>%
  group_by(DODID, section, num) %>%
  summarise(
    subtlex.n = n(),
    subtlex.na = sum(is.na(SUBTLWF)),
    wf = mean(SUBTLWF, na.rm = FALSE), # FALSE ensures that the resp
    cd = mean(SUBTLCD, na.rm = FALSE)  # wont be counted in average
  ) %>%
  ungroup()

dod_hsct_tbi_subtlus_ss <- 
  dod_hsct_tbi_subtlus_1 %>%
  group_by(DODID, section) %>%
  summarise(
    subtlex.N = n(), 
    subtlex.NA = sum(is.na(wf)),
    m_wf = mean(wf, na.rm = TRUE), 
    m_cd = mean(cd, na.rm = TRUE)
  ) %>%
  mutate(group = "TBI") %>%
  ungroup()

# HSCT Controls 
# First let's get the max num of multi-word responses
ctl_hsct_calc_numwords <- 
  ctl_hsct_calc %>%
  rowwise() %>%
  mutate(num_words = wordcount(resp)) # wordcount() is from ngram

# The max number of multi-word resp
# max(ctl_hsct_calc_numwords$num_words) # answer is 2

# Calculates WF measures for each word (even if multi-word resp)
ctl_hsct_calc_subtlus <- 
  ctl_hsct_calc %>%
  separate(
    resp, 
    into = c("resp1", "resp2"), 
    sep = " "
  ) %>% # uses above calc
  select(DODID, section, num, resp1, resp2) %>%
  gather(resp, Word, -DODID, -section, -num) %>%
  filter(complete.cases(Word)) %>% # removes the NAs for single word responses
  left_join(., subtlus)

# Takes the average for multi-word resp (if there are some)
ctl_hsct_calc_subtlus_1 <- 
  ctl_hsct_calc_subtlus %>%
  group_by(DODID, section, num) %>%
  summarise(
    subtlex.n = n(),
    subtlex.na = sum(is.na(SUBTLWF)),
    wf = mean(SUBTLWF, na.rm = FALSE), # FALSE ensures that the resp
    cd = mean(SUBTLCD, na.rm = FALSE)  # wont be counted in average
  ) %>%
  ungroup()

ctl_hsct_calc_subtlus_ss <- 
  ctl_hsct_calc_subtlus_1 %>%
  group_by(DODID, section) %>%
  summarise(
    subtlex.N = n(), 
    subtlex.NA = sum(is.na(wf)),
    m_wf = mean(wf, na.rm = TRUE), 
    m_cd = mean(cd, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(DODID = as.character(DODID), group = "Control")

# Next, combine into one big df
wf_data <- 
  bind_rows(
    ctl_hsct_calc_subtlus_ss,
    dod_hsct_tbi_subtlus_ss,
    dod_hsct_ctl_subtlus_ss
    )

save(wf_data, file = "../output/wf-data.RData") # Saves out WF and CD data

# Clean up variables ----
rm(
  control_hsct_data,
  ctl_hsct_calc,
  ctl_hsct_calc_numwords,
  ctl_hsct_calc_subtlus,
  ctl_hsct_calc_subtlus_1,
  ctl_hsct_calc_subtlus_ss,
  dod_hayling_ctl_trim,
  dod_hayling_tbi_trim,
  dod_hsct_ctl_calc,
  dod_hsct_ctl_calc_numwords,
  dod_hsct_ctl_subtlus,
  dod_hsct_ctl_subtlus_1,
  dod_hsct_ctl_subtlus_ss,
  dod_hsct_tbi_calc,
  dod_hsct_tbi_calc_numwords,
  dod_hsct_tbi_subtlus,
  dod_hsct_tbi_subtlus_1,
  dod_hsct_tbi_subtlus_ss,
  EN_100k,
  hsct_sents,
  lsa_data,
  pkgs,
  subtlus,
  wf_data
  )




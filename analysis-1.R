# Main analysis script for the HSCT project
# Matt Kmiecik
# Re-purposed 21 JAN 2021

# Loading packages ----
library(Hmisc)
library(tidyverse)
library(broom)
library(RColorBrewer)

# Plotting tools ----
pd <- position_dodge(width = .1)
pj <- position_jitter(width = .1)
pu_pal <- brewer.pal(9, "Purples") # display.brewer.pal(9, "Purples")

# Preprocessing scripts ----
# run this section if you haven't already:
# (not run)
# source("dod-prepro.R")
# the above prepro script creates the following:
# load("dod-pre-data.RData")          # From DOD study
# load("dod-hayling-tbi-trim.RData")  # From DOD study
# load("dod-hayling-ctl-trim.RData")  # From DOD study

# source("control-prepro.R")
# the above prepro script creates the following:
# load("control-hsct-data.RData")   # From HSCT Control study
# load("control-iq-data.RData")     # From HSCT Control study
# load("control-demo-data.RData")   # From HSCT Control study

# source("lsa-prepro.R")
# The above prepro script creates the following:
# load("lsa-data.RData")

# Loading data ----

# Neuropsych data
# These data were created by running: source("dod-prepro.R")
load("../output/dod-pre-data.RData")            # From DOD study

# From source("control-prepro.R")
load("../output/control-hsct-data.RData")       # From HSCT Control study

# These data were created by running: source("control-prepro.R")
load("../output/control-iq-data.RData")         # From HSCT Control study

# Demographic data
load("../output/control-demo-data.RData")       # From HSCT Control study

# LSA, WF, and CD measures
load("../output/lsa-data.RData")
load("../output/wf-data.RData")

# Organizing demographic data
# DOD
# This pipeline prepares the DOD neuropsych data to match the HSCT control study
dod_neuropsych <- 
  dod_pre %>%
  select(
    DODID = dodID, 
    group = participantType, 
    yrEd, 
    sex, 
    age, 
    audit, 
    bdi,
    sc.time.raw = sec1RawScore,
    sc.time.scaled = sec1SS,
    uc.time.raw = sec2RawScore,
    uc.time.scaled = sec2SS,
    cat.a.errors = catAErrors,
    a.score = aScore,
    cat.b.errors = catBErrors,
    b.score = bScore,
    ab.score = abScore,
    errors.scaled = sec2ErrorsSS,
    total.scaled = abcSS,
    hsct.scaled = overallSS,
    wasi.vocab.rscore = vocabRawScore,
    wasi.vocab.tscore = vocabTScore,
    wasi.similarities.rscore = similaritiesRawScore,
    wasi.similarities.tscore = similaritiesTScore,
    wasi.matrix.rscore = matrixRawScore,
    wasi.matrix.tscore = matrixTScore,
    # wasi.viq = ,    Not available in DOD data, but is possible to obtain
    wasi.full2IQ = wasiIQ,
    wtar.rscore = wtarRawScore,
    wtar.sscore = wtarSS
    ) %>%
  rowwise() %>%
  mutate(
    wasi.tverbal = sum(wasi.vocab.tscore, wasi.similarities.tscore),
    wasi.full2subtest = sum(wasi.vocab.tscore, wasi.matrix.tscore),
    audit = as.numeric(audit)
  ) %>%
  ungroup() %>%
  filter(complete.cases(hsct.scaled)) %>% # removes ss if they don't have HSCT
  # removes if they don't have HSCT words
  filter(DODID %in% unique(lsa_data$DODID)) %>%
  # The following participants need to be removed bc of the following:
  #   DOD-0276 : brain tumor
  #   DOD-0381 : has Erb's Palsy and embolisms due to diving accident
  #   DOD-1114 : has bipolar and depression
  #   DOD-5359 : did not have a TBI, but had LOC due to lack of oxygen
  filter(DODID %nin% c("DOD-0276", "DOD-0381", "DOD-1114", "DOD-5359"))

# HSCT controls
# This pipeline prepares the HSCT control data to match the DOD neuropsych data
hsct_neuropsych <- 
  control_iq_data %>% 
  inner_join(
    ., 
    control_demo_data %>% mutate(ssID = as.numeric(ID)), 
    by = "ssID"
    ) %>%
  inner_join(., control_hsct_data, by = "ssID") %>%
  select(
    DODID = ssID,
    starts_with("wasi"),
    sex = Sex,
    age = Age,
    yrEd = Edu,
    audit = AUDIT_SCORE,
    bdi = BDI_SCORE,
    sc.time.raw:hsct.scaled,
    starts_with("wasi"),
    starts_with("wtar")
    ) %>%
  filter(complete.cases(hsct.scaled)) %>%
  filter(DODID != 3231 | DODID != 5481 | DODID != 1370) %>% # DROP THESE SS
  mutate(
    group = "Control", 
    DODID = as.character(DODID), 
    yrEd = as.numeric(yrEd),
    age = as.numeric(age),
    bdi = as.numeric(bdi),
    audit = as.numeric(audit)
  ) 

# Combines all demo and neuropsych into one df
demo_neuropsych <- bind_rows(hsct_neuropsych, dod_neuropsych)

# A few notes ----
# Now, let's drop participants that have AUDIT scores > 10, 
# as Kmiecik et al. (2018) did this, and summaize all measures separated by group.
# 
# A couple of important NAs will emerge:
# 
# * DOD-1879 has no BDI
# * DOD-3484 was only administered 1/2 of the BDI form
# 
# The rest of the NAs that did emerge: we went through the paper copies and updated the "dod-neuropsych-data.xlsx" sheet = "mattDatabase_addedHSCT_SS"
# 
# Also, we are taking a look at if we were to look at the groups separated by OSU worst injury score:
# 
# 1 = "improbable TBI"
# 2 = "possible TBI"
# 3 = "mild TBI"
# 4 = "moderately severe TBI"
# 5 = "more severe TBI"
# 
# We split this into 3 comparisons:
# 
# * Control vs TBI (all severities)
# * Possible TBI vs. Mild + Moderate/Severe TBI
# * Mild vs. Moderate/Severe 

# Let's first gather the TBI injury information
osu <- dod_pre %>% select(DODID = dodID, osuWorstInjury)

# Cleaning up the big df with various measures
demo_neuropsych_clean <- 
  demo_neuropsych %>% 
  filter(audit <= 10) %>% # drops ss with elevated audit scores
  left_join(., osu, by = "DODID") %>% # inserts OSU Scores
  mutate(
    osuWorstInjury = case_when(
      is.na(osuWorstInjury) ~ "Control",
      osuWorstInjury == 2 ~ "Possible",
      osuWorstInjury == 3 ~ "Mild",
      osuWorstInjury == 4 ~ "Mod/Severe",
      osuWorstInjury == 5 ~ "Mod/Severe"
    ),
    osuWorstInjury = factor(osuWorstInjury) # converts to factor
  )

# Notes about drops:
# Started out with 152 TBI participants
# write_csv(
#   demo_neuropsych_clean %>% select(DODID, group), 
#   path = "final-ss.csv"
#   )
# test <- demo_neuropsych %>% filter(audit > 10)

# Specifies contrasts
# This helps with the linear modeling
contrasts(demo_neuropsych_clean$osuWorstInjury) <-
  cbind(
    CvTBI = c(+3, -1, -1, -1),
    PvMMS = c(0, -1, -1, +2),
    MvMS  = c(0, +1, -1, 0)
  )

# Long format
demo_neuropsych_long <- 
  demo_neuropsych_clean %>%
  select(-group) %>% # don't need this anymore
  gather(meas, value, -DODID, -osuWorstInjury)

# Summarizes demographic and HSCT standard data
demo_clean_desc <- 
  demo_neuropsych_long %>%
  filter(meas %nin% "sex") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(osuWorstInjury, meas) %>%
  summarise(
    n = n(),
    n.na = sum(is.na(value)),
    m = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  ungroup()

# Saves out these results
write_csv(
  demo_clean_desc, 
  file = "../output/res-demo-neuropsych-descriptives.csv"
  )

# Looking a those with NAs
# yo <- demo_neuropsych_clean %>% filter(is.na(value))
# Notes about the NAs:
# - None of the control DOD participants were administered the WTAR
# - The DOD database does not store the verbal IQ, just the t-scores

# TBI measures
dod_pre %>%
  filter(dodID %in% unique(demo_neuropsych_clean$DODID)) %>%
  select(dodID, osuWorstInjury) %>%
  count(osuWorstInjury) # The NA's are from DOD controls

# Runs linear model on whole data set
demo_neuropsych_lm <- 
  demo_neuropsych_long %>%
  # filters out non numeric vars and vars with all NAs
  filter(meas %nin% c("sex", "wasi.viq", "wtar.rscore", "wtar.sscore")) %>% 
  nest_by(meas) %>%
  mutate(mod = list(lm(value ~ 1 + osuWorstInjury, data = data)))

# Tidying the lms to plot
demo_neuropsych_tidy <- 
  demo_neuropsych_lm %>% 
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = .95)) %>%
  mutate(sig = ifelse(p.value < .05, "True", "False")) %>%
  ungroup()

# For table
demo_neuropsych_tidy %>%
  filter(meas %in% c("age", "wasi.full2IQ")) %>%
  filter(term %nin% c("(Intercept)")) %>%
  select(meas, term, t = statistic, p = p.value)

# plotting the lms to see what is different between the groups
ggplot(
  demo_neuropsych_tidy %>% filter(term == "osuWorstInjuryCvTBI"),
  aes(statistic, reorder(meas, statistic), color = sig)
  ) +
  geom_point() +
  labs(x = "t-value", y = "measure", title = "osuWorstInjuryCvTBI") +
  theme_minimal()

ggplot(
  demo_neuropsych_tidy %>% filter(term == "osuWorstInjuryPvMMS"),
  aes(statistic, reorder(meas, statistic), color = sig)
  ) +
  geom_point() +
  labs(x = "t-value", y = "measure", title = "osuWorstInjuryPvMMS") +
  theme_minimal()

ggplot(
  demo_neuropsych_tidy %>% filter(term == "osuWorstInjuryMvMS"),
  aes(statistic, reorder(meas, statistic), color = sig)
  ) +
  geom_point() +
  labs(x = "t-value", y = "measure", title = "osuWorstInjuryMvMS") +
  theme_minimal()

# For paper table of category A + category B errors and HSCT Scaled scores ----
cat_A_mod <- 
  lm(
    cat.a.errors ~ 1 + scale(age, scale = FALSE) + 
      scale(wasi.full2IQ, scale = FALSE) + osuWorstInjury, 
    data = demo_neuropsych_clean
    )

cat_B_mod <- 
  lm(cat.b.errors ~ 1 + scale(age, scale = FALSE) + 
       scale(wasi.full2IQ, scale = FALSE) + osuWorstInjury, 
     data = demo_neuropsych_clean
     )

hsct_scaled_mod <- 
  lm(hsct.scaled ~ 1 + scale(age, scale = FALSE) + 
    scale(wasi.full2IQ, scale = FALSE) + osuWorstInjury, 
    data = demo_neuropsych_clean
    )

# Tidying up the models
cat_A_mod_tidy <- 
  tidy(cat_A_mod, conf.int = TRUE, conf.level = .95) %>% 
  mutate(Model = "Category A Errors")

cat_B_mod_tidy <- 
  tidy(cat_B_mod, conf.int = TRUE, conf.level = .95) %>% 
  mutate(Model = "Category B Errors")

hsct_scaled_mod_tidy <- 
  tidy(hsct_scaled_mod, conf.int = TRUE, conf.level = .95) %>% 
  mutate(Model = "HSCT Overall Scaled Scores")

# Combining them into one df and renaming
above_models_tidy <- 
  bind_rows(cat_A_mod_tidy, cat_B_mod_tidy, hsct_scaled_mod_tidy) %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "scale(age, scale = FALSE)" ~ "Age",
    term == "scale(wasi.full2IQ, scale = FALSE)" ~ "IQ",
    term == "osuWorstInjuryCvTBI" ~ "Control vs. TBI",
    term == "osuWorstInjuryPvMMS" ~ "Possible vs. Mild/Moderate/Severe TBI",
    term == "osuWorstInjuryMvMS" ~ "Mild vs. Moderate/Severe TBI",
    TRUE ~ as.character(term)
    )
  ) %>%
  select(
    Model, Term = term, b = estimate,  LL = conf.low, UL = conf.high,
    SE = std.error, t = statistic, p = p.value, 
  )

# Save out to further polish in excel for paper
write_csv(above_models_tidy, path = "../output/paper-table-error-mods.csv")



# LSA Analysis ----

# Calculates LSA summary data
lsa_data_sum <- 
  lsa_data %>%
  filter(DODID %in% unique(demo_neuropsych_long$DODID)) %>%
  group_by(DODID, section) %>%
  summarise(lsa.m = mean(lsa, na.rm = TRUE), sd = sd(lsa, na.rm = TRUE), n = n()) %>%
  ungroup()

# Calculates the main effect of HSCT section
# joins with neuropsych
lsa_sum_iq <- 
  lsa_data_sum %>%
  group_by(DODID) %>%
  summarise(lsa_diff = -diff(lsa.m)) %>%
  inner_join(
    ., 
    demo_neuropsych_clean, 
    by = "DODID"
  ) %>%
  mutate(group = NULL) %>%
  ungroup()

# Modeling LSA
lsa_mod <- 
  lm(
    lsa_diff ~ 1 + scale(age, scale = FALSE) + 
      scale(wasi.full2IQ, scale = FALSE) + osuWorstInjury, 
    data = lsa_sum_iq)
summary(lsa_mod)

# SUBJECT-WISE summary
lsa_data_ss <- 
  lsa_data %>%
  filter(DODID %in% unique(demo_neuropsych_long$DODID)) %>%
  group_by(DODID, group, section) %>%
  summarise(n = n(), n.na = sum(is.na(lsa)), mLSA = mean(lsa, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(., demo_neuropsych_clean, by = "DODID") %>%
  mutate(group.x = NULL, group.y = NULL) %>%
  mutate(
    c1 = ifelse(osuWorstInjury == "Control", "Control", "TBI"),
    c2 = case_when(
      osuWorstInjury == "Possible" ~ "Possible",
      osuWorstInjury == "Mild" ~ "Mild/Mod/Severe",
      osuWorstInjury == "Mod/Severe" ~ "Mild/Mod/Severe",
      TRUE ~ as.character(osuWorstInjury)
    ),
    c3 = case_when(
      osuWorstInjury == "Mild" ~ "Mild",
      osuWorstInjury == "Mod/Severe" ~ "Mod/Severe",
      TRUE ~ as.character(osuWorstInjury)
    )
  )

# GROUP-WISE summary
lsa_data_sum_c1 <- 
  lsa_data_ss %>% 
  group_by(c1, section) %>%
  summarise(N = n(), m = mean(mLSA), sd = sd(mLSA), sem = sd/sqrt(N)) %>%
  ungroup()

# PAPER FIGURE
# SD Error Bars
ggplot(lsa_data_sum_c1, aes(section, m, group = c1, color = c1)) +
  geom_point(
    data = lsa_data_ss, aes(y = mLSA), 
    shape = 1, alpha = 2/3, position = pj, size = 2
  ) +
  geom_point(position = pd, size = 2) +
  geom_errorbar(
    aes(ymin = m-sd, ymax = m+sd), 
    width = .05, position = pd
  ) +
  geom_path(position = pd) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Section", y="Mean LSA Score \n", color = "group") +
  #coord_cartesian(ylim = c(.4, .6)) +
  theme_minimal() +
  theme(legend.position = "none")

# Exports SVG for further work in omnigraffle (uncomment if necessary) 
# ggsave(
#   "../output/paper-figure-lsa-sd-error-bars.svg", 
#   width = 95, 
#   height = 117, 
#   units = "mm"
#   )

# SEM Error Bars
ggplot(lsa_data_sum_c1, aes(section, m, group = c1, color = c1)) +
  geom_point(
    data = lsa_data_ss, aes(y = mLSA), 
    shape = 1, alpha = 2/3, position = pj, size = 2
  ) +
  geom_point(position = pd, size = 2) +
  geom_errorbar(
    aes(ymin = m-sem, ymax = m+sem), 
    width = .05, position = pd
  ) +
  geom_path(position = pd) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Section", y = "Mean LSA Score \n", color = "group") +
  #coord_cartesian(ylim = c(.4, .6)) +
  theme_minimal() +
  theme(legend.position = "none")

# Exports SVG for further work in omnigraffle (uncomment if necessary) 
# ggsave(
#   "../output/paper-figure-lsa-sem-error-bars.svg", 
#   width = 95, 
#   height = 117, 
#   units = "mm"
#   )

# Correlation
ggplot(lsa_sum_iq, aes(wasi.full2IQ, lsa_diff)) +
  geom_point(aes(color = osuWorstInjury)) +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    color = "black", 
    linetype = 2, 
    alpha = 1/3
    ) +
  theme_minimal()


# HSCT RT Analysis ----

# HSCT Time model
hsct_time_mod <- 
  lm(
    sc.time.raw - uc.time.raw ~ 1 + 
    scale(age, scale = FALSE) + 
    scale(wasi.full2IQ, scale = FALSE) + 
    osuWorstInjury, 
    data = demo_neuropsych_clean
)
summary(hsct_time_mod)
anova(hsct_time_mod)

# Age vs. main effect of section time
ggplot(demo_neuropsych_clean, aes(age, sc.time.raw - uc.time.raw)) +
  geom_point(aes(color = osuWorstInjury)) +
  geom_smooth(method = "lm", color = "black", linetype = 2, alpha = 1/3) +
  theme_minimal()

hsct_time_ss <- 
  demo_neuropsych_clean %>% 
  select(DODID, sc.time.raw, uc.time.raw, group) %>%
  gather(section, time, -DODID, -group)

hsct_time_sum <- 
  hsct_time_ss %>%
  group_by(group, section) %>%
  summarise(m = mean(time), sd = sd(time), n = n(), sem = sd/sqrt(n)) %>%
  ungroup()

# PAPER FIGURE
# SEM error bars
ggplot(hsct_time_sum, aes(section, m, group = group, color = group)) +
  geom_point(data = hsct_time_ss, aes(y = time), position = pj, shape = 1, alpha = 2/3) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m - sem, ymax = m + sem), width = .1, position = pd) +
  #coord_cartesian(ylim = c(0, 40)) +
  labs(x = "HSCT Section", y = "Mean HSCT Section Time \n") +
  theme_minimal() +
  theme(legend.position = "none")

# Exports SVG for further work in omnigraffle (uncomment if necessary) 
# ggsave(
#   "../output/paper-figure-hsct-time-sem-error-bars.svg", 
#   width = 95, 
#   height = 117, 
#   units = "mm"
#   )

# SD error bars
ggplot(hsct_time_sum, aes(section, m, group = group, color = group)) +
  geom_point(data = hsct_time_ss, aes(y = time), position = pj, shape = 1, alpha = 2/3) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m - sd, ymax = m + sd), width = .1, position = pd) +
  #coord_cartesian(ylim = c(0, 40)) +
  labs(x = "HSCT Section", y = "Mean HSCT Section Time \n") +
  theme_minimal() +
  theme(legend.position = "none")

# Exports SVG for further work in omnigraffle (uncomment if necessary) 
# ggsave(
#   "../output/paper-figure-hsct-time-sd-error-bars.svg", 
#   width = 95, 
#   height = 117, 
#   units = "mm"
#   )


# Word Frequency Analysis ----

# Calculates the main effect of HSCT section wf + cd
# joins with neuropsych
wf_sum_iq <- 
  wf_data %>%
  filter(DODID %in% unique(demo_neuropsych_long$DODID)) %>%
  group_by(DODID) %>%
  summarise(wf_diff = -diff(m_wf), cd_diff = -diff(m_cd)) %>%
  inner_join(
    ., 
    demo_neuropsych_clean, 
    by = "DODID"
  ) %>%
  mutate(group = NULL) %>%
  ungroup()

# WF linear model
wf_mod <- 
  lm(
    wf_diff ~ 1 + 
    scale(age, scale = FALSE) +
    scale(wasi.full2IQ, scale = FALSE) + 
    osuWorstInjury, 
    data = wf_sum_iq
    )
summary(wf_mod)

# SV linear model
cd_mod <- 
  lm(
    cd_diff ~ 1 + 
    scale(age, scale = FALSE) +
    scale(wasi.full2IQ, scale = FALSE) + 
    osuWorstInjury, 
    data = wf_sum_iq
    )
summary(cd_mod)

ggplot(wf_sum_iq, aes(wasi.full2IQ, wf_diff)) +
  geom_point(aes(color = osuWorstInjury)) +
  geom_smooth(
    method = "lm", se = TRUE, alpha = 1/3, color = "black", linetype = 2
  ) +
  theme_minimal()

ggplot(wf_sum_iq, aes(wasi.full2IQ, cd_diff)) +
  geom_point(aes(color = osuWorstInjury)) +
  geom_smooth(
    method = "lm", se = TRUE, alpha = 1/3, color = "black", linetype = 2
  ) +
  theme_minimal()

# summarises
wf_data_sum <- 
  wf_data %>%
  filter(DODID %in% unique(demo_neuropsych_long$DODID)) %>%
  select(DODID, section, m_wf, m_cd) %>%
  group_by(section) %>%
  summarise_at(
    vars(c(m_wf:m_cd)), 
    list(mean = mean, sd = sd, n = function(x){n()})
    ) %>%
  mutate(sem_wf = m_wf_sd/sqrt(m_wf_n), sem_cd = m_cd_sd/sqrt(m_cd_n))

# Converting to long format
wf_data_sum_long <- 
  wf_data_sum %>% pivot_longer(m_wf_mean:sem_cd)



# Creating linear modeling table ----

# Tidys up models and computes 95% CIs
time_mod_tidy <- tidy(hsct_time_mod, conf.int = TRUE, conf.level = .95) %>% mutate(Model = "Time")
lsa_mod_tidy  <- tidy(lsa_mod, conf.int = TRUE, conf.level = .95)  %>% mutate(Model = "LSA")
wf_mod_tidy   <- tidy(wf_mod, conf.int = TRUE, conf.level = .95)  %>% mutate(Model = "Word Frequency")
cd_mod_tidy   <- tidy(cd_mod, conf.int = TRUE, conf.level = .95)  %>% mutate(Model = "Contextual Diversity")

# Assembles to a table and cleans up naming/order of columns
all_models_tidy <- 
  bind_rows(time_mod_tidy, lsa_mod_tidy, wf_mod_tidy, cd_mod_tidy) %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "scale(age, scale = FALSE)" ~ "Age",
    term == "scale(wasi.full2IQ, scale = FALSE)" ~ "IQ",
    term == "osuWorstInjuryCvTBI" ~ "Control vs. TBI",
    term == "osuWorstInjuryPvMMS" ~ "Possible vs. Mild/Moderate/Severe TBI",
    term == "osuWorstInjuryMvMS" ~ "Mild vs. Moderate/Severe TBI",
    TRUE ~ as.character(term)
    )
  ) %>%
  select(
    Model, Term = term, b = estimate,  LL = conf.low, UL = conf.high,
    SE = std.error, t = statistic, p = p.value, 
  )

# Writes out to CSV for paper
write_csv(all_models_tidy, file = "../output/paper-table-reg-mods.csv")


# HSCT Descriptive tables ----

# Hayling Data
hsct_desc <- 
  demo_neuropsych_clean %>%
  mutate(
    c1 = ifelse(osuWorstInjury == "Control", "Control", "TBI"),
    c2 = case_when(
      osuWorstInjury == "Control" ~ "Do Not Include",
      osuWorstInjury == "Possible" ~ "Possible",
      osuWorstInjury == "Mild" ~ "Mild/Moderate/Severe",
      osuWorstInjury == "Mod/Severe" ~ "Mild/Moderate/Severe",
      TRUE ~ as.character(osuWorstInjury)
    ),
    c3 = case_when(
      osuWorstInjury %in% c("Control", "Possible") ~ "Do Not Include",
      TRUE ~ as.character(osuWorstInjury)
    )
  ) %>%
  select(DODID, c1:c3, sc.time.raw:hsct.scaled)

# WF + CD Data
wf_data_wide <- 
  wf_data %>% 
  select(DODID, section, m_wf) %>% 
  spread(key = section, value = m_wf) %>%
  rename(wf_sc = SC, wf_uc = UC)

cd_data_wide <- 
  wf_data %>% 
  select(DODID, section, m_cd) %>% 
  spread(key = section, value = m_cd) %>%
  rename(cd_sc = SC, cd_uc = UC)

# LSA Data
lsa_data_wide <- 
  lsa_data_ss %>% 
  select(DODID, section, mLSA) %>% 
  spread(section, mLSA) %>%
  rename(lsa_sc = SC, lsa_uc = UC)

# Combining data
hsct_desc_data <- 
  left_join(hsct_desc, wf_data_wide, by = "DODID") %>%
  left_join(., cd_data_wide, by = "DODID") %>%
  left_join(., lsa_data_wide, by = "DODID")

# Contrast 1 - Controls vs. TBI
c1_desc <- 
  hsct_desc_data %>%
  select(-c2, -c3) %>%
  gather(meas, value, -DODID, -c1) %>%
  group_by(c1, meas) %>%
  summarise(m = mean(value), sd = sd(value), n = n(), sem = sd/sqrt(n)) %>%
  ungroup() %>%
  mutate(contrast = "c1") %>%
  rename(group = c1)

# Contrast 2 - Possible vs. Mild/Moderate/Severe TBI
c2_desc <- 
  hsct_desc_data %>%
  select(-c1, -c3) %>%
  gather(meas, value, -DODID, -c2) %>%
  group_by(c2, meas) %>%
  summarise(m = mean(value), sd = sd(value), n = n(), sem = sd/sqrt(n)) %>%
  ungroup() %>%
  mutate(contrast = "c2") %>%
  rename(group = c2)

# Contrast 3 - Mild vs. Moderate/Severe TBI
c3_desc <- 
  hsct_desc_data %>%
  select(-c1, -c2) %>%
  gather(meas, value, -DODID, -c3) %>%
  group_by(c3, meas) %>%
  summarise(m = mean(value), sd = sd(value), n = n(), sem = sd/sqrt(n)) %>%
  ungroup() %>%
  mutate(contrast = "c3") %>%
  rename(group = c3)

# Now clean up the names and drop uncessary info from the table.
hayling_desc_table <- 
  bind_rows(c1_desc, c2_desc, c3_desc) %>%
  filter(group %nin% c("Do Not Include")) %>%
  filter(meas %in% c(
    "sc.time.raw", "uc.time.raw",
    "cat.a.errors", "cat.b.errors", "hsct.scaled", 
    "lsa_sc", "lsa_uc", 
    "wf_sc", "wf_uc", 
    "cd_sc", "cd_uc"
    )
  ) %>%
  mutate(
    meas = case_when(
      meas == "sc.time.raw" ~ "RT_Sensible",
      meas == "uc.time.raw" ~ "RT_Unconnected",
      meas == "cat.a.errors" ~ "Category A Errors_Unconnected",
      meas == "cat.b.errors" ~ "Category B Errors_Unconnected",
      meas == "lsa_sc" ~ "LSA_Sensible",
      meas == "lsa_uc" ~ "LSA_Unconnected",
      meas == "wf_sc" ~ "Word Frequency_Sensible",
      meas == "wf_uc" ~ "Word Frequency_Unconnected",
      meas == "cd_sc" ~ "Contextual Diversity_Sensible",
      meas == "cd_uc" ~ "Contextual Diversity_Unconnected",
      meas == "hsct.scaled" ~ "Hayling Scaled Score_Total"
    )
  ) %>%
  separate(meas, into = c("Measure", "Section"), sep = "_")

# writes out to csv
write_csv(hayling_desc_table, path = "../output/paper-table-hsct-desc.csv")



# Andrew's Demographic Table ----
# Creating the tables
# Andrew

# Row names
demo_row_names <- c("Vocab", "Similarities", "Matrix", "Full2", "Age", "Male", "Female", "Asian",
                    "Black or African American", "Hispanic, spanish, or Latino", "Mixed Race", "White")
# Hayling row names
hayling_row_names <- c("sc.time.raw", "sc.time.scaled", "sc.wf", "sc.cd", "uc.time.raw", "uc.time.scaled",
                       "uc.wf", "uc.cd", "cat.a.errors", "cat.b.errors")

# Column names
col_names <- c("control m(sd)", "control N", "tbi m(sd)", "tbi N", "possible m(sd)", "possible N", "mild/mod/sev m(sd)",
               "mild/mod/sev N", "mild m(sd)", "mild N", "mod/sev m(sd)", "mod/sev N")

# Prepare a table with all of the participants and the relevant measures
contrast_table <- left_join(
  full_join(
    lsa_data_ss %>%
      select(DODID, section, wasi.vocab.rscore, wasi.similarities.rscore, wasi.matrix.rscore, wasi.full2subtest,
             sc.time.raw, sc.time.scaled, uc.time.raw, uc.time.scaled, cat.a.errors, cat.b.errors),
    rbind(
      dod_pre %>%
        select(DODID = dodID, sex, race, age) %>%
        left_join(demo_neuropsych_clean %>% select(DODID, osuWorstInjury), .) %>%
        na.omit(),
      control_demo_data %>%
        select(DODID = ID, sex = Sex, race=Race, age=Age) %>%
        left_join(demo_neuropsych_clean %>% select(DODID, osuWorstInjury), .) %>%
        na.omit()
    )
  ),
  wf_data %>%
    select(DODID, section, m_wf, m_cd)
) %>%
  mutate(age = as.numeric(age))

contrast_table <- left_join(
  contrast_table %>%
    filter(section == "UC") %>%
    select(DODID, uc.wf = m_wf, uc.cd = m_cd),
  contrast_table
) %>%
  filter(section == "SC") %>%
  mutate(sc.wf = m_wf, m_wf = NULL, sc.cd = m_cd, m_cd = NULL)

# Get the vocab, similarities, matrix, full2, and age for the controls
c1.a <- contrast_table %>% 
  filter(osuWorstInjury == "Control")
c1.b <- contrast_table %>% 
  filter(osuWorstInjury %in% c("Mild","Mod/Severe","Possible"))
c2.a <- contrast_table %>% 
  filter(osuWorstInjury == "Possible")
c2.b <- contrast_table %>% 
  filter(osuWorstInjury %in% c("Mild","Mod/Severe"))
c3.a <- contrast_table %>% 
  filter(osuWorstInjury == "Mild")
c3.b <- contrast_table %>% 
  filter(osuWorstInjury == "Mod/Severe")

# Fill out the values for the first contrast
c1 <- full_join(rbind(
  c1.a %>%
    summarise(vocab = mean(wasi.vocab.rscore),
              similarities = mean(wasi.similarities.rscore),
              matrix = mean(wasi.matrix.rscore),
              full2 = mean(wasi.full2subtest),
              age = mean(age),
              sc.time.raw = mean(sc.time.raw),
              sc.time.scaled = mean(sc.time.scaled),
              uc.time.raw = mean(uc.time.raw),
              uc.time.scaled = mean(uc.time.scaled),
              cat.a.errors = mean(cat.a.errors),
              cat.b.errors = mean(cat.b.errors)) %>%
    gather(measure, controls),
  c1.a %>%
    group_by(sex) %>%
    summarise(controls = n()) %>%
    mutate(measure = sex, sex = NULL)
),
rbind(c1.b %>%
        summarise(vocab = mean(wasi.vocab.rscore),
                  similarities = mean(wasi.similarities.rscore),
                  matrix = mean(wasi.matrix.rscore),
                  full2 = mean(wasi.full2subtest),
                  age = mean(age),
                  sc.time.raw = mean(sc.time.raw),
                  sc.time.scaled = mean(sc.time.scaled),
                  uc.time.raw = mean(uc.time.raw),
                  uc.time.scaled = mean(uc.time.scaled),
                  cat.a.errors = mean(cat.a.errors),
                  cat.b.errors = mean(cat.b.errors)) %>%
        gather(measure, tbi),
      c1.b %>%
        group_by(sex) %>%
        summarise(tbi = n()) %>%
        mutate(measure = sex, sex = NULL)
)
)

# Fill out the values for the second contrast
c2 <- full_join(
  rbind(c2.a %>%
          summarise(vocab = mean(wasi.vocab.rscore),
                    similarities = mean(wasi.similarities.rscore),
                    matrix = mean(wasi.matrix.rscore),
                    full2 = mean(wasi.full2subtest),
                    age = mean(age),
                    sc.time.raw = mean(sc.time.raw),
                    sc.time.scaled = mean(sc.time.scaled),
                    uc.time.raw = mean(uc.time.raw),
                    uc.time.scaled = mean(uc.time.scaled),
                    cat.a.errors = mean(cat.a.errors),
                    cat.b.errors = mean(cat.b.errors)) %>%
          gather(measure, possible),
        c2.a %>%
          group_by(sex) %>%
          summarise(possible = n()) %>%
          mutate(measure = sex, sex = NULL)
  ),
  rbind(c2.b %>%
          summarise(vocab = mean(wasi.vocab.rscore),
                    similarities = mean(wasi.similarities.rscore),
                    matrix = mean(wasi.matrix.rscore),
                    full2 = mean(wasi.full2subtest),
                    age = mean(age),
                    sc.time.raw = mean(sc.time.raw),
                    sc.time.scaled = mean(sc.time.scaled),
                    uc.time.raw = mean(uc.time.raw),
                    uc.time.scaled = mean(uc.time.scaled),
                    cat.a.errors = mean(cat.a.errors),
                    cat.b.errors = mean(cat.b.errors)) %>%
          gather(measure, mild.mod.sev),
        c2.b %>%
          group_by(sex) %>%
          summarise(mild.mod.sev = n()) %>%
          mutate(measure = sex, sex = NULL)
  )
)

# Fill out the values for the third contrast
c3 <- full_join(
  rbind(c3.a %>%
          summarise(vocab = mean(wasi.vocab.rscore),
                    similarities = mean(wasi.similarities.rscore),
                    matrix = mean(wasi.matrix.rscore),
                    full2 = mean(wasi.full2subtest),
                    age = mean(age),
                    sc.time.raw = mean(sc.time.raw),
                    sc.time.scaled = mean(sc.time.scaled),
                    uc.time.raw = mean(uc.time.raw),
                    uc.time.scaled = mean(uc.time.scaled),
                    cat.a.errors = mean(cat.a.errors),
                    cat.b.errors = mean(cat.b.errors)) %>%
          gather(measure, mild),
        c3.a %>%
          group_by(sex) %>%
          summarise(mild = n()) %>%
          mutate(measure = sex, sex = NULL)
  ),
  rbind(c3.b %>%
          summarise(vocab = mean(wasi.vocab.rscore),
                    similarities = mean(wasi.similarities.rscore),
                    matrix = mean(wasi.matrix.rscore),
                    full2 = mean(wasi.full2subtest),
                    age = mean(age),
                    sc.time.raw = mean(sc.time.raw),
                    sc.time.scaled = mean(sc.time.scaled),
                    uc.time.raw = mean(uc.time.raw),
                    uc.time.scaled = mean(uc.time.scaled),
                    cat.a.errors = mean(cat.a.errors),
                    cat.b.errors = mean(cat.b.errors)) %>%
          gather(measure, mod.sev),
        c3.b %>%
          group_by(sex) %>%
          summarise(mod.sev = n()) %>%
          mutate(measure = sex, sex = NULL)
  )
)

demographics_contrast_table <- full_join(c1, full_join(c2, c3))

# saves out demographic table
write_csv(demographics_contrast_table, file = "../output/paper-table-demo.csv")

# Missing responses in analysis ----
# Andrew Vaughn

# Missing responses in analysis
# Andrew Vaughn

wf_na_data <- 
  wf_data %>%
  filter(DODID %in% unique(demo_neuropsych_long$DODID)) %>%
  group_by(subtlex.NA, section) %>%
  summarise(n = n()) %>%
  mutate(total_na = subtlex.NA * n)

lsa_na_data <- 
  lsa_data %>%
  filter(DODID %in% unique(demo_neuropsych_long$DODID)) %>%
  group_by(DODID, section) %>%
  summarise(missing_words = sum(is.na(lsa))) %>%
  group_by(missing_words, section) %>%
  summarise(n = n()) %>%
  mutate(total_na = missing_words * n)


sum(wf_na_data$total_na) # Number of responses not found by subtlex
sum(lsa_na_data$total_na) # Number of responses not found with LSA



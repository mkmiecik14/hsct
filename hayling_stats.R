# Imports ----
library(tidyr)
library(tidyverse)
library(LSAfun)
library(dplyr)
library(ggplot2)
library(readxl)

# Loads the hsct data and hayling sentences into R and removes rows with no data entered ----
hsctData <- read_excel('raw-data/hsct-data-updated.xlsx') %>%
  filter(sc.time.raw != '')

hsctSentences <- read_excel('hsct_sentences.xlsx') %>%
  gather(sentence, section)

# Load a semantic space for LSA ----
load(url('http://www.lingexp.uni-tuebingen.de/z2/LSAspaces/EN_100k.rda'))

# Remove unwanted columns from the data ----
hsctData <- hsctData %>%
  mutate(join = NULL, sc.time.raw = NULL, sc.time.scaled = NULL, uc.time.raw = NULL, uc.time.scaled = NULL,
         cat.a.errors = NULL, a.score = NULL, cat.b.errors = NULL, b.score = NULL, ab.score = NULL,
         errors.scaled = NULL, total.scaled = NULL, hsct.scaled = NULL)

# Function for getting the average multicostring ----
mean_multicos <- function(a, b)
{
  mean_mc <- mean(multicostring(a, b, tvectors = EN_100k), na.rm = TRUE)
  # Make sure NA values are represented correctly
  if(mean_mc == "NaN")
    return(NA)
  else
    return(mean_mc)
}

# Make a joined table with the sentences and participant responses
hsctResponses <- full_join(hsctData %>%
                             gather(section, response, -ssID), hsctSentences)

# Create table with the LSA value of each word/sentence pairing ----
lsaScored <- hsctResponses %>%
  separate("section", c("section", "number"), sep = ".word.", extra = "drop") %>%
  rowwise() %>%
  mutate(mean.multicos = mean_multicos(sentence, response)) %>%
  filter(mean.multicos.is.null)

# LSA test function ----
multicostring('The captain wanted to stay with the sinking', 'ship', tvectors = EN_100k)

# Plot the difference between sections ----
groupedLSA <- lsaScored %>%
  mutate(number = NULL, word = NULL, response = NULL, sentence = NULL) %>%
  aggregate(list(.$ssID, .$section), FUN = "mean") %>%
  mutate(ssID = Group.1, section = Group.2, Group.1 = NULL, Group.2 = NULL)

ggplot(data = groupedLSA, mapping = aes(x = section, y = mean.multicos)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")


linMod <- lm(mean.uc ~ mean.sc, data = hsctScoredClean)


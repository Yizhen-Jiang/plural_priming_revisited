# This document consists of leftover code from "priming task.R" 
# and is only used to keep a record of Yizhen's wandering thoughts.


# Require packages
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggtext)
library(glue)
library(ggsignif)


# Load raw data
demographics <- read.csv("raw_demographics.csv")
primingtask <- read.csv("raw_primingtask.csv")
itemlist <- read.csv("item_list_with_expected_response_info.csv")


# Observe raw data
glimpse(demographics)
glimpse(primingtask)


# Organize raw data into
# data.info: selected info 
#            expected response info 
#            accuracy.prime1, accuracy.prime2, accuracy.filler
#            distributive.target (distributive: 1; cumulative: 0)


data.info <- primingtask %>% 
# Select responses: 75 (participants) * 224 (trials) = 16,800  
  select(Schedule.ID, Spreadsheet.Row, Trial.Number,
         condition, trial.condition, Response, Reaction.Time) %>% 
  filter(Response != "") %>% 
  filter(trial.condition != "") %>% 
# Add expected response info
  left_join(itemlist, 
            by=c("Spreadsheet.Row", "condition", "trial.condition")) %>% 
  select(Schedule.ID, Spreadsheet.Row, Trial.Number,
         condition, trial.condition, Response, 
         expected.response.filler, expected.response.prime, 
         numeric.combination,
         Reaction.Time) %>% 
# Relabel condition levels
  mutate(condition = factor(condition,
                            levels = c("D", "C", "Control", ""),
                            labels = c("Distributive", "Cumulative", "Control", "Baseline"))) %>% 
# Code accuracy.prime as 1 if responses are correct
  mutate(accuracy.prime = case_when(
    startsWith(trial.condition, "prime") & 
      Response == expected.response.prime ~ 1,
    startsWith(trial.condition, "prime") & 
      Response != expected.response.prime ~ 0
  )) %>% 
# Code accuracy.filler as 1 if responses are correct
  mutate(accuracy.filler = case_when(
    trial.condition == "filler" & 
      Response == expected.response.filler ~ 1,
    trial.condition == "filler" & 
      Response != expected.response.filler ~ 0
  )) %>% 
# Code distributive.target as 1 if responses are distributive
  mutate(distributive.target = case_when(
    endsWith(Response, "D.png") ~ 1,
    endsWith(Response, "C.png") ~ 0
  ))


# Tried to code accuracy.twoprimes for targets = sum of accuracy.prime of two preceding primes
# The following code didn't work. 
triplet <-
  data.info %>% 
  select(Schedule.ID, Trial.Number,
         condition, trial.condition, 
         Response, accuracy.prime,
         Reaction.Time,
         Spreadsheet.Row) %>% 
  filter(condition != "Baseline") %>% 
  filter(trial.condition != "filler")

Schedule.ID <- c()
Spreadsheet.Row <- c()
accuracy.twoprimes <-c()

for (j in unique(triplet$Schedule.ID)){
  for (i in seq(from = 38, to = 179, by = 3)){ 
    Schedule.ID <- 
      append(Schedule.ID, paste(triplet[triplet$Schedule.ID==j,]$Schedule.ID))
    Spreadsheet.Row <- 
      append(Spreadsheet.Row, paste(triplet[triplet$Schedule.ID==j,]$Speadsheet.Row))
    while(i %% 3 == 1){
      accuracy.twoprimes <- append(accuracy.twoprimes,
                                   sum(triplet[triplet$Schedule.ID==j & triplet$Spreadsheet.Row==i-2, ]$accuracy.prime1,
                                       triplet[triplet$Schedule.ID==j & triplet$Spreadsheet.Row==i-1, ]$accuracy.prime2))
    } 
  }}

triplet.frame <- data.frame(
  Schedule.ID = Schedule.ID, 
  Spreadsheet.Row = Spreadsheet.Row,
  accuracy.twoprimes = accuracy.twoprimes)


# Animation
library(gganimate)

MyData %>% 
  select(Schedule.ID, condition, trial.condition, distributive.target, accuracy.twoprimes, participant.group) %>% 
  filter(trial.condition == "target") %>% 
  filter(accuracy.twoprimes == 2 | 
           condition == "Control" | 
           condition == "Baseline") %>% 
  filter(participant.group != "Baseline = 50%") %>% 
  group_by(Schedule.ID, condition, participant.group) %>% 
  summarise(Distributive.response = mean(distributive.target*100)) %>%
  # Basic plot  
  ggplot(aes(x = Distributive.response, fill = condition, color = condition)) +
  geom_density()+
  xlab("Distributive choice (%)") +
  ylab("Density") + 
  
  transition_states(condition,
                    transition_length = 2,
                    state_length = 4) +
  enter_fade() +
  exit_shrink()


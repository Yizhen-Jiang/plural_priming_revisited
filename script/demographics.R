## @knitr demographics

# This script contains the analyses of demographic information.

# Require packages

library("tidyverse")
library("here")

# Load and organize data

demographics <- read.csv(here("raw_data", "raw_demographics.csv"))

# Question.Key info (the names of Question.Key are the mutated ones)

#   age and gender
#     age: What is your age?
#     gender: How do you identify yourself regarding your gender?
#             female = 1, male = 2, other = 3

#   Yes/No Questions: Y = 1, N = 2
#     native.language: Are you a native speaker of English?
#     language1: Did you live in the United Kingdom or in the United States from birth until (at least) age 13?
#     language2: Did your parents speak English to you during those years?
#     language3: Do you speak a language other than English?

data <- demographics %>% 
# Select relevant info 
  select(Schedule.ID, Question.Key, Response) %>% 
  filter(Question.Key == "age" | 	
           Question.Key == "gender-quantised" |
           Question.Key == "native-language-quantised" |
           Question.Key == "language1-quantised" |
           Question.Key == "language2-quantised" |
           Question.Key == "language-3-quantised") %>%
  pivot_wider(
    names_from = Question.Key,
    values_from = Response) %>% 
# Rename variables and change variable types
  mutate(age = as.numeric(age),
         gender.quantised = as.numeric(`gender-quantised`),
         native.language = as.numeric(`native-language-quantised`),
         language1 = as.numeric(`language1-quantised`),
         language2 = as.numeric(`language2-quantised`),
         language3 = as.numeric(`language-3-quantised`),
         .keep = c("unused")) # retain only the columns not used in mutation
 
demographics.summary <- data %>%  
  summarise(mean.age = mean(age),
            min.age = min(age),
            max.age = max(age),
            female.number = sum(gender.quantised == 1),
            male.number = sum(gender.quantised == 2),
            other.number = sum(gender.quantised == 3),
            prop.native.language = mean(native.language == 1)*100,
            prop.language1 = mean(language1 == 1)*100,
            prop.language2 = mean(language2 == 1)*100,
            prop.language1or2 = mean(language1 == 1 | language2 == 1)*100,
            prop.language3 = mean(language3 == 1)*100
            )%>%   
  t() # rotate the data frame

demographics.summary

# Participant exclusion 

# Exclude participants whose 
# native.language = 2

# Excluded zero participant

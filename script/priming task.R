## @knitr Load packages

# Require packages
library(here) # A Simpler Way to Find Your Files

library(tidyverse) # Easily Install and Load the 'Tidyverse' # Easily Install and Load the 'Tidyverse'
library(Hmisc) # Harrell Miscellaneous

library(glue) # Interpreted String Literals

library(gt) # Easily Create Presentation-Ready Display Tables
library(gtsummary) # Presentation-Ready Data Summary and Analytic Result Tables

library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggridges) # Ridgeline Plots in 'ggplot2' # Ridgeline Plots in 'ggplot2'
library(raincloudplots) # The easy way to create raincloud plots # The easy way to create raincloud plots

library(ggpmisc) # Miscellaneous Extensions to 'ggplot2'
library(ggpubr) # 'ggplot2' Based Publication Ready Plots
library(ggtext) # Improved Text Rendering Support for 'ggplot2'



# Define %!in% operator
`%!in%` <- Negate(`%in%`)



## @knitr Data processing

################ Organize raw data ################

# Load raw data
primingtask <- read.csv(here(
  "raw_data",
  "raw_primingtask.csv"
))
itemlist <- read.csv(here(
  "raw_data",
  "item_list_with_expected_response_info.csv"
))

# Observe raw data
glimpse(primingtask)

# Organize raw data into
# `data_info`: selected info
#            expected response info (from `item_list_with_expected_response_info.csv`)
#            accuracy.prime, accuracy.filler
#            distributive.target (distributive: 1; cumulative: 0)

data_info <- primingtask %>%
  # Select responses: 75 (participants) * 224 (trials) = 16,800
  select(
    Schedule.ID, Spreadsheet.Row, Trial.Number,
    condition, trial.condition, Response,
    Reaction.Time
  ) %>%
  filter(Response != "") %>%
  filter(trial.condition != "") %>%
  # Add expected response info
  left_join(itemlist,
    by = c("Spreadsheet.Row", "condition", "trial.condition")
  ) %>%
  select(
    Schedule.ID, Spreadsheet.Row, Trial.Number,
    condition, trial.condition, Response,
    expected.response.filler, expected.response.prime,
    numeric.combination,
    Reaction.Time
  ) %>%
  # Relabel condition levels
  mutate(condition = factor(condition,
    levels = c("", "D", "C", "Control"),
    labels = c(
      "Baseline", "Distributive",
      "Cumulative", "Control"
    )
  )) %>%
  # Rename "condition" column
  rename(Condition = condition) %>%
  # Code accuracy.prime as 1 when prime responses are correct
  mutate(accuracy.prime = case_when(
    startsWith(trial.condition, "prime") &
      Response == expected.response.prime ~ 1,
    startsWith(trial.condition, "prime") &
      Response != expected.response.prime ~ 0
  )) %>%
  # Code accuracy.filler as 1 when filler responses are correct
  mutate(accuracy.filler = case_when(
    trial.condition == "filler" &
      Response == expected.response.filler ~ 1,
    trial.condition == "filler" &
      Response != expected.response.filler ~ 0
  )) %>%
  # Code distributive.target as 1 when responses are distributive
  mutate(distributive.target = case_when(
    trial.condition == "target" & endsWith(Response, "D.png") ~ 1,
    trial.condition == "target" & endsWith(Response, "C.png") ~ 0
  ))


# Code accuracy.twoprimes for targets in the priming block
# = sum of accuracy.prime of the two preceding primes

for (j in unique(data_info$Schedule.ID)) {
  for (i in seq(40, 181, by = 3)) { # Spreadsheet.Row of 48 targets in Block 2
    data_info$accuracy.twoprimes[data_info$Schedule.ID == j & data_info$Spreadsheet.Row == i] <-
      data_info$accuracy.prime[data_info$Schedule.ID == j & data_info$Spreadsheet.Row == i - 1] +
      data_info$accuracy.prime[data_info$Schedule.ID == j & data_info$Spreadsheet.Row == i - 2]
  }
}


# Change variable type to factor
# Get `processed_data`
processed_data <- data_info %>%
  mutate(
    Schedule.ID = factor(Schedule.ID),
    Spreadsheet.Row = factor(Spreadsheet.Row),
    Condition = factor(Condition,
      levels = c(
        "Baseline", "Distributive",
        "Cumulative", "Control"
      )
    ),
    numeric.combination = factor(numeric.combination)
  )


# Export `processed_data`
write.csv(processed_data, here("processed_data", "processed_data.csv"))




# Completion time (minute)
# Completion time by participant
completiontime <- processed_data %>%
  group_by(Schedule.ID) %>%
  summarise(compeletion.time = sum(Reaction.Time) / 1000 / 60) %>%
  arrange(desc(compeletion.time))
completiontime

# Average completion time
mean(completiontime$compeletion.time)


# Participant device type
primingtask %>%
  group_by(Participant.Device.Type, Schedule.ID) %>%
  summarise() %>%
  arrange(desc(Participant.Device.Type))

unique(primingtask$Schedule.ID[primingtask$Participant.Device.Type != "computer"])
# Observed that 22123585 used mobile and 22124197 used tablet
# Hence, exclude 22123585 and 22124197


# Check response time per trial

# Select trials of which response time > 5 mins (upper limit)
processed_data %>%
  filter(Reaction.Time / 1000 / 60 > 5)

# Observed that 22123587's reaction time in trial #161 (prime 1)
# was 1700599 ms (about 28 mins)
# Exclude 22123587 or the triplet containing #161 ?

# Check 22123587's accuracy rates of primes and fillers

# Primes
processed_data %>%
  filter(Schedule.ID == 22123587) %>%
  filter(startsWith(trial.condition, "prime") == 1) %>%
  group_by(Condition) %>%
  summarise(accuracy.rate.prime = mean(accuracy.prime) * 100)

# Fillers
processed_data %>%
  filter(Schedule.ID == 22123587) %>%
  filter(trial.condition == "filler") %>%
  group_by(Condition) %>%
  summarise(accuracy.rate.filler = mean(accuracy.filler) * 100)

# No action wrt 22123587 as:
# trial #161 (prime 1 & distributive) received a correct response
# and 22123587 performed well in prime and filler trials (accuracy > 80%)


# Select trials whose response time < 500 ms (lower limit)
processed_data %>%
  filter(Reaction.Time < 500)

# Exclude individual target trials with response time < 500 ms ?
# as in Meyer & Feiman (2021)

# Noticed 28 out of 41 (about 68%) target trials whose response time < 500 ms
# had at least one incorrect response on the previous two priming trials
# and hence will be removed in the following steps.

# Hence, no action wrt the target trials whose response time < 500 ms


# Check participants' mean response time per trial
# Observed that one participant's (22124018) mean response time < 1 s
mean_rt_per_trial <-
  processed_data %>%
  group_by(Schedule.ID) %>%
  summarise(mean = mean(Reaction.Time)) %>%
  arrange(mean)
mean_rt_per_trial

# Observed 22124018's accuracy rates of primes and fillers
# were quite low

# Primes
processed_data %>%
  filter(Schedule.ID == 22124018) %>%
  filter(startsWith(trial.condition, "prime") == 1) %>%
  #  group_by(Condition) %>%
  summarise(accuracy.rate.prime = mean(accuracy.prime) * 100)

# Fillers
processed_data %>%
  filter(Schedule.ID == 22124018) %>%
  filter(trial.condition == "filler") %>%
  #  group_by(Condition) %>%
  summarise(accuracy.rate.filler = mean(accuracy.filler) * 100)

# Hence, exclude 22124018 whose mean response time < 1 s
# following Maldonado et al. (2017)


# Mean accuracy rates of primes and fillers by participant
accuracy_rate_prime_filler <- processed_data %>%
  filter(startsWith(trial.condition, "prime") == 1 |
    trial.condition == "filler") %>%
  mutate(accuracy.prime.filler = case_when(
    startsWith(trial.condition, "prime") == 1 ~ accuracy.prime,
    trial.condition == "filler" ~ accuracy.filler
  )) %>%
  group_by(Schedule.ID) %>%
  summarise(accuracy.rate.prime.filler = mean(accuracy.prime.filler) * 100) %>%
  arrange(accuracy.rate.prime.filler)
accuracy_rate_prime_filler

# Noted that accuracy rates of primes and fillers < 80% for the following subjects:
# 22124018 (is the participant whose mean per trial response time < 1s)
# 22123911
# Hence, exclude 22124018 and 22123911


# Mean accuracy rates of primes and fillers by participant
# and by prime/filler type

# Primes
processed_data %>%
  filter(startsWith(trial.condition, "prime") == 1) %>%
  group_by(Schedule.ID, Condition) %>%
  summarise(accuracy.rate.prime = mean(accuracy.prime) * 100)

# Fillers
processed_data %>%
  filter(trial.condition == "filler") %>%
  group_by(Schedule.ID, Condition) %>%
  summarise(accuracy.rate.filler = mean(accuracy.filler) * 100)

# No action wrt this



###### Participant exclusion ######

# `data` = `processed_data` excluding 4 participants ((75-4)*224 obs)
data <- processed_data %>%
  # Exclude 2 participants using mobile and tablet
  filter(Schedule.ID %!in%
    primingtask$Schedule.ID[primingtask$Participant.Device.Type != "computer"]) %>%
  # Exclude 1 participant whose mean response time per trial < 1 s
  filter(Schedule.ID %!in%
    mean_rt_per_trial$Schedule.ID[mean_rt_per_trial$mean < 1000]) %>%
  # Exclude 1 more participant whose accuracy rates of primes and fillers < 80%
  filter(Schedule.ID %!in%
    accuracy_rate_prime_filler$Schedule.ID[accuracy_rate_prime_filler$accuracy.rate.prime.filler < 80])



# After participant exclusion (75-4 = 71 participants)

# Mean accuracy rates of primes and fillers respectively
# after participant exclusion

# Mean accuracy rate of primes
data %>%
  filter(startsWith(trial.condition, "prime") == 1) %>%
  summarise(
    mean.accuracy.prime = mean(accuracy.prime) * 100,
    lower.CI = binconf(sum(accuracy.prime), length(accuracy.prime))[2] * 100,
    upper.CI = binconf(sum(accuracy.prime), length(accuracy.prime))[3] * 100
  ) %>%
  mutate_if(is.numeric, round, 1)

# Mean accuracy rate of fillers
data %>%
  filter(trial.condition == "filler") %>%
  summarise(
    mean.accuracy.filler = mean(accuracy.filler) * 100,
    lower.CI = binconf(sum(accuracy.filler), length(accuracy.filler))[2] * 100,
    upper.CI = binconf(sum(accuracy.filler), length(accuracy.filler))[3] * 100
  ) %>%
  mutate_if(is.numeric, round, 1)


# Mean accuracy rates of primes (Distributive, Cumulative, Control)
mean_accuracy_prime <- data %>%
  filter(startsWith(trial.condition, "prime") == 1) %>%
  group_by(Condition) %>%
  summarise(
    mean.accuracy.prime = mean(accuracy.prime) * 100,
    lower.CI = binconf(sum(accuracy.prime), length(accuracy.prime))[2] * 100,
    upper.CI = binconf(sum(accuracy.prime), length(accuracy.prime))[3] * 100
  )
mean_accuracy_prime


# Table `t_accuracy_prime` for report
# Mean accuracy rates of primes (Distributive, Cumulative, Control)
t_accuracy_prime <- mean_accuracy_prime %>%
  mutate_if(is.numeric, round, 1) %>%
  gt() %>%
  cols_label(
    mean.accuracy.prime = md("Mean accuracy (%)"),
    lower.CI = md("Lower"), upper.CI = md("Upper")
  ) %>%
  cols_align(
    align = "left",
    columns = Condition
  ) %>%
  cols_align(
    align = "center",
    columns = c(mean.accuracy.prime, lower.CI, upper.CI)
  ) %>%
  tab_spanner(label = "95%CI", columns = c(lower.CI, upper.CI)) %>%
  tab_header(
    title = "Mean accuracy rates of prime trials"
  )
t_accuracy_prime

t_accuracy_prime %>%
  gtsave(here(
    "report",
    "Mean accuracy rates of prime trials by Condition.png"
  ))


# Mean accuracy rates of fillers (Distributive, Cumulative, Baseline)
mean_accuracy_filler <- data %>%
  filter(trial.condition == "filler") %>%
  mutate(Condition = factor(Condition,
    levels = c("Distributive", "Cumulative", "Baseline")
  )) %>%
  group_by(Condition) %>%
  summarise(
    mean.accuracy.filler = mean(accuracy.filler) * 100,
    lower.CI = binconf(sum(accuracy.filler), length(accuracy.filler))[2] * 100,
    upper.CI = binconf(sum(accuracy.filler), length(accuracy.filler))[3] * 100
  )
mean_accuracy_filler


# Table `t_accuracy_filler` for report
# Mean accuracy rates of fillers (Distributive, Cumulative, Baseline)
t_accuracy_filler <- mean_accuracy_filler %>%
  mutate_if(is.numeric, round, 1) %>%
  gt() %>%
  cols_label(
    mean.accuracy.filler = md("Mean accuracy (%)"),
    lower.CI = md("Lower"), upper.CI = md("Upper")
  ) %>%
  cols_align(
    align = "left",
    columns = Condition
  ) %>%
  cols_align(
    align = "center",
    columns = c(mean.accuracy.filler, lower.CI, upper.CI)
  ) %>%
  tab_spanner(label = "95%CI", columns = c(lower.CI, upper.CI)) %>%
  tab_header(
    title = "Mean accuracy rates of filler trials"
  )
t_accuracy_filler

t_accuracy_filler %>%
  gtsave(here(
    "report",
    "Mean accuracy rates of filler trials by Condition.png"
  ))


# `target_response`: target responses whose accuracy.twoprimes == 2
target_response <- data %>%
  select(
    Schedule.ID, Condition, trial.condition,
    distributive.target, accuracy.twoprimes,
    numeric.combination
  ) %>%
  filter(trial.condition == "target") %>%
  # Exclude accuracy.twoprimes !=2 (in Distributive/Cumulative/Control conditions in Block 2)
  # Applied to Control condition for consistency
  filter(accuracy.twoprimes == 2 |
    Condition == "Baseline")


# `rate`: Rate of Distributive choice (%) (distributive.rate) by subject and condition
# N = 71 (75-4)

rate <- target_response %>%
  group_by(Schedule.ID, Condition) %>%
  summarise(distributive.rate = mean(distributive.target) * 100)
rate


# Calculate the number/proportion of the targets that were removed in the previous step
# due to the incorrect responses on the two preceding prime trials
removed_target <- data %>%
  filter(Condition != "Baseline") %>%
  filter(trial.condition == "target") %>%
  filter(accuracy.twoprimes != 2) %>%
  group_by(Condition) %>%
  summarise(
    number = n(),
    proportion.target.B2 = (number / (length(unique(data$Schedule.ID)) * 48)) * 100,
    proportion.target.B12 = (number / (length(unique(data$Schedule.ID)) * (48 + 16))) * 100,
    proportion.whole.data = (number / (length(unique(data$Schedule.ID)) * 224)) * 100
  )

removed_target %>%
  mutate_if(is.numeric, round, 1)



################ Preliminary analysis ################

# Unimodality test for baseline preferences

library("diptest") # Hartigan's Dip Test Statistic for Unimodality - Corrected
library("multimode") # Mode Testing and Exploring

baseline_rate <- rate %>%
  filter(Condition == "Baseline")

dip.test(baseline_rate$distributive.rate, simulate.p.value = FALSE, B = 5000)
locmodes(baseline_rate$distributive.rate, mod0 = 2, display = TRUE)

# Save plot "Kernel density estimation.png"
png(here("report", "Kernel density estimation.png"))
locmodes(baseline_rate$distributive.rate, mod0 = 2, display = TRUE)
dev.off()


# Classify participants into three groups (Baseline >, =, < 50%)
# based on distributive.rate in the Baseline conditions

# `target_response_group` = `target_response` + `responder.group`
target_response_group <- target_response %>%
  # Add responder.group column
  mutate(responder.group = case_when(
    Schedule.ID %in% rate$Schedule.ID[rate$Condition == "Baseline" & rate$distributive.rate > 50] == 1
    ~ "Baseline > 50%",
    Schedule.ID %in% rate$Schedule.ID[rate$Condition == "Baseline" & rate$distributive.rate < 50] == 1
    ~ "Baseline < 50%",
    Schedule.ID %in% rate$Schedule.ID[rate$Condition == "Baseline" & rate$distributive.rate == 50] == 1
    ~ "Baseline = 50%"
  )) %>%
  # Change responder.group to factor
  mutate(responder.group = factor(responder.group,
    levels = c(
      "Baseline < 50%",
      "Baseline = 50%",
      "Baseline > 50%"
    )
  ))

# `target_response_twogroups` = `target_response` without participants whose Baseline = 50%
target_response_twogroups <- target_response_group %>%
  filter(responder.group != "Baseline = 50%")
target_response_twogroups

# `rate_group` = `rate` + `responder.group`
rate_group <- rate %>%
  # Add responder.group column
  mutate(responder.group = case_when(
    Schedule.ID %in% rate$Schedule.ID[rate$Condition == "Baseline" & rate$distributive.rate > 50] == 1
    ~ "Baseline > 50%",
    Schedule.ID %in% rate$Schedule.ID[rate$Condition == "Baseline" & rate$distributive.rate < 50] == 1
    ~ "Baseline < 50%",
    Schedule.ID %in% rate$Schedule.ID[rate$Condition == "Baseline" & rate$distributive.rate == 50] == 1
    ~ "Baseline = 50%"
  )) %>%
  # Change responder.group to factor
  mutate(responder.group = factor(responder.group,
    levels = c(
      "Baseline < 50%",
      "Baseline = 50%",
      "Baseline > 50%"
    )
  ))

# `rate_twogroups` = `rate` without participants whose Baseline = 50%
rate_twogroups <- rate_group %>%
  filter(responder.group != "Baseline = 50%")
rate_twogroups


# `MyData` = `data` + `responder.group` (71 * 224 obs)
# It contains all the info we are interested in.
MyData <- rate_group %>%
  # Merge with data
  right_join(data, by = c("Schedule.ID", "Condition"))


# Number of participants in each group
participant_group_summary <- MyData %>%
  group_by(responder.group) %>%
  summarise(
    number = length(unique(Schedule.ID)),
    proportion = (number / length(unique(MyData$Schedule.ID))) * 100
  )
participant_group_summary


# Number of participants in each group
# (used in data viz)
total_n <- participant_group_summary %>%
  pull(number) %>%
  sum()
distributive_n <- participant_group_summary %>%
  filter(responder.group == "Baseline > 50%") %>%
  pull(number)
cumulative_n <- participant_group_summary %>%
  filter(responder.group == "Baseline < 50%") %>%
  pull(number)
neutral_n <- participant_group_summary %>%
  filter(responder.group == "Baseline = 50%") %>%
  pull(number)

# 4 participants whose Baseline = 50%
# 22123849 22124053 22124282 22124283
unique(MyData$Schedule.ID[MyData$responder.group == "Baseline = 50%"])



# Distributive choice (%) by Condition
# `summary_condition` summarizes means, medians, lowerCIs, upperCIs
# of Distributive choice
# by Condition

summary_condition <- target_response %>%
  group_by(Condition) %>%
  summarise(
    mean = mean(distributive.target) * 100,
    lower.CI = binconf(sum(distributive.target), length(distributive.target))[2] * 100,
    upper.CI = binconf(sum(distributive.target), length(distributive.target))[3] * 100
  ) %>%
  full_join(rate %>%
    group_by(Condition) %>%
    summarise(median = median(distributive.rate)),
  by = "Condition"
  )

# Table `t_mean_target` for report
t_mean_target <- summary_condition %>%
  mutate_if(is.numeric, round, 1) %>%
  gt() %>%
  cols_label(
    mean = md("Distributive choice (%)"),
    lower.CI = md("Lower"), upper.CI = md("Upper"),
    median = md("Median")
  ) %>%
  cols_align(
    align = "left",
    columns = Condition
  ) %>%
  cols_align(
    align = "center",
    columns = c(mean, lower.CI, upper.CI)
  ) %>%
  tab_spanner(label = "95%CI", columns = c(lower.CI, upper.CI)) %>%
  tab_header(
    title = "Mean proportions of Distributive choice on target trials"
  )
t_mean_target

t_mean_target %>%
  gtsave(here(
    "report",
    "Mean proportions of Distributive choice on target trials by Condition.png"
  ))


# Distributive choice (%) by Condition and Responder group
# `summary_twogroups` summarizes means, medians, lowerCIs, upperCIs
# of Distributive choice
# by Condition and Responder group (2 groups)

summary_twogroups <- target_response_twogroups %>%
  group_by(responder.group, Condition) %>%
  summarise(
    mean = mean(distributive.target) * 100,
    lower.CI = binconf(sum(distributive.target), length(distributive.target))[2] * 100,
    upper.CI = binconf(sum(distributive.target), length(distributive.target))[3] * 100
  ) %>%
  full_join(rate_twogroups %>%
    group_by(responder.group, Condition) %>%
    summarise(median = median(distributive.rate)),
  by = c("responder.group", "Condition")
  )

# Table `t_mean_target_twogroups` for report
t_mean_target_twogroups <- summary_twogroups %>%
  mutate_if(is.numeric, round, 1) %>%
  gt() %>%
  cols_label(
    mean = md("Distributive choice (%)"),
    lower.CI = md("Lower"), upper.CI = md("Upper"),
    median = md("Median")
  ) %>%
  cols_align(
    align = "left",
    columns = Condition
  ) %>%
  cols_align(
    align = "center",
    columns = c(mean, lower.CI, upper.CI)
  ) %>%
  tab_spanner(
    label = md("95%CI"),
    columns = c(lower.CI, upper.CI)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(),
      cells_column_spanners()
    )
  ) %>%
  tab_header(
    title = "Mean proportions of Distributive choice on target trials"
  )
t_mean_target_twogroups

t_mean_target_twogroups %>%
  gtsave(here(
    "report",
    "Mean proportions of Distributive choice on target trials by Responder group and Condition.png"
  ))




## @knitr Data analysis by Responder group

################ Data analysis by Responder group ################

# Prepare data
data_frame <- MyData %>%
  dplyr::select(
    Schedule.ID, Spreadsheet.Row,
    Condition, trial.condition,
    distributive.target, accuracy.twoprimes,
    responder.group
  ) %>%
  filter(trial.condition == "target") %>%
  filter(responder.group != "Baseline = 50%") %>%
  # Exclude accuracy.twoprimes !=2 (in Distributive/Cumulative/Control conditions in Block 2)
  # Applied to Control condition for consistency
  filter(accuracy.twoprimes == 2 |
    Condition == "Baseline") %>%
  rename(
    Response = distributive.target,
    SUBJECT = Schedule.ID,
    ITEM = Spreadsheet.Row
  ) %>%
  group_by(SUBJECT, Condition)


# Require packages
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4
library(lmerTest) # Tests in Linear Mixed Effects Models
library(car) # Companion to Applied Regression
library(MASS) # Support Functions and Datasets for Venables and Ripley's MASS
library(broom.mixed) # Tidying Methods for Mixed Models

library(tidyverse) # Easily Install and Load the 'Tidyverse' # Easily Install and Load the 'Tidyverse'



###### Cumulative responders: Baseline < 50% ######

# df.cumulative.1: Distributive as reference level
# Distributive vs Cumulative

df.cumulative.1 <- data_frame %>%
  filter(responder.group == "Baseline < 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Distributive", "Cumulative",
      "Baseline", "Control"
    )
  ))
contrasts(df.cumulative.1$Condition)


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# cmodel1.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.cumulative.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel1.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# cmodel1.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.cumulative.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel1.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# cmodel1.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.cumulative.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel1.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# cmodel1.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.cumulative.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel1.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

cmodel1.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.cumulative.1,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(cmodel1.5, tol = 1e-05)

Anova(cmodel1.5)
summary(cmodel1.5)

cDvs <- tidy(cmodel1.5)

cDvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

cmodel1.5 %>%
  tbl_regression()



# df.cumulative.2: Baseline as reference level
# Distributive vs Baseline; Cumulative vs Baseline
# Control vs Baseline


df.cumulative.2 <- data_frame %>%
  filter(responder.group == "Baseline < 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Baseline", "Distributive",
      "Cumulative", "Control"
    )
  ))
contrasts(df.cumulative.2$Condition)


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# cmodel2.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.cumulative.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel2.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# cmodel2.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.cumulative.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel2.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# cmodel2.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.cumulative.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel2.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# cmodel2.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.cumulative.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel2.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

cmodel2.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.cumulative.2,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(cmodel2.5, tol = 1e-05)

Anova(cmodel2.5)
summary(cmodel2.5)

cBvs <- tidy(cmodel2.5)

cBvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

cmodel2.5 %>%
  tbl_regression()



###### Distributive responders: Baseline > 50% ######

# df.distributive.1: Distributive as reference level
# Distributive vs Cumulative

df.distributive.1 <- data_frame %>%
  filter(responder.group == "Baseline > 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Distributive", "Cumulative",
      "Baseline", "Control"
    )
  ))
contrasts(df.distributive.1$Condition)


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# dmodel1.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.distributive.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel1.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# dmodel1.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.distributive.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel1.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# dmodel1.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.distributive.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel1.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# dmodel1.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.distributive.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel1.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

dmodel1.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.distributive.1,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)

isSingular(dmodel1.5, tol = 1e-05)
Anova(dmodel1.5)
summary(dmodel1.5)

dDvs <- tidy(dmodel1.5)

dDvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

dmodel1.5 %>%
  tbl_regression()



# df.distributive.2: Baseline as reference level
# Distributive vs Baseline; Cumulative vs Baseline
# Control vs Baseline

df.distributive.2 <- data_frame %>%
  filter(responder.group == "Baseline > 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Baseline", "Distributive",
      "Cumulative", "Control"
    )
  ))
contrasts(df.distributive.2$Condition)


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# dmodel2.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.distributive.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel2.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# dmodel2.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.distributive.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel2.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# dmodel2.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.distributive.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel2.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# dmodel2.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.distributive.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
#
# isSingular(dmodel2.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

dmodel2.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.distributive.2,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)

isSingular(dmodel2.5, tol = 1e-05)

Anova(dmodel2.5)
summary(dmodel2.5)

dBvs <- tidy(dmodel2.5)

dBvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

dmodel2.5 %>%
  tbl_regression()



# 8 comparisons (4 for each group: DvsC, BvsD, BvsC, BvsControl)
# Calculate adjusted p values
pvalue8 <- c(
  cDvs %>%
    filter(term == "ConditionCumulative") %>%
    pull(p.value),
  cBvs %>%
    filter(term == "ConditionDistributive" |
      term == "ConditionCumulative" |
      term == "ConditionControl") %>%
    pull(p.value),
  dDvs %>%
    filter(term == "ConditionCumulative") %>%
    pull(p.value),
  dBvs %>%
    filter(term == "ConditionDistributive" |
      term == "ConditionCumulative" |
      term == "ConditionControl") %>%
    pull(p.value)
)

values8 <- p.adjust(pvalue8, method = "bonferroni")
format(values8, scientific = FALSE)



###### GLMER model summary table (2*4 comparisons) ######

# GLMER model summary table for Cumulative responders
regression_c4 <- cDvs %>%
  filter(term == "ConditionCumulative") %>%
  rbind(cBvs %>% filter(term == "ConditionDistributive" |
    term == "ConditionCumulative" |
    term == "ConditionControl")) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    beta = estimate,
    S.E. = std.error,
    Z = statistic
  ) %>%
  mutate(Comparison = c(
    "Distributive vs. Cumulative",
    "Distributive vs. Baseline",
    "Cumulative vs. Baseline",
    "Control vs. Baseline"
  )) %>%
  dplyr::select(Comparison, beta, S.E., Z, p.value) %>%
  mutate(Adjusted = values8[1:4]) %>%
  mutate(p.symbol = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01 ~ "**",
    p.value <= 0.05 ~ "*",
    p.value > 0.05 ~ "ns"
  )) %>%
  mutate(Adjusted.symbol = case_when(
    Adjusted <= 0.001 ~ "***",
    Adjusted <= 0.01 ~ "**",
    Adjusted <= 0.05 ~ "*",
    Adjusted > 0.05 ~ "ns"
  )) %>%
  dplyr::select(
    Comparison, beta, S.E., Z,
    p.value, p.symbol,
    Adjusted, Adjusted.symbol
  )

t_regression_c4 <- regression_c4 %>%
  mutate_at(vars(p.value, Adjusted),
    format.pval,
    eps = .001, digits = 3
  ) %>%
  mutate_at(vars(beta, S.E., Z), round, 3) %>%
  gt() %>%
  cols_label(
    Z = md("*Z*"),
    p.value = md("*p*-value"),
    p.symbol = md(""),
    Adjusted.symbol = md("")
  ) %>%
  tab_header(title = "Cumulative responders")

t_regression_c4

t_regression_c4 %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for Cumulative responders (4 comparisons).png"
    )
  )





# GLMER model summary table for Distributive responders
regression_d4 <- dDvs %>%
  filter(term == "ConditionCumulative") %>%
  rbind(dBvs %>% filter(term == "ConditionDistributive" |
    term == "ConditionCumulative" |
    term == "ConditionControl")) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    beta = estimate,
    S.E. = std.error,
    Z = statistic
  ) %>%
  mutate(Comparison = c(
    "Distributive vs. Cumulative",
    "Distributive vs. Baseline",
    "Cumulative vs. Baseline",
    "Control vs. Baseline"
  )) %>%
  dplyr::select(Comparison, beta, S.E., Z, p.value) %>%
  mutate(Adjusted = values8[5:8]) %>%
  mutate(p.symbol = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01 ~ "**",
    p.value <= 0.05 ~ "*",
    p.value > 0.05 ~ "ns"
  )) %>%
  mutate(Adjusted.symbol = case_when(
    Adjusted <= 0.001 ~ "***",
    Adjusted <= 0.01 ~ "**",
    Adjusted <= 0.05 ~ "*",
    Adjusted > 0.05 ~ "ns"
  )) %>%
  dplyr::select(
    Comparison, beta, S.E., Z,
    p.value, p.symbol,
    Adjusted, Adjusted.symbol
  )

t_regression_d4 <- regression_d4 %>%
  mutate_at(vars(p.value, Adjusted),
    format.pval,
    eps = .001, digits = 3
  ) %>%
  mutate_at(vars(beta, S.E., Z), round, 3) %>%
  gt() %>%
  cols_label(
    Z = md("*Z*"),
    p.value = md("*p*-value"),
    p.symbol = md(""),
    Adjusted.symbol = md("")
  ) %>%
  tab_header(title = "Distributive responders")

t_regression_d4

t_regression_d4 %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for Distributive responders (4 comparisons).png"
    )
  )





## @knitr Data viz by Responder group

################ Data viz by Responder group (2*4 comparisons) ################


# Preparation


# Significance stars for data viz
Adjusted.symbol_c4 <- regression_c4 %>%
  pull(Adjusted.symbol)
Adjusted.symbol_d4 <- regression_d4 %>%
  pull(Adjusted.symbol)


# Responder group facet labels
responder.group.labs <- c(
  glue("Cumulative responders<br>N = {cumulative_n}"),
  glue("Neutral responders<br>N = {neutral_n}"),
  glue("Distributive responders<br>N = {distributive_n}")
)

names(responder.group.labs) <- c(
  "Baseline < 50%",
  "Baseline = 50%",
  "Baseline > 50%"
)

# Custom color
color_baseline <- "#F8766D"
color_distributive <- "#7CAE00"
color_cumulative <- "#00BFC4"
color_control <- "#C77CFF"



###### Ridgeline plot by Responder group (2*4 comparisons) ######

# Preparation
library(ggridges) # Ridgeline Plots in 'ggplot2' # Ridgeline Plots in 'ggplot2'

# Tibble for significance lines
lines2x4 <- tibble(
  responder.group = c(
    # Cumulative responders
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    # Distributive responders
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%"
  ),
  # 1 Baseline, 2 Distributive, 3 Cumulative, 4 Control
  # Cumulative responders, Distributive responders
  # DvsC, DvsB, CvsB, ControlvsB
  x = c(
    115, 115, 125, 135,
    115, 115, 125, 135
  ),
  xend = c(
    115, 115, 125, 135,
    115, 115, 125, 135
  ),
  y = c(
    3 - .4, 2 - .4, 2 - .4, 2 - .4,
    3 - .4, 2 - .4, 2 - .4, 2 - .4
  ),
  yend = c(
    3 + .4, 2 + .4, 3 + .4, 4 + .4,
    3 + .4, 2 + .4, 3 + .4, 4 + .4
  )
)

# Tibble for significance lines and stars
stars2x4 <- tibble(
  responder.group = c(
    # Cumulative responders
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    # Distributive responders
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%"
  ),
  # 1 Baseline, 2 Distributive, 3 Cumulative, 4 Control
  # Cumulative responders, Distributive responders
  # DvsC, DvsB, CvsB, ControlvsB
  x = c(
    117, 117, 130, 140,
    120, 117, 127, 137
  ),
  y = c(
    3, 2, 2.5, 3,
    3, 2, 2.5, 3
  ),
  label = c(Adjusted.symbol_c4, Adjusted.symbol_d4)
)

# Ridgeline plot by Condition
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition, color = Condition
  )) +
  geom_density_ridges(
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    alpha = 0.1,
    draw_baseline = FALSE,
    size = 0,
    color = NA # Get rid of lines
  ) +
  facet_wrap(~responder.group,
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5
  ) +
  # Means: numerical values
  geom_text(
    data = summary_twogroups,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25,
    width = 0.08,
  ) +
  # Add significance lines
  geom_segment(
    data = lines2x4,
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # Add significance stars
  geom_text(
    data = stars2x4,
    aes(x = x, y = y, label = label),
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 140),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # This moves the space below histograms
    name = "Condition"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_markdown(),
  )

ggsave(here("report", "Ridgeline plot by Responder group (2x4 comparisons).png"),
  width = 5,
  height = 4
)




###### Raincloud plot ######

library(raincloudplots) # The easy way to create raincloud plots # The easy way to create raincloud plots


# Raincloud plot for Cumulative responders
df_cumulative_participant <- rate_group %>%
  filter(responder.group == "Baseline < 50%") %>%
  filter(Condition != "Control") %>%
  arrange(Condition)

df_BDBC_cumulative_participant <- data_2x2(
  array_1 = df_cumulative_participant$distributive.rate[df_cumulative_participant$Condition == "Baseline"],
  array_2 = df_cumulative_participant$distributive.rate[df_cumulative_participant$Condition == "Distributive"],
  array_3 = df_cumulative_participant$distributive.rate[df_cumulative_participant$Condition == "Baseline"],
  array_4 = df_cumulative_participant$distributive.rate[df_cumulative_participant$Condition == "Cumulative"],
  labels = (c("BvsD", "BvsC")),
  jit_distance = 0.09,
  jit_seed = 321,
  spread_x_ticks = TRUE
)

df_BDBC_cumulative_p_summary <- summary_twogroups %>%
  filter(responder.group == "Baseline < 50%") %>%
  filter(Condition != "Control") %>%
  mutate(x_axis = case_when(
    Condition == "Baseline" ~ 1,
    Condition == "Distributive" ~ 2,
    Condition == "Cumulative" ~ 4
  )) %>%
  rbind(summary_twogroups %>%
    filter(responder.group == "Baseline < 50%") %>%
    filter(Condition == "Baseline") %>%
    mutate(x_axis = 3)) %>%
  arrange(x_axis)


raincloud_BDBC_C <- raincloud_2x2_repmes(
  data = df_BDBC_cumulative_participant,
  colors = (c(
    color_baseline, color_distributive,
    color_baseline, color_cumulative
  )),
  fills = (c(
    color_baseline, color_distributive,
    color_baseline, color_cumulative
  )),
  line_color = "gray",
  line_alpha = 0.3,
  size = 1,
  alpha = 0.5,
  spread_x_ticks = TRUE
) +
  # Add lines connecting the two means
  geom_line(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 1 | x_axis == 2),
    aes(x = c(1 - 0.13, 2 + 0.13), y = mean),
    color = "gray",
    size = 1
  ) +
  geom_line(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 3 | x_axis == 4),
    aes(x = c(3 - 0.13, 4 + 0.13), y = mean),
    color = "gray",
    size = 1
  ) +
  # 95%CIs 1 Baseline
  geom_point(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 1),
    aes(x = x_axis, y = mean),
    position = position_nudge(-0.13),
    color = color_baseline,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 1),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(-0.13),
    color = color_baseline,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 2 Distributive
  geom_point(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 2),
    aes(x = x_axis, y = mean),
    position = position_nudge(0.13),
    color = color_distributive,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 2),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(0.13),
    color = color_distributive,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 3 Baseline
  geom_point(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 3),
    aes(x = x_axis, y = mean),
    position = position_nudge(-0.13),
    color = color_baseline,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 3),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(-0.13),
    color = color_baseline,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 4 Cumulative
  geom_point(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 4),
    aes(x = x_axis, y = mean),
    position = position_nudge(0.13),
    color = color_cumulative,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_cumulative_p_summary %>%
      filter(x_axis == 4),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(0.13),
    color = color_cumulative,
    width = 0.05,
    size = 0.4
  ) +
  # Add significance lines and stars
  # Distributive vs Cumulative
  geom_line(
    data = tibble(
      x = c(2, 4),
      y = c(109, 109)
    ),
    aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(
      x = 3,
      y = 110
    ),
    aes(
      x = x, y = y,
      label = Adjusted.symbol_c4[1]
    )
  ) +
  # Baseline vs Distributive
  geom_line(
    data = tibble(
      x = c(1, 2),
      y = c(103, 103)
    ),
    aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(
      x = 1.5,
      y = 104
    ),
    aes(
      x = x, y = y,
      label = Adjusted.symbol_c4[2]
    )
  ) +
  # Baseline vs Cumulative
  geom_line(
    data = tibble(
      x = c(3, 4),
      y = c(103, 103)
    ),
    aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(
      x = 3.5,
      y = 106
    ),
    aes(
      x = x, y = y,
      label = Adjusted.symbol_c4[3]
    )
  ) +
  # Theme
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c(
      "Baseline", "Distributive",
      "Baseline", "Cumulative"
    ),
    limits = c(0, 5)
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 112)
  ) +
  labs(
    x = "Condition",
    y = "Distributive choice (%)"
  ) +
  theme_classic() +
  # Title
  ggtitle(
    label = "Cumulative responders",
    subtitle = glue("N = {cumulative_n}")
  ) +
  # Center the title
  theme(
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5)
  )

raincloud_BDBC_C



# Raincloud plot for Distributive responders
df_distributive_participant <- rate_group %>%
  filter(responder.group == "Baseline > 50%") %>%
  filter(Condition != "Control") %>%
  arrange(Condition)

df_BDBC_distributive_participant <- data_2x2(
  array_1 = df_distributive_participant$distributive.rate[df_distributive_participant$Condition == "Baseline"],
  array_2 = df_distributive_participant$distributive.rate[df_distributive_participant$Condition == "Distributive"],
  array_3 = df_distributive_participant$distributive.rate[df_distributive_participant$Condition == "Baseline"],
  array_4 = df_distributive_participant$distributive.rate[df_distributive_participant$Condition == "Cumulative"],
  labels = (c("BvsC", "BvsD")),
  jit_distance = 0.09,
  jit_seed = 321,
  spread_x_ticks = TRUE
)

df_BDBC_distributive_p_summary <- summary_twogroups %>%
  filter(responder.group == "Baseline > 50%") %>%
  filter(Condition != "Control") %>%
  mutate(x_axis = case_when(
    Condition == "Baseline" ~ 1,
    Condition == "Distributive" ~ 2,
    Condition == "Cumulative" ~ 4
  )) %>%
  rbind(summary_twogroups %>%
    filter(responder.group == "Baseline > 50%") %>%
    filter(Condition == "Baseline") %>%
    mutate(x_axis = 3)) %>%
  arrange(x_axis)


raincloud_BDBC_D <- raincloud_2x2_repmes(
  data = df_BDBC_distributive_participant,
  colors = (c(
    color_baseline, color_distributive,
    color_baseline, color_cumulative
  )),
  fills = (c(
    color_baseline, color_distributive,
    color_baseline, color_cumulative
  )),
  line_color = "gray",
  line_alpha = 0.3,
  size = 1,
  alpha = 0.5,
  spread_x_ticks = TRUE
) +
  # Add lines connecting the two means
  geom_line(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 1 | x_axis == 2),
    aes(x = c(1 - 0.13, 2 + 0.13), y = mean),
    color = "gray",
    size = 1
  ) +
  geom_line(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 3 | x_axis == 4),
    aes(x = c(3 - 0.13, 4 + 0.13), y = mean),
    color = "gray",
    size = 1
  ) +
  # 95%CIs 1 Baseline
  geom_point(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 1),
    aes(x = x_axis, y = mean),
    position = position_nudge(-0.13),
    color = color_baseline,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 1),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(-0.13),
    color = color_baseline,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 2 Distributive
  geom_point(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 2),
    aes(x = x_axis, y = mean),
    position = position_nudge(0.13),
    color = color_distributive,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 2),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(0.13),
    color = color_distributive,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 3 Baseline
  geom_point(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 3),
    aes(x = x_axis, y = mean),
    position = position_nudge(-0.13),
    color = color_baseline,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 3),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(-0.13),
    color = color_baseline,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 4 Cumulative
  geom_point(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 4),
    aes(x = x_axis, y = mean),
    position = position_nudge(0.13),
    color = color_cumulative,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_distributive_p_summary %>%
      filter(x_axis == 4),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(0.13),
    color = color_cumulative,
    width = 0.05,
    size = 0.4
  ) +
  # Add significance lines and stars
  # Distributive vs Cumulative
  geom_line(
    data = tibble(
      x = c(2, 4),
      y = c(109, 109)
    ),
    aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(
      x = 3,
      y = 112
    ),
    aes(
      x = x, y = y,
      label = Adjusted.symbol_d4[1]
    )
  ) +
  # Baseline vs Distributive
  geom_line(
    data = tibble(
      x = c(1, 2),
      y = c(103, 103)
    ),
    aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(
      x = 1.5,
      y = 104
    ),
    aes(
      x = x, y = y,
      label = Adjusted.symbol_d4[2]
    )
  ) +
  # Baseline vs Cumulative
  geom_line(
    data = tibble(
      x = c(3, 4),
      y = c(103, 103)
    ),
    aes(x = x, y = y),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(
      x = 3.5,
      y = 104
    ),
    aes(
      x = x, y = y,
      label = Adjusted.symbol_d4[3]
    )
  ) +
  # Theme
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c(
      "Baseline", "Distributive",
      "Baseline", "Cumulative"
    ),
    limits = c(0, 5)
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 112)
  ) +
  labs(
    x = "Condition",
    y = NULL
  ) +
  theme_classic() +
  # Title
  ggtitle(
    label = "Distributive responders",
    subtitle = glue("N = {distributive_n}")
  ) +
  # Center the title
  theme(
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5)
  )

raincloud_BDBC_D


# Combine raincloud plots for Cumulative and Distributive responders
ggarrange(raincloud_BDBC_C,
  raincloud_BDBC_D,
  nrow = 1
)

ggsave(here("report", "Raincloud plot by Responder group.png"),
  width = 8,
  height = 6
)



###### Bar plot ######

# Bar plot by Condition and Responder group
summary_twogroups %>%
  ggplot(aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Condition",
    y = "Distributive choice (%)"
  ) +
  facet_wrap(vars(responder.group),
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  theme(strip.text.x = element_markdown()) +
  geom_errorbar(aes(
    ymin = lower.CI, ymax = upper.CI,
    x = Condition
  ),
  size = 0.5,
  width = 0.25
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100)
  ) +
  # Theme
  theme_bw() +
  theme(
    legend.position = "none", # Remove legend
    strip.text.x = element_markdown(), # Strip
  )

ggsave(here("report", "Bar plot by Responder group.png"),
  width = 8,
  height = 6
)





################ Post hoc analysis ################


###### Data analysis on global results ######

# df.1: Distributive as reference level
# Distributive vs Cumulative
df.1 <- data_frame %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Distributive", "Cumulative",
      "Baseline", "Control"
    )
  ))


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# gmodel1.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel1.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# gmodel1.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel1.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# gmodel1.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel1.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# gmodel1.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.1,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel1.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

gmodel1.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.1,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(gmodel1.5, tol = 1e-05)

Anova(gmodel1.5)
summary(gmodel1.5)

gDvs <- tidy(gmodel1.5)

gDvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

gmodel1.5 %>%
  tbl_regression()


# df.2: Control as reference level
# Distributive vs Control; Cumulative vs Control
# Baseline vs Control

df.2 <- data_frame %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Control", "Distributive",
      "Cumulative", "Baseline"
    )
  ))
contrasts(df.2$Condition)


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# gmodel2.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel2.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# gmodel2.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel2.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# gmodel2.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel2.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# gmodel2.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.2,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(gmodel2.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

gmodel2.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.2,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(gmodel2.5, tol = 1e-05)

Anova(gmodel2.5)
summary(gmodel2.5)

gControlvs <- tidy(gmodel2.5)

gControlvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

gmodel2.5 %>%
  tbl_regression()



###### GLMER model summary table for global results (3 comparisons) ######

# Calculate adjusted p values
pvalue3 <- c(
  gDvs %>%
    filter(term == "ConditionCumulative") %>%
    pull(p.value),
  gControlvs %>%
    filter(term == "ConditionDistributive" |
      term == "ConditionCumulative") %>%
    pull(p.value)
)

values3 <- p.adjust(pvalue3, method = "bonferroni")
format(values3, scientific = FALSE)

# GLMER model summary table for global results
regression_g3 <- gDvs %>%
  filter(term == "ConditionCumulative") %>%
  rbind(gControlvs %>% filter(term == "ConditionDistributive" |
    term == "ConditionCumulative")) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    beta = estimate,
    S.E. = std.error,
    Z = statistic
  ) %>%
  mutate(Comparison = c(
    "Distributive vs. Cumulative",
    "Distributive vs. Control",
    "Cumulative vs. Control"
  )) %>%
  dplyr::select(Comparison, beta, S.E., Z, p.value) %>%
  mutate(Adjusted = values3) %>%
  mutate(p.symbol = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01 ~ "**",
    p.value <= 0.05 ~ "*",
    p.value > 0.05 ~ "ns"
  )) %>%
  mutate(Adjusted.symbol = case_when(
    Adjusted <= 0.001 ~ "***",
    Adjusted <= 0.01 ~ "**",
    Adjusted <= 0.05 ~ "*",
    Adjusted > 0.05 ~ "ns"
  )) %>%
  dplyr::select(
    Comparison, beta, S.E., Z,
    p.value, p.symbol,
    Adjusted, Adjusted.symbol
  )

t_regression_g3 <- regression_g3 %>%
  mutate_at(vars(p.value, Adjusted),
    format.pval,
    eps = .001, digits = 3
  ) %>%
  mutate_at(vars(beta, S.E., Z), round, 3) %>%
  gt() %>%
  cols_label(
    Z = md("*Z*"),
    p.value = md("*p*-value"),
    p.symbol = md(""),
    Adjusted.symbol = md("")
  ) %>%
  tab_header(title = "All participants")

t_regression_g3

t_regression_g3 %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for all participants (3 comparisons).png"
    )
  )




###### Data viz for global results ######

# Preparation

# Significance stars for data viz
Adjusted.symbol_g <- regression_g3 %>%
  pull(Adjusted.symbol)


###### Ridgeline plot ######
rate %>%
  mutate(all.participants = "All participants") %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition, color = Condition
  )) +
  # # Add grey background to Baseline panel ()
  # geom_rect(
  #   data = rate_group %>% filter(Condition == "Baseline"),
  #   aes(
  #     xmin = -Inf, xmax = 117.5,
  #     ymin = 1, ymax = 2
  #   ),
  #   fill = "grey95",
  #   inherit.aes = FALSE
  # ) +
  # Density plot
  geom_density_ridges(
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    alpha = 0.1,
    draw_baseline = FALSE,
    size = 0,
    color = NA # Get rid of lines
  ) +
  facet_wrap(
    vars(all.participants),
    labeller = labeller(
      all.participants = c(`All participants` = glue("All participants<br>N = {total_n}"))
    ), # Add strip title
    strip.position = "top"
  ) +

  # Means: vertical lines
  geom_segment(
    data = summary_condition,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5
  ) +
  # Means: numerical values
  geom_text(
    data = summary_condition,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Medians: crosses
  geom_point(
    data = summary_condition,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_condition,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25, width = 0.08,
  ) +
  # Add significance lines and stars
  # Distributive vs Cumulative
  # line
  geom_segment(
    x = 115, xend = 115,
    y = 3 - 0.4, yend = 3 + 0.4,
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # star
  geom_text(
    x = 116, y = 3,
    label = Adjusted.symbol_g[1],
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Distributive vs Control
  # line
  geom_segment(
    x = 125, xend = 125,
    y = 3 - 0.4, yend = 4 + 0.4,
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # star
  geom_text(
    x = 128, y = 3.5,
    label = Adjusted.symbol_g[2],
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Cumulative vs Control
  # line
  geom_segment(
    x = 115, xend = 115,
    y = 4 - 0.4, yend = 4 + 0.4,
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # star
  geom_text(
    x = 118, y = 4,
    label = Adjusted.symbol_g[3],
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 130),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # this moves the space below histograms
    name = "Condition"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x.top = element_markdown()
  )
# # Title
# ggtitle(
#   label = "All participants",
#   subtitle = glue("N = {total_n}")
# ) +
# # Center the title
# theme(
#   plot.title = element_markdown(hjust = 0.5),
#   plot.subtitle = element_markdown(hjust = 0.5)
# )

ggsave(here("report", "Post hoc global results Ridgeline plot (3 comparisons).png"),
  width = 4,
  height = 4
)




###### Ridgeline plot (alpha = Responder group) ######

# It displays density by Responder group and Condition.
# The bins of each responder group have overlap.
# Thus, not quite useful.

# I want to show the density by Condition, relative to whole participants.
# Hence, stacked histogram fits this purpose, see "Stacked histogram by Condition".

# rate_twogroups %>% # This does not include Baseline = 50%

rate_group %>% # This includes Baseline = 50%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition,
    color = Condition,
    alpha = responder.group
  )) +
  geom_density_ridges(
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    draw_baseline = FALSE,
    size = 0,
    color = NA
  ) + # Get rid of lines
  scale_alpha_manual(
    values = c(
      `Baseline < 50%` = 0.1,
      `Baseline = 50%` = 0.25,
      `Baseline > 50%` = 0.4
    )
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_condition,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  # Means: numerical values
  geom_text(
    data = summary_condition,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  # Medians: crosses
  geom_point(
    data = summary_condition,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4,
    inherit.aes = FALSE
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_condition,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25, width = 0.08,
  ) +
  # Add significance lines and stars
  # Distributive vs Cumulative
  # line
  geom_segment(
    x = 115, xend = 115,
    y = 3 - 0.4, yend = 3 + 0.4,
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # star
  geom_text(
    x = 116, y = 3,
    label = Adjusted.symbol_g[1],
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Distributive vs Control
  # line
  geom_segment(
    x = 125, xend = 125,
    y = 3 - 0.4, yend = 4 + 0.4,
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # star
  geom_text(
    x = 128, y = 3.5,
    label = Adjusted.symbol_g[2],
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Cumulative vs Control
  # line
  geom_segment(
    x = 115, xend = 115,
    y = 4 - 0.4, yend = 4 + 0.4,
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # star
  geom_text(
    x = 118, y = 4,
    label = Adjusted.symbol_g[3],
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 130),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # this moves the space below histograms
    name = "Condition"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
  ) +
  # Title
  ggtitle(
    label = "All participants",
    subtitle = glue("N = {total_n}")
  ) +
  # Center the title
  theme(
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5)
  )

ggsave(here("report", "Post hoc global results Ridgeline plot (alpha = Responder group).png"),
  width = 4,
  height = 4
)




###### Stacked histogram by Condition ######

# Legend on the top
rate_group %>%
  mutate(all.participants = "All participants") %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate,
    fill = Condition
  )) +
  # Add grey background to Baseline panel
  geom_rect(
    data = rate_group %>% filter(Condition == "Baseline"),
    aes(
      xmin = -Inf, xmax = 117.5,
      ymin = -Inf, ymax = Inf
    ),
    fill = "grey95",
    inherit.aes = FALSE
  ) +
  geom_histogram(
    aes(alpha = responder.group),
    # aes(alpha = fct_rev(responder.group)),  # Reorder stacked bars (make Baseline > 50% on top)
    binwidth = 20
  ) +
  facet_grid(
    vars(fct_rev(Condition)), # Reverse order of factor levels so that Baseline appears at bottom
    vars(all.participants),
    labeller = labeller(
      all.participants = c(`All participants` = glue("All participants<br>N = {total_n}"))
    ),
    switch = "y"
  ) +
  labs(
    x = "Distributive choice (%)",
    y = "Number of participants"
  ) +
  scale_alpha_manual("Responder group", # Change legend name
    values = c(
      `Baseline < 50%` = 0.1,
      `Baseline = 50%` = 0.25,
      `Baseline > 50%` = 0.4
    )
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(-10, 130)
  ) +
  # Means: numeric values
  geom_text(
    data = summary_condition,
    aes(
      x = mean, y = 9,
      label = round(mean, digits = 1),
      color = Condition
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_condition,
    aes(
      x = mean, xend = mean,
      y = 4, yend = -4,
      color = Condition
    ),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  # Medians: crosses
  geom_point(
    data = summary_condition,
    aes(
      x = median, y = 0,
      color = Condition
    ),
    size = 1.2,
    shape = 4,
    inherit.aes = FALSE
  ) +
  # Add 95% CIs
  geom_errorbar(
    data = summary_condition,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = c(0, 0, 0, 0),
      color = Condition
    ),
    width = 4,
    inherit.aes = FALSE
  ) +
  # Theme
  theme_classic() +
  # Remove background and y axis
  theme(
    panel.background = element_blank(),
    axis.line.y = element_blank(),
  ) +
  # Strips
  theme(
    strip.background.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0, # Horizontal y labels
      hjust = 1, # Right align
      vjust = 0
    ),
    strip.text.x = element_markdown(),
    panel.spacing.y = unit(0, "pt"),
    legend.position = "top"
  ) +
  # Hide Condition legend
  guides(fill = "none", color = "none") +
  # Significance lines
  geom_segment(
    data = tibble(
      Condition = c(
        "Control", "Control",
        "Cumulative", "Cumulative", "Cumulative",
        "Distributive", "Distributive"
      ),
      x = c(
        115, 125,
        115, 125, 115,
        115, 125
      ),
      y = c(
        10, 10,
        Inf, Inf, 10,
        Inf, Inf
      ),
      yend = c(
        -Inf, -Inf,
        20, -Inf, -Inf,
        20, 20
      )
    ),
    aes(x = x, xend = x, y = y, yend = yend)
  ) +
  # Significance stars
  geom_text(
    data = tibble(
      Condition = c(
        "Control", "Cumulative",
        "Cumulative",
        "Cumulative", "Distributive"
      ),
      x = c(118, 118, 128, 116, 116),
      y = c(-Inf, Inf, 15, -Inf, Inf),
      label = c("ns", "ns", "ns", "***", "***")
    ),
    aes(x = x, y = y, label = label),
    angle = -90
  )


ggsave(here("report", "Post hoc global results Stacked histogram (top legend).png"),
  width = 7,
  height = 6
)



# Legend on the right
rate_group %>%
  mutate(all.participants = "All participants") %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate,
    fill = Condition
  )) +
  # Add grey background to Baseline panel
  geom_rect(
    data = rate_group %>% filter(Condition == "Baseline"),
    aes(
      xmin = -Inf, xmax = 117.5,
      ymin = -Inf, ymax = Inf
    ),
    fill = "grey95",
    inherit.aes = FALSE
  ) +
  geom_histogram(
    aes(alpha = responder.group),
    # aes(alpha = fct_rev(responder.group)),  # Reorder stacked bars (make Baseline > 50% on top)
    binwidth = 20
  ) +
  scale_alpha_manual("Responder group", # Change legend name
    values = c(
      `Baseline < 50%` = 0.1,
      `Baseline = 50%` = 0.25,
      `Baseline > 50%` = 0.4
    )
  ) +
  facet_grid(
    vars(fct_rev(Condition)), # Reverse order of factor levels so that Baseline appears at bottom
    vars(all.participants),
    labeller = labeller(
      all.participants = c(`All participants` = glue("All participants<br>N = {total_n}"))
    ),
    switch = "y"
  ) +
  labs(
    x = "Distributive choice (%)",
    y = "Number of participants"
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(-10, 130)
  ) +
  # Means: numeric values
  geom_text(
    data = summary_condition,
    aes(
      x = mean, y = 9,
      label = round(mean, digits = 1),
      color = Condition
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_condition,
    aes(
      x = mean, xend = mean,
      y = 4, yend = -4,
      color = Condition
    ),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  # Medians: crosses
  geom_point(
    data = summary_condition,
    aes(
      x = median, y = 0,
      color = Condition
    ),
    size = 1.2,
    shape = 4,
    inherit.aes = FALSE
  ) +
  # Add 95% CIs
  geom_errorbar(
    data = summary_condition,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = c(0, 0, 0, 0),
      color = Condition
    ),
    width = 4,
    inherit.aes = FALSE
  ) +
  # Theme
  theme_classic() +
  # Remove background and y axis
  theme(
    panel.background = element_blank(),
    axis.line.y = element_blank(),
  ) +
  # Strips
  theme(
    strip.background.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0, # Horizontal y labels
      hjust = 1, # Right align
      vjust = 0
    ),
    strip.text.x = element_markdown(),
    panel.spacing.y = unit(0, "pt")
  ) +
  # Hide Condition legend
  guides(fill = "none", color = "none") +
  # Significance lines
  geom_segment(
    data = tibble(
      Condition = c(
        "Control", "Control",
        "Cumulative", "Cumulative", "Cumulative",
        "Distributive", "Distributive"
      ),
      x = c(
        115, 125,
        115, 125, 115,
        115, 125
      ),
      y = c(
        10, 10,
        Inf, Inf, 10,
        Inf, Inf
      ),
      yend = c(
        -Inf, -Inf,
        20, -Inf, -Inf,
        20, 20
      )
    ),
    aes(x = x, xend = x, y = y, yend = yend)
  ) +
  # Significance stars
  geom_text(
    data = tibble(
      Condition = c(
        "Control", "Cumulative",
        "Cumulative",
        "Cumulative", "Distributive"
      ),
      x = c(118, 118, 128, 116, 116),
      y = c(-Inf, Inf, 15, -Inf, Inf),
      label = c("ns", "ns", "ns", "***", "***")
    ),
    aes(x = x, y = y, label = label),
    angle = -90
  )


ggsave(here("report", "Post hoc global results Stacked histogram (right legend).png"),
  width = 6,
  height = 5
)




###### Raincloud plot ######

# Global results (N = 71)
df_global <- rate_group %>%
  filter(Condition != "Control") %>%
  arrange(Condition)

# Create data frames for plotting
df_BDBC_global <- data_2x2(
  array_1 = df_global$distributive.rate[df_global$Condition == "Baseline"],
  array_2 = df_global$distributive.rate[df_global$Condition == "Distributive"],
  array_3 = df_global$distributive.rate[df_global$Condition == "Baseline"],
  array_4 = df_global$distributive.rate[df_global$Condition == "Cumulative"],
  labels = (c("BvsD", "BvsC")),
  jit_distance = 0.09,
  jit_seed = 321,
  spread_x_ticks = TRUE # Set 4 ticks
)

df_BDBC_global_summary <- summary_condition %>%
  filter(Condition != "Control") %>%
  mutate(x_axis = case_when(
    Condition == "Baseline" ~ 1,
    Condition == "Distributive" ~ 2,
    Condition == "Cumulative" ~ 4
  )) %>%
  rbind(summary_condition %>%
    filter(Condition == "Baseline") %>%
    mutate(x_axis = 3)) %>%
  arrange(x_axis)


# Raincloud plot for global results
raincloud_BDBC_global <- raincloud_2x2_repmes(
  data = df_BDBC_global,
  colors = (c(
    color_baseline, color_distributive,
    color_baseline, color_cumulative
  )),
  fills = (c(
    color_baseline, color_distributive,
    color_baseline, color_cumulative
  )),
  line_color = "gray",
  line_alpha = 0.3,
  size = 1,
  alpha = 0.5,
  spread_x_ticks = TRUE
) +
  # Add lines connecting the two means
  geom_line(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 1 | x_axis == 2),
    aes(x = c(1 - 0.13, 2 + 0.13), y = mean),
    color = "gray",
    size = 1
  ) +
  geom_line(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 3 | x_axis == 4),
    aes(x = c(3 - 0.13, 4 + 0.13), y = mean),
    color = "gray",
    size = 1
  ) +
  # 95%CIs 1 Baseline
  geom_point(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 1),
    aes(x = x_axis, y = mean),
    position = position_nudge(-0.13),
    color = color_baseline,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 1),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(-0.13),
    color = color_baseline,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 2 Distributive
  geom_point(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 2),
    aes(x = x_axis, y = mean),
    position = position_nudge(0.13),
    color = color_distributive,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 2),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(0.13),
    color = color_distributive,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 3 Baseline
  geom_point(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 3),
    aes(x = x_axis, y = mean),
    position = position_nudge(-0.13),
    color = color_baseline,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 3),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(-0.13),
    color = color_baseline,
    width = 0.05,
    size = 0.4
  ) +
  # 95%CIs 4 Cumulative
  geom_point(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 4),
    aes(x = x_axis, y = mean),
    position = position_nudge(0.13),
    color = color_cumulative,
    size = 1.5
  ) +
  geom_errorbar(
    data = df_BDBC_global_summary %>%
      filter(x_axis == 4),
    aes(
      x = x_axis, y = mean,
      ymin = lower.CI, ymax = upper.CI
    ),
    position = position_nudge(0.13),
    color = color_cumulative,
    width = 0.05,
    size = 0.4
  ) +
  # Theme
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c(
      "Baseline", "Distributive",
      "Baseline", "Cumulative"
    ),
    limits = c(0, 5)
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100)
  ) +
  labs(
    x = "Condition",
    y = "Distributive choice (%)"
  ) +
  theme_classic() +
  # Title
  ggtitle(
    label = "All participants",
    subtitle = glue("N = {total_n}")
  ) +
  # Center the title
  theme(
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5)
  )

raincloud_BDBC_global

ggsave(here("report", "Raincloud plot for global results.png"),
  width = 5,
  height = 6
)



###### Bar plot ######

# Bar plot by Condition
summary_condition %>%
  ggplot(aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Condition",
    y = "Distributive choice (%)"
  ) +
  geom_errorbar(aes(
    ymin = lower.CI, ymax = upper.CI,
    x = Condition
  ),
  size = 0.5,
  width = 0.25
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100)
  ) +
  # Theme
  theme_bw() +
  # Remove legend
  theme(legend.position = "none")

ggsave(here(
  "report",
  "Post hoc global results Bar plot by Condition.png"
),
width = 5,
height = 6
)






###### Control vs Priming condition eliciting the preferred reading ######


# The comparison between the Control condition
# and the Priming condition eliciting the preferred interpretation
# is to examine the effects of the preferred interpretation
# based on the assumption that the target trials in these two conditions
# were influenced by a comparable size of the spillover effects
# from the primes eliciting the less favored interpretation
# (because of the random administration of triplets and fillers in Block 2)


# Control vs Cumulative for Cumulative responders

# df.cumulative.3: Cumulative as reference level
# Control vs Cumulative


df.cumulative.3 <- data_frame %>%
  filter(responder.group == "Baseline < 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Cumulative", "Distributive",
      "Baseline", "Control"
    )
  ))
contrasts(df.cumulative.3$Condition)


# Jump to the fifth attempt for the maximal converging model
# with Random effects: (1 | SUBJECT) + (1 | ITEM)


# # First attempt
# # Random effects: (1 + Condition | SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# cmodel3.1 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.cumulative.3,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel3.1, tol = 1e-05)
#
# # Second attempt
# # Random effects: (1 + Condition || SUBJECT) + (1 | ITEM)
# # No: Cause singular fit
#
# cmodel3.2 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT) + (1 | ITEM),
# family = binomial(link = logit),
# data = df.cumulative.3,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel3.2, tol = 1e-05)
#
# # Third attempt
# # Random effects: (1 + Condition | SUBJECT)
# # No: Cause singular fit
#
# cmodel3.3 <- glmer(Response ~ Condition +
#   (1 + Condition | SUBJECT),
# family = binomial(link = logit),
# data = df.cumulative.3,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel3.3, tol = 1e-05)
#
# # Fourth attempt
# # Random effects: (1 + Condition || SUBJECT)
# # No: Cause singular fit
#
# cmodel3.4 <- glmer(Response ~ Condition +
#   (1 + Condition || SUBJECT),
# family = binomial(link = logit),
# data = df.cumulative.3,
# control = glmerControl(
#   optimizer = "bobyqa",
#   optCtrl = list(maxfun = 100000)
# )
# )
# isSingular(cmodel3.4, tol = 1e-05)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

cmodel3.5 <- glmer(Response ~ Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.cumulative.3,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(cmodel3.5, tol = 1e-05)

Anova(cmodel3.5)
summary(cmodel3.5)

cCvs <- tidy(cmodel3.5)

cCvs %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

cmodel3.5 %>%
  tbl_regression()


# Control vs Distributive for Distributive responders
# Can be extracted from dDvs


# Calculate adjusted p values
# 10 comparisons
pvalue10 <- c(
  cDvs %>%
    filter(term == "ConditionCumulative") %>%
    pull(p.value),
  cBvs %>%
    filter(term == "ConditionDistributive" |
      term == "ConditionCumulative" |
      term == "ConditionControl") %>%
    pull(p.value),
  cCvs %>%
    filter(term == "ConditionControl") %>%
    pull(p.value),
  dDvs %>%
    filter(term == "ConditionCumulative") %>%
    pull(p.value),
  dBvs %>%
    filter(term == "ConditionDistributive" |
      term == "ConditionCumulative" |
      term == "ConditionControl") %>%
    pull(p.value),
  dDvs %>%
    filter(term == "ConditionControl") %>%
    pull(p.value)
)

values10 <- p.adjust(pvalue10, method = "bonferroni")
format(values10, scientific = FALSE)



###### GLMER model summary table (2*5 comparisons) ######

# + Control vs C for Cumulative responders
# + Control vs D for Distributive responders

# GLMER model summary table for Cumulative responders
regression_c5 <- cDvs %>%
  filter(term == "ConditionCumulative") %>%
  rbind(cBvs %>% filter(term == "ConditionDistributive" |
    term == "ConditionCumulative" |
    term == "ConditionControl")) %>%
  rbind(cCvs %>% filter(term == "ConditionControl")) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    beta = estimate,
    S.E. = std.error,
    Z = statistic
  ) %>%
  mutate(Comparison = c(
    "Distributive vs. Cumulative",
    "Distributive vs. Baseline",
    "Cumulative vs. Baseline",
    "Control vs. Baseline",
    "Cumulative vs. Control"
  )) %>%
  dplyr::select(Comparison, beta, S.E., Z, p.value) %>%
  mutate(Adjusted = values10[1:5]) %>%
  mutate(p.symbol = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01 ~ "**",
    p.value <= 0.05 ~ "*",
    p.value > 0.05 ~ "ns"
  )) %>%
  mutate(Adjusted.symbol = case_when(
    Adjusted <= 0.001 ~ "***",
    Adjusted <= 0.01 ~ "**",
    Adjusted <= 0.05 ~ "*",
    Adjusted > 0.05 ~ "ns"
  )) %>%
  dplyr::select(
    Comparison, beta, S.E., Z,
    p.value, p.symbol,
    Adjusted, Adjusted.symbol
  )

t_regression_c5 <- regression_c5 %>%
  mutate_at(vars(p.value, Adjusted),
    format.pval,
    eps = .001, digits = 3
  ) %>%
  mutate_at(vars(beta, S.E., Z), round, 3) %>%
  gt() %>%
  cols_label(
    Z = md("*Z*"),
    p.value = md("*p*-value"),
    p.symbol = md(""),
    Adjusted.symbol = md("")
  ) %>%
  tab_header(title = "Cumulative responders")

t_regression_c5

t_regression_c5 %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for Cumulative responders (5 comparisons).png"
    )
  )


# GLMER model summary table for Distributive responders
regression_d5 <- dDvs %>%
  filter(term == "ConditionCumulative") %>%
  rbind(dBvs %>% filter(term == "ConditionDistributive" |
    term == "ConditionCumulative" |
    term == "ConditionControl")) %>%
  rbind(dDvs %>% filter(term == "ConditionControl")) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    beta = estimate,
    S.E. = std.error,
    Z = statistic
  ) %>%
  mutate(Comparison = c(
    "Distributive vs. Cumulative",
    "Distributive vs. Baseline",
    "Cumulative vs. Baseline",
    "Control vs. Baseline",
    "Distributive vs. Control"
  )) %>%
  dplyr::select(Comparison, beta, S.E., Z, p.value) %>%
  mutate(Adjusted = values10[6:10]) %>%
  mutate(p.symbol = case_when(
    p.value <= 0.001 ~ "***",
    p.value <= 0.01 ~ "**",
    p.value <= 0.05 ~ "*",
    p.value > 0.05 ~ "ns"
  )) %>%
  mutate(Adjusted.symbol = case_when(
    Adjusted <= 0.001 ~ "***",
    Adjusted <= 0.01 ~ "**",
    Adjusted <= 0.05 ~ "*",
    Adjusted > 0.05 ~ "ns"
  )) %>%
  dplyr::select(
    Comparison, beta, S.E., Z,
    p.value, p.symbol,
    Adjusted, Adjusted.symbol
  )


t_regression_d5 <- regression_d5 %>%
  mutate_at(vars(p.value, Adjusted),
    format.pval,
    eps = .001, digits = 3
  ) %>%
  mutate_at(vars(beta, S.E., Z), round, 3) %>%
  gt() %>%
  cols_label(
    Z = md("*Z*"),
    p.value = md("*p*-value"),
    p.symbol = md(""),
    Adjusted.symbol = md("")
  ) %>%
  tab_header(title = "Distributive responders")

t_regression_d5

t_regression_d5 %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for Distributive responders (5 comparisons).png"
    )
  )



################ Data viz by Responder group (2*5 comparisons) ################


###### Ridgeline plot by Responder group (2*5 comparisons) ######

# Preparation


# Significance stars for data viz
Adjusted.symbol_c5 <- regression_c5 %>%
  pull(Adjusted.symbol)
Adjusted.symbol_d5 <- regression_d5 %>%
  pull(Adjusted.symbol)


# Tibble for significance lines
lines2x5 <- tibble(
  responder.group = c(
    # Cumulative responders
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    # Distributive responders
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%"
  ),
  # 1 Baseline, 2 Distributive, 3 Cumulative, 4 Control
  # Cumulative responders, Distributive responders
  # DvsC, DvsB, CvsB, ControlvsB, Controlvs(C/D)
  x = c(
    115, 115, 125, 135, 145,
    115, 115, 125, 135, 145
  ),
  xend = c(
    115, 115, 125, 135, 145,
    115, 115, 125, 135, 145
  ),
  y = c(
    3 - .4, 2 - .4, 2 - .4, 2 - .4, 4 + .4,
    3 - .4, 2 - .4, 2 - .4, 2 - .4, 4 + .4
  ),
  yend = c(
    3 + .4, 2 + .4, 3 + .4, 4 + .4, 4 - .4,
    3 + .4, 2 + .4, 3 + .4, 4 + .4, 3 - .4
  )
)

# Tibble for significance lines and stars
stars2x5 <- tibble(
  responder.group = c(
    # Cumulative responders
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    "Baseline < 50%",
    # Distributive responders
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%",
    "Baseline > 50%"
  ),
  # 1 Baseline, 2 Distributive, 3 Cumulative, 4 Control
  # Cumulative responders, Distributive responders
  # DvsC, DvsB, CvsB, ControlvsB
  x = c(
    117, 117, 130, 140, 150,
    120, 117, 127, 137, 150
  ),
  y = c(
    3, 2, 2.5, 3, 4,
    3, 2, 2.5, 3, 3.5
  ),
  label = c(Adjusted.symbol_c5, Adjusted.symbol_d5)
)

# Ridgeline plot by Condition
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition, color = Condition
  )) +
  geom_density_ridges(
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    alpha = 0.1,
    draw_baseline = FALSE,
    size = 0,
    color = NA # Get rid of lines
  ) +
  facet_wrap(~responder.group,
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5
  ) +
  # Means: numerical values
  geom_text(
    data = summary_twogroups,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25,
    width = 0.08,
  ) +
  # Add significance lines
  geom_segment(
    data = lines2x5,
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # Add significance stars
  geom_text(
    data = stars2x5,
    aes(x = x, y = y, label = label),
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 150),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # This moves the space below histograms
    name = "Condition"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_markdown()
  )

ggsave(here("report", "Ridgeline plot by Responder group (2x5 comparisons).png"),
  width = 6,
  height = 4
)




###### Ridgeline plot by Responder group (2*5 comparisons, alpha = Responder group, ncol = 2) ######

# Ridgeline plot by Condition
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition, color = Condition,
  )) +
  geom_density_ridges(
    aes(
      alpha = responder.group
    ),
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    draw_baseline = FALSE,
    size = 0,
    color = NA # Get rid of lines
  ) +
  scale_alpha_manual("Responder group", # Change legend name
    values = c(
      `Baseline < 50%` = 0.1,
      `Baseline > 50%` = 0.4
    )
  ) +
  facet_wrap(~responder.group,
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5
  ) +
  # Means: numerical values
  geom_text(
    data = summary_twogroups,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25,
    width = 0.08,
  ) +
  # Add significance lines
  geom_segment(
    data = lines2x5,
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # Add significance stars
  geom_text(
    data = stars2x5,
    aes(x = x, y = y, label = label),
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 150),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # This moves the space below histograms
    name = "Condition"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_markdown()
  )

ggsave(here("report", "Ridgeline plot by Responder group (2x5 comparisons, alpha = Responder group, ncol = 2).png"),
  width = 6,
  height = 4
)



###### Ridgeline plot by Responder group (2*5 comparisons, alpha = Responder group, ncol = 1) ######

# Ridgeline plot by Condition
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition, color = Condition,
  )) +
  geom_density_ridges(
    aes(
      alpha = responder.group
    ),
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    draw_baseline = FALSE,
    size = 0,
    color = NA # Get rid of lines
  ) +
  scale_alpha_manual("Responder group", # Change legend name
    values = c(
      `Baseline < 50%` = 0.1,
      `Baseline > 50%` = 0.4
    )
  ) +
  facet_wrap(~responder.group,
    ncol = 1,
    # strip.position = "left",
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5
  ) +
  # Means: numerical values
  geom_text(
    data = summary_twogroups,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25,
    width = 0.08,
  ) +
  # Add significance lines
  geom_segment(
    data = lines2x5,
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # Add significance stars
  geom_text(
    data = stars2x5,
    aes(x = x, y = y, label = label),
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 150),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # This moves the space below histograms
    name = "Condition",
    # position = "right"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y.left = element_text(),
    axis.ticks.y = element_blank(),
    strip.text.x.top = element_markdown(),
    strip.placement = "outside"
  )

ggsave(here("report", "Ridgeline plot by Responder group (2x5 comparisons, alpha = Responder group, ncol = 1).png"),
  width = 5,
  height = 8
)



###### Ridgeline plot by Responder group (2*5 comparisons, ncol = 1) ######

# Ridgeline plot by Condition
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate, y = Condition,
    fill = Condition, color = Condition,
  )) +
  geom_density_ridges(
    alpha = 0.1,
    stat = "binline",
    binwidth = 20,
    scale = 0.9,
    draw_baseline = FALSE,
    size = 0,
    color = NA # Get rid of lines
  ) +
  facet_wrap(~responder.group,
    ncol = 1,
    # strip.position = "left",
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = mean,
      xend = mean,
      y = as.numeric(as.factor(Condition)) + 0.075,
      yend = as.numeric(as.factor(Condition)) - 0.075,
      color = Condition
    ),
    size = 0.5
  ) +
  # Means: numerical values
  geom_text(
    data = summary_twogroups,
    aes(
      x = mean,
      y = as.numeric(as.factor(Condition)) + 0.2,
      color = Condition,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(
      x = median,
      y = Condition,
      color = Condition
    ),
    size = 1,
    shape = 4
  ) +
  # 95%CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = Condition,
      color = Condition
    ),
    inherit.aes = FALSE,
    size = 0.25,
    width = 0.08,
  ) +
  # Add significance lines
  geom_segment(
    data = lines2x5,
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.075,
    inherit.aes = FALSE
  ) +
  # Add significance stars
  geom_text(
    data = stars2x5,
    aes(x = x, y = y, label = label),
    angle = -90,
    size = 3,
    inherit.aes = FALSE
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(from = 0, to = 100, by = 25),
    limits = c(-17.5, 150),
    expand = expansion(add = c(0, 5)),
    name = "Distributive choice (%)"
  ) +
  # y axis
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1)), # This moves the space below histograms
    name = "Condition",
    # position = "right"
  ) +
  # Theme
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y.left = element_text(),
    axis.ticks.y = element_blank(),
    strip.text.x.top = element_markdown(),
    strip.placement = "outside"
  )

ggsave(here("report", "Ridgeline plot by Responder group (2x5 comparisons, ncol = 1).png"),
  width = 5,
  height = 8
)



###### Relative-to-Baseline priming effects ~ Baseline preference rate ######


# Relative-to-Baseline priming effects: Priming condition - Baseline
rate_group %>%
  pivot_wider(names_from = Condition, values_from = distributive.rate) %>%
  mutate(
    priming.C = Cumulative - Baseline,
    priming.D = Distributive - Baseline
  ) %>%
  pivot_longer(
    cols = c(priming.C, priming.D),
    names_to = "priming.condition",
    values_to = "relative.to.Baseline.priming"
  ) %>%
  mutate(
    Condition = case_when(
      endsWith(priming.condition, "C") == 1 ~ "Cumulative",
      endsWith(priming.condition, "D") == 1 ~ "Distributive"
    ),
    .keep = "unused"
  ) %>%
  mutate(Condition = factor(Condition,
    levels = c("Distributive", "Cumulative")
  )) %>%
  ggplot(aes(
    x = Baseline, y = relative.to.Baseline.priming,
    color = Condition,
  )) +
  geom_point(size = 2) +
  geom_smooth(
    formula = y ~ x,
    method = lm,
    # method = loess,
    se = TRUE
  ) +
  # Labels
  labs(
    x = "Baseline Distributive choice (%)",
    y = "Relative-to-Baseline priming effects"
  ) +
  # Color
  scale_color_manual(
    values = alpha(c(color_distributive, color_cumulative), 0.6)
  ) +
  scale_fill_manual(
    values = alpha(c(color_distributive, color_cumulative), 0.6)
  ) +
  # Theme
  theme_bw() +
  # Add line y = 0
  geom_hline(
    yintercept = 0,
    alpha = 0.5,
    size = 0.5
  ) +
  # Adjust y scale
  scale_y_continuous(
    breaks = c(-100, -50, 0, 50, 100),
    limits = c(-100, 100)
  ) +
  # Stat info
  stat_poly_eq(aes(
    label =
      paste(after_stat(eq.label), "*\" with \"*",
        after_stat(rr.label), "*\", \"*",
        after_stat(p.value.label), "*\".\"",
        sep = ""
      )
  ),
  formula = y ~ x,
  size = 4,
  alpha = 1,
  label.y = c(0.98, 0.93)
  )


ggsave(here(
  "report",
  "Post hoc global results Relative-to-Baseline priming effects ~ Baseline preference.png"
),
width = 6,
height = 5
)






###### Interaction between Condition and Numeric combination ######

# Bar plot by Condition and Numeric combination

# summary_condition_combination summarizes means, uppers, and lowers
# by Condition and Numeric combination
# Include the Baseline condition in the Different and Equal conditions
summary_condition_combination <- MyData %>%
  dplyr::select(
    Schedule.ID, Condition, trial.condition,
    distributive.target, accuracy.twoprimes,
    responder.group,
    numeric.combination
  ) %>%
  filter(Condition != "Baseline") %>%
  filter(responder.group != "Baseline = 50%") %>%
  filter(trial.condition == "target") %>%
  filter(accuracy.twoprimes == 2) %>%
  group_by(responder.group, Condition, numeric.combination) %>%
  summarise(
    mean = mean(distributive.target) * 100,
    lower.CI = binconf(sum(distributive.target), length(distributive.target))[2] * 100,
    upper.CI = binconf(sum(distributive.target), length(distributive.target))[3] * 100
  ) %>%
  rbind(summary_twogroups %>%
    filter(Condition == "Baseline") %>%
    mutate(numeric.combination = "Different")) %>%
  rbind(summary_twogroups %>%
    filter(Condition == "Baseline") %>%
    mutate(numeric.combination = "Equal"))

# Bar plot by Condition and Numeric combination
summary_condition_combination %>%
  ggplot(aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Condition",
    y = "Distributive choice (%)"
  ) +
  facet_grid(
    responder.group ~ numeric.combination,
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  geom_errorbar(aes(
    ymin = lower.CI, ymax = upper.CI,
    x = Condition
  ),
  size = 0.5,
  width = 0.25,
  inherit.aes = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100)
  ) +
  # Theme
  theme_bw() +
  theme(
    legend.position = "none", # Remove legend
    strip.text.y = element_markdown()
  )

ggsave(here(
  "report",
  "Post hoc global results Bar plot by Condition and Numeric combination.png"
),
width = 8,
height = 6
)



# GLMER model analysis
# on the interaction between Condition and Numeric combination
# Results: No interaction

# Prepare data
data_frame_numeric <- MyData %>%
  dplyr::select(
    Schedule.ID, Spreadsheet.Row,
    Condition, trial.condition,
    distributive.target, accuracy.twoprimes,
    responder.group,
    numeric.combination
  ) %>%
  filter(trial.condition == "target") %>%
  filter(responder.group != "Baseline = 50%") %>%
  # Exclude accuracy.twoprimes !=2 (in Distributive/Cumulative/Control conditions in Block 2)
  # Applied to Control condition for consistency
  filter(accuracy.twoprimes == 2) %>%
  rbind(MyData %>%
    dplyr::select(
      Schedule.ID, Spreadsheet.Row,
      Condition, trial.condition,
      distributive.target, accuracy.twoprimes,
      responder.group
    ) %>%
    filter(trial.condition == "target") %>%
    filter(responder.group != "Baseline = 50%") %>%
    filter(Condition == "Baseline") %>%
    mutate(numeric.combination = "Different")) %>%
  rbind(MyData %>%
    dplyr::select(
      Schedule.ID, Spreadsheet.Row,
      Condition, trial.condition,
      distributive.target, accuracy.twoprimes,
      responder.group
    ) %>%
    filter(trial.condition == "target") %>%
    filter(responder.group != "Baseline = 50%") %>%
    filter(Condition == "Baseline") %>%
    mutate(numeric.combination = "Equal")) %>%
  rename(
    Response = distributive.target,
    SUBJECT = Schedule.ID,
    ITEM = Spreadsheet.Row
  ) %>%
  group_by(SUBJECT, Condition, numeric.combination)



###### Cumulative responders: Baseline < 50% ######

# df.cumulative.numeric: Baseline as reference level
# Baseline vs Distributive/Cumulative/Control

df.cumulative.numeric <- data_frame_numeric %>%
  filter(responder.group == "Baseline < 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Baseline", "Distributive",
      "Cumulative", "Control"
    )
  ))
contrasts(df.cumulative.numeric$Condition)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

cmodel1.5numeric <- glmer(Response ~ numeric.combination * Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.cumulative.numeric,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(cmodel1.5numeric, tol = 1e-05)

Anova(cmodel1.5numeric)
summary(cmodel1.5numeric)

cBvsnumeric <- tidy(cmodel1.5numeric)

cBvsnumeric %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

cmodel1.5numeric %>%
  tbl_regression(
    label = list(numeric.combination ~ "Numeric combination"),
    intercept = TRUE
  ) %>%
  as_gt() %>%
  tab_header(
    title = "Cumulative responders"
  ) %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for Cumulative responders (interaction between Condition and Numeric combination).png"
    )
  )



###### Distributive responders: Baseline > 50% ######

# df.distributive.numeric: Baseline as reference level
# Baseline vs Distributive/Cumulative/Control

df.distributive.numeric <- data_frame_numeric %>%
  filter(responder.group == "Baseline > 50%") %>%
  mutate(Condition = factor(Condition,
    levels = c(
      "Baseline", "Distributive",
      "Cumulative", "Control"
    )
  ))
contrasts(df.distributive.numeric$Condition)


# Fifth attempt
# Random effects: (1 | SUBJECT) + (1 | ITEM)
# Yes!

dmodel1.5numeric <- glmer(Response ~ numeric.combination * Condition +
  (1 | SUBJECT) + (1 | ITEM),
family = binomial(link = logit),
data = df.distributive.numeric,
control = glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 100000)
)
)
isSingular(dmodel1.5numeric, tol = 1e-05)

Anova(dmodel1.5numeric)
summary(dmodel1.5numeric)

dBvsnumeric <- tidy(dmodel1.5numeric)

dBvsnumeric %>%
  mutate_at(vars(p.value),
    format.pval,
    eps = .001, digits = 3
  )

dmodel1.5numeric %>%
  tbl_regression(
    label = list(numeric.combination ~ "Numeric combination"),
    intercept = TRUE
  ) %>%
  as_gt() %>%
  tab_header(
    title = "Distributive responders"
  ) %>%
  gtsave(
    here(
      "report",
      "GLMER model summary for Distributive responders (interaction between Condition and Numeric combination).png"
    )
  )



###### Responses on target trials in the Baseline condition ######

# Assumption: During Block 1, participants' responses first fluctuated
# between the Distributive picture and the Cumulative picture.
# From a certain point on, they began to adopt a response strategy that
# they maintained the same interpretation (the favored interpretation)
# throughout the rest of Block 1.

# c.f. Rees and Bott (2018)


# Plot: Response ~ Trial.Number in Block 1
MyData %>%
  filter(
    Condition == "Baseline",
    trial.condition == "target"
  ) %>%
  ggplot(aes(
    y = distributive.target,
    x = as.numeric(Trial.Number),
    group = Schedule.ID, color = Schedule.ID
  )) +
  geom_line() +
  geom_point() +
  facet_wrap(distributive.rate ~ Schedule.ID) +
  # Theme
  theme(legend.position = "none") + # Remove legend
  # Scale
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = c(0, 1))

ggsave(here(
  "report",
  "Baseline behavior (Response ~ Trial number in Block 1).png"
),
width = 25,
height = 15
)


# Distributive choice (%): 1-8 vs 9-16

MyData %>%
  filter(
    Condition == "Baseline",
    trial.condition == "target"
  ) %>%
  group_by(Schedule.ID) %>%
  arrange(Trial.Number) %>%
  mutate(trial.order = row_number()) %>%
  mutate(
    trial.order.group = case_when(
      trial.order < 9 ~ "1-8",
      trial.order > 8 ~ "9-16"
    )
  ) %>%
  mutate(trial.order.group = factor(trial.order.group)) %>%
  group_by(responder.group, trial.order.group) %>%
  summarise(mean = mean(distributive.target))





###### Data viz leftovers ######

# This part is trivial.
# It only contains some leftover graphs (histograms and density plots).

###### Histogram ######

# The following histograms are created by `geom_histogram`
# which I first used for plotting.
# These histograms are similar to rigdeline plots.
# As it's not easy to create significance lines on the faceted histograms
# (have to use `annotation_custom2`),
# I finally moved to `geom_density_ridges` as in "Ridgeline plot"

# Histogram by Condition (N = 71)
rate %>%
  # Basic plot
  ggplot(aes(x = distributive.rate, fill = Condition)) +
  geom_histogram(bins = 5) +
  facet_wrap(vars(Condition),
    ncol = 1,
    strip.position = "right"
  ) +
  labs(
    x = "Distributive choice (%)",
    y = "Number of participants",
    fill = "Condition"
  ) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  # Means: numeric values
  geom_text(
    data = summary_condition,
    aes(
      x = mean, y = 9,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_condition,
    aes(
      x = mean, xend = mean,
      y = 4, yend = -4
    ),
    size = 0.5
  ) +
  # Medians: crosses
  geom_point(
    data = summary_condition,
    aes(x = median, y = 0),
    size = 1.2,
    shape = 4
  ) +
  # Add 95% CIs
  geom_errorbar(
    data = summary_condition,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = c(0, 0, 0, 0)
    ),
    width = 4,
    inherit.aes = FALSE
  ) +
  # Theme
  theme_classic() +
  # Remove background and y axis
  theme(
    panel.background = element_blank(),
    axis.line.y = element_blank(),
  ) +
  # Strips
  theme(
    strip.background.y = element_blank(),
    strip.text.y = element_text(angle = 0) # Horizontal y labels
  ) +
  # Remove legend
  theme(legend.position = "none")

# Histogram by Responder group (Baseline > 50%; Baseline < 50%) and condition
# N = 67 (71 - 4)
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate,
    fill = Condition
  )) +
  geom_histogram(bins = 5) +
  facet_grid(Condition ~ responder.group,
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  labs(
    x = "Distributive choice (%)",
    y = "Number of participants"
  ) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  # scale_y_continuous(breaks = NULL) +
  # Means: numeric values
  geom_text(
    data = summary_twogroups,
    aes(
      x = mean, y = 9,
      label = round(mean, digits = 1)
    ),
    size = 3
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = mean, xend = mean,
      y = 4, yend = -4
    ),
    size = 0.5
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(x = median, y = 0),
    size = 1.2,
    shape = 4
  ) +
  # Add 95% CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      xmin = lower.CI, xmax = upper.CI,
      y = c(0, 0, 0, 0, 0, 0, 0, 0)
    ),
    width = 4,
    inherit.aes = FALSE
  ) +
  # Theme
  theme_classic() +
  # Remove background and y axis
  theme(
    panel.background = element_blank(),
    axis.line.y = element_blank(),
    # axis.ticks.y = element_blank()
  ) +
  # Strips
  theme(
    strip.text.x = element_markdown(),
    strip.background.y = element_blank(),
    strip.text.y = element_text(angle = 0) # Horizontal y labels
  ) +
  # Remove legend
  theme(legend.position = "none")



###### Density plot ######

# Density plot by Condition (N = 71)
rate %>%
  ggplot(aes(
    x = distributive.rate,
    fill = Condition, color = Condition
  )) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Distributive choice (%)",
    y = "Density"
  ) +
  theme_classic()

# Density plot by Condition and Responder group (2 * 4)
# N = 67 (71 - 4)
rate_twogroups %>%
  mutate(responder.group = factor(responder.group,
    levels = c(
      "Baseline > 50%",
      "Baseline < 50%"
    )
  )) %>%
  # Basic plot
  ggplot(aes(
    y = distributive.rate,
    fill = Condition, color = Condition
  )) +
  geom_density() +
  facet_grid(
    responder.group ~ Condition,
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  labs(
    x = "Density",
    y = "Distributive choice (%)"
  ) +
  # Means: numeric values
  geom_text(
    data = summary_twogroups,
    aes(
      x = 0.015, y = mean,
      label = round(mean, digits = 1)
    ),
    size = 2.5,
    color = "Black"
  ) +
  # Means: vertical lines
  geom_segment(
    data = summary_twogroups,
    aes(
      x = 0.005, xend = -0.005,
      y = mean, yend = mean
    ),
    size = 0.01,
    color = "Black"
  ) +
  # Medians: crosses
  geom_point(
    data = summary_twogroups,
    aes(x = 0, y = median),
    size = 1, shape = 4,
    color = "Black"
  ) +
  # Add 95% CIs
  geom_errorbar(
    data = summary_twogroups,
    aes(
      ymin = lower.CI, ymax = upper.CI,
      x = c(0, 0, 0, 0, 0, 0, 0, 0)
    ),
    width = 0.005,
    inherit.aes = FALSE
  ) +
  # Theme
  theme_classic() +
  # Strips
  theme(
    strip.background.y = element_blank(),
    strip.text.y = element_markdown(angle = 0)
  ) +
  # Remove legend
  theme(legend.position = "none")

# Density plot by Responder group (panel by Condition)
rate_twogroups %>%
  # Basic plot
  ggplot(aes(
    x = distributive.rate,
    fill = Condition, color = Condition
  )) +
  geom_density(alpha = 0.3) +
  facet_wrap(
    vars(responder.group),
    labeller = labeller(responder.group = responder.group.labs)
  ) +
  labs(
    x = "Distributive choice (%)",
    y = "Density"
  ) +
  # Theme
  theme_classic() +
  # Strip
  theme(
    strip.text.x = element_markdown()
  )







# Session info:
sessionInfo()


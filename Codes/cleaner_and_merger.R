## Cleanup and Merge
## Descriptive Analyses

rm(list=ls())

library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(maps)
library(usdata)


setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/DC Inbox Data")

##### Clean up
dc_data <- read.csv("dc_data_coded.csv")

# Separate stance and reasoning
dc_data$Stance_reasoning <- sub(".*Reasoning: ?", "", dc_data$Stance)
dc_data$Stance <- sub("\\n.*", "", dc_data$Stance)

# Trim whitespace
dc_data$Stance <- trimws(dc_data$Stance)
dc_data$Stance_reasoning <- trimws(dc_data$Stance_reasoning)
dc_data$Stance_reasoning[dc_data$Stance_reasoning %in% c("Not Applicable", "SUPPORTS", "REJECTS")] <- NA
sum(!is.na(dc_data$Stance_reasoning)) # 138 reasoned by LLM

table(dc_data$TalksAboutDeepState, useNA = "always")
table(dc_data$Stance, useNA = "always")
table(dc_data$Stance, dc_data$Party)
# table(dc_data$Stance_reasoning)
table(dc_data$Stance, dc_data$State)

## Fix time
dc_data$Date <- as.POSIXct(dc_data$Unix.Timestamp, origin = "1970-01-01", tz = "UTC")
dc_data$Year <- format(dc_data$Date, "%Y")

## removing any dupes in Body
dc_data <- dc_data %>%
  distinct(Body, .keep_all = TRUE)

## executive branch control by year
dc_data <- dc_data %>%
  mutate(
    exec_branch_control = case_when(
      Unix.Timestamp >= 2010 & Unix.Timestamp <= 2016 ~ "Democrat",
      Unix.Timestamp >= 2017 & Unix.Timestamp <= 2020 ~ "Republican",
      Unix.Timestamp >= 2021 & Unix.Timestamp <= 2025 ~ "Democrat",
      TRUE ~ NA_character_))

## January 6 in Congress (117th)
dc_data$jan_6_in_office <- ifelse(dc_data$Congress == 117, 1, 0)

## certify 2020 election results - how they voted (Matched by first and last name and Congress number)
setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)")
elect <- read.csv("biden_election_vote.csv")
elect$First.Name <- iconv(elect$First.Name, from = "UTF-8", to = "ASCII//TRANSLIT")
elect$First.Name <- sub(" .*", "", elect$First.Name)
elect$Congress <- 117

dc_data <- dc_data %>%
  left_join(
    elect %>% select(First.Name, Last.Name, Congress, certify_2020_election),
    by = c("First.Name", "Last.Name", "Congress"))

table(dc_data$certify_2020_election)

rm(elect)

## Primary Endorsement by Trump
setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)")
trump_endorsements <- read.csv("trump_endorsements.csv")

dc_data <- dc_data %>%
  mutate(trump_endorsed = if_else(
    paste(First.Name, Last.Name) %in% paste(trump_endorsements$First.Name, trump_endorsements$Last.Name), "Yes", "No"))

table(dc_data$trump_endorsed, dc_data$Party)

rm(trump_endorsements)
#

##### MERGE DATA #####
## DW NOMINATE
setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/DW Nominate")
c111 <- read.csv("HS111_members.csv")
c112 <- read.csv("HS112_members.csv")
c113 <- read.csv("HS113_members.csv")
c114 <- read.csv("HS114_members.csv")
c115 <- read.csv("HS115_members.csv")
c116 <- read.csv("HS116_members.csv")
c117 <- read.csv("HS117_members.csv")
c118 <- read.csv("HS118_members.csv")
c119 <- read.csv("HS119_members.csv")

dw_nom <- rbind(c111, c112, c113, c114, c115, c116, c117, c118, c119)
rm(c111, c112, c113, c114, c115, c116, c117, c118, c119)

dw_nom$Congress <- dw_nom$congress
dw_nom$BioGuide.ID <- dw_nom$bioguide_id

dw_nom_subset <- dw_nom %>%
  select(Congress, BioGuide.ID, starts_with("nominate_"))

dc_data <- dc_data %>%
  left_join(dw_nom_subset, by = c("Congress", "BioGuide.ID")) # many expected relationships

rm(dw_nom, dw_nom_subset)

#

## Cleanup and Merge
## Descriptive Analyses

rm(list=ls())

library(dplyr)
library(magrittr)
library(tidyverse)
library(tidyr)
library(stringdist)
library(ggplot2)
library(patchwork)
library(scales)
library(maps)
library(usdata)
library(readxl)


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
    elect %>% 
      filter(Congress == 117) %>%
      select(First.Name, Last.Name, certify_2020_election), by = c("First.Name", "Last.Name"))

table(dc_data$certify_2020_election, useNA = "always")

rm(elect)

## Endorsement by Trump
setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)")
trump_endorsements <- read.csv("trump_endorsements.csv")

dc_data <- dc_data %>%
  mutate(trump_endorsed = if_else(Year < 2018, NA_character_,
    if_else(paste(First.Name, Last.Name) %in% paste(trump_endorsements$First.Name, trump_endorsements$Last.Name), "Yes", "No")))

table(dc_data$trump_endorsed, dc_data$Party)

rm(trump_endorsements)


## BioGuide IDs
library(yaml)
curr <- yaml::read_yaml("https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-current.yaml")
hist <- yaml::read_yaml("https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-historical.yaml")
legislators <- c(curr, hist)
leg_df <- lapply(legislators, function(x) {
  data.frame(
    First.Name = x$name$first,
    Last.Name = x$name$last,
    State = tail(x$terms, 1)[[1]]$state,
    Bioguide = x$id$bioguide,
    stringsAsFactors = FALSE)}) %>% 
  bind_rows()

## Cook PVI Data
pvi <- read_excel("Cook PVI 1997-2025 v3.xlsx")

pvi$State <- state.abb[match(pvi$State, state.name)]

pvi <- pvi %>%
  mutate(Cook_PVI = ifelse(Cook_PVI == "EVEN", "+0", Cook_PVI))

pvi <- pvi %>%
  mutate(Name = trimws(Name)) %>% 
  mutate(Name = ifelse(Name == "", NA, Name)) %>%
  separate(Name, into = c("Last.Name", "First.Name"),
    sep = ",\\s*", fill = "right", extra = "merge") %>%
  mutate(First.Name = trimws(First.Name))

has_space <- grepl("\\s", pvi$Last.Name)
split_names <- strsplit(pvi$Last.Name[has_space], "\\s+", perl = TRUE)
pvi$First.Name[has_space] <- sapply(split_names, `[`, 1)
pvi$Last.Name[has_space]  <- sapply(split_names, function(x) paste(x[-1], collapse = " "))
rm(has_space, split_names)

pvi_joined <- pvi %>%
  left_join(leg_df %>% select(First.Name, Last.Name, State, Bioguide), by = c("First.Name", "Last.Name", "State"))
pvi_joined <- pvi_joined %>%
  mutate(
    First.Name = ifelse(First.Name == "Scott" & Last.Name == "Franklin", "Franklin", First.Name),
    Last.Name  = ifelse(First.Name == "Franklin" & Last.Name == "Franklin", "Scott", Last.Name))


pvi_na <- pvi_joined %>%
  filter(is.na(Bioguide))
pvi_na_filled <- pvi_na %>%
  select(-Bioguide) %>% 
  left_join(leg_df %>% select(Last.Name, State, Bioguide), by = c("Last.Name", "State"))
pvi_final <- pvi_joined %>%
  filter(!is.na(Bioguide)) %>%   
  bind_rows(pvi_na_filled) 
sum(is.na(pvi_final$Bioguide)) # 208 still missing (issues because of names in PVI)


## fuzzy pass
pvi_missing <- pvi_final %>%
  filter(is.na(Bioguide)) %>%
  mutate(
    Full.Name = tolower(paste(First.Name, Last.Name)),
    State = toupper(trimws(State)))

leg_df_norm <- leg_df %>%
  mutate(
    Full.Name = tolower(paste(First.Name, Last.Name)),
    State = toupper(trimws(State)))

find_closest_full <- function(full_name, state, leg_df){
  candidates <- leg_df %>% filter(State == state)
  if(nrow(candidates) == 0) return(data.frame(Bioguide = NA_character_))
  distances <- stringdist(full_name, candidates$Full.Name, method = "jw")  # Jaro-Winkler
  best_idx <- which.min(distances)
  candidates[best_idx, "Bioguide", drop = FALSE]
}

closest_matches <- lapply(1:nrow(pvi_missing), function(i){
  find_closest_full(pvi_missing$Full.Name[i], pvi_missing$State[i], leg_df_norm)
})

closest_matches_df <- bind_rows(closest_matches)
pvi_missing$Bioguide <- closest_matches_df$Bioguide
pvi_final <- pvi_final %>%
  filter(!is.na(Bioguide)) %>%
  bind_rows(pvi_missing) 

sum(is.na(pvi_final$Bioguide)) 

pvi_final <- pvi_final %>%
  rename(BioGuide.ID = Bioguide,
         Disctrict_PVI = District)


## join into dc_data
dc_data <- dc_data %>%
  left_join(pvi_final %>%
      select(Disctrict_PVI, Cook_PVI, Raw_Cook_PVI, BioGuide.ID), by = "BioGuide.ID")
sum(is.na(dc_data$Cook_PVI)) # 709 NA (issues coming from PVI data)


rm(list = setdiff(ls(), c("dc_data", "leg_df")))
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


## SAVING
write.csv(dc_data, "dc_data_merged.csv", row.names = F)
#

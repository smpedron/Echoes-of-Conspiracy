## Data cleanup

rm(list=ls())

library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/DC Inbox Data")

dc_data <- read.csv("consolidated_data_082025.csv")


## removing cases with no valid Congress number
dc_data <- dc_data %>% 
  filter(Congress >= 111 & Congress <= 119)
table(dc_data$Congress)

## making sure no cases with NA in Body
dc_data <- dc_data %>% 
  filter(!is.na(Body))

## making sure no duplicates in ID
dupes_data <- dc_data %>%
  filter(ID %in% ID[duplicated(ID)]) # duplicates

dc_data <- dc_data %>% 
  distinct(ID, .keep_all = TRUE) # keep only first instance


## checks
table(dc_data$Party)
table(dc_data$Party, dc_data$Congress)


## phrases related to deep state conspiracy
phrases <- c("shadow government", "illuminati", "deepstate", "deep state", "deep-state",  "cabal",
             "puppeteer", "puppet", "invisible government", "regime", "secret government", "behind-the-scenes", "behind the scenes") 

## convert body to all lower case, keeps rows that contain wanted phrases
dc_data_deepstate_filtered <- dc_data %>%
  mutate(Body = str_to_lower(Body)) %>%  
  filter(str_detect(Body, str_c(phrases, collapse = "|"))) 

## checks
table(dc_data_deepstate_filtered$Party)
table(dc_data_deepstate_filtered$Congress, dc_data_deepstate_filtered$Party)

## saving
write.csv(dc_data_deepstate_filtered, "dcdata_deepstate_filtered_082025.csv", row.names = F)



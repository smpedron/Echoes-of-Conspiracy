## Data consolidation code

rm(list=ls())

library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/DC Inbox Data")

data1 <- read.csv("dcinbox_export_111_partial.csv")
data2 <- read.csv("dcinbox_export_112.csv")
data3 <- read.csv("dcinbox_export_113.csv")
data4 <- read.csv("dcinbox_export_114.csv")
data5 <- read.csv("dcinbox_export_115.csv")
data6 <- read.csv("dcinbox_export_116.csv")
data7 <- read.csv("dcinbox_export_117.csv")
data8 <- read.csv("dcinbox_export_118.csv")
data9 <- read.csv("dcinbox_export_119th_through_8_4_2025.csv")


cols_to_keep <- c("Subject", "Body", "Unix.Timestamp", "BioGuide.ID", "Congress",
                  "First.Name", "Last.Name", "Date.of.Birth", "Gender", "State",
                  "District", "Party", "Chamber", "Nickname", "ID") 

## classes differ by dataset, converting everything to character
all_data <- bind_rows(
  lapply(list(data1, data2, data3, data4, data5, data6, data7, data8, data9), function(df) {
    df %>%
      mutate(across(everything(), as.character)) %>%  
      select(all_of(cols_to_keep))
  })
)

## converting needed variables back
all_data <- all_data %>% 
  mutate(Congress = as.numeric(Congress),
         ID = as.numeric(ID), 
         Date.of.Birth = as.Date(Date.of.Birth, format = "%Y-%m-%d"),
         Unix.Timestamp = as.numeric(Unix.Timestamp) / 1000,
         Unix.Timestamp = as.POSIXct(Unix.Timestamp, origin = "1970-01-01", tz = "UTC"))

## saving
write.csv(all_data, "consolidated_data_082025.csv", row.names = FALSE)



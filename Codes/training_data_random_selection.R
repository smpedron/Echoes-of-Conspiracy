## randomly selecting 200 for manual coding

rm(list=ls())

library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/DC Inbox Data")

dc_inbox <- read.csv("dcdata_deepstate_filtered_082025.csv")

set.seed(5461328)

training_data <- dc_inbox %>% 
  sample_n(200)

write.csv(training_data, "training_data_dc_inbox.csv", row.names = F)

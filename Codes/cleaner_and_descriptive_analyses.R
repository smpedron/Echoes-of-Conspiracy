## Cleanup and Descriptive Analyses

rm(list=ls())

library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

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
sum(!is.na(dc_data$Stance_reasoning)) # 138 reasoned

table(dc_data$TalksAboutDeepState, useNA = "always")
table(dc_data$Stance, useNA = "always")
table(dc_data$Stance, dc_data$Party)
# table(dc_data$Stance_reasoning)

## Fix time
dc_data$Date <- as.POSIXct(dc_data$Unix.Timestamp, origin = "1970-01-01", tz = "UTC")
dc_data$Year <- format(dc_data$Date, "%Y")


##### BY YEAR AND PARTY ######
stance_by_year_party <- dc_data %>%
  filter(Stance != "Not Applicable") %>%  
  group_by(Year, Party, Stance) %>%
  summarise(count = n(), .groups = "drop")

supports_data <- subset(stance_by_year_party, Stance == "SUPPORTS")
rejects_data  <- subset(stance_by_year_party, Stance == "REJECTS")

p_supports1 <- ggplot(supports_data, aes(x = as.numeric(Year), y = count, color = Party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  scale_x_continuous(limits = c(2010, 2025), breaks = 2010:2025) +
  labs(
    title = "SUPPORTS",
    x = "Year",
    y = "Number of Newsletters",
    color = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97")
  )


p_rejects1 <- ggplot(rejects_data, aes(x = as.numeric(Year), y = count, color = Party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  scale_x_continuous(limits = c(2010, 2025), breaks = 2010:2025) +
  labs(
    title = "REJECTS",
    x = "Year",
    y = "Number of Newsletters",
    color = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97")
  )

p_supports1 / p_rejects1 + plot_layout(ncol = 1)


#
##### BY YEAR AND CHAMBER #####
stance_by_year_chamber <- dc_data %>%
  filter(Stance != "Not Applicable") %>%  
  group_by(Year, Chamber, Stance) %>%
  summarise(count = n(), .groups = "drop")

supports_data <- subset(stance_by_year_chamber, Stance == "SUPPORTS")
rejects_data  <- subset(stance_by_year_chamber, Stance == "REJECTS")

p_supports2 <- ggplot(supports_data, aes(x = as.numeric(Year), y = count, color = Chamber)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("House" = "#1f77b4", "Senate" = "#ff7f0e")) +
  scale_x_continuous(limits = c(2010, 2025), breaks = 2010:2025) +
  labs(
    title = "SUPPORTS",
    x = "Year",
    y = "Number of Newsletters",
    color = "Chamber"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97")
  )

p_rejects2 <- ggplot(rejects_data, aes(x = as.numeric(Year), y = count, color = Chamber)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("House" = "#1f77b4", "Senate" = "#ff7f0e")) +
  scale_x_continuous(limits = c(2010, 2025), breaks = 2010:2025) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "REJECTS",
    x = "Year",
    y = "Number of Newsletters",
    color = "Chamber"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97")
  )

p_supports2 / p_rejects2 + plot_layout(ncol = 1)


#
##### BY CONGRESS NUMBER & PARTY #####

## Subset
stance_by_congress_party <- dc_data %>%
  filter(Stance != "Not Applicable") %>%  
  group_by(Congress, Party, Stance) %>%
  summarise(count = n(), .groups = "drop")

supports_data <- subset(stance_by_congress_party, Stance == "SUPPORTS")
rejects_data  <- subset(stance_by_congress_party, Stance == "REJECTS")

## Plot SUPPORTS
p_supports3 <- ggplot(supports_data, aes(x = Congress, y = count, color = Party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  scale_x_continuous(limits = c(111, 119), breaks = 111:119) +
  labs(
    title = "SUPPORTS",
    x = "Congress",
    y = "Number of Newsletters",
    color = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97")
  )

## Plot REJECTS
p_rejects3 <- ggplot(rejects_data, aes(x = Congress, y = count, color = Party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  scale_x_continuous(limits = c(111, 119), breaks = 111:119) +
  labs(
    title = "REJECTS",
    x = "Congress",
    y = "Number of Newsletters",
    color = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97")
  )

## stack
p_supports3 / p_rejects3 + plot_layout(ncol = 1)

#




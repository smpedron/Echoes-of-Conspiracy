## Descriptive Analyses

rm(list=ls())

source("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/Codes/cleaner_and_merger.r")


table(dc_data$jan_6_in_office, dc_data$Stance, useNA = "always")
table(dc_data$trump_endorsed, dc_data$Stance, useNA = "always")
table(dc_data$Party, dc_data$TalksAboutDeepState)

library(multiwayvcov)
library(lmtest)

model1 <- glm(TalksAboutDeepState ~ nominate_dim1 + nominate_dim2 + Gender + jan_6_in_office +
              trump_endorsed + certify_2020_election, 
            dc_data, family = binomial(link = "logit"))
vcov_2way <- cluster.vcov(model1, cbind(dc_data$BioGuide.ID, dc_data$Chamber)) # cluster by congress member and chamber
coeftest(model1, vcov = vcov_2way)


#
##### DW-NOMINATE SCORE #####

## NOMINATE DIM 1
## Bin NOMINATE dimension one (-1 to 1 in 0.1 increments)
dc_data_binned <- dc_data %>%
  filter(Stance != "Not Applicable") %>%
  mutate(nominate_bin = cut(
    nominate_dim1,
    breaks = seq(-1, 1, by = 0.1),
    include.lowest = TRUE,
    right = FALSE
  )) %>%
  group_by(nominate_bin, Party, Stance) %>%
  summarise(count = n(), .groups = "drop")

supports_data <- subset(dc_data_binned, Stance == "SUPPORTS")
rejects_data  <- subset(dc_data_binned, Stance == "REJECTS")

## SUPPORTS plot
p_supports <- ggplot(supports_data, aes(x = nominate_bin, y = count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(
    title = "SUPPORTS",
    x = "Ideology (NOMINATE Dimension 1, binned)",
    y = "Number of Newsletters",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

## REJECTS plot
p_rejects <- ggplot(rejects_data, aes(x = nominate_bin, y = count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(
    title = "REJECTS",
    x = "Ideology (NOMINATE Dimension 1, binned)",
    y = "Number of Newsletters",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

## Combine
p_supports / p_rejects + plot_layout(ncol = 1)



## NOMINATE DIM 2
dc_data_binned <- dc_data %>%
  filter(Stance != "Not Applicable") %>%
  mutate(nominate_bin = cut(
    nominate_dim2,
    breaks = seq(-1, 1, by = 0.1),
    include.lowest = TRUE,
    right = FALSE
  )) %>%
  group_by(nominate_bin, Party, Stance) %>%
  summarise(count = n(), .groups = "drop")

supports_data <- subset(dc_data_binned, Stance == "SUPPORTS")
rejects_data  <- subset(dc_data_binned, Stance == "REJECTS")

p_supports <- ggplot(supports_data, aes(x = nominate_bin, y = count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(
    title = "SUPPORTS",
    x = "Ideology (NOMINATE Dimension 1, binned)",
    y = "Number of Newsletters",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

p_rejects <- ggplot(rejects_data, aes(x = nominate_bin, y = count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(
    title = "REJECTS",
    x = "Ideology (NOMINATE Dimension 2, binned)",
    y = "Number of Newsletters",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

p_supports / p_rejects + plot_layout(ncol = 1)

#
##### BY YEAR AND PARTY ######
setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)")

year_to_congress <- data.frame(
  Year = 2010:2025,
  Congress = c(rep(111, 1), rep(112, 2), rep(113, 2), rep(114, 2),
               rep(115, 2), rep(116, 2), rep(117, 2), rep(118, 2), 119))

year_to_congress$Year <- as.character(year_to_congress$Year)

stance_by_year_party <- dc_data %>%
  filter(Stance != "Not Applicable") %>%  
  group_by(Year, Party, Stance) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(year_to_congress, by = "Year") %>%
  mutate(YearLabel = paste0(Year, "\n(", Congress, ")"))

supports_data <- subset(stance_by_year_party, Stance == "SUPPORTS")
rejects_data  <- subset(stance_by_year_party, Stance == "REJECTS")

# SUPPORTS
p_supports1 <- ggplot(supports_data, aes(x = YearLabel, y = count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(
    title = "SUPPORTS",
    x = "Year (Congress)",
    y = "Number of Newsletters",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8, vjust = 1, hjust = 0.5, angle = 45,),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

# REJECTS
p_rejects1 <- ggplot(rejects_data, aes(x = YearLabel, y = count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(
    title = "REJECTS",
    x = "Year (Congress)",
    y = "Number of Newsletters",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8, vjust = 1, hjust = 0.5, angle = 45,),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

# Combine vertically
p_supports1 / p_rejects1 + plot_layout(ncol = 1)

#
##### BY YEAR AND CHAMBER #####
year_to_congress <- data.frame(
  Year = 2010:2025,
  Congress = c(rep(111, 1), rep(112, 2), rep(113, 2), rep(114, 2),
               rep(115, 2), rep(116, 2), rep(117, 2), rep(118, 2), 119))

year_to_congress$Year <- as.character(year_to_congress$Year)

stance_by_year_chamber <- dc_data %>%
  filter(Stance != "Not Applicable") %>%  
  group_by(Year, Chamber, Stance) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(year_to_congress, by = "Year") %>% 
  mutate(YearLabel = paste0(Year, "\n(", Congress, ")"))

supports_data <- subset(stance_by_year_chamber, Stance == "SUPPORTS")
rejects_data  <- subset(stance_by_year_chamber, Stance == "REJECTS")

p_supports2 <- ggplot(supports_data, aes(x = YearLabel, y = count, fill = Chamber)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("House" = "#1f77b4", "Senate" = "#ff7f0e")) +
  labs(
    title = "SUPPORTS",
    x = "Year (Congress)",
    y = "Number of Newsletters",
    fill = "Chamber"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8, vjust = 1, hjust = 0.5, angle = 45),
    panel.background = element_rect(fill = "grey97", color = "grey97")
  )

p_rejects2 <- ggplot(rejects_data, aes(x = YearLabel, y = count, fill = Chamber)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("House" = "#1f77b4", "Senate" = "#ff7f0e")) +
  labs(
    title = "REJECTS",
    x = "Year (Congress)",
    y = "Number of Newsletters",
    fill = "Chamber"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 8, vjust = 1, hjust = 0.5, angle = 45),
    panel.background = element_rect(fill = "grey97", color = "grey97")
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

##### State Heatmap ######

us_map <- map_data("state")

abbr2state <- function(x) {
  sapply(x, function(y) {
    y <- toupper(y)
    if (y %in% state.abb) {
      tolower(state.name[match(y, state.abb)])
    } else if (tolower(y) %in% c("dc", "d.c.", "washington dc")) {
      "district of columbia"
    } else {
      NA
    }
  })
}


dc_data <- dc_data %>%
  mutate(region = abbr2state(State)) %>%
  filter(!is.na(region))

## Count newsletters per state × stance
state_counts <- dc_data %>%
  filter(Stance != "Not Applicable") %>%
  group_by(region, Stance) %>%
  summarise(newsletters = n(), .groups = "drop")

## Compute centroids for each state
state_centers <- us_map %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)))

## Merge counts with centroids
plot_data <- left_join(state_counts, state_centers, by = "region")

## plot
ggplot() +
  # Base map (gray states)
  geom_polygon(
    data = us_map,
    aes(x = long, y = lat, group = group),
    fill = "gray90", color = "white"
  ) +
  
  # Red circles = "Support"
  geom_point(
    data = filter(plot_data, Stance == "SUPPORTS"),
    aes(x = long, y = lat, size = newsletters),
    color = "red", alpha = 0.6
  ) +
  
  # # Green circles = "Reject"
  # geom_point(
  #   data = filter(plot_data, Stance == "REJECTS"),
  #   aes(x = long, y = lat, size = newsletters),
  #   color = "forestgreen", alpha = 0.5
  # ) +
  
  scale_size_continuous(range = c(2, 18)) + # makes circles bigger
  guides(size = "none") + # removes legend
  
  # Map styling
  coord_fixed(1.3) +
  theme_void() +
  labs(
    title = "E-Newsletters Supporting Deep-State Narratives by State")


#

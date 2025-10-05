## Newsletter LLM Lite Code

rm(list=ls())

library(ellmer)
library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/Conspiratorial Rhetoric (Miller)/DC Inbox Data")

training_data <- read.csv("dcinbox_training.csv")
dc_data <- read.csv("dcdata_deepstate_filtered_082025.csv")

## API Key
Sys.setenv(OPENAI_API_KEY = "API HERE")

## LLM Classifier Function
classify_newsletter <- function(newsletter_text, training_data) {
  newsletter_text <- as.character(newsletter_text)
  
  training_examples <- paste(
    sapply(seq_len(nrow(training_data)), function(i) {
      paste0(
        "Newsletter: ", as.character(training_data$text[i]), "\n",
        "TalksAboutDeepState: ", as.character(training_data$talks[i]), "\n",
        "Stance: ", as.character(training_data$stance[i])
      )
    }),
    collapse = "\n\n"
  )
  
  system_msg <- "You are an expert analyst who classifies newsletters based on whether they amplify a deep state conspiracy narrative and their stance toward it."
  
  user_msg <- paste0(
    "Classify each newsletter with:\n",
    "- TalksAboutDeepState: 1 if the newsletter talks about any deep state conspiracy narrative, otherwise 0.\n",
    "- Stance: SUPPORTS if the newsletter supports or endorses the narrative; REJECTS if it challenges or rejects it.\n\n",
    "Here are labeled examples:\n\n",
    training_examples,
    "\n\nNow classify this newsletter:\n",
    newsletter_text,
    "\n\nRespond in this exact format:\nTalksAboutDeepState: [0 or 1]\nStance: [SUPPORTS, REJECTS, or Not Applicable]"
  )
  
  chat_obj <- chat_openai(system_prompt = system_msg, model = "GPT-4.1", base_url = "https://litellmproxy.osu-ai.org/")
  
  chat_obj$chat(user_msg)
  
  last_turn <- chat_obj$last_turn("assistant")
  
  # Capture printed output as text
  reply_lines <- capture.output(print(last_turn))
  reply_text <- paste(reply_lines, collapse = "\n")
  
  #trimws(reply_text)
  # Extract the values using regex
  talks <- ifelse(grepl("TalksAboutDeepState: 1", reply_text), 1, 0)
  stance <- sub(".*Stance: (.*)", "\\1", reply_text)
  
  return(c(TalksAboutDeepState = talks, Stance = stance))
}

#

###### Running LLM #####
dc_data$TalksAboutDeepState <- NA
dc_data$Stance <- NA

for (i in seq_len(nrow(dc_data))) {
  cat("Classifying newsletter ", i, " of ", nrow(dc_data), "\n")
  result <- classify_newsletter(dc_data$Body[i], training_data)
  dc_data$TalksAboutDeepState[i] <- result["TalksAboutDeepState"]
  dc_data$Stance[i] <- result["Stance"]
}

write.csv(dc_data, "dc_data_coded.csv", row.names = F)

head(dc_data)

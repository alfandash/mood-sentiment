library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(shinyjs)
library(shinyWidgets)

# Wrangling and Plotting Package
library(tidyverse)

library(tokenizers)

#Visualization
library(plotly)
library(glue)
library(wordcloud)
library(RColorBrewer)

test <- "halo string"


#Twitter EDA
twit_df <- read.csv("./data/17april2019.csv")

twit_df <- twit_df %>% 
  mutate(text = as.character(text),
         text_clean = as.character(text_clean))

twit_label_df <- twit_df %>% 
  group_by(label) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

stop_words <- readLines("./data/stopword_list.txt")

twit_df <- twit_df %>%
  mutate(text_clean = tokenize_words(text_clean, stopwords = stop_words))

twit_tokenize <- twit_df

twit_df_clean <- twit_df %>% 
  mutate(label_num = factor(label, levels = c("anger", "fear", "sadness", "happy", "love")),
         label_num = as.numeric(label_num),
         label_num = label_num-1) %>%
  mutate(text_clean = sapply(text_clean, toString),
         text_clean = gsub(",", ' ', text_clean)) %>% 
  select(text_clean, label_num) %>% 
  na.omit()


# Spotfy EDA

tracks_df <- read.csv("./data/spotify/spotify-all-genre-recommendations-list.csv")

track_audio_feature_df <- read.csv("./data/spotify/spotify-audiofeature-list.csv")

track_audio_feature_clean <- track_audio_feature_df %>% 
  select(-c("analysis_url", "key", "mode", "type", "uri", "analysis_url", "time_signature"))

all_track_feature_df <- merge(tracks_df,track_audio_feature_clean, by = "id")

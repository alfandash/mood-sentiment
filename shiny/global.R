library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(shinyjs)
library(shinyWidgets)

# Wrangling and Plotting Package
library(tidyverse)
library(scales)
library(tokenizers)
library(rsample)
library(RVerbalExpressions)
library(textclean)
library(jsonlite)
library(stringi)
library(spotifyr)
library(httr)
library(katadasaR)

#modelling package
library(e1071)
library(caret)
library(factoextra)
library(class)
library(keras)
library(randomForest)
library(yardstick)
library(tensorflow)

#Visualization
library(plotly)
library(glue)
library(wordcloud)
library(RColorBrewer)
library(fmsb)

library(FactoMineR)
library(slam)
library(tm)
library(shinycssloaders)

test <- "halo string"


#Twitter EDA
twit_df <- read.csv("./data/05May2020.csv")

twit_df <- twit_df %>% 
  mutate(text = as.character(text),
         text_token = as.character(text_token))

twit_label_df <- twit_df %>% 
  group_by(label) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

stop_words <- readLines("./data/stopword_list.txt")

twit_df <- twit_df %>%
  mutate(text_token = tokenize_words(text_token, stopwords = stop_words))

twit_tokenize <- twit_df

twit_df_clean <- twit_df %>% 
  mutate(label_num = factor(label, levels = c("anger", "fear", "sadness", "happy", "love")),
         label_num = as.numeric(label_num),
         label_num = label_num-1) %>%
  mutate(text_token = sapply(text_token, toString),
         text_token = gsub(",", ' ', text_token)) %>% 
  select(text_token, label_num) %>% 
  na.omit()


# Spotfy EDA

tracks_df <- read.csv("./data/spotify/spotify-all-genre-recommendations-list-cluster.csv")
track_audio_feature_df <- read.csv("./data/spotify/spotify-audiofeature-list.csv")
genre_df <- read.csv("./data/spotify/genre-list.csv")

track_audio_feature_clean <- track_audio_feature_df %>% 
  select(-c("analysis_url", "key", "mode", "type", "uri", "analysis_url", "time_signature"))

all_track_feature_df <- merge(tracks_df,track_audio_feature_clean, by = "id")


# Spotify Clustering

track_audio_feature_num <- track_audio_feature_clean %>% select_if(is.numeric)
track_audio_feature_scale <- track_audio_feature_num %>% mutate(loudness = rescale(loudness),
                                                                tempo = rescale(tempo))

# Modelling Song 

matrix_result <- function(matrix, model_name) {
  matrix_1 <- as.data.frame(t(as.matrix(matrix, what = "overall")))
  matrix_2 <- as.data.frame(t(as.matrix(matrix, what = "classes")))
  Model <- c(model_name)
  matrix_result <- cbind(Model, matrix_1, matrix_2)
  matrix_result <- matrix_result %>% select(Model, Accuracy, Sensitivity, Specificity, "Pos Pred Value")
  return(matrix_result)
}

all_track_clean_df <- all_track_feature_df %>% 
  select(c("artist.name",
           "name",
           "album.name.1",
           "popularity", 
           "danceability", 
           "energy", 
           "loudness", 
           "speechiness", 
           "acousticness", 
           "instrumentalness", 
           "liveness", 
           "valence", 
           "tempo", 
           "cluster", 
           "genre")) %>% 
  mutate(cluster = as.factor(cluster))

track_modelling_df <- all_track_clean_df %>% 
  select(4:14)

set.seed(100)
initial_train <- initial_split(data = track_modelling_df, prop = 0.8, strata = "cluster")

data_train <- training(initial_train)
data_test <- testing(initial_train)

data_train_x <- data_train[,-11]
data_train_x <- scale(data_train_x, center = T)
data_test_x <- data_test[,-11]
data_test_x <- scale(data_test_x, center = T)
data_train_y <- data_train$cluster
data_test_y <- data_test$cluster


pred_knn <- knn(train = data_train_x,
                test = data_test_x,
                cl = data_train_y,
                k = round(sqrt(nrow(data_train_x))))
matrix_knn <- confusionMatrix(as.factor(pred_knn), as.factor(data_test_y), positive = "1")
result_knn <- as.data.frame(matrix_knn$table)

model_naivebayes <- naiveBayes(formula = cluster~., data = data_train, laplace = 1)
pred_naive_bayes <- predict(model_naivebayes, data_test)
matrix_naivebayes <- confusionMatrix(as.factor(pred_naive_bayes), as.factor(data_test_y), positive = "1")
result_naivebayes <- as.data.frame(matrix_naivebayes$table)

model_random_forest <- readRDS("./model/random_forest.rds")
pred_random_forest <- predict(model_random_forest, data_test)
matrix_random_forest <- confusionMatrix(as.factor(pred_random_forest), as.factor(data_test_y), positive = "1")
result_random_forest <- as.data.frame(matrix_random_forest$table)

accuracy_knn <-  as.data.frame(t(as.matrix(matrix_knn, what = "overall"))) %>% 
  mutate(Model = 'KNN') %>% 
  select(Model, Accuracy)

accuracy_naivebayes <-  as.data.frame(t(as.matrix(matrix_naivebayes, what = "overall"))) %>% 
  mutate(Model = 'Naive Bayes') %>% 
  select(Model, Accuracy)

accuracy_random_forest <-  as.data.frame(t(as.matrix(matrix_random_forest, what = "overall"))) %>% 
  mutate(Model = 'Random Foest') %>% 
  select(Model, Accuracy)

# Modelling twit
num_words <- 16384
# maxlen <- max(str_count(twit_df_clean$text_token, "\\w+")) + 1 
maxlen <- 49
tokenizer <- text_tokenizer(num_words = num_words,
                            lower = TRUE) %>% 
  fit_text_tokenizer(twit_df_clean$text_token)


#save model
model_lstm <- load_model_hdf5("./model/model-05052020_ver_4.h5")

# Save an object to a file
history_lstm <- readRDS(file = "./model/history-model-05052020_ver_4.rds")

# Save an object to a file
history_df <- readRDS(file = "./model/history-df-model-05052020_ver_4.rds")

initial_train <- readRDS(file = "./data/initial_train_twitter.rds")
initial_validation <- readRDS(file = "./data/initial_validation_twitter.rds")

data_train_lstm <- training(initial_train)
data_test_lstm <- training(initial_train)
data_validation_lstm <- training(initial_validation)
data_test_lstm <- testing(initial_validation)

data_train_lstm_x <- texts_to_sequences(tokenizer, data_train_lstm$text_token) %>% 
  pad_sequences(maxlen = maxlen)
data_validation_lstm_x <- texts_to_sequences(tokenizer, data_validation_lstm$text_token) %>% 
  pad_sequences(maxlen = maxlen)
data_test_lstm_x <- texts_to_sequences(tokenizer, data_test_lstm$text_token) %>% 
  pad_sequences(maxlen = maxlen)


data_train_lstm_y <- tf$keras$utils$to_categorical(data_train_lstm$label_num, num_classes = as.integer(5))
data_validation_lstm_y <- tf$keras$utils$to_categorical(data_validation_lstm$label_num, num_classes = as.integer(5))
data_test_lstm_y <- tf$keras$utils$to_categorical(data_test_lstm$label_num, num_classes = as.integer(5))

data_train_pred_lstm <- model_lstm %>% 
  predict_classes(data_train_lstm_x) %>% 
  as.vector()

data_validation_pred_lstm <- model_lstm %>% 
  predict_classes(data_validation_lstm_x) %>% 
  as.vector()

data_test_pred_lstm <- model_lstm %>% 
  predict_classes(data_test_lstm_x) %>% 
  as.vector()

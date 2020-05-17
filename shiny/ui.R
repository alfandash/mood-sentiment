twitter_eda <- fluidRow(
  box(width = 6, 
      plotlyOutput(outputId = "plotSummaryLabel")
  ),
  box(width = 6,
      wordcloud2Output(outputId = "twitWordCloud")
  ),
  box(width = 12,
      box(width = 12,
          box(width = 2,
              selectInput(
                inputId = "selectMood",
                label = "Select Mood",
                choices = list("Anger" = "anger",
                               "Sadness" = "sadness",
                               "Fear" = "fear",
                               "Happy" = "happy",
                               "love" = "love")
              )
          ),
          ),
      box(width = 8,
          plotlyOutput(outputId = "plotFreqWordByMood")
          ),
      valueBoxOutput(outputId = "totalCharByMood"),
      valueBoxOutput(outputId = "uniqueWordByMood"),
      valueBoxOutput(outputId = "maxWordByMood"),
  ),
)

spotify_eda <- fluidRow(
  box(width = 12,
      valueBoxOutput(outputId = "totalGenre"),
      valueBoxOutput(outputId = "totalTracks"),
      valueBoxOutput(outputId = "totalArtists"),
      ),
  box(width = 6,
      plotlyOutput(outputId = "topAveragePopularity")
      ),
  box(width = 6,
      plotlyOutput(outputId = "topArtistAlbumPopularity")
      ),
  box(width = 12,
      box(width = 2,
          selectInput(
            inputId = "selectGenre",
            label = "Select Genre",
            choices = genre_df$genre
          )
      ),
  ),
  box(width = 6,
      plotlyOutput(outputId = "distributionPopularityByGenre")
      ),
  box(width = 6,
      plotlyOutput(outputId = "radarAudioFeatureByGenre")
  )
)


eda_body <- fluidRow(
  tabBox(width = 12,
         tabPanel("Spotify", spotify_eda),
         tabPanel("Twitter", twitter_eda)
  )
)

eda <- tabPanel("Data Exploration",icon = icon("compass"),
                  eda_body
)

cluster1 <- fluidRow(
  box(width = 12,
      h3("Cluster 1: Love Mood"),
      p("High accousticness, danceability, Mid valence, energy, Low instrumentalness, speechiness")
  ),
  box(width = 12,
      box(width = 4,
          plotlyOutput(outputId = "radarAudioFeatureCluster1")
      ),
      box(width = 5,
          plotlyOutput(outputId = "plotAudioFeaturClusterGenre1")
      ),
      # box(width = 4,
      #     h4("Song With Characteristics"),
      #     tableOutput(outputId = "tableArtistTitleGenre1")
      # )
      box(width = 3,
          tags$style(".popover{width:400px;}"),
          bsTooltip(id = "image_example", title = "This gauge shows the input value from the slider.", placement = "bottom"),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_1_1"),
                 uiOutput(outputId = "image_cluster_1_2"),
                 uiOutput(outputId = "image_cluster_1_3"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_1_4"),
                 uiOutput(outputId = "image_cluster_1_5"),
                 uiOutput(outputId = "image_cluster_1_6"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_1_7"),
                 uiOutput(outputId = "image_cluster_1_8"),
                 uiOutput(outputId = "image_cluster_1_9"),
          )
      ),
  ),
)

cluster2 <- fluidRow(
  box(width = 12,
      h3("Cluster 2: Energetic Mood"),
      p("High Energy, Mid Danceability and Valence, Low Acousticness and Instrumentalness")
  ),
  box(width = 12,
      box(width = 4,
          plotlyOutput(outputId = "radarAudioFeatureCluster2")
      ),
      box(width = 5,
          plotlyOutput(outputId = "plotAudioFeaturClusterGenre2")
      ),
      # box(width = 4, 
      #     h4("Song With Characteristics"),
      #     tableOutput(outputId = "tableArtistTitleGenre2")
      # )
      box(width = 3,
          tags$style(".popover{width:400px;}"),
          bsTooltip(id = "image_example", title = "This gauge shows the input value from the slider.", placement = "bottom"),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_2_1"),
                 uiOutput(outputId = "image_cluster_2_2"),
                 uiOutput(outputId = "image_cluster_2_3"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_2_4"),
                 uiOutput(outputId = "image_cluster_2_5"),
                 uiOutput(outputId = "image_cluster_2_6"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_2_7"),
                 uiOutput(outputId = "image_cluster_2_8"),
                 uiOutput(outputId = "image_cluster_2_9"),
          )
      ),
  ),
)

cluster3 <- fluidRow(
  box(width = 12,
      h3("Cluster 3: Swing Mood"),
      p(" High Energy and Dancebility and Instrumentalness, Mid Valence, Low Acousticness")
  ),
  box(width = 12,
      box(width = 4,
          plotlyOutput(outputId = "radarAudioFeatureCluster3")
      ),
      box(width = 5,
          plotlyOutput(outputId = "plotAudioFeaturClusterGenre3")
      ),
      # box(width = 4,
      #     h4("Song With Characteristics"),
      #     tableOutput(outputId = "tableArtistTitleGenre3")
      # )
      box(width = 3,
          tags$style(".popover{width:400px;}"),
          bsTooltip(id = "image_example", title = "This gauge shows the input value from the slider.", placement = "bottom"),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_3_1"),
                 uiOutput(outputId = "image_cluster_3_2"),
                 uiOutput(outputId = "image_cluster_3_3"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_3_4"),
                 uiOutput(outputId = "image_cluster_3_5"),
                 uiOutput(outputId = "image_cluster_3_6"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_3_7"),
                 uiOutput(outputId = "image_cluster_3_8"),
                 uiOutput(outputId = "image_cluster_3_9"),
          )
      ),
  ),
)

cluster4 <- fluidRow(
  box(width = 12,
      h3("Cluster 4: Chill and Sad Mood"),
      p("High acousticness and Instrumentalness, Low Valence and Energy and Danceability Speechiness")
  ),
  box(width = 12,
      box(width = 4,
          plotlyOutput(outputId = "radarAudioFeatureCluster4")
      ),
      box(width = 5,
          plotlyOutput(outputId = "plotAudioFeaturClusterGenre4")
      ),
      # box(width = 3,
      #     h4("Song With Characteristics"),
      #     tableOutput(outputId = "tableArtistTitleGenre4")
      # )
      box(width = 3,
          tags$style(".popover{width:400px;}"),
          bsTooltip(id = "image_example", title = "This gauge shows the input value from the slider.", placement = "bottom"),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_4_1"),
                 uiOutput(outputId = "image_cluster_4_2"),
                 uiOutput(outputId = "image_cluster_4_3"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_4_4"),
                 uiOutput(outputId = "image_cluster_4_5"),
                 uiOutput(outputId = "image_cluster_4_6"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_4_7"),
                 uiOutput(outputId = "image_cluster_4_8"),
                 uiOutput(outputId = "image_cluster_4_9"),
          )
      ),
  ),
)

cluster5 <- fluidRow(
  box(width = 12,
      h3("Cluster 5: Happy & Joy Mood"),
      p("High Danceability Valence Energy Speachness, Low Acousticness Instrumentalness")
  ),
  box(width = 12,
      box(width = 4,
          plotlyOutput(outputId = "radarAudioFeatureCluster5")
      ),
      box(width = 5,
          plotlyOutput(outputId = "plotAudioFeaturClusterGenre5")
      ),
      # box(width = 4, 
      #     h4("Song With Characteristics"),
      #     tableOutput(outputId = "tableArtistTitleGenre5")
      # )
      box(width = 3,
          tags$style(".popover{width:400px;}"),
          bsTooltip(id = "image_example", title = "This gauge shows the input value from the slider.", placement = "bottom"),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_5_1"),
                 uiOutput(outputId = "image_cluster_5_2"),
                 uiOutput(outputId = "image_cluster_5_3"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_5_4"),
                 uiOutput(outputId = "image_cluster_5_5"),
                 uiOutput(outputId = "image_cluster_5_6"),
          ),
          column(width = 4,
                 uiOutput(outputId = "image_cluster_5_7"),
                 uiOutput(outputId = "image_cluster_5_8"),
                 uiOutput(outputId = "image_cluster_5_9"),
          )
      ),
  ),
)

cluster_body <- fluidRow(class="text-justify",
                        tabBox(width = 12,
                               tabPanel("Cluster 1: Love Mood", cluster1),
                               tabPanel("Cluster 2: Energetic Mood", cluster2),
                               tabPanel("Cluster 3: Swing Mood", cluster3),
                               tabPanel("Cluster 4: Chill and Sad Mood", cluster4),
                               tabPanel("Cluster 5: Happy & Joy Mood", cluster5)
                               )
)

cluster <- tabPanel("Song Clustering",icon = icon("project-diagram"),
                    cluster_body
)

modelling_body <- fluidRow(
  column(width = 12,
         h2("Spotify Prediction Model"),
         p("A Playlist made from prediction cluster gets from audio feature each song get from Spotify. 
         We compare 3 prediction model to decide which model give the best result to predict it. 
           We can see Random Forest is the best model to predict clusters of song")
  ),
  column(width = 12,
         h3("Result Confusion Matrix"),
  ),
  column(width = 4, class="text-center",
      plotOutput(outputId = "confusionMatrixKnn"),
      h4("Accuracy"),
      h4(textOutput(outputId = "accuracyKnn"))
  ),
  column(width = 4, class="text-center",
      plotOutput(outputId = "confusionMatrixNaiveBayes"),
      h4("Accuracy"),
      h4(textOutput(outputId = "accuracyNaiveBayes"))
  ),
  column(width = 4, class="text-center",
      plotOutput(outputId = "confusionMatrixRandomForest"),
      h4("Accuracy"),
      h4(textOutput(outputId = "accuracyRandomForest"))
  ),
  column(width = 12,
         h2("Twitter Prediction Model"),
         p("Tweet prediction model we make from Naive Bayes Model and Neural Network with LSTM architecture. 
           After we train the model to predict mood based on the dataset we have, We faound Naive Bayes have better result:")
  ),
  column(width = 12,
         h3("Result Confusion Matrix"),
  ),
  column(width = 6, class="text-center",
         h3("Prediction With Naive Bayes"),
         plotOutput(outputId = "confusionMatrixNaiveBayesTweet"),
         h4("Accuracy"),
         h4(textOutput(outputId = "accuracyNaiveBayesTweet"))
  ),
  column(width = 6, class="text-center",
         h3("Prediction Neural Network"),
         plotOutput(outputId = "confusionMatrixNeuralNetworkTest"),
         h4("Accuracy"),
         h4(textOutput(outputId = "accuracyNeuralNetworkTest"))
  ),
)

modelling <- tabPanel("Modelling", icon = icon("robot"),
                      modelling_body
                      )

inputAccount <- fluidRow(
  column(width= 3,
    box(width = 12,
        textInput("tweetAccountInput", h3("Your Account")),
        textOutput("accountValidation"),
        br(),
        uiOutput(outputId = "selectGenreAccount"),
        actionButton("submitTwitAccountAction", "Recommend My Playlist"),
        br(),
        br(),
        h4("Profile Image"),
        uiOutput("userProfileImageTweet"),
        # imageOutput(outputId = "userProfileImageTweetDummy", height = "10%"),
        h4("User Name :"),
        p(textOutput("userScreenNameTweet")),
        h4("User Last Tweet : "),
        p(textOutput("userLastTweet")),
    )
  ),
  column(width = 5,
         h3(textOutput("sentimentTwitAccount")),
         h3(textOutput("selectedGenreAccount")),
         withSpinner(dataTableOutput(outputId = "recommendationPlaylistTableAccount"))
  ),
  column(width = 4,
         h3(textOutput("radarTitlePredictionAccount")),
         br(),
         plotlyOutput(outputId = "radarRecommendPlaylistAccount")
  )
)

inputText <-fluidRow(
  column(width = 3,
         box(width = 12,
             textInput("twitInput", h3("Your Twit")),
             textOutput("twitValidation"),
             br(),
             uiOutput(outputId = "selectGenre"),
             actionButton("submitTwitAction", "Recommend My Playlist"),
             # uiOutput(outputId = "submitTwitAction"),
             # textOutput("test"),
         )
  ),
  column(width = 5,
         h2(textOutput("sentimentTwit")),
         h3(textOutput("selectedGenre")),
         withSpinner(dataTableOutput(outputId = "recommendationPlaylistTable"))
  ),
  column(width = 4,
         h3(textOutput("radarTitlePrediction")),
         br(),
         plotlyOutput(outputId = "radarRecommendPlaylist")
  )
)

recommendation_body <- fluidRow(
  box(width = 12,
      h2("Mood Prediction and Playlist Recommendation"),
      ),
  tabBox(width = 12,
         tabPanel("Input",inputText),
         tabPanel("Twitter Account", inputAccount)
  )
)

recommendation <- tabPanel("Recommendation", icon = icon("chart-line"),
                          recommendation_body,
                          )

about_body <- fluidRow(class="text-justify",
                      box(width = 12,
                           h2("Background"),
                           ),
                      column(width = 6, style = "font-size: 150%;",
                          htmlOutput("aboutText"),
                          h2("Work Flow"),
                          imageOutput(outputId = "aboutImageFlow")
                            ),
                      column(width = 6,
                          plotlyOutput(outputId = "stressLevel"),
                          plotlyOutput(outputId = "socialMediaLevel"),
                          ),
)

about <- tabPanel("About",icon = icon("info"),
                  about_body
)

home <- tabPanel("Home",icon = icon("home"),
                 box(width = 12, align = "center",
                     imageOutput(outputId = "homeImageLanding")
                     # uiOutput(outputId = "previewTrack")
                 )
                 
)



navbar <- navbarPage("Moodify", theme = shinytheme("flatly"),
                     # modelling,
                     # cluster,
                     # eda,
                     # about,
                     # recommendation,
                     home,
                     about,
                     eda,
                     cluster,
                     modelling,
                     recommendation,
                     tags$head(tags$style(HTML('.box{
                                               -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;
                                               border-top: none;
                                               }'))),
                     header = tagList(
                       useShinydashboard()
                     ),
                     useShinyjs()
)

shinyUI(navbar)
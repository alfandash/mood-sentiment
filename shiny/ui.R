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
      plotOutput(outputId = "distributionPopularityByGenre")
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

cluster_body <- fluidRow(class="text-justify",
                         box(width = 12,
                             h3("Cluster 1: Love Mood"),
                             p("High accousticness, danceability, Mid valence, energy, Low instrumentalness, speechiness")
                             ),
                         box(width = 12,
                             box(width = 4,
                                 plotlyOutput(outputId = "radarAudioFeatureCluster1")
                                 ),
                             box(width = 4,
                                 plotlyOutput(outputId = "plotAudioFeaturClusterGenre1")
                                 ),
                             box(width = 4,
                                 h4("Song With Characteristics"),
                                 tableOutput(outputId = "tableArtistTitleGenre1")
                                 )
                             ),
                         box(width = 12,
                             h3("Cluster 2: Energetic Mood"),
                             p("High Energy, Mid Danceability and Valence, Low Acousticness and Instrumentalness")
                         ),
                         box(width = 12,
                             box(width = 4,
                                 plotlyOutput(outputId = "radarAudioFeatureCluster2")
                             ),
                             box(width = 4,
                                 plotlyOutput(outputId = "plotAudioFeaturClusterGenre2")
                             ),
                             box(width = 4, 
                                 h4("Song With Characteristics"),
                                 tableOutput(outputId = "tableArtistTitleGenre2")
                             )
                         ),
                         box(width = 12,
                             h3("Cluster 3: Swing Mood"),
                             p(" High Energy and Dancebility and Instrumentalness, Mid Valence, Low Acousticness")
                         ),
                         box(width = 12,
                             box(width = 4,
                                 plotlyOutput(outputId = "radarAudioFeatureCluster3")
                             ),
                             box(width = 4,
                                 plotlyOutput(outputId = "plotAudioFeaturClusterGenre3")
                             ),
                             box(width = 4,
                                 h4("Song With Characteristics"),
                                 tableOutput(outputId = "tableArtistTitleGenre3")
                             )
                         ),
                         box(width = 12,
                             h3("Cluster 4: Chill and Sad Mood"),
                             p("High acousticness and Instrumentalness, Low Valence and Energy and Danceability Speechiness")
                         ),
                         box(width = 12,
                             box(width = 4,
                                 plotlyOutput(outputId = "radarAudioFeatureCluster4")
                             ),
                             box(width = 4,
                                 plotlyOutput(outputId = "plotAudioFeaturClusterGenre4")
                             ),
                             box(width = 4,
                                 h4("Song With Characteristics"),
                                 tableOutput(outputId = "tableArtistTitleGenre4")
                             )
                         ),
                         box(width = 12,
                             h3("Cluster 5: Happy & Joy Mood"),
                             p("High Danceability Valence Energy Speachness, Low Acousticness Instrumentalness")
                         ),
                         box(width = 12,
                             box(width = 4,
                                 plotlyOutput(outputId = "radarAudioFeatureCluster5")
                             ),
                             box(width = 4,
                                 h4("Song With Characteristics"),
                                 plotlyOutput(outputId = "plotAudioFeaturClusterGenre5")
                             ),
                             box(width = 4, 
                                 tableOutput(outputId = "tableArtistTitleGenre5")
                             )
                         ),
)

cluster <- tabPanel("Song Clustering",icon = icon("project-diagram"),
                    cluster_body
)

modelling_body <- fluidRow(
  column(width = 12,
         h2("Spotify Prediction Model"),
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
  ),
  column(width = 12,
         h3("Result Confusion Matrix"),
  ),
  column(width = 6, class="text-center",
         h3("Prediction With Data Train"),
         plotOutput(outputId = "confusionMatrixNeuralNetworkTrain"),
         h4("Accuracy"),
         h4(textOutput(outputId = "accuracyNeuralNetworkTrain"))
  ),
  column(width = 6, class="text-center",
         h3("Prediction Data Test"),
         plotOutput(outputId = "confusionMatrixNeuralNetworkTest"),
         h4("Accuracy"),
         h4(textOutput(outputId = "accuracyNeuralNetworkTest"))
  ),
)

modelling <- tabPanel("Modelling", icon = icon("robot"),
                      modelling_body
                      )

recommendation_body <- fluidRow(
  box(width = 12,
      h2("Mood Prediction and Playlist Recommendation"),
      ),
  column(width = 3,
      textInput("twitInput", h3("Your Twit")
      ),
      uiOutput(outputId = "selectGenre"),
      actionButton("submitTwitAction", "Recommend My Playlist"),
      # uiOutput(outputId = "submitTwitAction"),
      # textOutput("test"),
      ),
  column(width = 5,
         h2(textOutput("sentimentTwit")),
         h3(textOutput("selectedGenre")),
         withSpinner(tableOutput(outputId = "recommendationPlaylistTable"))
         ),
  column(width = 4,
         h3(textOutput("radarTitlePrediction")),
         br(),
         plotlyOutput(outputId = "radarRecommendPlaylist")
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
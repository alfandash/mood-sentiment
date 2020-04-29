twitter_eda <- fluidRow(
  box(width = 6, 
      plotlyOutput(outputId = "plotSummaryLabel")
  ),
  box(width = 6,
      plotOutput(outputId = "twitWordCloud")
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
                             h3("Cluster 1: Love Mood")
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
                             h3("Cluster 2: Energetic Mood")
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
                             h3("Cluster 3: Swing Mood")
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
                             h3("Cluster 4: Chill and Sad Mood")
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
                             h3("Cluster 3: Happy & Joy Mood")
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

about_body <- fluidRow(class="text-justify",
                      box(width = 12,
                           h2("Background"),
                           ),
                      box(width = 6, style = "font-size: 150%;",
                          htmlOutput("aboutText"),
                            ),
                      box(width = 6,
                          plotlyOutput(outputId = "stressLevel")
                          ),
                      box(width = 12,
                          h2("Flow")
                          ),
                      box(width = 12,
                          imageOutput(outputId = "aboutImageFlow")
                      )
)

about <- tabPanel("About",icon = icon("info"),
                  about_body
)

home <- tabPanel("Home",icon = icon("home"),
                 box(width = 12, align = "center",
                     imageOutput(outputId = "homeImageLanding")
                 )
                 
)

navbar <- navbarPage("Human Trafficking", theme = shinytheme("flatly"),
                     cluster,
                     eda,
                     about, 
                     home,
                     tags$head(tags$style(HTML('.box{
                                               -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;
                                               border-top: none;
                                               }'))),
                     header = tagList(
                       useShinydashboard()
                     )
)

shinyUI(navbar)
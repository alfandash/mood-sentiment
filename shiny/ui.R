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
      plotlyOutput(outputId = "topArtistAlbumPopularity"))
)


eda_body <- fluidRow(
  tabBox(width = 12,
    tabPanel("Twitter", twitter_eda),
    tabPanel("Spotify", spotify_eda)
  )
)

eda <- tabPanel("Data Exploration",icon = icon("compass"),
                  eda_body
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


home <- tabPanel("Home",icon = icon("home"),
                 box(width = 12, align = "center",
                   imageOutput(outputId = "homeImageLanding")
                 )
                 
)

about <- tabPanel("About",icon = icon("info"),
                  about_body
)

navbar <- navbarPage("Human Trafficking", theme = shinytheme("flatly"),
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
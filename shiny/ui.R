twitter_eda <- fluidRow(
  box(width = 12,
      box(width = 6, 
          plotlyOutput(outputId = "plotSummaryLabel")
          ),
      box(width = 6,
          plotOutput(outputId = "twitWordCloud")
          )
      ),
  box(width = 12,
      box(title = "Twitter Mood", width = 4, solidHeader = TRUE, status = "primary",
          checkboxGroupInput(
            inputId = "checkMood",
            label = "Select",
            choices = list("Anger" = "anger",
                           "Anger" = "anger",
                           "Anger" = "anger",
                           "Anger" = "anger")
          )
      ),
      box(title = "", width = 8, solidHeader = TRUE,
      )
  ),
)


eda_body <- fluidRow(
  tabBox(width = 12,
    tabPanel("Twitter", twitter_eda),
    tabPanel("Spotify")
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
                     home
)


shinyUI(navbar)
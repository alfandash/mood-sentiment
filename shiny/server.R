shinyServer(function(input, output) {
    output$stressLevel <- renderPlotly({
        city <- c("Istanbul","Mumbai","Tokyo","Manila","Taipei","Buenos Aires","Soul","Jakarta")
        level <- c(8,5,5,5,4,4,3,3)
        
        stress_level <- data.frame(city,level)
        
        data <- stress_level
        plot <- data %>%
            ggplot(aes(reorder(city, level), level)) +
            geom_col(aes(fill = level, text = glue("City: {city}
                                         Stress Level: {level}"))) +
            scale_fill_gradient(low= "lightblue",
                                high= "red") +
            coord_flip() +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                y = "Stress Level",
                x = "",
                title = "Highest City Stress Level"
            )
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    output$homeImageLanding <- renderImage({
        test
        return(list(
            src = "images/home-1.png",
            contentType = "image/png",
            height = "850"
        ))
    }, deleteFile = FALSE)
    
    output$aboutImageFlow <- renderImage({
        test
        return(list(
            src = "images/flow-1.png",
            contentType = "image/png",
            height = "80%"
        ))
    }, deleteFile = FALSE)
    
    output$aboutText <- renderText({
        paste0("<p>Stress is a feeling of emotional or physical tension.It can come from any event or thought that makes you feel frustrated, angry, or nervous.
        Stress is your body’s reaction to a challenge or demand.</p>
        
        <p>In short bursts, stress can be positive, such as when it helps you avoid danger or meet a deadline. 
        But when stress lasts for a long time, it may harm your health. </p>
        
        <p>From this paper source Stress in some level have crucial association with mood levels, especially stress with high level really contribute to mood swing. 
        This paper focusly in adult age, it mean can happen in productively working ages. </p>
        
        <p>Most of working people life in big city, one of them is Jakarta.
        Based in this source link Jakarta is 6th position for highest stress city in the world </p>")
    })
    
    # Eda twitter Section
    
    output$plotSummaryLabel <- renderPlotly({
        twit_label_df %>%
            ggplot(aes(x = reorder(label, freq), y = freq, fill = freq)) +
            geom_bar(stat = "identity", color = "black") +
            theme_minimal() 
    })
    
    output$twitWordCloud <- renderPlot({
        wordcloud_sample <- twit_df %>%
            select("text") %>% 
            sample_n(200)
        
        pal2 <- brewer.pal(8,"Dark2")

        wordcloud(wordcloud_sample$text, scale=c(8,.2),min.freq=3,
                  max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
        
    })
    
    output$plotFreqWordByMood <- renderPlotly({
        
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        char <- unlist(twit$text_clean)
        char <- data.frame(char)
        
        char <- char %>% 
            group_by(char) %>% 
            summarise(freq = n()) %>% 
            arrange(desc(freq))
        
        head(char, 25) %>% 
            ggplot(aes(x = reorder(char, freq), y = freq, fill = freq)) +
            geom_col() +
            scale_fill_gradient(low = "lightblue",
                                high = "navy") +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                y = "",
                x = "",
                title = paste0("Most Common Character Twit with Label ",input$selectMood)
            )
    })
    
    output$totalCharByMood <- renderValueBox({
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        data <- length(unlist(twit$text_clean))

        valueBox(
            data, "Total All Word", icon = icon("flag"),
            color = "blue"
        )
    })
    
    output$maxWordByMood <- renderValueBox({
        data <- max(lengths(gregexpr("\\W+", twit_df$text)) + 1)
        
        valueBox(
            data, "Max Word Twit", icon = icon("flag"),
            color = "green"
        )
    })
    
    output$uniqueWordByMood <- renderValueBox({
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        data <- length(unique(unlist(twit$text_clean)))
        
        valueBox(
            data, "Unique Word", icon = icon("flag"),
            color = "red"
        )
    })
    
    
    # Eda Spotify Section
    
    output$totalGenre <- renderValueBox({
        data <- length(unique(all_track_feature_df$genre))
        
        valueBox(
            data, "Total Genre", icon = icon("flag"),
            color = "green"
        )
    })
    
    output$totalTracks <- renderValueBox({
        data <- length(unique(all_track_feature_df$id))
        
        valueBox(
            data, "Total Tracks", icon = icon("flag"),
            color = "blue"
        )
    })
    
    output$totalArtists <- renderValueBox({
        data <- length(unique(all_track_feature_df$artist.name))
        
        valueBox(
            data, "Total Artists", icon = icon("flag"),
            color = "yellow"
        )
    })
    
    output$topAveragePopularity <- renderPlotly({
        data <- all_track_feature_df %>% 
            group_by(genre) %>% 
            summarise(popularity = mean(popularity)) %>% 
            arrange(desc(popularity)) %>% 
            head(20)
        
        plot <- data %>%
            ggplot(aes(reorder(genre, popularity), popularity)) +
            geom_col(aes(fill = popularity, text = glue("Genre: {genre}
                                         Average: {popularity}"))) +
            scale_fill_gradient(low= "#C0FFB3",
                                high= "#2C7873") +
            coord_flip() +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                y = "Average popularity",
                x = "",
                title = "Top 20 Highest Popular Genre"
            )
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$topArtistAlbumPopularity <- renderPlotly({
        data <- all_track_feature_df %>% 
            group_by(artist.name,album.name) %>% 
            summarise(popularity = mean(popularity)) %>% 
            arrange(desc(popularity)) %>% 
            head(20)
        
        plot <- data %>%
            ggplot(aes(reorder(album.name, popularity), popularity)) +
            geom_col(aes(fill = popularity, text = glue("Artists: {artist.name}
                                         Average: {popularity}"))) +
            scale_fill_gradient(low= "#C0FFB3",
                                high= "#2C7873") +
            coord_flip() +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                y = "Average popularity",
                x = "",
                title = "Top 20 Highest Popular Album"
            )
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$distributionPopularityByGenre <- renderPlot({
        data <- all_track_feature_df %>%
            filter(genre == input$selectGenre) 
        
        data %>% 
            ggplot(aes(x = popularity, fill = genre)) +
            geom_density(alpha=0.8) +
            geom_vline(aes(xintercept = mean(popularity), color = genre), linetype = "dashed") +
            scale_color_manual(values = c("#868686FF")) +
            scale_fill_manual(values = c("#ffba5a")) +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) 
    })
    
    output$radarAudioFeatureByGenre <- renderPlotly({
        data <- all_track_feature_df %>% 
            filter(genre == input$selectGenre) %>% 
            select(c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")) %>% 
            mutate(loudness = rescale(loudness),
                   tempo = rescale(tempo)) %>% 
            summarise_all(median)
        
        theta <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
        r <- as.numeric(data[1,])
        
        plot_ly(type = "scatterpolar", 
                r = r,
                theta = theta,
                fill = "toself",
                fillcolor = 'rgba(255,186,90,0.8)',
                line = list(color = "#629d66"),
                marker = list(size = 10, color = "#2c7873")
                ) %>% 
            layout(
                title = paste0("Audio Feature Characteristic"),
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                )
            )
    })
    
    # Song Cluster Section
    
    radarAudioFeatureCluster <- function(x) {
        data <- all_track_feature_df %>% 
            filter(cluster == x) %>% 
            select(c("valence", "energy", "danceability", "loudness", "acousticness", "instrumentalness", "speechiness")) %>% 
            mutate(loudness = rescale(loudness)) %>% 
            summarise_all(median)
        
        theta <- c("valence", "energy", "danceability", "loudness", "acousticness", "instrumentalness", "speechiness")
        r <- as.numeric(data[1,])
        
        plotly <- plot_ly(type = "scatterpolar", 
                r = r,
                theta = theta,
                fill = "toself") %>% 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                )
            )
        return(plotly)
    }
    
    plotAudioFeatureClusterGenre <- function(x) {
        data <- all_track_feature_df %>% 
            filter(cluster == x ) %>% 
            group_by(genre) %>% 
            summarise(freq = n()) %>% 
            arrange(desc(freq)) %>% 
            head(10)
        
        plot <- data %>%
            ggplot(aes(reorder(genre, freq), freq)) +
            geom_col(aes(fill = freq, text = glue("Genre: {genre}
                                         Frequency: {freq}"))) +
            scale_fill_gradient(low= "#C0FFB3",
                                high= "#2C7873") +
            coord_flip() +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                y = "",
                x = "",
                title = ""
            )
        
        return(ggplotly(plot, tooltip = "text"))
    }
    
    tableArtistTitleCluster <- function(x) {
        data <- all_track_feature_df %>% 
            filter(cluster == x) %>% 
            select(c("artist.name","name")) %>% 
            mutate("track_artist" = paste0(artist.name," - ", name)) %>% 
            select(track_artist) %>% 
            sample_n(10) %>% 
            rename(" " = track_artist)
        
        return(data)
    }
    
    output$radarAudioFeatureCluster1 <- renderPlotly({
        radarAudioFeatureCluster(1)
    })
    
    output$radarAudioFeatureCluster2 <- renderPlotly({
        radarAudioFeatureCluster(2)
    })
    
    output$radarAudioFeatureCluster3 <- renderPlotly({
        radarAudioFeatureCluster(3)
    })
    
    output$radarAudioFeatureCluster4 <- renderPlotly({
        radarAudioFeatureCluster(4)
    })
    
    output$radarAudioFeatureCluster5 <- renderPlotly({
        radarAudioFeatureCluster(5)
    })
    
    output$plotAudioFeaturClusterGenre1 <- renderPlotly({
        plotAudioFeatureClusterGenre(1)
    })
    
    output$plotAudioFeaturClusterGenre2 <- renderPlotly({
        plotAudioFeatureClusterGenre(2)
    })
    
    output$plotAudioFeaturClusterGenre3 <- renderPlotly({
        plotAudioFeatureClusterGenre(3)
    })
    
    output$plotAudioFeaturClusterGenre4 <- renderPlotly({
        plotAudioFeatureClusterGenre(4)
    })
    
    output$plotAudioFeaturClusterGenre5 <- renderPlotly({
        plotAudioFeatureClusterGenre(5)
    })
    
    output$tableArtistTitleGenre1 <- renderTable(
        tableArtistTitleCluster(1)
    )
    
    output$tableArtistTitleGenre2 <- renderTable(
        tableArtistTitleCluster(2)
    )
    
    output$tableArtistTitleGenre3 <- renderTable(
        tableArtistTitleCluster(3)
    )
    
    output$tableArtistTitleGenre4 <- renderTable(
        tableArtistTitleCluster(4)
    )
    
    output$tableArtistTitleGenre5 <- renderTable(
        tableArtistTitleCluster(5)
    )
})







shinyServer(function(input, output) {
    output$stressLevel <- renderPlotly({
        city <- c("Tokyo","Soul","Taipei","Manila","Istanbul","Jakarta","Buenos Aires","Mumbai")
        level <- c(9,7,7,5,5,4,3,2)
        
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
    
    # Modelling Section
    
    output$confusionMatrixKnn <- renderPlot ({
        result_knn %>% ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq), fontface = "bold", color = "white") +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                title = "KNN"
            )
        
    })
    
    output$accuracyKnn <- renderText({
        data <- round(
            accuracy_knn[1,2]*100
            ,2)
        paste0(data," %")
        
    })
    
    output$confusionMatrixNaiveBayes <- renderPlot ({
        result_naivebayes %>% ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq), fontface = "bold", color = "white") +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                title = "Naive Bayes"
            )
        
    })
    
    output$accuracyNaiveBayes <- renderText({
        data <- round(
            accuracy_naivebayes[1,2]*100
            ,2)
        paste0(data," %")
        
    })
    
    output$confusionMatrixRandomForest <- renderPlot ({
        result_random_forest %>% ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq), fontface = "bold", color = "white") +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) +
            labs(
                title = "Random Forest"
            )
        
    })
    
    output$accuracyRandomForest <- renderText({
        data <- round(
            accuracy_random_forest[1,2]*100
            ,2)
        paste0(data," %")
        
    })
    
    output$confusionMatrixNeuralNetworkTrain <- renderPlot ({
        matrix_lstm <- confusionMatrix(
            factor(data_train_pred_lstm, labels = c("anger", "fear", "sadness", "happy", "love")),
            factor(data_train_lstm$label_num, labels = c("anger", "fear", "sadness", "happy", "love"))
        )
        
        result_naivebayes <- as.data.frame(matrix_lstm$table)
        
        result_naivebayes %>% ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq), fontface = "bold", color = "white") +
            theme_minimal() +
            theme(
                legend.position = "none"
            )
        
    })
    
    output$accuracyNeuralNetworkTrain <- renderText({
        data <- round(
            accuracy_vec(
                truth = factor(data_train_lstm$label_num, labels = c("anger", "fear", "sadness", "happy", "love")),
                estimate = factor(data_train_pred_lstm, labels = c("anger", "fear", "sadness", "happy", "love"))
            ) * 100
            ,2)
        paste0(data," %")

    })
    
    output$confusionMatrixNeuralNetworkTest <- renderPlot ({
        matrix_lstm <- confusionMatrix(
            factor(data_test_pred_lstm, labels = c("anger", "fear", "sadness", "happy", "love")),
            factor(data_test_lstm$label_num, labels = c("anger", "fear", "sadness", "happy", "love"))
        )
        
        result_naivebayes <- as.data.frame(matrix_lstm$table)
        
        result_naivebayes %>% ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq), fontface = "bold", color = "white") +
            theme_minimal() +
            theme(
                legend.position = "none"
            )
        
    })
    
    output$accuracyNeuralNetworkTest <- renderText({
        data <- round(
            accuracy_vec(
                truth = factor(data_test_lstm$label_num, labels = c("anger", "fear", "sadness", "happy", "love")),
                estimate = factor(data_test_pred_lstm, labels = c("anger", "fear", "sadness", "happy", "love"))
            )* 100
            ,2)
        paste0(data," %")
        
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
    
    #Recommendation Section
    genreChoice <- reactive({
        genre_df$genre
    })
    
    validateTwit <- function() {
        shiny::validate(
            need(str_length(input$twitInput) >= 10, 'Type Something Dude! At least 10 Character la!'),
            need(str_length(input$twitInput) <= 220, 'Cant more than 220 Char')
        )
    }
    
    output$selectGenre <- renderUI({
        genre <- genreChoice()
        
        selectizeInput(
            inputId = "selectGenreTwit",
            label = "Select Genre You Like",
            choices = genre
        )
    })
    
    output$submitTwitAction <- renderUI({
        validateTwit()
        actionButton("submitTwitAction", "Recommend My Playlist")
    })
    
    output$test <- renderText({
        paste0(input$selectGenreTwit,input$twitInput,input$submitTwitAction)
    })
    
    # init variable here
    values <- reactiveValues(foo = NULL)
    values <- reactiveValues(mood_predict = NULL)
    values <- reactiveValues(label_predict = NULL)
    values <- reactiveValues(choosen_genre = NULL)
    
    output$sentimentTwit <- renderText({
        if(is.null(input$submitTwitAction) == TRUE)
            return()
        
        if(input$submitTwitAction == 0) 
            return()
        
        text = c(input$twitInput)
        label = c("")
        
        real_twit = as.data.frame(cbind(text, label))
        real_twit <- real_twit %>% mutate(
            text = as.character(text)
        )
        
        question <- rx() %>% 
            rx_find(value = "?") %>% 
            rx_one_or_more()
        
        exclamation <- rx() %>% 
            rx_find(value = "!") %>% 
            rx_one_or_more()
        
        punctuation <- rx_punctuation()
        
        number <- rx_digit()
        
        stop_words <- readLines("data/stopword_list.txt")
        
        real_twit <- real_twit %>% 
            mutate(text_clean = replace_html(text)) %>% 
            mutate(text_clean = replace_url(text_clean)) %>% 
            mutate(text_clean = replace_tag(text_clean, pattern = "@([A-Za-z0-9_]+)", replacement = "")) %>% 
            mutate(text_clean = replace_hash(text_clean, pattern = "#@([A-Za-z0-9_]+)", replacement = "")) %>% 
            mutate(text_clean = str_replace_all(text_clean, pattern = question, replacement = " tandatanya")) %>% 
            mutate(text_clean = str_replace_all(text_clean, pattern = exclamation, replacement = " tandaseru")) %>% 
            mutate(text_clean = str_replace_all(text_clean, pattern = punctuation, replacement = " ")) %>% 
            mutate(text_clean = str_remove_all(text_clean, pattern = number)) %>% 
            mutate(text_clean = gsub("USERNAME|URL", " ",text_clean)) %>% 
            mutate(text_clean = str_to_lower(text_clean)) %>% 
            mutate(text_clean = replace_word_elongation(text_clean)) %>% 
            mutate(text_clean = tokenize_words(text_clean, stopwords = stop_words)) %>% 
            mutate(text_clean = sapply(text_clean, toString),
                   text_clean = gsub(",", ' ', text_clean)) %>% 
            select(text_clean) %>% 
            na.omit()
        
        num_words <- 1024 
        maxlen <- 49
        
        tokenizer <- text_tokenizer(num_words = num_words,
                                    lower = TRUE) %>% 
            fit_text_tokenizer(real_twit$text_clean)
        
        data_real <- texts_to_sequences(tokenizer, real_twit$text_clean) %>% 
            pad_sequences(maxlen = maxlen)
        
        model <- load_model_hdf5("./model/model-18042020.h5")
        
        real_pred <- model %>% 
            predict_classes(data_real) %>% 
            as.vector()
        
        convert_label <- function(x) {
            if(x==0) {return("anger")}
            if(x==1) {return("fear")}
            if(x==2) {return("sadness")}
            if(x==3) {return("happy")}
            if(x==4) {return("love")}
        }
        
        emotion_label <- sapply(real_pred, convert_label)
        
        real_twit <- real_twit %>% mutate(emotion = emotion_label, label = real_pred)
        
        values$mood_predict <- real_twit[1,2]
        values$label_predict <- real_twit[1,3]
        
        paste0("Your Predict Mood is ",values$mood_predict)
    })
    
    output$selectedGenre <- renderText({
        paste0("Recomend playlist with genre ",input$selectGenreTwit)
    })
    
    output$recommendationPlaylistTable <- renderTable({
        print(paste0("playlist table ",values$mood_predict))
        if(is.null(input$submitTwitAction) == TRUE)
            return()
        
        if(input$submitTwitAction == 0) 
            return()
        
        if(is.null(values$mood_predict) == TRUE)
            return()
        
        
        id <- "d6e8fbb83d7a4948a13a3f3f1962ae29"
        secret <- "4bfd5b59bab3454f8e6510e2e9dc5d6a"
        
        base <- "https://api.spotify.com/v1/"
        
        # Get Token 
        token <- get_spotify_access_token(client_id = id,
                                          client_secret = secret)
        
        genre = input$selectGenre
        limit = "100"
        market = "ID"
        
        recommendationsTracksEndPoint <- paste0("recommendations?limit=",
                                                limit,"&market="
                                                ,market,"&seed_genres="
                                                ,genre)
        
        callTracksByGenre <- paste0(base,recommendationsTracksEndPoint)
        pages <- list()
        
        
        for (i in 0:10) {
            getTracksByGenre <- GET(callTracksByGenre,
                                    add_headers(
                                        Authorization = paste("Bearer ",token, sep = "")
                                    ))
            tracksText <- content(getTracksByGenre, "text")
            tracksJson <- fromJSON(tracksText, flatten = TRUE)
            pages[[i+1]] <- tracksJson$tracks
        }
        
        pages <- rbind_pages(pages)
        pages <- pages %>% select(1:29)
        pages <- pages %>%
            mutate(genre = genre)
        
        tracks_df <- pages
        
        artists <- tracks_df[,1][[1]][1,]
        
        for (row in 2:nrow(tracks_df)) {
            artists <- rbind(artists, tracks_df[,1][[row]][1,])
        }
        
        artists <- artists %>% 
            rename_all(funs(paste0("artist.",.)))
        
        
        album <- tracks_df[,16][[1]][1,]
        
        for (row in 2:nrow(tracks_df)) {
            album <- rbind(album, tracks_df[,16][[row]][1,])
        }
        
        album <- album %>% 
            rename_all(funs(paste0("album.",.)))
        
        
        album.img <- tracks_df[,19][[1]][1,]
        
        for (row in 2:nrow(tracks_df)) {
            img <- tracks_df[,19][[row]][1,]
            if(is.null(img$url)) {
                img = c(height = NA, url = NA, width = NA)
            }
            album.img <- rbind(album.img, img)
        }
        
        album.img <- album.img %>% 
            rename_all(funs(paste0("album.img.",.)))
        
        tracks_df <- tracks_df %>% 
            select(-c("artists","album.artists","album.images","album.href", "album.id", "album.name", "album.type", "album.uri", "album.external_urls.spotify"))
        
        tracks_df <- cbind(tracks_df, artists, album, album.img)
        
        id = tracks_df[,"id"]
        
        ids_df = as.data.frame(id)
        
        #code for crawling recomenddation
        rep <- round(nrow(ids_df) / 30)
        
        df_audio_feature <- data.frame()
        
        audioFeatureEndPoint <- paste0("audio-features/?ids=")
        
        for (row in 1:rep) {
            ids_list <- ids_df[,1]
            
            ids <- head(ids_list, 30)
            ids <- paste(ids, collapse =",")
            
            ids_df <- ids_df %>%
                slice(-(1:30))
            print(nrow(ids_df))
            
            callaudioFeature <- paste0(base,audioFeatureEndPoint,ids)
            getAudioFeatures <- GET(callaudioFeature,
                                    add_headers(
                                        Authorization = paste("Bearer ",token, sep = "")
                                    ))
            tracksText <- content(getAudioFeatures, "text")
            tracksJson <- fromJSON(tracksText, flatten = TRUE)
            df_audio_feature <- rbind(df_audio_feature, tracksJson$audio_features)
        }
        
        track_audio_feature_clean <- df_audio_feature %>% 
            select(-c("analysis_url", "key", "mode", "type", "uri", "analysis_url", "duration_ms", "time_signature"))
        
        all_track_feature_df <- merge(tracks_df,df_audio_feature, by = "id")
        all_track_clean_df <- all_track_feature_df %>% 
            select(c("artist.name","name","album.name","popularity", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "genre"))
        
        track_test <- all_track_clean_df %>% 
            select(4:14)
        
        model_random_forest <- readRDS("./model/random_forest.rds")
        
        cluster <- predict(model_random_forest, track_test)
        
        all_track_feature_df <- cbind(all_track_feature_df,as.data.frame(cluster))
        
        x = values$label_predict
        
        print(paste0("check",values$label_predict))
        print(x)
        
        if (x == 0) {
            recomend_track <- all_track_feature_df %>% 
                filter(cluster == 4 | cluster == 5)
        }
        
        if (x == 1) {
            recomend_track <- all_track_feature_df %>% 
                filter(cluster == 4 | cluster == 3)
        }
        
        if (x == 2) {
            recomend_track <- all_track_feature_df %>% 
                filter(cluster == 2 | cluster == 3)
        }
        
        if (x == 3) {
            recomend_track <- all_track_feature_df %>% 
                filter(cluster == 2 | cluster == 5)
        }
        
        if (x == 4) {
            recomend_track <- all_track_feature_df %>% 
                filter(cluster == 4 | cluster == 5)
        }
        
        recomend_track <- unique(recomend_track) %>% 
            sample_n(20) %>% 
            arrange(desc(popularity))
            
        
        data <- recomend_track %>%
            select(c("artist.name","name")) %>% 
            mutate("track_artist" = paste0(artist.name," - ", name)) %>% 
            select(track_artist) %>% 
            rename(" " = track_artist)
        
        data
    })
})







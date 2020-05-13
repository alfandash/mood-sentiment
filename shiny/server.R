shinyServer(function(input, output) {
    output$stressLevel <- renderPlotly({
        city <- c("Tokyo","Seoul","Taipei","Manila","Istanbul","Jakarta","Buenos Aires","Mumbai")
        level <- c(9,7,7,5,5,4,3,2)
        
        stress_level <- data.frame(city,level)
        
        data <- stress_level
        plot <- data %>%
            ggplot(aes(reorder(city, level), level)) +
            geom_col(width = 0.8, aes(fill = level, text = glue("City: {city}
                                         Stress Level: {level}"))) +
            scale_fill_gradient(low= "lightblue",
                                high= "red") +
            coord_flip() +
            theme_minimal() +
            theme(
                legend.position = "top",
                plot.subtitle = element_text()
            ) +
            labs(
                y = "Stress Level",
                x = "",
                title = "Highest City Stress Level"
            )
        
        plotly <- ggplotly(plot, tooltip = "text") %>% 
            layout(
            margin = list(t = 60),
            title = list(text = paste0("Highest City Stress Level",
                                       "<br>",
                                       "<sup>",
                                       "Source: data.tempo.co/read/594/jakarta-kota-dengan-tingkat-stress-ke-enam-dunia",
                                       "</sup>"))
            # annotations = list(text = 'Source: data.tempo.co/read/594/jakarta-kota-dengan-tingkat-stress-ke-enam-dunia',
            #                    font = list(size = 12),
            #                    showarrow = FALSE,
            #                    xref = 'paper', x = 1,
            #                    yref = 'paper', y = -0.)
            )
    })
    
    output$socialMediaLevel <- renderPlotly({
        apps <- c("Youtube", "Whatsapp", "Facebook", "Instagram", "Line", "Twitter", "FB Messenger", "BBM", "Linkedin", "Pinterest", "Skype", "WeChat", "Snapchat", "Path", "Tumblr", "Reddit")
        level <- c(88,83,81,80,59,52,47,38,33,29,28,28,26,25,20,16)
        type <- c("Social Network", "Messenger", "Social Network", "Social Network", "Messenger", "Social Network", "Messenger", "Messenger", "Social Network", "Social Network", "Messenger", "Messenger", "Social Network", "Social Network", "Social Network", "Social Network")
        
        social_media_platform <- data.frame(apps,level,type)
        
        social_media_platform %>% 
            mutate(type = as.factor(type))
        
        data <- social_media_platform
        plot <- head(data, 10) %>%
            ggplot(aes(reorder(apps, level), level)) +
            geom_col(width = 0.8,position = position_dodge(width=1), aes(fill = type, text = glue("Apps: {apps}
                                                   Percentage: {level} %
                                                   Type: {type}"))) +
            # scale_fill_gradient(low= "lightblue",
            #                     high= "red") +
            coord_flip() +
            theme_minimal() +
            theme(
                legend.position = "bottom",
                legend.title = element_blank()
            ) +
            labs(
                y = "Percentage Level (%)",
                x = "",
                title = "Most Active Social Media Platforms"
            )
        
        plotly <- ggplotly(plot, tooltip = "text") %>% 
            layout(
                margin = list(t = 60),
                title = list(text = paste0("Most Active Social Media Platforms",
                                           "<br>",
                                           "<sup>",
                                           "Source: Hootsuite We are Social Indonesian Digital Report 2019",
                                           "</sup>"))
                # annotations = list(text = 'Source: Hootsuite We are Social Indonesian Digital Report 2019',
                #                    font = list(size = 12),
                #                    showarrow = FALSE,
                #                    xref = 'paper', x = 1,
                #                    yref = 'paper', y = 0)
            )
    })
    
    output$homeImageLanding <- renderImage({
        test
        return(list(
            src = "images/home-1.png",
            contentType = "image/png",
            height = "130%"
        ))
    }, deleteFile = FALSE)
    
    output$aboutImageFlow <- renderImage({
        test
        return(list(
            src = "images/flow-1.png",
            contentType = "image/png",
            width = "100%"
        ))
    }, deleteFile = FALSE)
    
    output$aboutText <- renderText({
        # paste0("<p>Stress is a feeling of emotional or physical tension.It can come from any event or thought that makes you feel frustrated, angry, or nervous.
        # Stress is your body’s reaction to a challenge or demand.</p>
        # 
        # <p>In short bursts, stress can be positive, such as when it helps you avoid danger or meet a deadline. 
        # But when stress lasts for a long time, it may harm your health. </p>
        # 
        # <p>From this paper source Stress in some level have crucial association with mood levels, especially stress with high level really contribute to mood swing. 
        # This paper focusly in adult age, it mean can happen in productively working ages. </p>
        # 
        # <p>Most of working people life in big city, one of them is Jakarta.
        # Based in this source link Jakarta is 6th position for highest stress city in the world </p>")
        
        paste0("
               <p>Stress is a feeling of emotional or physical tension. It can come from any event or thought that makes you feel frustrated, angry, or nervous. 
               Stress is your body’s reaction to a challenge or demand. Most working people living in a big city, one of them in Jakarta. 
               Based in this reference Jakarta is the 6th position for the highest stress city in the world. </p>
               
               <p>From this paper <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5225218/'>(source)</a>, they take samples from adult age, it means can happen in productively working ages. 
               Stress at some level have a crucial association with mood levels, especially stress with high level contribute to a mood swing. </p>
               
               <p>Listening to music is one of the best methods to help people change their mood and it can help them release their stress. 
               Spotify right now one of popular music streaming in Indonesia, and based on reference twitter inside top 10 most active social media platform in Indonesia. </p>
               
               <p>The idea is we would try to predict mood from people tweet, and we try to recommend a list of music (playlist) 
               that suitable with their mood and hope it can get bring the better mood to them.</p>
               
               ")
    })
    
    # Eda twitter Section
    
    output$plotSummaryLabel <- renderPlotly({
        plot <- twit_label_df %>%
            ggplot(aes(x = reorder(label, freq), y = freq, fill = freq)) +
            geom_bar(stat = "identity", color = "black", aes(text=glue("Label = {label}
                                                                       Frequency = {freq}"))) +
            theme_minimal()  +
            theme(
                legend.position = "none"
            ) +
            labs(
                y = "Frequency",
                x = "",
                title = "Frequency Twitter Data Label"
            )
        
        ggplotly(plot, tooltip = "text")
    })
    
    output$twitWordCloud <- renderWordcloud2({
        # wordcloud_sample <- twit_df %>%
        #     select("text") %>% 
        #     sample_n(200)
        # 
        # pal2 <- brewer.pal(8,"Dark2")
        # 
        # wordcloud(wordcloud_sample$text, scale=c(8,.2),min.freq=3,
        #           max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
        
        char <- unlist(twit_tokenize$text_token)
        char <- data.frame(char)
        
        char <- char %>% 
            group_by(char) %>%
            summarise(freq = n()) %>%
            arrange(desc(freq)) %>% 
            head(500)
        
        wordcloud2a(char, color = "black", fontFamily = "arial")
        
    })
    
    output$plotFreqWordByMood <- renderPlotly({
        
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        char <- unlist(twit$text_token)
        char <- data.frame(char)
        
        char <- char %>% 
            group_by(char) %>% 
            summarise(freq = n()) %>% 
            arrange(desc(freq))
        
        plot <- head(char, 25) %>% 
            ggplot(aes(x = reorder(char, freq), y = freq, fill = freq)) +
            geom_col(aes(text=glue("Character = {char},
                                    Frequency = {freq}"))) +
            scale_fill_gradient(low = "lightblue",
                                high = "navy") +
            theme_minimal() +
            theme(
                legend.position = "none",
                axis.text.x = element_text(angle = 45)
            ) +
            labs(
                y = "Frequency",
                x = "",
                title = paste0("Most Common Character Twit with Label ",input$selectMood)
            )
        
        ggplotly(plot, tooltip = "text")
    })
    
    output$totalCharByMood <- renderValueBox({
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        data <- length(unlist(twit$text_token))

        valueBox(
            data, "Total All Word", icon = icon("flag"),
            color = "blue"
        )
    })
    
    output$maxWordByMood <- renderValueBox({
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        data <- max(lengths(gregexpr("\\W+", twit$text)) + 1)
        
        valueBox(
            data, "Max Word Tweet", icon = icon("flag"),
            color = "green"
        )
    })
    
    output$uniqueWordByMood <- renderValueBox({
        twit = twit_tokenize %>% filter(label == input$selectMood)
        
        data <- length(unique(unlist(twit$text_token)))
        
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
            summarise(popularity = round(mean(popularity), 2)) %>% 
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
    
    output$distributionPopularityByGenre <- renderPlotly({
        data <- all_track_feature_df %>%
            filter(genre == input$selectGenre) 
        
        plot <- data %>% 
            ggplot(aes(x = popularity, fill = genre)) +
            geom_density(alpha=0.8) +
            geom_vline(aes(xintercept = mean(popularity), color = genre), linetype = "dashed") +
            scale_color_manual(values = c("#868686FF")) +
            scale_fill_manual(values = c("#ffba5a")) +
            theme_minimal() +
            theme(
                legend.position = "none"
            ) + 
            labs(
                title = paste0("Population Popularity in Genre ", input$selectGenre)
            )
        
        plotly <- ggplotly(plot)
        
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
                marker = list(size = 10, color = "#2c7873"),
                # hovertemplate = paste("%{theta} : %{r}")
                hoverinfo = 'text',
                text = paste0(theta, " : ", round(r, 2))
                ) %>% 
            layout(
                title = paste0("Audio Feature Characteristic ", input$selectGenre),
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                ),
                margin = list(t = 50)
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
    
    output$confusionMatrixNaiveBayesTweet <- renderPlot ({
        
        matrix_naive_bayes <- readRDS("./model/confusion_naive_tweet.rds")
        
        result_naivebayes <- as.data.frame(matrix_naive_bayes$table)
        
        result_naivebayes %>% ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq), fontface = "bold", color = "white") +
            theme_minimal() +
            theme(
                legend.position = "none"
            )
        
    })
    
    output$accuracyNaiveBayesTweet <- renderText({
        data <- readRDS("./model/accuracy_vec_naive_tweet.rds")
        data <- round(data * 100, 2)
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
                fill = "toself",
                fillcolor = 'rgba(255,186,90,0.8)',
                line = list(color = "#629d66"),
                marker = list(size = 10, color = "#2c7873"),
                # hovertemplate = paste("%{theta} : %{r}")
                hoverinfo = 'text',
                text = paste0(theta, " : ", round(r, 2))
                ) %>% 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                ),
                margin = list(
                    r = 70,
                    l = 70
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
    
    output$submitTwitAction <- renderUI({
        validateTwit()
    })
    
    output$selectGenre <- renderUI({
        genre <- genreChoice()
        
        selectizeInput(
            inputId = "selectGenreTwit",
            label = "Select Genre You Like",
            choices = genre
        )
    })
    
    output$test <- renderText({
        paste0(input$selectGenreTwit,input$twitInput,input$submitTwitAction)
    })
    
    # init variable here
    values <- reactiveValues(foo = NULL)
    values <- reactiveValues(mood_predict = NULL)
    values <- reactiveValues(label_predict = NULL)
    values <- reactiveValues(choosen_genre = NULL)
    values <- reactiveValues(recommend_track_all = NULL)
    values <- reactiveValues(validationText = NULL)
    values <- reactiveValues(validationAccountText = NULL)
    values <- reactiveValues(validationBeforePrdictionText = NULL)
    
    observeEvent(input$tweetAccountInput, {
        whiteSpace <- str_count(input$tweetAccountInput, " ")
        
        if(str_count(input$tweetAccountInput) < 2 | whiteSpace >= 0) {
            values$validationAccountText <- "Minimum 2 Character without space"
            shinyjs::hide(id = "submitTwitAccountAction")
        }
        
        if(str_count(input$tweetAccountInput) >= 2 & whiteSpace == 0) {
            shinyjs::show(id = "submitTwitAccountAction")
            values$validationAccountText <- NULL
        }
        
    })
    
    output$twitValidation <- renderText({
        values$validationText
    })
    
    
    twitPrdictionFunction <- function(textInput, source) {
        print("TRIGGERR model twitter update")
        
        text = c(textInput, textInput)
        # text=c("test","test")
        label = c("", "")
        
        real_twit = as.data.frame(cbind(text, label))
        real_twit <- real_twit %>% mutate(
            text = as.character(text)
        )
        
        mention <- rx() %>% 
            rx_find(value = "@") %>% 
            rx_alnum() %>% 
            rx_one_or_more()
        
        hashtag <- rx() %>% 
            rx_find(value = "#") %>% 
            rx_alnum() %>% 
            rx_one_or_more()
        
        question <- rx() %>% 
            rx_find(value = "?") %>% 
            rx_one_or_more()
        
        exclamation <- rx() %>% 
            rx_find(value = "!") %>% 
            rx_one_or_more()
        
        punctuation <- rx_punctuation()
        
        number <- rx_digit()
        
        stemming <- function(x){
            paste(lapply(x,katadasar),collapse = " ")
        }
        
        stop_words <- readLines("data/stop_word.txt")
        spell_slang_lex <- read.csv("data/colloquial-indonesian-lexicon.csv")
        
        real_twit <- real_twit %>% 
            mutate(text_clean =  gsub("USERNAME|URL", " ",text)) %>% 
            mutate(text_clean = text_clean %>% 
                       replace_url() %>% 
                       replace_html() %>% 
                       replace_emoticon() %>% 
                       str_remove_all(pattern = mention) %>% 
                       str_remove_all(pattern = hashtag) %>% 
                       str_replace_all(pattern = question, replacement = " tandatanya") %>% 
                       str_replace_all(pattern = exclamation, replacement = " tandaseru") %>% 
                       str_replace_all(pattern = punctuation, replacement = " ") %>% 
                       str_remove_all(pattern = number) %>% 
                       str_to_lower() %>%
                       str_squish() %>% 
                       str_trim
            ) %>% 
            mutate(text_clean = replace_internet_slang(text_clean,
                                                       slang = paste0("\\b", spell_slang_lex$slang, "\\b"),
                                                       replacement = spell_slang_lex$formal,
                                                       ignore.case = TRUE)) 
        
        real_twit$text_clean <- lapply(tokenize_words(real_twit$text_clean), stemming)
        
        
        real_twit <- real_twit %>%
            mutate(text_clean = tokenize_words(text_clean, stopwords = stop_words)) %>%
            mutate(text_clean = sapply(text_clean, toString),
                   text_clean = gsub(",", ' ', text_clean)) %>% 
            select(text_clean)
        
        print("real twit")
        print(real_twit)
        
        # Neural Network section
        
        num_words <- 2048
        maxlen <- 50
        
        tokenizer <- text_tokenizer(num_words = num_words,
                                    lower = TRUE) %>% 
            fit_text_tokenizer(real_twit$text_clean)
        
        data_real <- texts_to_sequences(tokenizer, real_twit$text_clean) %>% 
            pad_sequences(maxlen = maxlen)
        
        
        model <- load_model_hdf5("./model/model-09052020_ver_1.h5")
        
        real_pred <- model %>% 
            predict_classes(data_real) %>% 
            as.vector()
        
        # Naive Bayes section
        
        model_bayes <- readRDS("./model/naive_bayes_tweet.rds")
        freq <- readRDS("./model/dict_freq.rds")
        
        bernoulli_conv <- function(x){
            x <- factor(
                ifelse(x > 0, 1, 0), levels = c(0,1), labels = c("Absent", "Present")
            )
            return(x)}
        
        tokenize_text <- function(text) {
            data <- VCorpus(VectorSource(text))
            
            # Document-Term Matrix and use only terms from data train
            data <- DocumentTermMatrix(data, 
                                       control = list(dictionary = freq))
            
            # Bernoulli Converter
            data <- apply(data, 2, bernoulli_conv)
            
            return(data)
        }
        
        pred_x <- tokenize_text(real_twit$text_clean)
        
        pred_result_naive <- predict(model_bayes, pred_x)
        
        convert_label <- function(x) {
            if(x==0) {return("anger")}
            if(x==1) {return("fear")}
            if(x==2) {return("sadness")}
            if(x==3) {return("happy")}
            if(x==4) {return("love")}
        }
        
        pred_result_naive_label <- sapply(pred_result_naive, convert_label)
        
        emotion_label <- sapply(real_pred, convert_label)
        
        print("emotion neural netowrk")
        print(emotion_label)
        
        print("emotion neural naive")
        print(pred_result_naive_label)
        
        real_twit <- real_twit %>% mutate(emotion = pred_result_naive_label, label = pred_result_naive)
        
        
        if(source == "account") {
            values$mood_predict_account <- real_twit[1,2]
            values$label_predict_account <- real_twit[1,3]
        }
        
        if(source == "text") {
            values$mood_predict <- real_twit[1,2]
            values$label_predict <- real_twit[1,3]
        }
       
    }
   
    
    twit_prediction_update <- eventReactive(input$submitTwitAction, {
        print("submit input")
        twitPrdictionFunction(input$twitInput, "text")
        
    })
    
    output$sentimentTwit <- renderText({
        twit_prediction_update()
      
        paste0("Your Predict Mood is ",values$mood_predict)
    })
    
    selectedgenre_reactive <- eventReactive(input$submitTwitAction, {
        if(is.null(values$validationBeforePrdictionText)) {
            paste0("Recomend playlist with genre ",input$selectGenreTwit)
        }
    })
    
    output$selectedGenre <- renderText({
        selectedgenre_reactive()
    })
    
    spotifyPrdictionFunction <- function(label_predict, selectGenre, source) {
        print("TRIGGERR model spotify")
        
        source("./script/spotify_key.R")
        
        base <- "https://api.spotify.com/v1/"
        
        # Get Token 
        token <- get_spotify_access_token(client_id = id,
                                          client_secret = secret)
        
        genre = selectGenre
        limit = "100"
        market = "ID"
        
        print("genre")
        print(genre)
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
            tracksText <- httr::content(getTracksByGenre, "text")
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
            tracksText <- httr::content(getAudioFeatures, "text")
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
        
        x = label_predict
        
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
        
        recomend_track <- unique(recomend_track)
        
        track_n = nrow(recomend_track)
        
        if(track_n > 50){
            track_n = 50
        }
        
        recomend_track <- recomend_track %>% 
            sample_n(track_n) %>% 
            arrange(desc(popularity))
        
        
        if(source == "account") {
            values$recommend_track_all_account <- recomend_track
        }
        
        if(source == "text") {
            values$recommend_track_all <- recomend_track
        }
        
        data <- recomend_track %>%
            # select(c("name","artist.name", "album.name", "popularity")) %>% 
            select(c("name","artist.name", "popularity")) %>% 
            rename("Song Name" = name,
                   "Artis" = artist.name,
                   # "Album" = album.name,
                   "Popularity" = popularity)
        
        data
    }
    
    spotify_prediction <- eventReactive(input$submitTwitAction, {
        spotifyPrdictionFunction(values$label_predict, input$selectGenreTwit, "text")
    })
    
   
    
    output$recommendationPlaylistTable <- renderDataTable({
        
        if(is.null(values$mood_predict) == TRUE)
            return()
        
        spotify_prediction()
    })
    
    radarTitlePrediction_reactive <- eventReactive(input$submitTwitAction, {
        "Playlist recomendation Characteristic"
    })
    
    output$radarTitlePrediction <- renderText({
        radarTitlePrediction_reactive()
    })
    
    playlist_table <- eventReactive(input$submitTwitAction, {
        data <- values$recommend_track_all
        
        data <- data %>% 
            select(c("valence", "energy", "danceability", "loudness", "acousticness", "instrumentalness", "speechiness")) %>% 
            mutate(loudness = rescale(loudness)) %>% 
            summarise_all(mean)
        
        theta <- c("valence", "energy", "danceability", "loudness", "acousticness", "instrumentalness", "speechiness")
        r <- as.numeric(data[1,])
        
        plotly <- plot_ly(type = "scatterpolar", 
                          r = r,
                          theta = theta,
                          fill = "toself",
                          fillcolor = 'rgba(255,186,90,0.8)',
                          line = list(color = "#629d66"),
                          marker = list(size = 10, color = "#2c7873"),
                          # hovertemplate = paste("%{theta} : %{r}")
                          hoverinfo = 'text',
                          text = paste0(theta, " : ", round(r, 2))
                          ) %>% 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                ),
                margin = list(
                    l = 70,
                    r = 70
                )
            )
        plotly
    })
    
    output$radarRecommendPlaylist <- renderPlotly({
        
        if(is.null(values$recommend_track_all) == TRUE)
            return()
        
        playlist_table()
        
    })
    
################################33
    
    values <- reactiveValues(mood_predict_account = NULL)
    values <- reactiveValues(label_predict_account = NULL)
    values <- reactiveValues(choosen_genre_account = NULL)
    values <- reactiveValues(recommend_track_all_account = NULL)
    
    spotify_prediction_account <- eventReactive(input$submitTwitAccountAction, {
        spotifyPrdictionFunction(values$label_predict_account, input$selectGenreTwitAccount, "account")
    })
    
    output$recommendationPlaylistTableAccount <- renderDataTable({
        
        if(is.null(values$mood_predict_account) == TRUE)
            return()
        
        spotify_prediction_account()
    })
    
    selectedgenre_reactive_account <- eventReactive(input$submitTwitAccountAction, {
        paste0("Recomend playlist with genre ",input$selectGenreTwitAccount)
    })
    
    output$selectedGenreAccount <- renderText({
        selectedgenre_reactive_account()
    })
    
    twit_prediction_by_account <- eventReactive(input$submitTwitAccountAction, {
        
        print("submit account twitter")
        
        source("./script/twitter_key.R")
        
        token <- create_token(
            consumer_key = consumer_key,
            consumer_secret = consumer_secret,
            access_token = access_token,
            access_secret = access_token_secret)
        
        users <- c(
            input$tweetAccountInput
        )
        
        ## get users data
        usr_df <- lookup_users(users)
        
        print(is.null(usr_df$text))
        
        ## view users data
        if(is.null(usr_df$text) == TRUE) {
            values$validationBeforePrdictionText <- paste0("Cant found Twitter Account: ",input$tweetAccountInput)
        } else {
            twitPrdictionFunction(usr_df$text, "account")
        }
        
    })
    
    output$sentimentTwitAccount <- renderText({
        twit_prediction_by_account()
        
        print(is.null(values$validationBeforePrdictionText))
        
        if(is.null(values$validationBeforePrdictionText)) {
            paste0("Your Predict Mood is ",values$mood_predict_account)
        } else {
            paste0(values$validationBeforePrdictionText)
        }
    })
    
    output$selectGenreAccount <- renderUI({
        genre <- genreChoice()
        
        selectizeInput(
            inputId = "selectGenreTwitAccount",
            label = "Select Genre You Like",
            choices = genre
        )
    })
    
    observeEvent(input$twitInput, {
        print(input$twitInput)
        
        word_count <- sapply(strsplit(input$twitInput, " "), length)
        
        
        if(str_count(input$twitInput) < 20 & word_count < 5) {
            values$validationText <- "Minimum 20 Character & 5 Word"
            shinyjs::hide(id = "submitTwitAction")
        }
        
        if(word_count > 5 & str_count(input$twitInput) > 20) {
            shinyjs::show(id = "submitTwitAction")
            values$validationText <- NULL
        }
        
    })
    
    output$accountValidation <- renderText({
        values$validationAccountText
    })
    
    radarTitlePredictionAccout_reactive <- eventReactive(input$submitTwitAccountAction, {
        "Playlist recomendation Characteristic"
    })
    
    output$radarTitlePredictionAccount <- renderText({
        radarTitlePredictionAccout_reactive()
    })
    
    playlist_table_account <- eventReactive(input$submitTwitAccountAction, {
        data <- values$recommend_track_all_account
        
        data <- data %>% 
            select(c("valence", "energy", "danceability", "loudness", "acousticness", "instrumentalness", "speechiness")) %>% 
            mutate(loudness = rescale(loudness)) %>% 
            summarise_all(mean)
        
        theta <- c("valence", "energy", "danceability", "loudness", "acousticness", "instrumentalness", "speechiness")
        r <- as.numeric(data[1,])
        
        plotly <- plot_ly(type = "scatterpolar", 
                          r = r,
                          theta = theta,
                          fill = "toself",
                          fillcolor = 'rgba(255,186,90,0.8)',
                          line = list(color = "#629d66"),
                          marker = list(size = 10, color = "#2c7873"),
                          # hovertemplate = paste("%{theta} : %{r}")
                          hoverinfo = 'text',
                          text = paste0(theta, " : ", round(r, 2))
        ) %>% 
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                ),
                margin = list(
                    l = 70,
                    r = 70
                )
            )
        plotly
    })
    
    output$radarRecommendPlaylistAccount <- renderPlotly({
        
        if(is.null(values$recommend_track_all_account) == TRUE)
            return()
        
        playlist_table_account()
        
    })
    
    output$previewTrack <- renderUI({
        test <- "https://p.scdn.co/mp3-preview/3eb16018c2a700240e9dfb8817b6f2d041f15eb1?cid=774b29d4f13844c495f206cafdad9c86"
        div(style = "padding: 0; position: fixed; bottom: 0;",
            HTML(paste0('<audio src="', test,'" type="audio/mp3" controls></audio>'))
        )
    })
})
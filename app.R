
library(shiny)
library(wordcloud2)
library(wordcloud)
library(tm)
library(colourpicker)
library(shinythemes)
library(tidytext)
library(ggplot2)
library(dplyr)
library(reshape2)
library(factoextra)
library(caret)
library(scales)
library(topicmodels)
library(fmsb)
library(circlize)

#runApp("app.R")

ui <- fluidPage(
  theme = shinytheme("darkly"),
  #shinythemes::themeSelector(),
  h1(HTML("<font color = pink><center>🌸Mini Text Analysis🌸</center></font>")),
  h4(HTML("<font color = beige><center>Alexandra Desaulniers</center></font>")),
  #h5(imageOutput("home_img")),
  h5(tags$img(src = "PolyLogo.png", height = "199px", width = "200px",
              style="display: block; margin-left: auto; margin-right: auto;", 
              inline = FALSE)),
  # Create a container for tab panels
  tabsetPanel(
    # Create a "Word cloud" tab
    tabPanel(
      title = "Hello Words🌸",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = HTML("<font color = beige>Word source</font>"),
            choices = c(
              "Use your own words" = "own",
              "Upload a File" = "file"
            )
          ),
          hr(),
          # Add the selector for the language of the text
          selectInput(
            inputId = "language",
            label = HTML("<font color = beige>Remove stopwords in</font>"),
            choices = c("English","Spanish"),
            multiple = FALSE,
            selected = "English"
          ),
          conditionalPanel(
            condition = "input.source == 'own'",
            textAreaInput("text", HTML("<font color = beige>Enter text</font>"), rows = 7)
          ),
          # Wrap the file input in a conditional panel
          conditionalPanel(
            # The condition should be that the user selects
            # "file" from the radio buttons
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          checkboxInput("remove_words", "Remove specific words?", FALSE),
          conditionalPanel(
            condition = "input.remove_words == 1",
            textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
            textAreaInput("words_to_remove2", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
            textAreaInput("words_to_remove3", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
            textAreaInput("words_to_remove4", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
            textAreaInput("words_to_remove5", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
            textAreaInput("words_to_remove6", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
            textAreaInput("words_to_remove7", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
            textAreaInput("words_to_remove8", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
            textAreaInput("words_to_remove9", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
            textAreaInput("words_to_remove10", "", rows = 1)
          ),
          hr(),
          numericInput("num", HTML("<font color = beige>Maximum number of words</font>"),
                       value = 100, min = 5
          ),
          hr(),
          colourInput("col", "Background color", value = "pink"),
          hr(),
          HTML('<p> Modified from: <a href="https://github.com/AntoineSoetewey/word-cloud/blob/master/app.R">code</a> <br> Reference: <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a></p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          div("1: Word cloud using frequency to decide size.",style = "color:beige"),
          br(),
          plotOutput("sentCloud", height = "900px"),
          br(),
          div("2: Word cloud using frequency to decide size separated by sentiment type.",style = "color:beige"),
          br(),
          plotOutput("sentiment"),
          br(),
          div("3: Bar graph depicting sentiment frequency in text.",style = "color:beige"),
          br(),
          plotOutput("sentList"),
          br(),
          div("4: Bar graphs comparing frequency of words per sentiment type.",style = "color:beige"),
          br(),
          plotOutput("sentChord"),
          br(),
          div("5: Chord graph visualizing frequency weighting on sentiment.",style = "color:beige"),
          br()
        )
      )
    ),
    tabPanel(
      title = "Instructions for Uploading🌸",
      sidebarLayout(
        sidebarPanel(
          h3(HTML("<font color = pink>How to create a CSV file easily:</font>")),
        ),
        mainPanel(
          h6(HTML("<font color = beige>Step 1: Copy the block of text you wish to analyze</font>"), tags$br(),
             HTML("<font color = beige>Step 2: Paste text into single cell in excel file</font>"), tags$br(),
             HTML("<font color = beige>Step 3: Name file anything you like and save as .csv</font>"), tags$br(),
             HTML("<font color = beige>Step 4: Click on 'Upload a File' option</font>"), tags$br(),
             HTML("<font color = beige>Step 5: Choose your newly created csv file</font>"), tags$br())
        )
      )
    )
  )
)



server <- function(input, output) {
  
  data_source <- reactive({
    if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "pink") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
  }
  
  create_sentiment <- function(data, background = "pink"){
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data))#, freq = as.numeric(data))
    }

    CleanWords <- data %>%
      inner_join(get_sentiments("nrc")) %>%
      group_by(sentiment, word) %>%
      count(mycount = n()) %>%
      distinct() %>%
      filter(sentiment %in%
               c("trust", "joy","surprise","anticipation", "fear", "anger", "disgust", "sadness"))
    return(CleanWords)
  }
  
  create_sentiment2 <- function(data, background = "pink"){
    
    # If text is provided, convert it to a dataframe of word frequencies
   # if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      #corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
   # }
    
    CleanWords2 <- data %>%
      #unnest_tokens(word, data, token = "words") %>%   
      filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) %>%
      inner_join(get_sentiments("nrc")) %>%
      group_by(sentiment, word) %>%
      #count(mycount = n()) %>%
      distinct() %>%
      filter(sentiment %in%
               c("trust", "joy","surprise","anticipation", "fear", "anger", "disgust", "sadness"))
    return(CleanWords2)
  }
  
  create_sentCloud <- function(data, background = "pink"){
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    return(data)
  }
  
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  output$sentiment <- renderPlot({
    dat <- create_sentiment(data_source(),
                     background = input$col)  
    dat %>%
      ggplot() + 
      geom_bar(aes(x = sentiment), color = 'purple', fill = 'pink') +
      coord_flip() + 
      labs(y = "Frequency",
           x = "Sentiment") +
      theme_minimal() + 
      theme_dark() + 
      ggtitle('Frequency of Sentiment')
    
  })
  
  
  output$sentCloud <- renderPlot({
    dat <- create_sentCloud(data_source(),
                            background = input$col)  
    dat %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word~sentiment, value.var ="n", fill=0) %>%
      comparison.cloud(colors = c("red","orange","grey","black","green","purple","deeppink","blue","lightblue","darkgreen"),
                       title.bg.colors = "beige", 
                       scale = c(6,.8),
                       max.words = 300, 
                       title.size = 1, 
                       random.order = FALSE)
    
  })
  
  
  output$sentChord <- renderPlot({
    dat <- create_sentiment2(data_source(),
                            background = input$col)  
    
    grid.col = c("anger" = "red", "anticipation" = "grey", "disgust" = "orange", "fear" = "black", "joy" = "green", "sadness" = "blue", "surprise" = "lightblue", "trust" = "darkgreen")
    
    Mood_mood <-  dat %>%
      filter(!sentiment %in% c("positive", "negative")) %>%
      count(sentiment, freq) %>%
      group_by(freq, sentiment) %>%
      summarise(sentiment_sum = sum(freq)) %>%
      ungroup()
    
    circos.clear()
    #Set the gap size
    circos.par(gap.after = c(rep(5, length(unique(Mood_mood[[1]])) - 1), 15,
                             rep(5, length(unique(Mood_mood[[2]])) - 1), 15))
    chordDiagram(Mood_mood, grid.col = grid.col, transparency = .2)
    title("Frequency Weight on Sentiment")
    
  })
  
  
  output$sentList <- renderPlot({
    dat <- create_sentiment2(data_source(),
                             background = input$col)  
    
    dat %>%
      inner_join(lexicon_nrc()) %>% 
      group_by(sentiment) %>% 
      #count(word, sort=T) %>% 
      top_n(5) %>% 
      ggplot(aes(reorder(word, freq), freq, fill=sentiment)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      facet_wrap(~sentiment, scales="free_y", ncol=4) +
      labs(y = "Contribution to sentiment", x = NULL) +
      coord_flip()
    
  })
  
#  output$home_img <- renderImage({
#    list(src = "www/PolyLogo.png",
#         width = "100%",
#         height = 330)
#    
#  }, deleteFile = F)
}

shinyApp(ui = ui, server = server)

#shinyAppDir(".")
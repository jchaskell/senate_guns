library(data.table)
library(tm)
library(SnowballC)
library(memoise)
library(shiny)

# Analyze filibuster data


# Import data ---------------------------------------------------------------------------
# path <- "/Users/karadowney/Documents/Data Viz/filibuster/"

fil <- read.delim(file = file.path(path, "filibuster_2016.tsv"), sep = "\t",
                  header = F, stringsAsFactors = F)
setDT(fil)

fil[, uniqueN(V1)]

# Fix Presiding officer
fil[grepl("PRESIDING OFFICER", V1), V1:="presiding officer"]

fil[, uniqueN(V1)]

#Fix names
library(stringr)
fil[, V1 := gsub("Mrs. |Mr. |Ms. ","",V1)]
fil[, V1 := gsub("\\.","",V1)]

# Senator corpuses
senators <- unique(fil$V1)
speech <- rep(" ", times = 43)

senators <- data.frame("senator" = senators, "speech" = speech)
setDT(senators)

for (i in 407) {
  senators[senator == fil[i,V1], speech := paste0(speech, fil$V2)]
}

senators[, senator := as.character(senator)][, speech := as.character(speech)]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  V2

# Save data
save(senators, fil, file = "clean_filibuster.RData")

# Playing with tm package, word frequencies ------------------------------------------------
# Overall
all_speeches <- paste(fil$V2, collapse=" ")
speech_src <- VectorSource(all_speeches)
corpus <- Corpus(speech_src)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

frequency <- colSums(dtm2)

frequency <- sort(frequency, decreasing = T)
frequency[5:15]

library(wordcloud)

words <- names(frequency)

wordcloud(words[5:105], frequency[5:105])

# Shiny app by Senator ------------------------------------------------------------------------
# Based on http://shiny.rstudio.com/gallery/word-cloud.html

getTermMatrix <- memoise(function(sen) {
  #Choose a senator's text
  text <- senators[senator == sen]
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), stopwords("english"), "say", "also", "even", "can", "cant", "said",
                      "will", "one", "get", "senator", "senators", "colleague", "colleagues"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


# Server
server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

ui <- 
  fluidPage(
    # Application title
    titlePanel("Word Cloud"),
    
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        selectInput("selection", "Choose a senator:",
                    choices = senators$senator),
        actionButton("update", "Change"),
        hr(),
        sliderInput("freq",
                    "Minimum Frequency:",
                    min = 1,  max = 50, value = 15),
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100)
      ),
      
      # Show Word Cloud
      mainPanel(
        plotOutput("plot")
      )
    )
  )

app <- shinyApp( ui = ui, server = server)

runApp(app)

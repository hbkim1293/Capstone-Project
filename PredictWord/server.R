library(shiny)
library(tidytext)
library(dplyr)
library(tidyr)
library(stringi)
library(data.table)

BigramP <- readRDS("AppData1.rds")
Bigram_raw <- readRDS("AppData2.rds")
TrigramP <- readRDS("AppData3.rds")
Trigram_raw <- readRDS("AppData4.rds")
FourgramP <- readRDS("AppData5.rds")
Fourgram_raw <- readRDS("AppData6.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    url1 <- a("Github page", href = "https://github.com/hbkim1293/Capstone-Project/blob/master/README.md")
    url2 <- a("Github download page", href = "https://github.com/hbkim1293/Capstone-Project/raw/master/BaseData.zip")
    output$tab1 <- renderUI({
        tagList("Here is documentation :", url1)
    })
    output$tab2 <- renderUI({
        tagList("Here is download link :", url2)
    })
    model <- function(sentence){
        df <- data_frame(text = sentence)
        df <- df %>% unnest_tokens(word, text) %>% anti_join(stop_words)
        char1 <- tail(df,3)$word[1]
        char2 <- tail(df,3)$word[2]
        char3 <- tail(df,3)$word[3]
        check1 <- FourgramP %>% filter(word1 == char1) %>%
            filter(word2 == char2) %>%
            filter(word3 == char3)
        check2 <- TrigramP %>% filter(word1 == char2) %>%
            filter(word2 == char3)
        check3 <- BigramP %>% filter(word1 == char3)
        if(nrow(check1) > 0){
            second <- check1[1:3,4] %>% rename(result = word4) %>% mutate(rank = 4:6)
        } else if(nrow(check2) > 0){
            second <- check2[1:3,3] %>% rename(result = word3) %>% mutate(rank = 4:6)
        } else if(nrow(check3) > 0){
            second <- check3[1:3,2] %>% rename(result = word2) %>% mutate(rank = 4:6)
        } else {
            second <- data_frame(result = "NA")
        }
        df <- data_frame(text = sentence) %>% unnest_tokens(word, text)
        char11 <- tail(df,3)$word[1]
        char12 <- tail(df,3)$word[2]
        char13 <- tail(df,3)$word[3]
        check11 <- Fourgram_raw %>% filter(word1 == char11) %>%
            filter(word2 == char12) %>%
            filter(word3 == char13)
        check12 <- Trigram_raw %>% filter(word1 == char12) %>%
            filter(word2 == char13)
        check13 <- Bigram_raw %>% filter(word1 == char13)
        if(nrow(check11) > 0){
            first <- check11[1:3,4] %>% rename(result = word4) %>% mutate(rank = 1:3)
        } else if(nrow(check12) > 0){
            first <- check12[1:3,3] %>% rename(result = word3) %>% mutate(rank = 1:3)
        } else if(nrow(check13) > 0){
            first <- check13[1:3,2] %>% rename(result = word2) %>% mutate(rank = 1:3)
        } else {
            first <- data_frame(result = "NA")
        }
        Result <- bind_rows(first, second)
        print(Result)
    }
    
    wordPred <- reactive({
        model(input$text)
    })
    output$Prediction <- renderDataTable({
        wordPred()
    })
    
})

library(shiny)
library(stringr)
library(wordcloud)

###
## ShinyServer() processes the inputs obtained from the user interface and 
##coordinates the operations in prediction for the result.  
###

source("predict.R")


shinyServer(function(input, output, session) {
        
        observe({
                VALUE <- 'Hello'
                if(input$go>0) {
                        isolate({
                                VALUE <- addResult()
                        })
                }
                updateTextInput(session, inputId = "textbox", value = VALUE)
        })
        
        output$inputValue <- renderText(gsub(profanity, "LLAMA", input$textbox))        
        
        output$plot <- renderPlot({
                if(require(RColorBrewer)){
                        palNames <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2",  "Set3")
                        palVals <- c(8,8,12,9,8,9,8,12)
                        palDF <- data.frame(name=palNames, val=palVals)
                        num <- sample(1:8,1,replace=T)
                        pal <- brewer.pal(palDF$val[num],as.character(palDF$name[num]))
                        
                        wordcloud(words=as.character(predictions()$word), 
                                  freq=predictions()$freq, 
                                  scale=c(3.5,.5), 
                                  min.freq=1,
                                  max.words=Inf,
                                  random.order=TRUE, 
                                  random.color=TRUE, 
                                  rot.per=0, 
                                  fixed.asp=TRUE,
                                  pal)
                }
        })
        
       
        output$columns = renderUI({
                selectInput("select", label = NULL, 
                            choices = giveChoices(), width="100%")
        })
        
        addResult <- function(){
                addedResult <- c()
                if(length(input$select)==0)
                        addedResult <- NULL
                else{
                        if(input$select == "Start over")
                                addedResult <- ""       
                        else{
                                suggestion <- strsplit(input$select, ": ")[[1]][2]
                                addedResult <- paste(input$textbox, suggestion)  
                        }
                        
                }
                addedResult
        }
        
        giveChoices <- function(){
                choices <- c()
                for(i in 1:nrow(predictions())){
                        content <- paste("Add the suggestion:", as.character(predictions()$word)[i])
                        choices <- c(choices, content)
                }
                choices<- c(choices, "Start over")
                choices
        }
        
        predictions <- reactive({predict(input$textbox)})
})
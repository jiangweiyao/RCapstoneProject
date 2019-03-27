#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    bigram <- readRDS("bigram_reduced.rds")
    trigram <- readRDS("trigram_reduced.rds")
    tetragram <- readRDS("tetragram_reduced.rds")
    
    parsetext <- reactive({
        parset <- tolower(input$input_text)
        parset <- strsplit(parset, split = " ")[[1]]
        t_length = length(parset)
        if(t_length > 2){
            output_word <- tetragram[word123 == paste(parset[t_length-2], parset[t_length-1],parset[t_length]) ,2]
            if(dim(output_word)[1] == 0){
                output_word <- trigram[word12 == paste(parset[t_length-1],parset[t_length]) ,2]
                if(dim(output_word)[1] == 0){
                    output_word <- bigram[word1 == parset[t_length],2]
                    if(dim(output_word)[1] == 0){
                        return("Error: Word Match Not Found")
                    } else{
                        return(output_word)
                    }
                } else{
                    return(output_word)
                }
            }
            else{
                return(output_word)
            } 
        } 
        else if(t_length > 1){
            output_word <- trigram[word12 == paste(parset[t_length-1],parset[t_length]) ,2]
            if(dim(output_word)[1] == 0){
                output_word <- bigram[word1 == parset[t_length],2]
                if(dim(output_word)[1] == 0){
                    return("Error: Word Match Not Found")
                } else{
                    return(output_word)
                }
            } else{
                return(output_word)
            }
        } 
        else if(t_length == 1){
            output_word <- bigram[word1 == parset[1],2]
            if(dim(output_word)[1] == 0){
                return("Error: Word Match Not Found")
            } else{
                return(output_word)
            }
        }
        #else if(t_length > 1){
        #    return(paste(parset[t_length - 1], parset[t_length]))
        #}
        
    })
    
    output$parsedtext <- renderText({ 
        paste(parsetext())})
    

})

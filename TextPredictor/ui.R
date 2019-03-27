#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Predictor"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
           h4("This app predicts the next word in the phrase you enter. Enter the your phrase in the text input box, and the next word will be predicted using the Stupid Back Off model from tetragrams, trigrams, and bigrams built from twitter, news, and blog data sets.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textInput("input_text", "Input", value = "", width = NULL, placeholder = NULL),
            submitButton(text = "Submit", icon = NULL, width = NULL),
            h3("The Next Predicted Word is", align = "left"),
            h2(strong(textOutput("parsedtext")), align = "left", size = 5)
        )
    )
))

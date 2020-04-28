#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    fluidRow(
        column(2,
               fileInput("file1", "Choose .JPG File",
                         multiple = FALSE,
                         accept = c('.jpg', '.jpeg')),
        ),
        column(8, leafletOutput("leafmap"))),
    fluidRow(
        column(3, textInput("text", label = "Text input", value = "Enter text...")),
        column(8, textInput("text", label = "Text input", value = "Enter text..."))
        )
    )
)
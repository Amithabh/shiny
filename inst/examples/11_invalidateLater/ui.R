library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("invalidateLater Example"),
  
  # of observations to view
  sidebarPanel(
    helpText("Returns results of rnorm(1) every 750ms")
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    verbatimTextOutput("rand")
  )
))

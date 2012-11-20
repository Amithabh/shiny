shinyUI(pageWithSidebar(
  headerPanel('Download Example'),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars"))
  ),
  mainPanel(
    downloadButton('downloadData', 'Download'),
    tableOutput('table')
  )
))
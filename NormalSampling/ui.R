library(shiny)

# Define UI
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Sampling from a Normal distribution"),
  
  sidebarPanel(
    tags$head(tags$style(type="text/css",
                         "label.radio { display: inline-block; }",
                         ".radio input[type=\"radio\"] { float: none; }")),
    
    radioButtons("n", "Sample size",choices=list(1,5,10,20,50,100,1000,10000),selected="10"),
    checkboxInput("add","Add to sample?",FALSE),
    actionButton("sample","Sample"),
    actionButton("clear","Clear sample"),
    checkboxInput("population","Show population density",TRUE)
    ),
  
  mainPanel(
    plotOutput(outputId = "samplePlot"),
    h3("Population parameters"),
    tableOutput(outputId = "populationDescriptives"),
    h3("Sample values"),
    tableOutput(outputId = "sampleDescriptives")
  )
))

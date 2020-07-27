library(shiny)

# Define UI
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Statistical distributions and probabilities"),
  
  sidebarPanel(
    selectInput("distribution", "Distribution", list(Normal="norm",t="t",F="F","Chi-Square"="chisq"), selected = "norm",
                multiple = FALSE),
    conditionalPanel(
      condition = "input.distribution == 'norm'",
      numericInput("norm_mu",HTML("&mu; (mean)"),0),
      numericInput("norm_sigma",HTML("&sigma; (standard deviation)"),1,min=0),
      numericInput("norm_value","value",0),
      radioButtons("norm_type","Probability",list("below","above"),selected="below")
    ),
    conditionalPanel(
      condition = "input.distribution == 't'",
      numericInput("t_df",HTML("df"),10),
      numericInput("t_value","value",0),
      radioButtons("t_type","Probability",list("below","above"),selected="below")
    ),
    conditionalPanel(
      condition = "input.distribution == 'F'",
      numericInput("F_df1",HTML("df<sub>1</sub>"),1),
      numericInput("F_df2",HTML("df<sub>2</sub>"),10),
      numericInput("F_value","value",1),
      radioButtons("F_type","Probability",list("below","above"),selected="above")
    ),
    conditionalPanel(
      condition = "input.distribution == 'chisq'",
      numericInput("chisq_df","df",1),
      numericInput("chisq_value","value",1),
      radioButtons("chisq_type","Probability",list("below","above"),selected="above")
    )
  ),
  
  mainPanel(
    plotOutput(outputId = "distributionPlot"),
    htmlOutput(outputId = "probabilityValue")
  )
))

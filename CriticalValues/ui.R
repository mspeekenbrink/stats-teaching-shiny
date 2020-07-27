library(shiny)

# Define UI
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Statistical distributions and critical values"),
  
  sidebarPanel(
    selectInput("distribution", "Distribution", list(Normal="norm",t="t",F="F","Chi-Square"="chisq"), selected = "norm",
                multiple = FALSE),
    conditionalPanel(
      condition = "input.distribution == 'norm'",
      numericInput("norm_mu",HTML("&mu; (mean)"),0),
      numericInput("norm_sigma",HTML("&sigma; (standard deviation)"),1,min=0),
      radioButtons("norm_type","Critical value",list("two-sided","one-sided (above)", "one-sided (below)"),selected="two-sided")
    ),
    conditionalPanel(
      condition = "input.distribution == 't'",
      numericInput("t_df",HTML("df"),10),
      radioButtons("t_type","Critical value",list("two-sided","one-sided (above)", "one-sided (below)"),selected="two-sided")
    ),
    conditionalPanel(
      condition = "input.distribution == 'F'",
      numericInput("F_df1",HTML("df<sub>1</sub>"),1),
      numericInput("F_df2",HTML("df<sub>2</sub>"),10)
    ),
    conditionalPanel(
      condition = "input.distribution == 'chisq'",
      numericInput("chisq_df","df",1)#,
    ),
    numericInput("p",HTML("significance level (&alpha;)"),.05)
  ),
  mainPanel(
    plotOutput(outputId = "distributionPlot"),
    htmlOutput(outputId = "criticalValue")
  )
))

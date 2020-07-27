library(shiny)
# Define server logic

shinyServer(function(input, output) {
  values <- reactiveValues()
  values$x <- vector()
  varEnv <- new.env()
  varEnv$clearCount <- 0
  
  myData <- reactive({
    if(input$sample == 0) return()
    if(input$clear > varEnv$clearCount) {
      assign("clearCount",input$clear,envir=varEnv)
      values$x <- vector()
      return(values$x)
    } else {
      isolate({
        if(input$add) values$x <- c(values$x,rnorm(input$n,mean=100,sd=15)) else values$x <- rnorm(input$n,mean=100,sd=15)
      })
    }
    values$x
  })
  
  updateData <- reactive({
    if(input$sample > 0) {
      if(input$add) values$x <- c(values$x,rnorm(input$n,mean=100,sd=15)) else values$x <- rnorm(input$n,mean=100,sd=15)
    }
  })
  
  output$samplePlot <- renderPlot({
    if(length(myData()) > 0) {
      hist(myData(),xlim=c(100-4*15,100+4*15),main="Histogram of the sample",xlab="Y",ylab="",breaks=c(-Inf,seq(100-4*15 - .5,100+4*15 + .5,by=2),Inf),freq=FALSE,axes=FALSE)
      axis(1)
      if(input$population) curve(dnorm(x,mean=100,sd=15),from=100-4*15,to=100+4*15,add=TRUE)
    } else {
      plot(1,1,xlim=c(100-4*15,100+4*15),ylim=c(0,dnorm(100,100,15)),type="n",axes=FALSE,ylab="",xlab="Y",main="Histogram of the sample")
      if(input$population) curve(dnorm(x,mean=100,sd=15),from=100-4*15,to=100+4*15,add=TRUE)
      axis(1)
    }
  })
  output$populationDescriptives <- renderTable({
    data.frame("Mean" = 100, "Median" = 100, "Standard deviation" = 15)
  },include.rownames=FALSE)
  
  output$sampleDescriptives <- renderTable({
    data.frame("Mean" = ifelse(length(myData())>0,round(mean(myData()),3),NA),"Median" = ifelse(length(myData())>0,round(median(myData()),3),NA),"Standard deviation" = ifelse(length(myData())>0,sd(myData()),NA),"Size"=length(myData()))
  },include.rownames=FALSE)
})

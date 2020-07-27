library(shiny)
# Define server logic

areaCol <- rgb(0,0,1,.5)

shinyServer(function(input, output, session) {
  pars <- reactiveValues()
  pars$norm_mu <- 0
  pars$norm_sigma <- 1
  pars$norm_value <- 0
  pars$t_df <- 10
  pars$t_value <- 0
  pars$F_df1 <- 1
  pars$F_df2 <- 1
  pars$F_value <- 1
  pars$chisq_df <- 1
  pars$chisq_value <- 1
  
  # validate Normal parameters
  observe({
    if(is.na(input$norm_mu) || input$norm_mu %in% c(".","-")) return()
    if(!is.numeric(input$norm_mu)) {
      updateNumericInput(session,"norm_mu",value=pars$norm_mu)
      return()
    }
    pars$norm_mu <- input$norm_mu
  })
  observe({
    if(is.na(input$norm_sigma) || input$norm_sigma %in% c(".","-")) return()
    if(!is.numeric(input$norm_sigma) || input$norm_sigma <= 0) {
      updateNumericInput(session,"norm_sigma",value=pars$norm_sigma)
      return()
    }
    pars$norm_sigma <- input$norm_sigma
  })
  observe({
    if(is.na(input$norm_value) || input$norm_value %in% c(".","-")) return()
    if(!is.numeric(input$norm_value)) {
      updateNumericInput(session,"norm_value",value=pars$norm_value)
      return()
    }
    pars$norm_value <- input$norm_value
  })
  # validate t parameters
  observe({
    if(is.na(input$t_df) || input$t_df %in% c(".","-")) return()
    if(!is.numeric(input$t_df) || input$t_df <= 0) {
      updateNumericInput(session,"t_df",value=pars$t_df)
      return()
    }
    pars$t_df <- input$t_df
  })
  observe({
    if(is.na(input$t_value) || input$t_value %in% c(".","-")) return()
    if(!is.numeric(input$t_value)) {
      updateNumericInput(session,"t_value",value=pars$t_value)
      return()
    }
    pars$t_value <- input$t_value
  })
  # validate F parameters
  observe({
    if(is.na(input$F_df1) || input$F_df1 %in% c(".","-")) return()
    if(!is.numeric(input$F_df1) || input$F_df1 <= 0) {
      updateNumericInput(session,"F_df1",value=pars$F_df1)
      return()
    }
    pars$F_df1 <- input$F_df1
  })
  observe({
    if(is.na(input$F_df2) || input$F_df2 %in% c(".","-")) return()
    if(!is.numeric(input$F_df2) || input$F_df2 <= 0) {
      updateNumericInput(session,"F_df2",value=pars$F_df2)
      return()
    }
    pars$F_df2 <- input$F_df2
  })
  observe({
    if(is.na(input$F_value) || input$F_value %in% c(".","-")) return()
    if(!is.numeric(input$F_value) || input$F_value < 0) {
      updateNumericInput(session,"F_value",value=pars$F_value)
      return()
    }
    pars$F_value <- input$F_value
  })
  # validate chisq parameters
  observe({
    if(is.na(input$chisq_df) || input$chisq_df %in% c(".","-")) return()
    if(!is.numeric(input$chisq_df) || input$chisq_df <= 0) {
      updateNumericInput(session,"chisq_df",value=pars$chisq_df)
      return()
    }
    pars$chisq_df <- input$chisq_df
  })
  observe({
    if(is.na(input$chisq_value) || input$chisq_value %in% c(".","-")) return()
    if(!is.numeric(input$chisq_value) || input$chisq_value < 0) {
      updateNumericInput(session,"chisq_value",value=pars$chisq_value)
      return()
    }
    pars$chisq_value <- input$chisq_value
  })
  # plot the density and area
  output$distributionPlot <- renderPlot({
    par(mar=c(5, 4, 0.5, 0.5) + 0.1)
    switch(input$distribution,
      norm={
        mu <- pars$norm_mu
        sigma <- pars$norm_sigma
        min <- mu-4*sigma
        if(pars$norm_value < min) min <- pars$norm_value
        max <- mu+4*sigma
        if(pars$norm_value > max) max <- pars$norm_value
        value <- pars$norm_value
        curve(dnorm(x,mean=mu,sd=sigma),from=min,to=max,xlab="Y",ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        # polygon
        if(input$norm_type == "below") {
          x <- seq(min,value,length=1000)
          y <- dnorm(x,mean=mu,sd=sigma)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value),c(0,y,0),col=areaCol)
        }
        if(input$norm_type == "above") {
          x <- seq(value,max,length=1000)
          y <- dnorm(x,mean=mu,sd=sigma)
          y[y == Inf] <- 1e+200
          polygon(c(value,x,max),c(0,y,0),col=areaCol)
        }
      },
      t={
        df  <- pars$t_df
        min <- -4
        if(pars$t_value < min) min <- pars$t_value
        max <- 4
        if(pars$t_value > max) max <- pars$t_value
        value <- pars$t_value
        curve(dt(x,df=df),from=min,to=max,xlab="t",ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        # polygon
        if(input$t_type == "below") {
          x <- seq(min,value,length=1000)
          y <- dt(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value),c(0,y,0),col=areaCol)
        }
        if(input$t_type == "above") {
          x <- seq(value,max,length=1000)
          y <- dt(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(value,x,max),c(0,y,0),col=areaCol)
        }
      },
      F={
        df1 <- pars$F_df1
        df2 <- pars$F_df2
        min <- 0
        max <- qf(.99,df1=df1,df2=df2)
        if(pars$F_value > max) max <- pars$F_value
        value <- pars$F_value
        curve(df(x,df1=df1,df2=df2),from=min,to=max,xlab="F",ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        if(input$F_type == "below") {
          x <- seq(min,value,length=1000)
          y <- df(x,df1=df1,df2=df2)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value),c(0,y,0),col=areaCol)
        }
        if(input$F_type == "above") {
          x <- seq(value,max,length=1000)
          y <- df(x,df1=df1,df2=df2)
          y[y == Inf] <- 1e+200
          polygon(c(value,x,max),c(0,y,0),col=areaCol)
        }
      },
      chisq={
        df <- pars$chisq_df
        min <- 0
        max <- qchisq(.999,df=df)
        if(pars$chisq_value > max) max <- pars$chisq_value
        value <- pars$chisq_value
        curve(dchisq(x,df=df),from=min,to=max,xlab=expression(chi^2),ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        if(input$chisq_type == "below") {
          x <- seq(min,value,length=1000)
          y <- dchisq(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value),c(0,y,0),col=areaCol)
        }
        if(input$chisq_type == "above") {
          x <- seq(value,max,length=1000)
          y <- dchisq(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(value,x,max),c(0,y,0),col=areaCol)
        }
      }
    )
  })
  output$probabilityValue <- renderText(HTML("<h3>Probability</h3><p>P(",
    switch(input$distribution,
          norm = {
            if(input$norm_type == "below") {
              out <- paste("Y &le;",pars$norm_value,") = ",pnorm(pars$norm_value,mean=pars$norm_mu,sd=pars$norm_sigma))
            }
            if(input$norm_type == "above") {
              out <- paste("Y &ge;",pars$norm_value,") = ",pnorm(pars$norm_value,mean=pars$norm_mu,sd=pars$norm_sigma,lower.tail=FALSE))
            }
            out
          },
          t = {
            if(input$t_type == "below") {
              out <- paste("t &le;",pars$t_value,") = ",pt(pars$t_value,df=pars$t_df))
            }
            if(input$t_type == "above") {
              out <- paste("t &ge;",pars$t_value,") = ",pt(pars$t_value,df=pars$t_df,lower.tail=FALSE))
            }
            out
          },
          F = {
            if(input$F_type == "below") {
              out <- paste("F &le;",pars$F_value,") = ",pf(pars$F_value,df1=pars$F_df1,df2=pars$F_df2))
            }
            if(input$F_type == "above") {
              out <- paste("F &ge;",pars$F_value,") = ",pf(pars$F_value,df1=pars$F_df1,df2=pars$F_df2,lower.tail=FALSE))
            }
            out
          },
          chisq = {
            if(input$chisq_type == "below") {
              out <- paste("&chi;<sup>2</sup> &le;",pars$chisq_value,") = ",pchisq(pars$chisq_value,df=pars$chisq_df))
            }
            if(input$chisq_type == "above") {
              out <- paste("&chi;<sup>2</sup> &ge;",pars$chisq_value,") = ",pchisq(pars$chisq_value,df=pars$chisq_df,lower.tail=FALSE))
            }
            out
          }
    ),"</p>"))
})

library(shiny)
# Define server logic

areaCol <- rgb(0,0,1,.5)

shinyServer(function(input, output, session) {
  pars <- reactiveValues()
  pars$norm_mu <- 0
  pars$norm_sigma <- 1
  pars$t_df <- 1
  pars$F_df1 <- 1
  pars$F_df2 <- 1
  pars$chisq_df <- 1
  pars$p <- .05
  pars$criticalValue <- NA
  
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
  # validate t parameters
  observe({
    if(is.na(input$t_df) || input$t_df %in% c(".","-")) return()
    if(!is.numeric(input$t_df) || input$t_df <= 0) {
      updateNumericInput(session,"t_df",value=pars$t_df)
      return()
    }
    pars$t_df <- input$t_df
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
      if(is.na(input$p) || input$p %in% c(".","-")) return()
      if(!is.numeric(input$p) || input$p < 0 || input$p > 1) {
        updateNumericInput(session,"p",value=pars$p)
        return()
      }
      pars$p <- input$p
    })
  # plot the density and area
  output$distributionPlot <- renderPlot({
    par(mar=c(5, 4, 0.5, 0.5) + 0.1)
    switch(input$distribution,
      norm={
        mu <- pars$norm_mu
        sigma <- pars$norm_sigma
        if(input$norm_type == "two-sided") {
          pars$criticalValue <- value <- c(qnorm(pars$p/2,mean=pars$norm_mu,sd=pars$norm_sigma),qnorm(1-pars$p/2,mean=pars$norm_mu,sd=pars$norm_sigma))
        }
        if(input$norm_type == "one-sided (above)") {
          pars$criticalValue <- value <- qnorm(1-pars$p,mean=mu,sd=sigma)
        }
        if(input$norm_type == "one-sided (below)") {
          pars$criticalValue <- value <- qnorm(pars$p,mean=mu,sd=sigma)
        }
        min <- mu-4*sigma
        if(min(value) < min) min <- min(value)
        max <- mu+4*sigma
        if(max(value) > max) max <- max(value)
        curve(dnorm(x,mean=mu,sd=sigma),from=min,to=max,xlab="Y",ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        # polygon
        if(input$norm_type == "two-sided") {
          x <- seq(min,value[1],length=1000)
          y <- dnorm(x,mean=mu,sd=sigma)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value[1]),c(0,y,0),col=areaCol)
          x <- seq(value[2],max,length=1000)
          y <- dnorm(x,mean=mu,sd=sigma)
          y[y == Inf] <- 1e+200
          polygon(c(value[2],x,max),c(0,y,0),col=areaCol)
        }
        if(input$norm_type == "one-sided (below)") {
          x <- seq(min,value,length=1000)
          y <- dnorm(x,mean=mu,sd=sigma)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value),c(0,y,0),col=areaCol)
        }
        if(input$norm_type == "one-sided (above)") {
          x <- seq(value,max,length=1000)
          y <- dnorm(x,mean=mu,sd=sigma)
          y[y == Inf] <- 1e+200
          polygon(c(value,x,max),c(0,y,0),col=areaCol)
        }
      },
      t={
        df <- pars$t_df
        if(input$t_type == "two-sided") {
          pars$criticalValue <- value <- c(qt(pars$p/2,df=df),qt(1-pars$p/2,df=df))
        }
        if(input$t_type == "one-sided (above)") {
          pars$criticalValue <- value <- qt(1-pars$p,df=df)
        }
        if(input$t_type == "one-sided (below)") {
          pars$criticalValue <- value <- qt(pars$p,df=df)
        }
        min <- -4
        if(min(value) < min) min <- min(value)
        max <- 4
        if(max(value) > max) max <- max(value)
        curve(dt(x,df=df),from=min,to=max,xlab="t",ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        # polygon
        if(input$t_type == "two-sided") {
          x <- seq(min,value[1],length=1000)
          y <- dt(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value[1]),c(0,y,0),col=areaCol)
          x <- seq(value[2],max,length=1000)
          y <- dt(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(value[2],x,max),c(0,y,0),col=areaCol)
        }
        if(input$t_type == "one-sided (below)") {
          x <- seq(min,value,length=1000)
          y <- dt(x,df=df)
          y[y == Inf] <- 1e+200
          polygon(c(min,x,value),c(0,y,0),col=areaCol)
        }
        if(input$t_type == "one-sided (above)") {
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
        pars$criticalValue <- value <- qf(1-pars$p,df1=df1,df2=df2)
        if(value > max) max <- value
        
        curve(df(x,df1=df1,df2=df2),from=min,to=max,xlab="F",ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        x <- seq(value,max,length=1000)
        y <- df(x,df1=df1,df2=df2)
        y[y == Inf] <- 1e+200
        polygon(c(value,x,max),c(0,y,0),col=areaCol)
      },
      chisq={
        df <- pars$chisq_df
        min <- 0
        max <- qchisq(.999,df=df)
        pars$criticalValue <- value <- qchisq(1-pars$p,df=df)
        if(value > max) max <- value
        curve(dchisq(x,df=df),from=min,to=max,xlab=expression(chi^2),ylab="density",axes=FALSE)
        axis(1)
        axis(2)
        x <- seq(value,max,length=1000)
        y <- dchisq(x,df=df)
        y[y == Inf] <- 1e+200
        polygon(c(value,x,max),c(0,y,0),col=areaCol)
      }
    )
  })
  output$criticalValue <- renderText(HTML("<h3>Critical value(s)</h3><p>",paste(pars$criticalValue,collapse=" and "),"</p><p>P(",
    switch(input$distribution,
          norm = {
            if(input$norm_type == "two-sided") {
              out <- paste("Y &le;",round(pars$criticalValue[1],4)," or ","Y &ge;",round(pars$criticalValue[2],4),") = ",pars$p)
            }
            if(input$norm_type == "one-sided (below)") {
              out <- paste("Y &le;",round(pars$criticalValue,4),") = ",pars$p)
            }
            if(input$norm_type == "one-sided (above)") {
              out <- paste("Y &ge;",round(pars$criticalValue,4),") = ",pars$p)
            }
            out
          },
          t = {
            if(input$t_type == "two-sided") {
              out <- paste("t &le;",round(pars$criticalValue[1],4)," or ","t &ge;",round(pars$criticalValue[2],4),") = ",pars$p)
            }
            if(input$t_type == "one-sided (below)") {
              out <- paste("t &le;",round(pars$criticalValue,4),") = ",pars$p)
            }
            if(input$t_type == "one-sided (above)") {
              out <- paste("t &ge;",round(pars$criticalValue,4),") = ",pars$p)
            }
            out
          },
          F = {
            out <- paste("F &ge;",round(pars$criticalValue,4),") = ",pars$p)
            out
          },
          chisq = {
            out <- paste("&chi;<sup>2</sup> &ge;",round(pars$criticalValue,4),") = ",pars$p)
            out
          }
    ),"</p>"))
})

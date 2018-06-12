#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(forecast)
library(lmtest)
library(MASS)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Time Series ModelR"),
  
  # Data, Modeling and Forecasting Tabs
  tabsetPanel(
    tabPanel("DATA", style = "width: 100%;",
             fileInput("TSD", "Choose a .rds file"),
             
             mainPanel(
               tags$div(id = "mainPanel", style = "width: 100%; height: 100%;",
                        tags$div(id = "inOptions", style = "width: 100%;")
               ))),
    tabPanel("MODELING",
             mainPanel(actionButton("NewModel", "New Model")),
             style = "margin-top: 10px;"),
    tabPanel("FORECASTING")
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$TSD, {
    
    
    # Function for plotting Time Series and Corsseponding ACF/PACF 
    gg_TS <- function(d, yVar, toPlot = c("TS", "ACF", "PACF"), xLim = c(1, nrow(d))) { 
      
      if(toPlot == "TS") { # Time Series
        dSlice <- slice(d, input$TSBegin:input$TSEnd)
        
        ggplot(dSlice, aes_string(input$timeVar, yVar)) +
          geom_line() +
          geom_point()
        
      } else if(toPlot == "ACF") { # ACF
        tsObj <- ts(d[yVar])
        acfObj <- Acf(tsObj, lag.max = length(tsObj) - 1, plot = FALSE)
        acfD <- data.frame("acf" = acfObj$acf, 
                           "lag" = acfObj$lag)
        
        
        ggplot(acfD, aes(lag, acf)) +
          geom_bar(stat = "identity") +
          xlim(xLim[1] - 1, xLim[2] + 1) +
          ylim(-1.1, 1.1)
        
        
      } else if(toPlot == "PACF") { # PACF
        tsObj <- ts(d[yVar])
        pacfObj <- Pacf(tsObj, lag.max = length(tsObj) - 1, plot = FALSE)
        pacfD <- data.frame("pacf" = c(pacfObj$acf), 
                            "lag" = pacfObj$lag)
        
        
        ggplot(pacfD, aes(lag, pacf)) +
          geom_bar(stat = "identity") +
          xlim(xLim[1] - 1, xLim[2] + 1) +
          ylim(-1.1, 1.1)
      }
      
      
    }
    
    TSD <<- data.frame(readRDS(input$TSD$datapath)) # Get data
    TSD <<- data.frame("Obs" = seq(1:length(TSD[,1])), TSD)
    varChoices <<- colnames(TSD) # Get Columns/Variables
    
    
    # User Options for Data
    insertUI("#inOptions",
             where = "afterBegin",
             ui = # Options
               fluidRow(
                 column(3, selectInput("seriesVar", "Series Variable", # Selecting Y Var
                                       choices = c("", varChoices))),
                 column(3, selectInput("timeVar", "Time Variable", # Selecting Time Var
                                       choices = c("", varChoices))),
                 column(3, textInput("SLength", "Choose Season Length")), # Selecting Season Length
                 column(3, actionButton("subOptions", "Submit"))
               ))
    
    observeEvent(input$subOptions, { # Add Content for Viewing and Transforming Data
      # TSD <<- cbind(Obs = 1:289, TSD)
      insertUI("#mainPanel",
               where = "beforeEnd",
               ui = tags$div(id = "viewManip", style = "width: 100%; margin-top: 10%;",
                             tags$div(id="tableContain", style = "display: inline-block; width: 40%;
                                      height: 315px; padding-top: 5px;
                                      border: 2px solid black; 
                                      border-radius: 10px;",
                                      tags$div(id = "dTable", 
                                               style = "height: 98%; width: 98%; 
                                               margin: 0 auto;
                                               border-radius: 8px;
                                               overflow-y: scroll;",
                                               tableOutput("TSTable")
                                      )),
                             tags$div(id = "transOptions", 
                                      style = "display: inline-block; width: 40%; float: right;
                                      border: 2px solid black; border-radius: 10px;",
                                      fluidRow(style = "margin-left: 2%;",
                                               selectInput("trVar", "Variable to Transform",
                                                           choices = input$seriesVar),
                                               textInput("newVar", "Name New Variable")),
                                      fluidRow(style = "margin-left: 2%; margin-bottom: 2%;",
                                               actionButton("Diff", "First Difference")),
                                      fluidRow(style = "margin-left: 2%; margin-bottom: 2%;",
                                               actionButton("SDiff", "Seasonal Difference")),
                                      fluidRow(
                                        column(3, numericInput("lamBCT", "Lambda",
                                                               1, -100, 100, .00001),
                                               style = "margin-left: 2%; margin-bottom: 2%;"),
                                        column(3,actionButton("BoxCox", "Box-Cox"),
                                               style = "margin-top: 30px;")
                                      ),
                                      fluidRow(style = "margin-left: 2%; margin-bottom: 2%;",
                                               actionButton("logY", "Log Transformation"))
                             ))
    )
      
      
      insertUI("#mainPanel", # Adding Data Plots
               where = "beforeEnd",
               ui = tags$div(id = "plots", style = "border: 2px solid black; 
                             border-radius: 10px; margin-top: 10%; margin-bottom: 10%; 
                             width: 100%;; padding: 5px 5px 0 5px;",
                             selectInput("plotVar", "Variable to Plot", 
                                         choices = input$seriesVar),
                             fluidRow(
                               column(10, plotOutput("TSPlot")), 
                               column(2, numericInput("TSBegin", "Start Obs", 1, min = 1, 
                                                      max = nrow(TSD)),
                                      numericInput("TSEnd", "End Obs", nrow(TSD),
                                                   min = 1, max = nrow(TSD)))),
                             fluidRow(
                               column(10, plotOutput("ACFPlot")),
                               column(2, numericInput("SLagsACF", "Start Lag", 1, 1, 
                                                      nrow(TSD))),
                               column(2, numericInput("ELagsACF", "End Lag", nrow(TSD), 1, 
                                                      nrow(TSD)))
                             ),
                             fluidRow(
                               column(10, plotOutput("PACFPlot")),
                               column(2, numericInput("SLagsPACF", "Start Lag", 1, 1, 
                                                      nrow(TSD))),
                               column(2, numericInput("ELagsPACF", "End Lag", nrow(TSD), 1, 
                                                      nrow(TSD)))
                             ),
                             
                             fluidRow(
                               column(10, plotOutput("HistPlot")),
                               column(2, numericInput("nBins", "# of Bins", 15, 1, nrow(TSD)))
                             ),
                             fluidRow(
                               column(10, plotOutput("QQPlot"))
                             ),
                             fluidRow(
                               column(10, plotOutput("BCPlot")),
                               column(2, numericInput("SLambda", "Begin Lambda", -10, 
                                                      -100, 100),
                                      numericInput("ELambda", "End Lambda", 10, -100, 100, 
                                                   1),
                                      numericInput("byLambda", "By", .05, .00001, 5,
                                                   .1)
                               )
                             )
               ))
      
      
      
      output$TSTable <- renderTable({TSD}) # Data Table
      observeEvent(input$TSBegin, {
        observeEvent(input$TSEnd, {
          output$TSPlot <- renderPlot(gg_TS(TSD, yVar = input$plotVar, toPlot = "TS"))
        })
      })
      
      observeEvent(input$SLagsACF, { # Time Series Plot
        observeEvent(input$ELagsACF, {
          output$ACFPlot <- renderPlot(gg_TS(TSD, yVar = input$plotVar, toPlot = "ACF",
                                             xLim = c(input$SLagsACF, input$ELagsACF)))
        })
      })
      
      observeEvent(input$SLagsPACF, { # ACF Plot
        observeEvent(input$ELagsPACF, {
          output$PACFPlot <- renderPlot(gg_TS(TSD, yVar = input$plotVar, toPlot = "PACF",
                                              xLim = c(input$SLagsPACF, input$ELagsPACF)))
        })
      })
      
      # Histogram and QQ Plot
      output$HistPlot <- renderPlot({ggplot(TSD, aes_string(input$plotVar)) +
          geom_histogram(bins = input$nBins, fill = "black", color = "blue")})
      output$QQPlot <- renderPlot({ggplot(TSD, aes_string(sample = input$plotVar)) +
          stat_qq()})
      
      # First Difference
      observeEvent(input$Diff, {
        TSD[input$newVar] <<- c(NA, diff(unlist(TSD[input$trVar])))
        output$TSTable <- renderTable({TSD})
        updateSelectInput(session, "trVar", choices = colnames(TSD), 
                          selected = input$newVar)
        updateSelectInput(session, "plotVar", choices = colnames(TSD), 
                          selected = input$newVar)
      })
      
      # Seasonal Difference
      observeEvent(input$SDiff, {
        TSD[input$newVar] <<- c(rep(NA, as.numeric(input$SLength)), 
                                diff(unlist(TSD[input$trVar]), lag = as.numeric(input$SLength)))
        output$TSTable <- renderTable({TSD})
        updateSelectInput(session, "trVar", choices = colnames(TSD), 
                          selected = input$newVar)
        updateSelectInput(session, "plotVar", choices = colnames(TSD), 
                          selected = input$newVar)
      })
      
      # Box Cox Transformation
      
      observeEvent(input$lamBCT, {
        observeEvent(input$BoxCox, {
          TSD[input$newVar] <<- BoxCox(unlist(TSD[input$trVar]), input$lamBCT)
          output$TSTable <- renderTable({TSD})
          updateSelectInput(session, "trVar", choices = colnames(TSD), 
                            selected = input$newVar)
          updateSelectInput(session, "plotVar", choices = colnames(TSD), 
                            selected = input$newVar)
        })
      })
      
      # Log Transformation
      observeEvent(input$logY, {
        TSD[input$newVar] <<- log10(unlist(TSD[input$trVar]))
        output$TSTable <- renderTable({TSD})
        updateSelectInput(session, "trVar", choices = colnames(TSD), 
                          selected = input$newVar)
        updateSelectInput(session, "plotVar", choices = colnames(TSD), 
                          selected = input$newVar)
      })
      
      # Box-Cox Plot
      checkSign <- function(vec) {
        if(all(vec > 0)) {
          posvec <- vec
        }
        else {
          posvec <- vec + abs(min(vec)) + 1
        }
        return(posvec)
      }
      
      
      bcPlot <- function(sLam = -10, eLam = 10, byLam = .05) {
        bc <- boxcox(checkSign(unlist(TSD[input$plotVar])) ~ 1, lambda = 
                       seq(sLam, eLam, byLam))
        bc <- data.frame(bc)
        bc$CI95 <- ifelse(bc$y > max(bc$y) - 1/2 * qchisq(.95, 1), TRUE, FALSE)
        confVals <- data.frame("x" = bc$x[bc$y > max(bc$y) - 1/2 * qchisq(.95, 1)], 
                               "y" = bc$y[bc$y > max(bc$y) - 1/2 * qchisq(.95, 1)])
        
        ggplot(bc, aes(x = x, y = y)) +
          geom_smooth() +
          geom_smooth(data = confVals, aes(x = x, y= y)) +
          geom_ribbon(data = confVals, aes(ymin = min(bc$y), ymax = y)) +
          geom_segment(data = confVals[confVals$y == max(confVals$y),], 
                       aes(x = x, xend = x, y = min(bc$y), yend = y), color = "red") +
          geom_text(data = confVals[confVals$y == max(confVals$y),], 
                    aes(x = 0, y = min(bc$y), label = paste("lambda =  ", x)), color = "red")
      }
      
      observeEvent(input$SLambda, {
        observeEvent(input$ELambda, {
          observeEvent(input$byLambda, {
            
            output$BCPlot <- renderPlot(bcPlot(input$SLambda, input$ELambda, input$byLambda))
            
          })
        })
        
      })
      
      #### MODELING
      
      # Need to make newest model active, and other models inactive
      
      i <<- 0
      observeEvent(input$NewModel, {
        i <<- i + 1
        shinyjs::removeClass(selector = ".activeMod", class = ".activeMod")
        insertUI(selector = "#NewModel",
                 where  = "afterEnd",
                 ui = tags$div(class = "model activeMod", 
                               style = "border: 1px solid black; border-radius: 10px; 
                               margin-top: 20px;",
                               "CHOOSE SARIMA MODEL",
                               tags$div(class = "options activeMod",
                                        fluidRow(
                                          column(5,
                                                 selectInput(paste0("modVar", i), "Variable to Model",
                                                             choices = c("", colnames(TSD)))),
                                          column(3, numericInput(paste0("SObsMod", i), 
                                                                 "Start Obs", 1, 1, 
                                                                 nrow(TSD))),
                                          column(3, numericInput(paste0("EObsMod", i), 
                                                                 "End Obs", nrow(TSD), 1, 
                                                                 nrow(TSD)))
                                        ),
                                        fluidRow(
                                          column(2, textInput(paste0("p", i), "p =")),
                                          column(2, textInput(paste0("d", i), "d = ")),
                                          column(2, textInput(paste0("q", i), "q = ")),
                                          column(2, textInput(paste0("P", i), "P = ")),
                                          column(2, textInput(paste0("D", i), "D = ")),
                                          column(2, textInput(paste0("Q", i), "Q = "))
                                        ),
                                        actionButton(paste0("subArima", i), "Submit")),
                               tags$div(class = "output activeMod",
                                        verbatimTextOutput(paste0("coeft", i)),
                                        verbatimTextOutput(paste0("summ", i)),
                                        verbatimTextOutput(paste0("BPTitle", i)),
                                        verbatimTextOutput(paste0("BPtest", i)),
                                        plotOutput(paste0("resACF", i)),
                                        plotOutput(paste0("resPACF", i)),
                                        plotOutput(paste0("resHist", i)),
                                        plotOutput(paste0("resQQ", i))
                               )
                 )
        )
        
        observeEvent(input[[paste0("subArima", i)]], {
          TSDM <- slice(TSD, input[[paste0("SObsMod", i)]]:input[[paste0("EObsMod", i)]])
          print(input[[paste0("SObsMod", i)]])
          tsModObj <- ts(TSDM[input[[paste0("modVar", i)]]])
          nsOrders <- as.numeric(c(input[[paste0("p", i)]], input[[paste0("d", i)]], 
                                   input[[paste0("q", i)]]))
          seOrders <- as.numeric(c(input[[paste0("P", i)]], input[[paste0("D", i)]], 
                                   input[[paste0("Q", i)]]))
          arimaMod <- Arima(tsModObj, order = nsOrders, 
                            seasonal = list(order = seOrders, period = as.numeric(input$SLength)))
          
          output[[paste0("coeft", i)]] <- renderPrint({coeftest(arimaMod)})
          output[[paste0("summ", i)]] <- renderPrint({summary(arimaMod)})
          arimaRes <- residuals(arimaMod)
          arimaRes <- data.frame("Index" = 1:length(arimaRes), "Residuals" = arimaRes)
          
          BPlags <- c(1, 2, 6, 12, 24, 36, 48, 60)
          BPDF <- data.frame("Lag" = BPlags,
                             "X-Squared" = sapply(BPlags, function(x) {
                               Box.test(arimaRes$Residuals, lag = x)$statistic
                             }),
                             "P-value" = sapply(BPlags, function(x) {
                               Box.test(arimaRes$Residuals, lag = x)$p.value
                             })
          )
          
          insertUI(paste0("#BPtest", i),
                   "beforeBegin",
                   h6("Box Pierce Test")
          )
          output[[paste0("BPtest", i)]] <- renderPrint(BPDF)
          
          output[[paste0("resACF", i)]] <- renderPlot(gg_TS(arimaRes, 
                                                            "Residuals", toPlot = "ACF"))
          output[[paste0("resPACF", i)]] <- renderPlot(gg_TS(arimaRes, 
                                                             "Residuals", toPlot = "PACF"))
          output[[paste0("resHist", i)]] <- renderPlot({ggplot(arimaRes, aes(arimaRes$Residuals)) +
              geom_histogram(fill = "black", color = "blue")})
          output[[paste0("resQQ", i)]] <- renderPlot({ggplot(arimaRes, aes(sample = arimaRes$Residuals)) +
              stat_qq()})
        })
        
      })
      
      
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


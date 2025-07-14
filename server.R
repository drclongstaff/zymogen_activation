library(shiny)
library(readxl)
library(DT)
library(tibble)
library(purrr)
library(dplyr)

# This function is for loading user data
load_file <- function(NAME, PATH, SHEET) {
  ext <- tools::file_ext(NAME)
  switch(ext,
    xlsx = read_excel(PATH, SHEET, .name_repair = "universal"),
    csv = vroom::vroom(PATH, delim = ",", show_col_types = FALSE, .name_repair = "universal"),
    tsv = vroom::vroom(PATH, delim = "\t", .name_repair = "universal"),
    txt = vroom::vroom(PATH, show_col_types = FALSE, .name_repair = "universal"),
    validate("Invalid file. Please upload a .csv or .txt file")
  )
}

# This function will subtract the first absorbance from all points in each well
BaselineNP <- function(n) {
  n <- n - n[1]
}

#Functions to calculate slopes and intercepts
#Using time
LMt <- function(Y, delta, PLATE){
  Time <- PLATE[[1]]
  Yd <- Y[Y<delta]
  Time <- Time[1:length(Yd)]
  regMod <- lm(Yd~Time)
  regRes <- summary(regMod)
  slope <- regRes$coef[2]*1e6
  int <- regRes$coef[1]
  allRes <- c(int, slope)
}
#Using time squared
LMtsq <- function(Y, delta, PLATE){
  Time <- PLATE[[1]]
  Timesq <- Time^2
  Yd <- Y[Y<delta]
  Timesq <- Timesq[1:length(Yd)]
  regMod <- lm(Yd~Timesq)
  regRes <- summary(regMod)
  slope <- regRes$coef[2]*1e9
  int <- regRes$coef[1]
  allRes <- c(int, slope)
}

function(input, output) { #Import user or supplied data
  readDatapre <- reactive({
    inputFile <- input$data1
    if (is.null(inputFile)) {
      return(read.csv(file.path("./Data/Feb 10 tPA variants.csv")))
    } else {
      (
        load_file(input$data1$name, input$data1$datapath, input$sheetd) |>
          as.data.frame()
      )
    }
  })
  output$maxt <- renderUI({
    numericInput("maxt",
      label = h5("Number of points"),
      value = length(readDatapre()[, 1])
    )
  })
#Update user data with max time or time delay or zero baseline
  readData <- reactive({
    if (is.null(input$maxt)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
    readData0 <- readDatapre()[1:input$maxt, ]#set max time
    readData0[[1]] <- readData0[[1]]+input$delay # add time delay
    if (input$zero) {
      #readData1<- as.data.frame(lapply(readData0, function(x) BaselineNP(x))) #This not quite working with time delay
      readData1 <- readData0[,-1] |> map_df(~BaselineNP(.x)) |> add_column("Time"= readData0[[1]], .before = 1) |> as.data.frame()
    } 
    else {
      readData1 <- readData0
    }
    
    readData1
    
  })


  var <- reactive({
    mycols <- colnames(readDatapre()[,-1])
  })


  var1 <- reactive({
    mycols <- colnames(readData()[,-1])
  })


  output$what <- renderUI({
    selectInput("colmnames",
      label = h5("Data in 'Curve' tab"),
      choices = var()
    )
  })


  output$contents <- renderDT({
    # output$contents<-renderDataTable({
    readData()
  })
#Generate results table using slope and intercept functions for time and timesq
  TabRes <- reactive({
    if (is.null(readData())) { return(NULL)} #Very useful to avoid error messages
    AbsCols <- readData()[,-1]
    TabRes <- AbsCols |> map_df(~data.frame(TInt=LMt(.x, input$num, readData())[1],
                                                    TSlope=LMt(.x, input$num, readData())[2],
                                                    TsqInt=LMtsq(.x, input$num, readData())[1],
                                                    TsqSlope=LMtsq(.x, input$num, readData())[2])) |> 
     add_column(Well=colnames(AbsCols), .before = 1)
    TabRes
    #clipr::write_clip(TabRes)   
  })

  #Make the results table into a matrix for presentation as a table   
  output$resultsTable <- renderTable({
    if (is.null(TabRes())) { return(NULL)}
    
    ratepMs <- 0.5 * input$Ext * (input$kcat * input$Sub) / (input$Km + input$Sub)
    
    if (input$names) {
      data <- colnames(readData()[,-1])
    } else if (input$sqr) {
      if (input$pM) {
        data <- (1e-9 * TabRes()[[5]] / ratepMs) * 1e12
      }
      else {
        data <- TabRes()[[5]]
      }
    } else {
      data <- TabRes()[[3]]
    }
    matrix(data, byrow = TRUE, nrow = input$numrows)
  })

  output$tabres <- renderTable({
    TabRes_round <- TabRes() |> mutate(across(2:5, \(x) signif(x, digits = 6)))
    colnames(TabRes_round) <-  c("Wells", "T_Intercept", "T_Slope x1e6", "Tsq_Intercept", "Tsq_Slope x1e9")
    TabRes_round
  })
  
  
  output$myplotAll <- renderPlot({
    if (is.null(input$colmnames)) {
     return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
   
    nWells <- length(readData()[,-1])
    par(mfrow = c(input$numrows, nWells/input$numrows))
    par(mar = c(0.2, 0.2, 0.2, 0.2))
    
    Time <- readData()[[1]]
    Timesq <- Time^2
    for (i in seq_along(readData()[,-1])) {
      Yd <- readData()[[i+1]]
      if (input$sqr) {
        plot(Timesq, Yd,col = "red", pch = 20, xaxt = "n", yaxt = "n", xlim = c(0, max(Timesq)), ylim = c(0, input$num))
        abline(TabRes()[i,4], TabRes()[i,5]*1e-9, col="black", lwd=1)
        legend(x=0, y=input$num, bty = "n", colnames(readData())[i+1], cex = 1.5, xjust = 0.5)
      }
      else{
        plot(Time, Yd,col = "red", pch = 20, xaxt = "n", yaxt = "n", xlim = c(0, max(Time)), ylim = c(0, input$num))
        abline(TabRes()[i,2], TabRes()[i,3]*1e-6, col="black", lwd=1)
        legend(x=0, y=input$num, bty = "n", colnames(readData())[i+1], cex = 1.5, xjust = 0.5) 
      }
      
    }
    
  })

  output$myplot <- renderPlot({
    k<-which(TabRes()[, 1]==input$colmnames)
    Yd <- readData()[[input$colmnames]]
    Time <- readData()[[1]][1:length(Yd)]
    Timesq <- Time^2
    
    if (input$sqr) { 
      plot(Timesq, Yd,col = "red", pch = 1,  xlim = c(0, max(Timesq)), ylim = c(0, input$num),
           main = paste("Slope for time sq plot of", input$colmnames,"=", signif(TabRes()[k,5], 4), "x 1e9 abs/s^2",
                        "over abs of ", input$num, "and", input$maxt, "points"))
      abline(TabRes()[k,4], TabRes()[k,5]*1e-9, col="black", lwd=1)
    }
    else{
      plot(Time, Yd,col = "red", pch = 20,  xlim = c(0, max(Time)), ylim = c(0, input$num),
           main = paste("Slope for time plot of", input$colmnames, "=", signif(TabRes()[k,3], 4), "x 1e6 abs/s",
                        "for absorbance of ", input$num, "and", input$maxt, "points"))
      abline(TabRes()[k,2], TabRes()[k,3]*1e-6, col="black", lwd=1)
    }
  })
  
  output$linear <- renderPlot({
    par(mfrow = c(2,2))
    
    Y <- readData()[[input$colmnames]]
    Yd <- Y[Y<input$num]
    Time <- readData()[[1]][1:length(Yd)]
    Timesq <- Time^2
    LMt <- lm(Yd~Time)
    LMtsq <- lm(Yd~Timesq)
    if (input$sqr) {
      plot(residuals.lm(LMtsq), bty = "n", main= paste("Residuals for fit with adj Rsq", signif(summary(LMtsq)$adj.r.squared),digits=4))
      qqnorm(LMtsq$residuals, main="qq norm", bty = "n")
      qqline(LMtsq$residuals, lty=2, bty = "n")
    }
    else {
      plot(residuals.lm(LMt), bty = "n", main= paste("Residuals for fit with adj Rsq", signif(summary(LMt)$adj.r.squared), digits=4))
      qqnorm(LMt$residuals, main="qq norm", bty = "n")
      qqline(LMt$residuals, lty=2, bty = "n")
    }
  })
  
  output$text3 <- renderText({
    if (input$names) {
      txt <- "Wells"
    } else if (input$sqr) {
      if (input$pM) {
        txt <- paste("Rates of activation in pM/s for maximum absorbance ", input$num, "and", input$maxt, "data points")
      }
      else {
        txt <- paste("Rates Abs/s² x 1e9 for maximum absorbance ", input$num, "and", input$maxt, "data points")
      }
    } else {
      txt <- paste("Rates Abs/s x 1e6 for maximum absorbance ", input$num, "and", input$maxt, "data points")
    }
    
  })
}

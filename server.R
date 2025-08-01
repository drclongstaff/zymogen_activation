library(shiny)
library(readxl)
library(DT)
library(tibble)
library(dplyr)
library(purrr)

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

# Functions to calculate slopes and intercepts
# Using time
LMt <- function(Y, delta, PLATE) {
  Time <- PLATE[[1]]
  Yd <- Y[Y < delta]
  Time <- Time[1:length(Yd)]
  regMod <- lm(Yd ~ Time)
  regRes <- summary(regMod)
  slope <- signif(regRes$coef[2] * 1e6, digits = 4) # 1e6 to make the numbers readable
  int <- regRes$coef[1]
  allRes <- c(int, slope)
}
# Using time squared
LMtsq <- function(Y, delta, PLATE) {
  Time <- PLATE[[1]]
  Timesq <- Time^2
  Yd <- Y[Y < delta]
  Timesq <- Timesq[1:length(Yd)]
  regMod <- lm(Yd ~ Timesq)
  regRes <- summary(regMod)
  slope <- signif(regRes$coef[2] * 1e9, digits = 4) # 1e9 to make the numbers readable
  int <- regRes$coef[1]
  allRes <- c(int, slope)
}

# Import user or supplied data
function(input, output) {
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

  # Get the number or readings or trim them
  output$maxt <- renderUI({
    numericInput("maxt",
      label = h5("Number of points"),
      value = length(readDatapre()[, 1])
    )
  })

  # Update user data with max time or time delay or zero baseline
  readData <- reactive({
    if (is.null(input$maxt)) {
      return(NULL)
    } # To prevent an error before the data has uploaded
    readData0 <- readDatapre()[1:input$maxt, ] # set max time
    readData0[[1]] <- readData0[[1]] + input$delay # add time delay
    # Optional zero by subtracting first reading using BaselineNP function
    if (input$zero) {
      readData1 <- as.data.frame(sapply(readData0[, -1], function(x) BaselineNP(x))) |>
        add_column("Time" = readData0[[1]], .before = 1)
    } else {
      readData1 <- readData0
    }
    readData1
  })

  # Get colnames (wellnames)
  var <- reactive({
    mycols <- colnames(readDatapre()[, -1])
  })


  var1 <- reactive({
    mycols <- colnames(readData()[, -1])
  })

  # Select well for Curve tab investigation
  output$what <- renderUI({
    selectInput("colmnames",
      label = h5("Data in 'Curve' tab"),
      choices = var()
    )
  })

  # This is for the Raw data tab
  output$contents <- renderDT({
    readData()
  })

  # Generate results table using slope and intercept functions for time and timesq
  TabRes <- reactive({
    if (is.null(readData())) {
      return(NULL)
    }
    rData <- readData()
    TabRes <- rData[, -1] |>
      imap(~ data.frame(
        Well = .y, # .y gives you the column name
        TInt = LMt(.x, input$num, rData)[1],
        TSlope = LMt(.x, input$num, rData)[2],
        TsqInt = LMtsq(.x, input$num, rData)[1],
        TsqSlope = LMtsq(.x, input$num, rData)[2]
      )) |>
      list_rbind()
  })

  # Make the TabRes into a matrix for presentation as a table in the UI
  output$resultsTable <- renderTable({
    if (is.null(TabRes())) {
      return(NULL)
    } # To avoid flagging an error on start up
    Tabres <- TabRes()
    # Equation to change time sq rates into pM/s
    ratepMs <- 0.5 * input$Ext * (input$kcat * input$Sub) / (input$Km + input$Sub)

    # Selection of appropriate table of names or of rates, vs time or time sq or in pM/s
    if (input$names) {
      data <- colnames(readData()[, -1])
    } else if (input$sqr) {
      if (input$pM) {
        data <- (1e-9 * Tabres[[5]] / ratepMs) * 1e12
      } else {
        data <- Tabres[[5]]
      }
    } else {
      data <- Tabres[[3]]
    }
    matRes <- matrix(data, byrow = TRUE, nrow = input$numrows)
    # rownames(matRes) <- (paste0("R_", seq(nrow(matRes))))
    colnames(matRes) <- (paste0("C_", seq(ncol(matRes))))
    matRes
  })

  # Table in tab of All results
  output$tabres <- renderTable({
    tabres <- TabRes() #|> mutate(across(2:5, \(x) signif(x, digits = 6)))
    # tabres <- formatC(TabRes(), format = "e", digits = 3)
    # Improve table headings
    colnames(tabres) <- c("Wells", "Intercept_t", "Slope_t x1e6", "Intercept_tsq", "Slope_tsq x1e9")
    tabres
  })

  # PLOTTING
  # Graph of all plots on opening Plots tab
  output$myplotAll <- renderPlot({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To avoid errors before the data has uploaded
    rdata <- readData()
    tabres <- TabRes()
    # Arrangement of plots
    nWells <- length(rdata[, -1])
    par(mfrow = c(input$numrows, nWells / input$numrows))
    par(mar = c(0.2, 0.2, 0.2, 0.2))

    Time <- rdata[[1]]
    Timesq <- Time^2

    # Loop to generate mini plots of wells using time or time sq
    for (i in seq_along(rdata[, -1])) {
      Yd <- rdata[[i + 1]]
      if (input$sqr) {
        plot(Timesq, Yd, col = "red", pch = 20, xaxt = "n", yaxt = "n", xlim = c(0, max(Timesq)), ylim = c(0, input$num))
        abline(tabres[i, 4], tabres[i, 5] * 1e-9, col = "black", lwd = 1) # return table results to correct scale
        legend(x = 0, y = input$num, bty = "n", colnames(rdata)[i + 1], cex = 1.5, xjust = 0.5)
      } else {
        plot(Time, Yd, col = "red", pch = 20, xaxt = "n", yaxt = "n", xlim = c(0, max(Time)), ylim = c(0, input$num))
        abline(tabres[i, 2], tabres[i, 3] * 1e-6, col = "black", lwd = 1) # return table results to correct scale
        legend(x = 0, y = input$num, bty = "n", colnames(rdata)[i + 1], cex = 1.5, xjust = 0.5)
      }
    }
  })

  # Output of single well plot in Curve tab
  output$myplot <- renderPlot({
    # Select the well of interest
    rdata <- readData()
    tabres <- TabRes()
    k <- which(tabres[, 1] == input$colmnames)
    Yd <- rdata[[input$colmnames]]
    Time <- rdata[[1]][1:length(Yd)]
    Timesq <- Time^2

    # Choose time sq or time to plot
    if (input$sqr) {
      plot(Timesq, Yd,
        col = "red", pch = 1, xlim = c(0, max(Timesq)), ylim = c(0, input$num),
        main = paste(
          "Slope for time sq plot of", input$colmnames, "=", TabRes()[k, 5], "x 1e9 abs/s²",
          "over abs of ", input$num, "and", input$maxt, "points"
        )
      )
      abline(tabres[k, 4], tabres[k, 5] * 1e-9, col = "black", lwd = 1)
    } else {
      plot(Time, Yd,
        col = "red", pch = 20, xlim = c(0, max(Time)), ylim = c(0, input$num),
        main = paste(
          "Slope for time plot of", input$colmnames, "=", tabres[k, 3], "x 1e6 abs/s",
          "for absorbance of ", input$num, "and", input$maxt, "points"
        )
      )
      abline(tabres[k, 2], tabres[k, 3] * 1e-6, col = "black", lwd = 1)
    }
  })

  # Add plots of residuals and qqnorm of chosen well in Curve tab
  output$linear <- renderPlot({
    par(mfrow = c(2, 2))
    rdata <- readData()
    Y <- rdata[[input$colmnames]]
    Yd <- Y[Y < input$num]
    Time <- rdata[[1]][1:length(Yd)]
    Timesq <- Time^2

    # In this case run the lm again for residuals
    LMt <- lm(Yd ~ Time)
    LMtsq <- lm(Yd ~ Timesq)
    # Select time or time sq
    if (input$sqr) {
      plot(residuals.lm(LMtsq), bty = "n", main = paste("Residuals for fit with adj Rsq", signif(summary(LMtsq)$adj.r.squared), digits = 4))
      qqnorm(LMtsq$residuals, main = "qq norm", bty = "n")
      qqline(LMtsq$residuals, lty = 2, bty = "n")
    } else {
      plot(residuals.lm(LMt), bty = "n", main = paste("Residuals for fit with adj Rsq", signif(summary(LMt)$adj.r.squared), digits = 4))
      qqnorm(LMt$residuals, main = "qq norm", bty = "n")
      qqline(LMt$residuals, lty = 2, bty = "n")
    }
  })

  # Text3 is the heading above the table (matrix) of result on the Plots tab
  output$text3 <- renderText({
    if (input$names) {
      txt <- "Wells"
    } else if (input$sqr) {
      if (input$pM) {
        txt <- paste("Rates of activation in pM/s for maximum absorbance ", input$num, "and", input$maxt, "data points")
      } else {
        txt <- paste("Rates abs/s² x 1e9 for maximum absorbance ", input$num, "and", input$maxt, "data points")
      }
    } else {
      txt <- paste("Rates abs/s x 1e6 for maximum absorbance ", input$num, "and", input$maxt, "data points")
    }
  })
}

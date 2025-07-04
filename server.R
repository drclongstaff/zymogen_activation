library(shiny)
library(readxl)
library(DT)

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
  output$maxt <- renderUI({
    numericInput("maxt",
      label = h5("Number of points"),
      value = length(readDatapre()[, 1])
    )
  })

  readData <- reactive({
    readDatapre()[1:input$maxt, ]
  })


  var <- reactive({
    plate0 <- readDatapre()
    plate1 <- plate0[, -1]
    mycols <- colnames(plate1)
  })


  var1 <- reactive({
    plate0 <- readData()
    plate1 <- plate0[, -1]
    mycols <- colnames(plate1)
  })


  output$what <- renderUI({
    selectInput("colmnames",
      label = h5("Data in 'Curve' tab"),
      choices = var()
    )
  })


  output$myplot <- renderPlot({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded


    plate0 <- readData()
    Time <- plate0[, 1] + input$delay
    tsq <- Time^2
    plate1 <- plate0[, -1]

    if (input$sqr) {
      tsq
    } else {
      (tsq <- Time)
    }

    delta <- input$num


    if (input$zero) {
      (yic <- plate1[, input$colmnames] - plate1[1, input$colmnames])
    } else {
      (yic <- plate1[, input$colmnames])
    }

    Yi <- yic[yic < delta]
    Tsq <- tsq[1:length(Yi)]
    regrCol <- lm(Yi ~ Tsq)
    Res <- summary(regrCol)

    switch(input$curveRes,
      "Absorbance" = plot(Tsq, Yi, pch = 21, col = "black", bg = "red", cex = 1.4, xlim = c(0, max(Tsq)), ylim = c(0, delta), xlab = "Time", ylab = "Absorbance", main = (if (input$sqr) {
        paste(input$colmnames, "rate for", input$maxt, "data points and", input$num, "absorbance change =", signif(Res$coef[2] * 1e9, digits = 4), "Abs/s² x 1e9")
      } else {
        (paste(input$colmnames, "rate for", input$maxt, "data points and", input$num, "absorbance change =", signif(Res$coef[2] * 1e6, digits = 4), "Abs/s x 1e6"))
      }), cex.lab = 1.6, abline(regrCol, lwd = 2, col = "blue")),
      "Residual" = plot(residuals.lm(regrCol), pch = 3, col = "red", main = paste("Residuals versus index; fit has adjusted R squared of ", signif(Res$adj.r.squared, digits = 4)), xlab = "Time", ylab = "Residuals", cex.lab = 1.6)
    )
  })

  output$contents <- renderDT({
    # output$contents<-renderDataTable({
    readData()
  })




  output$resultsTable <- renderTable({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded

    plate0 <- readData()
    Time <- plate0[, 1] + input$delay
    tsq <- Time^2
    plate1 <- plate0[, -1]

    if (input$sqr) {
      tsq
    } else {
      (tsq <- Time)
    }

    delta <- input$num

    absCols <- length(plate1[1, ])
    RowNum <- input$numrows
    vect1 <- 1:absCols
    # vectz<-1:absCols

    # if(input$zero) for (j in 1:length(plate1[1,])) {plate1[,j]<-plate1[,j]-plate1[1,j]}
    if (input$zero) {
      plate1 <- as.data.frame(lapply(plate1, function(x) BaselineNP(x)))
    } # new using a function call
    else {
      plate1
    }

    # for(i in 1:absCols){
    for (i in seq_along(plate1)) {
      yic <- plate1[, i]
      Yi <- yic[yic < delta]
      Tsq <- tsq[1:length(Yi)]
      regrCol <- lm(Yi ~ Tsq)
      Res <- summary(regrCol)
      if (input$sqr) {
        ans <- Res$coef[2] * 1e9
      } else {
        ans <- Res$coef[2] * 1e6
      }
      vect1[i] <- ans
    }

    ratepMs <- 0.5 * input$Ext * (input$kcat * input$Sub) / (input$Km + input$Sub)

    data <- matrix(vect1, byrow = TRUE, nrow = RowNum)
    # vectz<-1:dataCols
    if (input$names) {
      data <- matrix(colnames(plate1), byrow = TRUE, nrow = RowNum)
    } else if (input$sqr) {
      if (input$pM) {
        data <- round((1e-9 * data / ratepMs) * 1e12, digits = 3)
      } else {
        data <- round(data, digits = 3)
      }
    } else {
      data <- round(data, digits = 3)
    }

    # write.table(data, "clipboard", sep="\t", col.names=F, row.names=F) #enable this line if run locally
    data
  })

  output$text3 <- renderText({
    if (input$sqr) {
      paste("Rates Abs/s² x 1e9 for maximum absorbance ", input$num, "and", input$maxt, "data points")
    } else {
      (paste("Rates Abs/s x 1e6 for maximum absorbance ", input$num, "and", input$maxt, "data points"))
    }
  })

  output$text4 <- renderText({
    if (input$sqr) {
      if (input$pM) {
        paste("Rate of activation in pM/s for maximum absorbance", input$num, "and", input$maxt, "data points")
      } # else if (input$pM) paste( "pM", input$num)
      else {
        ""
      }
    } else {
      ""
    }
  })

  output$myplotAll <- renderPlot({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded

    plate0 <- readData()
    Time <- plate0[, 1] + input$delay
    tsq <- Time^2
    plate1 <- plate0[, -1]

    if (input$sqr) {
      tsq
    } else {
      (tsq <- Time)
    }

    delta <- input$num

    absCols <- length(plate1[1, ])
    RowNum <- input$numrows

    vect1 <- 1:absCols


    par(mfrow = c(RowNum, absCols / RowNum))
    par(mar = c(0.2, 0.2, 0.2, 0.2))

    # if(input$zero) for (j in 1:length(plate1[1,])) {plate1[,j]<-plate1[,j]-plate1[1,j]}
    if (input$zero) {
      plate1 <- as.data.frame(lapply(plate1, function(x) BaselineNP(x)))
    } # new using a function call
    else {
      plate1
    }

    # for(i in 1:absCols){
    for (i in seq_along(plate1)) {
      yic <- plate1[, i]
      Yi <- yic[yic < delta]
      Tsq <- tsq[1:length(Yi)]
      regrCol <- lm(Yi ~ Tsq)
      plot(Tsq, Yi, col = "red", pch = 20, xaxt = "n", yaxt = "n", xlim = c(0, max(tsq)), ylim = c(0, delta))
      abline(regrCol)
      Res <- summary(regrCol)
      if (input$sqr) {
        ans <- signif(Res$coef[2] * 1e9, digits = 4)
      } else {
        (ans <- signif(Res$coef[2] * 1e6, digits = 4))
      }
      legend(0, delta, bty = "n", ans)
      vect1[i] <- ans
    }
  })
}

# Taken from https://suryaprakash.shinyapps.io/Data_Exploration/ and modified.

library(shiny)
library(sqldf)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(reshape2)
library(gridExtra)
library(gtable)
library(grid)
library(plyr)
library(qcc)
library(DT)
library(shinyjs)
library(gtools)
library(V8)
library(VIM)
library(corrplot)
library(tabplot)
library(e1071)
library(vcd)
library(caret)
library(caretEnsemble)
library(randomForest)
library(kernlab)
library(rpart)
library(glmnet)
library(xgboost)
library(gbm)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
options(sqldf.driver = "SQLite")

LFC <- list()
options(shiny.maxRequestSize = 1000 * 1024^2)


server <- (function(input, output) {

  observeEvent(input$reset_button, { js$reset() })

  # Upload a Dataset

  loadfile1 <- reactive({


    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Dataset Loading..Kindly wait")

    S <- input$file1


    if (is.null(S))
      return(NULL)

    read.csv(S$datapath, header = TRUE, stringsAsFactors = TRUE)

  }) # End of Load File Reactive

  # File Upload

  output$FileUpload1 <- renderUI(
    fileInput('file1', 'Upload Training Dataset (CSV File)',
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'))
  )


  # Upload a Dataset

  loadfile2 <- reactive({


    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Dataset Loading..Kindly wait")

    S <- input$file2


    if (is.null(S))
      return(NULL)

    read.csv(S$datapath, header = TRUE, stringsAsFactors = FALSE)

  }) # End of Load File Reactive

  # File Upload

  output$FileUpload2 <- renderUI(
    fileInput('file2', 'Upload Testing Dataset (CSV File)',
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'))
  )


  # Upload a Dataset

  loadfile3 <- reactive({


    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Dataset Loading..Kindly wait")

    S <- input$file3


    if (is.null(S))
      return(NULL)

    read.csv(S$datapath, header = TRUE, stringsAsFactors = TRUE)

  }) # End of Load File Reactive

  # File Upload

  output$FileUpload3 <- renderUI(
    fileInput('file3', 'Upload Cleaned Training Dataset (CSV File)',
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'))
  )


  output$ExploreWay <- renderUI({
    radioButtons("ExploreWays", label = "Explore",
                 choices = list("Summary" = 1, "Structure" = 2, "Missing" = 3, "Correlation" = 4, "Skewness" = 5, "Kurtosis" = 6),
                 selected = 1)
  })

  output$NumPredictors <- renderUI({
    radioButtons("NumPredictor", label = "Include all the predictors",
                 choices = list("Yes" = 1, "No" = 2),
                 selected = 1)
  })


  # Drop down for modelling techniques
  output$MLTS <- renderUI({
    DF <- loadfile3()
    if (is.numeric(DF[, input$Target]) | is.integer(DF[, input$Target])) {
      radioButtons("MLT", label = "Machine Learning Technique",
                   choices = list("Linear Regression" = 1, "Regularized Regression" = 3, "Decision Tree" = 4, "Random Forest" = 5, "Gradient Boosting" = 6, "Extreme Gradient Boosting" = 7, "Support Vector Machine" = 8, "All(No Tuning)" = 9),
                   selected = 1)
    } else { radioButtons("MLT", label = "Machine Learning Technique",
                         choices = list("Logistic Regression" = 2, "Regularized Regression" = 3, "Decision Tree" = 4, "Random Forest" = 5, "Gradient Boosting" = 6, "Extreme Gradient Boosting" = 7, "Support Vector Machine" = 8, "All(No Tuning)" = 9),
                         selected = 1) }
  })

  # Drop down for modelling techniques
  output$PredictMLTS <- renderUI({
    DF <- loadfile3()
    if (is.numeric(DF[, input$Target]) | is.integer(DF[, input$Target])) {
      radioButtons("PredictMLT", label = "Prediction based on",
                   choices = list("Linear Regression" = 1, "Regularized Regression" = 3, "Decision Tree" = 4, "Random Forest" = 5, "Gradient Boosting" = 6, "Extreme Gradient Boosting" = 7, "Support Vector Machine" = 8),
                   selected = 1)
    } else { radioButtons("PredictMLT", label = "Prediction based on",
                         choices = list("Logistic Regression" = 2, "Regularized Regression" = 3, "Decision Tree" = 4, "Random Forest" = 5, "Gradient Boosting" = 6, "Extreme Gradient Boosting" = 7, "Support Vector Machine" = 8),
                         selected = 1) }
  })

  # Tuning of the parameters

  output$Tunings <- renderUI({
    radioButtons("Tuning", label = "Do you want to tune Model",
                 choices = list("Yes" = 1, "No" = 2),
                 selected = 2)
  })

  # Way of Tuning of the parameters

  output$TuningTypes <- renderUI(if (input$Tuning == 1) {
    radioButtons("TuningType", label = "Parameter Tuning Way:",
                 choices = list("Random Search" = 1, "Grid Search" = 2),
                 selected = 1)
  })

  # Tuning Length for Random Search

  output$NumFolds <- renderUI({
    numericInput("NumFolds", "Number of Folds Cross Validation:", 2, min = 1, max = 10)
  })

  # Tuning Length for Random Search

  output$TuneLength <- renderUI(if (input$TuningType == 1 & input$Tuning == 1) {
    textInput("TuneLengths", "Tune Length", value = "", width = NULL, placeholder = NULL)
  })
  # Paramter Tuning for Regularized Regression

  output$RegTunAlphaStart <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    textInput("RegTunAlphaStarts", "Start alpha", value = "", width = NULL, placeholder = NULL)
  })

  output$RegTunAlphaEnd <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    textInput("RegTunAlphaEnds", "End alpha", value = "", width = NULL, placeholder = NULL)
  })

  output$RegTunAlphaStep <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    textInput("RegTunAlphaSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$RegTunLambdaStart <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    textInput("RegTunLambdaStarts", "Start Lambda", value = "", width = NULL, placeholder = NULL)
  })

  output$RegTunLambdaEnd <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    textInput("RegTunLambdaEnds", "End Lambda", value = "", width = NULL, placeholder = NULL)
  })

  output$RegTunLambdaStep <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    textInput("RegTunLambdaSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  # Box for Tuning Regularized Regression
  output$RegTun <- renderUI(if (input$MLT == 3 & input$TuningType == 2) {
    box(column(6, box(uiOutput("RegTunAlphaStart"),
                      uiOutput("RegTunAlphaEnd"),
                      uiOutput("RegTunAlphaStep"), width = 10)),
        column(6, box(uiOutput("RegTunLambdaStart"),
                      uiOutput("RegTunLambdaEnd"),
                      uiOutput("RegTunLambdaStep"), width = 10)), title = "", status = "primary", solidHeader = T)

  })
  # Paramter Tuning for Random Forest

  output$RFTunMtryStart <- renderUI(if (input$MLT == 5 & input$TuningType == 2) {
    textInput("RFTunMtryStarts", "Start mtry", value = "", width = NULL, placeholder = NULL)
  })

  output$RFTunMtryEnd <- renderUI(if (input$MLT == 5 & input$TuningType == 2) {
    textInput("RFTunMtryEnds", "End mtry", value = "", width = NULL, placeholder = NULL)
  })

  output$RFTunMtryStep <- renderUI(if (input$MLT == 5 & input$TuningType == 2) {
    textInput("RFTunMtrySteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  # Box for Tuning Random Forest
  output$RFTun <- renderUI(if (input$MLT == 5 & input$TuningType == 2) {
    column(6, box(uiOutput("RFTunMtryStart"),
                  uiOutput("RFTunMtryEnd"),
                  uiOutput("RFTunMtryStep"), width = 10, title = "", status = "primary", solidHeader = T))

  })


  # Paramter Tuning for Decision Tree

  output$DtTunCpStart <- renderUI(if (input$MLT == 4 & input$TuningType == 2) {
    textInput("DtTunCpStarts", "Start Cp", value = "", width = NULL, placeholder = NULL)
  })

  output$DtTunCpEnd <- renderUI(if (input$MLT == 4 & input$TuningType == 2) {
    textInput("DtTunCpEnds", "End Cp", value = "", width = NULL, placeholder = NULL)
  })

  output$DtTunCpStep <- renderUI(if (input$MLT == 4 & input$TuningType == 2) {
    textInput("DtTunCpSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  # Box for Tuning Decision Tree
  output$DtTun <- renderUI(if (input$MLT == 4 & input$TuningType == 2) {
    column(6, box(uiOutput("DtTunCpStart"),
                  uiOutput("DtTunCpEnd"),
                  uiOutput("DtTunCpStep"), width = 10, title = "", status = "primary", solidHeader = T))

  })


  # Paramter Tuning for XGBoost

  output$XgbTunAlphaStart <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunAlphaStarts", "Start alpha", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunAlphaEnd <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunAlphaEnds", "End alpha", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunAlphaStep <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunAlphaSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunLambdaStart <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunLambdaStarts", "Start Lambda", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunLambdaEnd <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunLambdaEnds", "End Lambda", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunLambdaStep <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunLambdaSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunEtaStart <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunEtaStarts", "Start Eta", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunEtaEnd <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunEtaEnds", "End Eta", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunEtaStep <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunEtaSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunNroundStart <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunNroundStarts", "Start Nroud", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunNroundEnd <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunNroundEnds", "End Nround", value = "", width = NULL, placeholder = NULL)
  })

  output$XgbTunNroundStep <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    textInput("XgbTunNroundSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  # Box for Tuning XGboost
  output$XgbTun <- renderUI(if (input$MLT == 7 & input$TuningType == 2) {
    box(column(3, box(uiOutput("XgbTunAlphaStart"),
                      uiOutput("XgbTunAlphaEnd"),
                      uiOutput("XgbTunAlphaStep"), width = 20)),
        column(3, box(uiOutput("XgbTunLambdaStart"),
                      uiOutput("XgbTunLambdaEnd"),
                      uiOutput("XgbTunLambdaStep"), width = 20)),
        column(3, box(uiOutput("XgbTunEtaStart"),
                      uiOutput("XgbTunEtaEnd"),
                      uiOutput("XgbTunEtaStep"), width = 20)),
        column(3, box(uiOutput("XgbTunNroundStart"),
                      uiOutput("XgbTunNroundEnd"),
                      uiOutput("XgbTunNroundStep"), width = 20)), title = "", status = "primary", solidHeader = T)

  })


  # Paramter Tuning for GBM

  output$GbmTunDepthStart <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunDepthStarts", "Start Depth", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunDepthEnd <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunDepthEnds", "End Depth", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunDepthStep <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunDepthSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunShrinkageStart <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunShrinkageStarts", "Start Shrinkage", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunShrinkageEnd <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunShrinkageEnds", "End Shrinkage", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunShrinkageStep <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunShrinkageSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunMinObsNodeStart <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunMinObsNodeStarts", "Start MinObsNode", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunMinObsNodeEnd <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunMinObsNodeEnds", "End MinObsNode", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunMinObsNodeStep <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunMinObsNodeSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunNtreeStart <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunNtreeStarts", "Start Ntree", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunNtreeEnd <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunNtreeEnds", "End Ntree", value = "", width = NULL, placeholder = NULL)
  })

  output$GbmTunNtreeStep <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    textInput("GbmTunNtreeSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  # Box for Tuning Gbmoost
  output$GbmTun <- renderUI(if (input$MLT == 6 & input$TuningType == 2) {
    box(column(3, box(uiOutput("GbmTunDepthStart"),
                      uiOutput("GbmTunDepthEnd"),
                      uiOutput("GbmTunDepthStep"), width = 20)),
        column(3, box(uiOutput("GbmTunShrinkageStart"),
                      uiOutput("GbmTunShrinkageEnd"),
                      uiOutput("GbmTunShrinkageStep"), width = 20)),
        column(3, box(uiOutput("GbmTunMinObsNodeStart"),
                      uiOutput("GbmTunMinObsNodeEnd"),
                      uiOutput("GbmTunMinObsNodeStep"), width = 20)),
        column(3, box(uiOutput("GbmTunNtreeStart"),
                      uiOutput("GbmTunNtreeEnd"),
                      uiOutput("GbmTunNtreeStep"), width = 20)), title = "", status = "primary", solidHeader = T)

  })


  # Paramter Tuning for SVM

  output$SvmTunSigmaStart <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    textInput("SvmTunSigmaStarts", "Start Sigma", value = "", width = NULL, placeholder = NULL)
  })

  output$SvmTunSigmaEnd <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    textInput("SvmTunSigmaEnds", "End Sigma", value = "", width = NULL, placeholder = NULL)
  })

  output$SvmTunSigmaStep <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    textInput("SvmTunSigmaSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  output$SvmTunCStart <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    textInput("SvmTunCStarts", "Start C", value = "", width = NULL, placeholder = NULL)
  })

  output$SvmTunCEnd <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    textInput("SvmTunCEnds", "End C", value = "", width = NULL, placeholder = NULL)
  })

  output$SvmTunCStep <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    textInput("SvmTunCSteps", "Step", value = "", width = NULL, placeholder = NULL)
  })

  # Box for Tuning SVM
  output$SvmTun <- renderUI(if (input$MLT == 8 & input$TuningType == 2) {
    box(column(6, box(uiOutput("SvmTunSigmaStart"),
                      uiOutput("SvmTunSigmaEnd"),
                      uiOutput("SvmTunSigmaStep"), width = 10)),
        column(6, box(uiOutput("SvmTunCStart"),
                      uiOutput("SvmTunCEnd"),
                      uiOutput("SvmTunCStep"), width = 10)), title = "", status = "primary", solidHeader = T)

  })


  # Choose which Data Type to be Factor

  output$FactorList <- renderUI({
    selectInput(
      "FactorLists",
      label = "Convert following to Factor Data Type",
      "", selectize = TRUE, multiple = TRUE, choices = names(loadfile1())
    )
  })

  # Choose the Target Variable

  output$Targets <- renderUI({
    selectInput(
      "Target",
      label = "Chose the Target Variable",
      "", selectize = TRUE, multiple = FALSE, choices = names(loadfile3())
    )
  })


  # Choose the Target Variable

  output$Predictors <- renderUI(if (input$NumPredictor == 2) {
    selectInput(
      "Predictor",
      label = "Chose Predictors",
      "", selectize = TRUE, multiple = TRUE, choices = names(loadfile3())
    )
  })


  # Choose which Data Type to be Numeric

  output$NumericList <- renderUI({
    selectInput(
      "NumericLists",
      label = "Convert following to Factor Data Type",
      "", selectize = TRUE, multiple = TRUE, choices = names(loadfile1())
    )
  })

  # Choose Histogram Parameter

  output$HistParams <- renderUI(if (input$PlotType == "Histogram") {
    nums <- sapply(loadfile1(), is.numeric)
    numericcols <- as.list(colnames(loadfile1()[, nums]))
    selectInput(
      "HistParam",
      label = "Plot Histogram",
      "", selectize = TRUE, multiple = FALSE, choices = numericcols
    )
  })

  # Choose BoxPlot Parameter

  output$BoxParams <- renderUI(if (input$PlotType == "Box") {
    nums <- sapply(loadfile1(), is.numeric)
    numericcols <- as.list(colnames(loadfile1()[, nums]))
    selectInput(
      "BoxParam",
      label = "Box Plot",
      "", selectize = TRUE, multiple = FALSE, choices = numericcols
    )
  })


  # Choose Group By Parameter for Histogram

  output$GrByBox <- renderUI(if (input$PlotType == "Box") {
    nums <- sapply(loadfile1(), is.numeric)
    numericcols <- as.list(colnames(loadfile1()[, nums]))
    index <- which(names(loadfile1()) %in% numericcols)
    nonnumericcols <- as.list(colnames(loadfile1()[, -index]))
    selectInput(
      "GrByBoxs",
      label = "Group By",
      "", selectize = TRUE, multiple = FALSE, choices = nonnumericcols
    )
  })

  # Dropdown to select X-axis for plot

  output$XaxisTypes <- renderUI(if (input$PlotType == "Scatter") {
    nums <- sapply(loadfile1(), is.numeric)
    numericcols <- as.list(colnames(loadfile1()[, nums]))
    selectInput(
      "Xaxis",
      label = "Select Xaxis",
      "", selectize = TRUE, multiple = FALSE, choices = numericcols
    )
  })

  # Dropdown to select y-axis for plot

  output$YaxisTypes <- renderUI(if (input$PlotType == "Scatter") {
    nums <- sapply(loadfile1(), is.numeric)
    numericcols <- as.list(colnames(loadfile1()[, nums]))
    selectInput(
      "Yaxis",
      label = "Select Yaxis",
      "", selectize = TRUE, multiple = FALSE, choices = numericcols
    )
  })

  # Dropdown to select first variable for mosaic plot

  output$MosaicFirst <- renderUI(if (input$PlotType == "Mosaic") {
    factors <- sapply(loadfile1(), is.factor)
    factorcols <- as.list(colnames(loadfile1()[, factors]))
    selectInput(
      "Mosaic1st",
      label = "Select First Variable",
      "", selectize = TRUE, multiple = FALSE, choices = factorcols
    )
  })

  # Dropdown to select select variable for plot

  output$MosaicSecond <- renderUI(if (input$PlotType == "Mosaic") {
    factors <- sapply(loadfile1(), is.factor)
    factorcols <- as.list(colnames(loadfile1()[, factors]))
    selectInput(
      "Mosaic2nd",
      label = "Select Second Variable",
      "", selectize = TRUE, multiple = FALSE, choices = factorcols
    )
  })


  # Dropdown to select columns for tabplot

  output$SelectTabPlots <- renderUI({
    selectInput(
      "SelectTabPlot",
      label = "Select Columns",
      "", selectize = TRUE, multiple = TRUE, choices = names(loadfile1())
    )
  })

  # Dropdown to sort columns for tabplot

  output$SortTabPlots <- renderUI({
    selectInput(
      "SortTabPlot",
      label = "Select Sorting Column",
      "", selectize = TRUE, multiple = FALSE, choices = input$SelectTabPlot
    )
  })

  # Slider Input for range for the sorted column
  output$FilterTabPlots <- renderUI({

    sliderInput('FilterTabPlot',
                paste0("Choose Filter Range on Sorted Column"),
                min = 0,
                max = 100,
                value = c(0, 100), round = TRUE)

  })

  # Slider Input for range for the Correlation Range
  output$CorRanges <- renderUI({

    sliderInput('CorRange',
                paste0("Exclude Correlation Between"),
                min = -1,
                max = 1,
                step = 0.1,
                value = c(-1, 1), round = FALSE)

  })

  # Dropdown to choose Plot Type

  output$PlotTypes <- renderUI({
    selectInput(
      "PlotType",
      label = "Select Plot",
      "", selectize = TRUE, multiple = FALSE, choices = c("Missing", "Histogram", "Box", "Scatter", "Scatter Matrix", "Correlation", "Tabular", "Mosaic")
    )
  })


  # Dropdown to choose Metric Type

  output$MetricTypes <- renderUI({
    selectInput(
      "MetricType",
      label = "Select Metric",
      "", selectize = TRUE, multiple = FALSE, choices = c("RMSE", "ROC", "Accuracy", "Kappa")
    )
  })

  # changing Data Type of the Dataset

  DataTypeConversion <- reactive({


    DF <- loadfile1()
    DF[input$FactorLists] <- sapply(DF[input$FactorLists], as.factor)
    DF[input$NumericLists] <- sapply(DF[input$NumericLists], as.numeric)
    DF


  })

  # Provide missing Summmary of the Dataset

  output$Summary <- renderPrint(if (input$Go) {

    DF <- DataTypeConversion()

    if (isolate(input$ExploreWays) == 1) {
      summary(DF)
    } else if (isolate(input$ExploreWays) == 2) {
      str(DF)
    } else if (isolate(input$ExploreWays) == 3) {
      sort(colMeans(is.na(DF))[colMeans(is.na(DF)) > 0] * 100, decreasing = T)
    } else if (isolate(input$ExploreWays) == 4) {
      round(cor(DF[, sapply(DF, is.numeric)], use = "pairwise", method = "pearson"), 3)
    } else if (isolate(input$ExploreWays) == 5) {
      t <- sapply(DF, is.numeric)
      sort(sapply(DF[, t], skewness), decreasing = T)
    } else if (isolate(input$ExploreWays) == 6) {
      t <- sapply(DF, is.numeric)
      sort(sapply(DF[, t], kurtosis), decreasing = T)

    }

  })


  # Provide missing Summmary of the Dataset

  output$Model <- renderPrint(if (input$Go2) {

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Processing is going on..Kindly wait")

    set.seed(7)
    trainControl <- trainControl(method = "cv", number = isolate(as.numeric(input$NumFolds)))

    DF <- loadfile3()
    index <- which(names(DF) %in% input$Target)
    Metric <- input$MetricType
    if (input$NumPredictor == 1) {
      Formula <- as.formula(paste(colnames(DF)[index], paste0(colnames(DF)[-index], collapse = " + "), sep = " ~ "))
    } else {
      Formula <- as.formula(paste(colnames(DF)[index], paste0(isolate(input$Predictor), collapse = " + "), sep = " ~ "))
    }
    if (isolate(input$Tuning) == 1) {

      if (isolate(input$MLT) == 1) {
        fit.linear <<- train(Formula, data = DF, method = "lm", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(LinearRegression = fit.linear, LinearRegression = fit.linear))
        print(fit.linear)
        print(varImp(fit.linear))
        summary(results)
      } else if (isolate(input$MLT) == 2) {
        loggrid <<- expand.grid(lambda = seq(isolate(as.numeric(input$RegTunAlphaStarts)), isolate(as.numeric(input$RegTunAlphaEnds)), isolate(as.numeric(input$RegTunAlphaSteps))),
                                alpha = seq(isolate(as.numeric(input$RegTunLambdaStarts)), isolate(as.numeric(input$RegTunLambdaEnds)), isolate(as.numeric(input$RegTunLambdaSteps))))

        fit.log <<- train(Formula, data = DF, method = "glm", metric = "ROC",
                          preProc = c("center", "scale"), trControl = trainControl, tuneGrid = loggrid)

        results <<- resamples(list(Logistic = fit.log, Logistic = fit.log))

        print(fit.log)
        print(varImp(fit.log))
        summary(results)

      } else if (isolate(input$MLT) == 3 & isolate(input$TuningType) == 2) {
        reggrid <<- expand.grid(lambda = seq(isolate(as.numeric(input$RegTunAlphaStarts)), isolate(as.numeric(input$RegTunAlphaEnds)), isolate(as.numeric(input$RegTunAlphaSteps))),
                                alpha = seq(isolate(as.numeric(input$RegTunLambdaStarts)), isolate(as.numeric(input$RegTunLambdaEnds)), isolate(as.numeric(input$RegTunLambdaSteps))))

        fit.hybrid <<- train(Formula, data = DF, method = "glmnet", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl, tuneGrid = reggrid)
        results <<- resamples(list(Regularized = fit.hybrid, Regularized = fit.hybrid))
        print(fit.hybrid)
        print(varImp(fit.hybrid))
        summary(results)
      } else if (isolate(input$MLT) == 3 & isolate(input$TuningType) == 1) {
        fit.hybrid <<- train(Formula, data = DF, method = "glmnet", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl, tuneGrid = reggrid)
        results <<- resamples(list(Regularized = fit.hybrid, Regularized = fit.hybrid))
        print(fit.hybrid)
        print(varImp(fit.hybrid))
        summary(results)
      } else if (isolate(input$MLT) == 4 & isolate(input$TuningType) == 2) {
        dtgrid <<- expand.grid(cp = seq(isolate(as.numeric(input$DtTunCpStarts)), isolate(as.numeric(input$DtTunCpEnds)), isolate(as.numeric(input$DtTunCpSteps))))

        fit.rpart <<- train(Formula, data = DF, method = "rpart", metric = Metric,
                            preProc = c("center", "scale"), trControl = trainControl, tuneGrid = dtgrid)
        results <<- resamples(list(DecisionTree = fit.rpart, DecisionTree = fit.rpart))
        print(fit.rpart)
        print(varImp(fit.rpart))
        summary(results)

      } else if (isolate(input$MLT) == 4 & isolate(input$TuningType) == 1) {
        fit.rpart <<- train(Formula, data = DF, method = "rpart", metric = Metric,
                            preProc = c("center", "scale"), trControl = trainControl, tuneLength = isolate(as.numeric(input$TuneLengths)))
        results <<- resamples(list(DecisionTree = fit.rpart, DecisionTree = fit.rpart))
        print(fit.rpart)
        print(varImp(fit.rpart))
        summary(results)

      } else if (isolate(input$MLT) == 5 & isolate(input$TuningType) == 2) {
        rfgrid <<- expand.grid(mtry = seq(isolate(as.numeric(input$RFTunMtryStarts)), isolate(as.numeric(input$RFTunMtryEnds)), isolate(as.numeric(input$RFTunMtrySteps))))
        fit.rf <<- train(Formula, data = DF, method = "rf", metric = Metric,
                         preProc = c("center", "scale"), trControl = trainControl, tuneGrid = rfgrid, importance = T)
        results <<- resamples(list(RandomForest = fit.rf, RandomForest = fit.rf))
        print(fit.rf)
        print(varImp(fit.rf))
        summary(results)
      } else if (isolate(input$MLT) == 5 & isolate(input$TuningType) == 1) {

        fit.rf <<- train(Formula, data = DF, method = "rf", metric = Metric,
                         preProc = c("center", "scale"), trControl = trainControl, importance = T, tuneLength = isolate(as.numeric(input$TuneLengths)))
        results <<- resamples(list(RandomForest = fit.rf, RandomForest = fit.rf))
        print(fit.rf)
        print(varImp(fit.rf))
        summary(results)

      } else if (isolate(input$MLT) == 6 & isolate(input$TuningType) == 2) {
        gbmgrid <<- expand.grid(interaction.depth = seq(isolate(as.numeric(input$GbmTunDepthStarts)), isolate(as.numeric(input$GbmTunDepthEnds)), isolate(as.numeric(input$GbmTunDepthSteps))),
                                n.trees = seq(isolate(as.numeric(input$GbmTunNtreeStarts)), isolate(as.numeric(input$GbmTunNtreeEnds)), isolate(as.numeric(input$GbmTunNtreeSteps))),
                                shrinkage = seq(isolate(as.numeric(input$GbmTunShrinkageStarts)), isolate(as.numeric(input$GbmTunShrinkageEnds)), isolate(as.numeric(input$GbmTunShrinkageSteps))),
                                n.minobsinnode = seq(isolate(as.numeric(input$GbmTunMinObsNodeStarts)), isolate(as.numeric(input$GbmTunMinObsNodeEnds)), isolate(as.numeric(input$GbmTunMinObsNodeSteps))))

        fit.gbm <<- train(Formula, data = DF, method = "gbm", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl, tuneGrid = gbmgrid)
        results <<- resamples(list(GradientBoosting = fit.gbm, GradientBoosting = fit.gbm))
        print(fit.gbm)
        print(varImp(fit.gbm))
        summary(results)
      } else if (isolate(input$MLT) == 6 & isolate(input$TuningType) == 1) {
        fit.gbm <<- train(Formula, data = DF, method = "gbm", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl, tuneLength = isolate(as.numeric(input$TuneLengths)))
        results <<- resamples(list(GradientBoosting = fit.gbm, GradientBoosting = fit.gbm))
        print(fit.gbm)
        print(varImp(fit.gbm))
        summary(results)

      } else if (isolate(input$MLT) == 7 & isolate(input$TuningType) == 2) {
        xgbgrid <<- expand.grid(lambda = seq(isolate(as.numeric(input$XgbTunAlphaStarts)), isolate(as.numeric(input$XgbTunAlphaEnds)), isolate(as.numeric(input$XgbTunAlphaSteps))),
                                alpha = seq(isolate(as.numeric(input$XgbTunLambdaStarts)), isolate(as.numeric(input$XgbTunLambdaEnds)), isolate(as.numeric(input$XgbTunLambdaSteps))),
                                eta = seq(isolate(as.numeric(input$XgbTunEtaStarts)), isolate(as.numeric(input$XgbTunEtaEnds)), isolate(as.numeric(input$XgbTunEtaSteps))),
                                nrounds = seq(isolate(as.numeric(input$XgbTunNroundStarts)), isolate(as.numeric(input$XgbTunNroundEnds)), isolate(as.numeric(input$XgbTunNroundSteps))))

        fit.xgb <<- train(Formula, data = DF, method = "xgbLinear", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl, tuneGrid = xgbgrid)
        results <<- resamples(list(ExtremeGradientBoosting = fit.xgb, ExtremeGradientBoosting = fit.xgb))
        #LFC$ExtremeGradientBoosting<-fit.xgb
        print(fit.xgb)
        print(varImp(fit.xgb))
        summary(results)
      } else if (isolate(input$MLT) == 7 & isolate(input$TuningType) == 1) {
        fit.xgb <<- train(Formula, data = DF, method = "xgbLinear", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl, tuneLength = isolate(as.numeric(input$TuneLengths)))
        results <<- resamples(list(ExtremeGradientBoosting = fit.xgb, ExtremeGradientBoosting = fit.xgb))
        #LFC$ExtremeGradientBoosting<-fit.xgb
        print(fit.xgb)
        print(varImp(fit.xgb))
        summary(results)
      } else if (isolate(input$MLT) == 8 & isolate(input$TuningType) == 2) {
        svmgrid <<- expand.grid(sigma = seq(isolate(as.numeric(input$SvmTunSigmaStarts)), isolate(as.numeric(input$SvmTunSigmaEnds)), isolate(as.numeric(input$SvmTunSigmaSteps))),
                                C = seq(isolate(as.numeric(input$SvmTunCStarts)), isolate(as.numeric(input$SvmTunCEnds)), isolate(as.numeric(input$SvmTunCSteps))))

        fit.svm <<- train(Formula, data = DF, method = "svmRadial", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl, tuneGrid = svmgrid)
        results <<- resamples(list(SupportVectorMachine = fit.svm, SupportVectorMachine = fit.svm))
        #LFC$SupportVectorMachine<-fit.svm
        print(fit.svm)
        print(varImp(fit.svm))
        summary(results)
      } else if (isolate(input$MLT) == 8 & isolate(input$TuningType) == 1) {
        fit.svm <<- train(Formula, data = DF, method = "svmRadial", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl, tuneLength = isolate(as.numeric(input$TuneLengths)))
        results <<- resamples(list(SupportVectorMachine = fit.svm, SupportVectorMachine = fit.svm))

        print(fit.svm)
        print(varImp(fit.svm))
        summary(results)
      } else if (isolate(input$MLT) == 9) {

        if (is.numeric(DF[, input$Target]) | is.integer(DF[, input$Target])) {

          fit.linear <<- train(Formula, data = DF, method = "lm", metric = Metric,
                               preProc = c("center", "scale"), trControl = trainControl)
        } else {

          fit.log <<- train(Formula, data = DF, method = "glm", metric = "ROC",
                            preProc = c("center", "scale"), trControl = trainControl)
        }

        fit.hybrid <<- train(Formula, data = DF, method = "glmnet", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl)

        fit.rpart <<- train(Formula, data = DF, method = "rpart", metric = Metric,
                            preProc = c("center", "scale"), trControl = trainControl)

        fit.rf <<- train(Formula, data = DF, method = "rf", metric = Metric,
                         preProc = c("center", "scale"), trControl = trainControl)

        fit.gbm <<- train(Formula, data = DF, method = "gbm", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        fit.xgb <<- train(Formula, data = DF, method = "xgbLinear", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        fit.svm <<- train(Formula, data = DF, method = "svmRadial", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        if (is.numeric(DF[, input$Target]) | is.integer(DF[, input$Target])) {
          results <<- resamples(list(Linear = fit.linear, DecisionTree = fit.rf, RandomForest = fit.rf, ExtremeGradientBoosting = fit.xgb, Regularized = fit.glmnet, GradientBoosting = fit.gbm, SupportVectorMachine = fit.svm))
        } else {
          results <<- resamples(list(Logistic = fit.log, DecisionTree = fit.rf, RandomForest = fit.rf, ExtremeGradientBoosting = fit.xgb, Regularized = fit.glmnet, GradientBoosting = fit.gbm, SupportVectorMachine = fit.svm))
        }
        summary(results)


      }
    } else {
      if (isolate(input$MLT) == 1) {
        fit.linear <<- train(Formula, data = DF, method = "lm", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(LinearRegression = fit.linear, LinearRegression = fit.linear))
        print(fit.linear)
        print(varImp(fit.linear))
        summary(results)
      } else if (isolate(input$MLT) == 2) {
        fit.log <<- train(Formula, data = DF, method = "glm", metric = "ROC",
                          preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(Logistic = fit.log, Logistic = fit.log))

        print(fit.log)
        print(varImp(fit.log))
        summary(results)

      } else if (isolate(input$MLT) == 3) {
        fit.hybrid <<- train(Formula, data = DF, method = "glmnet", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(Regularized = fit.hybrid, Regularized = fit.hybrid))
        print(fit.hybrid)
        print(varImp(fit.hybrid))
        summary(results)

      }else if (isolate(input$MLT) == 4) {
        fit.rpart <<- train(Formula, data = DF, method = "rpart", metric = Metric,
                            preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(DecisionTree = fit.rpart, DecisionTree = fit.rpart))
        print(fit.rpart)
        print(varImp(fit.rpart))
        summary(results)
      } else if (isolate(input$MLT) == 5) {
        fit.rf <<- train(Formula, data = DF, method = "rf", metric = Metric,
                         preProc = c("center", "scale"), trControl = trainControl, importance = T)

        results <- resamples(list(RandomForest = fit.rf, RandomForest = fit.rf))
        print(fit.rf)
        print(varImp(fit.rf))
        summary(results)
      } else if (isolate(input$MLT) == 6) {
        fit.gbm <<- train(Formula, data = DF, method = "gbm", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        results <- resamples(list(GradientBoosting = fit.gbm, GradientBoosting = fit.gbm))
        print(fit.gbm)
        print(varImp(fit.gbm))
        summary(results)
      }else if (isolate(input$MLT) == 7) {
        fit.xgb <<- train(Formula, data = DF, method = "xgbLinear", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(ExtremeGradientBoosting = fit.xgb, ExtremeGradientBoosting = fit.xgb))
        print(fit.xgb)
        print(varImp(fit.xgb))
        summary(results)
      } else if (isolate(input$MLT) == 8) {
        fit.svm <<- train(Formula, data = DF, method = "svmRadial", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        results <<- resamples(list(SupportVectorMachine = fit.svm, SupportVectorMachine = fit.svm))
        print(fit.svm)
        print(varImp(fit.svm))
        summary(results)
      } else if (isolate(input$MLT) == 9) {

        if (is.numeric(DF[, input$Target]) | is.integer(DF[, input$Target])) {

          fit.linear <<- train(Formula, data = DF, method = "lm", metric = Metric,
                               preProc = c("center", "scale"), trControl = trainControl)
        } else {

          fit.log <<- train(Formula, data = DF, method = "glm", metric = "ROC",
                            preProc = c("center", "scale"), trControl = trainControl)
        }

        fit.hybrid <<- train(Formula, data = DF, method = "glmnet", metric = Metric,
                             preProc = c("center", "scale"), trControl = trainControl)

        fit.rpart <<- train(Formula, data = DF, method = "rpart", metric = Metric,
                            preProc = c("center", "scale"), trControl = trainControl)

        fit.rf <<- train(Formula, data = DF, method = "rf", metric = Metric,
                         preProc = c("center", "scale"), trControl = trainControl)

        fit.gbm <<- train(Formula, data = DF, method = "gbm", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        fit.xgb <<- train(Formula, data = DF, method = "xgbLinear", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        fit.svm <<- train(Formula, data = DF, method = "svmRadial", metric = Metric,
                          preProc = c("center", "scale"), trControl = trainControl)

        if (is.numeric(DF[, input$Target]) | is.integer(DF[, input$Target])) {
          results <<- resamples(list(Linear = fit.linear, DecisionTree = fit.rf, RandomForest = fit.rf, ExtremeGradientBoosting = fit.xgb, Regularized = fit.hybrid, GradientBoosting = fit.gbm, SupportVectorMachine = fit.svm))
        } else {
          results <<- resamples(list(Logistic = fit.log, DecisionTree = fit.rf, RandomForest = fit.rf, ExtremeGradientBoosting = fit.xgb, Regularized = fit.hybrid, GradientBoosting = fit.gbm, SupportVectorMachine = fit.svm))
        }
        summary(results)


      }
    }

  })


  # Provide Do's and Dont's in file upload

  output$TestUploadRules <- renderText({
    print(paste("Guidelines:", "1. Test File is clean with no missing values",
                "2. Number of columns should be equal to one less in Number of columns
                in training dataset", "3. Column Names should Match", "5. Same Transformations applied across Training and Test DataSet",
                "6. Algorithm has been processed in previous step", "If any of the above assumption is failed then
Model will not run or predictions will not be correct", sep = "\n"))
  })


  # Provide guidelines to upload the test dataset

  output$PredictModel <- renderDataTable(if (input$Go3) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Processing is going on..Kindly wait")

    DFTest <<- loadfile2()
    if (isolate(input$PredictMLT) == 1) {
      DFTest$Prediction <- predict(fit.linear, DFTest)
    } else if (isolate(input$PredictMLT) == 2) {
      DFTest$Prediction <- predict(fit.log, DFTest)
    } else if (isolate(input$PredictMLT) == 3) {
      DFTest$Prediction <- predict(fit.hybrid, DFTest)
    } else if (isolate(input$PredictMLT) == 4) {
      DFTest$Prediction <- predict(fit.rpart, DFTest)
    } else if (isolate(input$PredictMLT) == 5) {
      DFTest$Prediction <- predict(fit.rf, DFTest)
    } else if (isolate(input$PredictMLT) == 6) {
      DFTest$Prediction <- predict(fit.gbm, DFTest)
    } else if (isolate(input$PredictMLT) == 7) {
      DFTest$Prediction <- predict(fit.xgb, DFTest)
    } else if (isolate(input$PredictMLT) == 8) {
      DFTest$Prediction <- predict(fit.svm, DFTest)
    }
    write.csv(DFTest, "Prediction.csv")
    DFTest
  }, options = list(lengthMenu = c(5, 10, 15), pageLength = 5, scrollX = T))

  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      dev.off()
    })

  # Provide guidelines to upload the train dataset during model development

  output$GuideTrain <- renderText({
    print(paste("Note:1.There should not be missing data in the file",
                "2. This is because few Models does not accept missing values", sep = "\n"))
  })

  # Missing Pattern Plot
  output$MissingPattern <- renderPlot({
    DF <- DataTypeConversion()
    aggr(DF, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(data), cex.axis = .7, gap = 3, ylab = c("Histogram of missing data", "Pattern"))
  })

  # Correlation Plot
  output$CorPlot <- renderPlot(if (input$Go1) {
    DF <- DataTypeConversion()
    M <- cor(na.omit(DF[, sapply(DF, is.numeric)]))


    # correlations
    row_indic <- apply(M, 1, function(x) sum(x > input$CorRange[2] | x < input$CorRange[1]) > 1)

    correlations <- M[row_indic, row_indic]
    corrplot(correlations, method = "number")

    #}
  })

  # Scatter Plot All Pairs
  output$ScatterAllPairs <- renderPlot({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Plot will be displayed..Kindly wait")

    DF <- DataTypeConversion()

    pairs(DF[, sapply(DF, is.numeric)],
          main = "Simple Scatterplot Matrix")
    #}
  })


  # Scatter Plot Single Pairs
  output$ScatterSinglePair <- renderPlotly(if (input$PlotType == "Scatter") {
    DF <- DataTypeConversion()

    ggplot(DF, aes_string(x = input$Xaxis, y = input$Yaxis)) +
      geom_jitter(size = 2) +
      xlab(paste(input$Xaxis)) +
      ylab(input$Yaxis) +
      geom_smooth(method = lm) +
      ggtitle(paste(input$Xaxis, " Vs ", input$Yaxis)) +
      theme(plot.title = element_text(size = 15, face = "bold"), axis.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(vjust = 0.5, size = 10, face = "bold"), axis.text.y = element_text(size = 10, face = "bold"), legend.text = element_text(size = 12))

  })


  # Histogram
  output$Hist <- renderPlotly(if (input$PlotType == "Histogram") {
    DF <- DataTypeConversion()

    H <- hist(DF[, input$HistParam], plot = FALSE)
    minimum <- min(H$breaks, na.rm = TRUE)
    maximum <- max(H$breaks, na.rm = TRUE)
    step <- H$breaks[2] - H$breaks[1]

    ggplot(DF, aes_string(x = input$HistParam)) +
      stat_bin(binwidth = step, colour = "blue", fill = "pink") +
      stat_bin(binwidth = step, geom = "text", aes(label = scales::percent((..count.. / sum(..count..)))), vjust = -1.5) +
      scale_x_continuous(breaks = seq(minimum, maximum, by = step)) +
      theme_bw()


  })

  # Box Plot
  output$BoxPlot <- renderPlotly(if (input$PlotType == "Box") {
    DF <- DataTypeConversion()

    ggplot(DF, aes_string(x = input$GrByBoxs, y = input$BoxParam, fill = input$GrByBoxs)) +
      geom_boxplot() +
      theme_bw()


  })


  # tabPlot
  output$tabPlot <- renderPlot(if (input$Go1 & input$PlotType == "Tabular") {

    DF <- DataTypeConversion()
    tableplot(DF, select_string = isolate(input$SelectTabPlot), sortCol = isolate(input$SortTabPlot), from = as.numeric(isolate(input$FilterTabPlot[1])), to = as.numeric(isolate(input$FilterTabPlot[2])))


  })

  # Mosaic Plot
  output$MosaicPlot <- renderPlot(if (input$PlotType == "Mosaic") {

    DF <- DataTypeConversion()
    x <- table(DF[, input$Mosaic1st], DF[, input$Mosaic2nd])
    mosaicplot(x, shade = T, legend = T, xlab = input$Mosaic1st, ylab = input$Mosaic2nd, main = "")


  })

  # Association Plot
  output$AssocPlot <- renderPlot(if (input$PlotType == "Mosaic") {

    DF <- DataTypeConversion()
    x <- table(DF[, input$Mosaic1st], DF[, input$Mosaic2nd])
    assoc(x, xlab = input$Mosaic1st, ylab = input$Mosaic2nd, main = "", shade = T)


  })


  # tabPlot
  output$Plots <- renderUI({

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Processing is going on..Kindly wait")

    if (input$PlotType == "Box") {
      box(uiOutput("BoxParams"),
          uiOutput("GrByBox"),
          plotlyOutput("BoxPlot", height = 520, width = 1200), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Histogram") {
      box(uiOutput("HistParams"),
          plotlyOutput("Hist", height = 520, width = 1200), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Scatter") {
      box(uiOutput("XaxisTypes"),
          uiOutput("YaxisTypes"),
          plotlyOutput("ScatterSinglePair", height = 520, width = 1200), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Tabular") {
      box(uiOutput("SelectTabPlots"),
          uiOutput("SortTabPlots"),
          uiOutput("FilterTabPlots"),
          fluidRow(column(6, actionButton("Go1", "Plot"))),
          plotOutput("tabPlot", height = 500, width = 1200), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Scatter Matrix") {
      box(
        fluidRow(plotOutput("ScatterAllPairs", height = 1000, width = 1000)), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Correlation") {
      box(uiOutput("CorRanges"),
          fluidRow(column(6, actionButton("Go1", "Plot"))),
          plotOutput("CorPlot", height = 1000, width = 1000), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Missing") {
      box(
        plotOutput("MissingPattern", height = 500, width = 1200), title = "", status = "primary", width = 100, solidHeader = T)
    } else if (input$PlotType == "Mosaic") {
      box(uiOutput("MosaicFirst"),
          uiOutput("MosaicSecond"),
          plotOutput("MosaicPlot", height = 500, width = 1200),
          plotOutput("AssocPlot", height = 500, width = 1200), title = "", status = "primary", width = 100, solidHeader = T)
    }
  })


})


# ui part


ui <- dashboardPage(
  dashboardHeader(title = "Data Exploration and Machine Learning", titleWidth = 430),
  dashboardSidebar(disable = TRUE, tags$style(HTML(".sidebar { height: 110vh; overflow-y: auto; }"))),
  dashboardBody(useShinyjs(),
                # Refreshing Shint APP
                extendShinyjs(text = jsResetCode, functions = "winprint"),
                # Hiding Errors
                tags$style(
                  type = "text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"),

                tabsetPanel(
                  tabPanel("Data", fluidRow(column(6, isolate(uiOutput("FileUpload1"))))),
                  tabPanel("Explore", uiOutput("ExploreWay"),
                           fluidRow(column(6, actionButton("Go", "Process"))),
                           verbatimTextOutput("Summary")),
                  tabPanel("Plots",
                           uiOutput("PlotTypes"),
                           uiOutput("Plots")),
                  tabPanel("Run and Tune Model",
                           fluidRow(column(4, box(
                             verbatimTextOutput("GuideTrain"),
                             uiOutput("FileUpload3"),
                             uiOutput("Targets"),
                             uiOutput("NumPredictors"),
                             uiOutput("Predictors"),
                             uiOutput("MetricTypes"),
                             uiOutput("NumFolds"),
                             uiOutput("MLTS"),
                             uiOutput("Tunings"),
                             uiOutput("TuningTypes"),
                             uiOutput("TuneLength"),
                             actionButton("Go2", "Process"), width = 10)),
                                    uiOutput("RegTun"),
                                    uiOutput("DtTun"),
                                    uiOutput("RFTun"),
                                    uiOutput("XgbTun"),
                                    uiOutput("GbmTun"),
                                    uiOutput("SvmTun")),
                           verbatimTextOutput("Model")),
                  tabPanel("Prediction",
                           verbatimTextOutput("TestUploadRules"),
                           uiOutput("FileUpload2"),
                           uiOutput("PredictMLTS"),
                           actionButton("Go3", "Predict"),
                           dataTableOutput("PredictModel")))

  )
)


shinyApp(ui = ui, server = server)

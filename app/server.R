library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(caret)
library(e1071)
library(randomForest)
library(ROCR)
library(pROC)
library(shinyWidgets)
library(jsonlite)
library(xlsx)
# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive value to store the dataset
  data <- reactiveVal(NULL)
  
  # Load Dataset
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    output$ext <- renderText(ext)
    if (ext == "csv" || ext == "data" || ext == "dat" || ext == "tsv" || ext == "txt") {
      if (ext == "csv"){sep <- ","}
      else if (ext == "tsv"){sep <- "\t"}
      else {sep <- input$sep}
      df <- read.csv(input$file$datapath, header = input$header, sep = sep, quote = input$quote)
    } 
    else if (ext == "xls"|| ext == "xlsx"){
      df <- as.data.frame(read.xlsx(input$file$datapath, sheetIndex = 1))
    }
    else if (ext == "json"){
      df <- as.data.frame(fromJSON(txt=input$file$datapath))
    }
    else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    data(df)
    updatePickerInput(session, "target_var", choices = names(df))
  })
  
  # Data Preview
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Variable Diagnosis
  observeEvent(input$diagnose_btn, {
    req(data())
    df <- data()
    var_info <- data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      MissingValues = sapply(df, function(x) sum(is.na(x))),
      UniqueValues = sapply(df, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    # Identify if variable is unbalanced
    var_info$IsUnbalanced <- sapply(df, function(x) {
      if (is.numeric(x)) {
        FALSE
      } else {
        tbl <- table(x)
        max(tbl) / sum(tbl) > 0.9  # If one category represents more than 90%
      }
    })
    output$var_info <- renderDT({
      datatable(var_info, options = list(scrollX = TRUE))
    })
  })
  
  # Variable Selection UI
  output$var_select <- renderUI({
    req(data())
    selectInput("var1", "Select Variable:", choices = names(data()))
  })
  
  output$var_select_2 <- renderUI({
    req(data())
    selectInput("var2", "Select Second Variable (for bivariate analysis):", choices = names(data()))
  })
  
  # Univariate and Bivariate Analysis
  observeEvent(input$analyze_btn, {
    output$uni_plot <- renderPlotly({
      req(input$var1)
      df <- data()
      var <- df[[input$var1]]
      p <- if(is.numeric(var)) {
        ggplot(df, aes_string(x = input$var1)) + geom_histogram(binwidth = diff(range(var, na.rm=TRUE))/30, fill = "blue", color = "black")
      } else {
        ggplot(df, aes_string(x = input$var1)) + geom_bar(fill = "blue", color = "black")
      }
      ggplotly(p)
    })
    
    output$uni_summary <- renderPrint({
      req(input$var1)
      var <- data()[[input$var1]]
      if(is.numeric(var)) {
        summary(var)
      } else {
        as.data.frame(table(var))
      }
    })
    
    # Bivariate Analysis
    output$bi_plot <- renderPlotly({
      req(input$var1, input$var2)
      df <- data()
      var1 <- df[[input$var1]]
      var2 <- df[[input$var2]]
      p <- ggplot(df, aes_string(x = input$var1, y = input$var2)) +
        {if(is.numeric(var1) && is.numeric(var2)) geom_point(color = "blue")
         else if(is.factor(var1) && is.numeric(var2)) geom_boxplot(aes_string(group = input$var1), fill = "blue")
         else if(is.numeric(var1) && is.factor(var2)) geom_boxplot(aes_string(group = input$var2), fill = "blue")
         else geom_jitter(aes(color = var1), width = 0.2)}
      ggplotly(p)
    })
    
    output$bi_summary <- renderPrint({
      req(input$var1, input$var2)
      var1 <- data()[[input$var1]]
      var2 <- data()[[input$var2]]
      if(is.numeric(var1) && is.numeric(var2)) {
        cor.test(var1, var2)
      } else if(is.factor(var1) && is.factor(var2)) {
        chisq.test(table(var1, var2))
      } else {
        summary(lm(as.numeric(var2) ~ as.numeric(var1), data = data()))
      }
    })
  })
  
  # Preprocess Data
  preprocessed_data <- eventReactive(input$preprocess_btn, {
    req(data(), input$target_var)
    df <- data()
    
    # Handle missing values
    if (input$missing_values == "remove") {
      df <- na.omit(df)
    } else if (input$missing_values == "impute") {
      for(col in names(df)) {
        if(is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        } else {
          df[[col]][is.na(df[[col]])] <- as.character(names(sort(-table(df[[col]])))[1])
        }
      }
    } else if (input$missing_values == "zero") {
      for(col in names(df)) {
        if(is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- 0
        } else {
          df[[col]][is.na(df[[col]])] <- "Missing"
        }
      }
    }
    
    # Normalize
    if(input$normalize) {
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- scale(df[num_cols])
    }
    
    # Dummify
    if(input$dummify) {
      df <- data.frame(model.matrix(~ . -1, data = df))
    }
    
    # Balance Classes
    if(input$balance) {
      df <- upSample(x = df[ , names(df) != input$target_var], y = df[[input$target_var]])
      names(df)[ncol(df)] <- input$target_var
    }
    
    df
  })
  
  # Train Models
  observeEvent(input$train_btn, {
    req(preprocessed_data(), input$models)
    df <- preprocessed_data()
    target <- input$target_var
    models <- list()
    results <- data.frame(Model = character(), Accuracy = numeric(), Precision = numeric(),
                          Recall = numeric(), F1 = numeric(), AUC = numeric(), stringsAsFactors = FALSE)
    set.seed(123)
    trainIndex <- createDataPartition(df[[target]], p = .8, list = FALSE)
    trainData <- df[trainIndex, ]
    testData  <- df[-trainIndex, ]
    
    # Ensure target variable is factor
    trainData[[target]] <- as.factor(trainData[[target]])
    testData[[target]] <- as.factor(testData[[target]])
    
    # Train Selected Models
    for(model in input$models) {
      fit <- switch(model,
                    glm = train(as.formula(paste(target, "~ .")), data = trainData, method = "glm", family = "binomial"),
                    rf = train(as.formula(paste(target, "~ .")), data = trainData, method = "rf"),
                    svm = train(as.formula(paste(target, "~ .")), data = trainData, method = "svmRadial"))
      models[[model]] <- fit
      preds <- predict(fit, testData)
      if ("prob" %in% fit$control$classProbs) {
        probs <- predict(fit, testData, type = "prob")
        prob_positive <- probs[,2]
      } else {
        prob_positive <- as.numeric(preds)
      }
      cm <- confusionMatrix(preds, testData[[target]])
      roc_obj <- roc(as.numeric(testData[[target]]), prob_positive)
      results <- rbind(results, data.frame(
        Model = model,
        Accuracy = cm$overall['Accuracy'],
        Precision = cm$byClass['Precision'],
        Recall = cm$byClass['Recall'],
        F1 = cm$byClass['F1'],
        AUC = roc_obj$auc
      ))
    }
    
    # Display Results
    output$model_results <- renderDT({
      datatable(results)
    })
    
    # ROC Plot
    output$roc_plot <- renderPlotly({
      roc_data <- data.frame()
      for(model in input$models) {
        fit <- models[[model]]
        if ("prob" %in% fit$control$classProbs) {
          probs <- predict(fit, testData, type = "prob")
          prob_positive <- probs[,2]
        } else {
          prob_positive <- as.numeric(predict(fit, testData))
        }
        pred <- prediction(prob_positive, as.numeric(testData[[target]]))
        perf <- performance(pred, "tpr", "fpr")
        roc_df <- data.frame(FPR = perf@x.values[[1]], TPR = perf@y.values[[1]], Model = model)
        roc_data <- rbind(roc_data, roc_df)
      }
      p <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) + geom_line() + geom_abline(linetype = "dashed") + theme_minimal()
      ggplotly(p)
    })
    
    # Feature Importance
    output$feature_importance <- renderPrint({
      if("rf" %in% input$models) {
        fit <- models[["rf"]]
        importance <- varImp(fit)
        print(importance)
      } else {
        print("Feature importance is available for Random Forest model only.")
      }
    })
    
  })
  
}

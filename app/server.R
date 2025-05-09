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
library(rJava)
library(randomcoloR)
library(rmarkdown)
library(nnet)
library(MLmetrics)

# Define Server Logic
server <- function(input, output, session) {
  
    report_content <- reactive({
      report_file <- "./../report_original.Rmd"
      html_report <- rmarkdown::render(report_file, output_format = "html_document", quiet = TRUE)
      return(html_report)  
    })
    
    output$report_preview <- renderUI({
      report_file <- report_content()
      req(report_file)   
      HTML(readLines(report_file))
      
    })
  
  
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
    data_type <- sapply(df, function(x) class(x)[1])
    Type <- c()
    i <- 1
    for (x in data_type){
      if (x == "numeric") Type[i] <- "Quantitative" 
      else Type[i] <- "Categorical"
      i <- i + 1
    }
    
    
    
    var_info <- data.frame(
      Variable = names(df),
      Type = Type,
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
    output$dist <- renderPlotly({
      req(input$var1)
      df <- data()
      var <- df[[input$var1]]
      p <- if(is.numeric(var)) {
        ggplot(df, aes_string(x = input$var1)) + geom_histogram(binwidth = diff(range(var, na.rm=TRUE))/30, fill = randomColor(), color = "black")
      } else {
        ggplot(df, aes_string(x = input$var1)) + geom_bar(fill = randomColor(), color = "black")
      }
      ggplotly(p)
    })
    
    output$cum_dist <- renderPlotly({
      req(input$var1)
      df <- data()
      var <- df[[input$var1]]
      
      ecdf_function <- ecdf(var)
      ecdf_data <- data.frame(x = sort(var), y = ecdf_function(sort(var)))
      
      ggplot(ecdf_data, aes(x = x, y = y)) +
        geom_step(color = randomColor()) +
        geom_point() +
        labs(title = "Empirical Cumulative Distribution Function",
             x = "Data", y = "ECDF") +
        theme_minimal()
      
    })
    
    output$stat_table <- renderTable({
      req(input$var1)
      df <- data()
      var <- df[[input$var1]]
      
      if (is.numeric(var)){
        hist_result <- hist(var, plot = FALSE)
        data.frame(
          Centers = as.character(hist_result$mids),
          Freq = hist_result$counts,
          Rel_Freq = hist_result$counts / sum(hist_result$counts),
          Cum_Freq = cumsum(hist_result$counts),
          Cum_Rel_Freq = cumsum(hist_result$counts) / sum(hist_result$counts)
        )
      }
      else {
        freq_table <- table(var)
        data.frame(
          Modality = as.numeric(names(freq_table)),
          Frequency = as.vector(freq_table),
          Relative_Frequency = prop.table(freq_table)$Freq,
          Cumulative_Frequency = cumsum(freq_table),
          Cumulative_Relative_Frequency = cumsum(prop.table(freq_table))
        )
        
      }
    })
    
    output$box_plot <- renderPlotly({
      req(input$var1)
      df <- data()
      var <- df[[input$var1]]
      if(is.numeric(var)){
        p <- ggplot(df, aes(y=var)) +
          geom_boxplot(fill=randomColor(), color="black")+
          theme_classic()
        ggplotly(p)
      }
      
    })
    
    output$uni_summary <- renderTable({
      req(input$var1)
      v <- data()[[input$var1]]
      if(is.numeric(v)) {
        var_summary <- summary(v)
        var_names <- names(var_summary)
        t(data.frame(Statistics = c(var_names, "var", "std"), Value = round(c(as.numeric(var_summary), var(v), sd(v)), digits = 2)))
      } else {
        t(as.data.frame(table(v)))
      }
    })
    
    
    output$outliers <- renderTable({
      req(input$var1)
      df <- data()
      var <- data()[[input$var1]]
      var_name <- input$var1
      if(is.numeric(var)) {
        res<-quantile(var, probs = c(0.25,0.75))
        ecart <- res["75%"] - res["25%"]
        subset(df, df[[var_name]] < res["25%"] - 1.5*ecart | df[[var_name]] > res["75%"] + 1.5*ecart)
      }
    })
    
    # Bivariate Analysis
    output$bi_plot <- renderPlotly({
      req(input$var1, input$var2)
      df <- data()
      var1 <- df[[input$var1]]
      var2 <- df[[input$var2]]
      p <- ggplot(df, aes_string(x = input$var1, y = input$var2)) +
        {if(is.numeric(var1) && is.numeric(var2)) geom_point(color = randomColor())
          else if(is.factor(var1) && is.numeric(var2)) geom_boxplot(aes_string(group = input$var1), fill = randomColor())
          else if(is.numeric(var1) && is.factor(var2)) geom_boxplot(aes_string(group = input$var2), fill = randomColor())
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
  preprocessed_data <- reactiveVal(NULL)
  observeEvent(input$preprocess_btn, {
    
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
    updatePickerInput(session, "ref_class", choices = unique(df[[input$target_var]]))
    #print(input$target_var)
    preprocessed_data(df)
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
    glm_tune_grid <- expand.grid(decay = input$decay)
    rf_tune_grid <- expand.grid(mtry = length(colnames(df)) - 1)
    svm_tune_grid <- expand.grid(C = input$C, sigma = input$sigma)
    
    trControl = trainControl(method = "cv", number = 10)
    for(model in input$models) {
      fit <- switch(model,
                    glm = train(as.formula(paste(target, "~ .")), data = trainData, tuneGrid = glm_tune_grid, 
                                trControl = trControl, method = "multinom", maxit = input$maxit),
                    rf = train(as.formula(paste(target, "~ .")), data = trainData, tuneGrid = rf_tune_grid, 
                               trControl = trControl, method = "rf", 
                               ntree = input$ntree, 
                               nodesize = input$nodesize, 
                               maxnodes = input$maxnodes,
                               replace = input$replace),
                    svm = train(as.formula(paste(target, "~ .")), data = trainData, tuneGrid = svm_tune_grid, 
                                trControl = trControl, method = "svmRadial"))
      models[[model]] <- fit
      preds <- predict(fit, testData)
      if ("prob" %in% fit$control$classProbs) {
        probs <- predict(fit, testData, type = "prob")
        prob_positive <- probs[,2]
      } else {
        prob_positive <- as.numeric(preds)
      }
      cm <- confusionMatrix(preds, testData[[target]])
      conf_matrix <- renderTable({
        if (!is.null(cm)) {
          cm_matrix <- cm$table
          actual <- rownames(cm_matrix)
          predicted <- colnames(cm_matrix)
          
          matrix_output <- matrix("", nrow = length(actual) + 1, ncol = length(predicted) + 1)
          matrix_output[1, ] <- c("Actual/Predicted", colnames(cm_matrix))
          matrix_output[, 1] <- c("Actual/Predicted", rownames(cm_matrix))
          
          cm_table <- as.data.frame(cm$table)
          for (i in 1:length(actual)){
            for (j in 1:length(predicted)){
              matrix_output[i + 1, j + 1] <- cm_table[cm_table$Reference == matrix_output[i + 1, 1] & cm_table$Prediction == matrix_output[1, j + 1], 3]
            }
          }
          matrix_output
        }
      }, rownames = TRUE, colnames = TRUE)
      
      if (model == "glm") {output$glm_conf_matrix = conf_matrix}
      else if (model == "rf") {output$rf_conf_matrix = conf_matrix}
      else if (model == "svm") {output$svm_conf_matrix = conf_matrix}
      
      acc <- renderValueBox({valueBox(
        format(cm$overall['Accuracy'], digits = 4),
        "Accuracy",
        icon = icon("chart-line"),
        color = "aqua"
      )})
      
      if (model == "glm") {output$glm_acc = acc}
      else if (model == "rf") {output$rf_acc = acc}
      else if (model == "svm") {output$svm_acc = acc}
      #print(cm$byClass)
      byclass_results <- renderTable(data.frame(Class = rownames(cm$table), cm$byClass))
      
      if (model == "glm") {output$glm_byclass_results = byclass_results}
      else if (model == "rf") {output$rf_byclass_results = byclass_results}
      else if (model == "svm") {output$svm_byclass_results = byclass_results}
      conf_matrix <- cm
      
      precision_macro <- mean(conf_matrix$byClass[, "Precision"])
      recall_macro <- mean(conf_matrix$byClass[, "Recall"])
      f1_macro <- mean(conf_matrix$byClass[, "F1"])
      
      metrics <- c("precision", "recall", "F1")
      macro_avg <- renderTable({
        data.frame(Metrics = metrics, Values = c(precision_macro, recall_macro, f1_macro))
      })
      if (model == "glm") {output$glm_macro_avg = macro_avg}
      else if (model == "rf") {output$rf_macro_avg = macro_avg}
      else if (model == "svm") {output$svm_macro_avg = macro_avg}
      
      support <- conf_matrix$table
      weights <- support / sum(support)
      
      precision_weighted_macro <- sum(weights * conf_matrix$byClass[, "Precision"])
      recall_weighted_macro <- sum(weights * conf_matrix$byClass[, "Recall"])
      f1_weighted_macro <- sum(weights * conf_matrix$byClass[, "F1"])
      
      weighted_macro_avg <- renderTable({
        data.frame(Metrics = metrics, Values = c(precision_weighted_macro, recall_weighted_macro, f1_weighted_macro))
      })
      
      if (model == "glm") {output$glm_weighted_macro_avg = weighted_macro_avg}
      else if (model == "rf") {output$rf_weighted_macro_avg = weighted_macro_avg}
      else if (model == "svm") {output$svm_weighted_macro_avg = weighted_macro_avg}
      
      
      conf_matrix_all <- conf_matrix$table
      true_positives <- sum(diag(conf_matrix_all))  
      false_positives <- sum(conf_matrix_all) - true_positives  
      false_negatives <- sum(conf_matrix_all) - true_positives
      
      precision_micro <- true_positives / (true_positives + false_positives)
      recall_micro <- true_positives / (true_positives + false_negatives)
      f1_micro <- 2 * (precision_micro * recall_micro) / (precision_micro + recall_micro)
      
      micro_avg <- renderTable({
        data.frame(Metrics = metrics, Values = c(precision_micro, recall_micro, f1_micro))
      })
      
      if (model == "glm") {output$glm_micro_avg = micro_avg}
      else if (model == "rf") {output$rf_micro_avg = micro_avg}
      else if (model == "svm") {output$svm_micro_avg = micro_avg}
    }
    
    # ROC Plot
    
    output$roc_plot <- renderPlotly({
      req(input$target_var, input$ref_class, data())
      df <- data()
      target_classes <- unique(df[[input$target_var]])
      index <- which(target_classes == input$ref_class)
      
      
      roc_data <- data.frame()
      for(model in input$models) {
        fit <- models[[model]]

        prob_positive <- as.numeric(predict(fit, testData))
        prob_positive <- ifelse(prob_positive != index, 0, prob_positive)
        #print(prob_positive)
        targ <- as.numeric(testData[[target]])
        print(targ)
        targ <- ifelse(targ != index, 0, targ)
        pred <- prediction(prob_positive, targ)
        
        #print(str(performance(pred, measure = "auc")))
        
        
        AUC <- renderValueBox({valueBox(
          format(performance(pred, measure = "auc")@y.values[[1]], digits = 4),
          "AUC",
          icon = icon("chart-line"),
          color = "aqua"
        )})
      
        if (model == "glm") {output$glm_AUC = AUC}
        else if (model == "rf") {output$rf_AUC = AUC}
        else if (model == "svm") {output$svm_AUC = AUC}
        
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
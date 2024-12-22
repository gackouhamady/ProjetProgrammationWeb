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
library(recipes)
library(themis)
library(DMwR2)
library(UBL)
library(kernlab)
library(data.table)
library(plyr)





# Define Server Logic
server <- function(input, output, session) {
  
  
  
  # Report file render
  
  report_content <- reactive({
    report_file <- "./report_original.Rmd"
    
    if (file.exists(report_file)) { 
      html_report <- rmarkdown::render(report_file, output_format = "html_document", quiet = TRUE)
      return(html_report)  
    } else {
      return("Le fichier rapport.Rmd est introuvable.")
    }
  })
  
  output$report_preview_1 <- renderUI({
    report_file <- report_content()
    req(report_file)   
    
    if (file.exists(report_file)) {
      HTML(readLines(report_file))
    } else {
      "Erreur dans le rendu du fichier."
    }
  })
  
  
  
  
 # Reactive value to store the dataset
  
  
  original_data <- reactiveVal(NULL)
  data <- reactiveVal(NULL)
  
  observeEvent(input$load_btn, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    output$ext <- renderText(ext)
    if (ext == "csv" || ext == "data" || ext == "dat" || ext == "tsv" || ext == "txt") {
      if (ext == "csv"){sep <- ","}
      else if (ext == "tsv"){sep <- "\t"}
      else {sep <- input$sep}
      df <- read.csv(input$file$datapath, header = input$header, sep = sep, quote = input$quote)
    } 
    else if (ext == "xls" || ext == "xlsx") {
      df <- as.data.frame(read.xlsx(input$file$datapath, sheetIndex = 1))
    }
    else if (ext == "json") {
      df <- as.data.frame(fromJSON(txt = input$file$datapath))
    }
    else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    original_data(df)
    data(df)
    updatePickerInput(session, "target_var", choices = names(df))
  })
  
  
  # Data Preview
 
  
   output$data_preview <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE, pageLength = 5))
  })
  
 
  # Data Summary
  
   
   output$data_summary <- renderPrint({
    req(data())
    summary_data <- summary(data())  # Génère un résumé statistique des données
    print(summary_data)  # Affiche le résumé statistique sous forme de texte brut
  })
  
  
  # Data Table render function  for  Data Exploratory  analysis
 
   
    output$data_table <- renderDT({
    req(data())  # Assurez-vous que les données sont disponibles
    datatable(data(), options = list(scrollX = TRUE, pageLength = 10))  # Affiche les données sous forme de table interactive
  })
    
    
  
    # Correlation Matrix of Numeric Attributes  
  
  
    output$correlation <- renderPlotly({
    req(data())
    df <- data()
    names(df) <- gsub(" ", "_", names(df))
    names(df) <- iconv(names(df), from = "UTF-8", to = "ASCII//TRANSLIT")
    names(df) <- make.names(names(df))
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_cols) > 1) {
      corr_matrix <- cor(df[numeric_cols], use = "complete.obs", method = "pearson")
      
      corr_melt <- melt(corr_matrix)
      
      heatmap_plot <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
        labs(title = "Heatmap des Corrélations", x = "", y = "") +
        theme_minimal()
      
      ggplotly(heatmap_plot)
    } else {
      plot_ly(type = "scatter", mode = "text", text = "Pas assez de variables numériques pour calculer les corrélations.")
    }
  })
  
  
  
  


    
    # 2D Visualtion 
  
   
    output$var1 <- renderUI({
    req(data())  # Attendre que les données soient chargées
    selectInput("var1", "Select First Variable", choices = names(data()))
  })
  
  
  output$var2 <- renderUI({
    req(data())  # Attendre que les données soient chargées
    selectInput("var2", "Select Second Variable", choices = names(data()))
  })
  
 
    
  # Analyse bivariate event delclencher  
  
   observeEvent(input$analyze_button, {
    req(input$var1, input$var2)
    var1 <- input$var1
    var2 <- input$var2
    df <- data()
    
    df <- df %>%
      mutate(across(everything(), ~ {
        if (is.character(.) && length(unique(.)) < 10) {
          as.factor(.) 
        } else {
          . 
        }
      }))
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    factor_cols <- names(df)[sapply(df, is.factor)]
    
    if (var1 %in% numeric_cols && var2 %in% numeric_cols ) {
      scatter_plot <- ggplot(df, aes_string(x = var1, y = var2)) +
        geom_point(color = "blue", alpha = 0.7) +
        labs(title = paste("Scatter plot between", var1, "and", var2)) +
        theme_minimal()
      output$two_d_visualization <- renderPlotly({ ggplotly(scatter_plot) })
    } else if (var1 %in% numeric_cols && var2 %in% factor_cols  | var2 %in% numeric_cols && var1 %in% factor_cols ) {
      box_plot <- ggplot(df, aes_string(x = var2, y = var1)) +
        geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
        labs(title = paste("Boxplot between", var2, "and", var1)) +
        theme_minimal()
      output$two_d_visualization <- renderPlotly({ ggplotly(box_plot) })
    } else {
      output$two_d_visualization <- renderPlotly({
        plot_ly(type = "scatter", mode = "text", text = "Invalid selection. Please choose appropriate variables.")
      })
    }
  })
  
  
# 1D Visualisation 
  
  output$var1_1d <- renderUI({
    req(data())
    selectInput("var1_1d", "Select Variable", choices = names(data()))
  })
  
  
observeEvent(input$analyze_univariate_button, {
    req(input$var1_1d)  # Vérifier qu'une variable est sélectionnée
    var <- input$var1_1d
    df <- data()
    
    # Transformation des variables
    df <- df %>%
      mutate(across(everything(), ~ {
        if (is.character(.) && length(unique(.)) < 10) {
          as.factor(.)
        } else {
          .
        }
      }))
    
  # Identifier le type de la variable sélectionnée
    if (is.numeric(df[[var]])) {
      # Histogramme pour les variables numériques
      hist_plot <- ggplot(df, aes_string(x = var)) +
        geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme_minimal()
      output$one_d_visualization <- renderPlotly({ ggplotly(hist_plot) })
      
    } else if (is.factor(df[[var]])) {
      # Barplot pour les variables catégorielles
      bar_plot <- ggplot(df, aes_string(x = var)) +
        geom_bar(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = paste("Bar plot of", var), x = var, y = "Frequency") +
        theme_minimal()
      output$one_d_visualization <- renderPlotly({ ggplotly(bar_plot) })
      
    } else {
      # Message d'erreur pour les types non supportés
      output$one_d_visualization <- renderPlotly({
        plot_ly(type = "scatter", mode = "text", text = "Invalid selection. Please choose an appropriate variable.")
      })
    }
  })
  



  

# Data Transformation 
# Observateur pour traiter les valeurs manquantes
observeEvent(input$apply_missing_values, {
  req(input$missing_method)
  df <- data()
  
  # Vérification des données disponibles
  if (is.null(df)) {
    output$missing_values_status <- renderText("No data available to process.")
    return()
  }
  
  if (sum(is.na(df)) == 0) {
    output$missing_values_status <- renderText("No missing values detected.")
  } else {
    if (input$missing_method == "remove") {
      df <- na.omit(df)
    } else if (input$missing_method == "mean_mode") {
      df <- df %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
        mutate(across(where(is.factor), ~ ifelse(is.na(.), Mode(.), .)))
    } else if (input$missing_method == "zero") {
      df <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    }
    
    data(df) # Mise à jour des données
    output$missing_values_status <- renderText("Missing values handled successfully!")
  }
})

Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Observateur pour appliquer la normalisation
observeEvent(input$apply_normalization, {
  df <- data()
  
  # Vérification des données disponibles
  if (is.null(df)) {
    output$normalization_status <- renderText("No data available to process.")
    return()
  }
  
  if (input$normalize_method == "z_score") {
    df <- normalize_data_zscore(df)
    output$normalization_status <- renderText("Data normalized using Z-score scaling.")
  } else if (input$normalize_method == "min_max") {
    df <- normalize_data_minmax(df)
    output$normalization_status <- renderText("Data normalized using Min-Max scaling.")
  }
  
  data(df) # Mise à jour des données
})

normalize_data_zscore <- function(df) {
  numeric_columns <- sapply(df, is.numeric)
  if (any(numeric_columns)) {
    df[, numeric_columns] <- scale(df[, numeric_columns, drop = FALSE])
  }
  return(df)
}

normalize_data_minmax <- function(df) {
  numeric_columns <- sapply(df, is.numeric)
  if (any(numeric_columns)) {
    df[, numeric_columns] <- lapply(df[, numeric_columns, drop = FALSE], function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    })
  }
  return(df)
}

# Observateur pour transformer les variables catégorielles
observeEvent(input$apply_categorical_transformation, {
  df <- data()
  
  # Vérification des données disponibles
  if (is.null(df)) {
    output$categorical_transformation_status <- renderText("No data available to process.")
    return()
  }
  
  df <- transform_to_categorical(df)
  data(df) # Mise à jour des données
  output$categorical_transformation_status <- renderText("Categorical transformation applied successfully!")
})

transform_to_categorical <- function(df) {
  df <- df %>%
    mutate(across(everything(), ~ {
      if (is.character(.) && length(unique(.)) < 10) {
        as.factor(.)
      } else {
        .
      }
    }))
  return(df)
}

# Observateur pour appliquer la dummification
observeEvent(input$apply_dummification, {
  df <- data()
  
  # Vérification des données disponibles
  if (is.null(df)) {
    output$dummification_status <- renderText("No data available to process.")
    return()
  }
  
  if (sum(sapply(df, is.factor)) == 0) {
    output$dummification_status <- renderText("No categorical variables to dummify.")
  } else {
    df <- dummy_variables(df)
    data(df) # Mise à jour des données
    output$dummification_status <- renderText("Dummification applied successfully!")
  }
})

dummy_variables <- function(df) {
  df <- as.data.frame(df)
  
  names(df) <- gsub(" ", "_", names(df)) 
  names(df) <- iconv(names(df), from = "UTF-8", to = "ASCII//TRANSLIT")
  names(df) <- make.names(names(df)) 
  
  factor_columns <- sapply(df, is.factor)
  
  if (any(factor_columns)) {
    dummy_model <- dummyVars("~ .", data = df)
    dummy_data <- predict(dummy_model, newdata = df)
    dummy_data <- as.data.frame(dummy_data)
    df <- cbind(df[, !factor_columns], dummy_data)
  }
  return(df)
}

# Observateur pour identifier les outliers
observeEvent(input$outliers_btn, {
  df <- data()
  
  # Vérification des données disponibles
  if (is.null(df)) {
    output$outliers_status <- renderText("No data available to process.")
    return()
  }
  
  # Essayer d'identifier les outliers, tout en gérant les exceptions
  tryCatch({
    outliers <- identify_outliers(df)
    
    if (nrow(outliers) == 0) {
      output$outliers_status <- renderText("No outliers detected.")
    } else {
      output$outliers_status <- renderText("Outliers identified successfully!")
      output$outliers_table <- renderDT({
        datatable(outliers)
      })
    }
  }, error = function(e) {
    # Gestion de l'erreur
    output$outliers_status <- renderText(paste("Error while identifying outliers:", e$message))
  })
})

# Fonction pour identifier les outliers
identify_outliers <- function(data) {
  outliers <- list()
  
  for (i in which(sapply(data, is.numeric))) {
    perc <- quantile(data[, i], c(.25, .75 ), na.rm = TRUE)
    lower_fence <- perc[1] - 1.5 * IQR(data[, i])
    upper_fence <- perc[2] + 1.5 * IQR(data[, i])
    
    outlier_indices <- which(data[, i] < lower_fence | data[, i] > upper_fence)
    if(length(outlier_indices) > 0) {
      outliers[[names(data)[i]]] <- data[outlier_indices, i, drop = FALSE]
    }
  }
  
  # Convertir la liste en data frame tout en gérant les données non uniformes
  outliers_df <- do.call(rbind.fill, lapply(outliers, function(x) data.frame(x)))
  rownames(outliers_df) <- NULL
  return(outliers_df)
}

# Observateur pour gérer les outliers
observeEvent(input$handle_outliers_btn, {
  df <- data()
  
  # Vérification des données disponibles
  if (is.null(df)) {
    output$handled_outliers_status <- renderText("No data available to process.")
    return()
  }
  
  # Essayer de gérer les outliers tout en gérant les exceptions
  tryCatch({
    df <- handle_outliers(df)
    data(df) # Mise à jour des données
    output$handled_outliers_status <- renderText("Outliers handled successfully!")
  }, error = function(e) {
    output$handled_outliers_status <- renderText(paste("Error while handling outliers:", e$message))
  })
})

# Fonction pour gérer les outliers
handle_outliers <- function(df) {
  for (i in which(sapply(df, is.numeric))) {
    perc <- quantile(df[, i], c(.25, .75 ), na.rm = TRUE)
    lower_fence <- perc[1] - 1.5 * IQR(df[, i])
    upper_fence <- perc[2] + 1.5 * IQR(df[, i])
    
    # Remplacer les outliers par les bornes
    df[, i] <- ifelse(df[, i] < lower_fence, lower_fence, df[, i])
    df[, i] <- ifelse(df[, i] > upper_fence, upper_fence, df[, i])
  }
  
  return(df)
}














 



  
# Update Select Input for data
   
   observeEvent(data(), {
      req(data(), original_data())
      tryCatch({
        if (is.null(data())) return()
        if (is.null(original_data())) return()
        
        char_or_num_cols_2levels <- sapply(original_data(), function(col) {
          if (is.character(col) || is.numeric(col)) {
            length(unique(col)) == 2
          } else if (is.factor(col)) {
            length(levels(col)) == 2
          } else {
            FALSE
          }
        })
        
        target_var <- input$target_var
        
        if (!is.null(target_var) && target_var %in% names(original_data())) {
          if (is.numeric(original_data()[[target_var]])) {
            original_data()[[target_var]] <- as.numeric(original_data()[[target_var]])
          } else if (is.factor(original_data()[[target_var]])) {
            if (length(levels(original_data()[[target_var]])) == 2) {
              original_data()[[target_var]] <- as.factor(original_data()[[target_var]])
            } else {
              levels_to_use <- levels(original_data()[[target_var]])[1:2]
              original_data()[[target_var]] <- factor(ifelse(original_data()[[target_var]] %in% levels_to_use, 
                                                             levels_to_use[1], levels_to_use[2]))
            }
          } else if (is.character(original_data()[[target_var]])) {
            if (length(unique(original_data()[[target_var]])) == 2) {
              original_data()[[target_var]] <- as.factor(original_data()[[target_var]])
            } else {
              unique_values <- unique(original_data()[[target_var]])
              original_data()[[target_var]] <- factor(ifelse(original_data()[[target_var]] %in% unique_values[1:2], 
                                                             unique_values[1], unique_values[2]))
            }
          }
        }
        
        char_or_num_cols_2levels_target_variable <- sapply(original_data(), function(col) {
          if (is.character(col) || is.numeric(col)) {
            length(unique(col)) == 2
          } else if (is.factor(col)) {
            length(levels(col)) == 2
          } else {
            FALSE
          }
        })
        
        target_variable <- input$target_variable
        
        if (!is.null(target_variable) && target_variable %in% names(original_data())) {
          if (is.numeric(original_data()[[target_variable]])) {
            original_data()[[target_variable]] <- as.numeric(original_data()[[target_variable]])
          } else if (is.factor(original_data()[[target_variable]])) {
            if (length(levels(original_data()[[target_variable]])) == 2) {
              original_data()[[target_variable]] <- as.factor(original_data()[[target_variable]])
            } else {
              levels_to_use <- levels(original_data()[[target_variable]])[1:2]
              original_data()[[target_variable]] <- factor(ifelse(original_data()[[target_variable]] %in% levels_to_use, 
                                                                  levels_to_use[1], levels_to_use[2]))
            }
          } else if (is.character(original_data()[[target_variable]])) {
            if (length(unique(original_data()[[target_variable]])) == 2) {
              original_data()[[target_variable]] <- as.factor(original_data()[[target_variable]])
            } else {
              unique_values <- unique(original_data()[[target_variable]])
              original_data()[[target_variable]] <- factor(ifelse(original_data()[[target_variable]] %in% unique_values[1:2], 
                                                                  unique_values[1], unique_values[2]))
            }
          }
        }
        
        updateSelectInput(session, "var1", choices = names(data()))
        updateSelectInput(session, "var2", choices = names(data()))
        updateSelectInput(session, "var1_1d", choices = names(data()))
        
        if (any(char_or_num_cols_2levels)) {
          updateSelectInput(session, "target_var", choices = names(original_data())[char_or_num_cols_2levels])
        } else {
          updateSelectInput(session, "target_var", choices = "Bad DataFrame")
          shinyjs::runjs('$("#target_var").css("color", "red");')
        }
        
        if (any(char_or_num_cols_2levels_target_variable)) {
          updateSelectInput(session, "target_variable", choices = names(original_data())[char_or_num_cols_2levels_target_variable])
        } else {
          updateSelectInput(session, "target_variable", choices = "Bad DataFrame")
          shinyjs::runjs('$("#target_variable").css("color", "red");')
        }
        
      }, error = function(e) {
        cat("An error occurred: ", e$message, "\n")
      })
    })
    
    
  
    
# Functions for Data Level Methods
    
    random_undersampling <- function(df, target_variable) {
      df <- df %>%
        mutate(dependent = as.factor(df[[target_variable]]))
      
      recipe_obj <- recipe(dependent ~ ., data = df) %>%
        step_center(all_numeric_predictors()) %>%
        step_normalize(all_numeric_predictors()) %>%
        step_downsample(dependent)
      
      df_balanced <- recipe_obj %>%
        prep(training = df) %>%
        bake(new_data = NULL)
      
      df_balanced <- df_balanced %>%
        select(-dependent)
      
      return(df_balanced)
    }
    
    random_oversampling <- function(df, target_variable) {
      df <- df %>%
        mutate(dependent = as.factor(df[[target_variable]]))
      
      recipe_obj <- recipe(dependent ~ ., data = df) %>%
        step_center(all_numeric_predictors()) %>%
        step_normalize(all_numeric_predictors()) %>%
        step_upsample(dependent)
      
      df_balanced <- recipe_obj %>%
        prep(training = df) %>%
        bake(new_data = NULL)
      
      df_balanced <- df_balanced %>%
        select(-dependent)
      
      return(df_balanced)
    }
    
    
    # Functions for Algorithm Level Methods
    cost_sensitive_learning <- function(df, target_variable) {
      class_weights <- ifelse(df[[target_variable]] == "minority_class", 2, 1)
      df <- df %>%
        mutate(weights = class_weights)
      return(df)
    }
    
    
    one_class_learning <- function(df, target_variable) {
      # Apply One-Class Learning (you can implement this with One-Class SVM or Isolation Forest)
      model <- svm(df[[target_variable]] ~ ., data = df, type = "one-classification")
      return(model)
    }
  
  
    
    
    
    
#Train the model

    observeEvent(input$train_model, {
      req(input$target_var)
      
      output$training_status <- renderUI({
        tagList(
          h3("Please be patient, model training may take some time...")
        )
      })
      
      
      tryCatch({
        Sys.sleep(10) 
        df <- original_data()
        
        target_var <- input$target_var
        target <- df[[target_var]]
        df <- df[, !(names(df) == target_var)]
        
        
        
        # Handle Missing Values
        df <- df %>%
          mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
          mutate(across(where(is.factor), ~ ifelse(is.na(.), Mode(.), .)))
        
        # Handle outliers
        for (i in which(sapply(df, is.numeric))) {
          perc <- quantile(df[[i]], c(0.25, 0.75), na.rm = TRUE)
          IQR_val <- IQR(df[[i]])
          lower <- perc[1] - 1.5 * IQR_val
          upper <- perc[2] + 1.5 * IQR_val
          df[[i]] <- pmax(pmin(df[[i]], upper), lower)
        }
        
        # Normalization
        df <- df %>%
          mutate(across(where(is.numeric), ~ scale(.) %>% as.vector())) 
        
        # Dummifia=cation
        df <- dummy_variables(df)
        
        df[[target_var]] <- target
        
        original_data(df)
        
        # Handle Imbalance
        imbalance_method <- input$imbalance_method
        if (imbalance_method == "Random Over-sampling") {
          df <- random_oversampling(df, target_var)
        } else if (imbalance_method == "Random Under-sampling") {
          df <- random_undersampling(df, target_var)
        } else if (imbalance_method == "Cost-Sensitive Learning") {
          df <- cost_sensitive_learning(df, target_var)
        } else if (imbalance_method == "One-Class Learning") {
          df <- one_class_learning(df, target_var)
        }
        
        original_data(df)
        
        set.seed(123)
        train_index <- createDataPartition(df[[target_var]], p = 0.8, list = FALSE)
        train_data <- df[train_index, ]
        test_data <- df[-train_index, ]
        
        if (is.numeric(train_data[[target_var]])) {
          train_data[[target_var]] <- as.numeric(train_data[[target_var]])
          test_data[[target_var]] <- as.numeric(test_data[[target_var]])
        } else if (is.factor(train_data[[target_var]])) {
          if (length(levels(train_data[[target_var]])) == 2) {
            train_data[[target_var]] <- as.factor(train_data[[target_var]])
            test_data[[target_var]] <- as.factor(test_data[[target_var]])
          } else {
            levels_to_use <- levels(train_data[[target_var]])[1:2]
            train_data[[target_var]] <- factor(ifelse(train_data[[target_var]] %in% levels_to_use, 
                                                      levels_to_use[1], levels_to_use[2]))
            test_data[[target_var]] <- factor(ifelse(test_data[[target_var]] %in% levels_to_use, 
                                                     levels_to_use[1], levels_to_use[2]))
          }
        } else if (is.character(train_data[[target_var]])) {
          if (length(unique(train_data[[target_var]])) == 2) {
            train_data[[target_var]] <- as.factor(train_data[[target_var]])
            test_data[[target_var]] <- as.factor(test_data[[target_var]])
          } else {
            unique_values <- unique(train_data[[target_var]])
            train_data[[target_var]] <- factor(ifelse(train_data[[target_var]] %in% unique_values[1:2], 
                                                      unique_values[1], unique_values[2]))
            test_data[[target_var]] <- factor(ifelse(test_data[[target_var]] %in% unique_values[1:2], 
                                                     unique_values[1], unique_values[2]))
          }
        } else {
          stop("Type de la variable cible non pris en charge")
        }
        
        train_data[[target_var]] <- as.numeric(train_data[[target_var]]) - 1
        test_data[[target_var]] <- as.numeric(test_data[[target_var]]) - 1
        
        train_data[[target_var]] <- as.factor(train_data[[target_var]])
        test_data[[target_var]] <- as.factor(test_data[[target_var]])
        
        print(names(train_data))
        
        if (!(target_var %in% names(train_data))) {
          stop("La variable cible spécifiée n'existe pas dans le jeu de données.")
        }
        
        print(as.formula(paste(target_var, "~ .")))
        
        model <- switch(
          input$model_select,
          "Random Forest" = train(
            as.formula(paste(target_var, "~ .")), data = train_data, method = "rf",
            trControl = trainControl(method = "cv", number = 5),
            tuneGrid = expand.grid(mtry = 2),
            ntree = 50,
            maxnodes = 10
          ),
          "Decision Trees" =  train(
            as.formula(paste(target_var, "~ .")), 
            data = train_data, 
            method = "rpart",  # Méthode de l'arbre de décision
            trControl = trainControl(method = "cv", number = 5)  # Validation croisée avec 5 plis
          ),
          "SVM" = train(
            as.formula(paste(target_var, "~ .")), data = train_data, method = "svmRadial",
            trControl = trainControl(method = "cv", number = 5),
            tuneGrid = expand.grid(sigma = 0.05, C = 1)
          ),
          "Logistic Regression" = train(
            as.formula(paste(target_var, "~ .")), data = train_data, method = "glm", family = "binomial",
            trControl = trainControl(method = "cv", number = 5),
            control = glm.control(maxit = 25)
          ),
          "KNN" = train(
            as.formula(paste(target_var, "~ .")), data = train_data, method = "knn",
            trControl = trainControl(method = "cv", number = 5),
            tuneGrid = expand.grid(k = 3)
          )
        )
        
        predictions <- predict(model, test_data)
        cm <- confusionMatrix(predictions, test_data[[target_var]])
        accuracy <- cm$overall["Accuracy"]
        precision <- cm$byClass["Pos Pred Value"]
        recall <- cm$byClass["Sensitivity"]
        f1_score <- 2 * ((precision * recall) / (precision + recall))
        roc_curve <- roc(as.numeric(test_data[[target_var]]), as.numeric(predictions))
        auc_value <- auc(roc_curve)
        
        model_results <- data.frame(
          Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC"),
          Value = c(accuracy, precision, recall, f1_score, auc_value)
        )
        
        output$model_results <- renderDT({
          datatable(model_results, options = list(pageLength = 5))
        })
        
        output$roc_plot <- renderPlotly({
          probs <- if ("prob" %in% model$control$classProbs) {
            predict(model, test_data, type = "prob")[, 2]
          } else {
            as.numeric(predictions)
          }
          
          pred <- prediction(probs, as.numeric(test_data[[target_var]]))
          perf <- performance(pred, "tpr", "fpr")
          
          roc_df <- data.frame(
            FPR = perf@x.values[[1]], 
            TPR = perf@y.values[[1]], 
            Model = input$model_select
          )
          
          p <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
            geom_line() +
            geom_abline(linetype = "dashed") +
            labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
            theme_minimal()
          
          ggplotly(p)
        })
        
        output$feature_importance <- renderPrint({
          req(input$model_select)
          model_choice <- input$model_select
          
          if (model_choice == "Random Forest" || model_choice == "Decision Trees") {
            numeric_columns <- sapply(train_data, is.numeric)
            train_data[numeric_columns] <- lapply(train_data[numeric_columns], scale)
            
            categorical_columns <- sapply(train_data, is.character)
            train_data[categorical_columns] <- lapply(train_data[categorical_columns], as.factor)
            
            train_data <- train_data[, sapply(train_data, function(col) var(as.numeric(col), na.rm = TRUE) > 0)]
            
            importance <- varImp(model, scale = TRUE)
            
            if (!is.null(importance) && all(!is.na(importance$importance))) {
              print(importance)
            } else {
              print("L'importance des variables n'est pas calculable avec ce modèle ou ces données.")
            }
          } else {
            print(paste("Feature importance is not available for", model_choice, "model."))
          }
        })
        
        output$training_status <- renderUI({
          tagList(
            h3("Training successfully completed!"),
            div(id = "loading", style = "display:none;")
          )
        })
        
      }, error = function(e) {
        output$training_status <- renderUI({
          tagList(
            h3("An error occurred while training the model."),
            p(paste("Error message:", e$message))
          )
        })
      })
    })
    
    
    
    
  
  
}
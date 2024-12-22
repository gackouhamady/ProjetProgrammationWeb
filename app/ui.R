library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(rmarkdown)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Exploration and Modeling App"),
  dashboardSidebar(
    sidebarMenu( 
      
      menuItem("Report", tabName = "project_report", icon = icon("file-alt")), 
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("chart-bar")),
      menuItem("Model Training", tabName = "model_training", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "project_report",
             fluidRow(
                box(
                  title = "Analyse Statistique : Etude de cas : Données de soins de Santé ", status = "primary", solidHeader = TRUE, width = 12,
                  htmlOutput("report_preview") # Dynamic preview of the markdown report
                )
              )
      ),
      
      # Data Upload Tab
      tabItem(tabName = "data_upload",
        fluidRow(
          box(
            title = "Upload Dataset", status = "primary", solidHeader = TRUE, width = 4,
            fileInput("file", "Choose Data File", accept = c(".csv", ".data", ".dat", ".xls", ".xlsx",".tsv",".json", "txt")),
            checkboxInput("header", "Header", TRUE),
            
            conditionalPanel(
              condition = "output.ext == 'data' || output.ext == 'dat' || output.ext == 'txt'",
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "),
                           selected = ",")
            ),
            radioButtons("quote", "Quote",
                         choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                         selected = '"')
          ),
          box(
            title = "Data Preview", status = "primary", solidHeader = TRUE, width = 8,
            DTOutput("data_preview")
          )
        )
      ),
      # Data Exploration Tab
      tabItem(tabName = "data_exploration",
        fluidRow(
          box(
            title = "Variable Information", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("diagnose_btn", "Diagnose Variables"),
            DTOutput("var_info")
          )
        ),
        fluidRow(
          box(
            title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 6,
            uiOutput("var_select"),
            uiOutput("var_select_2"),
            actionButton("analyze_btn", "Analyze")
          )),
        fluidRow(
            box(
              title = "Distribution Plot", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("dist")
            )),
        fluidRow(
          box(
            title = "Cumlative Distribution Plot", status = "primary", solidHeader = TRUE, width = 7,
            plotlyOutput("cum_dist")
          ),
          box(
            title = "Statistical Table", status = "primary", solidHeader = TRUE, width = 5,
            tableOutput("stat_table")
          )),
        fluidRow(
            box(
              title = "Box Plot", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("box_plot"),
            ),
            box(
              
              fluidRow(
                box(
                  title = "Statistics summary", status = "primary", solidHeader = TRUE, width = 12,
                  tableOutput("uni_summary"),)
              ),
              fluidRow(
                box(
                  title = "List of outliers", status = "primary", solidHeader = TRUE, width = 12,
                  tableOutput("outliers"),
                )
              )
            )
          
          ),
        fluidRow(
          box(
            title = "Bivariate Analysis", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("bi_plot"),
            verbatimTextOutput("bi_summary")
          ))
        
      ),
      # Model Training Tab
      tabItem(tabName = "model_training",
        fluidRow(
          box(
            title = "Preprocessing Options", status = "primary", solidHeader = TRUE, width = 4,
            pickerInput("target_var", "Select Target Variable", choices = NULL),
            checkboxInput("normalize", "Normalize Numeric Variables", FALSE),
            checkboxInput("dummify", "Convert Categorical Variables to Dummies", FALSE),
            checkboxInput("balance", "Apply Class Balancing", FALSE),
            radioButtons("missing_values", "Handle Missing Values",
                         choices = c("Remove Rows" = "remove", "Replace with Mean/Mode" = "impute", "Replace with Zero" = "zero"),
                         selected = "impute"),
            actionButton("preprocess_btn", "Preprocess Data")
          ),
          box(
            title = "Select positive class of reference", status = "primary", solidHeader = TRUE, width = 4,
            pickerInput("ref_class", "Select positive class", choices = NULL)
          ),
          box(
            title = "Model Selection and Training", status = "primary", solidHeader = TRUE, width = 4,
            checkboxGroupInput("models", "Select Models to Train",
                               choices = list("Logistic Regression" = "glm",
                                              "Random Forest" = "rf",
                                              "Support Vector Machine" = "svm"),
                               selected = c("glm", "rf")),
            actionButton("train_btn", "Train Models")
          )
        ),
        
        
        conditionalPanel( condition = "input.models.includes('glm')",
                          box(
                            title = "Logistic Regression Parameters", status = "primary", solidHeader = TRUE, width = 12,
                            numericInput("decay", label = "Regularization coef",value = 0.1, 
                                         min = 0, max = 10),
                            numericInput("maxit", label = "Max iterations",value = 10, 
                                         min = 1, max = 200)
                          ) ),
        
        conditionalPanel( condition = "input.models.includes('rf')",
                          box(
                            title = "Random Forest Parameters", status = "primary", solidHeader = TRUE, width = 12,
                            numericInput("ntree", label = "Num of trees", value = 10, 
                                         min = 1, max = 100),
                            numericInput("nodesize", label = "Min points in leaf", value = 10, 
                                         min = 1),
                            numericInput("maxnodes", label = "Max num of leaves", value = 10, 
                                         min = 1),
                            checkboxInput("replace", label = "Sample with replacement?")
                          ) ),
        
        conditionalPanel( condition = "input.models.includes('svm')",
                          box(
                            title = "Support Vector Machine Parameters", status = "primary", solidHeader = TRUE, width = 12,
                            numericInput("C", label = "Coef C", value = 1, 
                                         min = 0),
                            numericInput("sigma", label = "Coef Sigma", value = 1, 
                                         min = 0)
                            ) ),
        
        
        conditionalPanel(condition = "input.models.includes('glm')",
                         fluidRow(
                           box(
                             title = "Confusion Matrix", status = "primary", solidHeader = TRUE, width = 6,
                             tableOutput("glm_conf_matrix"),
                           ),
                           box(
                             title = "Macro Averaging", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("glm_macro_avg")
                           ),
                           box(
                             title = "Weighted Macro", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("glm_weighted_macro_avg")
                           ),
                           box(
                             title = "Micro Averaging", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("glm_micro_avg")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Model Evaluation", status = "primary", solidHeader = TRUE, width = 12,
                             valueBoxOutput("glm_acc", width = 4),
                             valueBoxOutput("glm_AUC", width = 4),
                             #tableOutput("glm_byclass_results"),
                             div(style = "overflow-x: auto; width: 100%;",
                                 tableOutput("glm_byclass_results"))
                             #verbatimTextOutput("feature_importance")
                           ))
                         
                         
                        ),
        conditionalPanel(condition = "input.models.includes('rf')",
                         fluidRow(
                           box(
                             title = "Confusion Matrix", status = "primary", solidHeader = TRUE, width = 6,
                             tableOutput("rf_conf_matrix"),
                           ),
                           box(
                             title = "Macro Averaging", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("rf_macro_avg")
                           ),
                           box(
                             title = "Weighted Macro", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("rf_weighted_macro_avg")
                           ),
                           box(
                             title = "Micro Averaging", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("rf_micro_avg")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Model Evaluation", status = "primary", solidHeader = TRUE, width = 12,
                             
                             valueBoxOutput("rf_acc", width = 4),
                             valueBoxOutput("rf_AUC", width = 4),
                             div(style = "overflow-x: auto; width: 100%;",
                                 tableOutput("rf_byclass_results")),
                             #verbatimTextOutput("feature_importance")
                           ))
                         
                         
                ),
        conditionalPanel(condition = "input.models.includes('svm')",
                         fluidRow(
                           box(
                             title = "Confusion Matrix", status = "primary", solidHeader = TRUE, width = 6,
                             tableOutput("svm_conf_matrix"),
                           ),
                           box(
                             title = "Macro Averaging", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("svm_macro_avg")
                           ),
                           box(
                             title = "Weighted Macro", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("svm_weighted_macro_avg")
                           ),
                           box(
                             title = "Micro Averaging", status = "primary", solidHeader = TRUE, width = 2,
                             tableOutput("svm_micro_avg")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Model Evaluation", status = "primary", solidHeader = TRUE, width = 12,
                             valueBoxOutput("svm_acc", width = 4),
                             valueBoxOutput("svm_AUC", width = 4),
                             div(style = "overflow-x: auto; width: 100%;",
                                 tableOutput("svm_byclass_results")),
                             #verbatimTextOutput("feature_importance")
                           ))
                         
                         
                  ),
          fluidRow(
            box(
              title = "ROC Curve", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("roc_plot")
            ))
        
        )
      )
    )
  
)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(rmarkdown)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "R Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed; overflow: visible;",
      menuItem("Report", tabName = "project_report", icon = icon("file-alt")),
      br(),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      br(),
      menuItem("EDA and Data Processing", tabName = "data_exploration", icon = icon("chart-bar")),
      br(),
      menuItem("Model Training", tabName = "model_training", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Project Report Tab
      tabItem(tabName = "project_report",
              fluidRow(
                box(
                  title = "Analyse Statistique : Etude de cas", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("Données d'attrition d'IBM",
                             htmlOutput("report_preview_1")
                    )
                  )
                )
              )
      ),
      
      # Data Upload Tab
      tabItem(tabName = "data_upload",
              fluidRow(
                box(
                  title = "Chargement des données", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("Upload Dataset",
                             fileInput("file", "Choose Data File", accept = c(".csv", ".data", ".dat", ".xls", ".xlsx", ".tsv", ".json", "txt")),
                             checkboxInput("header", "Header", TRUE),
                             conditionalPanel(
                               condition = "output.ext == 'data' || output.ext == 'dat' || output.ext == 'txt'",
                               radioButtons("sep", "Separator",
                                            choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "),
                                            selected = ",")
                             ),
                             radioButtons("quote", "Quote",
                                          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                          selected = '"'),
                             actionButton("load_btn", "Load")
                    ),
                    tabPanel("Data Summary", 
                             verbatimTextOutput("data_summary")
                    ),
                    tabPanel("Data Table", 
                             DTOutput("data_preview")
                    )
                  )
                )
              )
      ),
      
# Data Transformation  

tabItem(
  tabName = "data_exploration",
  fluidRow(
    # Ajouter un style CSS personnalisé pour la couleur du titre et la disposition du navbar
    tags$head(
      tags$style(HTML("
        .navbar-default .navbar-brand {
          color: blue !important;  /* Change the title color to blue */
          font-size: 24px;
        }
        .navbar-nav {
          margin-top: 10px;  /* Push navbar content below the title */
        }
      "))
    ),
    
    navbarPage(
      "EDA &  Preprocessing : Purposes : Models  Training",
      
      # Data Exploration Tab
      tabPanel("Data Table", DTOutput("data_table")),
      
      tabPanel("Correlation", plotlyOutput("correlation")),
      
      tabPanel(
        "2D Visualization",
        fluidRow(
          column(6, selectInput("var1", "Select First Variable", choices = NULL)),
          column(6, selectInput("var2", "Select Second Variable", choices = NULL))
        ),
        actionButton("analyze_button", "Analyze Bivariate Relationship"),
        plotlyOutput("two_d_visualization")
      ),
      
      tabPanel(
        "1D Visualization Types",
        fluidRow(
          column(12, selectInput("var1_1d", "Select Variable", choices = NULL))
        ),
        actionButton("analyze_univariate_button", "Analyze Univariate"),
        plotlyOutput("one_d_visualization")
      ),
      
      # Data Transformation
      tabPanel(
        "Missing Values",
        fluidRow(
          column(6,
                 radioButtons(
                   inputId = "missing_method",
                   label = "Select Method to Handle Missing Values:",
                   choices = c(
                     "Remove Rows" = "remove",
                     "Replace with Mean/Mode" = "mean_mode",
                     "Replace with Zero" = "zero"
                   ),
                   selected = "remove"
                 ),
                 actionButton("apply_missing_values", "Apply")
          )
        ),
        textOutput("missing_values_status")
      ),
      
      tabPanel(
        "Normalization",
        fluidRow(
          column(12,
                 radioButtons(
                   "normalize_method", 
                   "Normalization Method", 
                   choices = list(
                     "Standard Scaling (Z-score)" = "z_score",
                     "Min-Max Scaling" = "min_max"
                   ),
                   inline = TRUE
                 ),
                 actionButton("apply_normalization", "Apply Normalization")
          )
        ),
        textOutput("normalization_status"),
        DTOutput("normalized_table")
      ),
      
      tabPanel(
        "Variable Transformation",
        fluidRow(
          column(6, 
                 h3("Categorical Transformation"),
                 actionButton("apply_categorical_transformation", "Apply Categorization", class = "btn-primary"),
                 br(), br(),
                 h3("Dummification"),
                 actionButton("apply_dummification", "Apply Dummification", class = "btn-primary")
          ),
          column(6, 
                 textOutput("categorical_transformation_status"),
                 textOutput("dummification_status"),
                 DTOutput("dummified_table")
          )
        )
      ),
      
      tabPanel(
        "Outliers",
        fluidRow(
          column(6, 
                 actionButton("outliers_btn", "Identify Outliers"),
                 actionButton("handle_outliers_btn", "Handle Outliers")
          ),
          column(6, 
                 textOutput("outliers_status"),
                 DTOutput("outliers_table"),
                 textOutput("handled_outliers_status"),
                 DTOutput("handled_outliers_table")
          )
        )
      )
    )
  )
)
,




      # Model Training Tab
      tabItem(tabName = "model_training",
              fluidRow(
                box(
                  title = "Model Training", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("Model Training",
                             fluidRow(
                               column(3,
                                      selectInput("target_var", "Select Target : Only variables with two levels : ", choices = NULL)
                               ),
                               column(3,
                                      selectInput("model_select", "Select Model", 
                                                  choices = c("Random Forest", "Decision Trees", "SVM", "Logistic Regression", "KNN"))
                               ),
                               column(3,
                                      selectInput("imbalance_method", "Select Imbalance Method",
                                                  choices = c("Cost-sensitive learning", "Random Over-sampling", "Random Under-sampling"))
                               ),
                               column(3,
                                      selectInput("process_data", "Default : No Changes",
                                                  choices = c("Prepare Data"))
                               )
                             ),
                             fluidRow(
                               column(6, 
                                      div(style = "display: flex; justify-content: center; align-items: center;",
                                          actionButton("train_model", "Train Model", 
                                                       class = "btn btn-primary", 
                                                       style = "width: 100%; height: 45px; font-size: 18px;")
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      uiOutput("training_status_")
                               ),
                               column(6,
                                      uiOutput("training_status")
                               )
                             )
                    ),
                    tabPanel("Evaluation Results", 
                             box(
                               title = "Model Evaluation", 
                               status = "primary", 
                               solidHeader = TRUE, 
                               width = 12,
                               fluidRow(
                                 column(6, 
                                        DTOutput("model_results")
                                 ),
                                 column(6, 
                                        plotlyOutput("roc_plot")
                                 )
                               )
                             )
                    ),
                    tabPanel("Feature Importance", 
                             box(
                               title = "Feature Importance", 
                               status = "primary", 
                               solidHeader = TRUE, 
                               width = 12,
                               conditionalPanel(
                                 condition = "input.model_select == 'Random Forest' || input.model_select == 'Decision Trees'",
                                 verbatimTextOutput("feature_importance")
                               ),
                               conditionalPanel(
                                 condition = "input.model_select != 'Random Forest' && input.model_select != 'Decision Trees'",
                                 div(
                                   style = "color: red; font-weight: bold;",
                                   "Feature importance is not available for the selected model."
                                 )
                               )
                             )
                    )
                  )
                )
              )
      )
    )
  )
)

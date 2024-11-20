library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Exploration and Modeling App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("chart-bar")),
      menuItem("Model Training", tabName = "model_training", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data_upload",
        fluidRow(
          box(
            title = "Upload Dataset", status = "primary", solidHeader = TRUE, width = 4,
            fileInput("file", "Choose Data File", accept = c(".csv", ".data")),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "),
                         selected = ","),
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
            title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 4,
            uiOutput("var_select"),
            uiOutput("var_select_2"),
            actionButton("analyze_btn", "Analyze")
          ),
          box(
            title = "Univariate Analysis", status = "primary", solidHeader = TRUE, width = 4,
            plotlyOutput("uni_plot"),
            verbatimTextOutput("uni_summary")
          ),
          box(
            title = "Bivariate Analysis", status = "primary", solidHeader = TRUE, width = 4,
            plotlyOutput("bi_plot"),
            verbatimTextOutput("bi_summary")
          )
        )
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
            title = "Model Selection and Training", status = "primary", solidHeader = TRUE, width = 8,
            checkboxGroupInput("models", "Select Models to Train",
                               choices = list("Logistic Regression" = "glm",
                                              "Random Forest" = "rf",
                                              "Support Vector Machine" = "svm"),
                               selected = c("glm", "rf")),
            actionButton("train_btn", "Train Models")
          )
        ),
        fluidRow(
          box(
            title = "Model Evaluation", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("model_results"),
            plotlyOutput("roc_plot"),
            verbatimTextOutput("feature_importance")
          )
        )
      )
    )
  )
)

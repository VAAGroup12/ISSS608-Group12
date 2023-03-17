# step: 
#1. insert packages, arranged data set(避免重名)
#2. 已有图片（事前完成ui和server）加入dashboardBody



#---------install and load packages-----------------

pacman::p_load(shiny, readxl, ggstatsplot, ggplot2, shinydashboard, plotly, tidyverse)

#---------import data---------------------------------

loandata <- read_xlsx("data/loan.xlsx")


#---------model input---------------------------------


#---------dashboard structure [siderbar]-------------------------
header <- 
  dashboardHeader( title = HTML("Loan Default Prediction"))

siderbar <- 
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
                menuItem("Loan Data Exploration", tabName = "eda", startExpanded = FALSE, icon = icon("search"),
                         menuSubItem("Overall Analysis", tabName = "eda_1"),
                         menuSubItem("Distribution Analysis", tabName = "eda_2"), 
                         menuSubItem("Correlation Analysis", tabName = "eda_3") ),
                         
                menuItem("Model Building", tabName = "model_build", startExpanded = FALSE, icon = icon("cogs"),
                         menuSubItem("Model 1", tabName = "model_1"),
                         menuSubItem("Model 2", tabName = "model_2"),
                         menuSubItem("Model 3", tabName = "model_3"),
                         menuSubItem("Model Compare", tabName = "model_compare")
                ),
                menuItem("Repayment Prediction", tabName = "prediction", startExpanded = FALSE, icon = icon("dollar-sign")
                )
    )
  )

#--------dashboardbody--------------------------------
eda_tab_1 <- 
  fluidRow(
    box(
      title = "Loan Amount Distribution",
      status = "primary",
      solidHeader = TRUE,
      selectInput(inputId = "y_var",
                  label = "Select y variable:",
                  choices = c("Default_ratio", "Total_loan", "Total_due"),
                  selected = "Default_ratio"),
      plotOutput(outputId = "tween")
    ),
    box( #add a new graph
      title = "Interest Rate Trend",
      status = "primary",
      solidHeader = TRUE,
      plotOutput(outputId = "interest_rate_trend")
    )
  )

eda_tab_2 <-
  fluidRow(
    tabBox(
      title = "Overall Analysis",
      id = "tabset1",
      selected = "tab1",
      width = 12,
      height = "600px",
      tabPanel("111",
               # then same as above
               box(
                 title = NULL,
                 status = "primary",
                 solidHeader = TRUE,
                 selectInput(inputId = "y_var",
                             label = "Select y variable:",
                             choices = c("Default_ratio", "Total_loan", "Total_due"),
                             selected = "Default_ratio"),
                 plotOutput(outputId = "123")
               )),
      # Another graph
      tabPanel("222",
               box(
                 title = NULL,
                 status = "primary",
                 solidHeader = TRUE,
                 plotOutput(outputId = "interest_rate_trend")
               ))
    )
  )

      
#--------------------------------------------------------      

boardbody <- 
  dashboardBody(
    tabItems(
      tabItem("intro", 
              h2("Welcome to our Loan Default Prediction APP"),
              p(a("Click here for user guide", href = "https://example.com/user-guide", target="_blank", style = "font-size: 15px;")),
              fluidRow(
                column(width = 6, img(src = "picture1.jpg", width = "100%")),
                column(width = 6, img(src = "picture2.jpg", width = "100%"))
              ),
              fluidRow(
                column(width = 12, img(src = "picture3.jpg", width = "100%"))
              )
      ),
      tabItem("eda_1", eda_tab_1),
      tabItem("eda_2", "eda_tab_2"),
      tabItem("eda_3", "Correlation Analysis content"),
      tabItem("model_1", "Model 1 content"),
      tabItem("model_2", "Model 2 content"),
      tabItem("model_3", "Model 3 content"),
      tabItem("model_compare", "Model Comparison content"),
      tabItem("prediction", "Repayment Option 1 content")
    )
  )

ui <- dashboardPage(skin = "yellow",
                    header, 
                    siderbar,
                    boardbody)

server <- function(input, output) {
  output$tween <- renderPlot({
    ggbetweenstats(
      data = loandata,
      x = good_bad_flag,
      y = !!sym(input$y_var),
      type = "p",
      mean.ci = TRUE,
      pairwise.comparisons = TRUE,
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE
    )
  }) 
}

shinyApp(ui, server)



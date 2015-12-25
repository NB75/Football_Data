# The user-interface definition of the Shiny web app.
library(shiny)
library(rCharts)
library(dplyr)

league_desc <- c("Premier League","Serie A","Bundesliga","La Liga")

shinyUI(
  fluidPage(
    headerPanel(fluidRow(h1('Football Statistics', align = "center"),
                h2('Data on the 4 main european football leagues. Period 2011 - 2015', align = "center"))),
             sidebarPanel(
               h4('Selection Parameters', align = "center"),
               radioButtons("League_RB", 
                            "Select League", 
                            league_desc, 
                            selected = "Premier League", 
                            inline = FALSE, 
                            width = NULL),
               radioButtons("Year_RB", 
                                  "Select Season", 
                                  c(2011:2015), 
                                  selected = 2011, 
                                  inline = FALSE, 
                                  width = NULL),
               sliderInput("matchdays", 
                            "Matchdays:", 
                            min = 1,
                            max = 38,
                            value = c(1, 38))
             ),
             mainPanel(
               tabsetPanel(
                       tabPanel(p(icon("line-chart"), "Charts"),
                               h4('Standings', align = "center"),
                               showOutput("chart1","nvd3"),
                               h4('Shots & Goals', align = "center"),
                               showOutput("chart2","nvd3"),
                               h4('Relationship Shots vs Goals', align = "center"),
                               showOutput("chart3","nvd3"),
                               h4('Fouls', align = "center"),
                               showOutput("chart4","nvd3")),
                       tabPanel(p(icon("table"), "Full Data"),
                                dataTableOutput("dataTable")),
                       tabPanel(p(icon("cogs"), "Regression"),
                                h4('Predict Number of Goals based on Number of Shots on Target', align = "left"),
                                numericInput("sot","1) Expected Number of Shots on Target",value=0,min=0),
                                radioButtons("League_Pred_RB", 
                                             "2) Select League for which you want to predict", 
                                             league_desc, 
                                             selected = "Premier League", 
                                             inline = FALSE, 
                                             width = NULL),
                                h5("3) Click the button to generate the prediction"),
                                actionButton("predButton", "Generate Prediction"),
                                h5('4) Number of Goals Predicted, based on Historical Data'),
                                verbatimTextOutput("pred_goals"),
                                br(),
                                h4('Average Number of Goals, SOT and Regression Coefficients in the period 2011 - 2015', align = "left"),
                                dataTableOutput("regressionTable")
                       )
               )
    )
)
)
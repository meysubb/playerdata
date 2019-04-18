library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  
  selectInput("player","Select Player:",
              choices = sort(unique(as.character(finalWeeklyData$Player2))),
              selected = "Aaron Rodgers, QB"),
  sliderInput("year","Select Year:",2000,2018,2018,1),
  radioButtons("scoring","Select Scoring:",
               choices = c("PPR","HalfPPR","Standard","FourPtTD","SixPtTD"),
               selected = "PPR", inline = TRUE)
  
)

ui <- dashboardPage(
  
  dashboardHeader(title = "Player Data"),
  
  sidebar,
  
  dashboardBody(
    
    fluidRow(
      tabBox(title = "Volume by Quarters",
             tabPanel("Pass Attempts",plotlyOutput("passquarter")),
             tabPanel("Targets",plotlyOutput("targetquarter")),
             tabPanel("Rush Attempts",plotlyOutput("rushquarter"))),
      tabBox(title = "Volume% by Quarters",
             tabPanel("Targets%",plotlyOutput("targetquarter2")),
             tabPanel("Rush Attempts%",plotlyOutput("rushquarter2")))
    ),
    
    fluidRow(
    box(title = "Yearly Position Rank", plotOutput("yearchart"), collapsible = TRUE, solidHeader = TRUE, status = "primary"),
    box(title = "Weekly Points Scored", plotOutput("weeklypoints"), collapsible = TRUE, solidHeader = TRUE, status = "primary")
    ),
    
    br(),
    
    fluidRow(
      box(title = "Game Log", dataTableOutput("gamelog"), collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12)
    ),
    
    br(),

    fluidRow(
      box(title = "Career Stats", dataTableOutput("careerstats"), collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12)
    )
    
    
  ),
  tags$head(tags$style(HTML('
                              .sidebar {
                            color: #FFF;
                            position: fixed;
                            width: 230px;
                            white-space: nowrap;
                            overflow: visible;
                            }
                            .main-header {
                            position: fixed;
                            width:100%;
                            }
                            .content-wrapper, .right-side {
                            min-height: 100%;
                            background-color: #efefef;
                            z-index: 800;
                            }
                            table.dataTable thead th, table.dataTable thead td {
                            padding: 10px 18px;
                            border-bottom: 1px solid #111;
                            background: #ffffff;
                            }
                            .content {
                            padding-top: 120px!important;
                            }
                            
                            @media (min-width:768px) {
                            .content {
                            padding-top: 80px!important;
                            }
                            }
                            
                            @media (max-width:767px) {
                            .skin-black .main-header>.logo {
                            text-align: left;
                            }
                            }')))
)



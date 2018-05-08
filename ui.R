library(shiny)
library(shinyjs)
library(DT)

shinyUI(
  fluidPage(
    
    shinyjs::useShinyjs(),
    
    # Application title
    titlePanel("SMS Scheduling"),
    
    tags$head(
      tags$style(HTML("hr {border-top: solid #000000; margin-top: 8px;}"))
    ),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      
      sidebarPanel(
        
        uiOutput("debugging_ui"),
        
        tabsetPanel(
          
          tabPanel(
            "Scheduling",
            
            br(),
            
            splitLayout(
              cellWidths = c("30%", "70%"),
              p("Message", align = "center"),
              hr()
            ),
            textInput("sms_body", "Enter Text Message"),
            br(),
            br(),
            
            splitLayout(
              cellWidths = c("30%", "70%"),
              p("Frequency", align = "center"),
              hr()
            ),
            numericInput("sms_quantity", "Number of messages", 3, min = 0),
            numericInput("sms_frequency", "Frequency of messages (in minutes)", 30, min = 0),
            br(),
            br(),
            
            splitLayout(
              cellWidths = c("30%", "70%"),
              p("Scheduling", align = "center"),
              hr()
            ),
            
            br(),
            
            uiOutput("time_ranges"),
            
            hr(),
            br(),
            
            div(
              align="center",
              actionButton(
                "schedule_sms",
                "Schedule it!"
              )
            )
          ),
          
          tabPanel(
            "General",
            
            br(),
            
            uiOutput("general_ui")
            
          )
        )
      ),
      
      mainPanel(
        DT::dataTableOutput("my_table")
      )
    )
  ))

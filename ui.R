library(shiny)
library(DT)

shinyUI(fluidPage(

  # Application title
  titlePanel("Sending SMS Demo"),

  tags$head(
    tags$style(HTML("hr {border-top: solid #000000; margin-top: 8px;}"))
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      actionButton("debugging", "Debug"),
      
      splitLayout(
        cellWidths = c("30%", "70%"),
        p("Recipient and Message", align = "center"),
        hr()
      ),
      textInput("email", "Email Address"),
      numericInput("my_number", "Enter Phone Number", 0000000000, min = 0000000000, max = 9999999999),
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

      sliderInput(
        "time_range",
        "Time Range",
        value = c(strptime("09:00 AM", "%I:%M %p", tz = "EST"), strptime("02:00 PM", "%I:%M %p", tz="EST")),
        min = strptime("06:00 AM", "%I:%M %p", tz = "EST"),
        max = strptime("09:00 PM", "%I:%M %p", tz = "EST"),
        dragRange = T,
        step = 900,
        ticks = F,
        timeFormat = "%I:%M %p",
        timezone = "-0500"
      ),

      checkboxGroupInput("weekdays_input", "Days for messaging",
        choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
      ),

      hr(),
      br(),

      div(
        align="center",
        actionButton("schedule_sms", "Schedule it!")
      )
    ),

    mainPanel(
      DT::dataTableOutput("my_table"),
      textOutput("test_emoji")
    )
  )
))

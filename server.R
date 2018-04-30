library(shiny)
library(RMySQL)
library(twilio)
library(DT)
library(dplyr)
library(RCurl)

values <<- reactiveValues(status = "Send a message", twilio_phone = "7866193595", mydb = NULL)
values$my_table <<- data.frame(stringsAsFactors = FALSE)
values$get_participants <<- 0
time_range <<- c(strptime("09:00 AM", "%I:%M %p", tz="America/New_York"), strptime("02:00 PM", "%I:%M %p", tz="America/New_York"))
default_time_range <<- c(strptime("09:00 AM", "%I:%M %p", tz="America/New_York"), strptime("02:00 PM", "%I:%M %p", tz="America/New_York"))
start_date <<- Sys.Date()
week_range <<- 0

# Anything that calls autoInvalidate will automatically invalidate
# every 2 seconds.
autoInvalidate <- reactiveTimer(2000)

observeEvent(autoInvalidate(), {

  todo_mail <- values$my_table[values$my_table$quantity > 0 & values$my_table$scheduled_sms <= Sys.time(),]

  if(nrow(todo_mail) > 0) {
    for(recipient in row.names(todo_mail)) {

      values$my_table[recipient,]$quantity <<- values$my_table[recipient,]$quantity - 1
      values$my_table[recipient,] <<- GetNextScheduleTime(values$my_table, recipient)

      tw_send_message(from = values$twilio_phone, to = 3057930763,#values$my_table[recipient,]$phone,
                      body = paste(as.character(values$my_table[recipient,]$sms_body), as.character(values$my_table[recipient,]$survey_link)))
    }
  }

  #30 seconds before next text is due, refresh the survey links in the table
  new_surveys <- values$my_table[values$my_table$quantity > 0 & values$my_table$scheduled_sms - 30 <= Sys.time(),]

  if(nrow(new_surveys) > 0) {
    values$get_participants <<- values$get_participants + 1
  }
})

observeEvent(values$get_participants, ignoreNULL = F, ignoreInit = F, {
  
  week_number <- (-1 * GetWeekNumber("2018-04-18")) + GetWeekNumber(Sys.Date())
  
  source("get_participant_list.R")
  
  lscb_records <<- redcapExportRecords(api_url, api_token, "lscb_parent", paste0("week_", week_number, "_arm_1"))
  bcb_records <<- redcapExportRecords(api_url, api_token, "bcb_parent", paste0("week_", week_number, "_arm_1"))
  ccb_records <<- redcapExportRecords(api_url, api_token, "ccb_parent", paste0("week_", week_number, "_arm_1"))

  for(i in rownames(contact_list)) {
    
    record_id <- contact_list[i,]$record_id
    
    if(tail(rownames(contact_list[contact_list$record_id == record_id,]), 1) == i) {
    
      this_phone <- head(contact_list[contact_list$record_id == record_id,], 1)$phone
      this_phone <- gsub("\\(", "", this_phone)
      this_phone <- gsub(")", "", this_phone)
      this_phone <- gsub("-", "", this_phone)
      this_phone <- gsub(" ", "", this_phone)
        
      instrument <- randomization[randomization$record_id == record_id, paste0("week_", week_number, "_arm_1")]
      
      instrument_records <- get(paste0(instrument, "_records"))
      
      survey_link <- tail(instrument_records[instrument_records$record == record_id,]$survey_link, 1)
      
      if(this_phone %in% values$my_table$phone) {
        
        values$my_table[values$my_table$phone == this_phone,]$survey_link <<- survey_link
        values$my_table[values$my_table$phone == this_phone,]$instrument <<- instrument
      }
      
      else {
        
        values$my_table <<- rbind(
          values$my_table,
          data.frame(
            email = tail(instrument_records[instrument_records$record == record_id,]$email, 1),
            phone = this_phone,
            quantity = 0,
            frequency = 30,
            days = "",
            time_start = as.POSIXct(default_time_range[1]),
            time_end = as.POSIXct(default_time_range[2]),
            instrument = instrument,
            scheduled_sms = as.POSIXlt(Sys.time(), tz = "UTC"),
            sms_body = "",
            survey_link = survey_link,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
})


shinyServer(function(input, output) {
  
  output$general_ui <- renderUI({
    div(
      dateInput(
        "start_date",
        "Start Date",
        value = start_date
      ),
      
      numericInput(
        "week_range",
        label = "Week Range",
        value = week_range,
        min = 0
      )
    )
  })
  
  observeEvent(input$debugging, {
    browser()
  })
  
  observeEvent({
    input$start_date
  }, ignoreInit = T, {
    
    start_date <<- input$start_date
  })
  
  observeEvent({
    input$week_range
  }, ignoreInit = T, {
    
    week_range <<- input$week_range
  })

  observeEvent({
    input$time_range
  }, {
    time_range <<- input$time_range
  })

  observeEvent({
    input$schedule_sms
  }, ignoreInit = T, {

    if(isTruthy(input$my_table_rows_selected)) {
      values$my_table[input$my_table_rows_selected,]$quantity <<- input$sms_quantity
      values$my_table[input$my_table_rows_selected,]$frequency <<- input$sms_frequency
      values$my_table[input$my_table_rows_selected,]$time_start <<- as.POSIXct(time_range[1])
      values$my_table[input$my_table_rows_selected,]$time_end <<- as.POSIXct(time_range[2])
      values$my_table[input$my_table_rows_selected,]$days <<- toString(input$weekdays_input)
      values$my_table[input$my_table_rows_selected,] <<- GetNextScheduleTime(values$my_table, rownames(values$my_table[input$my_table_rows_selected,])[1])
      values$my_table[input$my_table_rows_selected,]$sms_body <<- input$sms_body
    }
    
    reset("time_range")
    reset("sms_body")
    reset("sms_quantity")
    reset("sms_frequency")
    reset("weekdays_input")
  })

  output$my_table <- DT::renderDataTable({
    if(nrow(values$my_table) > 0) {
      datatable(values$my_table %>%
        mutate(
          time_start = format.POSIXct(time_start, "%I:%M %p", tz="America/New_York"),
          time_end = format.POSIXct(time_end, "%I:%M %p", tz="America/New_York"),
          scheduled_sms = format(scheduled_sms, "%a %b %d %Y %I:%M %p", tz = "America/New_York"),
          frequency = paste0(as.character(frequency), " minutes")
        ),
        rownames = F,
        selection = "single"
      )
    }
    else {
      return(data.frame())
    }
  })

})

library(shiny)
library(RMySQL)
library(twilio)
library(DT)
library(dplyr)
library(RCurl)

values <<- reactiveValues(status = "Send a message", twilio_phone = "7866193595", mydb = NULL)
values$get_participants <<- 0
time_range <<- c(strptime("09:00 AM", "%I:%M %p", tz="America/New_York"), strptime("02:00 PM", "%I:%M %p", tz="America/New_York"))
default_time_range <<- c(strptime("09:00 AM", "%I:%M %p", tz="America/New_York"), strptime("02:00 PM", "%I:%M %p", tz="America/New_York"))
start_date <<- "2018-04-18"
week_range <<- 0

# Anything that calls autoInvalidate will automatically invalidate
# every 2 seconds.
autoInvalidate <- reactiveTimer(2000)

observeEvent(autoInvalidate(), {
  
  todo_mail <<- names(values$my_table$quantity[values$my_table$quantity > 0])
  remove_from_todo <<- NULL

  if(!is.null(todo_mail) && length(todo_mail) > 0) {
    
    lapply(1:length(todo_mail), function(todo_i) {
      
      todo_sms <- todo_mail[[todo_i]]
      
      # remove the phone number from the sms-todo list if the scheduled sms delivery time has not arrived yet
      if(values$my_table$scheduled_sms[[todo_sms]] > Sys.time()) {

        remove_from_todo <<- c(remove_from_todo, todo_mail[[todo_i]])
      }
    })
  
    todo_mail <<- todo_mail[!(todo_mail %in% remove_from_todo)]
  
    if(length(todo_mail) > 0) {
      for(recipient in todo_mail) {
        
        values$my_table$quantity[[recipient]] <<- values$my_table$quantity[[recipient]] - 1
        values$my_table <<- GetNextScheduleTime(values$my_table, recipient)
  
        tw_send_message(from = values$twilio_phone, to = 3057930763,#values$my_table[recipient,]$phone,
                        body = paste(as.character(values$my_table$sms_body[[recipient]]), as.character(values$my_table$survey_link[[recipient]])))
      }
    }
  
    #30 seconds before next text is due, refresh the survey links in the table
    new_surveys <- lapply(names(values$my_table$quantity[values$my_table$quantity > 0]), function(x) {values$my_table$scheduled_sms[[x]] - 30 <= Sys.time()})
  
    if(T %in% new_surveys) {
      values$get_participants <<- values$get_participants + 1
    }
  }
})

observeEvent(values$get_participants, ignoreNULL = F, ignoreInit = F, {
  
  week_number <- (-1 * GetWeekNumber(start_date) - 1) + GetWeekNumber(Sys.Date())
  
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
        
        values$my_table$survey_link[[this_phone]] <<- survey_link
        values$my_table$instrument[[this_phone]] <<- instrument
      }
      
      else {
        
        values$my_table$email[[this_phone]] <<- tail(instrument_records[instrument_records$record == record_id,]$email, 1)
        values$my_table$phone[[this_phone]] <<- this_phone
        values$my_table$quantity[[this_phone]] <<- 0
        values$my_table$frequency[[this_phone]] <<- 30
        values$my_table$days[[this_phone]] <<- ""
        values$my_table$time_start[[this_phone]]$time_start_1 <<- default_time_range[1]#$time_start_1 <<- as.POSIXct(default_time_range[1])
        values$my_table$time_end[[this_phone]]$time_end_1 <<- default_time_range[2]
        values$my_table$instrument[[this_phone]] <<- instrument
        values$my_table$scheduled_sms[[this_phone]] <<- as.POSIXlt(Sys.time())
        values$my_table$sms_body[[this_phone]] <<- ""
        values$my_table$survey_link[[this_phone]] <<- survey_link
      }
    }
  }
})


shinyServer(function(input, output, session) {
  
  observeEvent(input$debugging, {
    browser()
  })
  
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
  
  output$time_ranges <- renderUI({
    
    if(is.null(input$my_table_rows_selected) == F) {
      
      selected_phone <- values$my_table$phone[[input$my_table_rows_selected]]
      
      time_ranges <- list(time_start = values$my_table$time_start[[selected_phone]], time_end = values$my_table$time_end[[selected_phone]])
      
      inputTagList <- tagList()
      
      sapply(1:length(time_ranges$time_start), function(i) {
        inputTagList <<- tagAppendChild(inputTagList, sliderInput(
          inputId = paste0("time_range_", i),
          label = paste0("Time Range ", i),
          value = c(as.POSIXlt(time_ranges$time_start[[paste0("time_start_", i)]], "%I:%M %p", tz = "America/New_York"), as.POSIXlt(time_ranges$time_end[[paste0("time_end_", i)]], "%I:%M %p", tz="America/New_York")),
          min = strptime("06:00 AM", "%I:%M %p", tz = "America/New_York"),
          max = strptime("09:00 PM", "%I:%M %p", tz = "America/New_York"),
          dragRange = T,
          step = 900,
          ticks = F,
          timeFormat = "%I:%M %p",
          timezone = NULL
        ))
      })

      return(
        div(
          inputTagList,
          
          splitLayout(
            
            cellWidths = c("33%", "34%", "33%"),
            
            actionButton(
              inputId = "reset_time_ranges",
              label = "Reset Time Ranges"
            ),
            
            actionButton(
              inputId = "add_time_ranges",
              label = "Add Time Ranges"
            ),
            
            actionButton(
              inputId = "submit_time_ranges",
              label = "Submit Time Ranges"
            )
          )
        )
      )
    }
    
    else {
      return(div())
    }
  })
  
  observeEvent({
    input$add_time_ranges
  }, ignoreInit = T, {
    
    if(is.null(input$my_table_rows_selected) == F) {
      
      selected_phone <- values$my_table$phone[[input$my_table_rows_selected]]
      
      num_time_ranges <- length(values$my_table$time_start[[selected_phone]])
      values$my_table$time_start[[selected_phone]][[paste0("time_start_", num_time_ranges + 1)]] <<- default_time_range[1]
      values$my_table$time_end[[selected_phone]][[paste0("time_end_", num_time_ranges + 1)]] <<- default_time_range[2]
    }
  })
  
  observeEvent({
    input$submit_time_ranges
  }, ignoreInit = T, {

    if(is.null(input$my_table_rows_selected) == F) {
      
      selected_phone <- values$my_table$phone[[input$my_table_rows_selected]]
      
      time_ranges <- list(time_start = values$my_table$time_start[[selected_phone]], time_end = values$my_table$time_end[[selected_phone]])
      
      num_times <- length(time_ranges$time_start)
      
      sapply(1:num_times, function(i) {
        
        values$my_table$time_start[[selected_phone]][[paste0("time_start_", i)]] <<- input[[paste0("time_range_", i)]][1]
        values$my_table$time_end[[selected_phone]][[paste0("time_end_", i)]] <<- input[[paste0("time_range_", i)]][2]
      })
    }
  })
  
  observeEvent({
    input$reset_time_ranges
  }, ignoreInit = T, {
    
    if(is.null(input$my_table_rows_selected) == F) {
      
      selected_phone <- values$my_table$phone[[input$my_table_rows_selected]]
      values$my_table$time_start[[selected_phone]] <<- NULL
      values$my_table$time_end[[selected_phone]] <<- NULL
      values$my_table$time_start[[selected_phone]]$time_start_1 <<- default_time_range[1]
      values$my_table$time_end[[selected_phone]]$time_end_1 <<- default_time_range[2]
    }
  })
  
  observeEvent({
    input$start_date
  }, ignoreInit = T, {
    
    start_date <<- input$start_date
    values$get_participants <<- values$get_participants + 1
  })
  
  observeEvent({
    input$week_range
  }, ignoreInit = T, {
    
    week_range <<- input$week_range
  })

  observeEvent({
    input$schedule_sms
  }, ignoreInit = T, {

    if(isTruthy(input$my_table_rows_selected)) {
      
      selected_phone <- values$my_table$phone[[input$my_table_rows_selected]]
      
      values$my_table$quantity[[selected_phone]] <<- input$sms_quantity
      values$my_table$frequency[[selected_phone]] <<- input$sms_frequency
      values$my_table$days[[selected_phone]] <<- toString(input$weekdays_input)
      values$my_table <<- GetNextScheduleTime(values$my_table, selected_phone)
      values$my_table$sms_body[[selected_phone]] <<- input$sms_body
    }
    
    shinyjs::reset("sms_body")
    shinyjs::reset("sms_quantity")
    shinyjs::reset("sms_frequency")
    shinyjs::reset("weekdays_input")
  })
  
  observeEvent({
    input$my_table_rows_selected
  }, ignoreInit = T, ignoreNULL = F, {
    
    if(!is.null(input$my_table_rows_selected) && input$my_table_rows_selected > 0) {
      
      selected_phone <- values$my_table$phone[[input$my_table_rows_selected]]
      
      updateTextInput(session, "sms_body", value = values$my_table$sms_body[[selected_phone]])
      updateNumericInput(session, "sms_quantity", value = values$my_table$quantity[[selected_phone]])
      updateNumericInput(session, "sms_frequency", value = values$my_table$frequency[[selected_phone]])
      updateCheckboxInput(session, "weekdays_input", value = strsplit(values$my_table$days[[selected_phone]], ", ")[[1]])
    }
    
    else {
      
      updateTextInput(session, "sms_body", value = "")
      updateNumericInput(session, "sms_quantity", value = 0)
      updateNumericInput(session, "sms_frequency", value = 30)
      updateCheckboxInput(session, "weekdays_input", value = character(0))
    }
      
  })

  output$my_table <- DT::renderDataTable({
    
    temp_table <- values$my_table
    
    temp_table$time_start <- as.character(
      lapply(temp_table$time_start, function(x) {
        paste(lapply(x, format, format="%I:%M %p", tz="America/New_York"), collapse = ",\n")
      })
    )
    
    temp_table$time_end <- as.character(
      lapply(temp_table$time_end, function(x) {
        paste(lapply(x, format, format="%I:%M %p", tz="America/New_York"), collapse = ",\n")
      })
    )
    
    temp_table$scheduled_sms <- as.character(
      lapply(temp_table$scheduled_sms, function(x) {
        format(x, format="%a %b %d %Y %I:%M %p", tz="America/New_York")
      })
    )
    
    temp_table$frequency <- as.character(
      lapply(temp_table$frequency, function(x) {
        paste0(as.character(x), " minutes")
      })
    )
    
    #return(as.data.frame(temp_table))
    
    if(length(temp_table$email) > 0) {
      datatable(
        as.data.frame(
          temp_table,
          optional = T,
          col.names = c(
            "Email",
            "Phone",
            "Messages",
            "Frequency",
            "Days",
            "Start Time",
            "End Time",
            "Instrument",
            "Next SMS",
            "SMS Body",
            "Survey Link"
          )
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

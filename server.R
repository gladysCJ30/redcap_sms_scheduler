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

# Anything that calls autoInvalidate will automatically invalidate
# every 2 seconds.
autoInvalidate <- reactiveTimer(2000)

observeEvent(autoInvalidate(), {

  todo_mail <- values$my_table[values$my_table$quantity > 0 & values$my_table$scheduled_sms <= Sys.time(),]

  if(nrow(todo_mail) > 0) {
    for(recipient in row.names(todo_mail)) {

      values$my_table[recipient,]$quantity <<- values$my_table[recipient,]$quantity - 1
      values$my_table[recipient,] <<- GetNextScheduleTime(values$my_table, recipient)

      tw_send_message(from = values$twilio_phone, to = values$my_table[recipient,]$phone,
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

  source("get_participant_list.R")

  for(i in rownames(participant_list)) {
    if(participant_list[i,]$email %in% values$my_table$email) {
      values$my_table[values$my_table$email == participant_list[i,]$email,]$survey_link <<- as.character(participant_list[i,]$survey_link)
    }
    else {
      values$my_table <<- rbind(
        values$my_table,
        data.frame(
          email = participant_list[i,]$email,
          phone = as.numeric(gsub("-", "", participant_list[i,]$phone)),
          quantity = 0,
          frequency = 30,
          days = "",
          time_start = as.POSIXct(default_time_range[1]),
          time_end = as.POSIXct(default_time_range[2]),
          scheduled_sms = as.POSIXlt(Sys.time(), tz = "UTC"),
          sms_body = "",
          survey_link = participant_list[i,]$survey_link,
          stringsAsFactors = FALSE
        )
      )
    }
  }
})


shinyServer(function(input, output) {

  observeEvent(input$time_range, {
    time_range <<- input$time_range
  })

  observeEvent({
    input$schedule_sms
  }, ignoreInit = T,
  {
    my_phone_number <- input$my_number

    if(input$my_number %in% values$my_table$phone) {
      values$my_table[values$my_table$phone == input$my_number,]$email <<- input$email
      values$my_table[values$my_table$phone == input$my_number,]$quantity <<- input$sms_quantity
      values$my_table[values$my_table$phone == input$my_number,]$frequency <<- input$sms_frequency
      values$my_table[values$my_table$phone == input$my_number,]$time_start <<- as.POSIXct(time_range[1])
      values$my_table[values$my_table$phone == input$my_number,]$time_end <<- as.POSIXct(time_range[2])
      values$my_table[values$my_table$phone == input$my_number,]$days <<- toString(input$weekdays_input)
      values$my_table[values$my_table$phone == input$my_number,] <<- GetNextScheduleTime(values$my_table, rownames(values$my_table[values$my_table$phone == input$my_number,])[1])
      values$my_table[values$my_table$phone == input$my_number,]$sms_body <- input$sms_body
    }
    else if(input$my_number > 0 && input$my_number <= 9999999999){
      if(input$email %in% values$my_table$email) {
        #values$my_table[values$my_table$email == input$email,]$email
        values$my_table[values$my_table$email == input$email,]$quantity <<- input$sms_quantity
        values$my_table[values$my_table$email == input$email,]$frequency <<- input$sms_frequency
        values$my_table[values$my_table$email == input$email,]$phone <<- input$my_number
        values$my_table[values$my_table$email == input$email,]$time_start <<- as.POSIXct(time_range[1])
        values$my_table[values$my_table$email == input$email,]$time_end <<- as.POSIXct(time_range[2])
        values$my_table[values$my_table$email == input$email,]$days <<- toString(input$weekdays_input)
        values$my_table[values$my_table$email == input$email,] <<- GetNextScheduleTime(values$my_table, rownames(values$my_table[values$my_table$email == input$email,])[1])
        values$my_table[values$my_table$email == input$email,]$sms_body <- input$sms_body
      }
    }
  })

  output$my_table <- DT::renderDataTable({
    if(nrow(values$my_table) > 0) {
      datatable(values$my_table %>%
        mutate(
          time_start = format.POSIXct(time_start, "%I:%M %p", tz="America/New_York"),
          time_end = format.POSIXct(time_end, "%I:%M %p", tz="America/New_York"),
          scheduled_sms = format(scheduled_sms, "%a %b %d %Y %I:%M %p", tz = "America/New_York"),
          frequency = paste0(as.character(frequency), " minutes")
        ), rownames = F)
    }
    else {
      return(data.frame())
    }
  })

})

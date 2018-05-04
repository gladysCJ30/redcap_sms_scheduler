#These are global functions

options("stringsAsFactors" = FALSE, shiny.trace = T)

source("configuration.R")

##
# Gets the next available time to schedule a sms message.
# INPUT: the schedule dataframe, the row id
##
GetNextScheduleTime <- function(my_table, recipient) {
  #If the next message wont fit within the time range today OR today is not on the list of weekdays to survey:
  
  time_difference <- as.difftime(c(format(my_table$scheduled_sms[[recipient]], "%I:%M %p", tz = "America/New_York"), format(my_table$time_end[[recipient]], "%I:%M %p", tz = "America/New_York")), format = "%I:%M %p", units = "secs")
  scheduled_time <- time_difference[[1]] + (my_table$frequency[[recipient]] * 60)
  ending_time <- time_difference[[2]]
  
  
  if( scheduled_time > ending_time || length(grep(format(my_table$scheduled_sms[[recipient]] + (my_table$frequency[[recipient]] * 60), "%A", tz = "America/New_York"), my_table$days[[recipient]])) == 0 ) {

    #Find the next weekday that a message should be sent
    #86400 is the number of seconds in a day
    for(i in 1:7) {
      if(length(grep(format(my_table$scheduled_sms[[recipient]] + (86400 * i), "%A", tz = "America/New_York"), my_table$days[[recipient]])) == 1) {
        my_table$scheduled_sms[[recipient]] <- strptime(paste0(as.character(format(my_table$scheduled_sms[[recipient]] + (86400 * i), "%a %b %d %Y ", tz = "America/New_York")), format.POSIXct(my_table$time_start[[recipient]], "%I:%M %p", tz = "America/New_York")), "%a %b %d %Y %I:%M %p")
        break
      }

      #If no weekdays are chosen to send a survey, then set remaining message quantity to 0, as there is no day to send a survey.
      else if(i == 7) {
        my_table$quantity[[recipient]] <- 0
      }
    }
  }
  else {
    my_table$scheduled_sms[[recipient]] <- as.POSIXct(Sys.time() + (my_table$frequency[[recipient]] * 60))
  }

  return(my_table)
}

GetWeekNumber <- function(date) {
  if(is.na(strptime(date, format = "%Y-%m-%d"))) {
    date <- "2017-01-01"
  }
  
  return(
    floor(
      length(
        seq(
          from=strptime("2017-01-01", format = "%Y-%m-%d"), 
          to=strptime(date, format = "%Y-%m-%d"), 
          by="days"
        )
      )/7
    )
  )
}

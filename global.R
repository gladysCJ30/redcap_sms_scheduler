#These are global functions

options("stringsAsFactors" = FALSE, shiny.trace = T)

source("configuration.R")

##
# Gets the next available time to schedule a sms message.
# INPUT: the schedule dataframe, the row id
##
GetNextScheduleTime <- function(my_table, recipient) {
  #If the next message wont fit within the time range today OR today is not on the list of weekdays to survey:
  
  time_difference <- as.difftime(c(format(my_table[recipient,]$scheduled_sms, "%I:%M %p", tz = "America/New_York"), format(my_table[recipient,]$time_end, "%I:%M %p", tz = "America/New_York")), format = "%I:%M %p", units = "secs")
  scheduled_time <- time_difference[[1]] + (my_table[recipient,]$frequency * 60)
  ending_time <- time_difference[[2]]
  
  
  if( scheduled_time > ending_time || length(grep(format(my_table[recipient,]$scheduled_sms + (my_table[recipient,]$frequency * 60), "%A", tz = "America/New_York"), my_table[recipient,]$days)) == 0 ) {

    #Find the next weekday that a message should be sent
    #86400 is the number of seconds in a day
    for(i in 1:7) {
      if(length(grep(format(my_table[recipient,]$scheduled_sms + (86400 * i), "%A", tz = "America/New_York"), my_table[recipient,]$days)) == 1) {
        my_table[recipient,]$scheduled_sms <- strptime(paste0(as.character(format(my_table[recipient,]$scheduled_sms + (86400 * i), "%a %b %d %Y ", tz = "America/New_York")), format.POSIXct(my_table[recipient,]$time_start, "%I:%M %p", tz = "America/New_York")), "%a %b %d %Y %I:%M %p")
        break
      }

      #If no weekdays are chosen to send a survey, then set remaining message quantity to 0, as there is no day to send a survey.
      else if(i == 7) {
        my_table[recipient,]$quantity <- 0
      }
    }
  }
  else {
    my_table[recipient,]$scheduled_sms <- as.POSIXct(Sys.time() + (my_table[recipient,]$frequency * 60))
  }

  return(my_table[recipient,])
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

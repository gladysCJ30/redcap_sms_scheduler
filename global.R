#These are global functions

source("configuration.R")

##
# Gets the next available time to schedule a sms message.
# INPUT: the schedule dataframe, the row id
##
GetNextScheduleTime <- function(my_table, recipient) {
  #If the next message wont fit within the time range today OR today is not on the list of weekdays to survey:
  if(
    format(my_table[recipient,]$scheduled_sms + (my_table[recipient,]$frequency * 60), "%I:%M %p", tz = "EST")
    > my_table[recipient,]$time_end
    ||
    length(grep(format(my_table[recipient,]$scheduled_sms + (my_table[recipient,]$frequency * 60), "%A", tz = "EST"), my_table[recipient,]$days)) == 0
  ) {

    #Find the next weekday that a message should be sent
    for(i in 1:7) {
      if(length(grep(format(my_table[recipient,]$scheduled_sms + (86400 * i), "%A", tz = "EST"), my_table[recipient,]$days)) == 1) {
        my_table[recipient,]$scheduled_sms <- strptime(paste0(as.character(format(my_table[recipient,]$scheduled_sms + (86400 * i), "%a %b %d %Y ", tz = "EST")), my_table[recipient,]$time_start), "%a %b %d %Y %I:%M %p", tz = "EST")
        break
      }

      #If no weekdays are chosen to send a survey, then set remaining message quantity to 0, as there is no day to send a survey.
      else if(i == 7) {
        my_table[recipient,]$quantity <- 0
      }
    }
  }
  else {
    my_table[recipient,]$scheduled_sms <- as.POSIXct(Sys.time() + (my_table[recipient,]$frequency * 60), tz = "EST")
  }

  return(my_table[recipient,])
}

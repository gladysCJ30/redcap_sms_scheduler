#These are global functions

options("stringsAsFactors" = FALSE, shiny.trace = T)

source("configuration.R")

##
# Gets the next available time to schedule a sms message.
# INPUT: the schedule dataframe, the row id
##
GetNextScheduleTime <- function(my_table, recipient) {
  #If the next message wont fit within the time range today OR today is not on the list of weekdays to survey:
  
  scheduled_time <<- NULL
  temp_scheduled_time <<- NULL
  
  lapply(1:length(my_table$time_start[[recipient]]), function(time_i) {
    
    diff_times <- as.difftime(
      c(
        as.character(
          format(
            my_table$scheduled_sms[[recipient]], 
            format = "%I:%M %p", 
            tz = "America/New_York"
          )
        ),
        as.character(
          format(
            my_table$time_start[[recipient]][[paste0("time_start_", time_i)]], 
            format = "%I:%M %p", 
            tz = "America/New_York"
          )
        ),
        as.character(
          format(
            my_table$time_end[[recipient]][[paste0("time_end_", time_i)]], 
            format = "%I:%M %p", 
            tz = "America/New_York"
          )
        )
      ), 
      format = "%I:%M %p", 
      units = "secs"
    )
    
    currently_scheduled_time <- diff_times[[1]] + (my_table$frequency[[recipient]] * 60)
    start_time <- diff_times[[2]]
    end_time <- diff_times[[3]]
    
    if( currently_scheduled_time > end_time || length(grep(format(my_table$scheduled_sms[[recipient]] + (my_table$frequency[[recipient]] * 60), "%A", tz = "America/New_York"), my_table$days[[recipient]])) == 0 ) {

      #Find the next weekday that a message should be sent
      #86400 is the number of seconds in a day
      for(i in 1:7) {

        if(length(grep(format(my_table$scheduled_sms[[recipient]] + (86400 * i), "%A", tz = "America/New_York"), my_table$days[[recipient]])) == 1) {

          temp_scheduled_time <<- strptime(paste0(as.character(format(my_table$scheduled_sms[[recipient]] + (86400 * i), format = "%a %b %d %Y ", tz = "America/New_York")), format(my_table$time_start[[recipient]][[paste0("time_start_", time_i)]], format = "%I:%M %p", tz = "America/New_York")), format = "%a %b %d %Y %I:%M %p")
          break
        }
        
        #If no weekdays are chosen to send a survey, then set remaining message quantity to 0, as there is no day to send a survey.
        else if(i == 7) {
          
          my_table$quantity[[recipient]] <- 0
        }
      }
    }
    
    else {
      
      #Send the next message on the same day that is scheduled, at the correct starting time
      if(currently_scheduled_time <= start_time) {

        temp_scheduled_time <<- strptime(paste0(as.character(format(my_table$scheduled_sms[[recipient]], format = "%a %b %d %Y ", tz = "America/New_York")), format(my_table$time_start[[recipient]][[paste0("time_start_", time_i)]], format = "%I:%M %p", tz = "America/New_York")), format = "%a %b %d %Y %I:%M %p")
      }
      
      else {
        temp_scheduled_time <<- as.POSIXlt(Sys.time() + (my_table$frequency[[recipient]] * 60))
      }
    }
    
    if(!is.null(temp_scheduled_time)) {
      
      if(is.null(scheduled_time)) {
        scheduled_time <<- temp_scheduled_time
      }
      
      else if(temp_scheduled_time < scheduled_time) {
        scheduled_time <<- temp_scheduled_time
      }
      
    }
    
  })
  
  my_table$scheduled_sms[[recipient]] <- scheduled_time
  
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

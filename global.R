#These are global functions

options("stringsAsFactors" = FALSE, shiny.trace = T)

source("configuration.R")

if(.Platform$OS.type == "windows") {
  if(file_test("-f", "data_table.rds")) {
    #readRDS("sms_data_table.rds")
  }
} else {
  if(file_test("-f", "~/sms_data_table.rds")) {
    #readRDS("~/sms_data_table.rds")
  }
}

##
# Gets the next available time to schedule a sms message.
# INPUT: the schedule dataframe, the row id
##
GetNextScheduleTime <- function(my_table, recipient) {
  #If the next message wont fit within the time range today OR today is not on the list of weekdays to survey:

  scheduled_time <- NULL
  
  for(time_i in 1:length(my_table$time_start[[recipient]])) {
    
    temp_scheduled_time <- NULL
    
    diff_times <- as.difftime(
      c(
        as.character(
          format(
            #my_table$scheduled_sms[[recipient]],
            Sys.time(),
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
    
    if( currently_scheduled_time > end_time || length(grep(format(my_table$scheduled_sms[[recipient]] + (my_table$frequency[[recipient]] * 60), "%A", tz = "America/New_York"), my_table$days[[recipient]][[paste0("weekdays_", time_i)]])) == 0 ) {

      #Find the next weekday that a message should be sent
      #86400 is the number of seconds in a day
      for(i in 1:7) {

        if(is.null(temp_scheduled_time) && length(grep(format(my_table$scheduled_sms[[recipient]] + (86400 * i), "%A", tz = "America/New_York"), my_table$days[[recipient]][[paste0("weekdays_", time_i)]])) == 1) {

          temp_scheduled_time <- strptime(paste0(as.character(format(my_table$scheduled_sms[[recipient]] + (86400 * i), format = "%a %b %d %Y ", tz = "America/New_York")), format(my_table$time_start[[recipient]][[paste0("time_start_", time_i)]], format = "%I:%M %p", tz = "America/New_York")), format = "%a %b %d %Y %I:%M %p")
        }
      }
    }
    
    else {
      
      #Send the next message on the same day that is scheduled, at the correct starting time
      if(diff_times[[1]] <= start_time) {

        temp_scheduled_time <- strptime(paste0(as.character(format(my_table$scheduled_sms[[recipient]], format = "%a %b %d %Y ", tz = "America/New_York")), format(my_table$time_start[[recipient]][[paste0("time_start_", time_i)]], format = "%I:%M %p", tz = "America/New_York")), format = "%a %b %d %Y %I:%M %p")
      }
      
      else {
        temp_scheduled_time <- as.POSIXlt(Sys.time() + (my_table$frequency[[recipient]] * 60))
      }
    }
    
    if(!is.null(temp_scheduled_time)) {
      
      if(is.null(scheduled_time)) {
        scheduled_time <- temp_scheduled_time
      }
      
      else if(temp_scheduled_time < scheduled_time) {
        scheduled_time <- temp_scheduled_time
      }
    }
    
    else {
      my_table$quantity[[recipient]] <- 0
    }
  }
  
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
          # start on a Thursday
          from=strptime("2017-01-05", format = "%Y-%m-%d"), 
          to=strptime(date, format = "%Y-%m-%d"), 
          by="days"
        )
      )/7
    )
  )
}

###############################################################################
# Author:   Alejandro Camacho
# Info:     FIU - CCF
# Date:     March 2017
# Version:  1.0
# Used:
#   *R version 3.2.3 (2015-12-10)
#   *Library: RCurl version 1.95-4.8
#
# Sources:
# - Code:
#   * https://github.com/graywh/redcap
#
# - Explanation:
#   * http://biostat.mc.vanderbilt.edu/wiki/pub/Main/JoAnnAlvarez/api.pdf
#   * https://redcapdev.fiu.edu/api/help/index.php?content=exp_records
#
#
###############################################################################

instruments <- c("lscb_parent", "bcb_parent", "ccb_parent")
events <- c("week_1_arm_1", "week_2_arm_1", "week_3_arm_1")

# Load libraries
library(RCurl)

# FUNCTION: redcapExportRecords (get the data from REDCap)
#
# Parameters:
#   - file_name:    file name to save the data (with or without complete path)
#   - api_url:      URL to the API (e.g. https://redcap.fiu.edu/api/)
#   - api_token:    the API token specific to your REDCap project and username (each token is unique to each user for each project)
#   - content:      record
#   - format:       csv, json, xml [default]
#   - type:         flat [default] - output as one record per row
#

redcapExportRecords <- function(api_url, api_token, instrument, event) {
  
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  
  mydata <- read.csv(
    text=postForm(
      # Redcap API required
      uri=api_url
      , token=api_token
      , content='participantList'
      , format='csv'
      , type='flat'
      , instrument=instrument
      # Redcap API optional
      , event=event#NULL
      # RCurl options
      ,.opts=curlOptions(ssl.verifyhost=2)
    )
    ,stringsAsFactors=FALSE
    ,na.strings='')
  
  return(mydata)
  #write.csv(mydata, file = file_name)
  
}

redcapExportReport <- function(api_url, api_token, file_name) {
  
  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }
  
  text <- postForm(
    # Redcap API required
    uri=api_url
    , token=api_token
    , content='report'
    , format='csv'
    , report_id= 4
    # RCurl options
    ,.opts=curlOptions(ssl.verifyhost=2)
  )
  
  mydata <- read.csv(
    text = text,
    stringsAsFactors=FALSE,
    na.strings=''
  )

  write.csv(mydata, file = file_name)
  
}

# Config: URL
api_url <- 'https://redcapdev.fiu.edu/api/'

# Config: Tokens for each database
api_token <- "09C6537FF5EAFE92BD74E1AA1B9BEF67"#"F66E35FDC22C3BE97BD3C5FCE0F5201E" #CCF Programs Database

# Set the working directory
#setwd("D:/dev/CCF/redcap_sms_scheduler/")
#setwd("D:/CCF BI Projects/SSIS Projects/SSIS - ETL Clinic DW Project")

#file_name <- paste(getwd(), paste("ccf_programs_",gsub("[[:punct:][:space:]]","",Sys.time()),".csv",sep=""), sep="/")
#setwd("~/")
file_name <- "~/ccf_programs_participant_list.csv"

# Function calls
#redcapExportRecords(file_name,api_url,api_token,'csv','flat')

redcapExportReport(api_url,api_token, "~/phone_number_list.csv")

contact_list <<- read.csv("~/phone_number_list.csv", stringsAsFactors = F)

#participant_list <<- read.csv("ccf_programs_participant_list.csv", stringsAsFactors = F)

#participant_list <<- participant_list[!is.na(participant_list$phone) & !is.na(participant_list$survey_link),]

randomization <<- read.csv(
  file = "~/random.csv",
  stringsAsFactors=FALSE,
  na.strings=''
)


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

redcapExportRecords <- function(file_name, api_url, api_token, format, type) {

  if (!require('RCurl')) {
    stop('RCurl is not installed')
  }

  mydata <- read.csv(
              text=postForm(
                          # Redcap API required
                            uri=api_url
                          , token=api_token
                          , content='participantList'
                          , format=format
                          , type=type
                          , instrument="i1"
                          # Redcap API optional
                          , events=NULL
                          # RCurl options
                          ,.opts=curlOptions(ssl.verifyhost=2)
                   )
              ,stringsAsFactors=FALSE
              ,na.strings='')

  write.csv(mydata, file = file_name)

}

# Config: URL
api_url <- 'https://redcap.fiu.edu/api/'

# Config: Tokens for each database
api_token <- "F66E35FDC22C3BE97BD3C5FCE0F5201E" #CCF Programs Database

# Set the working directory
setwd("D:/dev/CCF/redcap_sms_scheduler/")
#setwd("D:/CCF BI Projects/SSIS Projects/SSIS - ETL Clinic DW Project")

#file_name <- paste(getwd(), paste("ccf_programs_",gsub("[[:punct:][:space:]]","",Sys.time()),".csv",sep=""), sep="/")
#setwd("~/")
file_name <- paste(getwd(), paste("ccf_programs_participant_list.csv",sep=""), sep="/")

# Function calls
redcapExportRecords(file_name,api_url,api_token,'csv','flat')

participant_list <<- read.csv("ccf_programs_participant_list.csv", stringsAsFactors = F)

participant_list <<- participant_list[!is.na(participant_list$phone) & !is.na(participant_list$survey_link),]




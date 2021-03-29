source("ona_package.R") ## this is a copy paste of the onaR package source file

library(tidyverse)
library(splitstackshape)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(future)

## targets using the bens listing file
target <- readxl::read_excel("R5 bens lookup file_updated.xlsx")%>%
  group_by(Member_id, District_Name,Community_Name)%>%
  dplyr::summarise(Target = n())

## variables to be used -- just for initializing the dashboard
varshown <- c("today", "ONA_username","e.Interview_criteria","Which.is.the.agency.supporting.the.beneficiary.","District_Name","Community_Name","start", "end","_duration")

# Initialize a empty dataframe
listData <- function(){
  d <- data.frame(matrix(ncol=length(varshown), nrow = 0))
  colnames(d)<-varshown
  names(d)[match("Which.is.the.agency.supporting.the.beneficiary.",names(d))] <- "Member_id"
  return(d)
}

## function to format the data 
prepare_data <- function(data){
  
  data$today <- as.character(as.Date(data$end, tz = "GMT"))
  data$end <- as.POSIXct(data$end)
  data$start <- as.POSIXct(data$start)
  data$interviewDuration <- difftime(data$end, data$start, units='mins')
  data$long <- data$interviewDuration>120
  data$short <- between(data$interviewDuration, 0, 12)
  
  
  names(data)[match("a.Member_id",names(data))] <- "Member_id"
  names(data)[match("e.Interview_criteria",names(data))] <- "Interview_criteria"
  names(data)[match("d.consensus",names(data))] <- "consent"
  names(data)[match("consensus",names(data))] <- "consent"
  names(data) <- str_remove_all(names(data),"g.HH_confirmation.")
  names(data) <- str_remove_all(names(data),"q.q2.COVID.19.")

  data <- data[-1,]
  
  data <- data%>%
    mutate(Interview_criteria = ifelse(is.na(Interview_criteria) | 
                                         Interview_criteria ==  "If most information is matching and respondent confirms that you are speaking with the selected household, please proceed with the survey","1","0"))
  return(data)
}

# Retrieve data from ona
get_data <- function(login, password ){
  
  sn_r5 <- tryCatch(onaDownload("Condensed_PDM_2020_Survey_R5_L", 
                                "BRCiS",login,password, keepGroupNames=FALSE), 
                    error=function(e){message("can't access data")})
  
    return(prepare_data(sn_r5))
  
}

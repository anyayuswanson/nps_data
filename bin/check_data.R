#Anya Yu- Swanson
#NOAA CINMS
#CINMS Visitor Use

#Goal: To review national park visitor data download 
#------------------
library(tidyverse) #load library each time so code can run independently 

#-------------------
#remove data from environment
remove(list=ls())      # clean slate, do each time you start new script
setwd("/Users/anya/Documents/r_projects/nps_data")      #set up working directory to nps_data parent folder where all related files are stored

#troubleshooting error message
getwd()    #check and display current working directory 
list.files()      #check file names to see why getting error

# read in file
d1 <- read_csv("./data/nps_chis_visitors.csv")%>%    #d1=data frame, we are assigning csv file to R data frame 
  mutate(date=make_date(year=year, month=month))%>% #creates new variable/column named date that combines month and year, automatically sets it to the first of the month becuase no exact date
  glimpse() #lets you see tibble in directory 
d1
unique(d1$stat_desc) #give list of unique texts in column 

#output 
#[1] "Total Recreation Visits"                    "Total Non-Recreation Visits"               
#[3] "Total Visits (Recreation + Non-Recreation)" "Total Recreation Visitor Hours"            
#[5] "Total Hours (Recreation + Non-Recreation)"  "Concessioner Lodging Overnight Stays"      
#[7] "Concessioner Campground Overnight Stays"    "Tent Camping Overnight Stays (NPS)"        
#[9] "Total Recreation Overnight Stays"           "Total Tent and RV Camping Stays"           
#[11] "Backcountry Camping Overnight Stays"        "Miscellaneous Overnight Stays"             
#[13] "Non-Recreation Overnight Stays"             "Total Overnight Stays"                     
#[15] "Total Non-Recreation Visitor Hours"        

range(d1$year) #gives the range of numbers in a column

d2<-d1%>%  #subset of d1 that is one of the variables
  filter(statistic=="TRV")%>%
  glimpse() 
unique(d2$stat_desc) #gives unique text in d2 tibble


#Anya Yu- Swanson
#NOAA CINMS
#CINMS Visitor Use

#Goal: To review national park visitor data download and create data sets for graphing
#add this section to every script:  
#------------------
library(tidyverse) #load library each time so code can run independently 
#-----------------
#remove data from environment
remove(list=ls())      # clean slate, do each time you start new script
setwd("/Users/anya/Documents/r_projects/nps_data")      #set up working directory to nps_data parent folder where all related files are stored

#-------------------
#troubleshooting error message
getwd()    #check and display current working directory 
list.files()      #check file names to see why getting error

# Load files----------- 
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

# check for missing months-----
missing_months_detail <- d1 %>%
  distinct(year, statistic, month) %>%
  group_by(year, statistic) %>%
  summarise(
    missing_months = paste(
      setdiff(1:12, month),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  filter(missing_months != "")

#December 2005 (month 12) is missing data 

# -------------------------
# Annual total visits dataset filtering for recreation and non-recreation visits and starting in 1980 ---------
# Fig 1, 2
# -----------------------

total_annual_use <- d1 %>%
  filter(
    statistic %in% c("TRV", "TNRV"),
    year >= 1980
  ) %>%
  group_by(year, statistic) %>%
  summarise(
    annual_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = annual_value
  ) %>%
  mutate(
    total_visits = TRV + TNRV
  )

#create csv file
write_csv(total_annual_use, "./results/total_annual_use.csv")

#----------------------
# Seasonal monthly averages dataset ----- 
# Fig 2
#----------------------
#combine non-recreational and recreational visits 
seasonal_means <- d1 %>%
  filter(
    statistic %in% c("TRV", "TNRV"),
    year >= 1980
  ) %>%
  
  # Put TRV and TNRV into separate columns
  select(year, month, statistic, value) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  
  # Create combined monthly total visits
  mutate(
    total_visits = TRV + TNRV,
    
    season = case_when(
      month %in% c(10, 11, 12, 1, 2, 3) ~ "Winter",
      month %in% c(4, 5, 6, 7, 8, 9) ~ "Summer"
    ),
    
    season_year = case_when(
      month %in% c(10, 11, 12) ~ year + 1,
      TRUE ~ year
    )
  ) %>%
  
  group_by(season_year, season) %>%
  
  summarise(
    seasonal_total = sum(total_visits, na.rm = TRUE),
    monthly_mean_visits = mean(total_visits, na.rm = TRUE),
    sd_visits = sd(total_visits, na.rm = TRUE),
    n = sum(!is.na(total_visits)),
    se_visits = sd_visits / sqrt(n),
    .groups = "drop"
  ) %>%
  
  # Remove incomplete seasons at beginning and end of record
  filter(
    !(season_year == 1980 & season == "Winter"),
    !(season_year == 2026 & season == "Winter")
  )

# Create csv file
write_csv(
  seasonal_means,
  "./results/seasonal_means.csv"
)

#notes-----
#winter 2006 n=5 because data from December 2005 is missing from original file
#Winter 1980 and Winter 2026 excluded because only have 3 months of data 



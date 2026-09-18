#1. create repository on github
#2. create new version control project in R studio and like it to github repository by pasting link, put in correct folder and give name
#3. save scripts to bin folder, where all of the scripts will be saved

#Basic Header to include in all scripts:
  #name
  #organization
  #topic

  #goal of project
#---------------------------

#Anya Yu-Swanson
#NOAA CINMS
#CINMS Visitor Use

#Goal: Driver file to send code for visitor use project 
#------------
library(tidyverse) #load library each time so code can run independently 

#-------------------
#remove data from environment
remove(list=ls())      # clean slate, do each time you start new script
#set up working directory so R will automatically look for files to read and save files to this folder

setwd("/Users/anya/Documents/r_projects")     #set up driver to parent folder with all r projects
#found the navigation string by right clicking on folder in Finder and pressing Get Info

# ALL VISITORS - ANNUAL AND SEASONAL GRAPHS -----------------------

#check data and modify data sets for graphing------------------
source("~/Documents/r_projects/nps_data/bin/check_data.R")    #right click on open script tab and "copy path" and paste here for source
#input: ./data/nps_chis_visitors.csv
#output: ./results/total_annual_use.csv     #data for fig 1,3
#       ./results/seasonal_means.csv        #data for fig 2, se = standard error, sd = standard deviation


#Graphs for Visitor Use trends
source( "~/Documents/r_projects/nps_data/final_graphs_annual_use.R")
#input:  ./results/total_annual_use.csv
#output: ./doc/fig_1a_total_annual_use.png      #total annual visits (recreational + non-recreational) since 1980 with labels 
#       ./doc/fig_1b_total_annual_use.png       #add label for when started counting boat data
#       ./doc/fig_3_visit_types.png             #total annual recreational and non-recreational visits
#input: ./results/seasonal_means.csv            #seasonal monthly means and seasonal totals
#output:./doc/fig_2a_seasonal_use.png           #seasonal monthly visitor means (recreational + non-recreational)
#       ./doc/fig_2b_seasonal_use.png           #seasonal monthly means with labels 
#       ./doc/fig_2c_seasonal_totals.png        #total seasonal visitors 


# ISLAND VISITORS - ANNUAL, MONTHLY AND SEASONAL GRAPHS GRAPHS ------------------

#Review national park statistics island specific data and create data sets for graphing
source("~/Documents/r_projects/nps_data/bin/check_island_data.R")
#input: ./data/CHIS_island_visitor_use_monthly.csv
#output: ./results/annual_island_totals.csv   #fig 4
#       ./results/average_annual_island_use_2020_present.csv  #fig 7 
#       ./results/monthly_island_data.csv  #fig 5
#       ./results/island_seasonal_summary.csv  #fig 6

#Graphs showing total island visitors (visitors ashore + boats) by island
source("~/Documents/r_projects/nps_data/bin/final_graphs_island_use.R")
#ANNUAL VISITS BY ISLAND------
#input: ./results/annual_island_totals.csv
#output: ./doc/fig_4a_annual_onshore_use.png
#       ./doc/fig_4b_annual_boat_use.png
#VISIT TYPES BY ISLAND --------
#input: ./results/average_annual_island_use_2020_present.csv
#output: fig_7a_island_average_use_type.png
#         fig_7b_average_annual_use_faceted.png
#MONTHLY VISITS BY ISLAND-------
#input: ./results/monthly_island_data.csv
#output: ./doc/fig_5a_monthly_onshore_use.png
#       ./doc/fig_5b_monthly_boat_use.png
#SEASONAL VISITS BY ISLAND --------
#input: ./results/island_seasonal_summary.csv
#output: ./doc/fig_6a_seasonal_means_onshore_use.png
#       ./doc/fig_6b_seasonal_means_boat_use.png"
#       ./doc/fig_6c_seasonal_totals_onshore_use.png
#       ./doc/fig_6d_seasonal_totals_boat_use.png




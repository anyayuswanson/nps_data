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
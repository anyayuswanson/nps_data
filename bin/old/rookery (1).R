#rookery data analysis

library(ggplot2)
library(ggpubr)
library(here)
library(tidyverse)

# read in data
rookery_WP_format <- read.csv(here("data", "bird_rookery.csv")) %>% 
  pivot_wider(names_from = Species,
              values_from = Nests_Total)

# define metadata block

rookery_metadata <- c(
  "Sanctuary name: MBNMS",
  paste("Date created:", Sys.Date()),
  "Source program: ESNERR",
  "Source data URL: NA",
  "Source data contact: Kerstin Wasson (kerstin.wasson@gmail.com)",
  "Data formatted by: Grace Kumaishi (grace.kumaishi@noaa.gov)",
  "Indicator: nests",
  "Metric: counts",
  "Time format: YYYY",
  "Additional info: NA"
)

# Write the file out using a connection

rookery_file_conn <- file(here("data", "rookery_metadata.csv"), open = "w") 

writeLines(rookery_metadata, rookery_file_conn)

# save as csv

write.csv(rookery_WP_format, 
          rookery_file_conn,
          row.names = FALSE, quote = FALSE)

close(rookery_file_conn)

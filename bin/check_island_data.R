#Anya Yu-Swanson
#NOAA CINMS 
#CINMS Visitor Use

#Goal: To review national park statistics island specific data and create data sets for graphing

#load libraries
library(tidyverse)
#-----------------
remove(list=ls())      
setwd("/Users/anya/Documents/r_projects/nps_data")  

#Load files----------------
d1 <- read_csv("data/CHIS_island_visitor_use_monthly.csv")
# Check data
glimpse(d1)
names(d1)

# Check for missing months-----
missing_months <- d1 %>%
  group_by(year, island, measure) %>%
  summarise(
    n_months = n_distinct(month),
    months_present = paste(sort(unique(month)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_months < 12)

#no months missing

#-----------------------------------------------
#Annual total visits by island from original monthly dataset----------
#use for figure 4 (d1) 
#------------------------------------------------
#each measure in its own column (wide format)
# Create annual totals for each island
annual_totals <- d1 %>%
  group_by(year, island, measure) %>%
  summarise(
    annual_total = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = annual_total
  )

# Check result
glimpse(annual_totals)

#save dataset
write_csv(annual_totals,"./results/annual_island_totals.csv")

#---------------------------------------
# Average annual island use (2020-present)  --------
# use for figure 7 (d2)
#---------------------------------------

average_annual_use <- annual_totals %>%
  
  # Keep only 2020-present
  filter(year >= 2020) %>%
  
  # Convert use types into one column temporarily
  pivot_longer(
    cols = c(
      `Rec Visitors Ashore`,
      `Rec Visitors on Boats`,
      `Overnight Stays`,
      `Rec Overnight Boats`
    ),
    names_to = "use_type",
    values_to = "annual_total"
  ) %>%
  
  # Calculate summary statistics for each island and use type
  group_by(island, use_type) %>%
  summarise(
    mean_annual = mean(annual_total, na.rm = TRUE),
    sd_annual = sd(annual_total, na.rm = TRUE),
    n = sum(!is.na(annual_total)),
    se_annual = sd_annual / sqrt(n),
    .groups = "drop"
  )

# View/check dataset
glimpse(average_annual_use)

# Save dataset
write_csv(
  average_annual_use,
  "./results/average_annual_island_use_2020_present.csv"
)

#---------------------------------------
# Monthly island use dataset-------------
# use for figure 5 (d3)
#---------------------------------------

monthly_island_data <- d1 %>%
  select(
    year,
    month,
    island,
    measure,
    monthly_value
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = monthly_value
  ) %>%
  mutate(
    date = make_date(
      year = year,
      month = month,
      day = 1
    )
  )

# Check dataset
glimpse(monthly_island_data)

# Save dataset
write_csv(
  monthly_island_data,
  "./results/monthly_island_data.csv"
)

#---------------------------------------
# seasonal island data (monthly means & totals)-------
# Winter = Oct-Mar
# Summer = Apr-Sep
# Fig 6a-6b (seasonal monthly means), Fig 6c-6d (seasonal totals) (d4)
#---------------------------------------

island_seasonal_summary <- d1 %>%
  
  mutate(
    season = case_when(
      month %in% c(10, 11, 12, 1, 2, 3) ~ "Winter",
      month %in% c(4, 5, 6, 7, 8, 9) ~ "Summer"
    ),
    
    # Oct-Dec belong to the following winter year
    season_year = case_when(
      month %in% c(10, 11, 12) ~ year + 1,
      TRUE ~ year
    )
  ) %>%
  
  group_by(
    season_year,
    season,
    island,
    measure
  ) %>%
  
  summarise(
    seasonal_total = sum(monthly_value),
    mean_monthly = mean(monthly_value),
    sd_monthly = sd(monthly_value),
    n = n(),
    se_monthly = sd_monthly / sqrt(n),
    .groups = "drop"
  ) %>%

# Remove incomplete winter seasons at the beginning/end of data collection
filter(
  !(season_year == 1995 &
      season == "Winter" &
      measure %in% c(
        "Rec Visitors Ashore",
        "Overnight Stays"
      )),
  
  !(season_year == 2019 &
      season == "Winter" &
      measure %in% c(
        "Rec Visitors on Boats",
        "Rec Overnight Boats"
      )),
  
  !(season_year == 2026 &
      season == "Winter")
)

# Check dataset
glimpse(island_seasonal_summary)

# Save dataset
write_csv(
  island_seasonal_summary,
  "./results/island_seasonal_summary.csv"
)

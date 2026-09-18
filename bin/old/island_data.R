#Anya Yu- Swanson
#NOAA CINMS
#CINMS Visitor Use Island Report

#Goal: To review CHIS Island Report data
#------------------
library(tidyverse) 
remove(list=ls())      # clean slate, do each time you start new script
setwd("/Users/anya/Documents/r_projects/nps_data")      #set up working directory to nps_data parent folder where all related files are stored

island_report <- read_csv("data/CHIS_island_visitor_use_monthly.csv")
View(island_report)
glimpse(island_report)
list.files("data")

# Load park-wide visitor use data
park_report <- read_csv("data/nps_chis_visitors.csv")
names(park_report)
names(island_report)

#check categories in each dataset 
unique(park_report$statistic)
unique(island_report$measure)


#---------------------------------------------------------
Compare island report with park-wide TRV datasets
#--------------------------------------------------------
#Goal: Is the per island visitor data from the island report sum to get the total recreational visitors from the NPS report?
# Total recreational visitors ashore across all islands by month
island_monthly <- island_report %>%
  filter(measure == "Rec Visitors Ashore") %>%
  group_by(year, month) %>%
  summarise(
    visitors_ashore = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

# Total recreational visits for the park
park_monthly <- park_report %>%
  filter(statistic == "TRV") %>%
  select(year, month, park_TRV = value)

comparison <- left_join(
  park_monthly,
  island_monthly,
  by = c("year", "month")
)

View(comparison)
#-----
#Goal: does rec visitors ashore + rec visitors on boat (Island Report) add up to TRV (nps data)?

island_use_monthly <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Rec Visitors on Boats")) %>%
  group_by(year, month, measure) %>%
  summarise(
    value = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )

comparison2 <- park_monthly %>%
  left_join(island_use_monthly,
            by = c("year", "month")) %>%
  filter(year >= 2019) %>%
  mutate(
    island_total = `Rec Visitors Ashore` +
      `Rec Visitors on Boats`
  )

View(comparison2)

#conclusion: they do not match 
#How are “Rec Visitors Ashore” and “Rec Visitors on Boats” in the CHIS Park YTD island reports calculated? 
#Do these measures include Island Packers passengers, private recreational vessels, charter vessels, and recreational fishing vessels? 
#why doesn't the sum of island-level visitors ashore and visitors on boats equal park-wide TRV?

#rec visitors ashore, overnight stays  have data 1995-2025
#rec visiotrs on boats and overnight boats have data 2019-2025

#totals from vistitor center (from Interpretive Report) and Island totals match (+/- 1) monthly Park Totals for july, oct, march 2025 but should check this for all months

#-------------------------------
#Goal: Create line graphs showing annual vistors over time

#create data set for annual island use
annual_island_use <- island_report %>%
  group_by(year, island, measure) %>%
  summarise(
    annual_value = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

#Recreational visitors ashore
ggplot(
  filter(annual_island_use, measure == "Rec Visitors Ashore"),
  aes(x = year, y = annual_value, color = island)
) +
  geom_line() +
  labs(
    title = "Annual Recreational Visitors Ashore by Island",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Visitors Ashore",
    color = "Island"
  ) +
  theme_minimal()

#overnight stays
ggplot(
  filter(annual_island_use, measure == "Overnight Stays"),
  aes(x = year, y = annual_value, color = island)
) +
  geom_line() +
  labs(
    title = "Annual Overnight Stays by Island",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Overnight Stays",
    color = "Island"
  ) +
  theme_minimal()

#recreational visitors on boats
ggplot(
  filter(annual_island_use, measure == "Rec Visitors on Boats"),
  aes(x = year, y = annual_value, color = island)
) +
  geom_line() +
  labs(
    title = "Annual Recreational Visitors on Boats by Island",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Visitors on Boats",
    color = "Island"
  ) +
  theme_minimal()
    #anacapa has the most

#recreationl overnight boats
ggplot(
  filter(annual_island_use, measure == "Rec Overnight Boats"),
  aes(x = year, y = annual_value, color = island)
) +
  geom_line() +
  labs(
    title = "Annual Recreational Overnight Boats by Island",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Overnight Boats",
    color = "Island"
  ) +
  theme_minimal()

#all 4 annual total graphs together
ggplot(annual_island_use,
       aes(x = year, y = annual_value, color = island)) +
  geom_line() +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Annual Island Visitor Use by Measure",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Annual Total",
    color = "Island"
  ) +
  theme_minimal()

#--------------
#Goal: create graph showing overnight stays, visitors ashore, overnight boats, and visitors on boats over time
 
#make year and month seperate so all months not counted as one point
island_report <- island_report %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

#smooth line LOESS through the year to show overall trend  
ggplot(island_report,
       aes(x = date, y = monthly_value, color = island)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Island Visitor Use by Measure",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Monthly Value",
    color = "Island"
  ) +
  theme_minimal()

#not smoothed, looks chaotic but can see seasonal variaiton  
ggplot(
  island_report,
  aes(x = date, y = monthly_value, color = island)
) +
  geom_line() +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Monthly Island Visitor Use by Measure",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Monthly Value",
    color = "Island"
  ) +
  theme_minimal()

#--------------------------------------
#Goal: create graph showing total island visitors (visitors ashore + boats) by island

total_island_visitors <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Rec Visitors on Boats")) %>%
  group_by(year, island) %>%
  summarise(
    total_visitors = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

#note --- boat visitors only started being included in 2019
ggplot(total_island_visitors,
       aes(x = year, y = total_visitors, color = island)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Annual Recreational Visitors by Island",
    subtitle = "Rec Visitors Ashore + Rec Visitors on Boats",
    x = "Year",
    y = "Annual Visitors",
    color = "Island"
  ) +
  theme_minimal()




#--------------------------------------
#Goal: create graph showing seasonality by island by averaging visitors ashore for each month 
monthly_ashore <- island_report %>%
  filter(measure == "Rec Visitors Ashore") %>%
  group_by(month, island) %>%
  summarise(
    mean_visitors = mean(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(monthly_ashore,
       aes(x = month, y = mean_visitors, color = island)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Seasonal Patterns in Recreational Visitors Ashore",
    subtitle = "Channel Islands National Park",
    x = "Month",
    y = "Mean Monthly Visitors",
    color = "Island"
  ) +
  theme_minimal()

#could be interesting to show how seasonality of visitors on boat is less affected
#show all 4 types of vistor use seasonality
monthly_seasonality <- island_report %>%
  group_by(measure, month, island) %>%
  summarise(
    mean_value = mean(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(monthly_seasonality,
       aes(x = month, y = mean_value, color = island)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ measure, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Seasonal Patterns in Visitor Use by Island",
    subtitle = "Channel Islands National Park",
    x = "Month",
    y = "Average Monthly Value",
    color = "Island"
  ) +
  theme_minimal()


#-------
#Goal: bar graph showing overnight stay rate per island
#one visitor can have multiple overnight stays, so calculate the rate of overnight stays by doing overnight stays/rec visitors ashore x 100 
#uses historical totals since 1995
overnight_comparison <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Overnight Stays")) %>%
  group_by(island, measure) %>%
  summarise(
    total = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = total
  ) %>%
  mutate(
    overnight_stays_per_100 =
      (`Overnight Stays` / `Rec Visitors Ashore`) * 100
  )

ggplot(overnight_comparison,
       aes(x = reorder(island, -overnight_stays_per_100),
           y = overnight_stays_per_100)) +
  geom_col() +
  labs(
    title = "Overnight Stay Intensity by Island",
    subtitle = "Channel Islands National Park",
    x = "Island",
    y = "Overnight Stays per 100 Visitors Ashore"
  ) +
  theme_minimal()

#Goal: line graph showing overnight stay rate per island over time

overnight_annual <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Overnight Stays")) %>%
  group_by(year, island, measure) %>%
  summarise(
    annual_value = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = annual_value
  ) %>%
  mutate(
    overnight_stays_per_100 =
      (`Overnight Stays` / `Rec Visitors Ashore`) * 100
  )
ggplot(overnight_annual,
       aes(x = year,
           y = overnight_stays_per_100,
           color = island)) +
  geom_line() +
  labs(
    title = "Overnight Stay Intensity by Island",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Overnight Stays per 100 Visitors Ashore",
    color = "Island"
  ) +
  theme_minimal()

#----------
#bar graph showing total visitors and proportion of overnight stays per island annual avg
library(patchwork)

island_annual <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Overnight Stays")) %>%
  group_by(year, island, measure) %>%
  summarise(
    annual_total = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

island_summary_annual <- island_annual %>%
  group_by(island, measure) %>%
  summarise(
    average_annual = mean(annual_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = average_annual
  ) %>%
  mutate(
    overnight_per_100 =
      (`Overnight Stays` / `Rec Visitors Ashore`) * 100
  )

p_visitors <- ggplot(
  island_summary_annual,
  aes(x = reorder(island, -`Rec Visitors Ashore`),
      y = `Rec Visitors Ashore`)
) +
  geom_col() +
  labs(
    title = "Average Annual Recreational Visitors Ashore",
    subtitle = "1995–2025",
    x = NULL,
    y = "Average Annual Visitors"
  ) +
  theme_minimal()

p_overnight <- ggplot(
  island_summary_annual,
  aes(x = reorder(island, -`Rec Visitors Ashore`),
      y = overnight_per_100)
) +
  geom_col() +
  labs(
    title = "Overnight Use Relative to Island Visitation",
    subtitle = "1995–2025",
    x = "Island",
    y = "Overnight Stays per 100 Visitors Ashore"
  ) +
  theme_minimal()
p_visitors + p_overnight

#--------
#Goal: stacked bar chart showing island visitor use per island historical average


average_annual_use <- annual_island_use %>%
  group_by(island, measure) %>%
  summarise(
    average_annual = mean(annual_value, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(average_annual_use,
       aes(x = measure,
           y = average_annual,
           fill = island)) +
  geom_col() +
  labs(
    title = "Average Annual Visitor Use by Type and Island",
    subtitle = "Channel Islands National Park",
    x = NULL,
    y = "Average Annual Total",
    fill = "Island"
  ) +
  theme_minimal()


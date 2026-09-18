library(tidyverse)

#check working directory
getwd()
setwd("/Users/anya/Documents/r_projects/nps_data")

# Read data
island_report <- read_csv("data/CHIS_island_visitor_use_monthly.csv")
interpretive_report <- read_csv("data/CHIS_NPS_interpretive_report_monthly.csv")
concessioner_report <- read_csv("data/CHIS_NPS_concessioner_report_monthly.csv")
nps_visitors <- read_csv("data/nps_chis_visitors.csv")

# Monthly visitors to each island
# Rec Visitors Ashore + Rec Visitors on Boats
island_monthly <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Rec Visitors on Boats")) %>%
  group_by(year, month, island) %>%
  summarise(
    island_visitors = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = island,
    values_from = island_visitors,
    names_prefix = "island_",
    values_fill = 0
  )


# Total monthly visitors to all islands
island_totals <- island_report %>%
  filter(measure %in% c("Rec Visitors Ashore",
                        "Rec Visitors on Boats")) %>%
  group_by(year, month) %>%
  summarise(
    total_island_visitors = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )


# Monthly visitors to the two mainland visitor centers
visitor_centers <- interpretive_report %>%
  filter(measure %in% c("Mainland Visitor Center",
                        "Santa Barbara Outdoor VC")) %>%
  select(year, month, measure, monthly_value) %>%
  pivot_wider(
    names_from = measure,
    values_from = monthly_value,
    values_fill = 0
  ) %>%
  rename(
    mainland_visitor_center = `Mainland Visitor Center`,
    santa_barbara_outdoor_vc = `Santa Barbara Outdoor VC`
  ) %>%
  mutate(
    total_visitor_centers =
      mainland_visitor_center + santa_barbara_outdoor_vc
  )


# Original NPS Total Recreation Visits
nps_trv <- nps_visitors %>%
  filter(statistic == "TRV") %>%
  select(year, month, value) %>%
  rename(
    original_NPS_TRV = value
  )

# Monthly total concessioner passengers
concessioner_monthly <- concessioner_report %>%
  filter(
    group_description == "Summary of Total Trips",
    measure %in% c(
      "IPCO Day Trip Passengers (All Units)",
      "CIA Passengers",
      "Truth Aquatic Day Trip Passengers"
    )
  ) %>%
  group_by(year, month) %>%
  summarise(
    concessioner_total = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  )

# Combine all datasets
visitor_comparison <- island_monthly %>%
  full_join(island_totals, by = c("year", "month")) %>%
  full_join(visitor_centers, by = c("year", "month")) %>%
  full_join(concessioner_monthly, by = c("year", "month")) %>%
  full_join(nps_trv, by = c("year", "month")) %>%
  mutate(
    
    # Date column
    date = as.Date(paste(year, month, 1, sep = "-")),
    
    # Islands + visitor centers
    total_islands_and_visitor_centers =
      total_island_visitors + total_visitor_centers,
    
    # Difference without concessioner data
    difference_from_NPS =
      original_NPS_TRV - total_islands_and_visitor_centers,
    
    # Islands + visitor centers + concessioner passengers
    total_with_concessioners =
      total_islands_and_visitor_centers + concessioner_total,
    
    # Difference after adding concessioner passengers
    difference_with_concessioners =
      original_NPS_TRV - total_with_concessioners
  ) %>%
  relocate(date, .after = month) %>%
  arrange(date)


write_csv(
  visitor_comparison,
  "./results/visitor_comparison.csv"
)
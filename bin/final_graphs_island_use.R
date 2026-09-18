#Anya Yu-Swanson
#NOAA CINMS 
#CINMS Visitor Use

#Goal: Create graphs showing visitor use trends by island

#load libraries
library(tidyverse)
library(scales)
#-----------------

#clear environment and set working directory
remove(list=ls())      
setwd("/Users/anya/Documents/r_projects/nps_data")  

#Load files----------------
#d1 is for fig 4
d1 <- read_csv("./results/annual_island_totals.csv") %>%
  glimpse()

range(d1$year)
range(d1$`Rec Visitors Ashore`, na.rm = TRUE)
range(d1$`Rec Visitors on Boats`, na.rm = TRUE)

#d2 is for fig 7
d2 <- read_csv(
  "./results/average_annual_island_use_2020_present.csv"
) %>%
  glimpse()

#d3 is for figure 5
d3 <- read_csv(
  "./results/monthly_island_data.csv"
) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  glimpse()

#d4 is for figure 6
d4 <- read_csv("./results/island_seasonal_summary.csv") %>%
  mutate(
    season = factor(
      season,
      levels = c("Winter","Summer")
    )
  ) %>%
  glimpse()
#---------------------------------------
# Figure 4: Annual totals by Island -----------
#---------------------------------------
# Set axis ranges
# First year with visitors ashore data
ashore_start <- min(
  d1$year[!is.na(d1$`Rec Visitors Ashore`)],
  na.rm = TRUE
)

# First year with visitors on boats data
boat_start <- min(
  d1$year[!is.na(d1$`Rec Visitors on Boats`)],
  na.rm = TRUE
)

# Use the same y-axis maximum for both graphs
shared_y_max <- max(
  d1$`Rec Visitors Ashore`,
  d1$`Rec Visitors on Boats`,
  na.rm = TRUE
)

#---------------------------------------
# Figure 4a: Annual Onshore Recreational Use by Island-----------

fig_4a_annual_onshore_use <- ggplot(
  d1,
  aes(
    x = year,
    y = `Rec Visitors Ashore`,
    color = island,
    group = island
  )
) +
  
  geom_line(linewidth = 1) +
  
  # Y-axis
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, shared_y_max),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(
      floor(ashore_start / 5) * 5,
      ceiling(max(d1$year, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    limits = c(
      ashore_start,
      max(d1$year, na.rm = TRUE)
    ),
    expand = c(0, 0)
  ) +
  
  # Labels
  labs(
    title = "Annual Recreational Use by Island",
    subtitle = "Channel Islands National Park (1995-Present)",
    x = "Year",
    y = "Total Annual Recreational Visits Ashore",
    color = "Island"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    
    axis.title = element_text(size = 12),
    
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    axis.text.y = element_text(size = 10),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks.length = unit(0.15, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(size = 10)
  )

fig_4a_annual_onshore_use


#---------------------------------------
# Figure 4b: Annual Recreational Use on Boats by Island-------

fig_4b_annual_boat_use <- ggplot(
  d1,
  aes(
    x = year,
    y = `Rec Visitors on Boats`,
    color = island,
    group = island
  )
) +
  
  geom_line(linewidth = 1) +
  
  # Y-axis
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, shared_y_max),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(
      boat_start,
      max(d1$year, na.rm = TRUE),
      by = 1
    ),
    limits = c(
      boat_start,
      max(d1$year, na.rm = TRUE)
    ),
    expand = c(0, 0)
  ) +
  
  # Labels
  labs(
    title = "Annual Recreational Boat use by Island",
    subtitle = "Channel Islands National Park (2019-Present)",
    x = "Year",
    y = "Total Annual Recreational Visits on Boats",
    color = "Island"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    
    axis.title = element_text(size = 12),
    
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    axis.text.y = element_text(size = 10),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    
    axis.ticks.length = unit(0.15, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(size = 10)
  )

fig_4b_annual_boat_use


#---------------------------------------
# Save figures-------

ggsave(
  "./doc/fig_4a_annual_onshore_use.png",
  plot = fig_4a_annual_onshore_use,
  width = 10,
  height = 5.62,
  dpi = 300
)

ggsave(
  "./doc/fig_4b_annual_boat_use.png",
  plot = fig_4b_annual_boat_use,
  width = 10,
  height = 5.62,
  dpi = 300
)

#---------------------
#Fig 7: Average Annual Use by type (2020-present)---------
#--------------------
#Fig 7a: all together -----
fig_7a_island_average_use_type <- ggplot(
  d2,
  aes(
    x = use_type,
    y = mean_annual,
    fill = island
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_annual - se_annual,
      ymax = mean_annual + se_annual
    ),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  scale_x_discrete(
    labels = c(
      "Rec Visits Ashore" = "Visitors\nAshore",
      "Rec Visits on Boats" = "Visitors\non Boats",
      "Overnight Stays" = "Overnight\nStays",
      "Rec Overnight Boats" = "Overnight\nBoats"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    title = "Average Annual Recreational Use by Island and Use Type (2020–Present)",
    subtitle = "Channel Islands National Park",
    x = "Use Type",
    y = "Average Annual Use",
    fill = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      size = 10
    ),
    axis.text.y = element_text(
      size = 10
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(
      size = 10
    )
  )

fig_7a_island_average_use_type

#----------------------------
#Fig 7b: faceted by use type-------

fig_7b_island_average_use_type_faceted <- ggplot(
  d2,
  aes(
    x = island,
    y = mean_annual,
    fill = island
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_annual - se_annual,
      ymax = mean_annual + se_annual
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  facet_wrap(
    ~ use_type,
    ncol = 2,
    scales = "free_y"
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    title = "Average Annual Recreational Use by Island and Use Type (2020–Present)",
    subtitle = "Channel Islands National Park",
    x = "Island",
    y = "Average Annual Use",
    fill = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      size = 9,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 10
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(
      size = 10
    )
  )
fig_7b_island_average_use_type_faceted

#----------------------
#save graphs-------
ggsave(
  "./doc/fig_7a_average_annual_use.png",
  plot = fig_7a_island_average_use_type,
  width = 10,
  height = 5.62,
  dpi = 300
)

ggsave(
  "./doc/fig_7b_island_average_use_type_faceted.png",
  plot = fig_7b_island_average_use_type_faceted,
  width = 10,
  height = 7,
  dpi = 300
)

#---------------------------------------
# Figure 5: Monthly Recreational Use by Island-----
#---------------------------------------

#Figure 5a: Monthly Recreational Visitors Ashore -------

fig_5a_monthly_onshore_use <- ggplot(
  d3,
  aes(
    x = date,
    y = `Rec Visitors Ashore`,
    color = island,
    group = island
  )
) +
  
  geom_line(linewidth = 0.8) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("1995-01-01"),
      max(d3$date, na.rm = TRUE),
      by = "5 years"
    ),
    date_labels = "%Y",
    limits = c(
      as.Date("1995-01-01"),
      max(d3$date, na.rm = TRUE)
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Monthly Recreational Use by Island",
    subtitle = "Channel Islands National Park (1995-Present)",
    x = "Year",
    y = "Monthly Recreational Visits Ashore",
    color = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_5a_monthly_onshore_use

#---------------------------------------
# Figure 5b: Monthly Recreational Visitors on Boats-------

fig_5b_monthly_boat_use <- ggplot(
  d3,
  aes(
    x = date,
    y = `Rec Visitors on Boats`,
    color = island,
    group = island
  )
) +
  
  geom_line(linewidth = 0.8) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2019-01-01"),
      max(d3$date, na.rm = TRUE),
      by = "1 year"
    ),
    date_labels = "%Y",
    limits = c(
      as.Date("2019-01-01"),
      max(d3$date, na.rm = TRUE)
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Monthly Recreational Boat Use by Island",
    subtitle = "Channel Islands National Park (2019-Present)",
    x = "Year",
    y = "Monthly Recreational Visits on Boats",
    color = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_5b_monthly_boat_use
#---------------
#save graphs-----
ggsave(
  "./doc/fig_5a_monthly_onshore_use.png",
  plot = fig_5a_monthly_onshore_use,
  width = 10,
  height = 5.62,
  dpi = 300
)

ggsave(
  "./doc/fig_5b_monthly_boat_use.png",
  plot = fig_5b_monthly_boat_use,
  width = 10,
  height = 5.62,
  dpi = 300
)


#------------------
#Figure 6: seasonal visitor use by island --------
#-----------------
# Figure 6a: Seasonal Mean Onshore Use------
fig_6a_seasonal_means_onshore_use <- d4 %>%
  filter(measure == "Rec Visitors Ashore") %>%
  
  ggplot(
    aes(
      x = season_year,
      y = mean_monthly,
      color = island,
      group = island
    )
  ) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(
    ~ season,
    ncol = 1
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(1995, 2025, by = 5),
    limits = c(1995, 2025),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Seasonal Monthly Mean Recreational Use by Island",
    subtitle = "Channel Islands National Park (Summer 1995–Summer 2025)",
    x = "Year",
    y = "Mean Monthly Visits Ashore",
    color = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_6a_seasonal_means_onshore_use

#---------------------------------------
# Figure 6b: Seasonal Mean Boat Use-------

fig_6b_seasonal_means_boat_use <- d4 %>%
  filter(measure == "Rec Visitors on Boats") %>%
  
  ggplot(
    aes(
      x = season_year,
      y = mean_monthly,
      color = island,
      group = island
    )
  ) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(
    ~ season,
    ncol = 1
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(2019, 2025, by = 1),
    limits = c(2019, 2025),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Seasonal Monthly Mean Recreational Boat Use by Island",
    subtitle = "Channel Islands National Park (Summer 2019–Summer 2025)",
    x = "Year",
    y = "Mean Monthly Visits on Boats",
    color = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_6b_seasonal_means_boat_use

#---------------------------------------
# Figure 6c: Seasonal Total Onshore Use ------

fig_6c_seasonal_totals_onshore_use <- d4 %>%
  filter(measure == "Rec Visitors Ashore") %>%
  
  ggplot(
    aes(
      x = season_year,
      y = seasonal_total,
      color = island,
      group = island
    )
  ) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(
    ~ season,
    ncol = 1
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(1995, 2025, by = 5),
    limits = c(1995, 2025),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Seasonal Recreational Use by Island",
    subtitle = "Channel Islands National Park (Summer 1995–Summer 2025)",
    x = "Year",
    y = "Total Seasonal Visits Ashore",
    color = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_6c_seasonal_totals_onshore_use

#---------------------------------------
# Figure 6d: Seasonal Total Boat Use --------

fig_6d_seasonal_totals_boat_use <- d4 %>%
  filter(measure == "Rec Visitors on Boats") %>%
  
  ggplot(
    aes(
      x = season_year,
      y = seasonal_total,
      color = island,
      group = island
    )
  ) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(
    ~ season,
    ncol = 1
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(2019, 2025, by = 1),
    limits = c(2019, 2025),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Seasonal Recreational Boat Use by Island",
    subtitle = "Channel Islands National Park (Summer 2019–Summer 2025)",
    x = "Year",
    y = "Total Seasonal Visits on Boats",
    color = "Island"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_6d_seasonal_totals_boat_use

#---------------------------------------
# Save Figures-------------

ggsave(
  "./doc/fig_6a_seasonal_means_onshore_use.png",
  plot = fig_6a_seasonal_means_onshore_use,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "./doc/fig_6b_seasonal_means_boat_use.png",
  plot = fig_6b_seasonal_means_boat_use,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "./doc/fig_6c_seasonal_totals_onshore_use.png",
  plot = fig_6c_seasonal_totals_onshore_use,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "./doc/fig_6d_seasonal_totals_boat_use.png",
  plot = fig_6d_seasonal_totals_boat_use,
  width = 10,
  height = 7,
  dpi = 300
)

#Anya Yu-Swanson
#NOAA CINMS 
#CINMS Visitor Use

#Goal: Graphs for Visitor Use trends---- 

#load libraries
library(tidyverse)
library(scales)
#-----------------

#clear environment and set working directory
remove(list=ls())      
setwd("/Users/anya/Documents/r_projects/nps_data")  

#Load files created in check_data----------------
d1<-read_csv("./results/total_annual_use.csv") %>%
  glimpse()

range(d1$year)
range(d1$total_visits)

d2<-read_csv("./results/seasonal_means.csv")

#---------------------------------------
#Figure 1: Total Annual use----- 
#---------------------------------------
#Figure 1a. Text/lines with sanctuary creation and covid labels- raw data----
fig_1a_total_annual_use <- ggplot(d1, aes(x = year, y = total_visits)) +
  
  # Main line
  geom_line(linewidth = 1, color = "black") +
  
  # CINMS founding line
  geom_vline(
    xintercept = 1980,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +
  
  # CINMS founding label
  annotate(
    "text",
    x = 1980.5,
    y = max(d1$total_visits, na.rm = TRUE) * 0.8,
    label = "1980 CINMS founded",
    angle = 90,
    size = 2.5,
    color = "gray30"
  ) +
  
  # COVID shading
  annotate(
    "rect",
    xmin = 2020,
    xmax = 2023,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray70",
    alpha = 0.25
  ) +
  
  # Horizontal COVID-19 label
  annotate(
    "text",
    x = 2021.5,
    y = max(d1$total_visits, na.rm = TRUE) * 1,
    label = "COVID-19",
    angle = 0,
    size = 3,
    color = "gray30",
    fontface = "bold"
  ) +
  
  # Island Packers ferry closure line
  geom_vline(
    xintercept = 2020,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +
  
  # Ferry closure label
  annotate(
    "text",
    x = 2020.5,
    y = max(d1$total_visits, na.rm = TRUE) * 0.75,
    label = "2020 Island Packers Ferry Closure",
    angle = 90,
    size = 2.5,
    color = "gray30"
  ) +
  
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(
      floor(min(d1$year, na.rm = TRUE) / 5) * 5,
      ceiling(max(d1$year, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    limits = c(1980, max(d2$season_year, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Total Annual Use (1980-Present)",
    subtitle = "Channel Islands National Marine Sanctuary",
    x = "Year",
    y = "Total Annual Visits"
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
    
    # Smaller axis titles
    axis.title = element_text(size = 12),
    
    # Move axis titles closer to tick labels
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
    
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Keep border
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
    )
  )

#---------------------------------------
# Figure 1b: Add 2019 boat-count change ------

fig_1b_total_annual_use <- fig_1a_total_annual_use +
  
  # Boat data started being counted in 2019
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = 2019.5,
    y = max(d1$total_visits, na.rm = TRUE) * 0.82,
    label = "2019 Counts of Recreational Users on Boats Began",
    angle = 90,
    size = 1.9,
    color = "gray30"
  )

# Save Figures -----
#Figure 1a
ggsave("./doc/fig_1a_total_annual_use.png", plot = fig_1a_total_annual_use,
       width=10, height=5.62, dpi=300)
#Figure 1b
ggsave("./doc/fig_1b_total_annual_use.png",plot = fig_1b_total_annual_use,
       width = 10, height = 5.62, dpi = 300)

#---------------------------------------
#Figure 2: Seasonal Annual Visitation------- 
#---------------------------------------
#Figure 2a. Two multicolored lines showing winter and summer monthly means----- 

# Figure 2a: Winter and Summer seasonal means
fig_2a_seasonal_use <- ggplot(
  d2,
  aes(
    x = season_year,
    y = monthly_mean_visits,
    color = season,
    shape = season,
    group = season
  )
) +
  
  # Standard error ribbons
  geom_ribbon(
    aes(
      ymin = monthly_mean_visits - se_visits,
      ymax = monthly_mean_visits + se_visits,
      fill = season
    ),
    color = NA,
    alpha = 0.15
  ) +
  
  # Seasonal mean lines
  geom_line(linewidth = 1) +
  
  # Seasonal mean points
  geom_point(size = 2.5) +
  
  # Season colors
  scale_color_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    )
  ) +
  
  # Matching ribbon colors
  scale_fill_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    ),
    guide = "none"
  ) +
  
  # Different point symbols
  scale_shape_manual(
    values = c(
      "Winter" = 16,
      "Summer" = 17
    )
  ) +
  
  # Y-axis
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # X-axis: 5-year intervals
  scale_x_continuous(
    breaks = seq(
      floor(min(d2$season_year, na.rm = TRUE) / 5) * 5,
      ceiling(max(d2$season_year, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    limits = c(1979, max(d2$season_year, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Seasonal Use (1980-Present)",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Monthly Visits (Mean, Standard Error)",
    color = "Season",
    shape = "Season"
  ) +
  
  theme_minimal() +
  theme(
    # Match Figure 1 title formatting
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 15)
    ),
    
    # Match Figure 1 axes
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
    
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Match Figure 1 border/background
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
    
    # Smaller legend to match figure scale
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(size = 10)
  )

fig_2a_seasonal_use

#---------------------------
#Figure 2b.  With lines for sanctuary creation, COVID------

fig_2b_seasonal_use <- fig_2a_seasonal_use +
  
  # CINMS founding line
  geom_vline(
    xintercept = 1980,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +
  
  # CINMS founding label
  annotate(
    "text",
    x = 1980.5,
    y = max(d2$monthly_mean_visits, na.rm = TRUE) * 0.8,
    label = "1980 CINMS founded",
    angle = 90,
    size = 2.5,
    color = "gray30"
  ) +
  
  # COVID shading
  annotate(
    "rect",
    xmin = 2020,
    xmax = 2023,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray70",
    alpha = 0.25
  ) +
  
  # COVID label
  annotate(
    "text",
    x = 2021.5,
    y = max(d2$monthly_mean_visits, na.rm = TRUE),
    label = "COVID-19",
    size = 3,
    color = "gray30",
    fontface = "bold"
  ) +
  
  # Island Packers ferry closure line
  geom_vline(
    xintercept = 2020,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +
  
  # Ferry closure label
  annotate(
    "text",
    x = 2020.5,
    y = max(d2$monthly_mean_visits, na.rm = TRUE) * 0.75,
    label = "2020 Island Packers Ferry Closure",
    angle = 90,
    size = 2.5,
    color = "gray30"
  )

fig_2b_seasonal_use

#-------------------------
#Fig 2c: seasonal total visits
#combined monthly totals over 6 month period for each season
#Winter 2006 only has 5 months of data, so it may be artificially low because December 2005 is missing from original dataset

fig_2c_seasonal_totals <- ggplot(
  d2,
  aes(
    x = season_year,
    y = seasonal_total,
    color = season,
    group = season
  )
) +
  
  geom_line(linewidth = 1) +
  
  scale_color_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(
      floor(min(d2$season_year, na.rm = TRUE) / 5) * 5,
      ceiling(max(d2$season_year, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    limits = c(
      1980,
      max(d2$season_year, na.rm = TRUE)
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Seasonal Visitor Use (Summer 1980 - Summer 2025)",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Total Seasonal Visits",
    color = "Season"
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

fig_2c_seasonal_totals

# Save Figures -----
#Figure 1a
ggsave("./doc/fig_2a_seasonal_use.png", plot = fig_2a_seasonal_use,
       width=10, height=5.62, dpi=300)
#Figure 1b
ggsave("./doc/fig_2b_seasonal_use.png",plot = fig_2b_seasonal_use,
       width = 10, height = 5.62, dpi = 300)

ggsave("./doc/fig_2c_seasonal_totals.png",plot = fig_2c_seasonal_totals,
       width = 10, height = 5.62, dpi = 300)
#---------------------------------------
#Figure 3: Types of Use (total annual visits Recreation vs. Non-recreation) -----
#---------------------------------------
fig_3_visit_types <- ggplot(d1, aes(x = year)) +
  
  # Recreation visits
  geom_line(
    aes(y = TRV, color = "Recreation"),
    linewidth = 1
  ) +
  
  # Non-recreation visits
  geom_line(
    aes(y = TNRV, color = "Non-Recreation"),
    linewidth = 1
  ) +
  
  # Set line colors
  scale_color_manual(
    values = c(
      "Recreation" = "steelblue",
      "Non-Recreation" = "darkorange"
    )
  ) +
  
  # Y-axis
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(
      floor(min(d2$season_year, na.rm = TRUE) / 5) * 5,
      ceiling(max(d2$season_year, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    limits = c(
      1980,
      max(d2$season_year, na.rm = TRUE)
    ),
    expand = c(0, 0)
  ) +
  
  # Labels
  labs(
    title = "Annual Recreation and Non-Recreation Use (1980-Present)",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Total Annual Visits",
    color = "Use Type"
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
    
    axis.title = element_text(
      size = 12
    ),
    
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
    
    axis.text.y = element_text(
      size = 10
    ),
    
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Border
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.8
    ),
    
    # Background
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    # Legend
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    )
  )

# Save figure
ggsave(
  "./doc/fig_3_visit_types.png",
  plot = fig_3_visit_types,
  width = 10,
  height = 5.62,
  dpi = 300
)



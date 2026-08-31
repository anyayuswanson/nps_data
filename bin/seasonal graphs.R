#Anya Yu- Swanson
#NOAA CINMS
#CINMS Visitor Use

#GOAL: CREATE GRAPHS TO SHOW SEASONAL VARIATION OF VISITOR USE
library(tidyverse)
#Goal: create fill area graph color coded by season
#create new dataset that has season as a column based off of the month
d4 <- d1 %>%
  filter(statistic == "TRV", year >= 2005) %>%
  mutate(
    season = case_when(
      month %in% c(10,11,12,1,2,3) ~ "Winter",
      month %in% c(4,5,6,7,8,9) ~ "Summer"
    )
  )
View(d4)
# ------------------------------------------------------------
# CREATE MONTHLY RECREATION VISITS DATASET WITH SEASONS
# ------------------------------------------------------------

d4 <- d1 %>%
  
  # Keep only Total Recreation Visits from 2005 onward
  filter(statistic == "TRV", year >= 2005) %>%
  
  # Create season and seasonal grouping columns
  mutate(
    
    # Assign each month to Winter or Summer
    season = case_when(
      month %in% c(10, 11, 12, 1, 2, 3) ~ "Winter",
      month %in% c(4, 5, 6, 7, 8, 9) ~ "Summer"
    ),
    
    # Give each continuous season its own group
    # Oct-Dec are grouped with Jan-Mar of the following year
    season_group = case_when(
      month %in% c(10, 11, 12) ~ paste0("Winter_", year + 1),
      month %in% c(1, 2, 3) ~ paste0("Winter_", year),
      month %in% c(4, 5, 6, 7, 8, 9) ~ paste0("Summer_", year)
    )
  )


# ------------------------------------------------------------
# CREATE SMOOTH BOUNDARY POINTS BETWEEN SEASONS
# ------------------------------------------------------------

transition_points <- d4%>%
  
  # Put observations in chronological order
  arrange(date) %>%
  
  # Look ahead to the next month's information
  mutate(
    next_date = lead(date),
    next_value = lead(value),
    next_season = lead(season),
    next_group = lead(season_group)
  ) %>%
  
  # Keep only locations where one seasonal group changes to another
  filter(
    !is.na(next_group),
    season_group != next_group
  ) %>%
  
  # Create a boundary halfway between the two monthly observations
  mutate(
    boundary_date = date + (next_date - date) / 2,
    
    # Estimate the visitation value halfway between the two months
    boundary_value = (value + next_value) / 2
  )


# ------------------------------------------------------------
# MAKE TWO COPIES OF EACH BOUNDARY POINT
# One belongs to the season ending and one to the season beginning
# ------------------------------------------------------------

boundary_previous <- transition_points %>%
  transmute(
    date = boundary_date,
    value = boundary_value,
    season = season,
    season_group = season_group
  )

boundary_next <- transition_points %>%
  transmute(
    date = boundary_date,
    value = boundary_value,
    season = next_season,
    season_group = next_group
  )


# ------------------------------------------------------------
# COMBINE ORIGINAL DATA WITH THE EXTRA BOUNDARY POINTS
# This dataset is used only for drawing the colored area
# ------------------------------------------------------------

d4_area <- bind_rows(
  d4,
  boundary_previous,
  boundary_next
) %>%
  arrange(date)


# ------------------------------------------------------------
# CREATE GRAPH
# ------------------------------------------------------------

ggplot() +
  
  # Fill the area below the visitation line according to season
  geom_area(
    data = d4_area,
    aes(
      x = date,
      y = value,
      fill = season,
      group = season_group
    ),
    
    # Draw each seasonal area at its actual value instead of stacking them
    position = "identity",
    
    # Make the fill slightly transparent
    alpha = 0.6
  ) +
  
  # Draw the original monthly visitation line on top
  geom_line(
    data = d4,
    aes(x = date, y = value),
    linewidth = 0.5
  ) +
  
  # Assign colors to the seasons
  scale_fill_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    ),
    name = "Season"
  ) +
  
  # Customize the y-axis
  scale_y_continuous(
    
    # Put a tick mark every 10,000 visits
    breaks = seq(
      0,
      ceiling(max(d4$value, na.rm = TRUE) / 10000) * 10000,
      by = 10000
    ),
    
    # Add commas to large numbers
    labels = scales::comma,
    # Remove extra space below zero
    expand = c(0, 0)
  ) +
  
  # Customize the x-axis
  scale_x_date(
    
    # Show 2005 through the end of 2025
    limits = as.Date(c("2005-01-01", "2025-12-31")),
    
    # Put a tick mark at every year
    date_breaks = "1 year",
    
    # Display only the four-digit year
    date_labels = "%Y",
    
    # Remove extra space on either side
    expand = c(0, 0)
  ) +
  
  # Add title and axis labels
  labs(
    title = "Monthly Recreational Visits to Channel Islands National Park",
    x = "Year",
    y = "Total Monthly Recreation Visits"
  ) +
  
  # Use a clean base theme
  theme_minimal() +
  
  # Customize graph appearance
  theme(
    
    # Main title
    plot.title = element_text(
      size = 24,
      face = "bold",
      margin = margin(b = 20)
    ),
    
    # Both axis titles
    axis.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    # Add space below the x-axis numbers
    axis.title.x = element_text(
      margin = margin(t = 20)
    ),
    
    # Add space between y-axis numbers and title
    axis.title.y = element_text(
      margin = margin(r = 20)
    ),
    
    # X-axis year labels
    axis.text.x = element_text(
      size = 14,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    # Y-axis numbers
    axis.text.y = element_text(
      size = 16
    ),
    
    # Major gridlines
    panel.grid.major = element_line(
      linewidth = 0.6
    ),
    
    # Remove minor gridlines
    panel.grid.minor = element_blank(),
    
    # Add border around plotting area
    panel.border = element_rect(
      fill = NA,
      linewidth = 1.2
    ),
    
    # Legend title
    legend.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    # Legend labels
    legend.text = element_text(
      size = 14
    )
  )


#_______________________
#GOAL: seasonal visits line graph

# ------------------------------------------------------------
# CREATE HELPER DATASET SO SEASONAL LINE SEGMENTS CONNECT
# ------------------------------------------------------------

line_transition_points <- d4 %>%
  
  # Put observations in chronological order
  arrange(date) %>%
  
  # Save the previous season and previous seasonal group
  mutate(
    previous_season = lag(season),
    previous_group = lag(season_group)
  ) %>%
  
  # Keep only points where the seasonal group changes
  filter(
    !is.na(previous_group),
    season_group != previous_group
  ) %>%
  
  # Copy the first point of the new season into the previous season
  mutate(
    season = previous_season,
    season_group = previous_group
  ) %>%
  
  # Remove temporary columns
  select(-previous_season, -previous_group)


# Combine the original data with the copied transition points
d4_line <- bind_rows(
  d4,
  line_transition_points
) %>%
  
  # Put everything back in chronological order
  arrange(date)


# ------------------------------------------------------------
# GRAPH
# ------------------------------------------------------------

ggplot(
  d4_line,
  aes(
    x = date,
    y = value,
    color = season,
    group = season_group
  )
) +
  
  # Draw seasonal visitation line
  geom_line(linewidth = 1.5) +
  
  # Assign colors
  scale_color_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    ),
    name = "Season"
  ) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(d4$value, na.rm = TRUE) / 10000) * 10000,
      by = 10000
    ),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  
  # X-axis
  scale_x_date(
    limits = as.Date(c("2005-01-01", "2025-12-31")),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Recreation Visits to Channel Islands National Park",
    x = "Year",
    y = "Monthly Recreation Visits"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 24,
      face = "bold",
      margin = margin(b = 20)
    ),
    
    axis.title = element_text(
      size = 20,
    ),
    axis.title.x = element_text(
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      margin = margin(r = 20)
    ),
    
    axis.text.x = element_text(
      size = 14,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    axis.text.y = element_text(
      size = 16
    ),
    
    panel.grid.major = element_line(
      linewidth = 0.6
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      fill = NA,
      linewidth = 1.2
    ), 
    # Legend title
    legend.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    # Legend labels
    legend.text = element_text(
      size = 14
    )
  ))
#------------------------------------------------
#goal: create graph with 2 lines showing seasonal means

#Standard deviation = how much the individual monthly visitation values vary within that season
#Standard error = how uncertain/precise your estimate of the seasonal mean is
      # Shaded areas represent ±1 standard error of the six monthly visitation values within each season
      # wider --> more variation in monthly values with season --> estimated mean less precise

# Create seasonal summary dataset
season_summary <- d1 %>%
  filter(statistic == "TRV", year >= 2005) %>%
  mutate(
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
    mean_visits = mean(value, na.rm = TRUE),
    sd_visits = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    se_visits = sd_visits / sqrt(n),
    .groups = "drop"
  )

# Create graph
ggplot(
  season_summary,
  aes(
    x = season_year,
    y = mean_visits,
    color = season,
    shape = season,
    group = season
  )
) +
  
  # Add standard error ribbons matching each season's color
  # geom_ribbon(
  #   aes(
  #     ymin = mean_visits - se_visits,
  #     ymax = mean_visits + se_visits,
  #     fill = season
  #   ),
  #   color = NA,
  #   alpha = 0.15
  # ) +

  # Draw lines connecting seasonal means
  geom_line(linewidth = 1.2) +
  
  # Add a different symbol for each season
  geom_point(size = 3.5) +
  
  # Set Winter and Summer line colors
  scale_color_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    )
  ) +
  
  # Set matching ribbon colors
  scale_fill_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    ),
    guide = "none"
  ) +
  
  # Set different point symbols
  scale_shape_manual(
    values = c(
      "Winter" = 16,
      "Summer" = 17
    )
  ) +
  
  # Format y-axis
  scale_y_continuous(
    labels = scales::comma
  ) +
  
  # Show every year on x-axis
  scale_x_continuous(
    breaks = seq(
      min(season_summary$season_year),
      max(season_summary$season_year),
      by = 1
    )
  ) +
  
  labs(
    title = "Seasonal Recreation Visitation",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Mean Monthly Recreation Visits",
    color = "Season",
    shape = "Season"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 18,
      margin = margin(b = 15)
    ),
    axis.title = element_text(
      size = 18,
    ),
    axis.title.x = element_text(
      margin = margin(t = 18)
    ),
    axis.title.y = element_text(
      margin = margin(r = 18)
    ),
    axis.text.x = element_text(
      size = 13,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 16
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      linewidth = 1.2
    ),
    legend.title = element_text(
      size = 16,
      face = "bold"
    ),
    legend.text = element_text(
      size = 14
    )
  )

#--------------------------------------
#Goal: seasonal graph with 2 lines, LOESS, and labels
#--------------------------------------
#LOESS smooths yearly seasonal averages

library(tidyverse)

# Create seasonal summary dataset for full record
season_summary <- d1 %>%
  filter(statistic == "TRV") %>%
  mutate(
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
    mean_visits = mean(value, na.rm = TRUE),
    sd_visits = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    se_visits = sd_visits / sqrt(n),
    .groups = "drop"
  )

# Create graph
ggplot(
  season_summary,
  aes(
    x = season_year,
    y = mean_visits,
    color = season,
    shape = season,
    group = season
  )
) +
  
  # Standard error ribbons
  # geom_ribbon(
  #   aes(
  #     ymin = mean_visits - se_visits,
  #     ymax = mean_visits + se_visits,
  #     fill = season
  #   ),
  #   color = NA,
  #   alpha = 0.15
  # ) +
  
  # Winter and Summer mean lines
  # geom_line(linewidth = 1.2) +
  
  # Symbols for each yearly seasonal mean
  # geom_point(size = 3.5) +
  
  #smooth lines to show long term trend in winter and summer visitation
geom_smooth(
  aes(color = season, fill = season),
  method = "loess",
  span = 0.4,
  se = FALSE,  # change to TRUE to have SE shaded area or FALSE to get rid of it
  alpha = 0.15,
  linewidth = 1.5
) +
  # Shade the COVID-19 period
  annotate(
    "rect",
    xmin = 2020,
    xmax = 2021,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray50",
    alpha = 0.12
  ) +
  
  # Label the COVID-19 period
  annotate(
    "text",
    x = 2020.5,
    y = 68000,
    label = "COVID-19",
    size = 4.5
  ) +
  
  # Dashed lines where visitor-count methods changed
  geom_vline(
    xintercept = c(1998, 2004, 2005, 2019),
    linetype = "dashed",
    linewidth = 0.6,
    color = "gray50"
  ) +
  
  # Labels for visitor-count method changes
  annotate("text", x = 1997.6, y = 65000,
           label = "1998 (Overhaul)",
           angle = 90, hjust = 1, size = 4) +
  
  annotate("text", x = 2003.6, y = 61000,
           label = "2004 (Minor)",
           angle = 90, hjust = 1, size = 4) +
  
  annotate("text", x = 2004.6, y = 53000,
           label = "2005 (Major)",
           angle = 90, hjust = 1, size = 4) +
  
  annotate("text", x = 2018.6, y = 62000,
           label = "2019 (Major)",
           angle = 90, hjust = 1, size = 4) +
  
  # Winter and Summer colors
  scale_color_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    )
  ) +
  
  # Matching SE ribbon colors
  scale_fill_manual(
    values = c(
      "Winter" = "steelblue",
      "Summer" = "orange"
    ),
    guide = "none"
  ) +
  
  # Different symbols for each season
  scale_shape_manual(
    values = c(
      "Winter" = 16,
      "Summer" = 17
    )
  ) +
  
  # Y-axis
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  # X-axis: every year, with no gap at either edge
  scale_x_continuous(
    limits = c(
      min(season_summary$season_year),
      max(season_summary$season_year)
    ),
    breaks = seq(
      min(season_summary$season_year),
      max(season_summary$season_year),
      by = 1
    ),
    expand = c(0, 0)
  ) +
  
  # Titles
  labs(
    title = "Seasonal Recreation Visitation",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Mean Monthly Recreation Visits",
    color = "Season",
    shape = "Season",
    caption = "Dashed lines indicate changes in NPS visitor-count methods. Gray shading indicates the COVID-19 period, which affected visitation."
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 24,
      face = "bold",
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 18,
      margin = margin(b = 15)
    ),
    axis.title = element_text(
      size = 20,
    ),
    axis.title.x = element_text(
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      margin = margin(r = 20)
    ),
    axis.text.x = element_text(
      size = 12,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 16
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      fill = NA,
      linewidth = 1.2
    ),
    legend.title = element_text(
      size = 16,
      face = "bold"
    ),
    legend.text = element_text(
      size = 14
    )
  )


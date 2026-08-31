#Anya Yu- Swanson
#NOAA CINMS
#CINMS Visitor Use

#Goal: To create graph showing visits to CINMS over time
#------------------
library(tidyverse) #load library each time so code can run independently 

#-------------------
remove(list=ls())   
setwd("/Users/anya/Documents/r_projects/nps_data") 

#-----------------
#d1
#create data sets 
d1 <- read_csv("./data/nps_chis_visitors.csv")%>%    #d1=data frame, we are assigning csv file to R data frame 
  mutate(date=make_date(year=year, month=month))%>% #creates new variable/column named date that combines month and year, automatically sets it to the first of the month becuase no exact date
  glimpse() #lets you see tibble in directory 
d1
unique(d1$stat_desc) #give list of unique texts in column 

range(d1$year) #gives the range of numbers in a column

d2<-d1%>%  #subset of d1 that is one of the variables, only Total Recreational Visits
  filter(statistic=="TRV")%>%
  glimpse() 
unique(d2$stat_desc)
# ------------------------------------
#d3
#Create new data set that contains the annual total visits 
#combine monthly totals to show simplified trend of visits over the years
#create new annual data set (subset of d1)
d3 <- d1 %>%
  #Group together rows that have the same park, year, statistic, and statistic descriptions
  group_by(park, year, statistic, stat1, stat2, stat_desc) %>%
  #makes a column with a summary value for group 
  #title = "annual_visits", sums numbers in value column (monthly visits)
  #na.rm =TRUE --> Ignore missing values when calculating the sum because , if R tries to add numbers and one of them is NA, the answer can become NA
  summarise(
    annual_visits = sum(value, na.rm = TRUE), 
    .groups = "drop"
  )

view(d3)

#-----------------------

#create graph showing TRV annually
filter(d3, statistic == "TRV")%>%
  ggplot(aes(x=year, y=annual_visits))+ 
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Annual Recreational Visits to Channel Islands National Park",
    x = "Year",
    y = "Total Annual Visits"
  ) +
  theme_minimal()
#---------------------------

# create total visits line graph  per month

# Start with the full dataset, keep only Total Visits, then send that filtered data into ggplot
d1 %>%
  filter(statistic == "TV") %>%
#other option does same thing in one line: filter(d1, statistic == "TV")%>%
#other option: could create a subset of d1 with the filtered values and then make a plot using that
  # Create graph with date on the x-axis and number of visits on the y-axis
  ggplot(aes(x = date, y = value)) +
  
  # Draw the line to make it a line graph
  geom_line() +
  
  # Add commas to large numbers on the y-axis
  scale_y_continuous(labels = scales::comma) +
  
  # Add graph labels
  labs(
    title = "Total Visits to Channel Islands National Park",
    x = "Month and Year",
    y = "Total Monthly Visits"
  ) +
  
  # Use a clean theme
  theme_minimal()

#----------------------------
#Total Recreational Visits Graph with shaded area

#use d2 becuase that subset only has values for TRV
ggplot(d2, aes(x=date, y=value))+ 
  geom_area() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Recreational Visits to Channel Islands National Park",
    x = "Month and Year",
    y = "Total Monthly Visits"
  ) +
  theme_minimal()

#------------------------------
#Improve readability of TRV graph-- bigger text, smaller time frame

#1. MAKE LINE GRAPH 
# Start with the original dataset d1
d1 %>%
  # Keep only Total Recreation Visits and years 2005 or later
  filter(statistic == "TRV", year >= 2005) %>%
  
  # Start the graph using date on the x-axis and visit value on the y-axis
  ggplot(aes(x = date, y = value)) +
  
  # Draw the line
  geom_line(linewidth = 1.1) +


#2.CUSTOMIZE AXES
# customize the y-axis
  scale_y_continuous(
    # Put a gridline from 0 to 70,000 every 10,000 visits
    breaks = seq(0, 70000, by = 10000),
    # Force the y-axis to start at 0 and end at 70,000
    limits = c(0, 70000),
    # Display large numbers with commas
    labels = scales::comma,
    # Remove extra blank space above and below the y-axis range
    expand = c(0, 0)
  ) +
  
# Customize x-axis
  scale_x_date(     #ggplot function for x-axis that contains dates
    # Limit the graph to January 2005 through December 2025
    limits = as.Date(c("2005-01-01", "2025-12-31")),   
          #as.Date() converts strings into R date objects so it recognizes them as dates      
          #c()= combine end dates
    # Put a tick mark/gridline at every year
    date_breaks = "1 year",
    # Display only the 4-digit year for each tick mark
    date_labels = "%Y",
    # Remove extra blank space before and after the date range
    expand = c(0, 0)
  ) +

#3.LABELS  
# Add the graph title and axis titles
  labs(
    title = "Monthly Recreational Visits to Channel Islands National Park",
    x = "Year",
    y = "Total Monthly Recreation Visits"
  ) +
  
#4.GRAPH THEME/AESTHETICS/APPEARANCE
  #start with simple, clean graph theme
  theme_minimal() +
  # Customize the appearance of the text, gridlines, and border
  theme(
    # Make the main graph title larger and bold
    plot.title = element_text(
      size = 24,
      face = "bold",
      margin = margin(b = 25) #for adjusting margins: t=top, r=right, b=bottom, l=left
    ),
    # Make both axis titles larger and bold
    axis.title = element_text(
      size = 20
    ),
    # X-axis title
    axis.title.x = element_text(
      margin = margin(t = 25)
    ),
    # Y-axis title
    axis.title.y = element_text(
      margin = margin(r = 25)
    ),
#5. CHANGE APPEARANCE OF AXES LABELS
    # Change the appearance of the year labels on the x-axis
    axis.text.x = element_text(
      # Make the year numbers larger
      size = 14,
      # Rotate the year labels 90 degrees so they are vertical
      angle = 90,
      # Vertically center the rotated labels
      vjust = 0.5,
      # Adjust the horizontal position of the rotated labels
      hjust = 1
    ),
    
    # Make the numbers on the y-axis larger
    axis.text.y = element_text(
      size = 16
    ),
#6. CUSTOMIZE GRIDLINES
    # make main gridlines line up with axis intervals
    panel.grid.major = element_line(
      linewidth = 0.6
    ),
    # Remove the smaller gridlines between the labeled intervals
    panel.grid.minor = element_blank(),
    # Add a rectangular border around the plotting area
    panel.border = element_rect(
      # Do not fill in the rectangle, so the graph remains visible
      fill = NA,
      # Make the border slightly thicker
      linewidth = 1
    )
  )

#------------------------------
#Goal: Smooth Line LOESS

ggplot(d4, aes(x = date, y = value)) +
  
  # Original monthly visitation line
  geom_line(linewidth = 0.8) +
  
  # Add a separate LOESS curve for each year
  geom_smooth(
    aes(group = year),
    method = "loess",
    se = FALSE,
    span = 0.8,
    linewidth = 1.2
  ) +
  
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(d4$value, na.rm = TRUE) / 10000) * 10000,
      by = 10000
    ),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  
  scale_x_date(
    limits = as.Date(c("2005-01-01", "2025-12-31")),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Monthly Recreation Visitation to Channel Islands National Park",
    x = "Year",
    y = "Monthly Recreation Visits"
  ) +
  
  theme_minimal()

#---------------------------------------
#Goal: graph that shows different count methods
# ------------------------------------------------------------
# CREATE DATASET WITH VISITOR COUNT METHOD PERIODS
# ------------------------------------------------------------

d6 <- d1 %>%
  filter(statistic == "TRV") %>%
  arrange(date) %>%
  mutate(
    count_period = case_when(
      year <= 1997 ~ "1970–1997 (Original methods)",
      year >= 1998 & year <= 2003 ~ "1998–2003 (Overhaul)",
      year == 2004 ~ "2004 (Minor update)",
      year >= 2005 & year <= 2018 ~ "2005–2018 (Major update)",
      year >= 2019 ~ "2019–Present (Major update)"
    ),
    
    # Set the chronological order for the legend
    count_period = factor(
      count_period,
      levels = c(
        "1970–1997 (Original methods)",
        "1998–2003 (Overhaul)",
        "2004 (Minor update)",
        "2005–2018 (Major update)",
        "2019–Present (Major update)"
      )
    ),
    
    # Get the next monthly point so each line segment can be drawn
    next_date = lead(date),
    next_value = lead(value)
  )


# ------------------------------------------------------------
# CREATE GRAPH
# ------------------------------------------------------------

ggplot(d6) +
  
  # Draw each month-to-month section and color by count method period
  geom_segment(
    aes(
      x = date,
      y = value,
      xend = next_date,
      yend = next_value,
      color = count_period
    ),
    linewidth = 1.2
  ) +
  
  # Add dashed lines where counting methods changed
  geom_vline(
    xintercept = as.Date(c(
      "1998-01-01",
      "2004-01-01",
      "2005-01-01",
      "2019-01-01"
    )),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Assign colors to each count method period
  scale_color_manual(
    values = c(
      "1970–1997 (Original methods)" = "#4E79A7",
      "1998–2003 (Overhaul)" = "#59A14F",
      "2004 (Minor update)" = "#B07AA1",
      "2005–2018 (Major update)" = "#F28E2B",
      "2019–Present (Major update)" = "#E15759"
    ),
    name = "Count Procedure Changes"
  ) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(d6$value, na.rm = TRUE) / 10000) * 10000,
      by = 10000
    ),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  
  # X-axis
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  
  # Titles and labels
  labs(
    title = "Recreation Visitation and Changes in Visitor Count Procedures",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Monthly Recreation Visits"
  ) +
  
  # Base theme
  theme_minimal() +
  
  # Appearance
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18, margin = margin(b = 20)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 16),
    panel.grid.major = element_line(linewidth = 0.6),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, linewidth = 1.2),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

#-----------------------------
d1 %>%
  group_by(statistic, stat_desc) %>%
  summarise(
    total = sum(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    nonzero_months = sum(value > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total))
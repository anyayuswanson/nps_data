#Goal: create graphs comparing types of visits 

#install package to put panels of graphs together
install.packages("patchwork")   # only once
library(tidyverse)
library(patchwork) #re-run each time 

#test which types of visits only have 0 values so they can be excluded
d1 %>%
  group_by(statistic, stat_desc) %>%
  summarise(
    total = sum(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    nonzero_months = sum(value > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total))

#-----------------------
#goal: create basic graphs showing   
library(tidyverse)

# 1. VISITS
d1 %>%
  filter(statistic %in% c("TRV", "TNRV")) %>%
  ggplot(aes(x = date, y = value, color = statistic)) +
  geom_line() +
  labs(
    title = "Monthly Visits",
    x = "Year",
    y = "Visits",
    color = "Type"
  ) +
  theme_minimal()

# 2. VISITOR HOURS
d1 %>%
  filter(statistic %in% c("TRVH", "TNRVH")) %>%
  ggplot(aes(x = date, y = value, color = statistic)) +
  geom_line() +
  labs(
    title = "Monthly Visitor Hours",
    x = "Year",
    y = "Visitor Hours",
    color = "Type"
  ) +
  theme_minimal()

# 3. OVERNIGHT STAYS
d1 %>%
  filter(statistic %in% c("BC", "MISC", "NROS")) %>%
  ggplot(aes(x = date, y = value, color = statistic)) +
  geom_line() +
  labs(
    title = "Monthly Overnight Stays",
    x = "Year",
    y = "Overnight Stays",
    color = "Type"
  ) +
  theme_minimal()

#---------------------------------------
#goal: create graphs showing annual trends in visitor use

library(tidyverse)

# 1. ANNUAL VISITS
#create annual total visits dataset
annual_visits <- d1 %>%
  filter(statistic %in% c("TRV", "TNRV")) %>%
  group_by(year, statistic) %>%
  summarise(
    annual_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    visit_type = case_when(
      statistic == "TRV" ~ "Recreation Visits",
      statistic == "TNRV" ~ "Non-Recreation Visits"
    )
  )

p1 <- ggplot(
  annual_visits,
  aes(x = year, y = annual_value, color = visit_type)
) +
  geom_line(linewidth = 1.2) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(1980, 2025, by = 5),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Annual Visitation",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Annual Visits",
    color = "Visit Type"
  ) +
  
  guides(
    color = guide_legend(ncol = 1)
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
      size = 20
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

# 2. ANNUAL VISITOR HOURS
annual_hours <- d1 %>%
  filter(statistic %in% c("TRVH", "TNRVH")) %>%
  group_by(year, statistic) %>%
  summarise(
    annual_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour_type = case_when(
      statistic == "TRVH" ~ "Recreation Visitor Hours",
      statistic == "TNRVH" ~ "Non-Recreation Visitor Hours"
    )
  )

p2 <- ggplot(
  annual_hours,
  aes(x = year, y = annual_value, color = hour_type)
) +
  geom_line(linewidth = 1.2) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(1980, 2025, by = 5),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Annual Visitor Hours",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Annual Visitor Hours",
    color = "Visitor Type"
  ) +
  guides(
    color = guide_legend(ncol = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", margin = margin(b = 3)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 15)),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, linewidth = 1.2),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )


# 3. ANNUAL OVERNIGHT STAYS
annual_overnight <- d1 %>%
  filter(statistic %in% c("BC", "MISC", "NROS")) %>%
  group_by(year, statistic) %>%
  summarise(
    annual_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stay_type = case_when(
      statistic == "BC" ~ "Backcountry Camping",
      statistic == "MISC" ~ "Miscellaneous",
      statistic == "NROS" ~ "Non-Recreation"
    )
  )

p3 <- ggplot(
  annual_overnight,
  aes(x = year, y = annual_value, color = stay_type)
) +
  geom_line(linewidth = 1.2) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(1980, 2025, by = 5),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Annual Overnight Stays",
    subtitle = "Channel Islands National Park",
    x = "Year",
    y = "Annual Overnight Stays",
    color = "Stay Type"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", margin = margin(b = 3)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 15)),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, linewidth = 1.2),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  guides(
    color = guide_legend(ncol = 1)
  ) +
  
  theme(
    legend.position = "bottom"
  )

library(patchwork)

(p1 | p2 | p3) &
  scale_x_continuous(
    breaks = seq(
      floor(min(d1$year) / 5) * 5,
      ceiling(max(d1$year) / 5) * 5,
      by = 5
    ),
    expand = c(0, 0)
  ) &
  theme(
    legend.position = "bottom"
  )

#---------------------------------------------



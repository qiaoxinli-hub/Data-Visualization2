# ============================================================
# Blog Post 4
# Data Visualization
#
# Research Question:
# Does Economic Development Narrow the Gender Gap
# in Labor Force Participation?
# ============================================================

# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)


# ------------------------------------------------------------
# 2. Create results folder
# ------------------------------------------------------------

dir.create("results/figures",recursive = TRUE,showWarnings = FALSE)

# ------------------------------------------------------------
# 3. Read processed data
# ------------------------------------------------------------

selected_trends <- read_csv(
  "data/processed/selected_country_trends.csv",
  show_col_types = FALSE
)

cross_section_2024 <- read_csv(
  "data/processed/cross_section_2024.csv",
  show_col_types = FALSE
)

selected_gap_change <- read_csv(
  "data/processed/selected_gap_change_1990_2024.csv",
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 4. Publication-style theme
# ------------------------------------------------------------

custom_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      color = "gray30",
      size = 10,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      color = "gray50",
      size = 8,
      hjust = 0
    ),
    axis.title = element_text(
      size = 11
    ),
    axis.text = element_text(
      color = "gray20"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray88",
      linewidth = 0.3
    ),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 9
    ),
    legend.margin = margin(b = 6)
  )


# ------------------------------------------------------------
# Academic color palette
# ------------------------------------------------------------

country_colors <- c(
  "China" = "#c44e52",
  "Germany" = "#8172b3",
  "Japan" = "#4c72b0",
  "Korea, Rep." = "#55a868",
  "United Kingdom" = "#dd8452",
  "United States" = "#64b5cd"
)


# ============================================================
# FIGURE 1
# Gender gap in labor force participation over time
# ============================================================

# Identify the first and latest available years
first_year <- min(
  selected_trends$year,
  na.rm = TRUE
)

latest_year <- max(
  selected_trends$year,
  na.rm = TRUE
)

# Create x-axis breaks
# Show every 5 years, while always including the latest year
x_breaks <- sort(unique(c(
  seq(first_year, latest_year, by = 5),
  latest_year
)))

# Get observations from the latest available year
latest_values <- selected_trends |>
  filter(
    year == latest_year
  )

# Create Figure 1

fig1 <- ggplot(
  selected_trends,
  aes(
    x = year,
    y = gender_gap,
    group = country,
    color = country
  )
) +
  
  # Reference line: no gender gap
  geom_hline(
    yintercept = 0,
    color = "gray65",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  # Reference line for the latest observation
  geom_vline(
    xintercept = latest_year,
    color = "gray85",
    linewidth = 0.4,
    linetype = "dotted"
  ) +
  
  # Trend lines
  geom_line(
    linewidth = 0.8,
    lineend = "round"
  ) +
  
  # Show every annual observation
  geom_point(
    size = 1.5,
    alpha = 0.85
  ) +
  
  # Highlight the latest observations
  geom_point(
    data = latest_values,
    size = 2.5
  ) +
  
  # Country colors
  scale_color_manual(
    values = country_colors
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = x_breaks,
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  
  # Limit the visible plotting area without deleting data
  coord_cartesian(
    xlim = c(first_year, latest_year),
    ylim = c(0, 30)
  ) +
  
  labs(
    title = "Gender Gap in Labor Force Participation, 1990–2024",
    subtitle = "Male labor force participation minus female labor force participation",
    x = "Year",
    y = "Gender gap (percentage points)",
    color = "Country",
    caption = paste0(
      "Source: World Bank, World Development Indicators; ",
      "ILO modeled estimates. Latest observation: ",
      latest_year,
      "."
    )
  ) +
  
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  
  custom_theme +
  
  theme(
    legend.position = "top"
  )


# Display
print(fig1)

# Save Figure 1

ggsave(
  filename = "results/figures/figure1_gender_gap_trends.png",
  plot = fig1,
  width = 9,
  height = 6,
  dpi = 300
)

# ============================================================
# FIGURE 2
# Economic development and gender gap
# ============================================================

library(ggrepel)
library(scales)

# ------------------------------------------------------------
# 1. Countries to highlight
# ------------------------------------------------------------

countries_to_highlight <- c(
  "United States",
  "Germany",
  "Japan",
  "United Kingdom",
  "Korea, Rep.",
  "China"
)


# ------------------------------------------------------------
# 2. Prepare plotting data
# ------------------------------------------------------------

fig2_data <- cross_section_2024 |>
  mutate(
    # GDP per capita in units of $10,000
    gdp_pc_10k = gdp_pc / 10000,
    
    # Highlight selected countries
    plot_group = if_else(
      country %in% countries_to_highlight,
      country,
      "Other countries"
    )
  )


# ------------------------------------------------------------
# 3. Define colors
# ------------------------------------------------------------

fig2_colors <- c(
  country_colors,
  "Other countries" = "gray75"
)


# ------------------------------------------------------------
# 4. Determine axis ranges automatically
# ------------------------------------------------------------

# Y-axis: include all countries
y_min <- floor(
  min(fig2_data$gender_gap, na.rm = TRUE) / 5
) * 5

y_max <- ceiling(
  max(fig2_data$gender_gap, na.rm = TRUE) / 5
) * 5


# X-axis: start from zero and include largest GDP value
x_max <- ceiling(
  max(fig2_data$gdp_pc_10k, na.rm = TRUE)
)


# ------------------------------------------------------------
# 5. Countries to label
# ------------------------------------------------------------

# Define "high GDP" and "small gender gap"
high_gdp_cutoff <- quantile(
  fig2_data$gdp_pc_10k,
  0.75,
  na.rm = TRUE
)

small_gap_cutoff <- quantile(
  fig2_data$gender_gap,
  0.25,
  na.rm = TRUE
)


label_data <- bind_rows(
  
  # 1. Countries with the largest gender gaps
  fig2_data |>
    arrange(desc(gender_gap)) |>
    slice_head(n = 5),
  
  # 2. Countries with the smallest gender gaps
  fig2_data |>
    arrange(gender_gap) |>
    slice_head(n = 3),
  
  # 3. High-GDP countries with relatively small gender gaps
  fig2_data |>
    filter(
      gdp_pc_10k >= high_gdp_cutoff,
      gender_gap <= small_gap_cutoff
    ) |>
    arrange(desc(gdp_pc_10k)) |>
    slice_head(n = 5)
  
) |>
  distinct(country, .keep_all = TRUE)


# ------------------------------------------------------------
# 6. Create Figure 2
# ------------------------------------------------------------

fig2 <- ggplot(
  fig2_data,
  aes(
    x = gdp_pc_10k,
    y = gender_gap
  )
) +
  
  # ----------------------------------------------------------
# Reference line: no gender gap
# ----------------------------------------------------------

geom_hline(
  yintercept = 0,
  color = "gray65",
  linewidth = 0.4,
  linetype = "dashed"
) +
  
  # ----------------------------------------------------------
# All countries
# ----------------------------------------------------------

geom_point(
  aes(color = plot_group),
  size = 2,
  alpha = 0.7
) +
  
  # ----------------------------------------------------------
# Labels for extreme observations
# ----------------------------------------------------------

geom_text_repel(
  data = label_data,
  aes(label = country),
  color = "gray25",
  size = 3.2,
  box.padding = 0.5,
  point.padding = 0.3,
  min.segment.length = 0,
  segment.color = "gray60",
  show.legend = FALSE,
  seed = 123
) +
  
  # ----------------------------------------------------------
# Descriptive linear relationship
# ----------------------------------------------------------

geom_smooth(
  method = "lm",
  se = TRUE,
  color = "gray35",
  fill = "gray80",
  linewidth = 0.8,
  fullrange = TRUE
) +
  
  # ----------------------------------------------------------
# Country colors
# ----------------------------------------------------------

scale_color_manual(
  values = fig2_colors
) +
  
  # ----------------------------------------------------------
# X-axis
# ----------------------------------------------------------

scale_x_continuous(
  limits = c(0, x_max),
  breaks = seq(
    0,
    x_max,
    by = 1
  ),
  labels = scales::label_dollar(
    scale = 10000,
    accuracy = 1000
  ),
  expand = expansion(
    mult = c(0, 0.02)
  )
) +
  
  # ----------------------------------------------------------
# Y-axis
# ----------------------------------------------------------

scale_y_continuous(
  breaks = seq(
    y_min,
    y_max,
    by = 5
  ),
  labels = scales::label_number(
    suffix = " pp",
    accuracy = 1
  ),
  expand = expansion(
    mult = c(0.02, 0.03)
  )
) +
  
  # ----------------------------------------------------------
# Labels
# ----------------------------------------------------------

labs(
  title = "Economic Development and the Gender Gap in Labor Force Participation, 2024",
  x = "GDP per capita (constant 2021 international dollars)",
  y = "Gender gap (percentage points)",
  color = "Country",
  caption = paste0(
    "Source: World Bank, World Development Indicators; ",
    "ILO modeled estimates. The fitted line shows a descriptive ",
    "linear association."
  )
) +
  
  # ----------------------------------------------------------
# Publication-style theme
# ----------------------------------------------------------

custom_theme +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 8.5
    ),
    legend.margin = margin(
      b = 8
    ),
    plot.margin = margin(
      10, 10, 10, 10
    )
  )


# ------------------------------------------------------------
# 7. Display figure
# ------------------------------------------------------------

print(fig2)

# Save figure
ggsave(
  filename = "figure2_gdp_gender_gap_2024.png",
  plot = fig2,
  path = "results/figures",
  width = 9,
  height = 6,
  dpi = 300,
  device = "png"
)


# ============================================================
# 9. Statistical checks
# ============================================================

# Correlation between GDP per capita and gender gap
correlation_fig2 <- cor(
  fig2_data$gdp_pc_10k,
  fig2_data$gender_gap,
  use = "complete.obs"
)

cat(
  "\nCorrelation between GDP per capita and gender gap:",
  round(correlation_fig2, 3),
  "\n"
)


# Linear regression
model_fig2 <- lm(
  gender_gap ~ gdp_pc_10k,
  data = fig2_data
)

cat(
  "\nRegression coefficient on GDP per capita:",
  round(
    coef(model_fig2)["gdp_pc_10k"],
    3
  ),
  "\n"
)


# Regression summary
print(summary(model_fig2))

ggsave(
  filename = "results/figures/figure2_gdp_gender_gap_2024.png",
  plot = fig2,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# FIGURE 3
# Change in gender gap between 1990 and 2024
# All countries
# ============================================================

# Create the change in gender gap for all countries
fig3_data <- analysis_data |>
  filter(
    year %in% c(1990, 2024),
    !is.na(gender_gap)
  ) |>
  select(
    iso3c,
    country,
    year,
    gender_gap
  ) |>
  group_by(
    iso3c,
    country
  ) |>
  summarise(
    gap_1990 = gender_gap[year == 1990][1],
    gap_2024 = gender_gap[year == 2024][1],
    .groups = "drop"
  ) |>
  # Keep only countries with observations in both years
  filter(
    !is.na(gap_1990),
    !is.na(gap_2024)
  ) |>
  mutate(
    gap_change = gap_2024 - gap_1990,
    country = reorder(country, gap_change)
  )


# ------------------------------------------------------------
# Figure 3
# ------------------------------------------------------------

fig3 <- ggplot(
  fig3_data,
  aes(
    x = gap_change,
    y = country
  )
) +
  
  # Zero line: no change in the gender gap
  geom_vline(
    xintercept = 0,
    color = "gray55",
    linewidth = 0.5
  ) +
  
  # Horizontal bars
  geom_col(
    fill = "#2b5c8f",
    width = 0.65
  ) +
  
  # Reference labels
  scale_x_continuous(
    breaks = seq(
      floor(min(fig3_data$gap_change, na.rm = TRUE) / 5) * 5,
      ceiling(max(fig3_data$gap_change, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    expand = expansion(
      mult = c(0.02, 0.02)
    )
  ) +
  
  labs(
    title = "Change in the Gender Gap in Labor Force Participation, 1990–2024",
    subtitle = "Countries are ranked by the change in the male–female participation gap",
    x = "Change in gender gap (percentage points)",
    y = NULL,
    caption = paste0(
      "Source: World Bank, World Development Indicators; ",
      "ILO modeled estimates. Negative values indicate a narrowing gap."
    )
  ) +
  
  custom_theme +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      size = 7,
      color = "gray20"
    ),
    axis.text.x = element_text(
      size = 9
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "gray88",
      linewidth = 0.3
    ),
    plot.margin = margin(
      10, 20, 10, 10
    )
  )


# Display
print(fig3)


# ------------------------------------------------------------
# Save Figure 3
# ------------------------------------------------------------

ggsave(
  filename = "results/figures/figure3_gender_gap_change_all_countries.png",
  plot = fig3,
  width = 10,
  height = 18,
  dpi = 300
)


# ============================================================
# 6. Confirmation
# ============================================================

cat("\nVisualization completed successfully.\n")

cat("\nFigures saved to:\n")
cat("results/figures/figure1_gender_gap_trends.png\n")
cat("results/figures/figure2_gdp_gender_gap_2024.png\n")
cat("results/figures/figure3_gender_gap_change.png\n")
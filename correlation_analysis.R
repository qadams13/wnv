library(dplyr)
library(tidyr)
library(lubridate)


#read in WNND data and climate data
climate_df <- read.csv("neon_climate_monthly.csv") %>%
  select(!X)
climate_df %>% filter(year == 2004, month %in% c(10,11,12))

wnnd_df <- read.csv("neon_wnv_annual.csv")%>%
  select(!X)

wnnd_df <- wnnd_df %>%
  mutate(log_cases = log(cases))  #can use +1 to avoid -Inf

# Create adjusted year for climate variables
climate_lagged <- climate_df %>%
  mutate(
    climate_year = if_else(month >= 10, year + 1L, year)  # assign Oct–Dec to next year's WNND
  ) %>%
  filter(month %in% c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7))  # restrict to Oct–Jul

# Merge climate and WNND datasets using climate_year = WNND year
climate_cases <- climate_lagged %>%
  left_join(wnnd_df, by = c("neon_region", "climate_year" = "year"))

climate_cases %>% filter(climate_year == 2005) %>%
  count(year, month)

climate_long <- climate_cases %>%
  pivot_longer(
    cols = c(pdsi_wt, temp_wt, dpt_wt, prec_wt, vpd_wt, soil_wt),
    names_to = "climate_var",
    values_to = "climate_value"
  )

cor_results <- climate_long %>%
  group_by(neon_region, month, climate_var) %>%
  summarise(
    n_obs = n(),
    cor = cor(climate_value, cases, use = "complete.obs", method = "pearson"),
    .groups = "drop"
  )

#visualize strongest predictors

library(ggplot2)

cor_results %>%
  ggplot(aes(x = factor(month), y = cor, color = climate_var)) +
  geom_line(aes(group = climate_var)) +
  facet_wrap(~neon_region) +
  theme_minimal() +
  labs(x = "Month", y = "Pearson Correlation with WNND cases",
       title = "Climate-WNND Correlations by NEON Region")

ggsave("correlation_results.pdf", plot = last_plot())


#generate heat maps (from the supplement )

library(ggplot2)
library(dplyr)
library(forcats)

# Month labels
month_labels <- c(
  "10" = "prev. Oct", "11" = "prev. Nov", "12" = "prev. Dec",
  "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
  "5" = "May", "6" = "Jun", "7" = "Jul"
)

month_order <- c("prev. Oct", "prev. Nov", "prev. Dec",
                 "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")


# Apply month labels and ordering
cor_results_plot <- cor_results %>%
  mutate(
    month_label = month_labels[as.character(month)],
    month_label = factor(month_label, levels = month_order),
    label = sprintf("%.2f", cor)
  )

# Optional: add highlight for strongest correlation per region
cor_results_plot <- cor_results_plot %>%
  group_by(neon_region) %>%
  mutate(
    abs_cor = abs(cor),
    cor_rank = rank(-abs_cor, ties.method = "first"),  # 1 = highest
    highlight = cor_rank == 1,        # highest abs(cor)
    highlight2 = cor_rank == 2        # second highest abs(cor)
  ) %>%
  ungroup()


# Plot per NEON region
region_list <- unique(cor_results_plot$neon_region)

for (r in region_list) {
  region_data <- cor_results_plot %>%
    filter(neon_region == r, !is.na(cor))
  
  p <- ggplot(region_data, aes(x = month_label, y = fct_rev(climate_var), fill = cor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3) +
    
    # Color scale
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white", midpoint = 0,
      limits = c(-1, 1), name = "correlations"
    ) +
    
    # Black outline for highest absolute correlation
    geom_tile(
      data = region_data %>% filter(highlight),
      color = "black", size = 1.2, fill = NA
    ) +
    
    # Grey outline for second-highest absolute correlation
    geom_tile(
      data = region_data %>% filter(highlight2),
      color = "grey40", size = 1.2, fill = NA
    ) +
    
    # Labels and theme
    labs(
      title = paste("NEON Region", r, "Correlation Heat Map"),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


#filtering to 2022
wnnd_df_filter <- wnnd_df %>%
  filter(year <= 2022)

climate_lagged_filter <- climate_df %>%
  mutate(
    climate_year = if_else(month >= 10, year + 1L, year)
  ) %>%
  filter(month %in% c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)) %>%
  filter(climate_year <= 2022)

# Merge climate and WNND datasets using climate_year = WNND year
climate_cases_filter <- climate_lagged_filter %>%
  left_join(wnnd_df_filter, by = c("neon_region", "climate_year" = "year"))

climate_long_filter <- climate_cases_filter %>%
  pivot_longer(
    cols = c(pdsi_wt, temp_wt, dpt_wt, prec_wt, vpd_wt, soil_wt),
    names_to = "climate_var",
    values_to = "climate_value"
  )

cor_results_filter <- climate_long_filter %>%
  group_by(neon_region, month, climate_var) %>%
  summarise(
    n_obs = n(),
    cor = cor(climate_value, log_cases, use = "complete.obs", method = "pearson"),
    .groups = "drop"
  )
cor_results_plot_filter <- cor_results_filter %>%
  mutate(
    month_label = month_labels[as.character(month)],
    month_label = factor(month_label, levels = month_order),
    label = sprintf("%.2f", cor)
  )

cor_results_plot_filter <- cor_results_plot_filter %>%
  group_by(neon_region) %>%
  mutate(
    abs_cor = abs(cor),
    cor_rank = rank(-abs_cor, ties.method = "first"),  # 1 = highest
    highlight = cor_rank == 1,        # highest abs(cor)
    highlight2 = cor_rank == 2        # second highest abs(cor)
  ) %>%
  ungroup()

# Define climate groups
climate_groups <- list(
  "air" = c("temp_wt", "dpt_wt", "vpd_wt"),
  "soil" = c("soil_wt", "pdsi_wt"),
  "precip" = c("prec_wt")
)

# Reverse mapping: variable -> group
var_to_group <- unlist(lapply(names(climate_groups), function(g) {
  setNames(rep(g, length(climate_groups[[g]])), climate_groups[[g]])
}))

# Add group info to correlation results
cor_results_plot_filter <- cor_results_plot_filter %>%
  mutate(group = var_to_group[climate_var])

# Get top 2 (from different groups) per region
highlight_vars_df <- cor_results_plot_filter %>%
  filter(!is.na(cor)) %>%
  group_by(neon_region) %>%
  arrange(desc(abs(cor))) %>%
  summarise(
    primary_var = climate_var[1],
    primary_month = month[1],
    primary_group = group[1],
    secondary_var = climate_var[group != group[1]][1],
    secondary_month = month[group != group[1]][1],
    .groups = "drop"
  )

region_list <- unique(cor_results_plot_filter$neon_region)

for (r in region_list) {
  region_data <- cor_results_plot_filter %>%
    filter(neon_region == r, !is.na(cor))
  
  highlight_info <- highlight_vars_df %>% filter(neon_region == r)
  
  # Define which tiles to box
  region_data <- region_data %>%
    mutate(
      highlight = climate_var == highlight_info$primary_var & month == highlight_info$primary_month,
      highlight2 = climate_var == highlight_info$secondary_var & month == highlight_info$secondary_month
    )
  
  p <- ggplot(region_data, aes(x = month_label, y = fct_rev(climate_var), fill = cor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3) +
    
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white", midpoint = 0,
      limits = c(-1, 1), name = "correlations"
    ) +
    
    geom_tile(data = region_data %>% filter(highlight),
              color = "black", size = 1.2, fill = NA) +
    
    geom_tile(data = region_data %>% filter(highlight2),
              color = "grey40", size = 1.2, fill = NA) +
    
    labs(
      title = paste("NEON Region", r, "Correlation Heat Map"),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


library(dplyr)

# Define readable variable labels
var_labels <- c(
  temp_wt = "Mean Temp.",
  dpt_wt = "Dew Point",
  vpd_wt = "VPD Max.",
  soil_wt = "Soil Moisture",
  pdsi_wt = "PDSI",
  prec_wt = "Monthly Precip."
)

# Month labels
month_labels <- setNames(month.abb, 1:12)

# Create final summary table
top_predictor_table <- highlight_vars_df %>%
  mutate(
    primary_var_label = var_labels[primary_var],
    secondary_var_label = var_labels[secondary_var],
    primary_month_label = month_labels[as.character(primary_month)],
    secondary_month_label = month_labels[as.character(secondary_month)]
  ) %>%
  select(
    `NEON Region` = neon_region,
    `Primary Predictor` = primary_var_label,
    `Month` = primary_month_label,
    `Secondary Predictor` = secondary_var_label,
    `Month ` = secondary_month_label  # note space to avoid duplicate colname
  )

# View or export the table
print(top_predictor_table)

# First ensure the monthly climate data is in wide format (one column per var+month)
climate_wide <- climate_lagged_filter %>%
  mutate(month = sprintf("%02d", month)) %>%  # pad months BEFORE pivoting
  pivot_wider(
    id_cols = c(neon_region, climate_year),
    names_from = month,
    values_from = c(temp_wt, dpt_wt, vpd_wt, soil_wt, pdsi_wt, prec_wt),
    names_sep = "_"
  )

# Initialize output list
cor_between_vars <- list()

for (i in seq_len(nrow(highlight_vars_df))) {
  row <- highlight_vars_df[i, ]
  
  region <- row$neon_region
  var1 <- row$primary_var
  m1   <- row$primary_month
  var2 <- row$secondary_var
  m2   <- row$secondary_month
  
  col1 <- paste0(var1, "_", sprintf("%02d", m1))
  col2 <- paste0(var2, "_", sprintf("%02d", m2))
  
  # Print the column names for debugging
  message("Region: ", region, 
          " | col1: ", col1, 
          " | col2: ", col2)
  
  # Check that both columns exist
  if (!(col1 %in% names(climate_wide)) | !(col2 %in% names(climate_wide))) {
    message("Skipping region ", region, ": missing columns")
    next
  }
  
  sub_df <- climate_wide %>%
    filter(neon_region == region) %>%
    select(all_of(c(col1, col2))) %>%
    drop_na()
  
  message("  N obs: ", nrow(sub_df), " | Mean(col1): ", mean(sub_df[[1]]), 
          " | Mean(col2): ", mean(sub_df[[2]]))
  
  cor_val <- if (nrow(sub_df) > 1) cor(sub_df[[1]], sub_df[[2]]) else NA
  
  cor_between_vars[[i]] <- tibble(
    neon_region = region,
    primary_var = var1,
    secondary_var = var2,
    cor_between_predictors = round(cor_val, 3)
  )
}

predictor_cor_table <- bind_rows(cor_between_vars)

# Combine into final table
predictor_cor_table <- bind_rows(cor_between_vars)

final_table <- top_predictor_table %>%
  left_join(predictor_cor_table, by = c(
    "NEON Region" = "neon_region"))%>%
  dplyr::select(`NEON Region`, `Primary Predictor`, Month, `Secondary Predictor`, `Month `, cor_between_predictors )


# save table
library(gt)

final_table %>%
  gt() %>%
  gtsave("top_climate_predictors_by_region.png")


#creating maps!
neon_contig <- neon_shapefile %>%
  st_crop(xmin = -125, xmax = -66, ymin = 20, ymax = 50)


for (v in unique(cor_results_filter$climate_var)) {
  for (m in c(10, 1, 4, 7)) {  # e.g., Oct, Jan, Apr, Jul
    plot_df <- cor_results_filter %>%
      filter(climate_var == v, month == m)
    
    map_data <- neon_contig %>%
      left_join(plot_df, by = c("OBJECTID" = "neon_region"))
    
    p <- ggplot(map_data) +
      geom_sf(aes(fill = cor), color = "white") +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", midpoint = 0,
        na.value = "grey80", limits = c(-0.6, 0.6), name = "Correlation"
      ) +
      labs(title = paste0(v, " – ", month.name[m], " Correlation with WNND")) +
      theme_minimal()
    
    print(p)
  }
}



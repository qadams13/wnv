library(dplyr)
library(tidyr)
library(lubridate)


#read in WNND data and climate data
climate_df <- read.csv("neon_climate_monthly.csv") %>%
  select(!X)
wnnd_df <- read.csv("neon_wnv_annual.csv")%>%
  select(!X)
# Create adjusted year for climate variables
climate_lagged <- climate_df %>%
  mutate(
    climate_year = if_else(month >= 10, year + 1L, year)  # shift Oct–Dec forward
  ) %>%
  filter(month %in% c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7))  # Oct–Jul

# Merge climate and WNND datasets using climate_year = WNND year
climate_cases <- climate_lagged %>%
  left_join(wnnd_df, by = c("neon_region", "climate_year" = "year"))

climate_long <- climate_cases %>%
  pivot_longer(
    cols = c(pdsi_wt, temp_wt, dpt_wt, prec_wt, vpd_wt),
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
    max_abs_cor = max(abs(cor), na.rm = TRUE),
    highlight = abs(cor) == max_abs_cor
  ) %>%
  ungroup()

# Plot per NEON region
region_list <- unique(cor_results_plot$neon_region)

for (r in region_list) {
  p <- cor_results_plot %>%
    filter(neon_region == r) %>%
    filter(!is.na(cor)) %>%
    ggplot(aes(x = month_label, y = fct_rev(climate_var), fill = cor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                         limits = c(-1, 1), name = "correlations") +
    geom_tile(data = . %>% filter(highlight),
              color = "black", size = 1.2, fill = NA) +
    labs(
      title = paste("NEON Region", r, "Correlation Heat Map"),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

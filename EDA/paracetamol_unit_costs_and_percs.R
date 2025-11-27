library(dplyr)
library(ggplot2)

df <- mod_ch_flag_drug_df

para_df <- df %>% 
  filter(
    BNF_CHILD == "Paracetamol",
    METRIC %in% c("Mean drug cost PPM", "Mean prescription items PPM")
  ) %>% 
  tidyr::pivot_wider(names_from = METRIC, values_from = VALUE) %>% 
  arrange(CH_FLAG, FY) %>% 
  mutate(
    FY = as.integer(substr(FY, 1, 4)),
    CH_FLAG = case_match(CH_FLAG, 0 ~ "Non-CH", 1 ~ "CH"),
    UNIT_COST = `Mean drug cost PPM` / `Mean prescription items PPM`
  )

overall_df <- df %>% 
  filter(
    BNF_PARENT == "Chemical substance",
    METRIC == "Mean drug cost PPM"
  ) %>% 
  tidyr::pivot_wider(names_from = METRIC, values_from = VALUE) %>% 
  summarise(OVERALL_MEAN_COST_PPM = sum(`Mean drug cost PPM`), .by = c(FY, CH_FLAG)) %>% 
  arrange(CH_FLAG, FY) %>% 
  mutate(CH_FLAG = case_match(CH_FLAG, 0 ~ "Non-CH", 1 ~ "CH"))

ggplot(para_df, aes(x = FY, y = UNIT_COST, color = CH_FLAG)) +
  geom_point() +       # Adds the points
  geom_line() +        # Optional: Adds lines to connect points by group
  theme_minimal() +    # Optional: Clean theme
  labs(
    title = "Paracetamol Unit Cost by Financial Year",
    x = "Financial Year",
    color = "Care Home Flag"
  )


# Convert flag to factor to generate numeric color codes
para_df$CH_FLAG_FCT <- as.factor(para_df$CH_FLAG)

plot(
  para_df$FY, 
  para_df$`Mean drug cost PPM`, 
  col = para_df$CH_FLAG_FCT,  # Colors based on the factor
  pch = 19,                   # Solid circles
  xlab = "FY",
  ylab = "Mean drug cost PPM"
)

# You also have to manually add a legend in base R
legend("topright", 
       legend = levels(para_df$CH_FLAG_FCT), 
       col = 1:length(levels(para_df$CH_FLAG_FCT)), 
       pch = 19)





library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Prepare data: Pivot longer and calculate an Index (Base 100)
viz_df <- para_df %>%
  # Select only the columns we need
  select(FY, CH_FLAG, `Mean drug cost PPM`, `Mean prescription items PPM`) %>% 
  # Pivot longer so we can group by metric
  pivot_longer(
    cols = c(`Mean drug cost PPM`, `Mean prescription items PPM`), 
    names_to = "METRIC", 
    values_to = "VALUE"
  ) %>% 
  # Group by Flag and Metric to calculate change over time relative to start
  group_by(CH_FLAG, METRIC) %>% 
  arrange(FY) %>% 
  mutate(
    # Set the first year to 100, calculate rest relative to that
    INDEX = (VALUE / first(VALUE)) * 100
  ) %>% 
  ungroup()

# 2. Plot
ggplot(viz_df, aes(x = FY, y = INDEX, color = METRIC)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  # Create separate panels for Care Home vs Non-Care Home
  facet_wrap(~CH_FLAG) + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Relative Change: Paracetamol Cost vs Items",
    subtitle = "Base 100 = Start of Period. Divergence indicates different rates of change.",
    y = "Index (Year 1 = 100)",
    x = "Financial Year",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")





library(dplyr)

month_df <- mod_headline_figures_df %>% 
  filter(TYPE == "Monthly sum")

month_ts <- ts(month_df$PATS, frequency = 12, start = c(2020, 4))

month_decomp <- decompose(month_ts)  

plot(month_decomp)

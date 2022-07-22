require(tidyverse)
require(shorts)

# Load data set
load("simulation-results.RData")

# Relationship between MAC and MSS
sim_res_df_wide <- sim_res_df %>%
  select(MSS, MAC, flying_distance, model, parameter, estimate) %>%
  rename(true_MSS = MSS, true_MAC = MAC) %>%
  pivot_wider(
    id_cols = c("true_MSS", "true_MAC", "flying_distance", "model"),
    names_from = "parameter",
    values_from = "estimate")

# Relationship at flying distance = 0
sim_res_df_wide %>%
  filter(flying_distance == 0) %>%
  ggplot(aes(x = MSS, y = MAC)) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = lm) +
  facet_wrap(~model)

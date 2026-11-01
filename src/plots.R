library(tidyverse)

# Power threshold: below 0.8 there is less than an 80% chance that we notice
# a relationship that really exists, so we cannot trust the result
power_threshold <- 0.8

# setting a palette
our_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#003964"),
    plot.subtitle = element_text(size = 12, color = "#2C3E50"),
    axis.text = element_text(color = "#2C3E50"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# 1. Heatmap of Pearson correlations

# correlation matrix of event_split and all analysed variables
# (pairwise: each pair uses the rows where both values are not NA)
vars <- results_pearson$variable
corr_matrix <- cor(df[ , c("event_split", vars)], use = "pairwise.complete.obs")

corr_df <- as.data.frame(corr_matrix)
corr_df$var1 <- rownames(corr_df)

corr_long <- corr_df %>%
  pivot_longer(cols = -var1, names_to = "var2", values_to = "r")

ggp_heat <- corr_long %>%
  ggplot(aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "#00BBCE", mid = "white", high = "#F15B43",
                       limits = c(-1, 1)) +
  labs(title = "Pearson correlations between band characteristics",
       subtitle = "event_split: 1 = Split-up, 0 = Active",
       x = NULL, y = NULL, fill = "Pearson r",
       caption = "Source: Metal Archives, October 2024") +
  our_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggp_heat

# 2. Bar chart of power with a dashed line at 0.8
ggp_power <- results_pearson %>%
  mutate(enough = ifelse(power >= power_threshold, "power >= 0.8", "power < 0.8")) %>%
  ggplot(aes(x = power, y = reorder(variable, power), fill = enough)) +
  geom_col() +
  geom_text(aes(label = round(power, 2)), hjust = -0.2, size = 3) +
  geom_vline(xintercept = power_threshold, linetype = "dashed") +
  scale_fill_manual(values = c("power >= 0.8" = "#00665A",
                               "power < 0.8" = "#F15B43")) +
  xlim(0, 1.1) +
  labs(title = "Statistical power of each variable",
       subtitle = "Dashed line: power = 0.8",
       x = "Power", y = NULL,
       caption = "Source: Metal Archives, October 2024") +
  our_theme +
  theme(legend.position = "bottom", legend.title = element_blank())

ggp_power

# 3. Saving both charts
dir.create("graphs", showWarnings = FALSE)

ggsave("graphs/pearson_heatmap.png", ggp_heat,
       width = 20, height = 14, units = "cm", bg = "white")
ggsave("graphs/power_bars.png", ggp_power,
       width = 20, height = 12, units = "cm", bg = "white")

library(tidyverse)

setwd("C:/Users/Pochtarusa/Downloads/archive (5)")

bands <- read_csv("metal_bands.csv", show_col_types = FALSE)



noun_re <- "(Metal|Rock|core|grind|Punk|Crust|Noise|Ambient|Crossover|NWOBHM|Other)$"

split_genre <- function(s) {
  p <- trimws(unlist(strsplit(s, "[/,]")))
  p <- p[p != ""]

  if (length(p) > 1) {
    for (i in (length(p) - 1):1) {
      if (!grepl(noun_re, p[i], ignore.case = TRUE)) {
        w <- strsplit(p[i + 1], " ")[[1]]
        p[i] <- paste(p[i], if (length(w) > 1) paste(w[-1], collapse = " ") else "Metal")
      }
    }
  }

  p
}

top_n_genres <- 15

genre_df <- bands %>%
  distinct() %>%
  filter(!is.na(Genre), !is.na(Status), Status != "Disputed") %>%
  mutate(
    Primary_Genre = map_chr(Genre, ~ split_genre(.x)[1]),
    Genre_group = fct_lump_n(Primary_Genre, n = top_n_genres, other_level = "Other genres"),
    Band_Status = factor(Status,
                         levels = c("Active", "On hold", "Changed name", "Unknown", "Split-up"))
  )


                      

# Chi-square test of independence

tab <- table(genre_df$Genre_group, genre_df$Band_Status)

chi <- chisq.test(tab)

if (any(chi$expected < 5)) {
  chi <- chisq.test(tab, simulate.p.value = TRUE, B = 9999)
}

# Cramer's V
n <- sum(tab)
cramers_v <- sqrt(chi$statistic / (n * min(dim(tab) - 1)))

# Results
cat("Chi-square:", round(chi$statistic, 2), "\n")
cat("Degrees of freedom:", chi$parameter, "\n")
cat("P-value:", format.pval(chi$p.value, digits = 3), "\n")
cat("Cramer's V:", round(cramers_v, 3), "\n")

cat(
  "Conclusion:",
  if (chi$p.value < 0.05) {
    "There is a statistically significant association between genre and band status."
  } else {
    "There is no statistically significant association between genre and band status."
  },
  "\n"
)


# Graph

genre_distribution <- genre_df %>%
  count(Genre_group, Band_Status) %>%
  group_by(Genre_group) %>%
  mutate(
    total = sum(n),
    percentage = n / total * 100,
    active_share = sum(n[Band_Status == "Active"]) / total
  ) %>%
  ungroup() %>%
  mutate(
    label = fct_reorder(Genre_group, active_share)
  )

overall_active <- mean(genre_df$Band_Status == "Active") * 100

p_text <- if (chi$p.value < 0.001) {
  "p < 0.001"
} else {
  sprintf("p = %.3f", chi$p.value)
}

subtitle_text <- sprintf(
  "\u03c7\u00b2(%d) = %.0f, %s, Cram\u00e9r's V = %.2f\nDashed line = share of active bands overall (%.0f%%)",
  chi$parameter, chi$statistic, p_text, cramers_v, overall_active
)

genre_plot <- ggplot(
  genre_distribution,
  aes(x = percentage, y = label, fill = Band_Status)
) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = overall_active, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "Active"       = "#20C997",
      "On hold"      = "#FFB547",
      "Changed name" = "#B197FC",
      "Unknown"      = "#739BFF",
      "Split-up"     = "#F06A6A"
    )
  ) +
  labs(
    title = "Does Band Status Distribution Vary Across Metal Genres?",
    subtitle = subtitle_text,
    x = NULL,
    y = NULL,
    fill = "Band status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

print(genre_plot)

ggsave(
  "genre_band_status_plot.png",
  genre_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)



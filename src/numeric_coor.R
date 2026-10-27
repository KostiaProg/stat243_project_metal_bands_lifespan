library(tidyverse)
library(broom)
library(pwr)

analyze_var <- function(df, var_name) {
  sub_data <- df %>%
    filter(!is.na(.data[[var_name]]), !is.na(event_split)) # filter NAs
  
  # group sample sizes
  n_active <- sum(sub_data$event_split == 0)
  n_split <- sum(sub_data$event_split == 1)
  
  if (n_active < 2 || n_split < 2) return(NULL)
  
  # pearson correlation test
  cor_res <- cor.test(sub_data$event_split, sub_data[[var_name]], method = "pearson")
  r_val <- unname(cor_res$estimate) # direction how value affects splitting-up (0 - no effect, 1 - positive coor, -1 - negative)
  
  
  # convert Point-Biserial r to Cohen's d: d = (2 * r) / sqrt(1 - r^2)
  d_val <- if (abs(r_val) < 1) (2 * r_val) / sqrt(1 - r_val^2) else NA_real_
  
  # power for unequal sample sizes (pwr.t2n.test)
  power_val <- if (!is.na(d_val) && abs(d_val) > 0) {
    pwr.t2n.test(
      n1 = n_active,
      n2 = n_split,
      d  = abs(d_val),
      sig.level = 0.05
    )$power
  } else {
    1.00 # perfect power if effect/sample size is extreme
  }
  
  # tidy row with variables
  tibble(
    variable = var_name,
    n_active = n_active,
    n_split = n_split,
    
    t_statistic = cor_res$statistic,
    p_value = cor_res$p.value,
    
    ci_lower = cor_res$conf.int[1],
    ci_upper = cor_res$conf.int[2],
    
    r_point_biserial = r_val,
    cohens_d = d_val,
    power = power_val
  )
}

analysis_pearon <- function(df, target_vars = c(
  "mean_release_gap_years",
  "median_release_gap_years",
  "mean_album_ep_release_gap_years",
  "median_album_ep_release_gap_years",
  "single_count",
  "demo_count",
  "total_reviews",
  "mean_review_score"
)) {
  pb_power_results <- map_dfr(target_vars, ~ analyze_var(df, .x))
  
  # DISPLAY, REPLACE WITH GRAPHS
  pb_power_results %>%
    arrange(p_value) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    print(width = Inf)
  
  pb_power_results
}

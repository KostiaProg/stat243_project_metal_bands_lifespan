library(tidyverse)
library(dplyr)
source("src/helpers.R")

get_data <- function() {
  bands <- read_csv("data/complete_roster.csv")
  discography <- read_csv("data/all_bands_discography.csv")
  
  # add fields and summarizations
  discog_features <- discography %>%
    mutate(
      Year = as.numeric(Year),
      is_album_ep = Type %in% c("Full-length", "EP"),
      review_count = ifelse(Reviews == "No Reviews", 0, as.numeric(str_extract(Reviews, "^\\d+"))), # get count reviews
      review_score = as.numeric(str_extract(Reviews, "(?<=\\()\\d+(?=%\\))")) # get score, is 0 reviews NA
    ) %>%
    arrange(`Band ID`, Year) %>%
    select(-c(`Album Name`)) %>%
    group_by(`Band ID`) %>%
    summarize(
      # counts
      total_releases_count = n(),
      full_length_ep_count = sum(is_album_ep, na.rm = TRUE),
      single_count = sum(Type == "Single", na.rm = TRUE),
      demo_count = sum(Type == "Demo", na.rm = TRUE),
      split_count = sum(Type == "Split", na.rm = TRUE),
      
      # first and last release year BY RELEASES (mention this part in methodology)
      first_release_yr = suppressWarnings(min(Year, na.rm = TRUE)),
      last_release_yr  = suppressWarnings(max(Year, na.rm = TRUE)),
      
      # release gaps
      mean_release_gap_years = calc_gap(Year, "mean"),
      median_release_gap_years = calc_gap(Year, "median"),
      mean_album_ep_release_gap_years = calc_gap(Year[is_album_ep], "mean"),
      median_album_ep_release_gap_years = calc_gap(Year[is_album_ep], "median"),
      
      # reviews
      total_reviews = sum(review_count, na.rm = TRUE),
      mean_review_score = mean(review_score, na.rm = TRUE),
      
      .groups = "drop" # the same as .ungroup()
    ) %>%
    filter(total_releases_count > 0) %>% # IMPORTANT! We do not count bands without releases, mention it in methodology
    mutate(
      # Replace NAs with 0
      full_length_ep_count = coalesce(full_length_ep_count, 0),
      single_count = coalesce(single_count, 0),
      demo_count = coalesce(demo_count, 0),
      split_count = coalesce(split_count, 0),
      
      # just years lived based on discography
      years_lived = last_release_yr - first_release_yr,
      
      # Percentages of releases
      full_length_ep_perc = full_length_ep_count / total_releases_count,
      singles_perc = single_count / total_releases_count,
      demo_perc = demo_count / total_releases_count,
      split_perc = split_count / total_releases_count
    )
  
  # combine and filter
  df <- bands %>%
    select(-c("Name_x", "URL", "Photo_URL", `Label ID`, "Specialization")) %>%
    rename("Label" = "Name_y") %>%
    left_join(discog_features, by = 'Band ID') %>%
    filter(!is.na(total_releases_count)) %>%
    remove_outliers(c("full_length_ep_count", "single_count", "demo_count", "split_count", "total_reviews")) %>% # mb remove total_reviews, need to check
    mutate(event_split = ifelse(Status_x == "Split-up", 1, 0))
  
  df
}

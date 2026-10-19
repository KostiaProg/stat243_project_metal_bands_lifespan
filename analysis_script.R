library(tidyverse)
library(ggplot2)
library(dplyr)

bands <- read_csv("data/complete_roster")
discography <- read_csv("data/all_bands_discography")

calc_gap <- function(years, stat = c("mean", "median")) {
  valid_yrs <- sort(years[!is.na(years)])
  if (length(valid_yrs) < 2) return(NA_real_)
  
  gaps <- diff(valid_yrs)
  if (match.arg(stat) == "mean") mean(gaps) else median(gaps)
}


discog_features <- discography %>%
  mutate(
    year_clean = as.numeric(Year),
    is_album_ep = Type %in% c("Full-length", "EP"),
    review_count = ifelse(Reviews == "No Reviews", 0, as.numeric(str_extract(Reviews, "^\\d+"))), # get count reviews
    review_score = as.numeric(str_extract(Reviews, "(?<=\\()\\d+(?=\%\\))")) # get score, is 0 reviews NA
  ) %>%
  arrange(`Band ID`, year_clean) %>%
  select(-c("Album Name")) %>%
  group_by("Band ID") %>%
  summarize(
    # counts
    total_releases_count = n(),
    full_length_ep_count = sum(is_album_ep, na.rm = TRUE),
    singles_count = sum(Type == "Single", na.rm = TRUE),
    demo_count = sum(Type == "Demo", na.rm = TRUE),
    split_count = sum(Type == "Split", na.rm = TRUE),
    
    # first and last release
    first_release_yr = suppressWarnings(min(year_clean, na.rm = TRUE)),
    last_release_yr = suppressWarnings(max(year_clean, na.rm = TRUE)),
    
    # avg gaps for any release and album/ep
    mean_release_gap_years = calc_gap(year_clean, "mean"),
    median_release_gap_years = calc_gap(year_clean, "median"),
    mean_album_ep_release_gap_years = calc_gap(year_clean[is_album_ep], "mean"),
    median_album_ep_release_gap_years = calc_gap(year_clean[is_album_ep], "median"),
    
    # reviews
    total_reviews = sum(review_count, na.rm = TRUE),
    mean_review_score = mean(review_score, na.rm = TRUE),
    .groups = "drop" # same as ungroup()
  ) %>%
  mutate(
    # percentages of releases
    full_length_ep_perc = full_length_ep_count / total_releases_count,
    singles_perc = singles_count / total_releases_count,
    demo_perc = demo_count / total_releases_count,
    split_perc = split_count / total_releases_count,
    
    # handle NAs in years
    first_release_yr = ifelse(is.finite(first_release_yr), first_release_yr, NA),
    last_release_yr = ifelse(is.finite(last_release_yr), last_release_yr, NA),
    avg_score_clean = ifelse(is.na(mean_review_score), NA, mean_review_score)
  )

df <- bands %>%
  select(-c("URL", "Photo_URL", "Label ID", "Specialization")) %>%
  left_join(discog_features, by = "Band ID") %>%
  mutate(
    # fill NAs for bands with no discography logged of specific count
    total_releases_count = ifelse(is.na(total_releases_count), 0, total_releases_count),
    full_length_ep_count = ifelse(is.na(full_length_ep_count), 0, full_length_ep_count)
  )


### Main analysis variable - status == "Split-up" and status == "Active". Active in our Survival analysis is Type 1 right-censored event



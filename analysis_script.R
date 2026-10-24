library(tidyverse)
library(ggplot2)
library(dplyr)

source("src/get_data.R")
source("src/numeric_coor.R")

# get data
df <- get_data()

### Main analysis variable - status == "Split-up" and status == "Active". Active in our Survival analysis is Type 1 right-censored event
results_pearson <- analysis_pearon()
library(tidyverse)
# read in adjusted transitions

tr <- read_csv("https://github.com/timriffe/EDSD2025health/raw/refs/heads/master/data/adj_transitions.csv.gz") |> 
  filter(time == 2015,
         condition == "adl") |> 
  pivot_wider(names_from = sex,
              values_from = p) |> 
  mutate(delta = female - male,
         p_avg = (female + male) / 2)

# calculate sensitivity at average parameters
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/refs/heads/master/R/00_functions_classic.R")
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/refs/heads/master/R/00_sensitivity_functions.R")






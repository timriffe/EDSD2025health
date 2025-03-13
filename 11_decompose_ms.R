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

tr |> 
  rename(p = p_avg) |> 
  s2t(init = c(H=1,U=0), expectancy = "h") |> 
  filter(transition != "init") |> 
  mutate(age = age + 20) |> 
  left_join(tr, by = join_by(age, transition)) |> 
  mutate(dec = effect * delta) |> 
  ggplot(aes(x = age, y = dec, color = transition)) +
  geom_line()
  # group_by(transition) |> 
  # summarize(dec = sum(dec, na.rm = TRUE))









library(tidyverse)
# read in the file called 
"adj_transitions.csv.gz"
tr <- read_csv("https://github.com/timriffe/EDSD2025health/raw/refs/heads/master/data/adj_transitions.csv.gz")
# Exercise: read this in and make a plot
# of UD / HD for some year, sex, condition.
head(tr)
tr |> 
  filter(transition %in% c("HD","UD")) |> 
  pivot_wider(names_from = transition,
              values_from = p) |> 
  filter(condition == "gali") |> 
  mutate(ratio = UD / HD) |> 
  ggplot(aes(x = age, y = ratio, color = sex, linetype = as.factor(time))) +
  geom_line() +
  theme_minimal()







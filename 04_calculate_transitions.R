
library(tidyverse)

# 1 read in counts.csv.gz usig read_csv()

counts <- read_csv("https://github.com/timriffe/EDSD2025health/raw/refs/heads/master/data/counts.csv.gz")
head(counts)

# 2 calculate denominators by summing transitions
# originating from a state and age (these are exhaustive)
counts |> 
  group_by(condition, sex, time, age, from) |> 
  mutate(denom = sum(n)) |> 
  ungroup() |> 
  mutate(p = n / denom) |> 
  filter(condition == "srh",
         sex == "male",
         time == 2013) |> 
  mutate(transition = paste0(from, to)) |> 
  filter(!transition %in% c("HH","UU")) |> 
  ggplot(aes(x = age, y = p, color = transition)) +
  geom_point() +
  geom_smooth()

# 3 calculate transitions

# 4 make some exploratory plots, try to understand what the
# transitions mean. These could be noisy, so maybe geom_point() + geom_smooth() would help.

# 5 if you wanted to calculate interesting ratios,
# like UD / HD, then you'd probably want smoothed or modeled
# probabilities

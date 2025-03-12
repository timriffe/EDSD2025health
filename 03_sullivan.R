
# refer to lifetables calculated in 01 and prevalence in 02

# 1 notice that prevalence is in 2-year age groups,
# but lifetables are in single years.
# to join on age, you need like age groups
# either you gradaute prevalence (possibly smoothing at same time),
# or you group lifetables to 2-year age groups. What'll it be


# 1.1 smooth/graduate prevalence

# 1.2 group lifetables to 2-year age groups (just need Lx, but sure to make lt start at 50!)
source("01_lifetables.R")
source("02_prevalence.R")

lt_sullivan <-
  lifetables |> 
  filter(age >= 50) |> 
  select(sex, time, age, lx, Lx) |> 
  group_by(time, sex) |> 
  mutate(Lx = Lx / lx[age == 50],
         age = age - age %% 2) |> 
  group_by(time, sex, age) |> 
  summarize(Lx = sum(Lx), .groups = "drop")



# 2 join prev and lifetable (either single or 2-yr ages)
prev <- 
  prev |> 
  complete(condition, sex, time, age, 
           fill = list(prev = 0))
prev |> 
  left_join(lt_sullivan, by = join_by(sex, time, age)) |> 
  group_by(sex, time, condition) |> 
  summarize(HLE = sum(Lx * (1 - prev)),
            ULE = sum(Lx * prev),
            LE = sum(Lx)) |> 
  filter(condition != "chron") |> 
  mutate(compression = ULE / LE) |> 
  ggplot(aes(x= time, y = compression, color = sex)) +
  geom_line()+
  facet_wrap(~ condition)

# equivalent
# prev |> 
#   left_join(lt_sullivan, by = c("sex", "time", "age")) 

# Challenge:

# Use the Nusselder-Looman decomposition method
#https://www.proquest.com/docview/222945248?pq-origsite=gscholar&fromopenview=true&sourcetype=Scholarly%20Journals
# (expand the box to see the formulas)

# https://doi.org/10.1353/dem.2004.0017

prev |> 
  left_join(lt_sullivan, by = join_by(sex, time, age)) |> 
  filter(condition == "adl",
         time == 2015) |> 
  select(-H, -U) |> 
  pivot_wider(names_from = sex, values_from =c(prev, Lx)) |> 
  mutate(pd = (1-prev_female) - (1-prev_male),
         pm = ((1-prev_female) + (1-prev_male)) / 2,
         Ld = Lx_female - Lx_male,
         Lm = (Lx_female + Lx_male) / 2,
         DIS = pd * Lm,
         MOR = Ld * pm) |> 
  summarize(DIS = sum(DIS),
            MOR = sum(MOR))




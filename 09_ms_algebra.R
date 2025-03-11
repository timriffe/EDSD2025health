library(tidyverse)

# some custom functions
source("06_ms_util_functions.R")

# Read in the data (Daniel Schneider gave these to me, haha)
TR <- read_csv("data/adj_transitions.csv.gz")

# define a subset, reshape to wider along transitions
TRsub <-
  TR |> 
  filter(sex == "female",
         time == 2015,
         condition == "adl") |> 
  pivot_wider(names_from = transition, values_from = p)

# starting proportions in each state
init <- c(H = 0.99, U = 0.01)

# Make the submatrices of U, the transient matrix
HH <- pi2u(pivec = TRsub$HH, from = "H", to = "H")
HU <- pi2u(pivec = TRsub$HU, from = "H", to = "U")
UH <- pi2u(pivec = TRsub$UH, from = "U", to = "H")
UU <- pi2u(pivec = TRsub$UU, from = "U", to = "U")

## we need to bind these like this:
## |-------|
## | HH UH |
## | HU UU |
## |-------|

U <- u2U(HH = HH, # healthy to healthy
         HU = HU, # healthy to unhealthy
         UH = UH, # unhealthy to healthy
         UU = UU) # unhealthy to unhealthy

# U |> View()
# so far this is all matrix architecture, now we have 
# the transient matrix, which we can transform to the 
# fundamental matrix
interval <- 1
N <- U2N(U,  interval = interval) #- 

# This is where we stop with the algebra and move to 
# tidyverse for book-keeping.

# This thing has what we want: conditional expected time spent in each
# state and age (conditional on survival and starting state!)
# There are matrix tricks to select what we want. But this is where
# we instead use tidyverse to grab what we need:
N20 <-
  N |>  
  as.data.frame() |> 
  rownames_to_column("to") |>
  pivot_longer(-to, 
               names_to = "from", 
               values_to = "time") |> 
  separate_wider_delim(cols = to, 
                       delim = "::", 
                       names = c("to","age2")) |> 
  separate_wider_delim(cols = from, 
                       delim = "::", 
                       names = c("from","age1")) |> 
  mutate(age1 = as.integer(age1),
         age2 = as.integer(age2)) |> 
  dplyr::filter(age1 == 20,
                age2 >= age1)
N20

# calculate HLE and ULE from it:
HLE <-
  N20 |> 
  group_by(from, to) |> 
  summarize(Ex_cond = sum(time), .groups = "drop") |> 
  mutate(init = if_else(from == "H", init["H"], init["U"]),
         Ex = Ex_cond * init) |> 
  group_by(to) |> 
  summarize(Ex = sum(Ex), .groups = "drop")
lxs <-
  N20 |> 
  mutate(init = if_else(from == "H", init["H"], init["U"]),
         lxs = time * init) |> 
  group_by(to, age2) |> 
  summarize(lxs = sum(lxs))
Hx2 <- 
  lxs |> 
  filter(to == "H") |> 
  pull(lxs)

HLE$Ex
# compare w result from 07_ms_id.R, exact match
c(sum(Hx),sum(Ux))



# -------------------------------------------
# Some extra code for weighting together
# an average survival curve and prevalence,
# the Sullivan goods
# no need to run this, the results are
# pasted into the top of the Sullivan script.
# ------------------------------------------


lx <- N50 |> 
  mutate(init = ifelse(from == "H", init["H"], init["U"]),
         time2 = time * init) |> 
  group_by(age2) |> 
  summarize(lx = sum(time2)) |> 
  pull(lx)

pix <-
  N50 |> 
  mutate(init = ifelse(from == "H", init["H"], init["U"]),
         time2 = time * init) |> 
  group_by(to, age2) |> 
  summarize(time2 = sum(time2),
            .groups = "drop") |> 
  pivot_wider(names_from = to, values_from = time2) |> 
  mutate(pix = H / (H + U)) |> 
  pull(pix)






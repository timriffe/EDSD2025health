# install.packages("markovchain")
library(markovchain)
library(tidyverse)

source("06_ms_util_functions.R")
TR <- read_csv("data/adj_transitions.csv.gz")

# define a subset, reshape to wider along transitions
TRsub <-
  TR |> 
  filter(sex == "female",
         time == 2015,
         condition == "adl") |> 
  pivot_wider(names_from = transition, values_from = p)

# starting proportions in each state
init <- c(H=.99,U=.01)

# Make the submatrices of U, the transient matrix
HH <- pi2u(pivec = TRsub$HH, from = "H", to = "H")
HU <- pi2u(pivec = TRsub$HU, from = "H", to = "U")
UH <- pi2u(pivec = TRsub$UH, from = "U", to = "H")
UU <- pi2u(pivec = TRsub$UU, from = "U", to = "U")


# terciary edu females
U <- u2U(HH = HH, # healthy to healthy
         HU = HU, # healthy to unhealthy
         UH = UH, # unhealthy to healthy
         UU = UU) # unhealthy to unhealthy


# complete the Markov matrix with Dead state,
# this turns it into a stochastic matrix (colSums = 1)
U <- cbind(U, 0)
U <- rbind(U, 1 - colSums(U)) # death probabilities are 1-sum(transient_probabilities) ...

# Need to name the dims, concatenating state and age
age_state   <- c(outer(20:111, paste0("::", c("H","U")), paste0), "Dead")
(dimnames(U) <- list(to = age_state, from = age_state))

# ---------------
# switch from Leslie-Caswell to standard Markov orientation!!!!
# Gotta remember this!!!
U <- t(U)
# ---------------

# make s4 transition matrix from markovchain package
# RTFM
mcHLE <- new("markovchain", 
             states = rownames(U),
             byrow = TRUE, 
             transitionMatrix = U,
             name = "HLE")

# how many sequences should we generate?

# n = time steps
# object = markovchain object
# t0 = starting state + age
# replicate it N times.


# If you want to account for an initial mixture, then
# Do this twice, and vary the size of N accordingly...

# init, using prevalence
N  <- 2e4
Ns <- round(init * N)
set.seed(2025)
n <- length(21:111)
RHRS_H  <- replicate(Ns["H"],
                     rmarkovchain(n = n, 
                                  object = mcHLE, 
                                  t0 = "20::H", 
                                  parallel = TRUE)
) 
RHRS_U  <- replicate(Ns["U"],
                     rmarkovchain(n = n, 
                                  object = mcHLE, 
                                  t0 = "20::U", 
                                  parallel = TRUE)
) 

# stick together in one population,
# note, t0 from the random generator is not included
# in the trajectories, so we need to append it to be 
# consistent with the other approaches.
RHRS                   <- cbind(rbind("H",RHRS_H), rbind("U",RHRS_U))

# only need the states, don't need the age part of the labels
RHRS_clean             <- gsub(".*:","",RHRS)
dim(RHRS_clean)
RHRS_clean[,1]
a                    <- 20:111
rownames(RHRS_clean) <- a
colnames(RHRS_clean) <- 1:ncol(RHRS_clean)

# see the first one:
(RHRS_clean[,1] == "H") %>% sum() 

# Calculate HLE and ULE (compare w values in Sullivan.R, increase N to get closer)
(HLE <- sum(RHRS_clean == "H") / N)
(ULE <- sum(RHRS_clean == "U") / N)

# But this approach gives lots of goodies for free

# This is an equivalent way to get HLE
mean(colSums(RHRS_clean == "H"))

# but also distributional statistic you want:
# SD of HLE
sd(colSums(RHRS_clean == "H"))
# Quantiles of HLE (note the 2-year age bins :-())
quantile(colSums(RHRS_clean == "H"), c(.25,.5,.75))

# or the unhealthy part
sd(colSums(RHRS_clean == "U"))
quantile(colSums(RHRS_clean == "U"), c(.25,.5,.75))

# Note, distribution measures are measuring
# between-individual variability, NOT the variance
# of the estimate. For that, you'd want to repeat 
# the simulation many times (1000?) and take quantiles
# of the HLE estimates.

(colSums(RHRS_clean == "U")) %>% 
  hist(main = "Unhealthy life distribution")
(colSums(RHRS_clean == "H")) %>% 
  hist(main="Healthy life distribution")

# But really, I mean, really, simulated life courses are sooooo rich.
# you can look at spell distributions, etc, etc. easy peasy, you just
# need to learn to deal with trajectory data a bit.



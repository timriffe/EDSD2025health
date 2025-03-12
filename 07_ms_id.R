# based on script ID_HLE.R from 
# repo timriffe/PHDS_HLE_2021
# modified for our data

library(tidyverse) # install.packages("tidyverse")
# some custom functions

# Read in the data (Daniel Schneider gave these to me, haha)
# TR <- read_csv("data/adj_transitions.csv.gz")
TR <- read_csv("https://github.com/timriffe/EDSD2025health/raw/refs/heads/master/data/adj_transitions.csv.gz")
head(TR)

# define a subset, reshape to wider along transitions
TRsub <-
  TR |> 
  filter(sex == "female",
         time == 2015,
         condition == "adl") |> 
  pivot_wider(names_from = transition, values_from = p)

# how many age groups
n  <- nrow(TRsub)
# age interval width..
interval <- 1

# Get transition probabilities
hhx <- TRsub %>% pull(HH)
hux <- TRsub %>% pull(HU)
uux <- TRsub %>% pull(UU)
uhx <- TRsub %>% pull(UH)
hdx <- TRsub %>% pull(HD)
udx <- TRsub %>% pull(UD)

# just to make sure we have perfect compositions:
hhx + hux + hdx ; uux + uhx + udx
# decide on some starting conditions...
init <- c(H=.99,U=.01)
# init <- c(H=1,U=0)

# Now start the calcs
# Two containers
Hx <- rep(0, n+1)
Ux <- rep(0, n+1)

# we start off with everyone alive:
Hx[1] <- init["H"] * interval
Ux[1] <- init["U"] * interval

# following ages are all determined
# but they are sequentially dependent
for (i in 1:n){
  Hx[i+1] <- Hx[i] * hhx[i] + Ux[i] * uhx[i]
  Ux[i+1] <- Ux[i] * uux[i] + Hx[i] * hux[i]
}
sum(Hx)
sum(Ux)

sum((Hx[-1] + Hx[-length(Hx)]) / 2)
# add an age to close out
ages <- 20:111 
HLT <- data.frame(
  age = ages,
  Hx=Hx,
  Ux=Ux,
  hhx=c(hhx, 0),
  hux=c(hux, 0),
  hdx=c(hdx, 1),
  uux=c(uux, 0),
  uhx=c(uhx, 0),
  udx=c(udx, 1))


HLT
# Hx and Ux are now like denominators.
# The other columns are directed transition probabilities
# So you can calculate actual transitions as you please

# For example:
HLT %>% 
  mutate(DU = Ux * udx,
         DH = Hx * hdx) %>% 
  select(age, DU, DH) %>% 
  mutate(D = DU + DH) %>% 
  pivot_longer(DU:D, 
               names_to = "Health status",
               values_to = "Deaths") %>% 
  ggplot(aes(x = age, 
             y = Deaths, 
             color = `Health status`, 
             group = `Health status`)) +
  geom_line() +
  labs(color = "Health status\nat death",
       title = "Think of why unhealthy deaths are\non average later")


# a function that does this
IDLT <- function(dat, init, interval = 1){
  n  <- nrow(dat)
  Hx <- rep(0, n+1)
  Ux <- rep(0, n+1)
  
  hhx <- dat %>% pull(HH)
  hux <- dat %>% pull(HU)
  uux <- dat %>% pull(UU)
  uhx <- dat %>% pull(UH)
  
  hdx <- dat %>% pull(HD)
  udx <- dat %>% pull(UD)
  # if not given then assume constant.
  if (missing(init)){
    u1   <- matrix(c(hhx[1],hux[1],uhx[1],uux[1]),2)
    v1   <- eigen(u1)$vectors[,1]
    init <- v1 / sum(v1)
  }
  #cat(init)
  Hx[1] <- init[1] * interval
  Ux[1] <- init[2] * interval
  
  for (i in 1:n){
    Hx[i+1] <- Hx[i] * hhx[i] + Ux[i] * uhx[i]
    Ux[i+1] <- Ux[i] * uux[i] + Hx[i] * hux[i]
  }
  ages <- c(dat$age, max(dat$age)+1) |> sort()
  tibble(age = ages,
             Hx=Hx,
             Ux=Ux,
             hhx=c(hhx, 0),
             hux=c(hux, 0),
             hdx=c(hdx, 1),
             uux=c(uux, 0),
             uhx=c(uhx, 0),
             udx=c(udx, 1))
}











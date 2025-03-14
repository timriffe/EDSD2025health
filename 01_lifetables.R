
# we'll calculate lifetables here
library(tidyverse)

mx <- read_csv(
"https://github.com/timriffe/EDSD2025health/raw/refs/heads/master/data/mx.csv.gz")

head(mx)

# 1 get ax
# You could make a big case_when() statement
# to handle all cases.

mx_to_ax <- function(mx, age, sex){
  sex <- sex[1]
  m0 <- mx[1]
  a0 <- case_when(sex == "male" & between(m0,0,.023) ~ 0.14929 - 1.99545 * m0,
                  sex == "male" & between(m0,.02300001,0.08307) ~ 0.02832 + 3.26021 * m0,
                  sex == "male" & m0 > 0.08307001 ~ 0.29915,
                  sex == "female" & between(m0,0,0.01724) ~ 0.14903 - 2.05527 * m0,
                  sex == "female" & between(m0,0.01724001, 0.06891) ~ 0.04667 + 3.88089 * m0,
                  sex == "female" & m0 > 0.06891001 ~ 0.31411)
  m_open <- mx[length(mx)]
  a_open <- 1 / m_open
  ax     <- c(a0,rep(.5, length(mx)-2), a_open)
  return(ax)
}


# 2 use mx and ax to get qx

mx_to_qx <- function(mx, ax){
  qx <- mx / (1 + (1 - ax) * mx)
  qx[length(qx)] <- 1
  return(qx)
}

# 3 use qx to get lx via px

qx_to_lx <- function(qx){
  lx <- cumprod(1-qx)
  lx <- c(1, lx[-length(lx)])
  lx
}
# 4 get Lx, lifetable exposure via dx

dx_lx_to_Lx <- function(dx,lx,ax){
  Lx <- lx - (1 - ax) * dx
  n <- length(lx)
  Lx[n] <- lx[n] * ax[n]
  Lx
}

lifetables <-
  mx |> 
  group_by(sex, time) |> 
  mutate(ax = mx_to_ax(mx, age, sex),
         qx = mx_to_qx(mx, ax),
         lx = qx_to_lx(qx),
         dx = lx * qx,
         Lx = dx_lx_to_Lx(dx,lx,ax),
         Tx = rev(Lx) |> cumsum() |> rev(),
         ex = Tx / lx)

write_csv(lifetables, "data/lifetables.csv.gz")

lifetables |> 
  filter(age == 0) |> 
  select(sex, time, ex) |> 
  mutate(asdr = 1 / ex)


lifetables |> 
  filter(time == 2015, sex == "female") |> 
  ggplot(aes(x = age, y = mx)) +
  scale_y_log10() +
  theme_minimal() +
  geom_line()













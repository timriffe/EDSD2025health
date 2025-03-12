
# 1 get ms lifetable envelope
HLT |> 
  select(age, Hx, Ux) |> 
  mutate(lx = Hx + Ux,
         Lx = (lx + lead(lx, default = 0))/2) |> 
  summarize(e20 = sum(Lx))
  
source("01_lifetables.R")

HLT |> 
  select(age, Hx, Ux) |> 
  mutate(prev = Ux / (Ux + Hx)) |> 
  ggplot(aes(x = age, y = prev)) +
  geom_line() 

lifetables |> 
  filter(age == 20,
         sex == "female",
         time == 2015) |> 
  select(ex)



library(tidyverse)
# 1 read in data/prev.czv.gz using read_csv()
prev <- read_csv("https://github.com/timriffe/EDSD2025health/raw/refs/heads/master/data/prev.csv.gz")

head(prev)
# 2 reshape so that states H and U are side-by-side, with values n
# use pivot_wider() for this
# 3 calculate prev as U / (U + H) using mutate()
prev <-
  prev |> 
  pivot_wider(names_from = state, values_from = n) |> 
  mutate(prev = U / (H + U))

# 4 make some plots of prevalence, and some observations
# would you maybe want to smooth this?
# any odd features?
prev |> 
  filter(condition == "gali", time == 2015) |> 
  ggplot(aes(x = age, y = prev, color = sex)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

# 5 make one or more plots of sex ratios in prevalence
# what can be said about these?

prev |> 
  select(-H,-U) |> 
  pivot_wider(names_from = sex, values_from = prev) |> 
  mutate(ratio = female / male) |> 
  filter(condition == "gali", time == 2015) |> 
  ggplot(aes(x = age, y = ratio)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()




#Self related:
#  PH002_ HEALTH IN GENERAL QUESTION  V 1 | Would you say your health is ... | 1. Very good | 2. Good | 3. Fair | 4. Bad | 5. Very bad
#Chronic:
#  PH004_ LONG-TERM ILLNESS Some people suffer from chronic or long-term health problems. By  long-term we mean it has troubled you over a period of time or is  likely to affect you over a period of time. Do you have any long-term  health problems, illness, disability or infirmity? IWER: INCLUDING MENTAL HEALTH PROBLEMS 1. Yes 5. No 
#GALI:
#  PH005_ LIMITED ACTIVITIES For the past six months at least, to what extent have you been  limited because of a health problem in activities people usually do? IWER: READ OUT 1. Severely limited 2. Limited, but not severely 3. Not limited
#for ADL and IADL:
#  PH049_ MORE HEALTH AND ACTIVITIES Please look at card 10.Here are a few more everyday activities.  Please tell me if you have any difficulty with these because of a  physical, mental, emotional or memory problem. Again exclude any  difficulties you expect to last less than three months.(Because of a  health or memory problem, do you have difficulty doing any of the  activities on card 10?) IWER: PROBE: ANY OTHERS? CODE ALL THAT APPLY 1. Dressing, including putting on shoes and socks 2. Walking across a room 3. Bathing or showering 4. Eating, such as cutting up your food 5. Getting in or out of bed 6. Using the toilet, including getting up or down 7. Using a map to figure out how to get around in a strange place 8. Preparing a hot meal 9. Shopping for groceries 16 10. Making telephone calls 11. Taking medications 12. Doing work around the house or garden 13. Managing money, such as paying bills and keeping track of  expenses 96. None of these for IADL:

# explanations:
# for gali 0 is healthy otherwise U
# for self 1 2 3 is H, others U
# for chronic 0 is H otherwise U
# for ADL and IADL anything more than 0 is U


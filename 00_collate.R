library(tidyverse)

# save out raw transition counts
counts <- local(get(load("data/to_collate/prev_emp_n.RData"))) 
write_csv(counts, "data/counts.csv.gz")

# save out simplified file that cna be used to calculate prevalence
prev <- counts |> 
  group_by(condition,sex,time,age,from) |> 
  summarize(n=sum(n), .groups = "drop") |> 
  rename(state = from)

write_csv(prev,"data/prev.csv.gz")

# all file names
files <- dir("data/to_collate")

# collate unadjusted (but modelled and extrapolated) transitions
pre_adjust_files <- files[grepl(files,pattern = ".RData") & grepl(files,pattern = "lt")]
dat <- structure(list(condition = character(0), sex = character(0), 
                       time = numeric(0), age = integer(0), HH = numeric(0), HU = numeric(0), 
                       HD = numeric(0), UH = numeric(0), UU = numeric(0), UD = numeric(0)), row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"))
for (f in pre_adjust_files){
  cond_i <- gsub(x = f, pattern = ".RData", replacement ="") %>% 
  gsub(pattern = "lt_", replacement = "", x = .)
 file_i <- file.path("data","to_collate",f)
 dati <-
  local(get(load(file_i))) |> 
  select(sex,time,age,HH,HU,HD,UH,UU,UD) |> 
  mutate(condition = cond_i, .before = 1)
  dat <- bind_rows(dat,dati)
}
dat <-
  dat |> 
  pivot_longer(HH:UD, names_to = "transition", values_to = "p") |>
  mutate(variant = "modelled_unadjusted",.before=1)
write_csv(dat,"data/unadj_transitions.csv.gz")

# collate and save adjusted transitions
dat <- structure(list(condition = character(0), sex = character(0), 
                       time = numeric(0), age = numeric(0), HH = numeric(0), HU = numeric(0), 
                       HD = numeric(0), UH = numeric(0), UU = numeric(0), UD = numeric(0)), row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"))
adjusted_files <- files[grepl(files,pattern = ".csv.gz") & grepl(files,pattern = "lt")]
for (f in adjusted_files){
  cond_i <- gsub(x = f, pattern = "_adj.csv.gz", replacement ="") %>% 
    gsub(pattern = "lt_", replacement = "", x = .)
  
  file_i <- file.path("data","to_collate",f)
  dati <-
    read_csv(file_i, show_col_types = FALSE) |> 
    select(sex,time,age,HH,HU,HD,UH,UU,UD) |> 
    mutate(condition = cond_i, .before = 1)
  dat <- bind_rows(dat,dati)
}
dat <-
  dat |> 
  pivot_longer(HH:UD, names_to = "transition", values_to = "p") |>
  mutate(variant = "lifetable_adjusted",.before=1)
write_csv(dat,"data/adj_transitions.csv.gz")


 
library(tidyverse)
library(janitor)

#Read in dirty decathlon data
decathlon_original <- read_rds("raw_data/decathlon.rds")
#clean variable names
decathlon_clean_names <- clean_names(decathlon_original)

decathalon_col_names <- rownames_to_column(decathlon_clean_names, var = "name")

decacthalon_arr <- decathalon_col_names %>%
  arrange(name) %>%
  mutate(name = tolower(name))

id <- decacthalon_arr %>%
  mutate(id = group_indices(.,name))

id_moved <- id %>%
  select(id, everything())
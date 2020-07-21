 
library(tidyverse)
library(janitor)

#Read in dirty decathlon data
decathlon_original <- read_rds("raw_data/decathlon.rds")
#clean variable names
decathlon_clean <- decathlon_original %>%
  # put row names into a variable
  rownames_to_column( var = "name") %>%
  arrange(name) %>%
  # make all names lower case
  mutate(name = tolower(name)) %>%
  # create id number for every unique name
  mutate(id = group_indices(.,name)) %>%
  # id to the front
  select(id, everything()) %>%
  clean_names()


# trimmed data set for analysis
decathlon_trimmed <- decathlon_clean %>%
  select(id, name, x100m, long_jump, shot_put, x400m, rank, points, competition)


# save clean trimmed data as .csv
write_csv(decathlon_trimmed, "clean_data/decathlon_trimmed.csv")
library(tidyverse)
library(janitor)
library(readxl)

candy_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")


# Put all datasets in long format
long_2015 <- candy_2015 %>%
  pivot_longer(cols = starts_with("["),
               names_to = "candy",
               values_to = "rating")

long_2016 <- candy_2016 %>%
  pivot_longer(cols = starts_with("["),
               names_to = "candy",
               values_to = "rating")

long_2017 <- candy_2017 %>%
  pivot_longer(cols = starts_with("Q6"),
               names_to = "candy",
               values_to = "rating")

# Get rid of irrelevant variables and clean variable names.
clean_2015 <- long_2015 %>%
  select(Timestamp,
         `How old are you?`,
         `Are you going actually going trick or treating yourself?`,
         `Please list any items not included above that give you JOY.`,
         `Please list any items not included above that give you DESPAIR.`,
         candy, 
         rating)

names(clean_2015) <- c("timestamp",
                       "age",
                       "going_trick_or_treating", 
                       "other_joy", 
                       "other_dispair",
                       "candy", 
                       "rating")

clean_2016 <- long_2016 %>%
  select(Timestamp,
         `How old are you?`,
         `Are you going actually going trick or treating yourself?`,
         `Which country do you live in?`,
         `Please list any items not included above that give you JOY.`,
         `Please list any items not included above that give you DESPAIR.`,
         candy,
         rating)

names(clean_2016) <- c("timestamp",
                       "age",
                       "going_trick_or_treating",
                       "country",
                       "other_joy", 
                       "other_dispair",
                       "candy", 
                       "rating")

clean_2017 <- long_2017 %>%
  select(`Internal ID`,
         `Q3: AGE`,
         `Q1: GOING OUT?`,
         `Q4: COUNTRY`,
         `Q7: JOY OTHER`,
         `Q8: DESPAIR OTHER`,
         candy,rating)

names(clean_2017) <- c("id",
                       "age",
                       "going_trick_or_treating",
                       "country",
                       "other_joy", 
                       "other_dispair",
                       "candy", 
                       "rating")

# Join all datasets
boing_boing_candy <- bind_rows(clean_2015, clean_2016, clean_2017)

#pivot_wider to clean candy names
boing_candy_wide <- boing_boing_candy %>%
  pivot_wider(names_from = candy,
              values_from = rating)%>%
  clean_names()

boing_candy_long <- boing_candy_wide %>%
  pivot_longer(cols = -c(timestamp,
                         age,
                         going_trick_or_treating,
                         other_joy, other_dispair,
                         country,
                         id),
               names_to = "candy",
               values_to = "rating",
               names_prefix = "q6_")%>%

  # Filter out any non candy items.
  filter(!candy %in% c("abstained_from_m_ming", 
                       "bonkers_the_board_game",
                       "broken_glow_stick",
                       "cash_or_other_forms_of_legal_tender",
                       "chardonnay",
                       "creepy_religious_comics_chick_tracts",
                       "dental_paraphenalia",
                       "generic_brand_acetaminophen",
                       "glow_sticks",
                       "healthy_fruit",
                       "hugs_actual_physical_hugs",
                       "kale_smoothie",
                       "lapel_pins",
                       "minibags_of_chips",
                       "peanut_butter_jars",
                       "pencils",
                       "person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes",
                       "peterson_brand_sidewalk_chalk",
                       "real_housewives_of_orange_county_season_9_blue_ray",
                       "sandwich_sized_bags_filled_with_boo_berry_crunch",
                       "spotted_dick",
                       "trail_mix",
                       "vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein",
                       "vicodin",
                       "whatchamacallit_bars",
                       "white_bread",
                       "whole_wheat_anything",
                       "candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants",
                       "joy_joy_mit_iodine_2",
                       "joy_joy_mit_iodine")) %>%
  
# tidying candy names and combining duplicates.
  mutate(candy = 
           recode(candy, 
                  "anonymous_brown_globs_that_come_in_black_and_orange_wrappers" = "mary_janes",
                  "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes" = "mary_janes",
                  "bonkers_the_candy" = "bonkers",
                  "boxo_raisins" = "box_o_raisins",
                  "chick_o_sticks_we_don_t_know_what_that_is" = "chick_o_sticks",
                  "dark_chocolate_hershey" = "hersheys_dark_chocolate",
                  "jolly_rancher_bad_flavor" = "jolly_ranchers",
                  "jolly_ranchers_good_flavor" = "jolly_ranchers",
                  "licorice_not_black" = "licorice",
                  "licorice_yes_black" = "licorice",
                  "nown_laters" = "now_and_laters",
                  "sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year" = "sea_salt_flavored_chocolate",
                  "smarties_american" = "smarties",
                  "smarties_commonwealth" = "smarties",
                  "sourpatch_kids_i_e_abominations_of_nature" = "sourpatch_kids",
                  "sweetums_a_friend_to_diabetes" = "sweetums",
                  "those_odd_marshmallow_circus_peanut_things" = "circus_peanut",
                  "tolberone_something_or_other" = "tolberone",
                  "x100_grand_bar" = "100_grand_bar",
                  "york_peppermint_patties_ignore" = "york_peppermint_patties")
         ) 

# Looked into integrating the other joy/despair columns into the candy but there were far too many entries to clean.
# mutate(
#   other_joy = str_split(other_joy, ",")
# ) %>%
# unnest(other_joy) %>%
# mutate(other_joy = tolower(other_joy)) %>%
# select(-other_dispair, -other_joy)

# clean age column
age_cleaned <- boing_candy_long %>%
# extract all numeric data from age
  mutate(clean_age = 
           str_extract(age, "[0-9]*.")
         ) %>%
  mutate(clean_age = as.integer(clean_age)
         ) %>%
# Remove any age over the 122
  mutate(clean_age = 
           if_else(clean_age <= 122, clean_age, NA_integer_)
         ) %>%
# I made the assumption that the numbers in the country column were ages incorrectly filled out.
# I extracted all the numeric data from the country column.
  mutate(country_age = 
           str_extract(country, "[0-9]*.")
         ) %>%
  mutate(country_age = as.integer(country_age)
         ) %>%
# Combined the two extracted age variables and replaced the old age column.
  mutate(age = 
           coalesce(clean_age, country_age)
         ) %>%
  select(-clean_age, -country_age, -other_dispair, -other_joy)

# create patterns for srt_detect
usa_pattern <- "[u]* [s]* [a]|usa|ussa|us|
u s|united s|united  s|unhinged|unied|unite |
unites states|units|merica|yoo|amerca|alaska|california|
cascadia|fear|murica|murrika|new jersey|new york|
|north carolina|pittsburgh|republic|ud|trumpistan|u s|unites states|cascadia"

uk_pattern <- "endland|england|scotland|uk|united k"

canada_pattern <- "can|canada`|canae"

na_pattern <- "a |atlantis|denial|earth|gods country|
i dont know anymore|insanity lately|narnia|neverland|
one of best ones|see above|somewhere|subscribe to dmuz on youtube|
there isnt one for old men|this one|i dont know anymore|one of best ones|there isnt one for old men"

# Clean country column.  
countries_cleaned <- age_cleaned %>%
# remove punctuation.
  mutate(country =
           str_remove_all(country, "[[:punct:]]")
         ) %>%
# remove the ages from the column
  mutate(country =
           str_remove_all(country, "[0-9]")
         ) %>%
  mutate(country =
           str_to_lower(country)
         ) %>%
  mutate(country =
           str_remove_all(country, "the ")
         ) %>%
#combine any duplicate countries together using case_when and str_detect
  mutate(country =
           case_when(str_detect(country, usa_pattern) ~ "united states",
                     str_detect(country, uk_pattern) ~ "united kingdom",
                     str_detect(country, canada_pattern) ~ "canada",
                     str_detect(country, na_pattern) ~ as.character(NA),
                     country == "a" ~ as.character(NA),
                     country == "españa" ~ "spain",
                     country == "eua" ~ "europe",
                     country == "uae" ~ "united arab emirates",
                     TRUE ~ country)
         ) %>%
  mutate(country =
           str_to_title(country)
         )
  
boing_candy_clean <- clean_countries %>%
  filter(!is.na(rating))
  
write.csv(boing_candy_clean, "clean_data/boing_candy_clean.csv")
  





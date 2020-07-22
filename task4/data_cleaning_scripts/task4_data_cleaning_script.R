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
                       "whole_wheat_anything"))


ordered_candy <- boing_candy_long %>%
  select(candy) %>%
  arrange(candy) %>%
  distinct(candy)


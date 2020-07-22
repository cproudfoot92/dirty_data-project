
library(tidyverse)
library(janitor)

cake_ingredients <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_ingredients_code <- read_csv("raw_data/cake_ingredient_code.csv")

# I needed to pivot longer cake_ingredients.
cake_ingredients_long <- cake_ingredients %>%
  pivot_longer(-Cake, 
               names_to = "code",
               values_to = "amount")
# join to many cake_ingredients_long/code
cake_ingredients_joined <- left_join(cake_ingredients_long,
                                     cake_ingredients_code,
                                     "code")

#filter out any NAs
cake_ingredients_final <- cake_ingredients_joined %>%
  select(Cake, code, ingredient, measure, amount) %>%
  filter(!is.na(amount)) %>%
  mutate(Cake = tolower(Cake)) %>%
  mutate(ingredient = tolower(ingredient)) %>%
  clean_names()

write_csv(cake_ingredients_final, "clean_data/cake_ingredients_clean.csv")





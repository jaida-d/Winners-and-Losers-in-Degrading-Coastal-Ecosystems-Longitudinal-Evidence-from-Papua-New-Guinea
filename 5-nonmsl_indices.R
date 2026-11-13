# load("5-nonmsl_indices.RData")

#--------------------
# OPEN PACKAGES
library(dplyr)
library(tidyr)
library(lessR)

#--------------------
# LOAD DATA
data <- read.csv("Data/msl_data/data_clean.csv")
meta <- read.csv("Data/meta.csv")

#--------------------
# FOOD INSECURITY INDEX

## Check fi data
colnames(data)
data_fi2018 <- data %>%
  filter(year %in% c(2018, 2026), community == "AH") %>% 
  select(year, SharingUnitID_NEW, FoodSecurity_RESPONSES, food_security.no_eat_day:food_security.smaller_meal)
head(data_fi2018)

## NAs / blanks
data <- data %>%
  mutate(FoodSecurity_RESPONSES = ifelse(FoodSecurity_RESPONSES == "YES" &
                                           if_any(food_security.no_eat_day:food_security.smaller_meal, ~ .x == ""),
                                         "INCOMPLETE", FoodSecurity_RESPONSES))

## Re-check data
data_fi2018 <- data %>%
  filter(year %in% c(2018, 2026), community == "AH") %>% 
  select(year, SharingUnitID_NEW, FoodSecurity_RESPONSES, food_security.no_eat_day:food_security.smaller_meal)
data_fi2018

## Count observations with no/incomplete food security data in each year 
nrow(data %>% filter(year == 2018, community == "AH", FoodSecurity_RESPONSES %in% c("NO", "INCOMPLETE")))
nrow(data %>% filter(year == 2026, community == "AH", FoodSecurity_RESPONSES %in% c("NO", "INCOMPLETE")))

## Count observations with complete food security data in each year 
nrow(data %>% filter(year == 2018, community == "AH", FoodSecurity_RESPONSES == "YES"))
nrow(data %>% filter(year == 2026, community == "AH", FoodSecurity_RESPONSES == "YES"))

## food insecurity index = 2018-2026 AH only.
data <- data %>%
  mutate(food_security.no_eat_day = ifelse(food_security.no_eat_day == "never", 1,
                                           ifelse(food_security.no_eat_day == "rarely", 2,
                                                  ifelse(food_security.no_eat_day == "sometimes", 3,
                                                         ifelse(food_security.no_eat_day == "often", 4, NA)))),
         food_security.sleep_hungry = ifelse(food_security.sleep_hungry == "never", 1,
                                             ifelse(food_security.sleep_hungry == "rarely", 2,
                                                    ifelse(food_security.sleep_hungry == "sometimes", 3,
                                                           ifelse(food_security.sleep_hungry == "often", 4, NA)))),
         food_security.no_food_HH = ifelse(food_security.no_food_HH == "never", 1,
                                           ifelse(food_security.no_food_HH == "rarely", 2,
                                                  ifelse(food_security.no_food_HH == "sometimes", 3,
                                                         ifelse(food_security.no_food_HH == "often", 4, NA)))),
         food_security.fewer_meals = ifelse(food_security.fewer_meals == "never", 1,
                                            ifelse(food_security.fewer_meals == "rarely", 2,
                                                   ifelse(food_security.fewer_meals == "sometimes", 3,
                                                          ifelse(food_security.fewer_meals == "often", 4, NA)))),
         food_security.smaller_meal = ifelse(food_security.smaller_meal == "never", 1,
                                             ifelse(food_security.smaller_meal == "rarely", 2,
                                                    ifelse(food_security.smaller_meal == "sometimes", 3,
                                                           ifelse(food_security.smaller_meal == "often", 4, NA)))),
         fi_index = ifelse(FoodSecurity_RESPONSES == "YES",
                           food_security.no_eat_day + food_security.sleep_hungry + food_security.no_food_HH + food_security.fewer_meals + food_security.smaller_meal,
                           NA),
         FoodSecurity_RESPONSES = ifelse(is.na(fi_index), "INCOMPLETE", FoodSecurity_RESPONSES))


## Check final fi data
data_fi2018 <- data %>%
  filter(year %in% c(2018, 2026), community == "AH") %>% 
  select(year, SharingUnitID_NEW, FoodSecurity_RESPONSES, food_security.no_eat_day:food_security.smaller_meal, fi_index)
data_fi2018

table(data_fi2018$year, data_fi2018$FoodSecurity_RESPONSES, useNA = "ifany")

#--------------------
# SOCIAL CAPITAL: DECISION-MAKING INDEX

data <- data %>%
  mutate(social_capital.decision_involvement.index = ifelse(social_capital.decision_involvement == "actively_involved", 1,
                                                            ifelse(social_capital.decision_involvement == "passively_involved", 0.5,
                                                                   ifelse(social_capital.decision_involvement == "not_involved", 0, NA))),
         social_capital.marine_resources.index = ifelse(social_capital.marine_resources == "actively_involved", 1,
                                                        ifelse(social_capital.marine_resources == "passively_involved", 0.5,
                                                               ifelse(social_capital.marine_resources == "not_involved", 0, NA))),
         social_capital.decision.index = ifelse(SocialCapital_RESPONSES == "YES",
                                                social_capital.decision_involvement.index + social_capital.marine_resources.index,
                                                NA))

#--------------------
# SOCIAL CAPITAL: TRUST INDEX

data <- data %>%
  mutate(social_capital.trust_group.trust_pvillage = ifelse(social_capital.trust_group.trust_pvillage == "trust_all", 4,
                                                            ifelse(social_capital.trust_group.trust_pvillage == "trust_more", 3,
                                                                   ifelse(social_capital.trust_group.trust_pvillage == "trust_half", 2,
                                                                          ifelse(social_capital.trust_group.trust_pvillage == "trust_less", 1,
                                                                                 ifelse(social_capital.trust_group.trust_pvillage == "trust_none", 0,
                                                                                        NA))))),
         social_capital.trust_group.trust_comleaders = ifelse(social_capital.trust_group.trust_comleaders == "trust_all", 4,
                                                              ifelse(social_capital.trust_group.trust_comleaders == "trust_more", 3,
                                                                     ifelse(social_capital.trust_group.trust_comleaders == "trust_half", 2,
                                                                            ifelse(social_capital.trust_group.trust_comleaders == "trust_less", 1,
                                                                                   ifelse(social_capital.trust_group.trust_comleaders == "trust_none", 0,
                                                                                          NA))))),
         social_capital.trust_group.trust_lgov = ifelse(social_capital.trust_group.trust_lgov == "trust_all", 4,
                                                        ifelse(social_capital.trust_group.trust_lgov == "trust_more", 3,
                                                               ifelse(social_capital.trust_group.trust_lgov == "trust_half", 2,
                                                                      ifelse(social_capital.trust_group.trust_lgov == "trust_less", 1,
                                                                             ifelse(social_capital.trust_group.trust_lgov == "trust_none", 0,
                                                                                    NA))))),
         social_capital.trust_group.trust_police = ifelse(social_capital.trust_group.trust_police == "trust_all", 4,
                                                          ifelse(social_capital.trust_group.trust_police == "trust_more", 3,
                                                                 ifelse(social_capital.trust_group.trust_police == "trust_half", 2,
                                                                        ifelse(social_capital.trust_group.trust_police == "trust_less", 1,
                                                                               ifelse(social_capital.trust_group.trust_police == "trust_none", 0,
                                                                                      NA))))),
         social_capital.trust.index = ifelse(SocialCapital_RESPONSES == "YES",
                                             social_capital.trust_group.trust_pvillage + social_capital.trust_group.trust_comleaders + social_capital.trust_group.trust_lgov + social_capital.trust_group.trust_police,
                                             NA))

#--------------------
# SAVE FINAL DATA
write.csv(data, "Data/msl_data/data_final.csv")

#--------------------
# SAVE OUTPUTS
save.image("5-nonmsl_indices.RData")
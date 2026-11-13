# load("4-datacleaning.RData")

#--------------------
# OPEN PACKAGES
library(dplyr)
library(tidyr)
library(lessR)

#--------------------
# LOAD DATA

# Read in data from PCAs
data_AH <- read.csv("Data/msl_data/data_msl_AH_2016to2026.csv")
data_KA <- read.csv("Data/msl_data/data_msl_KA_2016to2026.csv")

# Combine data
data <- rbind(data_AH, data_KA)

# Read in meta
meta <- read.csv("Data/meta.csv")

#--------------------
# LIVELIHOODS DATA

# Replace NAs in livelihood rankings with 0
data <- data %>%
  mutate(across(livelihoods.rank.activity_fishing:livelihoods.rank.activity_atoll_farming, ~replace_na(.x, 0)))

# Check for incomplete livelihood data
data_no_rankedlivelihoods <- data %>% 
  filter(if_all(livelihoods.rank.activity_fishing:livelihoods.rank.activity_atoll_farming, ~ .x == 0)) %>%
  select(SharingUnitID_NEW, year, livelihoods.rank.activity_fishing:livelihoods.rank.activity_atoll_farming)
data_no_rankedlivelihoods #none

# Make 2016, 2018 and 2026 livelihood cols match
## In 2016, livelihoods.rank.activity_atoll_farming was included in livelihoods.rank.activity_farming; these were separate categories in 2018 and 2026.
## In 2016, livelihoods.rank.activity_remittance was included in livelihoods.rank.activity_other; these were separate categories in 2018 and 2026.
## Combine these categories in 2018 and 2026 to make sure they match the 2016 categories.
data <- data %>% 
  mutate(livelihoods.rank.activity_farming.NEW = ifelse(year %in% c(2018, 2026),
                                                        ifelse(livelihoods.rank.activity_farming == 0,
                                                               livelihoods.rank.activity_atoll_farming, #if farming not ranked, use the atoll_farming rank
                                                               ifelse(livelihoods.rank.activity_atoll_farming == 0,
                                                                      livelihoods.rank.activity_farming, #if atoll_farming not ranked, use the farming rank
                                                                      pmin(livelihoods.rank.activity_farming, livelihoods.rank.activity_atoll_farming))), #if both are ranked, use the smaller rank
                                                        livelihoods.rank.activity_farming), #for 2016, keep the original rank
         livelihoods.rank.activity_other.NEW = ifelse(year %in% c(2018, 2026),
                                                      ifelse(livelihoods.rank.activity_other == 0,
                                                             livelihoods.rank.activity_remittance, #if other not ranked, use the remittance rank
                                                             ifelse(livelihoods.rank.activity_remittance == 0,
                                                                    livelihoods.rank.activity_other, #if remittance not ranked, use the other rank
                                                                    pmin(livelihoods.rank.activity_remittance, livelihoods.rank.activity_other))), #if both are ranked, use the smaller rank
                                                      livelihoods.rank.activity_other)) #for 2016, keep the original rank

# Subset final livelihood col names
livelihoods_rankvars_NEW <- data %>%
  select(c(livelihoods.rank.activity_fishing:livelihoods.rank.activity_atoll_farming,
           livelihoods.rank.activity_farming.NEW:livelihoods.rank.activity_other.NEW)) %>%
  select(-c(livelihoods.rank.activity_farming, livelihoods.rank.activity_atoll_farming,
            livelihoods.rank.activity_other, livelihoods.rank.activity_remittance)) %>%
  colnames()
livelihoods_rankvars_NEW

# View rows with no recorded primary livelihood
data_no_primary <- data %>% 
  filter(!if_any(all_of(livelihoods_rankvars_NEW), ~ .x == 1)) %>%
  select(SharingUnitID_NEW, year, all_of(livelihoods_rankvars_NEW))
data_no_primary
## KA0033 in 2016, KA0041 in 2026, and KA0073 in 2026 = first two livelihoods equally ranked as 2, 2. Make these 1, 1.
## KA0012 in 2016 = invalid response. Include a col to identify in the dataset.
data <- data %>%
  mutate(across(all_of(livelihoods_rankvars_NEW), 
                ~ ifelse(((SharingUnitID_NEW == "KA0033" & year == 2016) |
                            (SharingUnitID_NEW %in% c("KA0041", "KA0073") & year == 2026)) &
                           .x == 2,
                         1, .x))) %>%
  mutate(Livelihoods_RESPONSES = ifelse(SharingUnitID_NEW == "KA0012" & year == 2016, 
                                        "NO", "YES"))

# Livelihood diversity = Count number of different livelihoods per househould
data <- data %>%
  mutate(livelihoods.diversity = rowSums(across(all_of(livelihoods_rankvars_NEW),
                                                ~ .x != 0)))

# Add column for if primary livelihood is marine/agriculture/other
## Primary livelihood is marine: livelihoods.rank.activity_fishing, livelihoods.rank.activity_gleaning, livelihoods.rank.activity_marketing_mp, livelihoods.rank.activity_tourism
## Primary livelihood is agriculture: livelihoods.rank.activity_farming, livelihoods.rank.activity_cashcrops, livelihoods.rank.activity_atoll_farming
## Primary livelihood is other: livelihoods.rank.activity_salaried_employment, livelihoods.rank.activity_informal_ea, livelihoods.rank.activity_remittance, livelihoods.rank.activity_other 
data <- data %>% 
  mutate(across(all_of(livelihoods_rankvars_NEW),
                ~ ifelse(.x == "u" | is.na(.x), 0, .x))) %>%
  mutate(livelihoods.primary = ifelse(livelihoods.rank.activity_fishing == 1 | 
                                        livelihoods.rank.activity_gleaning == 1 | 
                                        livelihoods.rank.activity_marketing_mp == 1 | 
                                        livelihoods.rank.activity_tourism == 1, 
                                      "marine",
                                      ifelse(livelihoods.rank.activity_farming.NEW == 1 |
                                               livelihoods.rank.activity_cashcrops == 1,
                                             "agriculture",
                                             "other"))) 

# Add column for specific primary livelihood
data <- data %>%
  mutate(livelihoods.primary.specific = ifelse(livelihoods.rank.activity_fishing == 1, "fishing",
                                               ifelse(livelihoods.rank.activity_gleaning == 1, "gleaning",
                                                      ifelse(livelihoods.rank.activity_marketing_mp == 1, "marketing_mp",
                                                             ifelse(livelihoods.rank.activity_tourism == 1, "tourism",
                                                                    ifelse(livelihoods.rank.activity_farming.NEW == 1, "farming",
                                                                           ifelse(livelihoods.rank.activity_cashcrops == 1, "cashcrops",
                                                                                  ifelse(livelihoods.rank.activity_salaried_employment == 1, "salaried_employment",
                                                                                         ifelse(livelihoods.rank.activity_informal_ea == 1, "informal_ea",
                                                                                                ifelse(livelihoods.rank.activity_other.NEW == 1, "other", "ERROR"))))))))))

# Add column for secondary livelihoods type
data <- data %>%
  mutate(livelihoods.secondary = ifelse(livelihoods.primary == "marine" & 
                                          (livelihoods.rank.activity_farming.NEW != 0 |
                                             livelihoods.rank.activity_cashcrops != 0) &
                                          (livelihoods.rank.activity_salaried_employment != 0 |
                                             livelihoods.rank.activity_informal_ea != 0 |
                                             livelihoods.rank.activity_other.NEW != 0),
                                        "agriculture, other",
                                        ifelse(livelihoods.primary == "agriculture" & 
                                                 (livelihoods.rank.activity_fishing != 0 |
                                                    livelihoods.rank.activity_gleaning != 0 |
                                                    livelihoods.rank.activity_marketing_mp != 0 |
                                                    livelihoods.rank.activity_tourism != 0) &
                                                 (livelihoods.rank.activity_salaried_employment != 0 |
                                                    livelihoods.rank.activity_informal_ea != 0 |
                                                    livelihoods.rank.activity_other.NEW != 0),
                                               "marine, other",
                                               ifelse(livelihoods.primary == "other" & 
                                                        (livelihoods.rank.activity_fishing != 0 |
                                                           livelihoods.rank.activity_gleaning != 0 |
                                                           livelihoods.rank.activity_marketing_mp != 0 |
                                                           livelihoods.rank.activity_tourism != 0) &
                                                        (livelihoods.rank.activity_farming.NEW != 0 |
                                                           livelihoods.rank.activity_cashcrops != 0),
                                                      "marine, agriculture",
                                                      ifelse((livelihoods.primary == "marine" | livelihoods.primary == "agriculture") &
                                                               (livelihoods.rank.activity_salaried_employment != 0 |
                                                                  livelihoods.rank.activity_informal_ea != 0 |
                                                                  livelihoods.rank.activity_other.NEW != 0),
                                                             "other",
                                                             ifelse((livelihoods.primary == "marine" | livelihoods.primary == "other") &
                                                                      (livelihoods.rank.activity_farming.NEW != 0 |
                                                                         livelihoods.rank.activity_cashcrops != 0),
                                                                    "agriculture",
                                                                    ifelse((livelihoods.primary == "agriculture" | livelihoods.primary == "other") &
                                                                             (livelihoods.rank.activity_fishing != 0 |
                                                                                livelihoods.rank.activity_gleaning != 0 |
                                                                                livelihoods.rank.activity_marketing_mp != 0 |
                                                                                livelihoods.rank.activity_tourism != 0),
                                                                           "marine",
                                                                           "none")))))))

# Add column for livelihood mix
data <- data %>%
  mutate(livelihoods.mix = ifelse((livelihoods.primary == "marine" & livelihoods.secondary == "agriculture, other") |
                                    (livelihoods.primary == "agriculture" & livelihoods.secondary == "marine, other") |
                                    (livelihoods.primary == "other" & livelihoods.secondary == "marine, agriculture"),
                                  "marine, agriculture, other",
                                  ifelse((livelihoods.primary == "marine" & livelihoods.secondary == "agriculture") |
                                           (livelihoods.primary == "agriculture" & livelihoods.secondary == "marine"),
                                         "marine, agriculture",
                                         ifelse((livelihoods.primary == "marine" & livelihoods.secondary == "other") |
                                                  (livelihoods.primary == "other" & livelihoods.secondary == "marine"),
                                                "marine, other",
                                                ifelse((livelihoods.primary == "agriculture" & livelihoods.secondary == "other") |
                                                         (livelihoods.primary == "other" & livelihoods.secondary == "agriculture"),
                                                       "agriculture, other",
                                                       livelihoods.primary)))))

# Livelihood types count
data <- data %>%
  mutate(livelihoods.types = ifelse(livelihoods.mix == "marine" | livelihoods.mix == "agriculture" | livelihoods.mix == "other", 1,
                                    ifelse(livelihoods.mix == "marine, agriculture" | livelihoods.mix == "marine, other" | livelihoods.mix == "agriculture, other", 2,
                                           ifelse(livelihoods.mix == "marine, agriculture, other", 3, 0))))

# Add cols for if primary livelihood is marine (1, 0) and agriculture (1, 0)
data <- data %>%
  mutate(livelihoods.primary.marine = ifelse(livelihoods.rank.activity_fishing == 1 | 
                                               livelihoods.rank.activity_gleaning == 1 | 
                                               livelihoods.rank.activity_marketing_mp == 1 | 
                                               livelihoods.rank.activity_tourism == 1, 
                                             1, 0),
         livelihoods.primary.agri = ifelse(livelihoods.rank.activity_farming.NEW == 1 |
                                             livelihoods.rank.activity_cashcrops == 1,
                                           1, 0))

#--------------------
# AGE GROUPS (FOR DESCRIPTIVE STATS ONLY)

## Identify age groups of combined AH and KA data in 2016
age_groups <- data %>%
  filter(year == 2016) %>%
  select(age.recorded) %>%
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)
age_groups

## Check that groups are similar in 2016 for Ah and KA
age_groups_checkAH <- data %>%
  filter(year == 2016 & community == "AH") %>%
  select(age.recorded) %>%
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)
age_groups_checkAH

age_groups_checkKA <- data %>%
  filter(year == 2016 & community == "KA") %>%
  select(age.recorded) %>%
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)
age_groups_checkKA

## Check that quantiles are similar in AH and KA across all years
age_groups_allyrs <- data %>%
  select(age.recorded) %>%
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)
age_groups

age_groups_checkAHallyrs <- data %>%
  filter(community == "AH") %>%
  select(age.recorded) %>%
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)
age_groups_checkAHallyrs

age_groups_checkKAallyrs <- data %>%
  filter(community == "KA") %>%
  select(age.recorded) %>%
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)
age_groups_checkKAallyrs

## Create age groups variable
age_groups
data <- data %>%
  mutate(age.group = ifelse(age.recorded <= 36, "<=36",
                            ifelse(age.recorded <= 48, "37-48", 
                                   ">=49")))

## Check structure of age groups
age_str <- data %>%
  count(community, year, age.group)
age_str

#--------------------
# EDUCATION, MIGRANT STATUS, MARITAL STATUS, CC IMPACT

data <- data %>%
  mutate(education = ifelse(is.na(education), 0, education), #If education is NA in 2018 or 2026, it means 0. Fix.
         migrantStatus = ifelse(migrantStatus == 0 | migrantStatus == "", NA, migrantStatus), #If migrantStatus is 0, it means NA. Fix
         maritalStatus = ifelse(maritalStatus == "divorced_widowed", "widowed", #If two marital statuses are listed, assign main one
                                ifelse(maritalStatus == "unknown", NA, #If marital status in unknown, make NA
                                       maritalStatus)),
         maritalStatus_b = ifelse(maritalStatus == "divorced" | maritalStatus == "widowed" | maritalStatus == "single", "single", 
                                  maritalStatus), #Make a married vs single variable
         risk_perceptions.scale_impact = ifelse(risk_perceptions.scale_impact == "" | risk_perceptions.scale_impact == 0 | is.na(risk_perceptions.scale_impact), NA,
                                                ifelse(risk_perceptions.scale_impact == "same" | risk_perceptions.scale_impact == "noimpact", "nochange",
                                                       risk_perceptions.scale_impact)), #clean risk_perceptions.scale_impact variable
         ccimpact = ifelse(risk_perceptions.scale_impact == "nochange" | risk_perceptions.scale_impact == "better", "sameorbetter",
                           risk_perceptions.scale_impact)) #create a ccimpact var with categories: sameorbetter vs worse
    
#--------------------
# MISSING RESPONDENT INFO

# Deal with blank SharingUnitID_NEWs
data <- data %>%
  mutate(blankid = ifelse(!is.na(SharingUnitID_NEW), "no", "yes"), #Identify blank SharingUnitID_NEWs
         SharingUnitID_NEW = ifelse(!is.na(SharingUnitID_NEW), SharingUnitID_NEW,
                                    to("blankid_", sum(is.na(SharingUnitID_NEW))))) #Assign IDs to blank SharingUnitID_NEWs

#--------------------
# RESPONSE COLS

## Add 2016and2018 response col
data <- data %>%
  mutate(X2016and2018 = ifelse(SharingUnitID_NEW %in% unique(data$SharingUnitID_NEW[data$year == "2016"]) &
                                 SharingUnitID_NEW %in% unique(data$SharingUnitID_NEW[data$year == "2018"]),
                               "YES", "NO"))

#--------------------
# SAVE FINAL DATA
write.csv(data, "Data/msl_data/data_clean.csv")

#--------------------
# SAVE OUTPUTS
save.image("4-datacleaning.RData")
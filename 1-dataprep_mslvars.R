# load("1-dataprep_mslvars.RData")

#--------------------
# OPEN PACKAGES
library(dplyr)

#--------------------
# READ IN DATA
data_full <- read.csv("Data/data_final.csv")
meta <- read.csv("Data/meta.csv")

#--------------------
# COMBINE MSL VARS WHERE POSSIBLE
data_full <- data_full %>% 
  mutate(MSL.power.electricity_generator = if_else(MSL.power.generator == 1 |
                                                     MSL.power.electricity == 1,
                                                   1, 0),
         MSL.appliances.water.ANY = if_else(MSL.appliances.water_tank == 1 |
                                              MSL.appliances.piped_water == 1,
                                            1, #access to any water = includes water tank or piped water
                                            0), #no access to water
         MSL.cooking.modern = if_else(coalesce(MSL.cooking.gas_electric, 0) == 1 |
                                        coalesce(MSL.cooking.kerosene, 0) == 1,
                                      1, #more modern cooking = includes gas/electric, kerosene
                                      0), #less modern cooking = includes firewood, charcoal, nothing
         MSL.wall.plank_wood = if_else(MSL.wall.plank_wood == 1 | MSL2.other_wall_hardyplank == 1, 
                                       1, 0),
         MSL.wall.metal = if_else(MSL.wall.metal == 1 | MSL2.other_wall_panelrib == 1,
                                  1, 0),
         MSL.wall.cement = if_else(MSL.wall.cement | MSL.wall.fibro == 1,
                                   1, 0))

#--------------------
# FILTER AND COUNT MSL RESPONDENTS

# Filter to only include rows with complete MSL data 
data <- data_full %>% filter(Msl_RESPONSES == "YES")

# Count number of unique HHs surveyed
n_respondents <- data %>%
  mutate(year = if_else(year == 2002, 2001, year)) %>%
  count(community, year, name = "n_respondents")
n_respondents

# Selecting which baseline year to use for PCA:
## We want a baseline year with a high number of respondents in each AH and KA for the PCA 
## PCAs can produce inconsistent results when the number of variables included in the analysis is high relative to the number of observations (Johnstone & Lu, 2009; Osborne & Costello, 2004). 
### 2001 = 51 respondents in AH and 41 respondents in KA -> much smaller samples than in 2016, 2018 and 2026. Would need to reduce number of MSL items included in PCAs to limit noise and ensure stable results.
### 2009 = 38 respondents in AH and 19 respondents in KA -> even smaller samples than 2001. Does not make sense to use as the baseline year over 2001.
### 2012 = 36 respondents in AH and 43 respondents in KA -> even smaller sample for AH than 2001, similar sample for KA as 2001. Does not make sense to use as the baseline year over 2001.
### 2016 = 88 respondents in AH and 82 respondents in KA -> highest number of respondents in KA in any survey year, relatively high in AH. Less concerns about unstable results. Could track 10 years of change in MSL from this baseline.
### 2018 = 135 respondents in AH, none in KA -> not an option if we want to examine KA.
### 2026 = 120 respondents in AH, 50 respondents in KA -> not ideal for use as a baseline year, changes in MSL would be assessed retrospectively. Also small sample size for KA relative to 2016.
## Baseline year chosen for PCA = 2016

# Subset 2016-2026 data
data_2016to2026 <- subset(data, year %in% c(2016, 2018, 2026))

#--------------------
# CLEAN MSL DATA

# Select only the binary MSL cols from the 2016 to 2026 data
colnames(data_2016to2026)
msldata_all <- data_2016to2026 %>% 
  select(year, community, MSL.power:MSL.structure.rooms, MSL.power.electricity_generator:MSL.cooking.modern) %>% #include year + community + all msl cols
  select(-c(MSL2.expenditure, MSL2.credit_access, MSL2.credit_access.binary)) %>% #remove expenditure and credit cols
  select(-c(MSL.power, MSL.appliances, MSL.lighting, #remove descriptive cols
            MSL.transport, MSL.cooking, MSL.roof, MSL.floor, 
            MSL.wall, MSL.fence, MSL.store_furniture, MSL.other, 
            MSL2.note_structure, MSL2.other_wall,
            MSL2.other_appliances, MSL.structure)) %>%
  select(-c(MSL.transport.motorcycle, #included in msl.trans.vehicle
            MSL.power.generator:MSL.power.car_battery, #included in MSL.power.electricity_generator
            MSL.cooking.firewood:MSL.cooking.charcoal, #included in MSL.cooking.modern
            MSL.appliances.water_tank, MSL.appliances.piped_water, #included in MSL.appliances.water.ANY
            MSL2.other_wall_hardyplank, #included in MSL.wall.plank_wood
            MSL2.other_wall_panelrib, #included in MSL.wall.metal,
            MSL.wall.fibro)) %>% #included in MSL.wall.cement
  select(-c(MSL2.number_structures:MSL2.number_boat_motor, MSL2.number_chickens, MSL2.number_pigs, #remove non-binary items
            MSL2.other_wall.plywood_inside, MSL2.other_roof)) #remove other items
colnames(msldata_all)

# Identify MSL vars consistently measured across all survey periods (2016, 2018, 2026)
msldata_2016 <- msldata_all %>% #identify vars measured in 2016
  filter(year == 2016) %>%
  select(where(~ !all(is.na(.)))) %>%
  select(-c(year, community))
colnames(msldata_2016)
ncol(msldata_2016)

msldata_2018and2026 <- msldata_all %>% #everything measured in 2016 was measured in 2018+2026 plus some extras
  select(-c(year, community)) 
colnames(msldata_2018and2026)
ncol(msldata_2018and2026)

common_cols <- intersect(names(msldata_2016), names(msldata_2018and2026))
common_cols
length(common_cols) #26 MSL vars available for analysis

# Subset MSL columns to be included in the PCA
msldata_all <- msldata_all %>% 
  select(year, community, all_of(common_cols))
colnames(msldata_all)

# Subset into AH and KA data
msldata_AH_all <- msldata_all %>%
  subset(community == "AH") %>%
  select(-community)
msldata_KA_all <- msldata_all %>%
  subset(community == "KA") %>%
  select(-community)

# Subset 2016 MSL data for PCA
msl_AH2016 <- msldata_AH_all %>%
  subset(year == 2016) %>%
  select(-year)
colnames(msl_AH2016)
msl_KA2016 <- msldata_KA_all %>%
  subset(year == 2016) %>%
  select(-year)
colnames(msl_KA2016)

#--------------------
# SUBSET COMPLETE DATA
data_AH2016 <- subset(data, year == 2016 & community == "AH")
data_KA2016 <- subset(data, year == 2016 & community == "KA")
data_AH2018 <- subset(data, year == 2018 & community == "AH")
data_KA2018 <- subset(data, year == 2018 & community == "KA")
data_AH2026 <- subset(data, year == 2026 & community == "AH")
data_KA2026 <- subset(data, year == 2026 & community == "KA")
data_AH <- subset(data, community == "AH")
data_KA <- subset(data, community == "KA")
data_AH_2016to2026 <- subset(data, year %in% c(2016, 2018, 2026) & community == "AH")
data_KA_2016to2026 <- subset(data, year %in% c(2016, 2018, 2026) & community == "KA")

#--------------------
# SAVE OUTPUTS
save.image("1-dataprep_mslvars.RData")
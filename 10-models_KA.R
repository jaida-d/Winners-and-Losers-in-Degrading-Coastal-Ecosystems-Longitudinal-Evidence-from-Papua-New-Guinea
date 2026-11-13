# load("10-models_KA.RData")

#--------------------
# OPEN PACKAGES
library(dplyr)
library(cmdstanr)
library(brms)
options(mc.cores = 4, brms.backend = 'cmdstanr', brms.file_refit = 'on_change')

#--------------------
# LOAD DATA

# Read in final combined data
data <- read.csv("Data/msl_data/data_final.csv")

# Read in meta data
meta <- read.csv("Data/meta.csv")

# Subset Ahus and Karkar data
data_AH <- data %>%
  filter(community == "AH")
data_KA <- data %>%
  filter(community == "KA")

#--------------------
# CLEAN DATA

# Check NAs
KA_modelvars <- data_KA %>%
  select(RC1, RC2, RC3, fi_index,
         year, SharingUnitID_NEW,
         clan, gender, age.recorded, maritalStatus, education, english, leader, work, migrantStatus, hh_size, n_adults, n_children,
         livelihoods.primary, livelihoods.mix, livelihoods.types, livelihoods.diversity,
         social_capital.decision_involvement, social_capital.marine_resources, social_capital.trust.index,
         self.efficacy.cope_impacts,
         risk_perceptions.scale_impact) %>%
  group_by(year) %>%
  summarise(across(everything(), ~ sum(is.na(.))))
# View(KA_modelvars)

# Check and clean data types
str(data_KA, list.len = Inf)
data_KA <- data_KA %>%
  mutate(year_int = year) %>%
  mutate(across(c(year,
                  village, community,
                  SharingUnitID_NEW,
                  clan, subclan,
                  gender, age.group, maritalStatus, maritalStatus_b, english, leader, salariedEmploy, work, migrantStatus, Religion,
                  livelihoods.remittances,
                  social_capital.decision_involvement, social_capital.marine_resources, social_capital.events_involvement,
                  self.efficacy.human_agency, self.efficacy.changes_impacts, self.efficacy.cope_impacts, self.efficacy.main_change_still, self.efficacy.want_adapt,
                  risk_perceptions.cc_impactation, risk_perceptions.comparison_impact, risk_perceptions.scale_impact, ccimpact, risk_perceptions.comparison_adapt,
                  livelihoods.primary, livelihoods.primary.specific, livelihoods.secondary, livelihoods.mix,
                  livelihoods.primary.marine, livelihoods.primary.agri),
                as.factor))

# Check levels of categorical data
table(data_KA$year) 

table(data_KA$year, data_KA$gender, useNA = "ifany")
levels(data_KA$gender)

table(data_KA$year, data_KA$migrantStatus, useNA = "ifany") 
levels(data_KA$migrantStatus)
## Not enough people in each level to include

table(data_KA$year, data_KA$livelihoods.primary, useNA = "ifany")
levels(data_KA$livelihoods.primary)
data_KA <- data_KA %>% 
  mutate(livelihoods.primary = relevel(livelihoods.primary, ref = "marine"))

data_KA <- data_KA %>%
  mutate(livelihoods.agriculture = as.factor(if_else(livelihoods.primary == "agriculture", 1, 0)))
table(data_KA$year, data_KA$livelihoods.agriculture)

table(data_KA$year, data_KA$maritalStatus_b, useNA = "ifany") 
levels(data_KA$maritalStatus_b)
# No data in 2016

table(data_KA$year, data_KA$age.group, useNA = "ifany") 
levels(data_KA$age.group)

#--------------------
# Standardise function for continuous vars

stdize <- function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}

#--------------------
# TRANSFORM DATA FOR REGRESSION TYPE

# For ordinal regression, make values ordinal
data_KA <- data_KA %>%
  mutate(RC1_ord = ordered(RC1, levels = sort(unique(RC1))),
         RC2_ord = ordered(RC2, levels = sort(unique(RC2))),
         RC3_ord = ordered(RC3, levels = sort(unique(RC3))))

#--------------------
# EFFECT OF YEAR ON MSL

# Check MSL data 
table(data_KA$year, useNA = "ifany") 

# MSL models
KA_RC1_year <- brm(
  bf(RC1_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_year)
pp_check(KA_RC1_year, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_year, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_year, type="trace", variable = c("b_year2026"))

KA_RC2_year <- brm(
  bf(RC2_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_year)
pp_check(KA_RC2_year, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_year, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_year, type="trace", variable = c("b_year2026"))

KA_RC3_year <- brm(
  bf(RC3_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_year)
pp_check(KA_RC3_year, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_year, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_year, type="trace", variable = c("b_year2026"))

#--------------------
# EFFECT OF GENDER ON MSL

# Check gender data
table(data_KA$year, data_KA$gender, useNA = "ifany") #good, use data_KA

# MSL models
KA_RC1_gender <- brm(
  bf(RC1_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_gender)
pp_check(KA_RC1_gender, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(KA_RC1_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_gender, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_gender, type="trace", variable = c("b_year2026", "b_genderm", "b_year2026:genderm"))

KA_RC2_gender <- brm(
  bf(RC2_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_gender)
pp_check(KA_RC2_gender, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(KA_RC2_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_gender, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_gender, type="trace", variable = c("b_year2026", "b_genderm", "b_year2026:genderm"))

KA_RC3_gender <- brm(
  bf(RC3_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 1000,
  seed = 17,
  sample_prior = "yes") 
summary(KA_RC3_gender)
pp_check(KA_RC3_gender, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(KA_RC3_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_gender, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_gender, type="trace", variable = c("b_year2026", "b_genderm", "b_year2026:genderm"))

#--------------------
# EFFECT OF PRIMARY AGRICULTURAL LIVELIHOODS ON MSL

# Check livelihoods.agriculture data
table(data_KA$year)
table(data_KA$year, data_KA$gender, useNA = "ifany") #good
table(data_KA$year, data_KA$livelihoods.agriculture, useNA = "ifany") #good
table(data_KA$year, data_KA$age.recorded, useNA = "ifany") #7 NAs in 2016 (8.5%), 6 in 2026 (12%) = do not include
table(data_KA$year, data_KA$hh_size, useNA = "ifany") #9 NAs in 2026 (18%) = do not include

KA_RC1_liv.pri <- brm(
  bf(RC1_ord ~ year*livelihoods.agriculture + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_liv.pri)
pp_check(KA_RC1_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.agriculture", ndraws = 200) 
pp_check(KA_RC1_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_liv.pri, type="trace", variable = c("b_year2026", "b_livelihoods.agriculture1", "b_year2026:livelihoods.agriculture1"))

KA_RC2_liv.pri <- brm(
  bf(RC2_ord ~ year*livelihoods.agriculture + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_liv.pri)
pp_check(KA_RC2_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.agriculture", ndraws = 200) 
pp_check(KA_RC2_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_liv.pri, type="trace", variable = c("b_year2026", "b_livelihoods.agriculture1", "b_year2026:livelihoods.agriculture1"))

KA_RC3_liv.pri <- brm(
  bf(RC3_ord ~ year*livelihoods.agriculture + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_liv.pri)
pp_check(KA_RC3_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.agriculture", ndraws = 200) 
pp_check(KA_RC3_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_liv.pri, type="trace", variable = c("b_year2026", "b_livelihoods.agriculture1", "b_year2026:livelihoods.agriculture1"))

#--------------------
# EFFECT OF LIVELIHOOD DIVERSITY ON MSL

# Check livelihoods.diversity data
table(data_KA$year)
table(data_KA$year, data_KA$gender, useNA = "ifany") #good
table(data_KA$year, data_KA$livelihoods.agriculture, useNA = "ifany") #good
table(data_KA$year, data_KA$livelihoods.diversity, useNA = "ifany") #good
table(data_KA$year, data_KA$age.recorded, useNA = "ifany") #7 NAs in 2016 (8.5%), 6 in 2026 (12%) = do not include
table(data_KA$year, data_KA$hh_size, useNA = "ifany") #9 NAs in 2026 (18%) = do not include

# MSL models
KA_RC1_liv.div <- brm(
  bf(RC1_ord ~ year*stdize(livelihoods.diversity) + livelihoods.agriculture + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_liv.div)
pp_check(KA_RC1_liv.div, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_liv.div, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_liv.div, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_liv.div, type="trace", variable = c("b_year2026", "b_stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

KA_RC2_liv.div <- brm(
  bf(RC2_ord ~ year*stdize(livelihoods.diversity) + livelihoods.agriculture + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_liv.div)
pp_check(KA_RC2_liv.div, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_liv.div, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_liv.div, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_liv.div, type="trace", variable = c("b_year2026", "b_stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

KA_RC3_liv.div <- brm(
  bf(RC3_ord ~ year*stdize(livelihoods.diversity) + livelihoods.agriculture + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_liv.div)
pp_check(KA_RC3_liv.div, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_liv.div, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_liv.div, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_liv.div, type="trace", variable = c("b_year2026", "b_stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

#--------------------
# EFFECT OF AGE ON MSL

# Check age data
table(data_KA$year, data_KA$age.recorded, useNA = "ifany") #7 NAs in 2016 (8.5%), 6 in 2026 (12%)
data_KA_age <- data_KA %>%
  filter(!is.na(age.recorded))

# MSL Models
KA_RC1_age <- brm(
  bf(RC1_ord ~ year*stdize(age.recorded) +
       (1 | SharingUnitID_NEW)),
  data = data_KA_age,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_age)
pp_check(KA_RC1_age, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_age, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_age, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_age, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_age, type="trace", variable = c("b_year2026", "b_stdizeage.recorded", "b_year2026:stdizeage.recorded"))

KA_RC2_age <- brm(
  bf(RC2_ord ~ year*stdize(age.recorded) +
       (1 | SharingUnitID_NEW)),
  data = data_KA_age,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_age)
pp_check(KA_RC2_age, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_age, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_age, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_age, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_age, type="trace", variable = c("b_year2026", "b_stdizeage.recorded", "b_year2026:stdizeage.recorded"))

KA_RC3_age <- brm(
  bf(RC3_ord ~ year*stdize(age.recorded) +
       (1 | SharingUnitID_NEW)),
  data = data_KA_age,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_age)
pp_check(KA_RC3_age, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_age, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_age, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_age, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_age, type="trace", variable = c("b_year2026", "b_stdizeage.recorded", "b_year2026:stdizeage.recorded"))

#--------------------
# EFFECT OF DECISION-MAKING ON MSL

# Check social_capital.decision.index data
table(data_KA$year)
table(data_KA$year, data_KA$gender, useNA = "ifany") #good
table(data_KA$year, data_KA$age.recorded, useNA = "ifany") #7 NAs in 2016 (8.5%), 6 in 2026 (12%) = do not use
table(data_KA$year, data_KA$hh_size, useNA = "ifany") #9 NAs in 2026 (18%) = do not use
table(data_KA$year, data_KA$social_capital.decision.index, useNA = "ifany") #good

#MSL models
KA_RC1_decision <- brm(
  bf(RC1_ord ~ year*stdize(social_capital.decision.index) + gender + 
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_decision)
pp_check(KA_RC1_decision, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_decision, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_decision, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_decision, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

KA_RC2_decision <- brm(
  bf(RC2_ord ~ year*stdize(social_capital.decision.index) + gender + 
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_decision)
pp_check(KA_RC2_decision, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_decision, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_decision, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_decision, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

KA_RC3_decision <- brm(
  bf(RC3_ord ~ year*stdize(social_capital.decision.index) + gender + 
       (1 | SharingUnitID_NEW)),
  data = data_KA,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_decision)
pp_check(KA_RC3_decision, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_decision, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_decision, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_decision, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

#--------------------
# EFFECT OF TRUST ON MSL
table(data_KA$year)
table(data_KA$year, data_KA$gender, useNA = "ifany") #good
table(data_KA$year, data_KA$age.recorded, useNA = "ifany") #7 NAs in 2016 (8.5%), 6 in 2026 (12%) = do not use
table(data_KA$year, data_KA$social_capital.trust.index, useNA = "ifany") #18 NAs in 2016 (22%), 6 in 2026 (12%)
data_KA_trust <- data_KA %>% #Remove NAs from dataset
  filter(!is.na(social_capital.trust.index))

# MSL models
KA_RC1_trust <- brm(
  bf(RC1_ord ~ year*stdize(social_capital.trust.index) + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA_trust,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_trust)
pp_check(KA_RC1_trust, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC1_trust, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_trust, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC1_trust, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

KA_RC2_trust <- brm(
  bf(RC2_ord ~ year*stdize(social_capital.trust.index) + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA_trust,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_trust)
pp_check(KA_RC2_trust, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC2_trust, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_trust, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC2_trust, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

KA_RC3_trust <- brm(
  bf(RC3_ord ~ year*stdize(social_capital.trust.index) + gender +
       (1 | SharingUnitID_NEW)),
  data = data_KA_trust,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_trust)
pp_check(KA_RC3_trust, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(KA_RC3_trust, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_trust, type = "bars_grouped", group = "year") 
mcmc_plot(KA_RC3_trust, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

#--------------------
# EFFECT OF CLIMATE CHANGE IMPACT ON MSL

# Check ccimpact data
table(data_KA$year) #2026 only
table(data_KA$year, data_KA$gender, useNA = "ifany") #good
table(data_KA$year, data_KA$livelihoods.agriculture, useNA = "ifany") #good
table(data_KA$year, data_KA$age.recorded, useNA = "ifany") #7 NAs in 2016 (8.5%), 6 in 2026 (12%) = do not use
table(data_KA$year, data_KA$hh_size, useNA = "ifany") #9 NAs in 2026 (18%) = do not use
table(data_KA$year, data_KA$social_capital.decision.index, useNA = "ifany") #good
table(data_KA$year, data_KA$ccimpact, useNA = "ifany") #All NAs in 2016, 11 NAs in 2026 (22%)
data_KA_ccimpact <- data_KA %>% #Remove NAs from dataset
  filter(!is.na(ccimpact)) %>%
  mutate(stdizesocial_capital.decision.index = stdize(social_capital.decision.index))

# MSL models
KA_RC1_ccimpact <- brm( 
  bf(RC1_ord ~ ccimpact + gender + livelihoods.agriculture + stdizesocial_capital.decision.index +
       (1 | SharingUnitID_NEW)),
  data = data_KA_ccimpact,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC1_ccimpact)
pp_check(KA_RC1_ccimpact, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC1_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200) 
pp_check(KA_RC1_ccimpact, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC1_ccimpact, type = "bars_grouped", group = "ccimpact") 
mcmc_plot(KA_RC1_ccimpact, type="trace", variable = c("b_ccimpactworse"))

KA_RC2_ccimpact <- brm( 
  bf(RC2_ord ~ ccimpact + gender + livelihoods.agriculture + stdizesocial_capital.decision.index +
       (1 | SharingUnitID_NEW)),
  data = data_KA_ccimpact,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC2_ccimpact)
pp_check(KA_RC2_ccimpact, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC2_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200) 
pp_check(KA_RC2_ccimpact, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC2_ccimpact, type = "bars_grouped", group = "ccimpact") 
mcmc_plot(KA_RC2_ccimpact, type="trace", variable = c("b_ccimpactworse"))

KA_RC3_ccimpact <- brm( 
  bf(RC3_ord ~ ccimpact + gender + livelihoods.agriculture + stdizesocial_capital.decision.index +
       (1 | SharingUnitID_NEW)),
  data = data_KA_ccimpact,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(KA_RC3_ccimpact)
pp_check(KA_RC3_ccimpact, type = "dens_overlay", ndraws = 200) 
pp_check(KA_RC3_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200) 
pp_check(KA_RC3_ccimpact, type = "ecdf_overlay", ndraws = 200) 
pp_check(KA_RC3_ccimpact, type = "bars_grouped", group = "ccimpact") 
mcmc_plot(KA_RC3_ccimpact, type="trace", variable = c("b_ccimpactworse"))

#--------------------
# SAVE OUTPUTS
save.image("10-models_KA.RData")

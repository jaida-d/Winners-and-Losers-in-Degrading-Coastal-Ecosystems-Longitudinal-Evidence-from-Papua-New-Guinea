load("8-models_AH.RData")

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
AH_modelvars <- data_AH %>%
  select(RC1, RC2, RC3, fi_index,
         year, SharingUnitID_NEW,
         clan, gender, age.recorded, maritalStatus, education, english, leader, work, migrantStatus, hh_size, n_adults, n_children,
         livelihoods.primary, livelihoods.mix, livelihoods.types, livelihoods.diversity,
         social_capital.decision_involvement, social_capital.marine_resources, social_capital.trust.index,
         self.efficacy.cope_impacts,
         risk_perceptions.scale_impact) %>%
  group_by(year) %>%
  summarise(across(everything(), ~ sum(is.na(.))))
# View(AH_modelvars)

# Check and clean data types
str(data_AH, list.len = Inf)
data_AH <- data_AH %>%
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
table(data_AH$year) 

table(data_AH$year, data_AH$gender, useNA = "ifany")
levels(data_AH$gender)

table(data_AH$year, data_AH$migrantStatus, useNA = "ifany") 
levels(data_AH$migrantStatus)
## Not enough people in each level to include

table(data_AH$year, data_AH$livelihoods.primary, useNA = "ifany")
levels(data_AH$livelihoods.primary)
data_AH <- data_AH %>% 
  mutate(livelihoods.primary = relevel(livelihoods.primary, ref = "marine"))

data_AH <- data_AH %>%
  mutate(livelihoods.agriculture = as.factor(if_else(livelihoods.primary == "agriculture", 1, 0)))
table(data_AH$year, data_AH$livelihoods.agriculture)

table(data_AH$year, data_AH$maritalStatus_b, useNA = "ifany") 
levels(data_AH$maritalStatus_b)
# No data in 2016

table(data_AH$year, data_AH$age.group, useNA = "ifany") 
levels(data_AH$age.group)

#--------------------
# Standardise function for continuous vars

stdize <- function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}

#--------------------
# TRANSFORM DATA FOR REGRESSION TYPE

# For ordinal regression, make values ordinal
data_AH <- data_AH %>%
  mutate(RC1_ord = ordered(RC1, levels = sort(unique(RC1))),
         RC2_ord = ordered(RC2, levels = sort(unique(RC2))),
         RC3_ord = ordered(RC3, levels = sort(unique(RC3))),
         fi_ord = ordered(fi_index, levels = sort(unique(fi_index))))

#--------------------
# EFFECT OF YEAR ON MSL

# Check MSL data 
table(data_AH$year, useNA = "ifany") 

# MSL models
AH_RC1_year <- brm(
  bf(RC1_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_AH,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC1_year)
pp_check(AH_RC1_year, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC1_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC1_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC1_year, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC1_year, type="trace", variable = c("b_year2018", "b_year2026"))

AH_RC2_year <- brm(
  bf(RC2_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_AH,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC2_year)
pp_check(AH_RC2_year, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC2_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC2_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC2_year, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC2_year, type="trace", variable = c("b_year2018", "b_year2026"))

AH_RC3_year <- brm(
  bf(RC3_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_AH,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC3_year)
pp_check(AH_RC3_year, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC3_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC3_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC3_year, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC3_year, type="trace", variable = c("b_year2018", "b_year2026"))

# Check FI data
table(data_AH$year, data_AH$fi_ord, useNA = "ifany") 
data_AH_fi <- data_AH %>%
  filter(!is.na(fi_ord))

# FI Model
AH_fi_year <- brm(
  bf(fi_ord ~ year +
       (1 | SharingUnitID_NEW)),
  data = data_AH_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.999,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_fi_year)
pp_check(AH_fi_year, type = "dens_overlay", ndraws = 200) 
pp_check(AH_fi_year, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_fi_year, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_fi_year, type = "bars") 
pp_check(AH_fi_year, type = "bars_grouped", group = "year") 
mcmc_plot(AH_fi_year, type="trace", variable = c("b_year2026"))

#--------------------
# EFFECT OF GENDER ON MSL

# Check gender data
table(data_AH$year, data_AH$gender, useNA = "ifany") #1 NA in 2026
data_AH_gender <- data_AH %>%
  filter(!is.na(gender))

# MSL models
AH_RC1_gender <- brm(
  bf(RC1_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_AH_gender,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.999,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC1_gender)
pp_check(AH_RC1_gender, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC1_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC1_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(AH_RC1_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC1_gender, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC1_gender, type="trace", variable = c("b_year2018", "b_year2026", "b_genderm", "b_year2018:genderm", "b_year2026:genderm"))

AH_RC2_gender <- brm(
  bf(RC2_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_AH_gender,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC2_gender)
pp_check(AH_RC2_gender, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC2_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC2_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(AH_RC2_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC2_gender, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC2_gender, type="trace", variable = c("b_year2018", "b_year2026", "b_genderm", "b_year2018:genderm", "b_year2026:genderm"))

AH_RC3_gender <- brm(
  bf(RC3_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_AH_gender,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 1000,
  seed = 17) 
summary(AH_RC3_gender)
pp_check(AH_RC3_gender, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC3_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC3_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(AH_RC3_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC3_gender, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC3_gender, type="trace", variable = c("b_year2018", "b_year2026", "b_genderm", "b_year2018:genderm", "b_year2026:genderm"))

# Check FI data
table(data_AH_gender$year, data_AH_gender$fi_ord, useNA = "ifany")
data_AH_gender_fi <- data_AH_gender %>%
  filter(!is.na(fi_ord))

AH_fi_gender <- brm(
  bf(fi_ord ~ year*gender +
       (1 | SharingUnitID_NEW)),
  data = data_AH_gender_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_fi_gender)
pp_check(AH_fi_gender, type = "dens_overlay", ndraws = 200) 
pp_check(AH_fi_gender, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_fi_gender, type = "dens_overlay_grouped", group = "gender", ndraws = 200) 
pp_check(AH_fi_gender, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_fi_gender, type = "bars") 
pp_check(AH_fi_gender, type = "bars_grouped", group = "year") 
mcmc_plot(AH_fi_gender, type="trace", variable = c("b_year2026", "b_genderm", "b_year2026:genderm"))

#--------------------
# EFFECT OF PRIMARY MARINE LIVELIHOODS ON MSL

# Check livelihoods data
table(data_AH$year, data_AH$gender, useNA = "ifany") #1 NA in 2026
table(data_AH$year, data_AH$age.recorded, useNA = "ifany") #3 NAs in 2018, 5 NAs in 2026
table(data_AH$year, data_AH$livelihoods.primary.marine, useNA = "ifany")
table(data_AH$year, data_AH$hh_size, useNA = "ifany")
data_AH_livelihoods.primary <- data_AH %>%
  filter(!is.na(gender) & !is.na(age.recorded)) %>%
  mutate(stdizehh_size = stdize(hh_size),
         stdizeage.recorded = stdize(age.recorded))

# MSL models
AH_RC1_liv.pri <- brm(
  bf(RC1_ord ~ year*livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC1_liv.pri)
pp_check(AH_RC1_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC1_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC1_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.primary.marine", ndraws = 200) 
pp_check(AH_RC1_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC1_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC1_liv.pri, type="trace", variable = c("b_year2018", "b_year2026", "b_livelihoods.primary.marine1", "b_year2018:livelihoods.primary.marine1", "b_year2026:livelihoods.primary.marine1"))

AH_RC2_liv.pri <- brm(
  bf(RC2_ord ~ year*livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC2_liv.pri)
pp_check(AH_RC2_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC2_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC2_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.primary.marine", ndraws = 200) 
pp_check(AH_RC2_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC2_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC2_liv.pri, type="trace", variable = c("b_year2018", "b_year2026", "b_livelihoods.primary.marine1", "b_year2018:livelihoods.primary.marine1", "b_year2026:livelihoods.primary.marine1"))

AH_RC3_liv.pri <- brm(
  bf(RC3_ord ~ year*livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_RC3_liv.pri)
pp_check(AH_RC3_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(AH_RC3_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_RC3_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.primary.marine", ndraws = 200) 
pp_check(AH_RC3_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_RC3_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(AH_RC3_liv.pri, type="trace", variable = c("b_year2018", "b_year2026", "b_livelihoods.primary.marine1", "b_year2018:livelihoods.primary.marine1", "b_year2026:livelihoods.primary.marine1"))

# Check FI data
table(data_AH_livelihoods.primary$year, data_AH_livelihoods.primary$fi_ord, useNA = "ifany")
data_AH_livelihoods.primary_fi <- data_AH_livelihoods.primary %>%
  filter(!is.na(fi_ord))

# FI Model
AH_fi_liv.pri <- brm(
  bf(fi_ord ~ year*livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0, 
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17) 
summary(AH_fi_liv.pri)
pp_check(AH_fi_liv.pri, type = "dens_overlay", ndraws = 200) 
pp_check(AH_fi_liv.pri, type = "dens_overlay_grouped", group = "year", ndraws = 200) 
pp_check(AH_fi_liv.pri, type = "dens_overlay_grouped", group = "livelihoods.primary.marine", ndraws = 200) 
pp_check(AH_fi_liv.pri, type = "ecdf_overlay", ndraws = 200) 
pp_check(AH_fi_liv.pri, type = "bars") 
pp_check(AH_fi_liv.pri, type = "bars_grouped", group = "year") 
mcmc_plot(AH_fi_liv.pri, type="trace", variable = c("b_year2026", "b_livelihoods.primary.marine1", "b_year2026:livelihoods.primary.marine1"))

#--------------------
# EFFECT OF LIVELIHOOD DIVERSITY ON MSL

# Check livelihoods.diversity data
table(data_AH$year, data_AH$gender, useNA = "ifany") #1 NA in 2026
table(data_AH$year, data_AH$age.recorded, useNA = "ifany") #3 NAs in 2018, 5 NAs in 2026
table(data_AH$year, data_AH$livelihoods.primary.marine, useNA = "ifany") 
table(data_AH$year, data_AH$livelihoods.diversity, useNA = "ifany") 
table(data_AH$year, data_AH$hh_size, useNA = "ifany") 
#Nothing additional to remove, can use data_AH_livelihoods.primary
data_AH_livelihoods.primary <- data_AH_livelihoods.primary %>%
  mutate(stdizelivelihoods.diversity = stdize(livelihoods.diversity))

# MSL models
AH_RC1_liv.div <- brm(
  bf(RC1_ord ~ year*stdizelivelihoods.diversity + livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC1_liv.div)
pp_check(AH_RC1_liv.div, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC1_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC1_liv.div, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC1_liv.div, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC1_liv.div, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizelivelihoods.diversity", "b_year2018:stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

AH_RC2_liv.div <- brm(
  bf(RC2_ord ~ year*stdizelivelihoods.diversity + livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC2_liv.div)
pp_check(AH_RC2_liv.div, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC2_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC2_liv.div, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC2_liv.div, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC2_liv.div, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizelivelihoods.diversity", "b_year2018:stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

AH_RC3_liv.div <- brm(
  bf(RC3_ord ~ year*stdizelivelihoods.diversity + livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC3_liv.div)
pp_check(AH_RC3_liv.div, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC3_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC3_liv.div, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC3_liv.div, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC3_liv.div, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizelivelihoods.diversity", "b_year2018:stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

# FI data = nothing additional to remove, can use can use data_AH_livelihoods.primary_fi
data_AH_livelihoods.primary_fi <- data_AH_livelihoods.primary_fi %>%
  mutate(stdizelivelihoods.diversity = stdize(livelihoods.diversity))

AH_fi_liv.div <- brm(
  bf(fi_ord ~ year*stdizelivelihoods.diversity + livelihoods.primary.marine + gender + stdizehh_size + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_fi_liv.div)
pp_check(AH_fi_liv.div, type = "dens_overlay", ndraws = 200)
pp_check(AH_fi_liv.div, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_fi_liv.div, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_fi_liv.div, type = "bars")
pp_check(AH_fi_liv.div, type = "bars_grouped", group = "year")
mcmc_plot(AH_fi_liv.div, type="trace", variable = c("b_year2026", "b_stdizelivelihoods.diversity", "b_year2026:stdizelivelihoods.diversity"))

#--------------------
# EFFECT OF AGE ON MSL

# Check age.recorded data
table(data_AH$year, data_AH$age.recorded, useNA = "ifany") #3 NAs in 2018, 5 NAs in 2026
view_nas <- data_AH %>%
  filter(is.na(age.recorded)) %>%
  select(year, gender, age.recorded, hh_size)
view_nas  #Same as rows removed for data_AH_livelihoods.primary

# MSL models
AH_RC1_age <- brm(
  bf(RC1_ord ~ year*stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC1_age)
pp_check(AH_RC1_age, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC1_age, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC1_age, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC1_age, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC1_age, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizeage.recorded", "b_year2018:stdizeage.recorded", "b_year2026:stdizeage.recorded"))

AH_RC2_age <- brm(
  bf(RC2_ord ~ year*stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC2_age)
pp_check(AH_RC2_age, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC2_age, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC2_age, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC2_age, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC2_age, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizeage.recorded", "b_year2018:stdizeage.recorded", "b_year2026:stdizeage.recorded"))

AH_RC3_age <- brm(
  bf(RC3_ord ~ year*stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC3_age)
pp_check(AH_RC3_age, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC3_age, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC3_age, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC3_age, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC3_age, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizeage.recorded", "b_year2018:stdizeage.recorded", "b_year2026:stdizeage.recorded"))

# FI data = nothing additional to remove, can use can use data_AH_livelihoods.primary_fi

AH_fi_age <- brm(
  bf(fi_ord ~ year*stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_livelihoods.primary_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_fi_age)
pp_check(AH_fi_age, type = "dens_overlay", ndraws = 200)
pp_check(AH_fi_age, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_fi_age, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_fi_age, type = "bars")
pp_check(AH_fi_age, type = "bars_grouped", group = "year")
mcmc_plot(AH_fi_age, type="trace", variable = c("b_year2026", "b_stdizeage.recorded", "b_year2026:stdizeage.recorded"))

#--------------------
# EFFECT OF DECISION-MAKING ON MSL

# Check social_capital.decision.index data
table(data_AH$year, data_AH$gender, useNA = "ifany") #1 NA in 2026
table(data_AH$year, data_AH$age.recorded, useNA = "ifany") #3 NAs in 2018, 5 NAs in 2026
table(data_AH$year, data_AH$social_capital.decision.index, useNA = "ifany") #31 blanks in 2018
table(data_AH$year, data_AH$hh_size, useNA = "ifany") 
data_AH_decision <- data_AH %>%
  filter(!is.na(gender) & !is.na(age.recorded) & !is.na(social_capital.decision.index)) %>%
  mutate(stdizesocial_capital.decision.index = stdize(social_capital.decision.index),
         stdizeage.recorded = stdize(age.recorded),
         stdizehh_size = stdize(hh_size))

# MSL models
AH_RC1_decision <- brm(
  bf(RC1_ord ~ year*stdizesocial_capital.decision.index + gender + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_decision,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC1_decision)
pp_check(AH_RC1_decision, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC1_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC1_decision, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC1_decision, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC1_decision, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizesocial_capital.decision.index", "b_year2018:stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

AH_RC2_decision <- brm(
  bf(RC2_ord ~ year*stdizesocial_capital.decision.index + gender + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_decision,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC2_decision)
pp_check(AH_RC2_decision, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC2_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC2_decision, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC2_decision, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC2_decision, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizesocial_capital.decision.index", "b_year2018:stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

AH_RC3_decision <- brm(
  bf(RC3_ord ~ year*stdizesocial_capital.decision.index + gender + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_decision,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC3_decision)
pp_check(AH_RC3_decision, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC3_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC3_decision, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC3_decision, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC3_decision, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizesocial_capital.decision.index", "b_year2018:stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

# Check FI data
table(data_AH_decision$year, data_AH_decision$fi_ord, useNA = "ifany")
data_AH_decision_fi <- data_AH_decision %>%
  filter(!is.na(fi_ord))

# FI Model
AH_fi_decision <- brm(
  bf(fi_ord ~ year*stdizesocial_capital.decision.index + gender + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_decision_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_fi_decision)
pp_check(AH_fi_decision, type = "dens_overlay", ndraws = 200)
pp_check(AH_fi_decision, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_fi_decision, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_fi_decision, type = "bars")
pp_check(AH_fi_decision, type = "bars_grouped", group = "year")
mcmc_plot(AH_fi_decision, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.decision.index", "b_year2026:stdizesocial_capital.decision.index"))

#--------------------
# EFFECT OF TRUST ON MSL

# Check social_capital.trust.index data
table(data_AH$year, data_AH$gender, useNA = "ifany") #1 NA in 2026
table(data_AH$year, data_AH$age.recorded, useNA = "ifany") #3 NAs in 2018, 5 NAs in 2026
table(data_AH$year, data_AH$social_capital.trust.index, useNA = "ifany") #13 NAs in 2016, 49 NAs in 2018, 23 NAs in 2026
data_AH_trust <- data_AH %>%
  filter(!is.na(gender) & !is.na(age.recorded) & !is.na(social_capital.trust.index)) %>%
  mutate(stdizesocial_capital.trust.index = stdize(social_capital.trust.index),
         stdizeage.recorded = stdize(age.recorded))

# MSL models
AH_RC1_trust <- brm(
  bf(RC1_ord ~ year*stdizesocial_capital.trust.index + gender + stdizeage.recorded + 
       (1 | SharingUnitID_NEW)),
  data = data_AH_trust,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC1_trust)
pp_check(AH_RC1_trust, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC1_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC1_trust, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC1_trust, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC1_trust, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizesocial_capital.trust.index", "b_year2018:stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

AH_RC2_trust <- brm(
  bf(RC2_ord ~ year*stdizesocial_capital.trust.index + gender + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_trust,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC2_trust)
pp_check(AH_RC2_trust, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC2_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC2_trust, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC2_trust, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC2_trust, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizesocial_capital.trust.index", "b_year2018:stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

AH_RC3_trust <- brm(
  bf(RC3_ord ~ year*stdizesocial_capital.trust.index + gender + stdizeage.recorded + 
       (1 | SharingUnitID_NEW)),
  data = data_AH_trust,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC3_trust)
pp_check(AH_RC3_trust, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC3_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC3_trust, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC3_trust, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC3_trust, type="trace", variable = c("b_year2018", "b_year2026", "b_stdizesocial_capital.trust.index", "b_year2018:stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

# Check FI data
table(data_AH_trust$year, data_AH_trust$fi_ord, useNA = "ifany")
data_AH_trust_fi <- data_AH_trust %>%
  filter(!is.na(fi_ord))

# FI Model
AH_fi_trust <- brm(
  bf(fi_ord ~ year*year*stdizesocial_capital.trust.index + gender + stdizeage.recorded +
       (1 | SharingUnitID_NEW)),
  data = data_AH_trust_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_fi_trust)
pp_check(AH_fi_trust, type = "dens_overlay", ndraws = 200)
pp_check(AH_fi_trust, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_fi_trust, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_fi_trust, type = "bars")
pp_check(AH_fi_trust, type = "bars_grouped", group = "year")
mcmc_plot(AH_fi_trust, type="trace", variable = c("b_year2026", "b_stdizesocial_capital.trust.index", "b_year2026:stdizesocial_capital.trust.index"))

#--------------------
# EFFECT OF CLIMATE CHANGE IMPACT ON MSL

# Check ccimpact data
table(data_AH$year, data_AH$gender, useNA = "ifany") #1 NA in 2026
table(data_AH$year, data_AH$age.recorded, useNA = "ifany") #3 NAs in 2018, 5 NAs in 2026
table(data_AH$year, data_AH$livelihoods.primary.marine, useNA = "ifany") #good
table(data_AH$year, data_AH$hh_size, useNA = "ifany") #good
table(data_AH$year, data_AH$social_capital.decision.index, useNA = "ifany") #31 NAs in 2018 = DO NOT USE
table(data_AH$year, data_AH$ccimpact, useNA = "ifany") #All NAs in 2016 (remove year), 4 NAs in 2026
data_AH_ccimpact <- data_AH %>%
  filter(!is.na(gender) & !is.na(age.recorded) & !is.na(ccimpact)) %>%
  mutate(stdizeage.recorded = stdize(age.recorded),
         stdizehh_size = stdize(hh_size))

# MSL models
AH_RC1_ccimpact <- brm(
  bf(RC1_ord ~ year*ccimpact + gender + livelihoods.primary.marine + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_ccimpact,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC1_ccimpact)
pp_check(AH_RC1_ccimpact, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC1_ccimpact, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC1_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200)
pp_check(AH_RC1_ccimpact, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC1_ccimpact, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC1_ccimpact, type="trace", variable = c("b_year2026", "b_ccimpactworse", "b_year2026:ccimpactworse"))

AH_RC2_ccimpact <- brm(
  bf(RC2_ord ~ year*ccimpact + gender + livelihoods.primary.marine + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_ccimpact,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC2_ccimpact)
pp_check(AH_RC2_ccimpact, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC2_ccimpact, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC2_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200)
pp_check(AH_RC2_ccimpact, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC2_ccimpact, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC2_ccimpact, type="trace", variable = c("b_year2026", "b_ccimpactworse", "b_year2026:ccimpactworse"))

AH_RC3_ccimpact <- brm(
  bf(RC3_ord ~ year*ccimpact + gender + livelihoods.primary.marine + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_ccimpact,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_RC3_ccimpact)
pp_check(AH_RC3_ccimpact, type = "dens_overlay", ndraws = 200)
pp_check(AH_RC3_ccimpact, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_RC3_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200)
pp_check(AH_RC3_ccimpact, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_RC3_ccimpact, type = "bars_grouped", group = "year")
mcmc_plot(AH_RC3_ccimpact, type="trace", variable = c("b_year2026", "b_ccimpactworse", "b_year2026:ccimpactworse"))

# Check FI data
table(data_AH_ccimpact$year, data_AH_ccimpact$fi_ord, useNA = "ifany")
data_AH_ccimpact_fi <- data_AH_ccimpact %>%
  filter(!is.na(fi_ord))

# FI Model
AH_fi_ccimpact <- brm(
  bf(fi_ord ~ year*ccimpact + gender + livelihoods.primary.marine + stdizeage.recorded + stdizehh_size +
       (1 | SharingUnitID_NEW)),
  data = data_AH_ccimpact_fi,
  family = cumulative("logit"),
  prior = c(prior(cauchy(0, 2.5), class = "b"),
            prior(cauchy(0, 1), class = "Intercept")),
  iter = 10000, warmup = 2500, chains = 4, cores = 4, init = 0,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15),
  refresh = 100,
  seed = 17)
summary(AH_fi_ccimpact)
pp_check(AH_fi_ccimpact, type = "dens_overlay", ndraws = 200)
pp_check(AH_fi_ccimpact, type = "dens_overlay_grouped", group = "year", ndraws = 200)
pp_check(AH_fi_ccimpact, type = "dens_overlay_grouped", group = "ccimpact", ndraws = 200)
pp_check(AH_fi_ccimpact, type = "ecdf_overlay", ndraws = 200)
pp_check(AH_fi_ccimpact, type = "bars")
pp_check(AH_fi_ccimpact, type = "bars_grouped", group = "year")
mcmc_plot(AH_fi_ccimpact, type="trace", variable = c("b_ccimpactworse"))

#--------------------
# SAVE OUTPUTS
save.image("8-models_AH.RData")

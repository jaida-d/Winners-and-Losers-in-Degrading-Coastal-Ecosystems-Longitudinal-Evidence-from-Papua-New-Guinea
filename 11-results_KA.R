# load("11-results_KA.RData")

#--------------------
# OPEN PACKAGES
library(brms)
library(dplyr)
library(ggplot2)
library(writexl) 

#--------------------
# LOAD MODEL RESULTS
load("10-models_KA.RData")

#--------------------
# HYPOTHESIS FUNCTION
extract_hypothesis <- function(model, #fitted brms model (e.g., KA_RC1_year)
                               hypothesis_string, #hypothesis to evaluate, what would normally be put into hypothesis() function (e.g., "year2026 = 0")
                               component, #MSL component label (e.g., "RC1")
                               effect, #effect label (e.g., "Year (2026 vs 2016)")
                               predictor) { #predictor label (e.g., "Year")
  hypothesis(model, hypothesis_string, alpha = 0.1)$hypothesis %>% #extract 90% CrIs
    mutate(CI.Lower50 = hypothesis(model, hypothesis_string, alpha = 0.5)$hypothesis$CI.Lower, #extract 50% CrIs
           CI.Upper50 = hypothesis(model, hypothesis_string, alpha = 0.5)$hypothesis$CI.Upper,
           Prob_greater1 = hypothesis(model, sub("=", ">", hypothesis_string))$hypothesis$Post.Prob, #extract post probs
           Prob_less1 = hypothesis(model, sub("=", "<", hypothesis_string))$hypothesis$Post.Prob) %>%
    mutate(across(c(Estimate, CI.Lower, CI.Upper, CI.Lower50, CI.Upper50), exp)) %>% #convert from log-odds to odds ratios
    select(-c(Evid.Ratio, Post.Prob, Star)) %>% #remove unused cols
    mutate(component = component, #add component, effect and predictor label cols
           effect = effect,
           predictor = predictor)}

#--------------------
# EFFECT OF YEAR ON MSL

# Extract results for each effect 
KA_hyp_RC1_year.26v16 <- extract_hypothesis(KA_RC1_year, "year2026 = 0", "RC1", "Year (2026 vs 2016)", "Year")
KA_hyp_RC2_year.26v16 <- extract_hypothesis(KA_RC2_year, "year2026 = 0", "RC2", "Year (2026 vs 2016)", "Year")
KA_hyp_RC3_year.26v16 <- extract_hypothesis(KA_RC3_year, "year2026 = 0", "RC3", "Year (2026 vs 2016)", "Year")

# Combine results and save
hyp_KA_year <- bind_rows(KA_hyp_RC1_year.26v16, KA_hyp_RC2_year.26v16, KA_hyp_RC3_year.26v16) %>%
  select(component, effect, everything())

write_xlsx(hyp_KA_year, "Plots/results_KA_year.xlsx")

# Clean results for plotting
hyp_KA_year_clean <- hyp_KA_year %>%
  mutate(component = factor(component, levels = c("RC1", "RC2", "RC3"), #Rename RC1, RC2 and RC3
                            labels = c("Housing Quality",
                                       "Electrical Assets",
                                       "Roofing Quality")),
         effect_order = row_number(), #give each row in hyp_KA_year a number
         effect_order = factor(effect_order, #reverse factor order so first effect appears first
                               levels = rev(unique(effect_order))))

# Convert from OR to log-odds for whisker plots
hyp_KA_year_whisker <- hyp_KA_year_clean %>%
  mutate(across(c(Estimate, CI.Lower, CI.Upper, CI.Lower50, CI.Upper50), log)) 

# Whisker plots (combine with Ahus in "12-plots.R")
KA_whiskerplot_year <- ggplot(hyp_KA_year_whisker, #use log-odds data for whisker plots
                              aes(x = Estimate, y = effect_order, colour = component)) +
  geom_vline(xintercept = 0, #add reference line at 0 (no effect)
             linetype = "dashed",
             colour = "grey65") +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper), #90% CrIs
                 width = 0, linewidth = 0.8) +
  geom_errorbarh(aes(xmin = CI.Lower50, xmax = CI.Upper50), #50% CrIs
                 width = 0, linewidth = 2) +
  geom_point(size = 3) + #estimate point
  facet_wrap(vars(component), ncol = 1, #make a panel for each component (RC1/RC2/RC3) all in one column
             strip.position = "left", #labels for each component on left of panels
             scales = "free_y", #y-axis just contains effects for component in each panel
             space = "free_y") + #adjust height of each panel depending on number of effects plotted
  geom_text(aes(x = Estimate, label = effect), #add effect labels above each estimate
            vjust = -0.7, size = 3.5) +
  scale_y_discrete(expand = expansion(add = 1.5)) + #adjust spacing between each effect
  scale_colour_manual(values = c("Housing Quality" = "darkcyan", #set different colours for each msl index
                                 "Electrical Assets" = "mediumpurple3",
                                 "Roofing Quality" = "coral3")) +
  labs(x = "Cumulative Log-Odds", 
       y = "MSL Index",
       title = "Karkar") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10, colour = "black"), #text formatting
        axis.text.x = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 10, colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none", #remove legend
        strip.placement = "outside", #place component labels outside panels (next to y axis)
        strip.background = element_blank(), #panel formatting
        panel.spacing.y = unit(0, "cm"),
        panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4))
KA_whiskerplot_year

# Extract conditional effects 
ce_KA_RC1_year <- conditional_effects(KA_RC1_year,
                                      effects = "year",
                                      re_formula = NA,
                                      prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))

ce_KA_RC2_year <- conditional_effects(KA_RC2_year,
                                      effects = "year",
                                      re_formula = NA,
                                      prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))

ce_KA_RC3_year <- conditional_effects(KA_RC3_year,
                                      effects = "year",
                                      re_formula = NA,
                                      prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_year <- function(data,title,colour){
  ggplot(data, aes(year, estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, fill = colour) +
    geom_line(linewidth = 1.4, colour = colour) +
    geom_point(size = 2.5, colour = colour) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_year_plot <- KA_plot_year(ce_KA_RC1_year, "Karkar: Housing Quality", "darkcyan")
ce_KA_RC1_year_plot
ce_KA_RC2_year_plot <- KA_plot_year(ce_KA_RC2_year, "Karkar: Electrical Assets", "mediumpurple3") +
  scale_y_continuous(breaks = c(14, 16, 18, 20, 22))
ce_KA_RC2_year_plot
ce_KA_RC3_year_plot <- KA_plot_year(ce_KA_RC3_year, "Karkar: Roofing Quality", "coral3") +
  scale_y_continuous(breaks = c(13, 15, 17, 19, 21))
ce_KA_RC3_year_plot

#--------------------
# EFFECT OF GENDER ON MSL

# Extract results for each effect 
KA_hyp_RC1_gender.16 <- extract_hypothesis(KA_RC1_gender, "genderm = 0", "RC1", "Gender (Male vs Female, 2016)", "Gender")
KA_hyp_RC1_gender.26 <- extract_hypothesis(KA_RC1_gender, "genderm + year2026:genderm = 0", "RC1", "Gender (Male vs Female, 2026)", "Gender")
KA_hyp_RC1_gender.int26v16 <- extract_hypothesis(KA_RC1_gender, "year2026:genderm = 0", "RC1", "Gender x Year (2026 vs 2016)", "Gender")

KA_hyp_RC2_gender.16 <- extract_hypothesis(KA_RC2_gender, "genderm = 0", "RC2", "Gender (Male vs Female, 2016)", "Gender")
KA_hyp_RC2_gender.26 <- extract_hypothesis(KA_RC2_gender, "genderm + year2026:genderm = 0", "RC2", "Gender (Male vs Female, 2026)", "Gender")
KA_hyp_RC2_gender.int26v16 <- extract_hypothesis(KA_RC2_gender, "year2026:genderm = 0", "RC2", "Gender x Year (2026 vs 2016)", "Gender")

KA_hyp_RC3_gender.16 <- extract_hypothesis(KA_RC3_gender, "genderm = 0", "RC3", "Gender (Male vs Female, 2016)", "Gender")
KA_hyp_RC3_gender.26 <- extract_hypothesis(KA_RC3_gender, "genderm + year2026:genderm = 0", "RC3", "Gender (Male vs Female, 2026)", "Gender")
KA_hyp_RC3_gender.int26v16 <- extract_hypothesis(KA_RC3_gender, "year2026:genderm = 0", "RC3", "Gender x Year (2026 vs 2016)", "Gender")

# Combine results
hyp_KA_gender <- bind_rows(KA_hyp_RC1_gender.16, KA_hyp_RC1_gender.26, KA_hyp_RC1_gender.int26v16,
                           KA_hyp_RC2_gender.16, KA_hyp_RC2_gender.26, KA_hyp_RC2_gender.int26v16,
                           KA_hyp_RC3_gender.16, KA_hyp_RC3_gender.26, KA_hyp_RC3_gender.int26v16) %>%
  select(component, effect, everything())

hyp_KA_gender_credible <- hyp_KA_gender %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
ce_KA_RC1_gender <- conditional_effects(KA_RC1_gender,
                                        effects = "year:gender",
                                        re_formula = NA,
                                        prob = 0.9)$'year:gender' %>%
  mutate(year = as.numeric(as.character(year)))

ce_KA_RC2_gender <- conditional_effects(KA_RC2_gender,
                                        effects = "year:gender",
                                        re_formula = NA,
                                        prob = 0.9)$'year:gender' %>%
  mutate(year = as.numeric(as.character(year)))

ce_KA_RC3_gender <- conditional_effects(KA_RC3_gender,
                                        effects = "year:gender",
                                        re_formula = NA,
                                        prob = 0.9)$'year:gender' %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_gender <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = gender, fill = gender)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c(f = "slateblue3", m = "darkorange2"),
                        labels = c(f = "Female", m = "Male")) +
    scale_fill_manual(values = c(f = "slateblue3", m = "darkorange2"),
                      labels = c(f = "Female", m = "Male")) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Gender", fill = "Gender") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_gender_plot <- KA_plot_gender(ce_KA_RC1_gender, "Karkar: Housing Quality")
ce_KA_RC1_gender_plot
ce_KA_RC2_gender_plot <- KA_plot_gender(ce_KA_RC2_gender, "Karkar: Electrical Assets")
ce_KA_RC2_gender_plot
ce_KA_RC3_gender_plot <- KA_plot_gender(ce_KA_RC3_gender, "Karkar: Roofing Quality")
ce_KA_RC3_gender_plot

#--------------------
# FORMULA FOR EXTRACTING REST OF HYPOTHESES

extract_KA <- function(model, #eg. KA_RC1_gender
                       var, #eg. genderm
                       component, #eg. RC1
                       predictor, #eg. Gender
                       comparison = NULL) { #eg. Male vs Female for categorical vars, or NULL for continuous vars (like age)
  bind_rows(extract_hypothesis(model = model, #Effect in 2016 (produce something like KA_hyp_RC1_gender.16)
                               hypothesis_string = paste0(var, " = 0"), #eg. genderm = 0
                               component = component,
                               effect = ifelse(is.null(comparison), paste0(predictor, " (2016)"), #continuous vars like age = eg. Age (2016)
                                               paste0(predictor, " (", comparison, ", 2016)")), #continuous vars like gender = eg. Gender (Male vs Female, 2016)
                               predictor = predictor),
            extract_hypothesis(model = model, #Effect in 2026 (produce something like KA_hyp_RC1_gender.26)
                               hypothesis_string = paste0(var, " + year2026:", var, " = 0"), #eg. genderm + year2026:genderm = 0
                               component = component,
                               effect = ifelse(is.null(comparison), paste0(predictor, " (2026)"), #continuous vars like age = eg. Age (2026)
                                               paste0(predictor, " (", comparison, ", 2026)")), #continuous vars like gender = eg. Gender (Male vs Female, 2026)
                               predictor = predictor),
            extract_hypothesis(model = model, #Interaction effect (produce something like KA_hyp_RC1_gender.int26v16)
                               hypothesis_string = paste0("year2026:", var, " = 0"), #eg. year2026:genderm = 0
                               component = component,
                               effect = paste0(predictor, " x Year (2026 vs 2016)"), #eg. Gender x Year (2026 vs 2016)
                               predictor = predictor))}

#--------------------
# EFFECT OF AGE ON MSL

# Extract results for each effect
hyp_KA_age_RC1 <- extract_KA(KA_RC1_age, "stdizeage.recorded", "RC1", "Age")
hyp_KA_age_RC2 <- extract_KA(KA_RC2_age, "stdizeage.recorded", "RC2", "Age")
hyp_KA_age_RC3 <- extract_KA(KA_RC3_age, "stdizeage.recorded", "RC3", "Age")

# Combine results
hyp_KA_age <- bind_rows(hyp_KA_age_RC1, hyp_KA_age_RC2, hyp_KA_age_RC3) %>%
  select(component, effect, everything())

hyp_KA_age_credible <- hyp_KA_age %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
KA_age_younger <- mean(data_KA$age.recorded, na.rm = TRUE) - sd(data_KA$age.recorded, na.rm = TRUE)
KA_age_older <- mean(data_KA$age.recorded, na.rm = TRUE) + sd(data_KA$age.recorded, na.rm = TRUE)

KA_age_younger
KA_age_older

ce_KA_RC1_age <- conditional_effects(KA_RC1_age,
                                     effects = "year:age.recorded",
                                     int_conditions = list(age.recorded = c(KA_age_younger, KA_age_older)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == KA_age_younger, "Younger", "Older"))

ce_KA_RC2_age <- conditional_effects(KA_RC2_age,
                                     effects = "year:age.recorded",
                                     int_conditions = list(age.recorded = c(KA_age_younger, KA_age_older)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == KA_age_younger, "Younger", "Older"))

ce_KA_RC3_age <- conditional_effects(KA_RC3_age,
                                     effects = "year:age.recorded",
                                     int_conditions = list(age.recorded = c(KA_age_younger, KA_age_older)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == KA_age_younger, "Younger", "Older"))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_age <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = age_group, fill = age_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Younger" = "slateblue3", "Older" = "darkorange2")) +
    scale_fill_manual(values = c("Younger" = "slateblue3", "Older" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Age", fill = "Age") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_age_plot <- KA_plot_age(ce_KA_RC1_age, "Karkar: Housing Quality") +
  scale_y_continuous(breaks = c(13, 15, 17, 19, 21))
ce_KA_RC1_age_plot
ce_KA_RC2_age_plot <- KA_plot_age(ce_KA_RC2_age, "Karkar: Electrical Assets")
ce_KA_RC2_age_plot
ce_KA_RC3_age_plot <- KA_plot_age(ce_KA_RC3_age, "Karkar: Roofing Quality")
ce_KA_RC3_age_plot

#--------------------
# EFFECT OF PRIMARY AGRICULTURE LIVELIHOOD ON MSL

# Extract results for each effect
hyp_KA_liv.pri_RC1 <- extract_KA(KA_RC1_liv.pri, "livelihoods.agriculture1", "RC1", "Primary Livelihood", "Agricultural vs Non-Agricultural")
hyp_KA_liv.pri_RC2 <- extract_KA(KA_RC2_liv.pri, "livelihoods.agriculture1", "RC2", "Primary Livelihood", "Agricultural vs Non-Agricultural")
hyp_KA_liv.pri_RC3 <- extract_KA(KA_RC3_liv.pri, "livelihoods.agriculture1", "RC3", "Primary Livelihood", "Agricultural vs Non-Agricultural")

# Combine results
hyp_KA_liv.pri <- bind_rows(hyp_KA_liv.pri_RC1, hyp_KA_liv.pri_RC2, hyp_KA_liv.pri_RC3) %>%
  select(component, effect, everything())

hyp_KA_liv.pri_credible <- hyp_KA_liv.pri %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
ce_KA_RC1_liv.pri <- conditional_effects(KA_RC1_liv.pri,
                                        effects = "year:livelihoods.agriculture",
                                        re_formula = NA,
                                        prob = 0.9)$'year:livelihoods.agriculture' %>%
  mutate(year = as.numeric(as.character(year)))

ce_KA_RC2_liv.pri <- conditional_effects(KA_RC2_liv.pri,
                                        effects = "year:livelihoods.agriculture",
                                        re_formula = NA,
                                        prob = 0.9)$'year:livelihoods.agriculture' %>%
  mutate(year = as.numeric(as.character(year)))

ce_KA_RC3_liv.pri <- conditional_effects(KA_RC3_liv.pri,
                                        effects = "year:livelihoods.agriculture",
                                        re_formula = NA,
                                        prob = 0.9)$'year:livelihoods.agriculture' %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_liv.pri <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = factor(livelihoods.agriculture), fill = factor(livelihoods.agriculture))) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("1" = "slateblue3", "0" = "darkorange2"),
                        labels = c("1" = "Agricultural", "0" = "Non-Agricultural")) +
    scale_fill_manual(values = c("1" = "slateblue3", "0" = "darkorange2"),
                      labels = c("1" = "Agricultural", "0" = "Non-Agricultural")) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Primary Livelihood", fill = "Primary Livelihood") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_liv.pri_plot <- KA_plot_liv.pri(ce_KA_RC1_liv.pri, "Karkar: Housing Quality") +
  scale_y_continuous(breaks = c(15, 17, 19, 21, 23, 25, 27))
ce_KA_RC1_liv.pri_plot
ce_KA_RC2_liv.pri_plot <- KA_plot_liv.pri(ce_KA_RC2_liv.pri, "Karkar: Electrical Assets") +
  scale_y_continuous(breaks = c(12, 14, 16, 18, 20, 22, 24))
ce_KA_RC2_liv.pri_plot
ce_KA_RC3_liv.pri_plot <- KA_plot_liv.pri(ce_KA_RC3_liv.pri, "Karkar: Roofing Quality") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28))
ce_KA_RC3_liv.pri_plot

#--------------------
# EFFECT OF LIVELIHOOD DIVERSITY ON MSL

# Extract results for each effect
hyp_KA_liv.div_RC1 <- extract_KA(KA_RC1_liv.div, "stdizelivelihoods.diversity", "RC1", "Livelihood Diversity")
hyp_KA_liv.div_RC2 <- extract_KA(KA_RC2_liv.div, "stdizelivelihoods.diversity", "RC2", "Livelihood Diversity")
hyp_KA_liv.div_RC3 <- extract_KA(KA_RC3_liv.div, "stdizelivelihoods.diversity", "RC3", "Livelihood Diversity")

# Combine results
hyp_KA_liv.div <- bind_rows(hyp_KA_liv.div_RC1, hyp_KA_liv.div_RC2, hyp_KA_liv.div_RC3) %>%
  select(component, effect, everything())

hyp_KA_liv.div_credible <- hyp_KA_liv.div %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
KA_liv.div_lower <- mean(data_KA$livelihoods.diversity, na.rm = TRUE) - sd(data_KA$livelihoods.diversity, na.rm = TRUE)
KA_liv.div_higher <- mean(data_KA$livelihoods.diversity, na.rm = TRUE) + sd(data_KA$livelihoods.diversity, na.rm = TRUE)
KA_liv.div_lower
KA_liv.div_higher

ce_KA_RC1_liv.div <- conditional_effects(KA_RC1_liv.div,
                                     effects = "year:livelihoods.diversity",
                                     int_conditions = list(livelihoods.diversity = c(KA_liv.div_lower, KA_liv.div_higher)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:livelihoods.diversity' %>%
  mutate(year = as.numeric(as.character(year)),
         live.div_group = ifelse(livelihoods.diversity == KA_liv.div_lower, "Lower", "Higher"))

ce_KA_RC2_liv.div <- conditional_effects(KA_RC2_liv.div,
                                     effects = "year:livelihoods.diversity",
                                     int_conditions = list(livelihoods.diversity = c(KA_liv.div_lower, KA_liv.div_higher)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:livelihoods.diversity' %>%
  mutate(year = as.numeric(as.character(year)),
         live.div_group = ifelse(livelihoods.diversity == KA_liv.div_lower, "Lower", "Higher"))

ce_KA_RC3_liv.div <- conditional_effects(KA_RC3_liv.div,
                                     effects = "year:livelihoods.diversity",
                                     int_conditions = list(livelihoods.diversity = c(KA_liv.div_lower, KA_liv.div_higher)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:livelihoods.diversity' %>%
  mutate(year = as.numeric(as.character(year)),
         live.div_group = ifelse(livelihoods.diversity == KA_liv.div_lower, "Lower", "Higher"))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_liv.div <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = live.div_group, fill = live.div_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_fill_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Livelihood Diversity", fill = "Livelihood Diversity") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_liv.div_plot <- KA_plot_liv.div(ce_KA_RC1_liv.div, "Karkar: Housing Quality") +
  scale_y_continuous(breaks = c(13, 15, 17, 19, 21, 23, 25, 27, 29))
ce_KA_RC1_liv.div_plot
ce_KA_RC2_liv.div_plot <- KA_plot_liv.div(ce_KA_RC2_liv.div, "Karkar: Electrical Assets") +
  scale_y_continuous(breaks = c(10, 12, 14, 16, 18, 20, 22, 24))
ce_KA_RC2_liv.div_plot
ce_KA_RC3_liv.div_plot <- KA_plot_liv.div(ce_KA_RC3_liv.div, "Karkar: Roofing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
ce_KA_RC3_liv.div_plot

#--------------------
# EFFECT OF DECISION MAKING ON MSL

# Extract results for each effect
hyp_KA_decision_RC1 <- extract_KA(KA_RC1_decision, "stdizesocial_capital.decision.index", "RC1", "Decision-Making")
hyp_KA_decision_RC2 <- extract_KA(KA_RC2_decision, "stdizesocial_capital.decision.index", "RC2", "Decision-Making")
hyp_KA_decision_RC3 <- extract_KA(KA_RC3_decision, "stdizesocial_capital.decision.index", "RC3", "Decision-Making")

# Combine results
hyp_KA_decision <- bind_rows(hyp_KA_decision_RC1, hyp_KA_decision_RC2, hyp_KA_decision_RC3) %>%
  select(component, effect, everything())

hyp_KA_decision_credible <- hyp_KA_decision %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
KA_decision_lower <- mean(data_KA$social_capital.decision.index, na.rm = TRUE) - sd(data_KA$social_capital.decision.index, na.rm = TRUE)
KA_decision_higher <- mean(data_KA$social_capital.decision.index, na.rm = TRUE) + sd(data_KA$social_capital.decision.index, na.rm = TRUE)
KA_decision_lower
KA_decision_higher

ce_KA_RC1_decision <- conditional_effects(KA_RC1_decision,
                                         effects = "year:social_capital.decision.index",
                                         int_conditions = list(social_capital.decision.index = c(KA_decision_lower, KA_decision_higher)),
                                         re_formula = NA,
                                         prob = 0.9)$'year:social_capital.decision.index' %>%
  mutate(year = as.numeric(as.character(year)),
         decision_group = ifelse(social_capital.decision.index == KA_decision_lower, "Lower", "Higher"))

ce_KA_RC2_decision <- conditional_effects(KA_RC2_decision,
                                         effects = "year:social_capital.decision.index",
                                         int_conditions = list(social_capital.decision.index = c(KA_decision_lower, KA_decision_higher)),
                                         re_formula = NA,
                                         prob = 0.9)$'year:social_capital.decision.index' %>%
  mutate(year = as.numeric(as.character(year)),
         decision_group = ifelse(social_capital.decision.index == KA_decision_lower, "Lower", "Higher"))

ce_KA_RC3_decision <- conditional_effects(KA_RC3_decision,
                                         effects = "year:social_capital.decision.index",
                                         int_conditions = list(social_capital.decision.index = c(KA_decision_lower, KA_decision_higher)),
                                         re_formula = NA,
                                         prob = 0.9)$'year:social_capital.decision.index' %>%
  mutate(year = as.numeric(as.character(year)),
         decision_group = ifelse(social_capital.decision.index == KA_decision_lower, "Lower", "Higher"))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_decision <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = decision_group, fill = decision_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_fill_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Decision-Making Involvement", fill = "Decision-Making Involvement") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_decision_plot <- KA_plot_decision(ce_KA_RC1_decision, "Karkar: Housing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC1_decision_plot
ce_KA_RC2_decision_plot <- KA_plot_decision(ce_KA_RC2_decision, "Karkar: Electrical Assets") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC2_decision_plot
ce_KA_RC3_decision_plot <- KA_plot_decision(ce_KA_RC3_decision, "Karkar: Roofing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC3_decision_plot

#--------------------
# EFFECT OF TRUST ON MSL

# Extract results for each effect
hyp_KA_trust_RC1 <- extract_KA(KA_RC1_trust, "stdizesocial_capital.trust.index", "RC1", "Trust")
hyp_KA_trust_RC2 <- extract_KA(KA_RC2_trust, "stdizesocial_capital.trust.index", "RC2", "Trust")
hyp_KA_trust_RC3 <- extract_KA(KA_RC3_trust, "stdizesocial_capital.trust.index", "RC3", "Trust")

# Combine results
hyp_KA_trust <- bind_rows(hyp_KA_trust_RC1, hyp_KA_trust_RC2, hyp_KA_trust_RC3) %>%
  select(component, effect, everything())

hyp_KA_trust_credible <- hyp_KA_trust %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
KA_trust_lower <- mean(data_KA$social_capital.trust.index, na.rm = TRUE) - sd(data_KA$social_capital.trust.index, na.rm = TRUE)
KA_trust_higher <- mean(data_KA$social_capital.trust.index, na.rm = TRUE) + sd(data_KA$social_capital.trust.index, na.rm = TRUE)

ce_KA_RC1_trust <- conditional_effects(KA_RC1_trust,
                                       effects = "year:social_capital.trust.index",
                                       int_conditions = list(social_capital.trust.index = c(KA_trust_lower, KA_trust_higher)),
                                       re_formula = NA,
                                       prob = 0.9)$'year:social_capital.trust.index' %>%
  mutate(year = as.numeric(as.character(year)),
         trust_group = ifelse(social_capital.trust.index == KA_trust_lower, "Lower", "Higher"))

ce_KA_RC2_trust <- conditional_effects(KA_RC2_trust,
                                       effects = "year:social_capital.trust.index",
                                       int_conditions = list(social_capital.trust.index = c(KA_trust_lower, KA_trust_higher)),
                                       re_formula = NA,
                                       prob = 0.9)$'year:social_capital.trust.index' %>%
  mutate(year = as.numeric(as.character(year)),
         trust_group = ifelse(social_capital.trust.index == KA_trust_lower, "Lower", "Higher"))

ce_KA_RC3_trust <- conditional_effects(KA_RC3_trust,
                                       effects = "year:social_capital.trust.index",
                                       int_conditions = list(social_capital.trust.index = c(KA_trust_lower, KA_trust_higher)),
                                       re_formula = NA,
                                       prob = 0.9)$'year:social_capital.trust.index' %>%
  mutate(year = as.numeric(as.character(year)),
         trust_group = ifelse(social_capital.trust.index == KA_trust_lower, "Lower", "Higher"))

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_trust <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = trust_group, fill = trust_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_fill_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_trust_plot <- KA_plot_trust(ce_KA_RC1_trust, "Karkar: Housing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC1_trust_plot
ce_KA_RC2_trust_plot <- KA_plot_trust(ce_KA_RC2_trust, "Karkar: Electrical Assets") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC2_trust_plot
ce_KA_RC3_trust_plot <- KA_plot_trust(ce_KA_RC3_trust, "Karkar: Roofing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC3_trust_plot

#--------------------
# EFFECT OF PERCEIVED CLIMATE IMPACTS ON MSL

# Extract results for each effect
hyp_RC1_ccimpact.26 <- extract_hypothesis(KA_RC1_ccimpact, "ccimpactworse = 0", "RC1", "Climate Change Impacts (2026)", "Climate Change Impacts")
hyp_RC2_ccimpact.26 <- extract_hypothesis(KA_RC2_ccimpact, "ccimpactworse = 0", "RC2", "Climate Change Impacts (2026)", "Climate Change Impacts")
hyp_RC3_ccimpact.26 <- extract_hypothesis(KA_RC3_ccimpact, "ccimpactworse = 0", "RC3", "Climate Change Impacts (2026)", "Climate Change Impacts")

# Combine results
hyp_KA_ccimpact <- bind_rows(hyp_RC1_ccimpact.26, hyp_RC2_ccimpact.26, hyp_RC3_ccimpact.26) %>%
  select(component, effect, everything())

hyp_KA_ccimpact_credible <- hyp_KA_ccimpact %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
ce_KA_RC1_ccimpact <- conditional_effects(KA_RC1_ccimpact,
                                         effects = "ccimpact",
                                         re_formula = NA,
                                         prob = 0.9)$'ccimpact'

ce_KA_RC2_ccimpact <- conditional_effects(KA_RC2_ccimpact,
                                          effects = "ccimpact",
                                          re_formula = NA,
                                          prob = 0.9)$'ccimpact'

ce_KA_RC3_ccimpact <- conditional_effects(KA_RC3_ccimpact,
                                          effects = "ccimpact",
                                          re_formula = NA,
                                          prob = 0.9)$'ccimpact'

# Plot conditional effects (combine with Ahus in "12-plots.R")
KA_plot_ccimpact <- function(data,title){
  ggplot(data, aes(ccimpact, estimate__, colour = ccimpact)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__),
                  width = 0.15, linewidth = 1) +
    geom_point(size = 3) +
    scale_x_discrete(labels = c("sameorbetter" = "Same or Better", "worse" = "Worse")) +
    scale_colour_manual(values = c("sameorbetter" = "slateblue3", "worse" = "darkorange2"),
                        labels = c("sameorbetter" = "Same or Better", "worse" = "Worse")) +
    labs(x = "Year: 2026",
         y = "MSL Category",
         title = title,
         colour = "Perceived Climate Impacts") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none",
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_KA_RC1_ccimpact_plot <- KA_plot_ccimpact(ce_KA_RC1_ccimpact, "Karkar: Housing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC1_ccimpact_plot
ce_KA_RC2_ccimpact_plot <- KA_plot_ccimpact(ce_KA_RC2_ccimpact, "Karkar: Electrical Assets") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC2_ccimpact_plot
ce_KA_RC3_ccimpact_plot <- KA_plot_ccimpact(ce_KA_RC3_ccimpact, "Karkar: Roofing Quality") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))
ce_KA_RC3_ccimpact_plot

#--------------------
# RESULTS: ALL SOCIAL PREDICTORS

# Combine results from all social predictors
hyp_KA_all <- bind_rows(hyp_KA_gender,
                        hyp_KA_age,
                        hyp_KA_liv.pri,
                        hyp_KA_liv.div,
                        hyp_KA_decision,
                        hyp_KA_trust,
                        hyp_KA_ccimpact) 
write_xlsx(hyp_KA_all, "Plots/Karkar_All_Results.xlsx")

hyp_KA_all_credible <- hyp_KA_all %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)
write_xlsx(hyp_KA_all_credible, "Plots/Karkar_All_Results_credible.xlsx")

# Clean results for plotting
hyp_KA_all_clean <- hyp_KA_all_credible %>%
  mutate(component = factor(component, levels = c("RC1", "RC2", "RC3"),
                            labels = c("Housing Quality",
                                       "Electrical Assets",
                                       "Roofing Quality")),
         effect_order = row_number(),
         effect_order = factor(effect_order,
                               levels = rev(unique(effect_order))))

# Convert from OR to log-odds for whisker plots
hyp_KA_all_whisker <- hyp_KA_all_clean %>%
  mutate(across(c(Estimate, CI.Lower, CI.Upper, CI.Lower50, CI.Upper50), log)) 

# Whisker plots (combine with Ahus in "12-plots.R")
KA_whiskerplot_all <- ggplot(hyp_KA_all_whisker, #use log-odds data for whisker plots
                              aes(x = Estimate, y = effect_order, colour = predictor)) +
  geom_vline(xintercept = 0, #add reference line at 0 (no effect)
             linetype = "dashed",
             colour = "grey65") +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper), #90% CrIs
                 width = 0, linewidth = 0.8) +
  geom_errorbarh(aes(xmin = CI.Lower50, xmax = CI.Upper50), #50% CrIs
                 width = 0, linewidth = 2) +
  geom_point(size = 3) + #estimate point
  facet_wrap(vars(component), ncol = 1, #make a panel for each component (RC1/RC2/RC3) all in one column
             strip.position = "left", #labels for each component on left of panels
             scales = "free_y", #y-axis just contains effects for component in each panel
             space = "free_y") + #adjust height of each panel depending on number of effects plotted
  geom_text(aes(x = Estimate, label = effect), #add effect labels above each estimate
            vjust = -0.7, size = 3.5) +
  scale_y_discrete(labels = setNames(hyp_KA_all_whisker$effect,
                                     hyp_KA_all_whisker$effect_order),
                   expand = expansion(add = 1.5)) + #adjust spacing between each effect
  scale_colour_manual(values = c("Gender" = "firebrick3",
                                 "Age" = "darkorange3",
                                 "Primary Livelihood" = "forestgreen",
                                 "Livelihood Diversity" = "darkcyan",
                                 "Decision-Making" = "steelblue3",
                                 "Trust" = "mediumpurple3",
                                 "Climate Change Impacts" = "deeppink4")) +
  labs(x = "Cumulative Log-Odds", 
       y = "MSL Index",
       title = "Karkar") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10, colour = "black"), #text formatting
        axis.text.x = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 10, colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none", #remove legend
        strip.placement = "outside", #place component labels outside panels (next to y axis)
        strip.background = element_blank(), #panel formatting
        panel.spacing.y = unit(0, "cm"),
        panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4))
KA_whiskerplot_all

#--------------------
# DESCRIPTIVE CHANGES IN MSL

# Percent of households who increase/decrease MSL scores between 2016 and 2026  
descriptive_changes_KA <- data_KA %>%
  select(SharingUnitID_NEW, year, RC1, RC2, RC3) %>%
  pivot_wider(names_from = year, values_from = c(RC1, RC2, RC3)) %>%
  mutate(change_RC1_16to26 = RC1_2026 - RC1_2016,
         change_RC2_16to26 = RC2_2026 - RC2_2016,
         change_RC3_16to26 = RC3_2026 - RC3_2016) %>%
  pivot_longer(starts_with("change_"), names_to = "period", values_to = "change") %>%
  filter(!is.na(change)) %>%
  group_by(period) %>%
  summarise(panel_n = n(),
            increase = (sum(change > 0)/panel_n)*100,
            decrease = (sum(change < 0)/panel_n)*100,
            same = (sum(change == 0)/panel_n)*100)
descriptive_changes_KA

# Change in percentile rank  
data_KA <- data_KA %>%
  group_by(year) %>%
  mutate(RC1_percentile = percent_rank(RC1)*100,
         RC2_percentile = percent_rank(RC2)*100,
         RC3_percentile = percent_rank(RC3)*100) %>%
  ungroup()

descriptive_percentiles_KA <- data_KA %>%
  select(SharingUnitID_NEW, year, RC1_percentile, RC2_percentile, RC3_percentile) %>%
  pivot_wider(names_from = year,
              values_from = c(RC1_percentile, RC2_percentile, RC3_percentile)) %>%
  mutate(change_RC1_16to26 = RC1_percentile_2026 - RC1_percentile_2016,
         change_RC2_16to26 = RC2_percentile_2026 - RC2_percentile_2016,
         change_RC3_16to26 = RC3_percentile_2026 - RC3_percentile_2016) %>%
  pivot_longer(starts_with("change_"), names_to = "period", values_to = "change") %>%
  filter(!is.na(change)) %>%
  group_by(period) %>%
  summarise(panel_n = n(),
            mean_change = mean(change),
            median_change = median(change),
            min_change = min(change),
            max_change = max(change),
            IQR_change = IQR(change))
descriptive_percentiles_KA

#--------------------
# SAVE OUTPUTS
save.image("11-results_KA.RData")

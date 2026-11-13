# load("9-results_AH.RData")

#--------------------
# OPEN PACKAGES
library(brms)
library(dplyr)
library(ggplot2)
library(writexl) 

#--------------------
# LOAD MODEL RESULTS
load("8-models_AH.RData")

#--------------------
# HYPOTHESIS FUNCTION
extract_hypothesis <- function(model, #fitted brms model (e.g., AH_RC1_year)
                               hypothesis_string, #hypothesis to evaluate, what would normally be put into hypothesis() function (e.g., "year2018 = 0")
                               component, #MSL component label (e.g., "RC1")
                               effect, #effect label (e.g., "Year (2018 vs 2016)")
                               predictor) { #predictor lavel (e.g., "Year")
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
AH_hyp_RC1_year.18v16 <- extract_hypothesis(AH_RC1_year, "year2018 = 0", "RC1", "Year (2018 vs 2016)", "Year")
AH_hyp_RC1_year.26v18 <- extract_hypothesis(AH_RC1_year, "year2026 - year2018 = 0", "RC1", "Year (2026 vs 2018)", "Year")
AH_hyp_RC1_year.26v16 <- extract_hypothesis(AH_RC1_year, "year2026 = 0", "RC1", "Year (2026 vs 2016)", "Year")

AH_hyp_RC2_year.18v16 <- extract_hypothesis(AH_RC2_year, "year2018 = 0", "RC2", "Year (2018 vs 2016)", "Year")
AH_hyp_RC2_year.26v18 <- extract_hypothesis(AH_RC2_year, "year2026 - year2018 = 0", "RC2", "Year (2026 vs 2018)", "Year")
AH_hyp_RC2_year.26v16 <- extract_hypothesis(AH_RC2_year, "year2026 = 0", "RC2", "Year (2026 vs 2016)", "Year")

AH_hyp_RC3_year.18v16 <- extract_hypothesis(AH_RC3_year, "year2018 = 0", "RC3", "Year (2018 vs 2016)", "Year")
AH_hyp_RC3_year.26v18 <- extract_hypothesis(AH_RC3_year, "year2026 - year2018 = 0", "RC3", "Year (2026 vs 2018)", "Year")
AH_hyp_RC3_year.26v16 <- extract_hypothesis(AH_RC3_year, "year2026 = 0", "RC3", "Year (2026 vs 2016)", "Year")

# Combine results and save
hyp_AH_year <- bind_rows(AH_hyp_RC1_year.18v16, AH_hyp_RC1_year.26v18, AH_hyp_RC1_year.26v16,
                         AH_hyp_RC2_year.18v16, AH_hyp_RC2_year.26v18, AH_hyp_RC2_year.26v16,
                         AH_hyp_RC3_year.18v16, AH_hyp_RC3_year.26v18, AH_hyp_RC3_year.26v16) %>%
  select(component, effect, everything()) #make component col first and effect col second
write_xlsx(hyp_AH_year, "Plots/results_AH_year.xlsx")

# Clean results for plotting
hyp_AH_year_clean <- hyp_AH_year %>%
  mutate(component = factor(component, levels = c("RC1", "RC2", "RC3"), #Rename RC1, RC2 and RC3
                            labels = c("Housing Quality",
                                       "Electrical Assets",
                                       "Roofing Quality")),
         effect_order = row_number(), #give each row in hyp_AH_year a number
         effect_order = factor(effect_order, #reverse factor order so first effect appears first
                               levels = rev(unique(effect_order))))

# Convert from OR to log-odds for whisker plots
hyp_AH_year_whisker <- hyp_AH_year_clean %>%
  mutate(across(c(Estimate, CI.Lower, CI.Upper, CI.Lower50, CI.Upper50), log)) 

# Whisker plots (combine with Karkar in "12-plots.R")
AH_whiskerplot_year <- ggplot(hyp_AH_year_whisker, #use log-odds data for whisker plots
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
       title = "Ahus") +
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
AH_whiskerplot_year

# Extract conditional effects 
ce_AH_RC1_year <- conditional_effects(AH_RC1_year,
                                      effects = "year",
                                      re_formula = NA,
                                      prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC2_year <- conditional_effects(AH_RC2_year,
                                      effects = "year",
                                      re_formula = NA,
                                      prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC3_year <- conditional_effects(AH_RC3_year,
                                      effects = "year",
                                      re_formula = NA,
                                      prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_year <- function(data,title,colour){
  ggplot(data, aes(year, estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, fill = colour) +
    geom_line(linewidth = 1.4, colour = colour) +
    geom_point(size = 2.5, colour = colour) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_AH_RC1_year_plot <- AH_plot_year(ce_AH_RC1_year, "Ahus: Housing Quality", "darkcyan")
ce_AH_RC1_year_plot
ce_AH_RC2_year_plot <- AH_plot_year(ce_AH_RC2_year, "Ahus: Electrical Assets", "mediumpurple3")
ce_AH_RC2_year_plot
ce_AH_RC3_year_plot <- AH_plot_year(ce_AH_RC3_year, "Ahus: Roofing Quality", "coral3")
ce_AH_RC3_year_plot

#--------------------
# EFFECT OF GENDER ON MSL

# Extract results for each effect 
AH_hyp_RC1_gender.16 <- extract_hypothesis(AH_RC1_gender, "genderm = 0", "RC1", "Gender (Male vs Female, 2016)", "Gender")
AH_hyp_RC1_gender.18 <- extract_hypothesis(AH_RC1_gender, "genderm + year2018:genderm = 0", "RC1", "Gender (Male vs Female, 2018)", "Gender")
AH_hyp_RC1_gender.26 <- extract_hypothesis(AH_RC1_gender, "genderm + year2026:genderm = 0", "RC1", "Gender (Male vs Female, 2026)", "Gender")
AH_hyp_RC1_gender.int18v16 <- extract_hypothesis(AH_RC1_gender, "year2018:genderm = 0", "RC1", "Gender x Year (2018 vs 2016)", "Gender")
AH_hyp_RC1_gender.int26v18 <- extract_hypothesis(AH_RC1_gender, "year2026:genderm - year2018:genderm = 0", "RC1", "Gender x Year (2026 vs 2018)", "Gender")
AH_hyp_RC1_gender.int26v16 <- extract_hypothesis(AH_RC1_gender, "year2026:genderm = 0", "RC1", "Gender x Year (2026 vs 2016)", "Gender")

AH_hyp_RC2_gender.16 <- extract_hypothesis(AH_RC2_gender, "genderm = 0", "RC2", "Gender (Male vs Female, 2016)", "Gender")
AH_hyp_RC2_gender.18 <- extract_hypothesis(AH_RC2_gender, "genderm + year2018:genderm = 0", "RC2", "Gender (Male vs Female, 2018)", "Gender")
AH_hyp_RC2_gender.26 <- extract_hypothesis(AH_RC2_gender, "genderm + year2026:genderm = 0", "RC2", "Gender (Male vs Female, 2026)", "Gender")
AH_hyp_RC2_gender.int18v16 <- extract_hypothesis(AH_RC2_gender, "year2018:genderm = 0", "RC2", "Gender x Year (2018 vs 2016)", "Gender")
AH_hyp_RC2_gender.int26v18 <- extract_hypothesis(AH_RC2_gender, "year2026:genderm - year2018:genderm = 0", "RC2", "Gender x Year (2026 vs 2018)", "Gender")
AH_hyp_RC2_gender.int26v16 <- extract_hypothesis(AH_RC2_gender, "year2026:genderm = 0", "RC2", "Gender x Year (2026 vs 2016)", "Gender")

AH_hyp_RC3_gender.16 <- extract_hypothesis(AH_RC3_gender, "genderm = 0", "RC3", "Gender (Male vs Female, 2016)", "Gender")
AH_hyp_RC3_gender.18 <- extract_hypothesis(AH_RC3_gender, "genderm + year2018:genderm = 0", "RC3", "Gender (Male vs Female, 2018)", "Gender")
AH_hyp_RC3_gender.26 <- extract_hypothesis(AH_RC3_gender, "genderm + year2026:genderm = 0", "RC3", "Gender (Male vs Female, 2026)", "Gender")
AH_hyp_RC3_gender.int18v16 <- extract_hypothesis(AH_RC3_gender, "year2018:genderm = 0", "RC3", "Gender x Year (2018 vs 2016)", "Gender")
AH_hyp_RC3_gender.int26v18 <- extract_hypothesis(AH_RC3_gender, "year2026:genderm - year2018:genderm = 0", "RC3", "Gender x Year (2026 vs 2018)", "Gender")
AH_hyp_RC3_gender.int26v16 <- extract_hypothesis(AH_RC3_gender, "year2026:genderm = 0", "RC3", "Gender x Year (2026 vs 2016)", "Gender")

# Combine results
hyp_AH_gender <- bind_rows(AH_hyp_RC1_gender.16, AH_hyp_RC1_gender.18, AH_hyp_RC1_gender.26,
                           AH_hyp_RC1_gender.int18v16, AH_hyp_RC1_gender.int26v18, AH_hyp_RC1_gender.int26v16,
                           AH_hyp_RC2_gender.16, AH_hyp_RC2_gender.18, AH_hyp_RC2_gender.26,
                           AH_hyp_RC2_gender.int18v16, AH_hyp_RC2_gender.int26v18, AH_hyp_RC2_gender.int26v16,
                           AH_hyp_RC3_gender.16, AH_hyp_RC3_gender.18, AH_hyp_RC3_gender.26,
                           AH_hyp_RC3_gender.int18v16, AH_hyp_RC3_gender.int26v18, AH_hyp_RC3_gender.int26v16) %>%
  select(component, effect, everything())

hyp_AH_gender_credible <- hyp_AH_gender %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
ce_AH_RC1_gender <- conditional_effects(AH_RC1_gender,
                                        effects = "year:gender",
                                        re_formula = NA,
                                        prob = 0.9)$'year:gender' %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC2_gender <- conditional_effects(AH_RC2_gender,
                                        effects = "year:gender",
                                        re_formula = NA,
                                        prob = 0.9)$'year:gender' %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC3_gender <- conditional_effects(AH_RC3_gender,
                                        effects = "year:gender",
                                        re_formula = NA,
                                        prob = 0.9)$'year:gender' %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_gender <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = gender, fill = gender)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c(f = "slateblue3", m = "darkorange2"),
                        labels = c(f = "Female", m = "Male")) +
    scale_fill_manual(values = c(f = "slateblue3", m = "darkorange2"),
                      labels = c(f = "Female", m = "Male")) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
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

ce_AH_RC1_gender_plot <- AH_plot_gender(ce_AH_RC1_gender, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_gender_plot
ce_AH_RC2_gender_plot <- AH_plot_gender(ce_AH_RC2_gender, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_gender_plot
ce_AH_RC3_gender_plot <- AH_plot_gender(ce_AH_RC3_gender, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_gender_plot

#--------------------
# EFFECT OF AGE ON MSL

# Extract results for each effect 
AH_hyp_RC1_age.16 <- extract_hypothesis(AH_RC1_age, "stdizeage.recorded = 0", "RC1", "Age (2016)", "Age")
AH_hyp_RC1_age.18 <- extract_hypothesis(AH_RC1_age, "stdizeage.recorded + year2018:stdizeage.recorded = 0", "RC1", "Age (2018)", "Age")
AH_hyp_RC1_age.26 <- extract_hypothesis(AH_RC1_age, "stdizeage.recorded + year2026:stdizeage.recorded = 0", "RC1", "Age (2026)", "Age")
AH_hyp_RC1_age.int18v16 <- extract_hypothesis(AH_RC1_age, "year2018:stdizeage.recorded = 0", "RC1", "Age x Year (2018 vs 2016)", "Age")
AH_hyp_RC1_age.int26v18 <- extract_hypothesis(AH_RC1_age, "year2026:stdizeage.recorded - year2018:stdizeage.recorded = 0", "RC1", "Age x Year (2026 vs 2018)", "Age")
AH_hyp_RC1_age.int26v16 <- extract_hypothesis(AH_RC1_age, "year2026:stdizeage.recorded = 0", "RC1", "Age x Year (2026 vs 2016)", "Age")

AH_hyp_RC2_age.16 <- extract_hypothesis(AH_RC2_age, "stdizeage.recorded = 0", "RC2", "Age (2016)", "Age")
AH_hyp_RC2_age.18 <- extract_hypothesis(AH_RC2_age, "stdizeage.recorded + year2018:stdizeage.recorded = 0", "RC2", "Age (2018)", "Age")
AH_hyp_RC2_age.26 <- extract_hypothesis(AH_RC2_age, "stdizeage.recorded + year2026:stdizeage.recorded = 0", "RC2", "Age (2026)", "Age")
AH_hyp_RC2_age.int18v16 <- extract_hypothesis(AH_RC2_age, "year2018:stdizeage.recorded = 0", "RC2", "Age x Year (2018 vs 2016)", "Age")
AH_hyp_RC2_age.int26v18 <- extract_hypothesis(AH_RC2_age, "year2026:stdizeage.recorded - year2018:stdizeage.recorded = 0", "RC2", "Age x Year (2026 vs 2018)", "Age")
AH_hyp_RC2_age.int26v16 <- extract_hypothesis(AH_RC2_age, "year2026:stdizeage.recorded = 0", "RC2", "Age x Year (2026 vs 2016)", "Age")

AH_hyp_RC3_age.16 <- extract_hypothesis(AH_RC3_age, "stdizeage.recorded = 0", "RC3", "Age (2016)", "Age")
AH_hyp_RC3_age.18 <- extract_hypothesis(AH_RC3_age, "stdizeage.recorded + year2018:stdizeage.recorded = 0", "RC3", "Age (2018)", "Age")
AH_hyp_RC3_age.26 <- extract_hypothesis(AH_RC3_age, "stdizeage.recorded + year2026:stdizeage.recorded = 0", "RC3", "Age (2026)", "Age")
AH_hyp_RC3_age.int18v16 <- extract_hypothesis(AH_RC3_age, "year2018:stdizeage.recorded = 0", "RC3", "Age x Year (2018 vs 2016)", "Age")
AH_hyp_RC3_age.int26v18 <- extract_hypothesis(AH_RC3_age, "year2026:stdizeage.recorded - year2018:stdizeage.recorded = 0", "RC3", "Age x Year (2026 vs 2018)", "Age")
AH_hyp_RC3_age.int26v16 <- extract_hypothesis(AH_RC3_age, "year2026:stdizeage.recorded = 0", "RC3", "Age x Year (2026 vs 2016)", "Age")

# Combine results
hyp_AH_age <- bind_rows(AH_hyp_RC1_age.16, AH_hyp_RC1_age.18, AH_hyp_RC1_age.26,
                        AH_hyp_RC1_age.int18v16, AH_hyp_RC1_age.int26v18, AH_hyp_RC1_age.int26v16,
                        AH_hyp_RC2_age.16, AH_hyp_RC2_age.18, AH_hyp_RC2_age.26,
                        AH_hyp_RC2_age.int18v16, AH_hyp_RC2_age.int26v18, AH_hyp_RC2_age.int26v16,
                        AH_hyp_RC3_age.16, AH_hyp_RC3_age.18, AH_hyp_RC3_age.26,
                        AH_hyp_RC3_age.int18v16, AH_hyp_RC3_age.int26v18, AH_hyp_RC3_age.int26v16) %>%
  select(component, effect, everything()) 

hyp_AH_age_credible <- hyp_AH_age %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
AH_age_younger <- mean(data_AH$age.recorded, na.rm = TRUE) - sd(data_AH$age.recorded, na.rm = TRUE)
AH_age_older <- mean(data_AH$age.recorded, na.rm = TRUE) + sd(data_AH$age.recorded, na.rm = TRUE)
AH_age_younger
AH_age_older

ce_AH_RC1_age <- conditional_effects(AH_RC1_age,
                                     effects = "year:age.recorded",
                                     int_conditions = list(age.recorded = c(AH_age_younger, AH_age_older)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == AH_age_younger, "Younger", "Older"))

ce_AH_RC2_age <- conditional_effects(AH_RC2_age,
                                     effects = "year:age.recorded",
                                     int_conditions = list(age.recorded = c(AH_age_younger, AH_age_older)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == AH_age_younger, "Younger", "Older"))

ce_AH_RC3_age <- conditional_effects(AH_RC3_age,
                                     effects = "year:age.recorded",
                                     int_conditions = list(age.recorded = c(AH_age_younger, AH_age_older)),
                                     re_formula = NA,
                                     prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == AH_age_younger, "Younger", "Older"))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_age <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = age_group, fill = age_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Younger" = "slateblue3", "Older" = "darkorange2")) +
    scale_fill_manual(values = c("Younger" = "slateblue3", "Older" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
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

ce_AH_RC1_age_plot <- AH_plot_age(ce_AH_RC1_age, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_age_plot
ce_AH_RC2_age_plot <- AH_plot_age(ce_AH_RC2_age, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_age_plot
ce_AH_RC3_age_plot <- AH_plot_age(ce_AH_RC3_age, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_age_plot

#--------------------
# FORMULA FOR EXTRACTING REST OF HYPOTHESES

extract_AH <- function(model, #eg. AH_RC1_gender
                       var, #eg. genderm
                       component, #eg. RC1
                       predictor, #eg. Gender
                       comparison = NULL) { #eg. Male vs Female for categorical vars, or NULL for continuous vars (like age)
  bind_rows(extract_hypothesis(model = model, #Main effect in 2016 (produce something like AH_hyp_RC1_gender.16)
                               hypothesis_string = paste0(var, " = 0"), #eg. genderm = 0
                               component = component,
                               effect = ifelse(is.null(comparison), paste0(predictor, " (2016)"), #continuous vars like age = eg. Age (2016)
                                               paste0(predictor, " (", comparison, ", 2016)")), #categorical vars like gender = eg. Gender (Male vs Female, 2016)
                               predictor = predictor),
            extract_hypothesis(model = model, #Main effect in 2018 (produce something like AH_hyp_RC1_gender.18)
                               hypothesis_string = paste0(var, " + year2018:", var, " = 0"), #eg. genderm + year2018:genderm = 0
                               component = component,
                               effect = ifelse(is.null(comparison), paste0(predictor, " (2018)"), #continuous vars like age = eg. Age (2018)
                                               paste0(predictor, " (", comparison, ", 2018)")), #categorical vars like gender = eg. Gender (Male vs Female, 2018)
                               predictor = predictor),
            extract_hypothesis(model = model, #Main effect in 2026 (produce something like AH_hyp_RC1_gender.26)
                               hypothesis_string = paste0(var, " + year2026:", var, " = 0"), #eg. genderm + year2026:genderm = 0
                               component = component,
                               effect = ifelse(is.null(comparison), paste0(predictor, " (2026)"), #continuous vars like age = eg. Age (2026)
                                               paste0(predictor, " (", comparison, ", 2026)")), #categorical vars like gender = eg. Gender (Male vs Female, 2026)
                               predictor = predictor),
            extract_hypothesis(model = model, #Interaction effect: 2018 vs 2016 (produce something like AH_hyp_RC1_gender.int18v16)
                               hypothesis_string = paste0("year2018:", var, " = 0"), #eg. year2018:genderm = 0
                               component = component,
                               effect = paste0(predictor, " x Year (2018 vs 2016)"), #eg. Gender x Year (2018 vs 2016)
                               predictor = predictor),
            extract_hypothesis(model = model, #Interaction effect: 2026 vs 2018 (produce something like AH_hyp_RC1_gender.int18v16)
                               hypothesis_string = paste0("year2026:", var, " - year2018:", var, " = 0"), #eg. year2026:genderm - year2018:genderm = 0
                               component = component,
                               effect = paste0(predictor, " x Year (2026 vs 2018)"), #eg. Gender x Year (2026 vs 2018)
                               predictor = predictor),
            extract_hypothesis(model = model, #Interaction effect: 2026 vs 2016 (produce something like AH_hyp_RC1_gender.int26v16)
                               hypothesis_string = paste0("year2026:", var, " = 0"), #eg. year2026:genderm = 0
                               component = component,
                               effect = paste0(predictor, " x Year (2026 vs 2016)"), #eg. Gender x Year (2026 vs 2016)
                               predictor = predictor))}

#--------------------
# EFFECT OF PRIMARY MARINE LIVELIHOOD ON MSL

# Extract results for each effect 
hyp_AH_liv.pri_RC1 <- extract_AH(AH_RC1_liv.pri, "livelihoods.primary.marine1", "RC1", "Primary Livelihood", "Marine vs Non-Marine")
hyp_AH_liv.pri_RC2 <- extract_AH(AH_RC2_liv.pri, "livelihoods.primary.marine1", "RC2", "Primary Livelihood", "Marine vs Non-Marine")
hyp_AH_liv.pri_RC3 <- extract_AH(AH_RC3_liv.pri, "livelihoods.primary.marine1", "RC3", "Primary Livelihood", "Marine vs Non-Marine")

# Combine results
hyp_AH_liv.pri <- bind_rows(hyp_AH_liv.pri_RC1, hyp_AH_liv.pri_RC2, hyp_AH_liv.pri_RC3) %>%
  select(component, effect, everything())

hyp_AH_liv.pri_credible <- hyp_AH_liv.pri %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
ce_AH_RC1_liv.pri <- conditional_effects(AH_RC1_liv.pri,
                                         effects = "year:livelihoods.primary.marine",
                                         re_formula = NA,
                                         prob = 0.9)$'year:livelihoods.primary.marine' %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC2_liv.pri <- conditional_effects(AH_RC2_liv.pri,
                                         effects = "year:livelihoods.primary.marine",
                                         re_formula = NA,
                                         prob = 0.9)$'year:livelihoods.primary.marine' %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC3_liv.pri <- conditional_effects(AH_RC3_liv.pri,
                                         effects = "year:livelihoods.primary.marine",
                                         re_formula = NA,
                                         prob = 0.9)$'year:livelihoods.primary.marine' %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_liv.pri <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = factor(livelihoods.primary.marine), fill = factor(livelihoods.primary.marine))) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("1" = "slateblue3", "0" = "darkorange2"),
                        labels = c("1" = "Marine", "0" = "Non-Marine")) +
    scale_fill_manual(values = c("1" = "slateblue3", "0" = "darkorange2"),
                      labels = c("1" = "Marine", "0" = "Non-Marine")) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
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

ce_AH_RC1_liv.pri_plot <- AH_plot_liv.pri(ce_AH_RC1_liv.pri, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_liv.pri_plot
ce_AH_RC2_liv.pri_plot <- AH_plot_liv.pri(ce_AH_RC2_liv.pri, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_liv.pri_plot
ce_AH_RC3_liv.pri_plot <- AH_plot_liv.pri(ce_AH_RC3_liv.pri, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_liv.pri_plot

#--------------------
# EFFECT OF LIVELIHOOD DIVERSITY ON MSL

# Extract results for each effect 
hyp_AH_liv.div_RC1 <- extract_AH(AH_RC1_liv.div, "stdizelivelihoods.diversity", "RC1", "Livelihood Diversity")
hyp_AH_liv.div_RC2 <- extract_AH(AH_RC2_liv.div, "stdizelivelihoods.diversity", "RC2", "Livelihood Diversity")
hyp_AH_liv.div_RC3 <- extract_AH(AH_RC3_liv.div, "stdizelivelihoods.diversity", "RC3", "Livelihood Diversity")

# Combine results
hyp_AH_liv.div <- bind_rows(hyp_AH_liv.div_RC1, hyp_AH_liv.div_RC2, hyp_AH_liv.div_RC3) %>%
  select(component, effect, everything())

hyp_AH_liv.div_credible <- hyp_AH_liv.div %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
AH_liv.div_lower <- mean(AH_RC1_liv.div$data$stdizelivelihoods.diversity, na.rm = TRUE) - sd(AH_RC1_liv.div$data$stdizelivelihoods.diversity, na.rm = TRUE)
AH_liv.div_higher <- mean(AH_RC1_liv.div$data$stdizelivelihoods.diversity, na.rm = TRUE) + sd(AH_RC1_liv.div$data$stdizelivelihoods.diversity, na.rm = TRUE)

AH_liv.div_lower_nonstdized <- mean(data_AH$livelihoods.diversity, na.rm = TRUE) - sd(data_AH$livelihoods.diversity, na.rm = TRUE)
AH_liv.div_higher_nonstdized <- mean(data_AH$livelihoods.diversity, na.rm = TRUE) + sd(data_AH$livelihoods.diversity, na.rm = TRUE)
AH_liv.div_lower_nonstdized
AH_liv.div_higher_nonstdized

ce_AH_RC1_liv.div <- conditional_effects(AH_RC1_liv.div,
                                         effects = "year:stdizelivelihoods.diversity",
                                         int_conditions = list(stdizelivelihoods.diversity = c(AH_liv.div_lower, AH_liv.div_higher)),
                                         re_formula = NA,
                                         prob = 0.9)$'year:stdizelivelihoods.diversity' %>%
  mutate(year = as.numeric(as.character(year)),
         live.div_group = ifelse(stdizelivelihoods.diversity == AH_liv.div_lower, "Lower", "Higher"))

ce_AH_RC2_liv.div <- conditional_effects(AH_RC2_liv.div,
                                         effects = "year:stdizelivelihoods.diversity",
                                         int_conditions = list(stdizelivelihoods.diversity = c(AH_liv.div_lower, AH_liv.div_higher)),
                                         re_formula = NA,
                                         prob = 0.9)$'year:stdizelivelihoods.diversity' %>%
  mutate(year = as.numeric(as.character(year)),
         live.div_group = ifelse(stdizelivelihoods.diversity == AH_liv.div_lower, "Lower", "Higher"))

ce_AH_RC3_liv.div <- conditional_effects(AH_RC3_liv.div,
                                         effects = "year:stdizelivelihoods.diversity",
                                         int_conditions = list(stdizelivelihoods.diversity = c(AH_liv.div_lower, AH_liv.div_higher)),
                                         re_formula = NA,
                                         prob = 0.9)$'year:stdizelivelihoods.diversity' %>%
  mutate(year = as.numeric(as.character(year)),
         live.div_group = ifelse(stdizelivelihoods.diversity == AH_liv.div_lower, "Lower", "Higher"))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_liv.div <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = live.div_group, fill = live.div_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_fill_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
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

ce_AH_RC1_liv.div_plot <- AH_plot_liv.div(ce_AH_RC1_liv.div, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_liv.div_plot
ce_AH_RC2_liv.div_plot <- AH_plot_liv.div(ce_AH_RC2_liv.div, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_liv.div_plot
ce_AH_RC3_liv.div_plot <- AH_plot_liv.div(ce_AH_RC3_liv.div, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_liv.div_plot

#--------------------
# EFFECT OF DECISION MAKING ON MSL

# Extract results for each effect
hyp_AH_decision_RC1 <- extract_AH(AH_RC1_decision, "stdizesocial_capital.decision.index", "RC1", "Decision-Making")
hyp_AH_decision_RC2 <- extract_AH(AH_RC2_decision, "stdizesocial_capital.decision.index", "RC2", "Decision-Making")
hyp_AH_decision_RC3 <- extract_AH(AH_RC3_decision, "stdizesocial_capital.decision.index", "RC3", "Decision-Making")

# Combine results
hyp_AH_decision <- bind_rows(hyp_AH_decision_RC1, hyp_AH_decision_RC2, hyp_AH_decision_RC3) %>%
  select(component, effect, everything())

hyp_AH_decision_credible <- hyp_AH_decision %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
AH_decision_lower <- mean(AH_RC1_decision$data$stdizesocial_capital.decision.index, na.rm = TRUE) - sd(AH_RC1_decision$data$stdizesocial_capital.decision.index, na.rm = TRUE)
AH_decision_higher <- mean(AH_RC1_decision$data$stdizesocial_capital.decision.index, na.rm = TRUE) + sd(AH_RC1_decision$data$stdizesocial_capital.decision.index, na.rm = TRUE)

AH_decision_lower_nonstdized <- mean(data_AH$social_capital.decision.index, na.rm = TRUE) - sd(data_AH$social_capital.decision.index, na.rm = TRUE)
AH_decision_higher_nonstdized <- mean(data_AH$social_capital.decision.index, na.rm = TRUE) + sd(data_AH$social_capital.decision.index, na.rm = TRUE)
AH_decision_lower_nonstdized
AH_decision_higher_nonstdized

ce_AH_RC1_decision <- conditional_effects(AH_RC1_decision,
                                          effects = "year:stdizesocial_capital.decision.index",
                                          int_conditions = list(stdizesocial_capital.decision.index = c(AH_decision_lower, AH_decision_higher)),
                                          re_formula = NA,
                                          prob = 0.9)$'year:stdizesocial_capital.decision.index' %>%
  mutate(year = as.numeric(as.character(year)),
         decision_group = ifelse(stdizesocial_capital.decision.index == AH_decision_lower, "Lower", "Higher"))

ce_AH_RC2_decision <- conditional_effects(AH_RC2_decision,
                                          effects = "year:stdizesocial_capital.decision.index",
                                          int_conditions = list(stdizesocial_capital.decision.index = c(AH_decision_lower, AH_decision_higher)),
                                          re_formula = NA,
                                          prob = 0.9)$'year:stdizesocial_capital.decision.index' %>%
  mutate(year = as.numeric(as.character(year)),
         decision_group = ifelse(stdizesocial_capital.decision.index == AH_decision_lower, "Lower", "Higher"))

ce_AH_RC3_decision <- conditional_effects(AH_RC3_decision,
                                          effects = "year:stdizesocial_capital.decision.index",
                                          int_conditions = list(stdizesocial_capital.decision.index = c(AH_decision_lower, AH_decision_higher)),
                                          re_formula = NA,
                                          prob = 0.9)$'year:stdizesocial_capital.decision.index' %>%
  mutate(year = as.numeric(as.character(year)),
         decision_group = ifelse(stdizesocial_capital.decision.index == AH_decision_lower, "Lower", "Higher"))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_decision <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = decision_group, fill = decision_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_fill_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
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

ce_AH_RC1_decision_plot <- AH_plot_decision(ce_AH_RC1_decision, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_decision_plot
ce_AH_RC2_decision_plot <- AH_plot_decision(ce_AH_RC2_decision, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_decision_plot 
ce_AH_RC3_decision_plot <- AH_plot_decision(ce_AH_RC3_decision, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_decision_plot

#--------------------
# EFFECT OF TRUST ON MSL

# Extract results for each effect
hyp_AH_trust_RC1 <- extract_AH(AH_RC1_trust, "stdizesocial_capital.trust.index", "RC1", "Trust")
hyp_AH_trust_RC2 <- extract_AH(AH_RC2_trust, "stdizesocial_capital.trust.index", "RC2", "Trust")
hyp_AH_trust_RC3 <- extract_AH(AH_RC3_trust, "stdizesocial_capital.trust.index", "RC3", "Trust")

# Combine results
hyp_AH_trust <- bind_rows(hyp_AH_trust_RC1, hyp_AH_trust_RC2, hyp_AH_trust_RC3) %>%
  select(component, effect, everything())

hyp_AH_trust_credible <- hyp_AH_trust %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
AH_trust_lower <- mean(AH_RC1_trust$data$stdizesocial_capital.trust.index, na.rm = TRUE) - sd(AH_RC1_trust$data$stdizesocial_capital.trust.index, na.rm = TRUE)
AH_trust_higher <- mean(AH_RC1_trust$data$stdizesocial_capital.trust.index, na.rm = TRUE) + sd(AH_RC1_trust$data$stdizesocial_capital.trust.index, na.rm = TRUE)

ce_AH_RC1_trust <- conditional_effects(AH_RC1_trust,
                                       effects = "year:stdizesocial_capital.trust.index",
                                       int_conditions = list(stdizesocial_capital.trust.index = c(AH_trust_lower, AH_trust_higher)),
                                       re_formula = NA,
                                       prob = 0.9)$'year:stdizesocial_capital.trust.index' %>%
  mutate(year = as.numeric(as.character(year)),
         trust_group = ifelse(stdizesocial_capital.trust.index == AH_trust_lower, "Lower", "Higher"))

ce_AH_RC2_trust <- conditional_effects(AH_RC2_trust,
                                       effects = "year:stdizesocial_capital.trust.index",
                                       int_conditions = list(stdizesocial_capital.trust.index = c(AH_trust_lower, AH_trust_higher)),
                                       re_formula = NA,
                                       prob = 0.9)$'year:stdizesocial_capital.trust.index' %>%
  mutate(year = as.numeric(as.character(year)),
         trust_group = ifelse(stdizesocial_capital.trust.index == AH_trust_lower, "Lower", "Higher"))

ce_AH_RC3_trust <- conditional_effects(AH_RC3_trust,
                                       effects = "year:stdizesocial_capital.trust.index",
                                       int_conditions = list(stdizesocial_capital.trust.index = c(AH_trust_lower, AH_trust_higher)),
                                       re_formula = NA,
                                       prob = 0.9)$'year:stdizesocial_capital.trust.index' %>%
  mutate(year = as.numeric(as.character(year)),
         trust_group = ifelse(stdizesocial_capital.trust.index == AH_trust_lower, "Lower", "Higher"))

# Plot conditional effects (combine with Ahus in "12-plots.R")
AH_plot_trust <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = trust_group, fill = trust_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_fill_manual(values = c("Lower" = "slateblue3", "Higher" = "darkorange2")) +
    scale_x_continuous(breaks = c(2016, 2018, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Trust Index", fill = "Trust Index") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_AH_RC1_trust_plot <- AH_plot_trust(ce_AH_RC1_trust, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_trust_plot
ce_AH_RC2_trust_plot <- AH_plot_trust(ce_AH_RC2_trust, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_trust_plot
ce_AH_RC3_trust_plot <- AH_plot_trust(ce_AH_RC3_trust, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_trust_plot

#--------------------
# EFFECT OF PERCEIVED CLIMATE IMPACTS ON MSL

# Extract results for each effect 
AH_hyp_RC1_ccimpact.18 <- extract_hypothesis(AH_RC1_ccimpact, "ccimpactworse = 0", "RC1", "Climate Change Impacts (Worse vs Same/Better, 2018)", "Climate Change Impacts")
AH_hyp_RC1_ccimpact.26 <- extract_hypothesis(AH_RC1_ccimpact, "ccimpactworse + year2026:ccimpactworse = 0", "RC1", "Climate Change Impacts (Worse vs Same/Better, 2026)", "Climate Change Impacts")
AH_hyp_RC1_ccimpact.int26v18 <- extract_hypothesis(AH_RC1_ccimpact, "year2026:ccimpactworse = 0", "RC1", "Climate Change Impacts x Year (2026 vs 2018)", "Climate Change Impacts")

AH_hyp_RC2_ccimpact.18 <- extract_hypothesis(AH_RC2_ccimpact, "ccimpactworse = 0", "RC2", "Climate Change Impacts (Worse vs Same/Better, 2018)", "Climate Change Impacts")
AH_hyp_RC2_ccimpact.26 <- extract_hypothesis(AH_RC2_ccimpact, "ccimpactworse + year2026:ccimpactworse = 0", "RC2", "Climate Change Impacts (Worse vs Same/Better, 2026)", "Climate Change Impacts")
AH_hyp_RC2_ccimpact.int26v18 <- extract_hypothesis(AH_RC2_ccimpact, "year2026:ccimpactworse = 0", "RC2", "Climate Change Impacts x Year (2026 vs 2018)", "Climate Change Impacts")

AH_hyp_RC3_ccimpact.18 <- extract_hypothesis(AH_RC3_ccimpact, "ccimpactworse = 0", "RC3", "Climate Change Impacts (Worse vs Same/Better, 2018)", "Climate Change Impacts")
AH_hyp_RC3_ccimpact.26 <- extract_hypothesis(AH_RC3_ccimpact, "ccimpactworse + year2026:ccimpactworse = 0", "RC3", "Climate Change Impacts (Worse vs Same/Better, 2026)", "Climate Change Impacts")
AH_hyp_RC3_ccimpact.int26v18 <- extract_hypothesis(AH_RC3_ccimpact, "year2026:ccimpactworse = 0", "RC3", "Climate Change Impacts x Year (2026 vs 2018)", "Climate Change Impacts")

# Combine results
hyp_AH_ccimpact <- bind_rows(AH_hyp_RC1_ccimpact.18, AH_hyp_RC1_ccimpact.26, AH_hyp_RC1_ccimpact.int26v18,
                             AH_hyp_RC2_ccimpact.18, AH_hyp_RC2_ccimpact.26, AH_hyp_RC2_ccimpact.int26v18,
                             AH_hyp_RC3_ccimpact.18, AH_hyp_RC3_ccimpact.26, AH_hyp_RC3_ccimpact.int26v18) %>%
  select(component, effect, everything())

hyp_AH_ccimpact_credible <- hyp_AH_ccimpact %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

# Extract conditional effects 
ce_AH_RC1_ccimpact <- conditional_effects(AH_RC1_ccimpact,
                                          effects = "year:ccimpact",
                                          re_formula = NA,
                                          prob = 0.9)$'year:ccimpact' %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC2_ccimpact <- conditional_effects(AH_RC2_ccimpact,
                                          effects = "year:ccimpact",
                                          re_formula = NA,
                                          prob = 0.9)$'year:ccimpact' %>%
  mutate(year = as.numeric(as.character(year)))

ce_AH_RC3_ccimpact <- conditional_effects(AH_RC3_ccimpact,
                                          effects = "year:ccimpact",
                                          re_formula = NA,
                                          prob = 0.9)$'year:ccimpact' %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects (combine with Karkar in "12-plots.R")
AH_plot_ccimpact <- function(data,title){
  ggplot(data, aes(year, estimate__, colour = ccimpact, fill = ccimpact)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c(sameorbetter = "slateblue3", worse = "darkorange2"),
                        labels = c(sameorbetter = "Same or Better", worse = "Worse")) +
    scale_fill_manual(values = c(sameorbetter = "slateblue3", worse = "darkorange2"),
                      labels = c(sameorbetter = "Same or Better", worse = "Worse")) +
    scale_x_continuous(breaks = c(2018, 2026)) +
    labs(x = "Year",
         y = "MSL Category",
         title = title,
         colour = "Perceived Climate Impacts", fill = "Perceived Climate Impacts") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))}

ce_AH_RC1_ccimpact_plot <- AH_plot_ccimpact(ce_AH_RC1_ccimpact, "Ahus: Housing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC1_ccimpact_plot
ce_AH_RC2_ccimpact_plot <- AH_plot_ccimpact(ce_AH_RC2_ccimpact, "Ahus: Electrical Assets") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC2_ccimpact_plot
ce_AH_RC3_ccimpact_plot <- AH_plot_ccimpact(ce_AH_RC3_ccimpact, "Ahus: Roofing Quality") +
  scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
ce_AH_RC3_ccimpact_plot

#--------------------
# RESULTS: ALL SOCIAL PREDICTORS

hyp_AH_all <- bind_rows(hyp_AH_gender,
                        hyp_AH_age,
                        hyp_AH_liv.pri,
                        hyp_AH_liv.div,
                        hyp_AH_decision,
                        hyp_AH_trust,
                        hyp_AH_ccimpact) 
write_xlsx(hyp_AH_all, "Plots/Ahus_All_Results.xlsx")

hyp_AH_all_credible <- hyp_AH_all %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)
write_xlsx(hyp_AH_all_credible, "Plots/Ahus_All_Results_credible.xlsx")

# Clean results for plotting
hyp_AH_all_clean <- hyp_AH_all_credible %>%
  mutate(component = factor(component, levels = c("RC1", "RC2", "RC3"),
                            labels = c("Housing Quality",
                                       "Electrical Assets",
                                       "Roofing Quality")),
         effect_order = row_number(),
         effect_order = factor(effect_order,
                               levels = rev(unique(effect_order))))

# Convert from OR to log-odds for whisker plots
hyp_AH_all_whisker <- hyp_AH_all_clean %>%
  mutate(across(c(Estimate, CI.Lower, CI.Upper, CI.Lower50, CI.Upper50), log)) 

# Whisker plots (combine with Ahus in "12-plots.R")
AH_whiskerplot_all <- ggplot(hyp_AH_all_whisker, #use log-odds data for whisker plots
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
  scale_y_discrete(labels = setNames(hyp_AH_all_whisker$effect,
                                     hyp_AH_all_whisker$effect_order),
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
       title = "Ahus") +
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
AH_whiskerplot_all

# ------------------------------------
# FOOD INSECURITY

# Extract results 
AH_hyp_fi_year.26v18 <- extract_hypothesis(AH_fi_year, "year2026 = 0", "FI", "Year (2026 vs 2018)", "Year")

AH_hyp_fi_gender.18 <- extract_hypothesis(AH_fi_gender, "genderm = 0", "FI", "Gender (Male vs Female, 2018)", "Gender")
AH_hyp_fi_gender.26 <- extract_hypothesis(AH_fi_gender, "genderm + year2026:genderm = 0", "FI", "Gender (Male vs Female, 2026)", "Gender")
AH_hyp_fi_gender.int26v18 <- extract_hypothesis(AH_fi_gender, "year2026:genderm = 0", "FI", "Gender x Year (2026 vs 2018)", "Gender")

AH_hyp_fi_age.18 <- extract_hypothesis(AH_fi_age, "stdizeage.recorded = 0", "FI", "Age (2018)", "Age")
AH_hyp_fi_age.26 <- extract_hypothesis(AH_fi_age, "stdizeage.recorded + year2026:stdizeage.recorded = 0", "FI", "Age (2026)", "Age")
AH_hyp_fi_age.int26v18 <- extract_hypothesis(AH_fi_age, "year2026:stdizeage.recorded = 0", "FI", "Age x Year (2026 vs 2018)", "Age")

AH_hyp_fi_liv.pri.18 <- extract_hypothesis(AH_fi_liv.pri, "livelihoods.primary.marine1 = 0", "FI", "Primary Livelihood (Marine vs Non-Marine, 2018)", "Primary Livelihood")
AH_hyp_fi_liv.pri.26 <- extract_hypothesis(AH_fi_liv.pri, "livelihoods.primary.marine1 + year2026:livelihoods.primary.marine1 = 0", "FI", "Primary Livelihood (Marine vs Non-Marine, 2026)", "Primary Livelihood")
AH_hyp_fi_liv.pri.int26v18 <- extract_hypothesis(AH_fi_liv.pri, "year2026:livelihoods.primary.marine1 = 0", "FI", "Primary Livelihood x Year (2026 vs 2018)", "Primary Livelihood")

AH_hyp_fi_liv.div.18 <- extract_hypothesis(AH_fi_liv.div, "stdizelivelihoods.diversity = 0", "FI", "Livelihood Diversity (2018)", "Livelihood Diversity")
AH_hyp_fi_liv.div.26 <- extract_hypothesis(AH_fi_liv.div, "stdizelivelihoods.diversity + year2026:stdizelivelihoods.diversity = 0", "FI", "Livelihood Diversity (2026)", "Livelihood Diversity")
AH_hyp_fi_liv.div.int26v18 <- extract_hypothesis(AH_fi_liv.div, "year2026:stdizelivelihoods.diversity = 0", "FI", "Livelihood Diversity x Year (2026 vs 2018)", "Livelihood Diversity")

AH_hyp_fi_decision.18 <- extract_hypothesis(AH_fi_decision, "stdizesocial_capital.decision.index = 0", "FI", "Decision-Making Index (2018)", "Decision-Making Index")
AH_hyp_fi_decision.26 <- extract_hypothesis(AH_fi_decision, "stdizesocial_capital.decision.index + year2026:stdizesocial_capital.decision.index = 0", "FI", "Decision-Making Index (2026)", "Decision-Making Index")
AH_hyp_fi_decision.int26v18 <- extract_hypothesis(AH_fi_decision, "year2026:stdizesocial_capital.decision.index = 0", "FI", "Decision-making x Year (2026 vs 2018)", "Decision-Making Index")

AH_hyp_fi_trust.18 <- extract_hypothesis(AH_fi_trust, "stdizesocial_capital.trust.index = 0", "FI", "Trust Index (2018)", "Trust Index")
AH_hyp_fi_trust.26 <- extract_hypothesis(AH_fi_trust, "stdizesocial_capital.trust.index + year2026:stdizesocial_capital.trust.index = 0", "FI", "Trust Index (2026)", "Trust Index")
AH_hyp_fi_trust.int26v18 <- extract_hypothesis(AH_fi_trust, "year2026:stdizesocial_capital.trust.index = 0", "FI", "Trust x Year (2026 vs 2018)", "Trust Index")

AH_hyp_fi_ccimpact.18 <- extract_hypothesis(AH_fi_ccimpact, "ccimpactworse = 0", "FI", "Climate Change Impacts (2018)", "Climate Change Impacts")
AH_hyp_fi_ccimpact.26 <- extract_hypothesis(AH_fi_ccimpact, "ccimpactworse + year2026:ccimpactworse = 0", "FI", "Climate Change Impacts (2026)", "Climate Change Impacts")
AH_hyp_fi_ccimpact.int26v18 <- extract_hypothesis(AH_fi_ccimpact, "year2026:ccimpactworse = 0", "FI", "Climate Change Impacts x Year (2026 vs 2018)", "Climate Change Impacts")

# Combine results
hyp_AH_fi <- bind_rows(AH_hyp_fi_year.26v18,
                       AH_hyp_fi_gender.18, AH_hyp_fi_gender.26, AH_hyp_fi_gender.int26v18,
                       AH_hyp_fi_age.18, AH_hyp_fi_age.26, AH_hyp_fi_age.int26v18,
                       AH_hyp_fi_liv.pri.18, AH_hyp_fi_liv.pri.26, AH_hyp_fi_liv.pri.int26v18,
                       AH_hyp_fi_liv.div.18, AH_hyp_fi_liv.div.26, AH_hyp_fi_liv.div.int26v18,
                       AH_hyp_fi_decision.18, AH_hyp_fi_decision.26, AH_hyp_fi_decision.int26v18,
                       AH_hyp_fi_trust.18, AH_hyp_fi_trust.26, AH_hyp_fi_trust.int26v18,
                       AH_hyp_fi_ccimpact.18, AH_hyp_fi_ccimpact.26, AH_hyp_fi_ccimpact.int26v18) %>%
  select(component, effect, everything()) 

hyp_AH_fi_credible <- hyp_AH_fi %>%
  filter(Prob_greater1 >= 0.95 | Prob_less1 >= 0.95)

write_xlsx(hyp_AH_fi, "Plots/hyp_AH_fi.xlsx")

# Clean results for plotting
hyp_AH_fi_clean <- hyp_AH_fi %>%
  filter(effect %in% c("Year (2026 vs 2018)", "Age (2026)", "Primary Livelihood (Marine vs Non-Marine, 2018)"
                       )) %>%
  mutate(component = factor(component, levels = c("FI"),
                            labels = c(""))) %>%
  mutate(effect_order = row_number(),
         effect_order = factor(effect_order,
                               levels = rev(unique(effect_order))))

# Convert from OR to log-odds for whisker plots
hyp_AH_fi_whisker <- hyp_AH_fi_clean %>%
  mutate(across(c(Estimate, CI.Lower, CI.Upper, CI.Lower50, CI.Upper50), log)) 

# Whisker plots 
AH_whiskerplot_fi <- ggplot(hyp_AH_fi_whisker, aes(x = Estimate, y = effect_order, colour = predictor)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey65") +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper),
                 width = 0, linewidth = 0.8) +
  geom_errorbarh(aes(xmin = CI.Lower50, xmax = CI.Upper50),
                 width = 0, linewidth = 2) +
  geom_point(size = 3) +
  facet_wrap(vars(component), ncol = 1,
             strip.position = "left",
             scales = "free_y", space = "free_y") +
  geom_text(aes(x = Estimate,
                label = effect),
            vjust = -0.7,
            size = 3.5) +
  scale_y_discrete(labels = setNames(hyp_AH_fi_whisker$effect,
                                     hyp_AH_fi_whisker$effect_order),
                   expand = expansion(add = 1.5)) +
  scale_colour_manual(values = c("Year" = "firebrick3",
                                 "Gender" = "firebrick3",
                                 "Age" = "darkorange3",
                                 "Primary Livelihood" = "forestgreen",
                                 "Decision-Making Index" = "steelblue3",
                                 "Trust Index" = "mediumpurple3",
                                 "Climate Change Impacts" = "deeppink4")) +
  labs(x = "Cumulative Log-Odds",
       y = "Food Insecurity",
       title = "Ahus") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
        axis.text.x = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 10, colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.placement = "outside", strip.background = element_blank(),
        panel.spacing.y = unit(0, "cm"),
        panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4))
AH_whiskerplot_fi

# Extract conditional effects 
ce_AH_fi_year <- conditional_effects(AH_fi_year,
                                     effects = "year",
                                     re_formula = NA,
                                     prob = 0.9)$year %>%
  mutate(year = as.numeric(as.character(year)))
                                      
ce_AH_fi_age <- conditional_effects(AH_fi_age,
                                    effects = "year:age.recorded",
                                    int_conditions = list(age.recorded = c(AH_age_younger, AH_age_older)),
                                    re_formula = NA,
                                    prob = 0.9)$'year:age.recorded' %>%
  mutate(year = as.numeric(as.character(year)),
         age_group = ifelse(age.recorded == AH_age_younger, "Younger", "Older"))

ce_AH_fi_liv.pri <- conditional_effects(AH_fi_liv.pri,
                                    effects = "year:livelihoods.primary.marine",
                                    re_formula = NA,
                                    prob = 0.9)$'year:livelihoods.primary.marine' %>%
  mutate(year = as.numeric(as.character(year)))

# Plot conditional effects
AH_plot_fi_year <- ggplot(ce_AH_fi_year, aes(year, estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, fill = "firebrick3") +
    geom_line(linewidth = 1.4, colour = "firebrick3") +
    geom_point(size = 2.5, colour = "firebrick3") +
    scale_x_continuous(breaks = c(2018, 2026)) +
    labs(x = "Year",
         y = "Food Insecurity") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))
AH_plot_fi_year

AH_plot_fi_age <- ggplot(ce_AH_fi_age, aes(year, estimate__, colour = age_group, fill = age_group)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Younger" = "slateblue3", "Older" = "darkorange2")) +
    scale_fill_manual(values = c("Younger" = "slateblue3", "Older" = "darkorange2")) +
    scale_x_continuous(breaks = c(2018, 2026)) +
    labs(x = "Year",
         y = "Food Insecurity",
         colour = "Age", fill = "Age") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
          axis.text = element_text(size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 8),
          plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))
AH_plot_fi_age

AH_plot_fi_liv.pri <- ggplot(ce_AH_fi_liv.pri, aes(year, estimate__, colour = livelihoods.primary.marine, fill = livelihoods.primary.marine)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("1" = "slateblue3", "0" = "darkorange2"),
                      labels = c("1" = "Marine", "0" = "Non-Marine")) +
  scale_fill_manual(values = c("1" = "slateblue3", "0" = "darkorange2"),
                    labels = c("1" = "Marine", "0" = "Non-Marine")) +
  scale_x_continuous(breaks = c(2018, 2026)) +
  labs(x = "Year",
       y = "Food Insecurity",
       colour = "Primary Livelihood", fill = "Primary Livelihood") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10, colour = "black"),
        axis.text = element_text(size = 8, colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        plot.background = element_rect(colour = "black", fill = NA, linewidth = 1))
AH_plot_fi_liv.pri

#--------------------
# DESCRIPTIVE CHANGES IN MSL AND FI

# Percent of households who increase/decrease MSL scores between 2016 and 2026  
descriptive_changes_AH <- data_AH %>%
  select(SharingUnitID_NEW, year, RC1, RC2, RC3, fi_index) %>%
  pivot_wider(names_from = year, values_from = c(RC1, RC2, RC3, fi_index)) %>%
  mutate(change_RC1_16to26 = RC1_2026 - RC1_2016,
         change_RC2_16to26 = RC2_2026 - RC2_2016,
         change_RC3_16to26 = RC3_2026 - RC3_2016,
         change_fi_index_16to26 = fi_index_2026 - fi_index_2016) %>%
  pivot_longer(starts_with("change_"), names_to = "period", values_to = "change") %>%
  filter(!is.na(change)) %>%
  group_by(period) %>%
  summarise(panel_n = n(),
            increase = (sum(change > 0)/panel_n)*100,
            decrease = (sum(change < 0)/panel_n)*100,
            same = (sum(change == 0)/panel_n)*100)
descriptive_changes_AH

# Change in percentile rank  
data_AH <- data_AH %>%
  group_by(year) %>%
  mutate(RC1_percentile = percent_rank(RC1)*100,
         RC2_percentile = percent_rank(RC2)*100,
         RC3_percentile = percent_rank(RC3)*100,
         fi_percentile = percent_rank(fi_index)*100) %>%
  ungroup()

descriptive_percentiles_AH <- data_AH %>%
  select(SharingUnitID_NEW, year, RC1_percentile, RC2_percentile, RC3_percentile, fi_percentile) %>%
  pivot_wider(names_from = year,
              values_from = c(RC1_percentile, RC2_percentile, RC3_percentile, fi_percentile)) %>%
  mutate(change_RC1_16to26 = RC1_percentile_2026 - RC1_percentile_2016,
         change_RC2_16to26 = RC2_percentile_2026 - RC2_percentile_2016,
         change_RC3_16to26 = RC3_percentile_2026 - RC3_percentile_2016,
         change_fi_16to26 = fi_percentile_2026 - fi_percentile_2016) %>%
  pivot_longer(starts_with("change_"), names_to = "period", values_to = "change") %>%
  filter(!is.na(change)) %>%
  group_by(period) %>%
  summarise(panel_n = n(),
            mean_change = mean(change),
            median_change = median(change),
            min_change = min(change),
            max_change = max(change),
            IQR_change = IQR(change))
descriptive_percentiles_AH

#--------------------
# SAVE OUTPUTS
save.image("9-results_AH.RData")

# load("6-descriptives.RData")

#--------------------
# OPEN PACKAGES
library(tidyverse)
library(flextable)
library(ggplot2)
library(patchwork)

# find("rename")
# detach("package:lessR", unload = TRUE)

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
# SAMPLE SIZES

# Total sample size in each survey year
sample_table <- data %>% 
  mutate(village = ifelse(village == "AH", "Ahus", 
                          ifelse(village == "MU", "Muluk", "Wadau"))) %>% 
  rename(Year = year) %>%
  count(Year, village, 
        name = "n_households") %>%
  pivot_wider(names_from = village,
              values_from = n_households,
              values_fill = 0) %>%
  mutate(Karkar = Muluk + Wadau,
         Total = Ahus + Muluk + Wadau)
sample_table

sample_table_ft <- sample_table %>% 
  mutate(Year = as.character(Year),
         Muluk = ifelse(Muluk == 0, "-", Muluk),
         Wadau = ifelse(Wadau == 0, "-", Wadau),
         Karkar = ifelse(Karkar == 0, "-", Karkar)) %>% 
  flextable(col_keys = c("Year", "Ahus", "Muluk", "Wadau", "Karkar", "Total")) %>%
  set_header_labels(Year = "", Ahus = "", Muluk = "Muluk", Wadau = "Wadau", Karkar = "Total", Total = "") %>%
  add_header_row(values = c("Sample Period", "Ahus", "Karkar", "Total"),
                 colwidths = c(1, 1, 3, 1)) %>%
  theme_vanilla() %>%
  align(part = "all", align = "right") %>%
  autofit()
sample_table_ft
save_as_docx(sample_table_ft, path = "Plots/sample_table_ft.docx") 

# Panel sample sizes
panel_table <- data %>%
  group_by(SharingUnitID_NEW, village) %>%
  mutate(village = ifelse(village == "AH", "Ahus", 
                          ifelse(village == "MU", "Muluk", "Wadau"))) %>% 
  mutate(panel_pattern = ifelse(X2016and2018and2026 == "YES", "2016, 2018 and 2026",
                                ifelse(X2016and2026 == "YES", "2016 and 2026",
                                       ifelse(X2018and2026 == "YES", "2018 and 2026",
                                              ifelse(X2016and2018 == "YES", "2016 and 2018",
                                                     ifelse(year == 2016, "2016 only",
                                                            ifelse(year == 2018, "2018 only",
                                                                   "2026 only"))))))) %>%
  group_by(village, panel_pattern) %>%
  summarise(n_households = n_distinct(SharingUnitID_NEW),
            .groups = "drop") %>%
  mutate(panel_pattern = factor(panel_pattern,
                                levels = c("2016 only",
                                           "2018 only",
                                           "2026 only",
                                           "2016 and 2026",
                                           "2016 and 2018",
                                           "2018 and 2026",
                                           "2016, 2018 and 2026"))) %>%
  arrange(panel_pattern) %>%
  pivot_wider(names_from = village,
              values_from = n_households,
              values_fill = 0) %>%
  mutate(Karkar = Muluk + Wadau,
         Total = Ahus + Muluk + Wadau) %>%
  rename(Year = panel_pattern)
panel_table

panel_table_ft <- panel_table %>% 
  mutate(Year = as.character(Year),
         Muluk = ifelse(Muluk == 0, "-", Muluk),
         Wadau = ifelse(Wadau == 0, "-", Wadau),
         Karkar = ifelse(Karkar == 0, "-", Karkar)) %>% 
  flextable(col_keys = c("Year", "Ahus", "Muluk", "Wadau", "Karkar", "Total")) %>%
  set_header_labels(Year = "", Ahus = "", Muluk = "Muluk", Wadau = "Wadau", Karkar = "Total", Total = "") %>%
  add_header_row(values = c("Sample Period", "Ahus", "Karkar", "Total"),
                 colwidths = c(1, 1, 3, 1)) %>%
  theme_vanilla() %>%
  align(part = "all", align = "right") %>%
  autofit()
panel_table_ft
save_as_docx(panel_table_ft, path = "Plots/panel_table_ft.docx") 

#--------------------
# DEMOGRAPHICS

demographics_tables <- data %>%
  mutate(Community = ifelse(community == "AH", "Ahus",
                            ifelse(community == "KA", "Karkar", NA))) %>%
  filter(!is.na(Community)) %>%
  distinct(year, Community, SharingUnitID_NEW, gender, age.recorded, hh_size,
           livelihoods.primary, livelihoods.diversity,
           social_capital.decision.index, social_capital.trust.index,
           ccimpact,
           RC1, RC2, RC3, fi_index) %>%
  group_by(Community, Year = year) %>%
  summarise(gender.m = sum(gender == "m", na.rm = TRUE),
            gender.f = sum(gender == "f", na.rm = TRUE),
            gender.missing = sum(is.na(gender) | gender == ""),
            priliv.marine = sum(livelihoods.primary == "marine", na.rm = TRUE),
            priliv.agri = sum(livelihoods.primary == "agriculture", na.rm = TRUE),
            priliv.other = sum(livelihoods.primary == "other", na.rm = TRUE),
            cc.sameorbetter = sum(ccimpact == "sameorbetter", na.rm = TRUE),
            cc.worse = sum(ccimpact == "worse", na.rm = TRUE),
            across(c(age.recorded, hh_size,
                     livelihoods.diversity,
                     social_capital.decision.index, social_capital.trust.index,
                     RC1, RC2, RC3, fi_index),
                   list(median = ~ median(.x, na.rm = TRUE),
                        range = ~ ifelse(all(is.na(.x)), NA,
                                         paste(min(.x, na.rm = TRUE),
                                               max(.x, na.rm = TRUE), sep = "-")))),
            .groups = "drop") %>%
  arrange(Year) %>%
  split(.$Community)

demographics_AH <- demographics_tables$Ahus %>%
  select(-Community)
demographics_KA <- demographics_tables$Karkar %>%
  select(-Community)

demographics_AH_table_ft <- demographics_AH %>%
  mutate(Year = as.character(Year)) %>%
  flextable() %>%
  theme_vanilla() %>%
  colformat_double(digits = 2) %>%
  align(part = "all", align = "right") %>%
  autofit()
demographics_AH_table_ft

demographics_KA_table_ft <- demographics_KA %>%
  mutate(Year = as.character(Year)) %>%
  flextable() %>%
  theme_vanilla() %>%
  colformat_double(digits = 2) %>%
  align(part = "all", align = "right") %>%
  autofit()
demographics_KA_table_ft

save_as_docx(demographics_AH_table_ft, path = "Plots/demographics_AH.docx") 
save_as_docx(demographics_KA_table_ft, path = "Plots/demographics_KA.docx") 

#--------------------
# LIVELIHOODS

# Primary livelihoods (marine vs agriculture vs other)
livelihoods.primary_stats <- data %>%
  filter(Livelihoods_RESPONSES == "YES") %>%
  group_by(year, community, livelihoods.primary) %>%
  summarise(n = n())%>%
  ungroup() %>%
  group_by(year, community) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

livelihoods.primary_plot <- ggplot(livelihoods.primary_stats,
                                   aes(x = community, y = percent, fill = livelihoods.primary)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  facet_grid(. ~ year, scales = "free_x", space = "free_x", switch = "x") +
  labs(title = "Primary Livelihoods",
       x = "",
       y = "Percent of Households",
       fill = "Primary Livelihood") +
  scale_fill_manual(values = c("marine" = "steelblue3",
                               "agriculture" = "olivedrab4",
                               "other" = "mediumpurple4"),
                    labels = c("marine" = "Marine",
                               "agriculture" = "Agriculture",
                               "other" = "Other")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("AH" = "Ahus", "KA" = "Karkar")) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
livelihoods.primary_plot

ggsave("Plots/livelihoods.primary_plot.png", plot = livelihoods.primary_plot,
       width = 20, height = 14, units = "cm", dpi = 500)

# Specific primary livelihoods for Ahus
livelihoods.primary.specific_stats_AH <- data %>%
  filter(Livelihoods_RESPONSES == "YES") %>%
  group_by(year, community, livelihoods.primary.specific) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(year, community) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()  %>%
  mutate(livelihood.group = ifelse(livelihoods.primary.specific %in% c("fishing", "gleaning", "marketing_mp", "tourism"), "marine",
                                          "other"),
         community.livelihood = interaction(community, livelihood.group)) %>%
  mutate(livelihoods.primary.specific = factor(livelihoods.primary.specific, levels = c("fishing", "tourism", "marketing_mp", "gleaning", "farming", "cashcrops", "salaried_employment", "informal_ea", "other")))

livelihoods.specific_AH_plot <- ggplot(livelihoods.primary.specific_stats_AH %>% filter(community == "AH"),
                                       aes(x = community.livelihood, y = percent, fill = livelihoods.primary.specific)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(. ~ year, scales = "free_x", space = "free_x", switch = "x") +
  labs(title = "Primary Livelihoods",
       x = "",
       y = "Percent of Households",
       fill = "Primary Livelihood: Ahus") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("AH.marine" = "Marine",
                              "AH.agriculture" = "Agriculture",
                              "AH.other"= "Other")) +
  scale_fill_manual(
    values = c("fishing" = "royalblue4",
               "tourism" = "royalblue",
               "marketing_mp" = "deepskyblue3",
               "gleaning" = "lightskyblue",
               "farming" = "darkgreen",
               "cashcrops" = "olivedrab",
               "salaried_employment" = "purple4",
               "informal_ea" = "mediumpurple3",
               "other" = "plum")) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
livelihoods.specific_AH_plot

ggsave("Plots/livelihoods.AH_plot.png", plot = livelihoods.specific_AH_plot,
       width = 20, height = 14, units = "cm", dpi = 500)

# Specific primary livelihoods for Karkar
livelihoods.primary.specific_stats_KA <- data %>%
  filter(Livelihoods_RESPONSES == "YES") %>%
  group_by(year, community, livelihoods.primary.specific) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(year, community) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(livelihood.group = ifelse(livelihoods.primary.specific %in% c("farming", "cashcrops"), "agriculture",
                                   "other"),
         livelihood.group = factor(livelihood.group, levels = c("agriculture", "other")),
         community.livelihood = interaction(community, livelihood.group)) %>%
  mutate(livelihoods.primary.specific = factor(livelihoods.primary.specific, levels = c("fishing", "tourism", "marketing_mp", "gleaning", "farming", "cashcrops", "salaried_employment", "informal_ea", "other")))

livelihoods.specific_KA_plot <- ggplot(livelihoods.primary.specific_stats_KA %>% filter(community == "KA"),
                                       aes(x = community.livelihood, y = percent, fill = livelihoods.primary.specific)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(. ~ year, scales = "free_x", space = "free_x", switch = "x") +
  labs(title = "Primary Livelihoods",
       x = "",
       y = "Percent of Households",
       fill = "Primary Livelihood: Karkar") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("KA.agriculture" = "Agriculture",
                              "KA.other" = "Other")) +
  scale_fill_manual(
    values = c("fishing" = "royalblue4",
               "tourism" = "royalblue",
               "marketing_mp" = "deepskyblue3",
               "gleaning" = "lightskyblue",
               "farming" = "darkgreen",
               "cashcrops" = "olivedrab",
               "salaried_employment" = "purple4",
               "informal_ea" = "mediumpurple3",
               "other" = "plum")) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
livelihoods.specific_KA_plot

ggsave("Plots/livelihoods.KA_plot.png", plot = livelihoods.specific_KA_plot,
       width = 20, height = 14, units = "cm", dpi = 500)

# Livelihoods mix
livelihoods.primaryANDmix_stats <- data %>%
  filter(Livelihoods_RESPONSES == "YES") %>%
  group_by(year, community, livelihoods.primary, livelihoods.mix) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(year, community) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(livelihoods.mix = factor(livelihoods.mix, levels = c("marine", "agriculture", "other", "marine, agriculture", "marine, other", "agriculture, other", "marine, agriculture, other")))

livelihoods.primaryANDmix_AH_plot <- ggplot(livelihoods.primaryANDmix_stats %>% filter(community == "AH"),
                                            aes(x = livelihoods.primary, y = percent, fill = livelihoods.mix)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(. ~ year, scales = "free_x", space = "free_x", switch = "x") +
  labs(title = "Livelihoods Mix",
       x = "Primary Livelihood",
       y = "Percent of Households",
       fill = "Livelihood Mix: AH") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 100)) +
  scale_x_discrete() +
  scale_fill_manual(values = c("marine" = "darkorange4",
                               "agriculture" = "darkorange2",
                               "other" = "orange",
                               "marine, agriculture" = "purple4",
                               "marine, other" = "mediumpurple3",
                               "agriculture, other" = "plum2",
                               "marine, agriculture, other" = "turquoise3"),
                    labels = c("marine" = "Marine Only",
                               "agriculture" = "Agriculture Only",
                               "other" = "Other Only",
                               "marine, agriculture" = "Marine and Agriculture",
                               "marine, other" = "Marine and Other",
                               "agriculture, other" = "Agriculture and Other",
                               "marine, agriculture, other" = "Marine, Agriculture and Other")) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
livelihoods.primaryANDmix_AH_plot              
ggsave("Plots/livelihoodsmix.AH_plot.png", plot = livelihoods.primaryANDmix_AH_plot,
       width = 20, height = 14, units = "cm", dpi = 500)

livelihoods.primaryANDmix_KA_plot <- ggplot(livelihoods.primaryANDmix_stats %>% filter(community == "KA"),
                                            aes(x = livelihoods.primary, y = percent, fill = livelihoods.mix)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(. ~ year, scales = "free_x", space = "free_x", switch = "x") +
  labs(title = "Livelihoods Mix",
       x = "Primary Livelihood",
       y = "Percent of Households",
       fill = "Livelihood Mix: KA") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 100)) +
  scale_x_discrete() +
  scale_fill_manual(values = c("marine" = "darkorange4",
                               "agriculture" = "darkorange2",
                               "other" = "orange",
                               "marine, agriculture" = "purple4",
                               "marine, other" = "mediumpurple3",
                               "agriculture, other" = "plum2",
                               "marine, agriculture, other" = "turquoise3"),
                    labels = c("marine" = "Marine Only",
                               "agriculture" = "Agriculture Only",
                               "other" = "Other Only",
                               "marine, agriculture" = "Marine and Agriculture",
                               "marine, other" = "Marine and Other",
                               "agriculture, other" = "Agriculture and Other",
                               "marine, agriculture, other" = "Marine, Agriculture and Other")) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
livelihoods.primaryANDmix_KA_plot
ggsave("Plots/livelihoodsmix.KA_plot.png", plot = livelihoods.primaryANDmix_KA_plot,
       width = 20, height = 14, units = "cm", dpi = 500)

#--------------------
# PANEL DATA: CHANGE IN MSL AND FI OVER TIME

outcome_year <- data %>%
  group_by(year,community) %>%
  summarise(n_total = n(),
            across(c(RC1, RC2, RC3, fi_index),
                   list(
                     mean = ~ mean(.x, na.rm = TRUE),
                     sd = ~ sd(.x, na.rm = TRUE),
                     se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
            .groups = "drop")

# PC1: Housing Quality Index
RC1_panelAH_plot <- ggplot() +
  geom_line(data = data_AH,
            aes(x = year, y = RC1,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.3,
            alpha = 0.5) +
  geom_point(data = data_AH,
             aes(x = year, y = RC1,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             stroke = 0,
             size = 1.5,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "AH"),
            aes(x = year,
                y = RC1_mean),
            colour = "darkcyan",
            linewidth = 1)  +
  geom_point(data = outcome_year %>% filter(community == "AH"),
             aes(x = year,
                 y = RC1_mean),
             colour = "darkcyan",
             size = 2.5)  +
  labs(x = "Year", 
       y = "Housing Quality Index Score", 
       title = "a. Housing Quality: Ahus") +
  scale_x_continuous(breaks = c(2016, 2018, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
RC1_panelAH_plot
ggsave("Plots/RC1_panelAH_plot.png", plot = RC1_panelAH_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")

RC1_panelKA_plot <- ggplot() +
  geom_line(data = data_KA,
            aes(x = year, y = RC1,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.3,
            alpha = 0.5) +
  geom_point(data = data_KA,
             aes(x = year, y = RC1,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             stroke = 0,
             size = 1.5,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "KA"),
            aes(x = year,
                y = RC1_mean),
            colour = "darkcyan",
            linewidth = 1)  +
  geom_point(data = outcome_year %>% filter(community == "KA"),
             aes(x = year,
                 y = RC1_mean),
             colour = "darkcyan",
             size = 2.5)  +
  labs(x = "Year", 
       y = "Housing Quality Index Score", 
       title = "b. Housing Quality: Karkar") +
  scale_x_continuous(breaks = c(2016, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
RC1_panelKA_plot
ggsave("Plots/RC1_panelKA_plot.png", plot = RC1_panelKA_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")

# PC2: Electrical Assets Index
RC2_panelAH_plot <- ggplot() +
  geom_line(data = data_AH,
            aes(x = year, y = RC2,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.3,
            alpha = 0.5) +
  geom_point(data = data_AH,
             aes(x = year, y = RC2,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             stroke = 0,
             size = 1.5,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "AH"),
            aes(x = year,
                y = RC2_mean),
            colour = "mediumpurple3",
            linewidth = 1)  +
  geom_point(data = outcome_year %>% filter(community == "AH"),
             aes(x = year,
                 y = RC2_mean),
             colour = "mediumpurple3",
             size = 2.5)  +
  labs(x = "Year", 
       y = "Electrical Assets Index Score", 
       title = "c. Electrical Assets: Ahus") +
  scale_x_continuous(breaks = c(2016, 2018, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
RC2_panelAH_plot
ggsave("Plots/RC2_panelAH_plot.png", plot = RC2_panelAH_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")

RC2_panelKA_plot <- ggplot() +
  geom_line(data = data_KA,
            aes(x = year, y = RC2,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.3,
            alpha = 0.5) +
  geom_point(data = data_KA,
             aes(x = year, y = RC2,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             stroke = 0,
             size = 1.5,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "KA"),
            aes(x = year,
                y = RC2_mean),
            colour = "mediumpurple3",
            linewidth = 1)  +
  geom_point(data = outcome_year %>% filter(community == "KA"),
             aes(x = year,
                 y = RC2_mean),
             colour = "mediumpurple3",
             size = 2.5)  +
  labs(x = "Year", 
       y = "Electrical Assets Index Score", 
       title = "d. Electrical Assets: Karkar") +
  scale_x_continuous(breaks = c(2016, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
RC2_panelKA_plot
ggsave("Plots/RC2_panelKA_plot.png", plot = RC2_panelKA_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")

#PC3: Roofing Quality Index
RC3_panelAH_plot <- ggplot() +
  geom_line(data = data_AH,
            aes(x = year, y = RC3,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.3,
            alpha = 0.5) +
  geom_point(data = data_AH,
             aes(x = year, y = RC3,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             stroke = 0,
             size = 1.5,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "AH"),
            aes(x = year,
                y = RC3_mean),
            colour = "deeppink4",
            linewidth = 1)  +
  geom_point(data = outcome_year %>% filter(community == "AH"),
             aes(x = year,
                 y = RC3_mean),
             colour = "deeppink4",
             size = 2.5)  +
  labs(x = "Year", 
       y = "Roofing Quality Index Score", 
       title = "e. Roofing Quality: Ahus") +
  scale_x_continuous(breaks = c(2016, 2018, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
RC3_panelAH_plot
ggsave("Plots/RC3_panelAH_plot.png", plot = RC3_panelAH_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")


RC3_panelKA_plot <- ggplot() +
  geom_line(data = data_KA,
            aes(x = year, y = RC3,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.3,
            alpha = 0.5) +
  geom_point(data = data_KA,
             aes(x = year, y = RC3,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             stroke = 0,
             size = 1.5,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "KA"),
            aes(x = year,
                y = RC3_mean),
            colour = "deeppink4",
            linewidth = 1)  +
  geom_point(data = outcome_year %>% filter(community == "KA"),
             aes(x = year,
                 y = RC3_mean),
             colour = "deeppink4",
             size = 2.5)  +
  labs(x = "Year", 
       y = "Roofing Quality Index Score", 
       title = "f. Roofing Quality: Karkar") +
  scale_x_continuous(breaks = c(2016, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
RC3_panelKA_plot
ggsave("Plots/RC3_panelKA_plot.png", plot = RC3_panelKA_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")

# Combine:

panel_year_plot <- wrap_plots(RC1_panelAH_plot, RC1_panelKA_plot, 
                              RC2_panelAH_plot, RC2_panelKA_plot,
                              RC3_panelAH_plot, RC3_panelKA_plot,
                              ncol = 2) &
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1))
panel_year_plot

ggsave("Plots/panel_year_plot.png", plot = panel_year_plot,
       width = 18, height = 25, units = "cm", 
       dpi = 500, bg = "white")

# FI: Food Insecurity Index (Ahus Only)
fi_panelAH_plot <- ggplot() +
  geom_line(data = data_AH,
            aes(x = year, y = fi_index,
                group = SharingUnitID_NEW),
            colour = "grey60",
            linewidth = 0.5,
            alpha = 0.5) +
  geom_point(data = data_AH,
             aes(x = year, y = fi_index,
                 group = SharingUnitID_NEW),
             colour = "grey60",
             size = 1,
             alpha = 0.5) +
  geom_line(data = outcome_year %>% filter(community == "AH"),
            aes(x = year,
                y = fi_index_mean),
            colour = "darkblue",
            linewidth = 1.5)  +
  geom_point(data = outcome_year %>% filter(community == "AH"),
             aes(x = year,
                 y = fi_index_mean),
             colour = "darkblue",
             size = 3)  +
  labs(x = "Year", 
       y = "Food Insecurity Index Score") +
  scale_x_continuous(breaks = c(2018, 2026), limits = c(2018, 2026)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
fi_panelAH_plot
ggsave("Plots/fi_panelAH_plot.png", plot = fi_panelAH_plot,
       width = 14, height = 14, units = "cm",
       dpi = 500,
       bg = "white")

#--------------------
# DISTRIBUTION OF MSL DATA

# PC1 (Housing Quality): Ahus
hist(data_AH$RC1, probability = TRUE)
lines(density(data_AH$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH$RC1))
sort(table(data_AH$RC1), decreasing = TRUE)

data_AH_2016 <- data_AH %>% filter(year == 2016)
hist(data_AH_2016$RC1, probability = TRUE)
lines(density(data_AH_2016$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2016$RC1))
sort(table(data_AH_2016$RC1), decreasing = TRUE)

data_AH_2018 <- data_AH %>% filter(year == 2018)
hist(data_AH_2018$RC1, probability = TRUE)
lines(density(data_AH_2018$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2018$RC1))
sort(table(data_AH_2018$RC1), decreasing = TRUE)

data_AH_2026 <- data_AH %>% filter(year == 2026)
hist(data_AH_2026$RC1, probability = TRUE)
lines(density(data_AH_2026$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2026$RC1))
sort(table(data_AH_2026$RC1), decreasing = TRUE)

# PC1 (Housing Quality): Karkar
hist(data_KA$RC1, probability = TRUE)
lines(density(data_KA$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA$RC1))
sort(table(data_KA$RC1), decreasing = TRUE)

data_KA_2016 <- data_KA %>% filter(year == 2016)
hist(data_KA_2016$RC1, probability = TRUE)
lines(density(data_KA_2016$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA_2016$RC1))
sort(table(data_KA_2016$RC1), decreasing = TRUE)

data_KA_2026 <- data_KA %>% filter(year == 2026)
hist(data_KA_2026$RC1, probability = TRUE)
lines(density(data_KA_2026$RC1, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA_2026$RC1))
sort(table(data_KA_2026$RC1), decreasing = TRUE)

# PC2 (Electrical Assets): Ahus
hist(data_AH$RC2, probability = TRUE)
lines(density(data_AH$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH$RC2))
sort(table(data_AH$RC2), decreasing = TRUE)

hist(data_AH_2016$RC2, probability = TRUE)
lines(density(data_AH_2016$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2016$RC2))
sort(table(data_AH_2016$RC2), decreasing = TRUE)

hist(data_AH_2018$RC2, probability = TRUE)
lines(density(data_AH_2018$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2018$RC2))
sort(table(data_AH_2018$RC2), decreasing = TRUE)

hist(data_AH_2026$RC2, probability = TRUE)
lines(density(data_AH_2026$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2026$RC2))
sort(table(data_AH_2026$RC2), decreasing = TRUE)

# PC2 (Electrical Assets): Karkar
hist(data_KA$RC2, probability = TRUE)
lines(density(data_KA$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA$RC2))
sort(table(data_KA$RC2), decreasing = TRUE)

hist(data_KA_2016$RC2, probability = TRUE)
lines(density(data_KA_2016$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA_2016$RC2))
sort(table(data_KA_2016$RC2), decreasing = TRUE)

hist(data_KA_2026$RC2, probability = TRUE)
lines(density(data_KA_2026$RC2, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA_2026$RC2))
sort(table(data_KA_2026$RC2), decreasing = TRUE)

# PC3 (Roofing Quality): Ahus
hist(data_AH$RC3, probability = TRUE)
lines(density(data_AH$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH$RC3))
sort(table(data_AH$RC3), decreasing = TRUE)

hist(data_AH_2016$RC3, probability = TRUE)
lines(density(data_AH_2016$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2016$RC3))
sort(table(data_AH_2016$RC3), decreasing = TRUE)

hist(data_AH_2018$RC3, probability = TRUE)
lines(density(data_AH_2018$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2018$RC3))
sort(table(data_AH_2018$RC3), decreasing = TRUE)

hist(data_AH_2026$RC3, probability = TRUE)
lines(density(data_AH_2026$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2026$RC3))
sort(table(data_AH_2026$RC3), decreasing = TRUE)

# PC3 (Roofing Quality): Karkar
hist(data_KA$RC3, probability = TRUE)
lines(density(data_KA$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA$RC3))
sort(table(data_KA$RC3), decreasing = TRUE)

hist(data_KA_2016$RC3, probability = TRUE)
lines(density(data_KA_2016$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA_2016$RC3))
sort(table(data_KA_2016$RC3), decreasing = TRUE)

hist(data_KA_2026$RC3, probability = TRUE)
lines(density(data_KA_2026$RC3, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_KA_2026$RC3))
sort(table(data_KA_2026$RC3), decreasing = TRUE)

# fi_index: Ahus
hist(data_AH$fi_index, probability = TRUE)
lines(density(data_AH$fi_index, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH$fi_index))
sort(table(data_AH$fi_index), decreasing = TRUE)

hist(data_AH_2018$fi_index, probability = TRUE)
lines(density(data_AH_2018$fi_index, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2018$fi_index))
sort(table(data_AH_2018$fi_index), decreasing = TRUE)

hist(data_AH_2026$fi_index, probability = TRUE)
lines(density(data_AH_2026$fi_index, na.rm = TRUE), col = "steelblue", lwd = 2)
length(unique(data_AH_2026$fi_index))
sort(table(data_AH_2026$fi_index), decreasing = TRUE)

#--------------------
# SAVE OUTPUTS
save.image("6-descriptives.RData")

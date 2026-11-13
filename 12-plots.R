#--------------------
# OPEN PACKAGES
library(ggplot2)
library(patchwork)

#--------------------
# LOAD PLOTS
load("9-results_AH.RData")
load("11-results_KA.RData")
load("2-pca_AH.RData")
load("3-pca_KA.RData")
load("6-descriptives.RData")

#--------------------
# CONDITIONAL EFFECT OF YEAR ON MSL

ce_year_plot <- wrap_plots(ce_AH_RC1_year_plot + labs(y = "Housing Quality"),
                           ce_AH_RC2_year_plot + labs(y = "Electrical Assets"),
                           ce_AH_RC3_year_plot + labs(y = "Roofing Quality"),
                           ce_KA_RC1_year_plot + labs(y = "Housing Quality"),
                           ce_KA_RC2_year_plot + labs(y = "Electrical Assets"),
                           ce_KA_RC3_year_plot + labs(y = "Roofing Quality"),
                           ncol = 3) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1))
ce_year_plot

ggsave("Plots/ce_year_plot.png", plot = ce_year_plot,
       width = 28, height = 16, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# CONDITIONAL EFFECT OF GENDER ON MSL

ce_gender_plot <- wrap_plots(ce_AH_RC1_gender_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                             ce_AH_RC2_gender_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                             ce_AH_RC3_gender_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)),
                             ce_KA_RC1_gender_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(13, 16, 19, 22, 25, 28)),
                             ce_KA_RC2_gender_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(13, 16, 19, 22, 25, 28)),
                             ce_KA_RC3_gender_plot + labs(y = "Roofing Quality"),
                             ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
ce_gender_plot

ggsave("Plots/ce_gender_plot.png", plot = ce_gender_plot,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# CONDITIONAL EFFECT OF AGE ON MSL

ce_age_plot <- wrap_plots(ce_AH_RC1_age_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                             ce_AH_RC2_age_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)),
                             ce_AH_RC3_age_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)),
                             ce_KA_RC1_age_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(13, 16, 19, 22, 25, 28)),
                             ce_KA_RC2_age_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(13, 15, 17, 19, 21, 23)),
                             ce_KA_RC3_age_plot + labs(y = "Roofing Quality"),
                             ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
ce_age_plot

ggsave("Plots/ce_age_plot.png", plot = ce_age_plot,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# CONDITIONAL EFFECT PRIMARY LIVELIHOOD ON MSL

ce_liv.pri_plot_AH <- wrap_plots(ce_AH_RC1_liv.pri_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                                 ce_AH_RC2_liv.pri_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60)),
                                 ce_AH_RC3_liv.pri_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)),
                                 ncol = 3, guides = "collect") 

ce_liv.pri_plot_KA <- wrap_plots(ce_KA_RC1_liv.pri_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(15, 20, 25, 30)) + 
                                   scale_colour_manual(values = c("1" = "darkcyan", "0" = "salmon2"), labels = c("1" = "Agricultural", "0" = "Non-Agricultural")) + 
                                   scale_fill_manual(values = c("1" = "darkcyan", "0" = "salmon2"), labels = c("1" = "Agricultural", "0" = "Non-Agricultural")),
                                 ce_KA_RC2_liv.pri_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(12, 15, 18, 21, 24)) + 
                                   scale_colour_manual(values = c("1" = "darkcyan", "0" = "salmon2"), labels = c("1" = "Agricultural", "0" = "Non-Agricultural")) + 
                                   scale_fill_manual(values = c("1" = "darkcyan", "0" = "salmon2"), labels = c("1" = "Agricultural", "0" = "Non-Agricultural")),
                                 ce_KA_RC3_liv.pri_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(6, 10, 14, 18)) + 
                                   scale_colour_manual(values = c("1" = "darkcyan", "0" = "salmon2"), labels = c("1" = "Agricultural", "0" = "Non-Agricultural")) + 
                                   scale_fill_manual(values = c("1" = "darkcyan", "0" = "salmon2"), labels = c("1" = "Agricultural", "0" = "Non-Agricultural")),
                                 ncol = 3, guides = "collect") 

ce_liv.pri_plot <- (ce_liv.pri_plot_AH / ce_liv.pri_plot_KA) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "right")
ce_liv.pri_plot

ggsave("Plots/ce_liv.pri_plot.png", plot = ce_liv.pri_plot,
       width = 28, height = 16, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# CONDITIONAL EFFECT OF LIVELIHOOD DIVERSITY ON MSL

ce_liv.div_plot <- wrap_plots(ce_AH_RC1_liv.div_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                          ce_AH_RC2_liv.div_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(20, 30, 40, 50)),
                          ce_AH_RC3_liv.div_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)),
                          ce_KA_RC1_liv.div_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(16, 20, 24, 28, 32)),
                          ce_KA_RC2_liv.div_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(8, 12, 16, 20, 24)),
                          ce_KA_RC3_liv.div_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(8, 12, 16, 20, 24)),
                          ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
ce_liv.div_plot

ggsave("Plots/ce_liv.div_plot.png", plot = ce_liv.div_plot,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# CONDITIONAL EFFECT OF DECISION MAKING ON MSL

ce_decision_plot <- wrap_plots(ce_AH_RC1_decision_plot + labs(y = "Housing Quality", colour = "Decision Involvement", fill = "Decision Involvement") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                              ce_AH_RC2_decision_plot + labs(y = "Electrical Assets", colour = "Decision Involvement", fill = "Decision Involvement") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                              ce_AH_RC3_decision_plot + labs(y = "Roofing Quality", colour = "Decision Involvement", fill = "Decision Involvement") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)),
                              ce_KA_RC1_decision_plot + labs(y = "Housing Quality", colour = "Decision Involvement", fill = "Decision Involvement") + scale_y_continuous(breaks = c(16, 20, 24, 28, 32)),
                              ce_KA_RC2_decision_plot + labs(y = "Electrical Assets", colour = "Decision Involvement", fill = "Decision Involvement") + scale_y_continuous(breaks = c(8, 12, 16, 20, 24)),
                              ce_KA_RC3_decision_plot + labs(y = "Roofing Quality", colour = "Decision Involvement", fill = "Decision Involvement") + scale_y_continuous(breaks = c(8, 12, 16, 20, 24)),
                              ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
ce_decision_plot

ggsave("Plots/ce_decision_plot.png", plot = ce_decision_plot,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# EFFECT OF TRUST ON MSL

ce_trust_plot <- wrap_plots(ce_AH_RC1_trust_plot + labs(y = "Housing Quality", colour = "Trust Level", fill = "Trust Level") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                              ce_AH_RC2_trust_plot + labs(y = "Electrical Assets", colour = "Trust Level", fill = "Trust Level") + scale_y_continuous(breaks = c(20, 30, 40, 50)),
                              ce_AH_RC3_trust_plot + labs(y = "Roofing Quality", colour = "Trust Level", fill = "Trust Level") + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)),
                              ce_KA_RC1_trust_plot + labs(y = "Housing Quality", colour = "Trust Level", fill = "Trust Level") + scale_y_continuous(breaks = c(12, 15, 18, 21, 24, 27)) + guides(colour = "none", fill = "none"),
                              ce_KA_RC2_trust_plot + labs(y = "Electrical Assets", colour = "Trust Level", fill = "Trust Level") + scale_y_continuous(breaks = c(6, 9, 12, 15, 18, 21, 24, 27)) + guides(colour = "none", fill = "none"),
                              ce_KA_RC3_trust_plot + labs(y = "Roofing Quality", colour = "Trust Level", fill = "Trust Level") + scale_y_continuous(breaks = c(6, 9, 12, 15, 18, 21, 24, 27)) + guides(colour = "none", fill = "none"),
                              ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
ce_trust_plot

ggsave("Plots/ce_trust_plot.png", plot = ce_trust_plot,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# CONDITIONAL EFFECT OF PERCEIVED CLIMATE IMPACTS ON MSL

ce_ccimpact_plot <- wrap_plots(ce_AH_RC1_ccimpact_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(15, 25, 35, 45, 55, 65)),
                             ce_AH_RC2_ccimpact_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)),
                             ce_AH_RC3_ccimpact_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70)),
                             ce_KA_RC1_ccimpact_plot + labs(y = "Housing Quality") + scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18)) + guides(colour = "none", fill = "none"),
                             ce_KA_RC2_ccimpact_plot + labs(y = "Electrical Assets") + scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24)) + guides(colour = "none", fill = "none"),
                             ce_KA_RC3_ccimpact_plot + labs(y = "Roofing Quality") + scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24)) + guides(colour = "none", fill = "none"),
                             ncol = 3, guides = "collect") + 
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
ce_ccimpact_plot

ggsave("Plots/ce_ccimpact_plot.png", plot = ce_ccimpact_plot,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# WHISKER PLOTS

# Effect of year
whisker_year_plot <- (AH_whiskerplot_year | KA_whiskerplot_year) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1))
whisker_year_plot

ggsave("Plots/whisker_year_plot.png", plot = whisker_year_plot,
       width = 25, height = 15, units = "cm", 
       dpi = 500, bg = "white")

# Effect of social predictors
whisker_social_plot <- (AH_whiskerplot_all | KA_whiskerplot_all) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1))
whisker_social_plot

ggsave("Plots/whisker_social_plot.png", plot = whisker_social_plot,
       width = 32, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# FOOD INSECURITY: AHUS

AH_fi_combined <- (AH_plot_fi_year | 
                     (AH_plot_fi_age + 
                        scale_fill_manual(values = c("Younger" = "darkcyan", "Older" = "salmon2")) +
                        scale_colour_manual(values = c("Younger" = "darkcyan", "Older" = "salmon2"))) | 
                     AH_plot_fi_liv.pri) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
AH_fi_combined

ggsave("Plots/AH_fi_combined.png", plot = AH_fi_combined,
       width = 28, height = 10, units = "cm", 
       dpi = 500, bg = "white")

AH_fi_otherplots <- ((AH_whiskerplot_fi + labs(title = NULL)) | fi_panelAH_plot) +
  plot_layout(widths = c(7, 4)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1))
AH_fi_otherplots

ggsave("Plots/AH_fi_otherplots.png", plot = AH_fi_otherplots,
       width = 30, height = 10, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# PCA: Parallel Analysis

parallel_analysis_plots <- (AH_pa_plot | KA_pa_plot) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1))
parallel_analysis_plots

ggsave("Plots/parallel_analysis_plots.png", plot = parallel_analysis_plots,
       width = 22, height = 9, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# PCA: Parallel Analysis

varcontrib_plots <- wrap_plots(pc1_contribs_plot_AH, 
                               pc2_contribs_plot_AH,
                               pc3_contribs_plot_AH,
                               pc1_contribs_plot_KA,
                               pc2_contribs_plot_KA,
                               pc3_contribs_plot_KA,
                               ncol = 3, guides = "collect") + 
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = "topleft",
        plot.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold"),
        plot.background = element_rect(colour = "grey80", fill = NA, linewidth = 1),
        legend.position = "bottom")
varcontrib_plots

ggsave("Plots/varcontrib_plots.png", plot = varcontrib_plots,
       width = 28, height = 18, units = "cm", 
       dpi = 500, bg = "white")

#--------------------
# SAVE OUTPUTS
save.image("12-plots.RData")

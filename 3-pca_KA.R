# load("3-pca_KA.RData")

#--------------------
# OPEN PACKAGES
library(ggcorrplot)
library(factoextra)
library(dplyr)
library(tidyr)
library(psych)

#--------------------
# LOAD DATA
load("1-dataprep_mslvars.RData")

#--------------------
# CLEAN MSL DATA FOR KA 2016

# Deal with nulls
## Check for NAs
colSums(is.na(msl_KA2016))
## Remove variables where all obs are nulls
msl_KA2016 <- msl_KA2016 %>%
  select(where(~ !all(is.na(.))))
### Replace other NAs with 0s
msl_KA2016[is.na(msl_KA2016)] <- 0

# Deal with all 0s
### Check cols where all values are 0 (no ownership)
all_0 <- msl_KA2016 %>%
  select(where(~ all(. == 0)))
colnames(all_0)
ncol(all_0)
### Remove cols that are all 0
msl_KA2016 <- msl_KA2016 %>%
  select(where(~ !all(. == 0)))

# Check final cols and values
summary(msl_KA2016)
colnames(msl_KA2016)

# Count number of variables in 2016
ncol(msl_KA2016)

#--------------------
# DISTRIBUTION OF MSL ITEMS IN KA 2016

# Count number of respondents with each MSL item
msldist_count_KA2016 <- msl_KA2016 %>%
  summarise(across(everything(), ~ sum(.x > 0))) %>%
  pivot_longer(cols = everything(), names_to = "msl_item", values_to = "count")
head(msldist_count_KA2016)

## Calculate percentage of respondents that own each MSL item
n_respondents_KA2016 <- nrow(data_KA2016)
msldist_KA2016 <- msldist_count_KA2016 %>%
  mutate(percent = count / n_respondents_KA2016 * 100)
head(msldist_KA2016)

## Visualise distribution of MSL items
msldist_KA2016_plot <- ggplot(msldist_KA2016,
                              aes(x = reorder(msl_item, -percent), 
                                  y = percent)) +
  geom_col(fill = "steelblue") + 
  labs(title = "Distribution of MSL Items: Karkar 2001",
       x = "MSL Item", 
       y = "% Presence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.8),
        axis.ticks = element_line(colour = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_y_continuous(expand = c(0, 0))
msldist_KA2016_plot

# Exclude very low (<=5%) and very high (>=95%) prevalence variables
low_prevalence <- msldist_KA2016 %>% subset(percent <= 5) %>% 
  select(msl_item) 
low_prevalence

high_prevalence <- msldist_KA2016 %>% subset(percent >= 95) %>% 
  select(msl_item) 
high_prevalence

msl_KA2016 <- msl_KA2016 %>% 
  select(-c(any_of(low_prevalence$msl_item))) %>% 
  select(-c(any_of(high_prevalence$msl_item)))
colnames(msl_KA2016)
ncol(msl_KA2016)

#--------------------
# KMO MSA
# Discard items with individual MSA below 0.5 one at a time

## Run initial KMO 
KMO(msl_KA2016)

## Drop MSL.other.chickens (indiv MSA = 0.42) and rerun KMO.
msl_KA2016 <- msl_KA2016 %>% select(-MSL.other.chickens)
KMO(msl_KA2016)

## Drop MSL.appliances.mobile_phone (indiv MSA = 0.45) and rerun KMO.
msl_KA2016 <- msl_KA2016 %>% select(-MSL.appliances.mobile_phone)
KMO(msl_KA2016)

## Check final variables at end of KMO MSA
colnames(msl_KA2016)
ncol(msl_KA2016)

#--------------------
# BARLETT'S TEST OF SPHERICITY 
cortest.bartlett(msl_KA2016)
## p<0.001 = PCA is appropriate

#--------------------
# CORRELATION MATRIX

# Review correlation matrix = Some strong correlations are good, means PCA can compress things into principal components
corrmatrix_KA2016 <- cor(msl_KA2016)
corrmatrix_KA2016_plot <- ggcorrplot(corrmatrix_KA2016, 
                                     hc.order = TRUE,
                                     type = "lower",
                                     lab = TRUE)            
corrmatrix_KA2016_plot

# Count number of times items are at least moderately correlated (>=0.3) and weakly correlated (>= 0.2) with other items
diag(corrmatrix_KA2016) <- NA
n_corr_KA2016 <- data.frame(
  msl_item = colnames(corrmatrix_KA2016),
  n_moderately_correlated = colSums(abs(corrmatrix_KA2016) >= 0.3, na.rm = TRUE),
  n_weakly_correlated = colSums(abs(corrmatrix_KA2016) >= 0.2, na.rm = TRUE))
n_corr_KA2016

# Candidates for removal:
## MSL.appliances.radio = 0 moderate correlations, only 1 weak correlation
## MSL.transport.boat = 0 moderate correlations, only 2 weak correlations 
## MSL.other.pigs = 0 moderate correlations, only 1 weak correlation 

#--------------------
# APPLY THE INITAL UNROTATED PCA

# Use prcomp function from base R stats package

pca_KA2016_v1 <- prcomp(msl_KA2016,
                     center = TRUE,
                     scale. = TRUE)
summary(pca_KA2016_v1) 

#--------------------
# DECIDE ON NUMBER OF PRINCIPAL COMPONENTS TO RETAIN

# Horn (1965)’s parallel analysis following Glorfeld (1995) methodology
## Identify the number of components where the eigenvalues of the observed data exceeded the 95th percentile of eigenvalues from 1,000 randomly generated datasets of the same size 
set.seed(17)
fa.parallel(msl_KA2016, fa="pc", n.iter = 1000, quant = .95, error.bars = TRUE)
## Suggests keeping 3 components

#--------------------
# APPLY VARIMAX-ROTATED PCA WITH 3 COMPONENTS

# Use principal function from psych package
pca_KA2016_v2 <- principal(msl_KA2016, nfactors = 3, rotate = "varimax", scores = TRUE)
pca_KA2016_v2

#--------------------
# CHECK COMMUNALITY

pca_KA2016_v2$communality

# Candidates for removal:
## MSL.appliances.radio = 0 moderate correlations, only 1 weak correlation, very low communality (<0.2)
## MSL.transport.boat = 0 moderate correlations, only 2 weak correlations 
## MSL.other.pigs = 0 moderate correlations, only 1 weak correlation, very low communality (<0.2)

#--------------------
# CONTRIBUTION OF VARIABLES TO EACH PC
## https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html 
## Contributions to each PC calculated as the item’s squared loading divided by the sum of all squared loadings for that component
## The average expected contribution was defined as the contribution each item would make if all items contributed equally, calculated as the inverse of the total number of items included in each PCA. 

# Average expected contribution of individual variables to each PC
expected_contrib <- (1/ncol(msl_KA2016)) * 100
expected_contrib

# Calculate contributions to PC1
## Coordinates/loadings of variables for PC1
pc1_loadings <- pca_KA2016_v2$loadings[,1]
## Squared loadings of variables for PC1
pc1_cos2 <- pc1_loadings^2
## Contributions of variables to PC1
pc1_contribs <- (pc1_cos2 * 100) / sum(pc1_cos2)
## Dataframe of loadings, square loadings, and contributions
pc1_df <- data.frame(msl_item = names(pc1_contribs), 
                     contrib = pc1_contribs,
                     sign = ifelse(pc1_loadings > 0, "+", "-"),
                     loadings = pc1_loadings,
                     cos2 = pc1_cos2)
pc1_df

# Calculate contributions to PC2
## Coordinates/loadings of variables for PC2
pc2_loadings <- pca_KA2016_v2$loadings[,2]
## Squared loadings of variables for PC2
pc2_cos2 <- pc2_loadings^2
## Contributions of variables to PC2
pc2_contribs <- (pc2_cos2 * 100) / sum(pc2_cos2)
## Dataframe of loadings, square loadings, and contributions
pc2_df <- data.frame(msl_item = names(pc2_contribs), 
                     contrib = pc2_contribs,
                     sign = ifelse(pc2_loadings > 0, "+", "-"),
                     loadings = pc2_loadings,
                     cos2 = pc2_cos2)
pc2_df

# Calculate contributions to PC3
## Coordinates/loadings of variables for PC3
pc3_loadings <- pca_KA2016_v2$loadings[,3]
## Squared loadings of variables for PC3
pc3_cos2 <- pc3_loadings^2
## Contributions of variables to PC3
pc3_contribs <- (pc3_cos2 * 100) / sum(pc3_cos2)
## Dataframe of loadings, square loadings, and contributions
pc3_df <- data.frame(msl_item = names(pc3_contribs), 
                     contrib = pc3_contribs,
                     sign = ifelse(pc3_loadings > 0, "+", "-"),
                     loadings = pc3_loadings,
                     cos2 = pc3_cos2)
pc3_df

## Identify items contributing above expected contrib
pc1_highcontrib <- pc1_df %>% 
  filter(pc1_df$contrib > expected_contrib)
pc1_highcontrib
pc2_highcontrib <- pc2_df %>% 
  filter(pc2_df$contrib > expected_contrib)
pc2_highcontrib
pc3_highcontrib <- pc3_df %>% 
  filter(pc3_df$contrib > expected_contrib)
pc3_highcontrib

# Function to plot contributions
plot_contributions <- function(df, title){
  ggplot(df, 
         aes(x = reorder(msl_item, -contrib), 
             y = contrib, 
             fill = sign)) +
    geom_col() +
    geom_hline(yintercept = expected_contrib,
               linetype = "dashed",
               colour = "red") +
    labs(x = "MSL Item",
         y = "Contribution (%)",
         title = title,
         fill = "Direction") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(colour = "black", linewidth = 0.8),
          axis.ticks = element_line(colour = "black", linewidth = 0.8),
          plot.title = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")) +
    scale_y_continuous(expand = c(0, 0))}

# Plot contributions to PC1, PC2 and PC3
pc1_contribs_plot <- plot_contributions(pc1_df, "Karkar: Variable Contributions to PC1")
pc1_contribs_plot
pc2_contribs_plot <- plot_contributions(pc2_df, "Karkar: Variable Contributions to PC2")
pc2_contribs_plot
pc3_contribs_plot <- plot_contributions(pc3_df, "Karkar: Variable Contributions to PC3")
pc3_contribs_plot

# Check that all variables are contributing above expected contrib to at least 1 component
colnames(msl_KA2016)
pc1_highcontrib
pc2_highcontrib
pc3_highcontrib

# Candidates for removal:
## MSL.appliances.radio = 0 moderate correlations, only 1 weak correlation, very low communality (<0.2), not contributing above expected % contrib to any PC
## MSL.transport.boat = 0 moderate correlations, only 2 weak correlations, 
## MSL.other.pigs = 0 moderate correlations, only 1 weak correlation, very low communality (<0.2), not contributing above expected % contrib to any PC

# Remove variables with 0 moderate correlations, very low communalities, and/or not contributing above expected % contrib to any PC
msl_KA2016 <- msl_KA2016 %>% select(-c(MSL.appliances.radio, MSL.other.pigs, MSL.transport.boat))

#--------------------
# RE-RUN PCA AFTER REMOVING VARS

# Re-check KMO MSA = okay
KMO(msl_KA2016)

## Barlett's test of sphericity = okay 
cortest.bartlett(msl_KA2016)

### Re-check parallel analysis = still 3 PCs
set.seed(17)
fa.parallel(msl_KA2016, fa="pc", n.iter = 100, quant=.95, error.bars = TRUE)

### Re-run varimax-rotated PCA
pca_KA2016_v3 <- principal(msl_KA2016, nfactors = 3, rotate = "varimax", scores = TRUE)
pca_KA2016_v3

## Look at communality again
pca_KA2016_v3$communality

## Re-check correlation matrix
corrmatrix_KA2016_pcav3 <- cor(msl_KA2016)
corrmatrix_KA2016_plot_pcav3 <- ggcorrplot(corrmatrix_KA2016_pcav3, 
                                           hc.order = TRUE,
                                           type = "lower",
                                           lab = TRUE)            
corrmatrix_KA2016_plot_pcav3

diag(corrmatrix_KA2016_pcav3) <- NA
n_corr_KA2016_pcav3 <- data.frame(
  msl_item = colnames(corrmatrix_KA2016_pcav3),
  n_moderately_correlated = colSums(abs(corrmatrix_KA2016_pcav3) >= 0.3, na.rm = TRUE),
  n_weakly_correlated = colSums(abs(corrmatrix_KA2016_pcav3) >= 0.2, na.rm = TRUE))
n_corr_KA2016_pcav3

# Average expected contribution of individual variables to each PC
expected_contrib <- (1/ncol(msl_KA2016)) * 100
expected_contrib

# Calculate contributions to PC1
pc1_loadings <- pca_KA2016_v3$loadings[,1]
pc1_cos2 <- pc1_loadings^2
pc1_contribs <- (pc1_cos2 * 100) / sum(pc1_cos2)
pc1_df <- data.frame(msl_item = names(pc1_contribs), 
                     contrib = pc1_contribs,
                     sign = ifelse(pc1_loadings > 0, "+", "-"),
                     loadings = pc1_loadings,
                     cos2 = pc1_cos2)
pc1_highcontrib <- pc1_df %>% 
  filter(pc1_df$contrib > expected_contrib)
pc1_highcontrib
pc1_contribs_plot <- plot_contributions(pc1_df, "Karkar: Variable Contributions to PC1")
pc1_contribs_plot

# Calculate contributions to PC2
pc2_loadings <- pca_KA2016_v3$loadings[,2]
pc2_cos2 <- pc2_loadings^2
pc2_contribs <- (pc2_cos2 * 100) / sum(pc2_cos2)
pc2_df <- data.frame(msl_item = names(pc2_contribs), 
                     contrib = pc2_contribs,
                     sign = ifelse(pc2_loadings > 0, "+", "-"),
                     loadings = pc2_loadings,
                     cos2 = pc2_cos2)
pc2_highcontrib <- pc2_df %>% 
  filter(pc2_df$contrib > expected_contrib)
pc2_highcontrib
pc2_contribs_plot <- plot_contributions(pc2_df, "Karkar: Variable Contributions to PC2")
pc2_contribs_plot

# Calculate contributions to PC3
pc3_loadings <- pca_KA2016_v3$loadings[,3]
pc3_cos2 <- pc3_loadings^2
pc3_contribs <- (pc3_cos2 * 100) / sum(pc3_cos2)
pc3_df <- data.frame(msl_item = names(pc3_contribs), 
                     contrib = pc3_contribs,
                     sign = ifelse(pc3_loadings > 0, "+", "-"),
                     loadings = pc3_loadings,
                     cos2 = pc3_cos2)
pc3_highcontrib <- pc3_df %>% 
  filter(pc3_df$contrib > expected_contrib)
pc3_highcontrib
pc3_contribs_plot <- plot_contributions(pc3_df, "Karkar: Variable Contributions to PC3")
pc3_contribs_plot

# Check that all variables are contributing above expected contrib to at least 1 component
colnames(msl_KA2016)
pc1_highcontrib
pc2_highcontrib
pc3_highcontrib

# Candidates for removal = NONE

#--------------------
# LOOK AT STRUCTURE AND INTERNAL CONSISTENCY OF EACH PC

# Internal consistency of main items contributing to PC1
## PC1 (wall + floor) = MSL.floor.bamboo_palm (-), MSL.floor.plank_wood (+), MSL.wall.bamboo_thatch (-), MSL.wall.plank_wood (+)
pc1_highcontrib
pc1_contribs_plot
## Subset vars and make all items in same direction for analysis
pc1_vars <- msl_KA2016 %>% 
  select(MSL.floor.bamboo_palm, MSL.floor.plank_wood, MSL.wall.bamboo_thatch, MSL.wall.plank_wood) %>% 
  mutate(MSL.floor.bamboo_palm = 1 - MSL.floor.bamboo_palm,
         MSL.wall.bamboo_thatch = 1 - MSL.wall.bamboo_thatch)
## Results = great internal consistency (alpha = 0.93) and inter-item corrs (0.78)
pc1_alpha <- alpha(pc1_vars)
pc1_alpha

# Internal consistency of main items contributing to PC2
## PC2 (power and appliances) = MSL.appliances.dvd (+), MSL.appliances.tv (+), MSL.power.electricity_generator (+)
pc2_highcontrib
pc2_contribs_plot
## Subset vars and make all items in same direction for analysis
pc2_vars <- msl_KA2016 %>% 
  select(MSL.appliances.dvd, MSL.appliances.tv, MSL.power.electricity_generator)
## Results = good internal consistency (std alpha = 0.74) and inter-item corrs (0.51)
pc2_alpha <- alpha(pc2_vars)
pc2_alpha

# Internal consistency of main items contributing to PC3
## PC3 (roof + transport) = msl.trans.vehicle (+), MSL.roof.iron_sheet (+), MSL.roof.thatch (-)
pc3_highcontrib
pc3_contribs_plot
## Subset vars and make all items in same direction for analysis
pc3_vars <- msl_KA2016 %>% 
  select(msl.trans.vehicle, MSL.roof.iron_sheet, MSL.roof.thatch) %>%
  mutate(MSL.roof.thatch = 1 - MSL.roof.thatch)
head(pc3_vars)
## Results = good internal consistency (alpha = 0.72) and inter-item corrs (0.46)
pc3_alpha <- alpha(pc3_vars)
pc3_alpha

#--------------------
# SAVE FINAL LOADINGS
loadings_KA <- as.data.frame(unclass(pca_KA2016_v3$loadings))
write.csv(loadings_KA, "Plots/loadings_KA.csv")

#--------------------
# FINAL PCA STATS TO REPORT

# Final variables used in PCA
colnames(msl_KA2016)

# KMO MSA: Overall MSA = 0.66; indiv MSAs all above 0.5 -> data suitable for PCA
KMO(msl_KA2016)

# Barlett's test of sphericity: chisq = 517.6426; p.value = 1.365359e-81; df = 45 -> data suitable for PCA
cortest.bartlett(msl_KA2016)

# Other stats:
### 74% of variance explained: 34% by PC1, 21% by PC2, 20% by PC3
### Mean item complexity = 1.2
### The root mean square of the residuals (RMSR) is  0.06 = good, 0.08 considered "acceptable", 3 components is sufficient
### empirical chi square  22.61  with prob <  0.21, good, 3 components sufficient
### Fit based upon off diagonal values = 0.98 (good)
pca_KA2016_v3

# Chronbach's alphas and mean inter-item corrs:
pc1_alpha
pc2_alpha
pc3_alpha

# Average expected contribution to each PC
expected_contrib

# Variables contributing above average expected contribution to each PC
pc1_highcontrib
pc2_highcontrib
pc3_highcontrib

#--------------------
# FINAL CONTRIBUTIONS PLOTS

item_labels <- c(
  "MSL.appliances.dvd" = "DVD Player",
  "MSL.appliances.tv" = "Television",
  "MSL.roof.iron_sheet" = "Metal Roof",
  "MSL.roof.thatch" = "Thatch roof",
  "msl.trans.vehicle" = "Vehicle",
  "MSL.floor.bamboo_palm" = "Bamboo Floor",
  "MSL.floor.plank_wood" = "Wooden Floor",
  "MSL.wall.bamboo_thatch" = "Thatch Wall",
  "MSL.wall.plank_wood" = "Wooden Wall",
  "MSL.power.electricity_generator" = "Power")

pc1_contribs_plot_KA <- pc1_contribs_plot +
  scale_x_discrete(labels = item_labels) +
  labs(title = "Karkar (PC1)")
pc2_contribs_plot_KA <- pc2_contribs_plot +
  scale_x_discrete(labels = item_labels) +
  labs(title = "Karkar (PC2)")
pc3_contribs_plot_KA <- pc3_contribs_plot +
  scale_x_discrete(labels = item_labels) +
  labs(title = "Karkar (PC3)")

pc1_contribs_plot_KA
pc2_contribs_plot_KA
pc3_contribs_plot_KA

ggsave(filename = "Plots/KA_pc1_contribs_plot.png", plot = pc1_contribs_plot_KA,
       width = 16, height = 12, units = "cm", dpi = 500)
ggsave(filename = "Plots/KA_pc2_contribs_plot.png", plot = pc2_contribs_plot_KA,
       width = 16, height = 12, units = "cm", dpi = 500)
ggsave(filename = "Plots/KA_pc3_contribs_plot.png", plot = pc3_contribs_plot_KA,
       width = 16, height = 12, units = "cm", dpi = 500)

#--------------------
# FINAL PARALLEL ANALYSIS PLOT

set.seed(17)
pa <- fa.parallel(msl_KA2016, fa="pc", n.iter = 1000, quant = .95, plot = FALSE)

# dataframe of observed and sumulated eigenvalues for each component
pa_df <- data.frame(component = 1:length(pa$pc.values),
                    observed_eig = pa$pc.values,
                    simulated_eig = pa$pc.sim)

KA_pa_plot <- ggplot(pa_df, aes(x = component)) +
  geom_line(aes(y = observed_eig, colour = "Observed Data"), linewidth = 0.8) +
  geom_point(aes(y = observed_eig, colour = "Observed Data"), size = 2) +
  geom_line(aes(y = simulated_eig, colour = "Simulated Data"), linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(y = simulated_eig, colour = "Simulated Data"), size = 2) +
  scale_colour_manual(values = c("Observed Data" = "steelblue",
                                 "Simulated Data" = "darkorange3")) +
  scale_x_continuous(breaks = seq_along(pa$pc.values)) +
  labs(title = "Karkar (2016)",
       x = "Principal Component",
       y = "Eigenvalue",
       colour = "Data") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "inside", legend.justification = c(1, 1))
KA_pa_plot

ggsave(filename = "Plots/KA_parallel-analysis.png", plot = KA_pa_plot,
       width = 12, height = 10, units = "cm", dpi = 500)

#--------------------
# ADD PC SCORES INTO ORIGINAL DATA

# Just for the KA 2016 data:
pc_scores <- pca_KA2016_v3$scores
pc_scores

data_KA2016 <- cbind(data_KA2016, pc_scores)
data_KA2016

# For the entire KA 2016-2026 dataset:
## Filter the original data to only include the msl cols used in the PCA
msl_allKA <- data_KA %>% select(all_of(colnames(msl_KA2016)))
## Check for NAs in this data:
colSums(is.na(msl_allKA))
## Replace NAs with 0s
msl_allKA[is.na(msl_allKA)] <- 0
## Get PCs scores for all HHs across all years (2001 to 2026)
pc_scores_allKA <- predict(pca_KA2016_v3, data = msl_allKA, old.data = msl_KA2016)
head(pc_scores_allKA)
data_pcs_KA <- cbind(data_KA, pc_scores_allKA)
head(data_pcs_KA)
## Get PCs scores for all HHs across 2016-2026
data_pcs_KA_from2016 <- data_pcs_KA %>% filter(year >= 2016)
head(data_pcs_KA_from2016)

# Check predicted PCs match original PC scores for 2016
head(pc_scores)
predicted_pc_scores <- data_pcs_KA_from2016 %>%
  subset(year == 2016) %>%
  select(c(RC1, RC2, RC3))
head(predicted_pc_scores)

# Save as CSV
write.csv(data_pcs_KA_from2016, file="Data/msl_data/data_msl_KA_2016to2026.csv")

#--------------------
# DISTRIBUTION OF MSL ITEMS IN EACH YEAR

# Count of respondents that own each MSL item
msldist_counts <- data_KA %>%
  filter(year %in% c(2016,2026)) %>%
  select(year,
         MSL.appliances.dvd, MSL.appliances.tv, MSL.appliances.radio,
         MSL.roof.iron_sheet, MSL.roof.thatch, 
         MSL.floor.cement, MSL.floor.dirt_sand,
         MSL.wall.bamboo_thatch, MSL.wall.plank_wood,
         MSL.power.electricity_generator, MSL.cooking.modern) %>%
  group_by(year) %>% 
  summarise(across(everything(), ~ sum(.x > 0, na.rm = TRUE))) %>%
  pivot_longer(cols = -year, names_to = "msl_item", values_to = "count")
head(msldist_counts)

## Percentage of respondents that own each MSL item
n_respondents_KA2026 <- nrow(data_KA2026)
msldist_KA <- msldist_counts %>%
  mutate(percent = if_else(year == 2016,
                           count / n_respondents_KA2016 * 100,
                           if_else(year == 2026, 
                                   count / n_respondents_KA2026 * 100,
                                   0)))
                                  
msldist_KA

# Save as csv
write.csv(msldist_KA, "Plots/msldist_KA.csv")

#--------------------
# SAVE OUTPUTS
save.image("3-pca_KA.RData")
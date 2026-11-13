# load("2-pca_AH.RData")

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
# CLEAN MSL DATA FOR AH 2016

# Deal with nulls
## Check for NAs
colSums(is.na(msl_AH2016))
## Remove variables where all obs are nulls
msl_AH2016 <- msl_AH2016 %>%
  select(where(~ !all(is.na(.))))
### Replace other NAs with 0s
msl_AH2016[is.na(msl_AH2016)] <- 0

# Deal with all 0s
### Check cols where all values are 0 (no ownership)
all_0 <- msl_AH2016 %>%
  select(where(~ all(. == 0)))
colnames(all_0)
ncol(all_0)
### Remove cols that are all 0
msl_AH2016 <- msl_AH2016 %>%
  select(where(~ !all(. == 0)))

# Check final cols and values
summary(msl_AH2016)
colnames(msl_AH2016)

# Count number of variables in 2016
ncol(msl_AH2016)

#--------------------
# DISTRIBUTION OF MSL ITEMS IN AH 2016

# Count number of respondents with each MSL item
msldist_count_AH2016 <- msl_AH2016 %>%
  summarise(across(everything(), ~ sum(.x))) %>%
  pivot_longer(cols = everything(), names_to = "msl_item", values_to = "count")
head(msldist_count_AH2016)

# Calculate percentage of respondents that own each MSL item
n_respondents_AH2016 <- nrow(data_AH2016)
msldist_AH2016 <- msldist_count_AH2016 %>%
  mutate(percent = count / n_respondents_AH2016 * 100)
head(msldist_AH2016)

# Visualise distribution of MSL items
msldist_AH2016_plot <- ggplot(msldist_AH2016,
                              aes(x = reorder(msl_item, -percent), 
                                  y = percent)) +
  geom_col(fill = "steelblue") + 
  labs(title = "Distribution of MSL Items: Ahus 2016",
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
msldist_AH2016_plot

# Exclude very low (<=5%) and very high (>=95%) prevalence variables
low_prevalence <- msldist_AH2016 %>% subset(percent <= 5) %>% 
  select(msl_item) 
low_prevalence

high_prevalence <- msldist_AH2016 %>% subset(percent >= 95) %>% 
  select(msl_item) 
high_prevalence

msl_AH2016 <- msl_AH2016 %>% 
  select(-c(any_of(low_prevalence$msl_item))) %>% 
  select(-c(any_of(high_prevalence$msl_item)))
colnames(msl_AH2016)
ncol(msl_AH2016)

#--------------------
# KMO MSA
# Discard items with individual MSA below 0.5 one at a time

## Run initial KMO 
KMO(msl_AH2016)

## Drop MSL.wall.metal (indiv MSA = 0.19) and rerun KMO.
msl_AH2016 <- msl_AH2016 %>% select(-MSL.wall.metal)
KMO(msl_AH2016)

## Drop MSL.other.chickens (indiv MSA = 0.26) and rerun KMO.
msl_AH2016 <- msl_AH2016 %>% select(-MSL.other.chickens)
KMO(msl_AH2016)

## Drop MSL.floor.plank_wood (indiv MSA = 0.38) and rerun KMO.
msl_AH2016 <- msl_AH2016 %>% select(-MSL.floor.plank_wood)
KMO(msl_AH2016)

## Drop msl.trans.vehicle (indiv MSA = 0.40) and rerun KMO.
msl_AH2016 <- msl_AH2016 %>% select(-msl.trans.vehicle)
KMO(msl_AH2016)
## Overall MSA = 0.67; no items with indiv MSA below 0.5

## Check final variables at end of KMO MSA
colnames(msl_AH2016)
ncol(msl_AH2016)

#--------------------
## BARLETT'S TEST OF SPHERICITY 
cortest.bartlett(msl_AH2016)
### p<0.001 = PCA is appropriate

#--------------------
# CORRELATION MATRIX

# Review correlation matrix = Some strong correlations are good, means PCA can compress things into principal components
corrmatrix_AH2016 <- cor(msl_AH2016)
corrmatrix_AH2016_plot <- ggcorrplot(corrmatrix_AH2016, 
                                     hc.order = TRUE,
                                     type = "lower",
                                     lab = TRUE)            
corrmatrix_AH2016_plot

## Count number of times items are at least moderately correlated (>=0.3) and weakly correlated (>= 0.2) with other items
diag(corrmatrix_AH2016) <- NA
n_corr_AH2016 <- data.frame(
  n_moderately_correlated = colSums(abs(corrmatrix_AH2016) >= 0.3, na.rm = TRUE),
  n_weakly_correlated = colSums(abs(corrmatrix_AH2016) >= 0.2, na.rm = TRUE))
n_corr_AH2016

# Candidates for removal:
## MSL.appliances.water.ANY = 0 moderate correlations, only 2 weak correlations
## MSL.transport.boat = 0 moderate or weak correlations
## MSL.other.pigs = 0 moderate correlations, only 1 weak correlation

#--------------------
# APPLY THE INITAL UNROTATED PCA

# Use prcomp function from base R stats package
pca_AH2016_v1 <- prcomp(msl_AH2016,
                        center = TRUE,
                        scale. = TRUE)
summary(pca_AH2016_v1)

#--------------------
# DECIDE ON NUMBER OF PRINCIPAL COMPONENTS TO RETAIN

# Horn (1965)’s parallel analysis following Glorfeld (1995) methodology
## Identify the number of components where the eigenvalues of the observed data exceeded the 95th percentile of eigenvalues from 1,000 randomly generated datasets of the same size 
set.seed(17)
fa.parallel(msl_AH2016, fa="pc", n.iter = 1000, quant = .95, error.bars = TRUE)
## Suggests keeping 3 components

#--------------------
# APPLY VARIMAX-ROTATED PCA WITH 3 COMPONENTS

# Use principal function from psych package
pca_AH2016_v2 <- principal(msl_AH2016, nfactors = 3, rotate = "varimax", scores = TRUE)
pca_AH2016_v2

#--------------------
# CHECK COMMUNALITY

pca_AH2016_v2$communality

# Candidates for removal:
## MSL.appliances.water.ANY = 0 moderate correlations, only 2 weak correlations
## MSL.transport.boat = 0 moderate or weak correlations, very low communality (<0.2)
## MSL.other.pigs = 0 moderate correlations, only 1 weak correlation, very low communality (<0.2)

#--------------------
# CONTRIBUTION OF VARIABLES TO EACH PC
## https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html 
## Contributions to each PC calculated as the item’s squared loading divided by the sum of all squared loadings for that component
## The average expected contribution was defined as the contribution each item would make if all items contributed equally, calculated as the inverse of the total number of items included in each PCA. 

# Average expected contribution of individual variables to each PC
expected_contrib <- (1/ncol(msl_AH2016)) * 100
expected_contrib

# Calculate contributions to PC1
## Coordinates/loadings of variables for PC1
pc1_loadings <- pca_AH2016_v2$loadings[,1]
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
pc2_loadings <- pca_AH2016_v2$loadings[,2]
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
pc3_loadings <- pca_AH2016_v2$loadings[,3]
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
pc1_contribs_plot <- plot_contributions(pc1_df, "Ahus: Variable Contributions to PC1")
pc1_contribs_plot
pc2_contribs_plot <- plot_contributions(pc2_df, "Ahus: Variable Contributions to PC2")
pc2_contribs_plot
pc3_contribs_plot <- plot_contributions(pc3_df, "Ahus: Variable Contributions to PC3")
pc3_contribs_plot

# Check that all variables are contributing above expected contrib to at least 1 component
colnames(msl_AH2016)
pc1_highcontrib
pc2_highcontrib
pc3_highcontrib

# Candidates for removal:
## MSL.appliances.water.ANY = 0 moderate correlations, only 2 weak correlations
## MSL.transport.boat = 0 moderate or weak correlations, very low communality (<0.2), not contributing above expected % contrib to any PC
## MSL.other.pigs = 0 moderate correlations, only 1 weak correlation, very low communality (<0.2), not contributing above expected % contrib to any PC

# Remove variables with 0 moderate correlations, very low communalities, and/or not contributing above expected % contrib to any PC
msl_AH2016 <- msl_AH2016 %>% select(-c(MSL.transport.boat, MSL.other.pigs, MSL.appliances.water.ANY))

#--------------------
# RE-RUN PCA AFTER REMOVING VARS

# Re-check KMO MSA = okay
KMO(msl_AH2016)

## Barlett's test of sphericity = okay 
cortest.bartlett(msl_AH2016)

### Re-check parallel analysis = still 3 PCs
set.seed(17)
fa.parallel(msl_AH2016, fa="pc", n.iter = 100, quant=.95, error.bars = TRUE)

### Re-run varimax-rotated PCA
pca_AH2016_v3 <- principal(msl_AH2016, nfactors = 3, rotate = "varimax", scores = TRUE)
pca_AH2016_v3

## Look at communality again
pca_AH2016_v3$communality

## Re-check correlation matrix
corrmatrix_AH2016_pcav3 <- cor(msl_AH2016)
corrmatrix_AH2016_plot_pcav3 <- ggcorrplot(corrmatrix_AH2016_pcav3, 
                                           hc.order = TRUE,
                                           type = "lower",
                                           lab = TRUE)            
corrmatrix_AH2016_plot_pcav3

diag(corrmatrix_AH2016_pcav3) <- NA
n_corr_AH2016_pcav3 <- data.frame(
  msl_item = colnames(corrmatrix_AH2016_pcav3),
  n_moderately_correlated = colSums(abs(corrmatrix_AH2016_pcav3) >= 0.3, na.rm = TRUE),
  n_weakly_correlated = colSums(abs(corrmatrix_AH2016_pcav3) >= 0.2, na.rm = TRUE))
n_corr_AH2016_pcav3

# Average expected contribution of individual variables to each PC
expected_contrib <- (1/ncol(msl_AH2016)) * 100
expected_contrib

# Calculate contributions to PC1
pc1_loadings <- pca_AH2016_v3$loadings[,1]
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
pc1_contribs_plot <- plot_contributions(pc1_df, "Ahus: Variable Contributions to PC1")
pc1_contribs_plot

# Calculate contributions to PC2
pc2_loadings <- pca_AH2016_v3$loadings[,2]
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
pc2_contribs_plot <- plot_contributions(pc2_df, "Ahus: Variable Contributions to PC2")
pc2_contribs_plot

# Calculate contributions to PC3
pc3_loadings <- pca_AH2016_v3$loadings[,3]
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
pc3_contribs_plot <- plot_contributions(pc3_df, "Ahus: Variable Contributions to PC3")
pc3_contribs_plot

## Check that all variables are contributing above expected contrib to at least 1 component
colnames(msl_AH2016)
pc1_highcontrib
pc2_highcontrib
pc3_highcontrib

# Candidates for removal:
## MSL.appliances.mobile_phone = very low communality (<0.2), not contributing above expected % contrib to any PC

## Remove MSL.appliances.mobile_phone
msl_AH2016 <- msl_AH2016 %>% select(-c(MSL.appliances.mobile_phone))

#--------------------
# RE-RUN PCA AFTER REMOVING MOBILE PHONE

# Re-check KMO MSA = okay
KMO(msl_AH2016)

## Barlett's test of sphericity = okay 
cortest.bartlett(msl_AH2016)

### Re-check parallel analysis = still 3 PCs
set.seed(17)
fa.parallel(msl_AH2016, fa="pc", n.iter = 100, quant=.95, error.bars = TRUE)

### Re-run varimax-rotated PCA
pca_AH2016_v4 <- principal(msl_AH2016, nfactors = 3, rotate = "varimax", scores = TRUE)
pca_AH2016_v4

## Look at communality again
pca_AH2016_v4$communality

## Re-check correlation matrix
corrmatrix_AH2016_pcav4 <- cor(msl_AH2016)
corrmatrix_AH2016_plot_pcav4 <- ggcorrplot(corrmatrix_AH2016_pcav4, 
                                           hc.order = TRUE,
                                           type = "lower",
                                           lab = TRUE)            
corrmatrix_AH2016_plot_pcav4

diag(corrmatrix_AH2016_pcav4) <- NA
n_corr_AH2016_pcav4 <- data.frame(
  msl_item = colnames(corrmatrix_AH2016_pcav4),
  n_moderately_correlated = colSums(abs(corrmatrix_AH2016_pcav4) >= 0.3, na.rm = TRUE),
  n_weakly_correlated = colSums(abs(corrmatrix_AH2016_pcav4) >= 0.2, na.rm = TRUE))
n_corr_AH2016_pcav4

# Average expected contribution of individual variables to each PC
expected_contrib <- (1/ncol(msl_AH2016)) * 100
expected_contrib

# Calculate contributions to PC1
pc1_loadings <- pca_AH2016_v4$loadings[,1]
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
pc1_contribs_plot <- plot_contributions(pc1_df, "Ahus: Variable Contributions to PC1")
pc1_contribs_plot

# Calculate contributions to PC2
pc2_loadings <- pca_AH2016_v4$loadings[,2]
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
pc2_contribs_plot <- plot_contributions(pc2_df, "Ahus: Variable Contributions to PC2")
pc2_contribs_plot

# Calculate contributions to PC3
pc3_loadings <- pca_AH2016_v4$loadings[,3]
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
pc3_contribs_plot <- plot_contributions(pc3_df, "Ahus: Variable Contributions to PC3")
pc3_contribs_plot

## Check that all variables are contributing above expected contrib to at least 1 component
colnames(msl_AH2016)
pc1_highcontrib
pc2_highcontrib
pc3_highcontrib

# Candidates for removal = NONE

#--------------------
# LOOK AT STRUCTURE AND INTERNAL CONSISTENCY OF EACH PC

# Internal consistency of main items contributing to PC1
## PC1 (wall + floor + cooking) = MSL.floor.cement (+), MSL.floor.dirt_sand (-), MSL.wall.bamboo_thatch (-),  MSL.wall.plank_wood (+), MSL.cooking.modern (+)
pc1_highcontrib
pc1_contribs_plot
## Subset vars and make all items in same direction for analysis
pc1_vars <- msl_AH2016 %>% 
  select(MSL.floor.cement, MSL.floor.dirt_sand, MSL.wall.bamboo_thatch, MSL.wall.plank_wood, MSL.cooking.modern) %>% 
  mutate(MSL.wall.bamboo_thatch = 1 - MSL.wall.bamboo_thatch,
         MSL.floor.dirt_sand = 1 - MSL.floor.dirt_sand)
head(pc1_vars)
## Results = good internal consistency (alpha = 0.81) and inter-item corrs (average_r = 0.44)
pc1_alpha <- alpha(pc1_vars)
pc1_alpha

# Internal consistency of main items contributing to PC2
### PC2 (power + apppliances) = MSL.appliances.dvd (+), MSL.appliances.tv (+), MSL.appliances.radio (+),  MSL.power.electricity_generator (+)
pc2_highcontrib
pc2_contribs_plot
## Subset vars and make all items in same direction for analysis
pc2_vars <- msl_AH2016 %>% 
  select(MSL.appliances.dvd, MSL.appliances.tv, MSL.appliances.radio, MSL.power.electricity_generator) 
head(pc1_vars)
## Results = questionable internal consistency (alpha = 0.64) and good inter-item corrs (average_r = 0.33). Would increase to 0.69 by dropping MSL.appliances.radio.
pc2_alpha <- alpha(pc2_vars)
pc2_alpha

# Internal consistency of main items contributing to PC3
### PC3 (roof) = MSL.roof.iron_sheet (-), MSL.roof.thatch (+)
pc3_highcontrib
pc3_contribs_plot
## Subset vars and make all items in same direction for analysis
pc3_vars <- msl_AH2016 %>% 
  select(MSL.roof.iron_sheet, MSL.roof.thatch) %>%
  mutate(MSL.roof.iron_sheet = 1 - MSL.roof.iron_sheet)
head(pc3_vars)
## Results = good internal consistency (alpha = 0.93) and inter-item corrs (average_r = 0.87)
pc3_alpha <- alpha(pc3_vars)
pc3_alpha

#--------------------
# FIX DIRECTION OF LOADINGS ON PC3 AND SAVE FINAL LOADINGS

# PC3 is the only component where better MSL items load negatively and worse MSL items load positively. 
# Switch direction of loadings so low PC3 scores represent lower MSL
pc3_loadings <- -pc3_loadings

# Re-calculate contribution of vars to PC3
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
pc3_contribs_plot <- plot_contributions(pc3_df, "Ahus: Variable Contributions to PC3")
pc3_contribs_plot

# Save final loadings
loadings_AH <- as.data.frame(unclass(pca_AH2016_v4$loadings))
loadings_AH$RC3 <- -loadings_AH$RC3
loadings_AH
write.csv(loadings_AH, "Plots/loadings_AH.csv")

#--------------------
# FINAL PCA STATS TO REPORT

# Final variables used in PCA
colnames(msl_AH2016)

# KMO MSA: Overall MSA = 0.7; indiv MSAs all above 0.5 -> data suitable for PCA
KMO(msl_AH2016)

# Barlett's test of sphericity: chisq = 387.2594; p.value = 1.805762e-51; df = 55 -> data suitable for PCA
cortest.bartlett(msl_AH2016)

# Other stats:
### 63% of variance explained: 25% by PC1, 19% by PC2, 18% by PC3
### Mean item complexity = 1.2
### The root mean square of the residuals (RMSR) is  0.06 = good, 0.08 considered "acceptable", 3 components is sufficient
### empirical chi square  38.86  with prob <  0.038 = sig, not good, 3 components not sufficient
### Fit based upon off diagonal values = 0.96 (good!)
pca_AH2016_v4

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
  "MSL.appliances.radio" = "Radio",
  "MSL.roof.iron_sheet" = "Metal Roof",
  "MSL.roof.thatch" = "Thatch roof",
  "MSL.floor.cement" = "Cement Floor",
  "MSL.floor.dirt_sand" = "Dirt/Sand Floor",
  "MSL.wall.bamboo_thatch" = "Thatch Wall",
  "MSL.wall.plank_wood" = "Wooden Wall",
  "MSL.power.electricity_generator" = "Power",
  "MSL.cooking.modern" = "Modern Cooking")

pc1_contribs_plot_AH <- pc1_contribs_plot +
  scale_x_discrete(labels = item_labels) +
  labs(title = "Ahus (PC1)")
pc2_contribs_plot_AH <- pc2_contribs_plot +
  scale_x_discrete(labels = item_labels) +
  labs(title = "Ahus (PC2)")
pc3_contribs_plot_AH <- pc3_contribs_plot +
  scale_x_discrete(labels = item_labels) +
  labs(title = "Ahus (PC3)")

pc1_contribs_plot_AH
pc2_contribs_plot_AH
pc3_contribs_plot_AH

ggsave(filename = "Plots/AH_pc1_contribs_plot.png", plot = pc1_contribs_plot_AH,
       width = 16, height = 12, units = "cm", dpi = 500)
ggsave(filename = "Plots/AH_pc2_contribs_plot.png", plot = pc2_contribs_plot_AH,
       width = 16, height = 12, units = "cm", dpi = 500)
ggsave(filename = "Plots/AH_pc3_contribs_plot.png", plot = pc3_contribs_plot_AH,
       width = 16, height = 12, units = "cm", dpi = 500)

#--------------------
# FINAL PARALLEL ANALYSIS PLOT

set.seed(17)
pa <- fa.parallel(msl_AH2016, fa="pc", n.iter = 1000, quant = .95, plot = FALSE)

# dataframe of observed and sumulated eigenvalues for each component
pa_df <- data.frame(component = 1:length(pa$pc.values),
                    observed_eig = pa$pc.values,
                    simulated_eig = pa$pc.sim)

AH_pa_plot <- ggplot(pa_df, aes(x = component)) +
  geom_line(aes(y = observed_eig, colour = "Observed Data"), linewidth = 0.8) +
  geom_point(aes(y = observed_eig, colour = "Observed Data"), size = 2) +
  geom_line(aes(y = simulated_eig, colour = "Simulated Data"), linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(y = simulated_eig, colour = "Simulated Data"), size = 2) +
  scale_colour_manual(values = c("Observed Data" = "steelblue",
                                 "Simulated Data" = "darkorange3")) +
  scale_x_continuous(breaks = seq_along(pa$pc.values)) +
  labs(title = "Ahus (2016)",
       x = "Principal Component",
       y = "Eigenvalue",
       colour = "Data") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "inside", legend.justification = c(1, 1))
AH_pa_plot

ggsave(filename = "Plots/AH_parallel-analysis.png", plot = AH_pa_plot,
       width = 12, height = 10, units = "cm", dpi = 500)

#--------------------
# ADD PC SCORES INTO ORIGINAL DATA

# Just for the AH 2016 data:
pc_scores <- pca_AH2016_v4$scores
pc_scores

data_AH2016 <- cbind(data_AH2016, pc_scores)
data_AH2016

# For the entire AH 2016-2026 dataset:
## Filter the original data to only include the msl cols used in the PCA
msl_allAH <- data_AH %>% select(all_of(colnames(msl_AH2016)))
## Check for NAs in this data:
colSums(is.na(msl_allAH))
## Replace NAs with 0s
msl_allAH[is.na(msl_allAH)] <- 0
## Get PCs scores for all HHs across all years (2001 to 2026)
pc_scores_allAH <- predict(pca_AH2016_v4, data = msl_allAH, old.data = msl_AH2016)
head(pc_scores_allAH)
data_pcs_AH <- cbind(data_AH, pc_scores_allAH)
head(data_pcs_AH)
## Get PCs scores for all HHs across 2016-2026
data_pcs_AH_from2016 <- data_pcs_AH %>% filter(year >= 2016)
head(data_pcs_AH_from2016)

# Check predicted PCs match original PC scores for 2016
head(pc_scores)
predicted_pc_scores <- data_pcs_AH_from2016 %>%
  subset(year == 2016) %>%
  select(c(RC1, RC2, RC3))
head(predicted_pc_scores)

# Fix direction of PC3
data_AH2016 <- data_AH2016 %>%
  mutate(RC3 = -RC3)
data_pcs_AH <- data_pcs_AH %>%
  mutate(RC3 = -RC3)
data_pcs_AH_from2016 <- data_pcs_AH_from2016 %>%
  mutate(RC3 = -RC3)

# Save as CSV
write.csv(data_pcs_AH_from2016, file="Data/msl_data/data_msl_AH_2016to2026.csv")

#--------------------
# DISTRIBUTION OF MSL ITEMS IN EACH YEAR

# Count of respondents that own each MSL item
msldist_counts <- data_AH %>%
  filter(year %in% c(2016, 2018,2026)) %>%
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
n_respondents_AH2018 <- nrow(data_AH2018)
n_respondents_AH2026 <- nrow(data_AH2026)
msldist_AH <- msldist_counts %>%
  mutate(percent = if_else(year == 2016,
                           count / n_respondents_AH2016 * 100,
                           if_else(year == 2018, 
                                   count / n_respondents_AH2018 * 100,
                                   if_else(year == 2026,
                                           count / n_respondents_AH2026 * 100,
                                           0))))
msldist_AH

# Save as csv
write.csv(msldist_AH, "Plots/msldist_AH.csv")

#--------------------
# SAVE OUTPUTS
save.image("2-pca_AH.RData")
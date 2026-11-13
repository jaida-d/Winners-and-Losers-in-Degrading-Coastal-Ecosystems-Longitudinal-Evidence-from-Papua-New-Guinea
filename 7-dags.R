# load("7-dags.RData")

#--------------------
# OPEN PACKAGES
library(tidyverse)
library(dagitty) 
library(ggdag)

#--------------------
# DAG

dag_all <- 
  dagify(MSL ~
           year + #MSL in community may change over time
           gender + #directly affects access to resources, how much is "earned" from same livelihoods, etc.
           age + #older people had more opportunities to accumulate wealth/lose wealth, younger people potentially different livelihood opportunities
           migrantStatus + #migrating is a sig cost, directly affects access to material assets
           livelihoods.primary + #primary livelihood directly affects msl
           livelihoods.diversity + #diversity of livelihoods directly affects msl
           trust +
           decision_making +
           cc.impact +
           hh_size, #more people in HH can accumulate more wealth, but also consume more
         livelihoods.primary ~
           year + 
           gender + #livelihood differences for men v women
           hh_size + #hh size influences number of people participating in each livelihood and therefore which is considered most important
           age + #younger vs older hh prioritise different livelihoods
           # cc.impact = impacts from climate change influence livelihood type
           migrantStatus, #more difficult for migrants to access certain livelihoods
         livelihoods.diversity ~
           year +
           livelihoods.primary + #primary livelihood influences number of supplementary livelihoods required, but less likely for the reverse
           hh_size + #number of ppl in HH directly affects number of livelihoods
           gender + #affects access to diverse number of livelihoods (men may have access to more than women?)
           migrantStatus + #also likely to limit the types of livelihoods hhs can engage in
           age, #age direcly affects number of livelihoods engaged in, eg old people may be more specialise
         hh_size ~
           age + #older people more likely to have larger, more established hhs 
           migrantStatus, #non-migrants have larger hhs than migrants
         trust ~ 
           year +
           decision.index + 
           gender + #differences in social capital for women and men influences trust levels
           age + #differences in social capital for older and younger people, influences trust levels
           migrantStatus, 
         decision_making ~
           year +
           gender + 
           age +
           migrantStatus +
           hh_size, #larger households have more people to be involved in decisionmaking
         cc.impact ~
           year +
           gender + 
           age +
           hh_size +
           migrantStatus +
           livelihoods.primary)

exposures <- c("cc.impact",
               "gender",
               "age",
               "migrantStatus",
               "livelihoods.primary",
               "livelihoods.diversity",
               "year",
               "decision_making",
               "trust")
                        
for(var in exposures) {
  dag_plots <- ggdag_adjustment_set(dag_all,
                                    exposure = var, outcome = "MSL",
                                    text_size = 2) +
    theme_dag_blank() +
    ggtitle(paste("Exposure: ", var))
  print(dag_plots)}

#--------------------
# SAVE OUTPUTS
save.image("7-dags.RData")
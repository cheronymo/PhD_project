##########################################################
#                GlM MANUSCRIPT                         #
##########################################################


library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(speedglm)
library(gridExtra )
library(tidyverse)
library(imager)
library(png)
library(RCurl)
library(magick)
library(sciplot)
library(ggpubr)
library(broom)
library(purrr)
library(ggbiplot)
library(ecodist)
library("viridis")



#Load data______________________________________________________________________________________
#data with same number of generation and same duration (100 years) can be loaded. 
out_K_trait_bind <- readRDS( file = "./output/output_update/Outpout_K_selected_strategies_100gen.rds")
out_K_trait_bind$LHS <- "ExtSlow"
out_r_trait_bind <- readRDS( file = "./output/output_update/Outpout_r_selected_strategies_100gen.rds")
out_r_trait_bind$LHS <- "ExtFast"
out_rMix_trait_bind <- readRDS( file = "./output/output_update/Outpout_rMix_selected_strategies_100gen.rds")
out_rMix_trait_bind$LHS <- "Fast"
out_KMix_trait_bind <- readRDS( file = "./output/output_update/Outpout_Kmix_selected_strategies_100gen.rds")
out_KMix_trait_bind$LHS <- "Slow"

head(out_KMix_trait_bind)
unique(out_r_trait_bind$climate_scenario)

data_output_gentrend <- rbind(out_K_trait_bind, out_r_trait_bind, out_rMix_trait_bind, out_KMix_trait_bind)
data_output_gentrend$LHS <-factor(data_output_gentrend$LHS ,c("ExtFast", "Fast", "Slow", "ExtSlow"))
data_output_gentrend$scenario_demo <-factor(data_output_gentrend$scenario_demo ,c("Linear1","Linear2", "Sigmoid1", "Sigmoid2", "BellShape1", "BellShape2"))
data_output_gentrend$scenario_trait <-factor(data_output_gentrend$scenario_trait ,c("LinPo", "LinNeg", "SigPo", "SigNeg","BellShape"))
data_output_gentrend$climate_scenario <-factor(data_output_gentrend$climate_scenario,
                                               c("nothing", "Sdt0.2", "Sdt0.5", "Inc0.3", "Deac0.3","red",
                                                 "MeanIn", "MeanIn.0.3", "M.SdInc", "M.SdDeac", "M.In.Red"))
data_output_gentrend$log_GR[is.infinite(data_output_gentrend$log_GR)] <- NA

data_output_gentrend

#Names modification into the data________________________________________________________________
data_final_glm <- data_output_gentrend %>% 
  unite("scenario_tot", scenario_trait,scenario_demo, remove = FALSE) %>% 
  mutate(Sce_trait_x_demo = case_when(scenario_tot == "LinPo_Linear1" ~ 'S1', scenario_tot == "LinNeg_Linear1" ~ 'S2', scenario_tot == "SigPo_Linear1" ~ 'S3', scenario_tot == "SigNeg_Linear1" ~ 'S4',scenario_tot == "BellShape_Linear1" ~ 'S5',
                                      scenario_tot == "LinPo_Linear2" ~ 'S2', scenario_tot == "LinNeg_Linear2" ~ 'S1', scenario_tot == "SigPo_Linear2" ~ 'S4', scenario_tot == "SigNeg_Linear2" ~ 'S4',scenario_tot == "BellShape_Linear2" ~ 'S6',
                                      scenario_tot == "LinPo_Sigmoid1" ~ 'S3', scenario_tot == "LinNeg_Sigmoid1" ~ 'S4', scenario_tot == "SigPo_Sigmoid1" ~ 'S7', scenario_tot == "SigNeg_Sigmoid1" ~ 'S8',scenario_tot == "BellShape_Sigmoid1" ~ 'S9',
                                      scenario_tot == "LinPo_Sigmoid2" ~ 'S4', scenario_tot == "LinNeg_Sigmoid2" ~ 'S3', scenario_tot == "SigPo_Sigmoid2" ~ 'S8', scenario_tot == "SigNeg_Sigmoid2" ~ 'S7', scenario_tot == "BellShape_Sigmoid2" ~ 'S10',
                                      scenario_tot == "LinPo_BellShape1" ~ 'S11', scenario_tot == "LinNeg_BellShape2" ~ 'S12', scenario_tot == "SigPo_BellShape1" ~ 'S13', scenario_tot == "SigNeg_BellShape1" ~ 'S14',scenario_tot == "BellShape_BellShape1" ~ 'S15',
                                      scenario_tot == "LinPo_BellShape2" ~ 'S12', scenario_tot == "LinNeg_BellShape1" ~ 'S11', scenario_tot == "SigPo_BellShape2" ~ 'S14', scenario_tot == "SigNeg_BellShape2" ~ 'S13',scenario_tot == "BellShape_BellShape2" ~ '16'
  ))  %>% 
  mutate(Sce_trait_x_demo_shape = case_when(scenario_tot == "LinPo_Linear1" ~ 'Linear', scenario_tot == "LinNeg_Linear1" ~ 'Linear', scenario_tot == "SigPo_Linear1" ~ 'Sigmoid', scenario_tot == "SigNeg_Linear1" ~ 'Sigmoid',scenario_tot == "BellShape_Linear1" ~ 'BellShape',
                                            scenario_tot == "LinPo_Linear2" ~ 'Linear', scenario_tot == "LinNeg_Linear2" ~ 'Linear', scenario_tot == "SigPo_Linear2" ~ 'Sigmoid', scenario_tot == "SigNeg_Linear2" ~ 'Sigmoid',scenario_tot == "BellShape_Linear2" ~ 'BellShape',
                                            scenario_tot == "LinPo_Sigmoid1" ~ 'Sigmoid', scenario_tot == "LinNeg_Sigmoid1" ~ 'Sigmoid', scenario_tot == "SigPo_Sigmoid1" ~ 'Sigmoid', scenario_tot == "SigNeg_Sigmoid1" ~ 'Sigmoid',scenario_tot == "BellShape_Sigmoid1" ~ 'BellShape',
                                            scenario_tot == "LinPo_Sigmoid2" ~ 'Sigmoid', scenario_tot == "LinNeg_Sigmoid2" ~ 'Sigmoid', scenario_tot == "SigPo_Sigmoid2" ~ 'Sigmoid', scenario_tot == "SigNeg_Sigmoid2" ~ 'Sigmoid', scenario_tot == "BellShape_Sigmoid2" ~ 'BellShape',
                                            scenario_tot == "LinPo_BellShape1" ~ 'BellShape', scenario_tot == "LinNeg_BellShape1" ~ 'BellShape', scenario_tot == "SigPo_BellShape1" ~ 'BellShape', scenario_tot == "SigNeg_BellShape1" ~ 'BellShape',scenario_tot == "BellShape_BellShape1" ~ 'BellShape',
                                            scenario_tot == "LinPo_BellShape2" ~ 'BellShape', scenario_tot == "LinNeg_BellShape2" ~ 'BellShape', scenario_tot == "SigPo_BellShape2" ~ 'BellShape', scenario_tot == "SigNeg_BellShape2" ~ 'BellShape',scenario_tot == "BellShape_BellShape2" ~ 'BellShape'
  ))  %>% 
  mutate(Shape_trait = case_when(scenario_trait == "LinPo" ~ 'Linear', scenario_trait == "LinNeg" ~ 'Linear', 
                                 scenario_trait == "SigPo" ~ 'Sigmoid', scenario_trait == "SigNeg" ~ 'Sigmoid', 
                                 scenario_trait == "BellShape" ~ 'BellShape'
  )) %>% 
  mutate(Cat_climat = case_when(climate_scenario == "nothing" ~ 'No change in mean', climate_scenario == "Sdt0.2" ~ 'No change in mean', climate_scenario == "Sdt0.5" ~ 'No change in mean', climate_scenario == "Inc0.3" ~ 'No change in mean', climate_scenario == "Deac0.3" ~ 'No change in mean', climate_scenario == "red" ~ 'No change in mean',
                                climate_scenario == "MeanIn" ~ 'Change in mean', climate_scenario == "MeanIn.0.3" ~ 'Change in mean', climate_scenario == "M.SdInc" ~ 'Change in mean', climate_scenario == "M.SdDeac" ~ 'Change in mean', climate_scenario == "M.In.Red" ~ 'Change in mean' )) %>% 
  
  mutate(Shape_demo = case_when(scenario_demo == "Linear1" ~ 'Linear', scenario_demo == "Linear2" ~ 'Linear', 
                                scenario_demo == "Sigmoid1" ~ 'Sigmoid', scenario_demo == "Sigmoid2" ~ 'Sigmoid', 
                                scenario_demo == "BellShape1" ~ 'BellShape', scenario_demo == "BellShape2" ~ 'BellShape')) %>% 
  mutate(Climate = case_when(climate_scenario == "nothing" ~ "Baseline", climate_scenario == "Sdt0.2" ~ "LowFlu.", climate_scenario == "Sdt0.5" ~ "HighFlu.", climate_scenario == "Inc0.3" ~ "Inc.Flu.",
                             climate_scenario == "Deac0.3" ~ "Dec.Flu.", climate_scenario == "MeanIn" ~ "MeanInc.", climate_scenario == "MeanIn.0.3" ~ "MeanInc.Flu", climate_scenario == "M.SdInc" ~ "MeanInc.IncFlu",
                             climate_scenario == "M.SdDeac" ~ "MeanInc.DecFlu", climate_scenario == "M.In.Red" ~ "MeanInc.Auto", climate_scenario == "red" ~ "Temp.Auto"))



data_final_glm$Sce_trait_x_demo = factor(data_final_glm$Sce_trait_x_demo, levels=c('S1','S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12', 'S13', 'S14', 'S15', 'S16'))
data_final_glm$Sce_trait_x_demo_shape = factor(data_final_glm$Sce_trait_x_demo_shape, levels=c('Linear', 'Sigmoid','BellShape'))
data_final_glm$Shape_trait = factor(data_final_glm$Shape_trait, levels=c('Linear', 'Sigmoid','BellShape'))
data_final_glm$Shape_demo = factor(data_final_glm$Shape_demo, levels=c('Linear', 'Sigmoid','BellShape'))
data_final_glm$Cat_climat = factor(data_final_glm$Cat_climat, levels=c('No change in mean', 'Change in mean'))
data_final_glm$Climate = factor(data_final_glm$Climate, 
                            levels = c("Baseline", "LowFlu.", "HighFlu.", "Inc.Flu.", "Dec.Flu.", "Temp.Auto",
                                       "MeanInc.", "MeanInc.Flu", "MeanInc.IncFlu", "MeanInc.DecFlu", "MeanInc.Auto"))


#GLM_______________________________________________________________________________
library(lme4)

M1 <- glm(data_final_glm$CV ~ data_final_glm$Shape_trait)
summary(M1)
M2 <- glm(data_final_glm$CV ~  data_final_glm$Shape_demo)
summary(M2)

M3 <- glm(data_final_glm$CV ~ data_final_glm$Shape_trait + data_final_glm$Shape_demo)
summary(M3)

M4 <- glm(data_final_glm$EXT ~ data_final_glm$Shape_trait + data_final_glm$Shape_demo)
summary(M4)

#Relative importance_______________________________________________________________________________
library(randomForest)

set.seed(4543)
mtcars.rf <- randomForest(data_final_glm$CV ~ data_final_glm$Shape_trait + data_final_glm$Shape_demo, data = data_final_glm, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
importance(mtcars.rf)
importance(mtcars.rf, type=1)



 library(relaimpo)
#CV
rela_cv <- glm(CV ~ Shape_trait +  Shape_demo +
                   Climate + LHS, data = data_final_glm)
calc.relimp(rela_cv, rela=TRUE)

bootresults_cv <- boot.relimp(rela_cv, b=1000, rela=TRUE) 
ci_cv <- booteval.relimp(bootresults_cv, norank=T)
plot(ci_cv)



#EXT
rela_EX <- glm(EXT ~ Shape_trait +  Shape_demo +
                 Climate + LHS, data = data_final_glm)
calc.relimp(rela_EX, rela=TRUE)

bootresults_ex <- boot.relimp(rela_EX, b=1000, rela=TRUE) 
ci_ex <- booteval.relimp(bootresults_ex, norank=T)
plot(ci_ex)







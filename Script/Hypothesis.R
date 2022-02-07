##########################################################
#                PLOT MANUSCRIPT                         #
##########################################################
#This code contains the figure for the manuscript.


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
out_K_trait_bind <- readRDS( file = "./output/output_update/Outpout_K_selected_strategies_100years.rds")
out_K_trait_bind$LHS <- "ExtSlow"
out_r_trait_bind <- readRDS( file = "./output/output_update/Outpout_r_selected_strategies_100years.rds")
out_r_trait_bind$LHS <- "ExtFast"
out_rMix_trait_bind <- readRDS( file = "./output/output_update/Outpout_rMix_selected_strategies_100years.rds")
out_rMix_trait_bind$LHS <- "Fast"
out_KMix_trait_bind <- readRDS( file = "./output/output_update/Outpout_Kmix_selected_strategies_100years.rds")
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

#Plot summury (unreadable)
tot_plot_CV <- ggplot(data_output_gentrend, aes(x = climate_scenario , y = CV, color = LHS   ))+
  geom_boxplot() + 
  facet_grid(scenario_demo~scenario_trait   )+
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")+
  xlab("Climatic scenario") + 
  ylab("CV")
tot_plot_CV

tot_plot_EX <- ggplot(data_output_gentrend, aes(x = climate_scenario , y = EXT, color = LHS   ))+
  geom_boxplot() + 
  facet_grid(scenario_demo~scenario_trait   )+
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")+
  xlab("Climatic scenario") + 
  ylab("Extinction probability")
tot_plot_EX

pdf("./plot/Supplement/Resum.pdf", 15, 30, pointsize = 300)
ggarrange(tot_plot_CV, tot_plot_EX,  nrow = 2, ncol = 1)
dev.off()


#Names modification into the data________________________________________________________________
data_final <- data_output_gentrend %>% 
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



data_final$Sce_trait_x_demo = factor(data_final$Sce_trait_x_demo, levels=c('S1','S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12', 'S13', 'S14', 'S15', 'S16'))
data_final$Sce_trait_x_demo_shape = factor(data_final$Sce_trait_x_demo_shape, levels=c('Linear', 'Sigmoid','BellShape'))
data_final$Shape_trait = factor(data_final$Shape_trait, levels=c('Linear', 'Sigmoid','BellShape'))
data_final$Shape_demo = factor(data_final$Shape_demo, levels=c('Linear', 'Sigmoid','BellShape'))
data_final$Cat_climat = factor(data_final$Cat_climat, levels=c('No change in mean', 'Change in mean'))
data_final$Climate = factor(data_final$Climate, 
                            levels = c("Baseline", "LowFlu.", "HighFlu.", "Inc.Flu.", "Dec.Flu.", "Temp.Auto",
                                       "MeanInc.", "MeanInc.Flu", "MeanInc.IncFlu", "MeanInc.DecFlu", "MeanInc.Auto"))


#HISTOGRAM________________________________________________________________ 

Histo_GR <- data_final %>% 
  ggplot(aes(x = log_GR)) +
  geom_histogram(bins = 30) +
  xlab("Population growth rate") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Histo_cv <- data_final %>% 
  ggplot(aes(x = CV)) +
  geom_histogram(bins = 30) +
  xlab("Coefficient of variation in population size") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Histo_ext <- data_final %>% 
  ggplot(aes(x = EXT)) +
  geom_histogram(bins = 30) +
  xlab("Extinction probability") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Histo_extTime <- data_final %>% 
  ggplot(aes(x = EXT_time)) +
  geom_histogram(bins = 30) + 
  xlab("Extinction time") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

pdf("./plot/plot_hypothese/Histogram.pdf", 15, 15, pointsize = 300)
ggarrange(Histo_GR, Histo_cv, Histo_ext, Histo_extTime,  nrow = 2, ncol = 2)
dev.off()

##########################################################################################################
#Figures Paper ##########################################################################################################


# Figure Climatic scenarios_____________________________________________________________________________

#CV
plot_climate_CV <- data_final %>%  
  group_by(Shape_trait, Climate) %>% 
  summarise(mean_extin_prob = mean(EXT), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR  = sd(log_GR, na.rm = TRUE ),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(cv_upper = mean_cv + sd_cv, cv_low = mean_cv - sd_cv) %>% 
  mutate(cv_lower = if_else(cv_low < 0, 0, cv_low )) %>% 
  mutate(upper_GR = mean_logGR + sd_logGR, lower_GR = mean_logGR - sd_logGR) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_cv) + 
  geom_point( aes(x = Shape_trait , y = mean_cv, colour = Climate ),
             position = position_dodge(0.6))  + 
  geom_linerange(aes(x = Shape_trait , 
                                           ymin=  cv_lower, ymax = cv_upper, colour = Climate),
                 position = position_dodge(0.6), size = 0.9)  +
  scale_color_manual(values=c('#B22222','#DC143C', '#FF4500', '#FF8C00', '#B8860B', '#DAA520',
                              '#ADD8E6', '#87CEEB', '#00BFFF', '#6495ED', '#1E90FF'))+
  labs(x = "Reaction norm", y = "Coefficient of variation \n in population size", 
       colour = "Climatic \n scenario" ) +
  theme_classic()

pdf("./plot/Figure_manuscript/Figure_climate_CV_sameTime.pdf", 7, 5)
ggarrange(plot_climate_CV)
dev.off()


#GR
plot_climate_GR <- data_final %>%  
  group_by(Shape_trait, Climate) %>% 
  summarise(mean_extin_prob = mean(EXT), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR  = sd(log_GR, na.rm = TRUE ),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(cv_upper = mean_cv + sd_cv, cv_low = mean_cv - sd_cv) %>% 
  mutate(cv_lower = if_else(cv_low < 0, 0, cv_low )) %>% 
  mutate(upper_GR = mean_logGR + sd_logGR, lower_GR = mean_logGR - sd_logGR) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_logGR) + 
  geom_point( aes(x = Shape_trait , y = mean_logGR, colour = Climate ),
             position = position_dodge(0.6))  + 
  geom_linerange(aes(x = Shape_trait , 
                                           ymin=  lower_GR, ymax = upper_GR, colour = Climate),
                 position = position_dodge(0.6), size = 0.9)  +
  scale_color_manual(values=c('#B22222','#DC143C', '#FF4500', '#FF8C00', '#B8860B', '#DAA520',
                              '#ADD8E6', '#87CEEB', '#00BFFF', '#6495ED', '#1E90FF'))+
  labs(x = "Reaction norm", y = "Coefficient of variation \n in population size", 
       colour = "Climatic \n scenario" ) +
  theme_classic()

pdf("./plot/Figure_manuscript/Figure_climate_GR_sameTime.pdf", 7, 5)
ggarrange(plot_climate_GR)
dev.off()

#EXT
plot_climate_EXT <- data_final %>%  
  group_by(Shape_trait, Climate) %>% 
  summarise(mean_extin_prob = mean(EXT), SD_extin_prob = sd(EXT),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(EX_upper = mean_extin_prob + SD_extin_prob, EX_low = mean_extin_prob - SD_extin_prob) %>% 
  mutate(EX_lower = if_else(EX_low < 0, 0, EX_low ))  %>%  
  ggplot() + aes(x = Shape_trait , y = mean_extin_prob) + 
  geom_point( aes(x = Shape_trait , y = mean_extin_prob, colour = Climate ),
             position = position_dodge(0.6))  + 
  geom_linerange(aes(x = Shape_trait , ymin=  EX_lower, ymax = EX_upper, colour = Climate),
                 position = position_dodge(0.6), size = 0.9)  +
  scale_color_manual(values=c('#B22222','#DC143C', '#FF4500', '#FF8C00', '#B8860B', '#DAA520',
                              '#ADD8E6', '#87CEEB', '#00BFFF', '#6495ED', '#1E90FF'))+
  labs(x = "Reaction norm", y = "Extinction probability", 
       colour = "Climatic \n scenario" ) +
  theme_classic()

pdf("./plot/Figure_manuscript/Figure_climate_EXT_sameTime.pdf", 7, 5)
ggarrange(plot_climate_EXT)
dev.off()

# Figure Trait scenarios_____________________________________________________________________________
plot_trait_cv <- data_final %>%  
  group_by(Shape_trait, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) )  %>%  
  group_by(Shape_trait, Cat_climat) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_cv) + 
  facet_grid(~ Cat_climat) +  
  geom_point(size = 2) +
  geom_errorbar(aes(ymin= 0, ymax = mean_cv + sd_cv), size = 0.9)  + 
  labs(x = "Reaction norm", y = "Coefficient of variation \n in population size", 
       colour = "Trait- \n demographic \n rate shape" ) +
  theme_classic()

pdf("./plot/Figure_manuscript/Figure_ReactionNorm_CV_sameTime.pdf", 5, 5)
ggarrange(plot_trait_cv)
dev.off()

plot_trait_EXT <- data_final %>%  
  group_by(Shape_trait, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT) , SD_extin_prob = sd(EXT, na.rm = TRUE ),
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) )%>%  
  group_by(Shape_trait, Cat_climat) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_extin_prob) + 
  facet_grid(~ Cat_climat) +  
  geom_point(size = 2) +
  geom_errorbar(aes(ymin= 0, ymax = mean_extin_prob + SD_extin_prob), size = 0.9)  + 
  labs(x = "Reaction norm", y = "Extinction probability", 
       colour = "Trait- \n demographic \n rate shape" ) +
  theme_classic()

pdf("./plot/Figure_manuscript/Figure_ReactionNorm_EXT_sameTime.pdf", 5, 5)
ggarrange(plot_trait_EXT)
dev.off()

# Figure dem scenarios_____________________________________________________________________________
plot_dem_cv <- data_final %>%  
  group_by(Shape_trait, Cat_climat, Shape_demo) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(cv_upper = mean_cv + sd_cv, cv_low = mean_cv - sd_cv) %>% 
  mutate(cv_lower = if_else(cv_low < 0, 0, cv_low )) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_cv) + 
  facet_grid(~ Cat_climat) +  
  geom_point(aes(x = Shape_trait , y = mean_cv, colour = Shape_demo ),
             position = position_dodge(0.5)) +
  geom_linerange( aes(ymin= cv_lower, ymax = cv_upper, colour = Shape_demo),
                 position = position_dodge(0.5), size = 0.9)  +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x = "Reaction norm", y = "Coefficient of variation \n in population size", 
       colour = "Trait- \n demographic \n rate shape" ) +
  theme_classic()

plot_dem_EXT <- data_final %>%  
  group_by(Shape_trait, Cat_climat, Shape_demo) %>% 
  summarise(mean_extin_prob = mean(EXT), sd_extin_prob = sd(EXT),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(ex_uppe = mean_extin_prob + sd_extin_prob, ex_low = mean_extin_prob - sd_extin_prob) %>% 
  mutate(ex_lower = if_else(ex_low < 0, 0, ex_low )) %>% 
  mutate(ex_upper = if_else(ex_uppe > 1, 1, ex_uppe )) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_extin_prob) + 
  facet_grid(~ Cat_climat) +  
  geom_point( aes(x = Shape_trait , y = mean_extin_prob, colour = Shape_demo ),
              position = position_dodge(0.5)) +
  geom_linerange(aes(ymin= ex_lower, ymax = ex_upper, colour = Shape_demo),
                 position = position_dodge(0.5), size = 0.9)  +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(x = "Reaction norm", y = "Extinction probability", 
       colour = "Trait- \n demographic \n rate shape" ) +
  theme_classic()


# Figure LHS scenarios_____________________________________________________________________________
plot_lhs_cv <- data_final %>%  
  group_by(Shape_trait, Cat_climat, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(cv_upper = mean_cv + sd_cv, cv_low = mean_cv - sd_cv) %>% 
  mutate(cv_lower = if_else(cv_low < 0, 0, cv_low )) %>%
  ggplot() + aes(x = Shape_trait , y = mean_cv) + 
  facet_grid(~ Cat_climat) + 
  geom_point( aes(x = Shape_trait , y = mean_cv, colour = LHS ),
             position = position_dodge(0.5)) +
  geom_linerange( aes(ymin= cv_lower, ymax = cv_upper, colour = LHS),
                 position = position_dodge(0.5), size = 0.9)  +
  scale_color_manual(values=c("#009E73","#CC79A7", "#0072B2", "#D55E00"))+
  labs(x = "Reaction norm", y = "Coefficient of variation \n in population size", 
       colour = "Life History \n Strategies" ) +
  theme_classic()

plot_lhs_ext <- data_final %>%  
  group_by(Shape_trait, Cat_climat, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), SD_extin_prob = sd(EXT),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  mutate(EX_uppe = mean_extin_prob + SD_extin_prob, EX_low = mean_extin_prob - SD_extin_prob) %>% 
  mutate(EX_lower = if_else(EX_low < 0, 0, EX_low )) %>% 
  mutate(EX_upper = if_else(EX_uppe > 1, 1, EX_uppe )) %>%
  ggplot() + aes(x = Shape_trait , y = mean_extin_prob) + 
  facet_grid(~ Cat_climat) + 
  geom_point( aes(x = Shape_trait , y = mean_extin_prob, colour = LHS ),
              position = position_dodge(0.5)) +
  geom_linerange( aes(ymin= EX_lower, ymax = EX_upper, colour = LHS),
                  position = position_dodge(0.5), size = 0.9)  +
  scale_color_manual(values=c("#009E73","#CC79A7", "#0072B2", "#D55E00"))+
  labs(x = "Reaction norm", y = "Extinction probability", 
       colour = "Life History \n Strategies" ) +
  theme_classic()

pdf("./plot/Figure_manuscript/Figure_TraitDemo_sameTime.pdf", 11, 8)
ggarrange(plot_dem_cv, plot_lhs_cv,plot_dem_EXT, plot_lhs_ext, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
dev.off()






















#####################################################################################################################
#OTHER FIGURE NOT USEFULL
#LHS but not very intersteting if we use next figure
data_plotH1.3 <- data_final %>%  
  group_by(Shape_trait, Climate, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) 


plotH1.3 <- data_final %>%  
  group_by(Shape_trait, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_cv) + 
  facet_grid(~ Cat_climat) +  
  geom_point(size = 2) +
  geom_errorbar(aes(ymin= 0, ymax = mean_cv + sd_cv), size = 0.9)  + 
  geom_point(data = data_plotH1.3, aes(x = Shape_trait , y = mean_cv, colour = LHS ),
             position = position_dodge(0.5)) +
  geom_linerange(data = data_plotH1.3, aes(ymin= 0, ymax = mean_cv + sd_cv, colour = LHS),
                 position = position_dodge(0.5))  +
  #geom_jitter(data = data_plotH1.1, aes(x = Shape_trait , y = mean_cv, colour = Shape_demo ),
  #           position=position_jitter(0.2), cex = 2, shape = 18) + 
  labs(x = "Climate-trait relationship", y = "Coefficeint of variation \n in population size", 
       colour = "Trait- \n demographic \n rate shape" )

#DEMOGRAPHIC RATE
plotH1.1.2 <- data_final %>%  
  group_by(Shape_demo, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_demo , y = mean_cv) + 
  facet_grid(~ Cat_climat) + ylim(0, 300) +
  geom_crossbar(aes(ymin=mean_cv-sd_cv, ymax=mean_cv+sd_cv), width = 0.4,ymin = 0) +
  xlab("Trait-demographic rate \n relationship") + ylab("Coefficeint of variation \n in population size")


plotH1.3.2 <- data_final %>%  
  group_by(Shape_demo) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_demo , y = mean_cv) + 
  geom_crossbar(aes(ymin=mean_cv-sd_cv, ymax=mean_cv+sd_cv), width = 0.4 ,ymin = 0) + ylim(0, 300) +
  xlab("Trait-demographic rate \n relationship") + ylab("Coefficeint of variation \n in population size")

pdf("./plot/plot_hypothese/plot_H1.2_bis.pdf", 15, 7)
ggarrange(plotH1.3.2, plotH1.1.2, nrow = 1, labels = c("A", "B"))
dev.off()


#CLIMATE DEMOGRAPHIC RATE
plotH1.1.3 <- data_final %>%  
  group_by(Sce_trait_x_demo_shape, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Sce_trait_x_demo_shape , y = mean_cv) + 
  facet_grid(~ Cat_climat) + ylim(0, 300) +
  geom_crossbar(aes(ymin=mean_cv-sd_cv, ymax=mean_cv+sd_cv), width = 0.4,ymin = 0) +
  xlab("Climate-demographic rate \n relationship") + ylab("Coefficeint of variation \n in population size")


plotH1.3.3 <- data_final %>%  
  group_by(Sce_trait_x_demo_shape) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Sce_trait_x_demo_shape , y = mean_cv) + 
  geom_crossbar(aes(ymin=mean_cv-sd_cv, ymax=mean_cv+sd_cv), width = 0.4 ,ymin = 0) + ylim(0, 300) +
  xlab("Climate-demographic rate \n relationship") + ylab("Coefficeint of variation \n in population size")

pdf("./plot/plot_hypothese/plot_H1.3.pdf", 15, 7)
ggarrange(plotH1.3.3, plotH1.1.3, nrow = 1, labels = c("A", "B"))
dev.off()

pdf("./plot/plot_hypothese/plot_H1CV.pdf", 15, 15)
ggarrange(plotH1.3, plotH1.1, plotH1.3.2, plotH1.1.2, plotH1.3.3, plotH1.1.3, nrow = 3, ncol = 2,  labels = c("A", "B", "C", "D", "F", "G"))
dev.off()

#EXT____________________________________________________________
#TRAIT
plotH12.1 <- data_final %>%  
  group_by(Shape_trait, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_extin_prob) + 
  facet_grid(~ Cat_climat)  +
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Climate-trait relationship") + ylab("Extinction probability")


plotH12.3 <- data_final %>%  
  group_by(Shape_trait) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_extin_prob) + 
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Climate-trait relationship") + ylab("Extinction probability")


#DEMOGRAPHIC RATE
plotH12.1.2 <- data_final %>%  
  group_by(Shape_demo, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_demo , y = mean_extin_prob) + 
  facet_grid(~ Cat_climat)  +
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Trait-demographic rate \n relationship") + ylab("Extinction probability")


plotH12.3.2 <- data_final %>%  
  group_by(Shape_demo) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_demo , y = mean_extin_prob) + 
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Trait-demographic rate \n relationship") + ylab("Extinction probability")


#CLIMATE DEMOGRAPHIC RATE
plotH12.1.3 <- data_final %>%  
  group_by(Sce_trait_x_demo_shape, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Sce_trait_x_demo_shape , y = mean_extin_prob) + 
  facet_grid(~ Cat_climat)  +
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Climate-demographic rate \n relationship") + ylab("Extinction probability")


plotH12.3.3 <- data_final %>%  
  group_by(Sce_trait_x_demo_shape) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Sce_trait_x_demo_shape , y = mean_extin_prob) + 
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Climate-demographic rate \n relationship") + ylab("Extinction probability")


pdf("./plot/plot_hypothese/plot_H1EXT.pdf", 15, 15)
ggarrange(plotH12.3, plotH12.1, plotH12.3.2, plotH12.1.2, plotH12.3.3, plotH12.1.3, nrow = 3, ncol = 2,  labels = c("A", "B", "C", "D", "F", "G"))
dev.off()


#GR____________________________________________________________
#TRAIT
plotH13.1 <- data_final %>%  
  group_by(Shape_trait, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_logGR) + 
  facet_grid(~ Cat_climat)  +
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Climate-trait relationship") + ylab("Population growth rate")


plotH13.3 <- data_final %>%  
  group_by(Shape_trait) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_trait , y = mean_logGR) + 
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Climate-trait relationship") + ylab("Population growth rate")


#DEMOGRAPHIC RATE
plotH13.1.2 <- data_final %>%  
  group_by(Shape_demo, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_demo , y = mean_logGR) + 
  facet_grid(~ Cat_climat)  +
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Trait-demographic rate \n relationship") + ylab("Population growth rate")


plotH13.3.2 <- data_final %>%  
  group_by(Shape_demo) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Shape_demo , y = mean_logGR) + 
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Trait-demographic rate \n relationship") + ylab("Population growth rate")


#CLIMATE DEMOGRAPHIC RATE
plotH13.1.3 <- data_final %>%  
  group_by(Sce_trait_x_demo_shape, Cat_climat) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Sce_trait_x_demo_shape , y = mean_logGR) + 
  facet_grid(~ Cat_climat)  +
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Climate-demographic rate \n relationship") + ylab("Population growth rate")


plotH13.3.3 <- data_final %>%  
  group_by(Sce_trait_x_demo_shape) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE) ,sd_extin_prob = sd(EXT, na.rm = TRUE), 
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE),
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = Sce_trait_x_demo_shape , y = mean_logGR) + 
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Climate-demographic rate \n relationship") + ylab("Population growth rate")


pdf("./plot/plot_hypothese/plot_H1GR.pdf", 15, 15)
ggarrange(plotH13.3, plotH13.1, plotH13.3.2, plotH13.1.2, plotH13.3.3, plotH13.1.3, nrow = 3, ncol = 2,  labels = c("A", "B", "C", "D", "F", "G"))
dev.off()


#Hythese 2________________________________________________________________

plotH2.1 <- data_final %>% 
  #filter(Cat_climat == 'Climatic mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S', Sce_trait_x_demo == 'S2' ~ 'F',Sce_trait_x_demo == 'S3' ~ 'S',
                               Sce_trait_x_demo == 'S4' ~ 'F', Sce_trait_x_demo == 'S5' ~ 'S', Sce_trait_x_demo == 'S6' ~ 'F',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'F', Sce_trait_x_demo == 'S12' ~ 'S',
                               Sce_trait_x_demo == 'S13' ~ 'F', Sce_trait_x_demo == 'S14' ~ 'S', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS, climate_scenario) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_logGR) + 
  facet_grid(climate_scenario~ LHS) +
  geom_point(stat = "identity") +
  #geom_pointrange(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR)) +
  xlab("Trade of between survival and fecundity") + ylab("Population growth rate")

#___________________________________________________________________________________
#data for dot______
data_H2_dot_nofilter_clim <- data_final %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S+', Sce_trait_x_demo == 'S2' ~ 'S-',Sce_trait_x_demo == 'S3' ~ 'S+',
                               Sce_trait_x_demo == 'S4' ~ 'S-', Sce_trait_x_demo == 'S5' ~ 'S+', Sce_trait_x_demo == 'S6' ~ 'S-',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'S-', Sce_trait_x_demo == 'S12' ~ 'S+',
                               Sce_trait_x_demo == 'S13' ~ 'S-', Sce_trait_x_demo == 'S14' ~ 'S+', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS, Climate) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE), sd_extin_prob = sd(EXT, na.rm = TRUE),
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) )  


data_H2_dot_filter_shape <- data_final %>% 
  filter(Cat_climat == 'Change in mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S+', Sce_trait_x_demo == 'S2' ~ 'S-',Sce_trait_x_demo == 'S3' ~ 'S+',
                               Sce_trait_x_demo == 'S4' ~ 'S-', Sce_trait_x_demo == 'S5' ~ 'S+', Sce_trait_x_demo == 'S6' ~ 'S-',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'S-', Sce_trait_x_demo == 'S12' ~ 'S+',
                               Sce_trait_x_demo == 'S13' ~ 'S-', Sce_trait_x_demo == 'S14' ~ 'S+', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS, Sce_trait_x_demo_shape) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE), sd_extin_prob = sd(EXT, na.rm = TRUE),
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) )  


#plot________________
plotH2.2 <- data_final %>% 
  #filter(Cat_climat == 'Climatic mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S+', Sce_trait_x_demo == 'S2' ~ 'S-',Sce_trait_x_demo == 'S3' ~ 'S+',
                               Sce_trait_x_demo == 'S4' ~ 'S-', Sce_trait_x_demo == 'S5' ~ 'S+', Sce_trait_x_demo == 'S6' ~ 'S-',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'S-', Sce_trait_x_demo == 'S12' ~ 'S+',
                               Sce_trait_x_demo == 'S13' ~ 'S-', Sce_trait_x_demo == 'S14' ~ 'S+', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_logGR) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin= mean_logGR - sd_logGR, ymax = mean_logGR + sd_logGR), size = 0.9) +
  facet_grid(~ LHS) +
  geom_jitter(data = data_H2_dot_nofilter_clim, aes(x = trade_off , y = mean_logGR, colour = Climate ),
              position=position_jitter(0.2), cex = 2, shape = 18)  + 
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(x = "Trade off", y = "Population growth rate", color = "Climatic \n scenario")


plotH2.3 <- data_final %>% 
  filter(Cat_climat == 'Change in mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S+', Sce_trait_x_demo == 'S2' ~ 'S-',Sce_trait_x_demo == 'S3' ~ 'S+',
                               Sce_trait_x_demo == 'S4' ~ 'S-', Sce_trait_x_demo == 'S5' ~ 'S+', Sce_trait_x_demo == 'S6' ~ 'S-',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'S-', Sce_trait_x_demo == 'S12' ~ 'S+',
                               Sce_trait_x_demo == 'S13' ~ 'S-', Sce_trait_x_demo == 'S14' ~ 'S+', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_logGR) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin= mean_logGR - sd_logGR, ymax = mean_logGR + sd_logGR), size =0.9) +
  facet_grid(~ LHS) +
  geom_point(data = data_H2_dot_filter_shape, aes(x = trade_off , y = mean_logGR, colour = Sce_trait_x_demo_shape ),
             position=position_dodge(0.4))  + 
  geom_linerange(data = data_H2_dot_filter_shape, aes(x = trade_off, ymin= mean_logGR - sd_logGR, ymax = mean_logGR + sd_logGR, 
                                                      colour = Sce_trait_x_demo_shape), position=position_dodge(0.4)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(x = "Trade off", y = "Population growth rate", color = "Total Shape")


plotH2.4 <- data_final %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S+', Sce_trait_x_demo == 'S2' ~ 'S-',Sce_trait_x_demo == 'S3' ~ 'S+',
                               Sce_trait_x_demo == 'S4' ~ 'S-', Sce_trait_x_demo == 'S5' ~ 'S+', Sce_trait_x_demo == 'S6' ~ 'S-',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'S-', Sce_trait_x_demo == 'S12' ~ 'S+',
                               Sce_trait_x_demo == 'S13' ~ 'S-', Sce_trait_x_demo == 'S14' ~ 'S+', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE), sd_extin_prob = sd(EXT, na.rm = TRUE),
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_extin_prob) + 
  geom_point(size = 2) +
  facet_grid(~ LHS) +
  geom_errorbar(aes(ymin= mean_extin_prob - sd_extin_prob, ymax = mean_extin_prob + sd_extin_prob), size = 0.9) +
  geom_jitter(data = data_H2_dot_nofilter_clim, aes(x = trade_off , y = mean_extin_prob, colour = Climate ),
              position=position_jitter(0.2), cex = 2, shape = 18)  + 
  labs(x = "Trade off", y = "Extinction probability", color = "Climatic \n scenario")

plotH2.5 <- data_final %>% 
  filter(Cat_climat == 'Change in mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S+', Sce_trait_x_demo == 'S2' ~ 'S-',Sce_trait_x_demo == 'S3' ~ 'S+',
                               Sce_trait_x_demo == 'S4' ~ 'S-', Sce_trait_x_demo == 'S5' ~ 'S+', Sce_trait_x_demo == 'S6' ~ 'S-',
                               Sce_trait_x_demo == 'S7' ~ 'N', Sce_trait_x_demo == 'S8' ~ 'N', Sce_trait_x_demo == 'S9' ~ 'N',
                               Sce_trait_x_demo == 'S10' ~ 'N', Sce_trait_x_demo == 'S11' ~ 'S-', Sce_trait_x_demo == 'S12' ~ 'S+',
                               Sce_trait_x_demo == 'S13' ~ 'S-', Sce_trait_x_demo == 'S14' ~ 'S+', Sce_trait_x_demo == 'S15' ~ 'N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE), sd_extin_prob = sd(EXT, na.rm = TRUE),
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_extin_prob) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin= mean_extin_prob - sd_extin_prob, ymax = mean_extin_prob + sd_extin_prob), size = 0.9) +
  facet_grid(~ LHS) +
  geom_point(data = data_H2_dot_filter_shape, aes(x = trade_off , y = mean_extin_prob, colour = Sce_trait_x_demo_shape ),
             position=position_dodge(0.6))  + 
  geom_linerange(data = data_H2_dot_filter_shape, aes(x = trade_off, ymin= mean_extin_prob - sd_extin_prob, ymax = mean_extin_prob + sd_extin_prob, 
                                                      colour = Sce_trait_x_demo_shape), position=position_dodge(0.6)) +
  labs(x = "Trade off", y = "Extinction probability", color = "Total Shape")


pdf("./plot/plot_hypothese/plot_H2_Results_errorBar_bis.pdf", 15, 15)
ggarrange(plotH2.2, plotH2.3, plotH2.4, plotH2.5, nrow = 2, ncol = 2, labels = c("A. \n All ", "B. \n Trend ", "C. \n All ", "D.\n Trend "))
dev.off()

#Same with split categories______________________________

plotH2.2.2 <- data_final %>% 
  #filter(Cat_climat == 'Climatic mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S', Sce_trait_x_demo == 'S2' ~ 'F',Sce_trait_x_demo == 'S3' ~ 'S',
                               Sce_trait_x_demo == 'S4' ~ 'F', Sce_trait_x_demo == 'S5' ~ 'S', Sce_trait_x_demo == 'S6' ~ 'F',
                               Sce_trait_x_demo == 'S7' ~ 'N_P', Sce_trait_x_demo == 'S8' ~ 'N_P', Sce_trait_x_demo == 'S9' ~ 'N_N',
                               Sce_trait_x_demo == 'S10' ~ 'N_P', Sce_trait_x_demo == 'S11' ~ 'F', Sce_trait_x_demo == 'S12' ~ 'S',
                               Sce_trait_x_demo == 'S13' ~ 'F', Sce_trait_x_demo == 'S14' ~ 'S', Sce_trait_x_demo == 'S15' ~ 'N_N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_logGR) + 
  facet_grid(~ LHS) +
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Trade of between survival and fecundity") + ylab("Population growth rate")



plotH2.3.2 <- data_final %>% 
  filter(Cat_climat == 'Change in mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S', Sce_trait_x_demo == 'S2' ~ 'F',Sce_trait_x_demo == 'S3' ~ 'S',
                               Sce_trait_x_demo == 'S4' ~ 'F', Sce_trait_x_demo == 'S5' ~ 'S', Sce_trait_x_demo == 'S6' ~ 'F',
                               Sce_trait_x_demo == 'S7' ~ 'N_P', Sce_trait_x_demo == 'S8' ~ 'N_P', Sce_trait_x_demo == 'S9' ~ 'N_N',
                               Sce_trait_x_demo == 'S10' ~ 'N_P', Sce_trait_x_demo == 'S11' ~ 'F', Sce_trait_x_demo == 'S12' ~ 'S',
                               Sce_trait_x_demo == 'S13' ~ 'F', Sce_trait_x_demo == 'S14' ~ 'S', Sce_trait_x_demo == 'S15' ~ 'N_N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_logGR) + 
  facet_grid(~ LHS) +
  geom_crossbar(aes(ymin=mean_logGR-sd_logGR, ymax=mean_logGR+sd_logGR), width = 0.4) +
  xlab("Trade of between survival and fecundity") + ylab("Population growth rate")

plotH2.4.2 <- data_final %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S', Sce_trait_x_demo == 'S2' ~ 'F',Sce_trait_x_demo == 'S3' ~ 'S',
                               Sce_trait_x_demo == 'S4' ~ 'F', Sce_trait_x_demo == 'S5' ~ 'S', Sce_trait_x_demo == 'S6' ~ 'F',
                               Sce_trait_x_demo == 'S7' ~ 'N_P', Sce_trait_x_demo == 'S8' ~ 'N_P', Sce_trait_x_demo == 'S9' ~ 'N_N',
                               Sce_trait_x_demo == 'S10' ~ 'N_P', Sce_trait_x_demo == 'S11' ~ 'F', Sce_trait_x_demo == 'S12' ~ 'S',
                               Sce_trait_x_demo == 'S13' ~ 'F', Sce_trait_x_demo == 'S14' ~ 'S', Sce_trait_x_demo == 'S15' ~ 'N_N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE), sd_extin_prob = sd(EXT, na.rm = TRUE),
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_extin_prob) + 
  facet_grid(~ LHS) +
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Trade of between survival and fecundity") + ylab("Extinction probability")

plotH2.5.2 <- data_final %>% 
  filter(Cat_climat == 'Change in mean')  %>% 
  mutate(trade_off = case_when(Sce_trait_x_demo == 'S1' ~ 'S', Sce_trait_x_demo == 'S2' ~ 'F',Sce_trait_x_demo == 'S3' ~ 'S',
                               Sce_trait_x_demo == 'S4' ~ 'F', Sce_trait_x_demo == 'S5' ~ 'S', Sce_trait_x_demo == 'S6' ~ 'F',
                               Sce_trait_x_demo == 'S7' ~ 'N_P', Sce_trait_x_demo == 'S8' ~ 'N_P', Sce_trait_x_demo == 'S9' ~ 'N_N',
                               Sce_trait_x_demo == 'S10' ~ 'N_P', Sce_trait_x_demo == 'S11' ~ 'F', Sce_trait_x_demo == 'S12' ~ 'S',
                               Sce_trait_x_demo == 'S13' ~ 'F', Sce_trait_x_demo == 'S14' ~ 'S', Sce_trait_x_demo == 'S15' ~ 'N_N')) %>% 
  group_by(trade_off, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT, na.rm = TRUE), sd_extin_prob = sd(EXT, na.rm = TRUE),
            mean_logGR = mean(log_GR, na.rm = TRUE), sd_logGR = sd(log_GR, na.rm = TRUE), 
            mean_SDT = mean(SDT, na.rm = TRUE), mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) %>% 
  ggplot() + aes(x = trade_off , y = mean_extin_prob) + 
  facet_grid(~ LHS) +
  geom_crossbar(aes(ymin=mean_extin_prob-sd_extin_prob, ymax=mean_extin_prob+sd_extin_prob), width = 0.4) +
  xlab("Trade of between survival and fecundity") + ylab("Extinction probability")


pdf("./plot/plot_hypothese/plot_H2_Split.pdf", 15, 15)
ggarrange(plotH2.2.2, plotH2.3.2, plotH2.4.2, plotH2.5.2, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
dev.off()



#Hythese 3________________________________________________________________

data_plotH3 <- data_final %>%  
  group_by( Cat_climat, Sce_trait_x_demo, LHS) %>% 
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE),
            Text_M = mean(EXT_time, na.rm = TRUE), 
            mean_cv = mean(CV, na.rm = TRUE ), sd_cv = sd(CV, na.rm = TRUE ) ) 


plotH3.1 <- data_final %>% 
  group_by(LHS, Cat_climat) %>% 
  summarise(ext_M = mean(EXT, na.rm = TRUE), ext_sd = sd(EXT, na.rm = TRUE)) %>%
  ggplot() + aes(x = LHS, y = ext_M) +
  facet_grid(~ Cat_climat) +
  geom_point() + 
  geom_errorbar(aes(ymin = ext_M - ext_sd , ymax = ext_M + ext_sd)) +
  geom_jitter(data = data_plotH3, aes(x = LHS , y = mean_extin_prob, colour = Sce_trait_x_demo ),
              position=position_jitter(0.2), cex = 2, shape = 18) + 
  labs(x = "Life history strategies", y = "Extinction probability", color = "Shape")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



plotH3.2 <- data_final %>% 
  group_by(LHS, Cat_climat) %>% 
  summarise(ext_M = mean(EXT, na.rm = TRUE), ext_sd = sd(EXT, na.rm = TRUE),
            Text_M = mean(EXT_time, na.rm = TRUE), Text_sd = sd(EXT_time, na.rm = TRUE)) %>%
  ggplot() + aes(x = LHS, y = Text_M) +
  facet_grid(~ Cat_climat) +
  geom_point() +
  geom_errorbar(aes(ymin= Text_M - Text_sd, ymax = Text_M + Text_sd)) +
  geom_jitter(data = data_plotH3, aes(x = LHS , y = Text_M, colour = Sce_trait_x_demo ),
              position=position_jitter(0.2), cex = 2, shape = 18) + 
  labs(x = "Life history strategies", y = "Extinction time", color = "Shape") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plotH3.3 <- data_final %>% 
  group_by(LHS, Cat_climat) %>% 
  summarise(ext_M = mean(EXT, na.rm = TRUE), ext_sd = sd(EXT, na.rm = TRUE),
            GR_M = mean(log_GR, na.rm = TRUE), GR_sd = sd(log_GR, na.rm = TRUE)) %>%
  ggplot() + aes(x = LHS, y = GR_M) +
  facet_grid(~ Cat_climat) +
  geom_point() +
  geom_errorbar(aes(ymin= GR_M - GR_sd, ymax = GR_M + GR_sd)) +
  geom_jitter(data = data_plotH3, aes(x = LHS , y = mean_logGR, colour = Sce_trait_x_demo ),
              position=position_jitter(0.2), cex = 2, shape = 18) + 
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(x = "Life history strategies", y = "Population growth rate", color = "Shape") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


pdf("./plot/plot_hypothese/plot_H3_Result_bis.pdf", 15, 5)
ggarrange(plotH3.1, plotH3.2, plotH3.3,  nrow = 1, ncol = 3, labels = c("A", "B", "C"))
dev.off()  


#Hythese 4________________________________________________________________
plotH4.1 <- data_final %>% 
  group_by(LHS, Climate) %>% 
  summarise(ext_M = mean(EXT, na.rm = TRUE), ext_sd = sd(EXT, na.rm = TRUE)) %>%
  ggplot() + aes(x = LHS, y = ext_M) +
  facet_grid(~ Climate) +
  geom_point() + 
  geom_errorbar(aes(ymin = ext_M - ext_sd , ymax = ext_M + ext_sd)) +
  labs(x = "Life history strategies", y = "Extinction probability") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plotH4.2 <- data_final %>% 
  group_by(LHS, Climate) %>% 
  summarise(mean_GR = mean(log_GR, na.rm = TRUE), sd_GR = sd(log_GR, na.rm = TRUE)) %>%
  ggplot() + aes(x = LHS, y = mean_GR) +
  facet_grid(~ Climate) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_GR - sd_GR , ymax = mean_GR + sd_GR)) +
  labs(x = "Life history strategies", y = "Population growth rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") 


pdf("./plot/plot_hypothese/plot_H4_Results.pdf", 15, 15)
ggarrange(plotH4.1, plotH4.2,  nrow = 2, ncol = 1, labels = c("A", "B"))
dev.off()  


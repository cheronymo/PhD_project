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

#Parameters
VAR <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
runs <- 100
rExt_tot <- readRDS( file = "./output/output1/Outpout_r_selected_strategies.rds")
KExt_tot <- readRDS( file = "./output/output1/Outpout_K_selected_strategies.rds")


#___________________________________________________________________________________________________________
# COMPARAISON DEMOGRAPHIC RATE PARAMETERS
rExt_tot$LHS <- "fast"
KExt_tot$LHS <- "slow"
data_tot <- rbind(rExt_tot, KExt_tot)
data_tot$scenario <- as.factor(data_tot$scenario)

tot_plot <- ggplot(data_tot, aes(x=scenario , y=log_GR, color = scenario))+
  geom_boxplot() + 
  facet_grid(data_tot$i ~ data_tot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot

tot_plot_fast <- ggplot(rExt_tot, aes(x=rExt_tot$i , y=rExt_tot$log_GR, color = rExt_tot$i))+
  geom_boxplot() + 
  facet_grid(rExt_tot$scenario ~ rExt_tot$trait)+
  theme(legend.title = element_blank()) +
  #labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation (mean constant)") + 
  ylab("Population growth rate") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"), labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"))
tot_plot_fast

tot_plot_slow <- ggplot(KExt_tot, aes(x=KExt_tot$i , y=KExt_tot$log_GR, color = KExt_tot$i))+
  geom_boxplot() + 
  facet_grid(KExt_tot$scenario ~ KExt_tot$trait)+
  theme(legend.title = element_blank()) +
  #labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation (mean constant)") + 
  ylab("Population growth rate") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"), labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"))
tot_plot_slow


#Sub fast

data_tot_sub1 <- subset(rExt_tot, scenario == 9)
data_tot_sub5 <- subset(rExt_tot, scenario == 10)
data_tot_sub9 <- subset(rExt_tot, scenario == 11)
data_tot_sub13 <- subset(rExt_tot, scenario == 12)
data_tot_subtot_fast <- rbind(data_tot_sub1, data_tot_sub9, data_tot_sub5, data_tot_sub13)

scenario_new <- c("Da", "Db", "Dc", "Dd")
names(scenario_new) <- c("9", "10", "11", "12")

tot_plot_sub_fast <- ggplot(data_tot_subtot_fast, aes(x=i , y=log_GR, color = i))+
  geom_boxplot() + 
  facet_grid(scenario ~ trait, 
             labeller = labeller(scenario = scenario_new))+
  theme(legend.title = element_blank()) +
  #labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation (mean constant)") + 
  ylab("Population growth rate") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"),labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"))
tot_plot_sub_fast


jpeg("plot_scenario_demo3_fast.jpg", width = 10, height = 8, units = 'in', res = 300)
tot_plot_sub_fast
dev.off()



#Sub Slow
data_tot_sub1 <- subset(KExt_tot, scenario == 9)
data_tot_sub5 <- subset(KExt_tot, scenario == 10)
data_tot_sub9 <- subset(KExt_tot, scenario == 11)
data_tot_sub13 <- subset(KExt_tot, scenario == 12)
data_tot_subtot_slow <- rbind(data_tot_sub1, data_tot_sub9, data_tot_sub5, data_tot_sub13)

scenario_new <- c("Da", "Db", "Dc", "Dd")
names(scenario_new) <- c("9", "10", "11", "12")

tot_plot_sub_slow <- ggplot(data_tot_subtot_slow, aes(x=i , y=log_GR, color = i))+
  geom_boxplot() + 
  facet_grid(scenario ~ trait, 
             labeller = labeller(scenario = scenario_new))+
  theme(legend.title = element_blank()) +
  #labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation (mean constant)") + 
  ylab("Population growth rate") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"),labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"))
tot_plot_sub_slow


jpeg("plot_scenario_slow.jpg", width = 10, height = 8, units = 'in', res = 300)
tot_plot_sub_slow
dev.off()

#######################################################################################
#Plot comparaison DEMOGRAPHIC RATE

#__________________________________________
#comparaison demographic slow
KExt_tot$scenario <- as.factor(KExt_tot$scenario)

climate_new <- c("0", "0.1", "0.2", "0.3", "0.4", "0.5")
names(climate_new) <- c("1", "2", "3", "4", "5", "6")

plot_sub_slow_compa <- ggplot(KExt_tot, aes(x=scenario , y=log_GR, color = factor(scenario, labels = c("Da_lin","Db_lin","Dc_lin","Dd_lin",
                                                                                                       "Da_C_up","Db_C_up","Dc_C_up","Dd_C_up",
                                                                                                       "Da_C_down", "Db_C_down","Dc_C_down","Dd_C_down",
                                                                                                       "Gaussian" ))))+
  geom_boxplot() + 
  labs(color = "Demographic \n rates scenario") +
  facet_grid(i ~ trait,  labeller = labeller(i = climate_new))+
  theme(legend.position="right") +
  #labs(title = "Growth rate function of climatic variation") +
  xlab("Scenario of demographic rates") + 
  ylab("Population growth rate") +
  scale_x_discrete(labels = c("Da_lin","Db_lin","Dc_lin","Dd_lin",
                              "Da_C_up","Db_C_up","Dc_C_up","Dd_C_up",
                              "Da_C_down", "Db_C_down","Dc_C_down","Dd_C_down",
                              "Da_gaussian" ))+
  theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=1))

 
plot_sub_slow_compa

jpeg("plot_scenario_comparison_slow.jpg", width = 10, height = 8, units = 'in', res = 300)
plot_sub_slow_compa
dev.off()

#__________________________________________
#comparaison demographic fast
rExt_tot$scenario <- as.factor(rExt_tot$scenario)

climate_new <- c("0", "0.1", "0.2", "0.3", "0.4", "0.5")
names(climate_new) <- c("1", "2", "3", "4", "5", "6")

plot_sub_fast_compa <- ggplot(rExt_tot, aes(x=scenario , y=log_GR, color = factor(scenario, labels = c("Da_lin","Db_lin","Dc_lin","Dd_lin",
                                                                                                       "Da_C_up","Db_C_up","Dc_C_up","Dd_C_up",
                                                                                                       "Da_C_down", "Db_C_down","Dc_C_down","Dd_C_down",
                                                                                                       "Gaussian" ))))+
  geom_boxplot() +
  labs(color = "Demographic \n rates scenario") +
  facet_grid(i ~ trait,  labeller = labeller(i = climate_new))+
  theme(legend.position="right") +
  #labs(title = "Growth rate function of climatic variation") +
  xlab("Scenario of demographic rates") + 
  ylab("Population growth rate") +
  scale_x_discrete(labels = c("Da_lin","Db_lin","Dc_lin","Dd_lin",
                              "Da_C_up","Db_C_up","Dc_C_up","Dd_C_up",
                              "Da_C_down", "Db_C_down","Dc_C_down","Dd_C_down",
                              "Da_gaussian" ))+
  theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=1))


plot_sub_fast_compa

jpeg("plot_scenario_comparison_fast.jpg", width = 10, height = 8, units = 'in', res = 300)
plot_sub_fast_compa
dev.off()

#___________________________________________________________________________________________________________
#Comparaison strategies

climate_new <- c("0", "0.1", "0.2", "0.3", "0.4", "0.5")
names(climate_new) <- c("1", "2", "3", "4", "5", "6")

plot_sub_TOT_compa <- ggplot(data_tot, aes(x=scenario , y=log_GR, color = LHS))+
  geom_boxplot() +
  labs(color = "Life history \n strategies") +
  facet_grid(i ~ trait,  labeller = labeller(i = climate_new))+
  theme(legend.position="right") +
  #labs(title = "Growth rate function of climatic variation") +
  xlab("Demographic rates scenario") + 
  ylab("Population growth rate") +
  scale_x_discrete(labels = c("Da_lin","Db_lin","Dc_lin","Dd_lin",
                              "Da_C_up","Db_C_up","Dc_C_up","Dd_C_up",
                              "Da_C_down", "Db_C_down","Dc_C_down","Dd_C_down",
                              "Da_gaussian" ))+
  theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=1))
plot_sub_TOT_compa

jpeg("plot_scenario_comparison_LHS.jpg", width = 10, height = 8, units = 'in', res = 300)
plot_sub_TOT_compa
dev.off()


#######################################################################################
#Plot comparaison STRATEGIES

#__________________________________________
#comparaison strategies
trait_label <- c("Linear", "Curve up", "Curve Down", "Gaussian")
names(trait_label) <- c("1", "5", "9", "13")

data_tot_sub1 <- subset(data_tot, scenario == 1)
data_tot_sub5 <- subset(data_tot, scenario == 5)
data_tot_sub9 <- subset(data_tot, scenario == 9)
data_tot_sub13 <- subset(data_tot, scenario == 13)
data_tot_subtot_fast <- rbind(data_tot_sub1, data_tot_sub9, data_tot_sub5, data_tot_sub13)

plot_sub_lhs_simp_compa <- ggplot(data_tot_subtot_fast, aes(x=i , y=log_GR, color = LHS))+
  geom_boxplot() + 
  labs(color = "Demographic \n rates scenario") +
  facet_grid(scenario ~ trait,  labeller = labeller(scenario = trait_label))+
  theme(legend.position="right") +
  #labs(title = "Growth rate function of climatic variation") +
  xlab("Climatic variation") + 
  ylab("Population growth rate") +
  scale_x_discrete(labels = climate_new)
plot_sub_lhs_simp_compa


jpeg("plot_scenario_comparison_LHS_symplify.jpg", width = 11, height = 8, units = 'in', res = 300)
plot_sub_lhs_simp_compa
dev.off()


#__________________________________________
#comparaison strategies
data_tot_subtot_mean <- data_tot

data_tot_subtot_mean <- data_tot_subtot_mean %>%
  mutate(scenario_demo = case_when(scenario == "1" ~ 'Linear', scenario == "2" ~ 'Linear', scenario == "3" ~ 'Linear', scenario == "4" ~ 'Linear',
                                    scenario == "5" ~ 'CurveUp',  scenario == "6" ~ 'CurveUp',  scenario == "7" ~ 'CurveUp',  scenario == "8" ~ 'CurveUp',
                                    scenario == "9" ~ 'CurveDown',  scenario == "10" ~ 'CurveDown',  scenario == "11" ~ 'CurveDown',  scenario == "12" ~ 'CurveDown',
                                    scenario == "13" ~ 'Gaussian'))

data_tot_subtot_mean$scenario_demo_f <- factor(data_tot_subtot_mean$scenario_demo, levels = c('Linear', 'CurveUp', 'CurveDown','Gaussian'))

plot_sub_lhs_simp_compa <- ggplot(data_tot_subtot_mean, aes(x=i , y=log_GR, color = LHS))+
  geom_boxplot() + 
    labs(color = "Life history \n strategies") +
  facet_grid(scenario_demo_f ~ trait)+
  theme(legend.position="right") +
  #labs(title = "Growth rate function of climatic variation") +
  xlab("Climatic variation") + 
  ylab("Population growth rate") +
  scale_x_discrete(labels = climate_new)
plot_sub_lhs_simp_compa


jpeg("plot_scenario_comparison_LHS_symplify_mean.jpg", width = 11, height = 8, units = 'in', res = 300)
plot_sub_lhs_simp_compa
dev.off()




#######################################################################################
#Plot comparaison climate scenario


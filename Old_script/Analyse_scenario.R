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



#Same GENTREND_______________________________________________________________________________________
out_K_trait_bind <- readRDS( file = "./output/Output_gentrend/trial2/Outpout_K_selected_strategies.rds")
out_K_trait_bind$LHS <- "ExtSlow"
out_r_trait_bind <- readRDS( file = "./output/Output_gentrend/trial2/Outpout_r_selected_strategies.rds")
out_r_trait_bind$LHS <- "ExtFast"
out_rMix_trait_bind <- readRDS( file = "./output/Output_gentrend/trial2/Outpout_rMix_selected_strategies.rds")
out_rMix_trait_bind$LHS <- "Fast"
out_KMix_trait_bind <- readRDS( file = "./output/Output_gentrend/trial2/Outpout_KMix_selected_strategies.rds")
out_KMix_trait_bind$LHS <- "Slow"

head(out_KMix_trait_bind)
unique(out_r_trait_bind$climate_scenario)

data_output_gentrend <- rbind(out_K_trait_bind, out_r_trait_bind, out_rMix_trait_bind, out_KMix_trait_bind)
data_output_gentrend$LHS <-factor(data_output_gentrend$LHS ,c("ExtFast", "Fast", "Slow", "ExtSlow"))
data_output_gentrend$scenario_demo <-factor(data_output_gentrend$scenario_demo ,c("Linear1","Linear2", "Sigmoid1", "Sigmoid2", "BellShape"))
data_output_gentrend$scenario_trait <-factor(data_output_gentrend$scenario_trait ,c("LinPo", "LinNeg", "SigPo", "SigNeg","BellShape"))
data_output_gentrend$climate_scenario <-factor(data_output_gentrend$climate_scenario,
                                      c("nothing", "Sdt0.2", "Sdt0.5", "Inc0.3", "Deac0.3","red",
                                        "MeanIn", "MeanIn.0.3", "M.SdInc", "M.SdDeac", "M.In.Red"))
data_output_gentrend$log_GR[is.infinite(data_output_gentrend$log_GR)] <- NA

data_output_gentrend


tot_plot <- ggplot(data_output_gentrend, aes(x = climate_scenario , y = EXT, color = LHS   ))+
  geom_boxplot() + 
  facet_grid(scenario~scenario_trait   )+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic scenario") +
  theme(legend.position="bottom")+
  xlab("Climatic scenario") + 
  ylab("Growth rate")
tot_plot


#Function plot____________________________________________________________________________________________________
#_____________________________________________________________
#plot image
library("ggplot2")
library("ggimage")

#climate
  data_output_gen %>% 
  filter(climate_scenario == "M.SdInc") %>% 
  group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
  mutate(mean_time_ext = log(1 - EXT)) %>%   
  summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), 
            mean_EXTtime = mean(EXT_time,na.rm = TRUE ), mean_EXT_proba = mean(mean_time_ext,na.rm = TRUE ) ) %>% 
  unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
  mutate(image_strategy = case_when(
    LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
    LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
    LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
    LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
  filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                             "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
  ggplot() + aes(x = mean_EXT_proba, y = scenario_tot)   +    
  geom_image(aes(image=image_strategy), size=.04)    +
  geom_vline(xintercept = 0,linetype="dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Scenario \n 'traitXdemographic rate'") + xlab("Population growth rate") +  ggtitle("nothing") +
  scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                              "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                   label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                             "LP x B","LN x B","SP x S:S+/F-",
                             "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                             "B x B"))  

#trait
  plot_sorti_ex <- data_output_gen %>% 
    filter(LHS == "ExtFast") %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    #summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ) ) %>% 
    filter(scenario_trait == "LinPo") %>% 
    filter(scenario == "1") %>% 
    mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
    ggplot() + aes(y = SDT, x = climate_scenario) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Climatic scenario") + xlab("Extinction probability") +  ggtitle(paste0("Trait"," LinPo", " and Demographic rate", "Linear"))

#Same gen_________________________________________________________________

c <- lapply(unique(data_output_gen$climate_scenario) , function(param){
  plot_sorti1 <- data_output_gen %>% 
    filter(climate_scenario == param) %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ) ) %>% 
    unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
    mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
    filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                               "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
    ggplot() + aes(x = mean_extin_prob, y = scenario_tot) +
    geom_image(aes(image=image_strategy), size=.04) + xlim(0,1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Scenario \n 'traitXdemographic rate'") + xlab("Extinction probability") +  ggtitle(param) +
    scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                                "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                     label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                               "LP x B","LN x B","SP x S:S+/F-",
                               "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                               "B x B"))
  
  plot_sorti2 <- data_output_gen %>% 
    filter(climate_scenario == param) %>% 
    mutate(ext_gen_time = case_when(LHS == "ExtSlow" ~ EXT_time/17.2,
                                    LHS == "Slow" ~ EXT_time/6.23,
                                    LHS == "Fast" ~ EXT_time/2.43,
                                    LHS == "ExtFast" ~ EXT_time/2.02)) %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(ext_gen_time, na.rm = TRUE )) %>% 
    unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
    mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
    filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                               "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
    ggplot() + aes(x = mean_EXTtime, y = scenario_tot) +  xlim(0,100) +
    geom_image(aes(image=image_strategy), size=.04)  +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Scenario \n 'traitXdemographic rate'") + xlab("Time to extinction") +  ggtitle(param) +
    scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                                "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                     label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                               "LP x B","LN x B","SP x S:S+/F-",
                               "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                               "B x B"))
  
  plot_sorti3 <- data_output_gen %>% 
    filter(climate_scenario == param) %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ) ) %>% 
    unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
    mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
    filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                               "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
    ggplot() + aes(x = mean_logGR, y = scenario_tot) +  xlim(-0.07, 0.07) +
    geom_image(aes(image=image_strategy), size=.04)    +
    geom_vline(xintercept = 0,linetype="dashed") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Scenario \n 'traitXdemographic rate'") + xlab("Population growth rate") +  ggtitle(param) +
    scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                                "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                     label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                               "LP x B","LN x B","SP x S:S+/F-",
                               "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                               "B x B"))
  
  return(grid.arrange(plot_sorti1, plot_sorti2, plot_sorti3, ncol=3))
})

for (i in 1:length(c)) {
  ext <- unique(data_output_gen$climate_scenario)[i]
  ext <- sub("\\.", "_", ext)
  jpeg(paste0("./plot/plot_gen/", i,"plotGen_output_", ext,".jpg"), width = 25, height = 10, units = 'in', res = 300)
  do.call(grid.arrange, c(c[[i]]))
  dev.off()
}


#Same lenght_________________________________________________________________

d <- lapply(unique(data_output_time$climate_scenario) , function(param){
  plot_sorti1 <- data_output_time %>% 
    filter(climate_scenario == param) %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ) ) %>% 
    unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
     mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>%
    filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                               "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
    ggplot() + aes(x = mean_extin_prob, y = scenario_tot) +
    geom_image(aes(image=image_strategy), size=.04) + xlim(0,1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Scenario \n 'traitXdemographic rate'") + xlab("Extinction probability") +  ggtitle(param)+
    scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                                "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                     label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                               "LP x B","LN x B","SP x S:S+/F-",
                               "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                               "B x B"))
  
  plot_sorti2 <- data_output_time %>% 
    filter(climate_scenario == param) %>% 
    mutate(ext_gen_time = case_when(LHS == "ExtSlow" ~ EXT_time/17.2,
                                    LHS == "Slow" ~ EXT_time/6.23,
                                    LHS == "Fast" ~ EXT_time/2.43,
                                    LHS == "ExtFast" ~ EXT_time/2.02)) %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time, na.rm = TRUE )) %>% 
    unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
    mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
    filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                               "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
    ggplot() + aes(x = mean_EXTtime, y = scenario_tot) +  xlim(0, 200) +
    geom_image(aes(image=image_strategy), size=.04)  +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Scenario \n 'traitXdemographic rate'") + xlab("Time to extinction") +  ggtitle(param) +
    scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                                "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                     label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                               "LP x B","LN x B","SP x S:S+/F-",
                               "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                               "B x B"))
  
  plot_sorti3 <- data_output_time %>% 
    filter(climate_scenario == param) %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ) ) %>% 
    unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>%   
    mutate(image_strategy = case_when(
      LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
      LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
      LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
      LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
    filter(scenario_tot %in% c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                               "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5")) %>% 
    ggplot() + aes(x = mean_logGR, y = scenario_tot) + xlim(-0.07, 0.07) +
    geom_image(aes(image=image_strategy), size=.04)  +
    geom_vline(xintercept = 0,linetype="dashed") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ylab("Scenario \n 'traitXdemographic rate'") + xlab("Population growth rate") +  ggtitle(param) +
    scale_y_discrete(breaks = c("LinPo_1", "LinPo_2", "LinPo_3", "LinPo_5", "LinNeg_5", "SigPo_3","SigPo_4",
                                "SigPo_5", "BellShape_1", "BellShape_2", "BellShape_3", "BellShape_5"), 
                     label = c("LP x L:S+/F-","LP x L:S-/F+","LP x S:S+/F-",
                               "LP x B","LN x B","SP x S:S+/F-",
                               "SP x S:S-/F+", "SP x B", "B x L:S+/F-", "B x L:S-/F+", "B x S:S+/F-",
                               "B x B"))
  return(grid.arrange(plot_sorti1, plot_sorti2, plot_sorti3, ncol=3))
})

for (i in 1:length(d)) {
  ext <- unique(data_output_time$climate_scenario)[i]
  ext <- sub("\\.", "_", ext)
  jpeg(paste0("./plot/plot_time/", i,"plotTime_output_", ext,".jpg"), width = 25, height = 10, units = 'in', res = 300)
  do.call(grid.arrange, c(d[[i]]))
  dev.off()
}

#Plot by climate for several trait____________________________________________________________________________________________
#SD
e <- lapply(unique(data_output_gen$scenario_trait), function(trait){
trait_fac <- as.factor(trait)  
data_trait <- data_output_gen %>% 
  filter(LHS == "Slow") %>% 
  group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
  #summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ), SD_mean = mean(SDT, na.rm = TRUE)) %>% 
  filter(scenario_trait == trait_fac)


lapply(unique(data_output_gen$scenario), function(demo){
data_dem <- data_trait %>% 
  filter(scenario == demo) %>% 
  mutate(image_strategy = case_when(
    LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
    LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
    LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
    LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
  ggplot() + aes(y = SDT, x = climate_scenario) +
  geom_boxplot() +  scale_y_continuous(limits=c(0,500)) + 
  #geom_image(aes(image=image_strategy), size=.04) + xlim(0,1) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Climatic scenario") + xlab("Standard deviation of population size") +  ggtitle(paste0("Trait ",trait, " and Demographic rate ", demo ))

return(data_dem)
})

})

trait_1 <- grid.arrange(e[[1]][[1]], e[[1]][[2]], e[[1]][[3]], 
             e[[1]][[4]], e[[1]][[5]], nrow = 5) 
trait_2 <- grid.arrange(e[[2]][[1]], e[[2]][[2]], e[[2]][[3]], 
                        e[[2]][[4]], e[[2]][[5]], nrow = 5) 
trait_3 <- grid.arrange(e[[3]][[1]], e[[3]][[2]], e[[3]][[3]], 
                        e[[3]][[4]], e[[3]][[5]], nrow = 5) 
trait_4 <- grid.arrange(e[[4]][[1]], e[[4]][[2]], e[[4]][[3]], 
                        e[[4]][[4]], e[[4]][[5]], nrow = 5)
trait_5 <- grid.arrange(e[[5]][[1]], e[[5]][[2]], e[[5]][[3]], 
                        e[[5]][[4]], e[[5]][[5]], nrow = 5) 
pdf("./plot/sum/plot_K_SD.pdf", 60, 60)
grid.arrange(trait_1, trait_2, trait_3, trait_4, trait_5, nrow = 1)
dev.off()

#EXT
f <- lapply(unique(data_output_gen$scenario_trait), function(trait){
  trait_fac <- as.factor(trait)  
  data_trait <- data_output_gen %>% 
    filter(LHS == "ExtFast") %>% 
    group_by(scenario_trait, LHS, scenario, climate_scenario) %>% 
    #summarise(mean_extin_prob = mean(EXT), mean_logGR = mean(log_GR, na.rm = TRUE), mean_EXTtime = mean(EXT_time,na.rm = TRUE ), SD_mean = mean(SDT, na.rm = TRUE)) %>% 
    filter(scenario_trait == trait_fac)
  
  
  lapply(unique(data_output_gen$scenario), function(demo){
    data_dem <- data_trait %>% 
      filter(scenario == demo) %>% 
      mutate(image_strategy = case_when(
        LHS == "ExtSlow" ~ "C:/Users/Chero/Pictures/image/whale.png",
        LHS == "Slow" ~ "C:/Users/Chero/Pictures/image/RoeDeer.png",
        LHS == "Fast" ~ "C:/Users/Chero/Pictures/image/rabbit.png",
        LHS == "ExtFast" ~"C:/Users/Chero/Pictures/image/rat.png")) %>% 
      ggplot() + aes(y = EXT, x = climate_scenario) +
      geom_col()  + 
      #geom_image(aes(image=image_strategy), size=.04) + xlim(0,1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ylab("Climatic scenario") + xlab("Extinction probability") +  ggtitle(paste0("Trait ",trait, " and Demographic rate ", demo ))
    
    return(data_dem)
  })
  
})

trait_1 <- grid.arrange(f[[1]][[1]], f[[1]][[2]], f[[1]][[3]], 
                        f[[1]][[4]], f[[1]][[5]], nrow = 5) 
trait_2 <- grid.arrange(f[[2]][[1]], f[[2]][[2]], f[[2]][[3]], 
                        f[[2]][[4]], f[[2]][[5]], nrow = 5) 
trait_3 <- grid.arrange(f[[3]][[1]], f[[3]][[2]], f[[3]][[3]], 
                        f[[3]][[4]], f[[3]][[5]], nrow = 5) 
trait_4 <- grid.arrange(f[[4]][[1]], f[[4]][[2]], f[[4]][[3]], 
                        f[[4]][[4]], f[[4]][[5]], nrow = 5)
trait_5 <- grid.arrange(f[[5]][[1]], f[[5]][[2]], f[[5]][[3]], 
                        f[[5]][[4]], f[[5]][[5]], nrow = 5) 
pdf("./plot/sum/plot_extr_EXT.pdf", 60, 60)
grid.arrange(trait_1, trait_2, trait_3, trait_4, trait_5, nrow = 1)
dev.off()


###################################################################################################################
#Plot summurize everything__________________________________________________________________________________________
max(data_output_gentrend$SDT)

S1 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S1.png")
S2 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S2.png")
S3 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S3.png")
S4 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S4.png")
S5 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S5.png")
S6 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S6.png")
S7 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S7.png")
S8 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S8.png")
S9 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S9.png")
S10 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S10.png")
S11 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S11.png")
S12 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S12.png")
S13 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S13.png")
S14 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S14.png")
S15 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/S15.png")

C1 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C1.png")
C2 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C2.png")
C3 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C3.png")
C4 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C4.png")
C5 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C5.png")
C6 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C6.png")
C7 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C7.png")
C8 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C8.png")
C9 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C9.png")
C10 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C10.png")
C11 <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/C11.png")

W <- image_read("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/white.png")


data_final <- data_output_gentrend %>% 
  unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>% 
  mutate(Sce_trait_x_demo = case_when(scenario_tot == "LinPo_1" ~ 'S1', scenario_tot == "LinNeg_1" ~ 'S2', scenario_tot == "SigPo_1" ~ 'S3', scenario_tot == "SigNeg_1" ~ 'S4',
                                      scenario_tot == "BellShape_1" ~ 'S11', scenario_tot == "LinPo_2" ~ 'S2', scenario_tot == "LinNeg_2" ~ 'S1', scenario_tot == "SigPo_2" ~ 'S4',
                                      scenario_tot == "SigNeg_2" ~ 'S3', scenario_tot == "BellShape_2" ~ 'S12', scenario_tot == "LinPo_3" ~ 'S3', scenario_tot == "LinNeg_3" ~ 'S4',
                                      scenario_tot == "SigPo_3" ~ 'S5', scenario_tot == "SigNeg_3" ~ 'S6', scenario_tot == "BellShape_3" ~ 'S13', scenario_tot == "LinPo_4" ~ 'S4', 
                                      scenario_tot == "LinNeg_4" ~ 'S3', scenario_tot == "SigPo_4" ~ 'S6', scenario_tot == "SigNeg_4" ~ 'S5', scenario_tot == "BellShape_4" ~ 'S14',
                                      scenario_tot == "LinPo_5" ~ 'S9', scenario_tot == "LinNeg_5" ~ 'S10', scenario_tot == "SigPo_5" ~ 'S7', scenario_tot == "SigNeg_5" ~ 'S8',
                                      scenario_tot == "BellShape_5" ~ 'S15'))
data_final$Sce_trait_x_demo = factor(data_final$Sce_trait_x_demo, levels=c('S1','S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12', 'S13', 'S14', 'S15'))


pdf("./plot/final/plot_sum_GrowthRate.pdf", 60, 60)
ggplot(data = data_final) + aes(x = LHS, y = log_GR, fill = LHS) +
  facet_grid(climate_scenario ~ data_final$Sce_trait_x_demo) +  scale_fill_viridis_d(option = "viridis") +
  #theme_void() + 
  geom_boxplot(na.rm = TRUE) + ylim(-0.01, 0.01)+ 
  geom_hline(aes(yintercept = 0), colour="#990000", linetype="dashed") + 
  theme(legend.position = "none", strip.text.x = element_text(size = 100, colour = "white"), strip.text.y = element_text(size = 500)) 
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 0.14, height = 4)
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 4, height = 0.08)

grid.raster(S1, x = unit(0.04, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015) 
grid.raster(S2, x = unit(0.10, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S3, x = unit(0.16, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S4, x = unit(0.22, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S5, x = unit(0.28, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S6, x = unit(0.34, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S7, x = unit(0.40, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S8, x = unit(0.46, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S9, x = unit(0.52, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S10, x = unit(0.58, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S11, x = unit(0.64, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S12, x = unit(0.70, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S13, x = unit(0.76, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S14, x = unit(0.81, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S15, x = unit(0.86, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)

grid.raster(C1, x = unit(0.945, "npc"), y = unit(0.933, "npc"), width = 0.09, height = 0.09) 
grid.raster(C2, x = unit(0.945, "npc"), y = unit(0.85, "npc"), width = 0.09, height = 0.09)
grid.raster(C3, x = unit(0.945, "npc"), y = unit(0.755, "npc"), width = 0.09, height = 0.09)
grid.raster(C4, x = unit(0.945, "npc"), y = unit(0.675, "npc"), width = 0.09, height = 0.09)
grid.raster(C5, x = unit(0.945, "npc"), y = unit(0.585, "npc"), width = 0.09, height = 0.09)
grid.raster(C6, x = unit(0.945, "npc"), y = unit(0.50, "npc"), width = 0.09, height = 0.09)
grid.raster(C7, x = unit(0.945, "npc"), y = unit(0.40, "npc"), width = 0.09, height = 0.09)
grid.raster(C8, x = unit(0.945, "npc"), y = unit(0.33, "npc"), width = 0.09, height = 0.09)
grid.raster(C9, x = unit(0.945, "npc"), y = unit(0.25, "npc"), width = 0.09, height = 0.09)
grid.raster(C10, x = unit(0.945, "npc"), y = unit(0.13, "npc"), width = 0.09, height = 0.09)
grid.raster(C11, x = unit(0.945, "npc"), y = unit(0.05, "npc"), width = 0.09, height = 0.09)

dev.off()


pdf("./plot/final/plot_sum_cv.pdf", 60, 60)
ggplot(data = data_final) + aes(x = LHS, y = CV, fill = LHS) +
  facet_grid(climate_scenario ~ data_final$Sce_trait_x_demo) +
  geom_boxplot() + ylim(0, 1000) + scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none", strip.text.x = element_text(size = 100, colour = "white"), strip.text.y = element_text(size = 500)) 
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 0.14, height = 4)
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 4, height = 0.08)

grid.raster(S1, x = unit(0.04, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015) 
grid.raster(S2, x = unit(0.10, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S3, x = unit(0.16, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S4, x = unit(0.22, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S5, x = unit(0.28, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S6, x = unit(0.34, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S7, x = unit(0.40, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S8, x = unit(0.46, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S9, x = unit(0.52, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S10, x = unit(0.58, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S11, x = unit(0.64, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S12, x = unit(0.70, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S13, x = unit(0.76, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S14, x = unit(0.81, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S15, x = unit(0.86, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)

grid.raster(C1, x = unit(0.945, "npc"), y = unit(0.933, "npc"), width = 0.09, height = 0.09) 
grid.raster(C2, x = unit(0.945, "npc"), y = unit(0.85, "npc"), width = 0.09, height = 0.09)
grid.raster(C3, x = unit(0.945, "npc"), y = unit(0.755, "npc"), width = 0.09, height = 0.09)
grid.raster(C4, x = unit(0.945, "npc"), y = unit(0.675, "npc"), width = 0.09, height = 0.09)
grid.raster(C5, x = unit(0.945, "npc"), y = unit(0.585, "npc"), width = 0.09, height = 0.09)
grid.raster(C6, x = unit(0.945, "npc"), y = unit(0.50, "npc"), width = 0.09, height = 0.09)
grid.raster(C7, x = unit(0.945, "npc"), y = unit(0.40, "npc"), width = 0.09, height = 0.09)
grid.raster(C8, x = unit(0.945, "npc"), y = unit(0.33, "npc"), width = 0.09, height = 0.09)
grid.raster(C9, x = unit(0.945, "npc"), y = unit(0.25, "npc"), width = 0.09, height = 0.09)
grid.raster(C10, x = unit(0.945, "npc"), y = unit(0.13, "npc"), width = 0.09, height = 0.09)
grid.raster(C11, x = unit(0.945, "npc"), y = unit(0.05, "npc"), width = 0.09, height = 0.09)

dev.off()


pdf("./plot/final/plot_sum_sdt.pdf", 60, 60)
ggplot(data = data_final) + aes(x = LHS, y = SDT, fill =LHS) +
  facet_grid(climate_scenario ~ data_final$Sce_trait_x_demo) +
  geom_boxplot() + ylim(0, 1000)+ scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none", strip.text.x = element_text(size = 100, colour = "white"), strip.text.y = element_text(size = 500)) 
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 0.14, height = 4)
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 4, height = 0.08)

grid.raster(S1, x = unit(0.04, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015) 
grid.raster(S2, x = unit(0.10, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S3, x = unit(0.16, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S4, x = unit(0.22, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S5, x = unit(0.28, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S6, x = unit(0.34, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S7, x = unit(0.40, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S8, x = unit(0.46, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S9, x = unit(0.52, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S10, x = unit(0.58, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S11, x = unit(0.64, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S12, x = unit(0.70, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S13, x = unit(0.76, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S14, x = unit(0.81, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S15, x = unit(0.86, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)

grid.raster(C1, x = unit(0.945, "npc"), y = unit(0.933, "npc"), width = 0.09, height = 0.09) 
grid.raster(C2, x = unit(0.945, "npc"), y = unit(0.85, "npc"), width = 0.09, height = 0.09)
grid.raster(C3, x = unit(0.945, "npc"), y = unit(0.755, "npc"), width = 0.09, height = 0.09)
grid.raster(C4, x = unit(0.945, "npc"), y = unit(0.675, "npc"), width = 0.09, height = 0.09)
grid.raster(C5, x = unit(0.945, "npc"), y = unit(0.585, "npc"), width = 0.09, height = 0.09)
grid.raster(C6, x = unit(0.945, "npc"), y = unit(0.50, "npc"), width = 0.09, height = 0.09)
grid.raster(C7, x = unit(0.945, "npc"), y = unit(0.40, "npc"), width = 0.09, height = 0.09)
grid.raster(C8, x = unit(0.945, "npc"), y = unit(0.33, "npc"), width = 0.09, height = 0.09)
grid.raster(C9, x = unit(0.945, "npc"), y = unit(0.25, "npc"), width = 0.09, height = 0.09)
grid.raster(C10, x = unit(0.945, "npc"), y = unit(0.13, "npc"), width = 0.09, height = 0.09)
grid.raster(C11, x = unit(0.945, "npc"), y = unit(0.05, "npc"), width = 0.09, height = 0.09)


dev.off()




pdf("./plot/final/plot_sum_extin.pdf", 60, 60)
data_final %>% 
  group_by(Sce_trait_x_demo, LHS, climate_scenario) %>%
  summarise(mean_extin_prob = mean(EXT)) %>% 
  ggplot(aes(x = LHS, y = mean_extin_prob, fill = LHS, color = LHS)) +
    facet_grid(climate_scenario ~ Sce_trait_x_demo) + scale_fill_viridis_d(option = "viridis") +
    geom_bar(stat = "identity") + ylim(0, 1)+
  theme(legend.position = "none", strip.text.x = element_text(size = 100, colour = "white"), strip.text.y = element_text(size = 500)) 
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 0.14, height = 4)
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 4, height = 0.08)

grid.raster(S1, x = unit(0.04, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015) 
grid.raster(S2, x = unit(0.10, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S3, x = unit(0.16, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S4, x = unit(0.22, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S5, x = unit(0.28, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S6, x = unit(0.34, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S7, x = unit(0.40, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S8, x = unit(0.46, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S9, x = unit(0.52, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S10, x = unit(0.58, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S11, x = unit(0.64, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S12, x = unit(0.70, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S13, x = unit(0.76, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S14, x = unit(0.81, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S15, x = unit(0.86, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)

grid.raster(C1, x = unit(0.945, "npc"), y = unit(0.933, "npc"), width = 0.09, height = 0.09) 
grid.raster(C2, x = unit(0.945, "npc"), y = unit(0.85, "npc"), width = 0.09, height = 0.09)
grid.raster(C3, x = unit(0.945, "npc"), y = unit(0.755, "npc"), width = 0.09, height = 0.09)
grid.raster(C4, x = unit(0.945, "npc"), y = unit(0.675, "npc"), width = 0.09, height = 0.09)
grid.raster(C5, x = unit(0.945, "npc"), y = unit(0.585, "npc"), width = 0.09, height = 0.09)
grid.raster(C6, x = unit(0.945, "npc"), y = unit(0.50, "npc"), width = 0.09, height = 0.09)
grid.raster(C7, x = unit(0.945, "npc"), y = unit(0.40, "npc"), width = 0.09, height = 0.09)
grid.raster(C8, x = unit(0.945, "npc"), y = unit(0.33, "npc"), width = 0.09, height = 0.09)
grid.raster(C9, x = unit(0.945, "npc"), y = unit(0.25, "npc"), width = 0.09, height = 0.09)
grid.raster(C10, x = unit(0.945, "npc"), y = unit(0.13, "npc"), width = 0.09, height = 0.09)
grid.raster(C11, x = unit(0.945, "npc"), y = unit(0.05, "npc"), width = 0.09, height = 0.09)

dev.off()


pdf("./plot/final/plot_sum_TimeToExtin.pdf", 60, 60)
data_final %>% 
  group_by(Sce_trait_x_demo, LHS, climate_scenario) %>%
  summarise(mean_TimeExtin_prob = mean(EXT_time)) %>% 
  ggplot(aes(x = LHS, y = mean_TimeExtin_prob, fill = LHS, color = LHS)) + scale_fill_viridis_d(option = "viridis") +
  facet_grid(climate_scenario ~ Sce_trait_x_demo) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", strip.text.x = element_text(size = 100, colour = "white"), strip.text.y = element_text(size = 500)) 
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 0.14, height = 4)
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 4, height = 0.08)

grid.raster(S1, x = unit(0.04, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015) 
grid.raster(S2, x = unit(0.10, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S3, x = unit(0.16, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S4, x = unit(0.22, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S5, x = unit(0.28, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S6, x = unit(0.34, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S7, x = unit(0.40, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S8, x = unit(0.46, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S9, x = unit(0.52, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S10, x = unit(0.58, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S11, x = unit(0.64, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S12, x = unit(0.70, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S13, x = unit(0.76, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S14, x = unit(0.81, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S15, x = unit(0.86, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)

grid.raster(C1, x = unit(0.945, "npc"), y = unit(0.933, "npc"), width = 0.09, height = 0.09) 
grid.raster(C2, x = unit(0.945, "npc"), y = unit(0.85, "npc"), width = 0.09, height = 0.09)
grid.raster(C3, x = unit(0.945, "npc"), y = unit(0.755, "npc"), width = 0.09, height = 0.09)
grid.raster(C4, x = unit(0.945, "npc"), y = unit(0.675, "npc"), width = 0.09, height = 0.09)
grid.raster(C5, x = unit(0.945, "npc"), y = unit(0.585, "npc"), width = 0.09, height = 0.09)
grid.raster(C6, x = unit(0.945, "npc"), y = unit(0.50, "npc"), width = 0.09, height = 0.09)
grid.raster(C7, x = unit(0.945, "npc"), y = unit(0.40, "npc"), width = 0.09, height = 0.09)
grid.raster(C8, x = unit(0.945, "npc"), y = unit(0.33, "npc"), width = 0.09, height = 0.09)
grid.raster(C9, x = unit(0.945, "npc"), y = unit(0.25, "npc"), width = 0.09, height = 0.09)
grid.raster(C10, x = unit(0.945, "npc"), y = unit(0.13, "npc"), width = 0.09, height = 0.09)
grid.raster(C11, x = unit(0.945, "npc"), y = unit(0.05, "npc"), width = 0.09, height = 0.09)


dev.off()



pdf("./plot/final/plot_sum_Abun.pdf", 60, 60)
data_final %>% 
  ggplot(aes(x = LHS, y = SDT, fill = LHS, color = LHS)) +
  facet_grid(climate_scenario ~ Sce_trait_x_demo) + scale_fill_viridis_d(option = "viridis") +
  geom_boxplot() + ylim(0, 2000)+
  theme(legend.position = "none", strip.text.x = element_text(size = 100, colour = "white"), strip.text.y = element_text(size = 500)) 
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 0.14, height = 4)
grid.raster(W, x = unit(0.96, "npc"), y = unit(0.98, "npc"), width = 4, height = 0.08)
grid.raster(S1, x = unit(0.04, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015) 
grid.raster(S2, x = unit(0.10, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S3, x = unit(0.16, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S4, x = unit(0.22, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S5, x = unit(0.28, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S6, x = unit(0.34, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S7, x = unit(0.40, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S8, x = unit(0.46, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S9, x = unit(0.52, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S10, x = unit(0.58, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S11, x = unit(0.64, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S12, x = unit(0.70, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S13, x = unit(0.76, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S14, x = unit(0.81, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)
grid.raster(S15, x = unit(0.86, "npc"), y = unit(0.96, "npc"), width = 0.04, height = 0.015)

grid.raster(C1, x = unit(0.945, "npc"), y = unit(0.933, "npc"), width = 0.09, height = 0.09) 
grid.raster(C2, x = unit(0.945, "npc"), y = unit(0.85, "npc"), width = 0.09, height = 0.09)
grid.raster(C3, x = unit(0.945, "npc"), y = unit(0.755, "npc"), width = 0.09, height = 0.09)
grid.raster(C4, x = unit(0.945, "npc"), y = unit(0.675, "npc"), width = 0.09, height = 0.09)
grid.raster(C5, x = unit(0.945, "npc"), y = unit(0.585, "npc"), width = 0.09, height = 0.09)
grid.raster(C6, x = unit(0.945, "npc"), y = unit(0.50, "npc"), width = 0.09, height = 0.09)
grid.raster(C7, x = unit(0.945, "npc"), y = unit(0.40, "npc"), width = 0.09, height = 0.09)
grid.raster(C8, x = unit(0.945, "npc"), y = unit(0.33, "npc"), width = 0.09, height = 0.09)
grid.raster(C9, x = unit(0.945, "npc"), y = unit(0.25, "npc"), width = 0.09, height = 0.09)
grid.raster(C10, x = unit(0.945, "npc"), y = unit(0.13, "npc"), width = 0.09, height = 0.09)
grid.raster(C11, x = unit(0.945, "npc"), y = unit(0.05, "npc"), width = 0.09, height = 0.09)


dev.off()



####################################################################################################################
#trial_____________________________________________________________________________________________________________

#relation
r1.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ .x , size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r1.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ -.x, size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S1.png", width = 400, height = 200)
r1 <- grid.arrange(r1.1, r1.2, nrow = 1)
dev.off()

png("./plot/plot_relation/S2.png", width = 400, height = 200)
r2 <- grid.arrange(r1.2, r1.1, nrow = 1)
dev.off()

r3.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (1/(1+exp(-5*.x)) - 0.5), size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r3.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (1/(1+exp(5*.x)) - 0.5), size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S3.png", width = 400, height = 200)
r3 <- grid.arrange(r3.1, r3.2, nrow = 1)
dev.off()

png("./plot/plot_relation/S4.png", width = 400, height = 200)
r4 <- grid.arrange(r3.2, r3.1, nrow = 1)
dev.off()


r5.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (1/(1+exp(-5*(1/(1+exp(-5*.x)) - 0.5))) - 0.5), size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r5.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (1/(1+exp(-5*(1/(1+exp(5*.x)) - 0.5))) - 0.5), size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S5.png", width = 400, height = 200)
r5 <- grid.arrange(r5.1, r5.2, nrow = 1)
dev.off()

png("./plot/plot_relation/S6.png", width = 400, height = 200)
r6 <- grid.arrange(r5.2, r5.1, nrow = 1)
dev.off()

r7.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ -0.5*(1/(1+exp(-5*.x) - 0.5))^2, size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r7.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ -0.5*(1/(1+exp(-5*.x) - 0.5))^2, size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S7.png", width = 400, height = 200)
r7 <- grid.arrange(r7.1, r7.2, nrow = 1)
dev.off()

r8.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ -0.5*(1/(1+exp(5*.x) - 0.5))^2, size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r8.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ -0.5*(1/(1+exp(5*.x) - 0.5))^2, size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S8.png", width = 400, height = 200)
r8 <- grid.arrange(r8.1, r8.2, nrow = 1)
dev.off()

r9.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (-0.5*.x^2) , size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r9.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (0.5*.x^2) , size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S9.png", width = 400, height = 200)
r9 <- grid.arrange(r9.1, r9.1, nrow = 1)
dev.off()

png("./plot/plot_relation/S10.png", width = 400, height = 200)
r10 <- grid.arrange(r9.2, r9.2, nrow = 1)
dev.off()

png("./plot/plot_relation/S11.png", width = 400, height = 200)
r11 <- grid.arrange(r9.1, r9.2, nrow = 1)
dev.off()

png("./plot/plot_relation/S12.png", width = 400, height = 200)
r12 <- grid.arrange(r9.2, r9.1, nrow = 1)
dev.off()

r13.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (1/(1+exp(-5*(-0.5*.x^2)) - 0.5)) , size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
r13.2 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (1/(1+exp(5*(-0.5*.x^2)) - 0.5)) , size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S13.png", width = 400, height = 200)
r13 <- grid.arrange(r13.1, r13.2, nrow = 1)
dev.off()

png("./plot/plot_relation/S14.png", width = 400, height = 200)
r14 <- grid.arrange(r13.2, r13.1, nrow = 1)
dev.off()

r15.1 <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) + stat_function(fun = ~ (-0.5*(-0.5*.x^2)^2) , size = 5) + labs(x="", y="") +theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +  theme_void() 
png("./plot/plot_relation/S15.png", width = 400, height = 200)
r15 <- grid.arrange(r15.1, r15.1, nrow = 1)
dev.off()

white <- ggdraw() + draw_image("C:/Users/Chero/Documents/Population _model/Population_model_matrix/plot/plot_relation/white.png")
colum_relation <- grid.arrange(white, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, nrow = 16)

#____________________________________________________________________________________________________________________
#Climate
png("./plot/plot_relation/C1.png", width = 200, height = 200)
ggplot(data.frame(t = 1:200, y = rnorm(200, 0, 0))) + aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()

png("./plot/plot_relation/C2.png", width = 200, height = 200)
ggplot(data.frame(t = 1:200, y = rnorm(200, 0, 0.2))) + aes(x = t, y = y)  + geom_line(size = 2) + ylim(-1, 1) + theme_void() 
dev.off()

png("./plot/plot_relation/C3.png", width = 200, height = 200)
ggplot(data.frame(t = 1:200, y = rnorm(200, 0, 0.5))) + aes(x = t, y = y)  + geom_line(size = 2) + ylim(-1, 1)  + theme_void() 
dev.off()

png("./plot/plot_relation/C4.png", width = 200, height = 200)
ggplot(data.frame(t = 1:201, y = (unlist(lapply(seq(0,0.3, 0.3/200), function(a){rnorm(1, 0, a)})))))+
                    aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()

png("./plot/plot_relation/C5.png", width = 200, height = 200)
ggplot(data.frame(t = 1:201, y = rev(unlist(lapply(seq(0,0.3, 0.3/200), function(a){rnorm(1, 0, a)})))))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()

png("./plot/plot_relation/C6.png", width = 200, height = 200)
ggplot(data.frame(t = 1:200, y = (noise(kind = "red", duration = 200 )@left)))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
 dev.off()

png("./plot/plot_relation/C7.png", width = 200, height = 200)
meaT_list <- seq(0, 2, length.out= 200)
ggplot(data.frame(t = 1:200, y = unlist(lapply(1:200, function(a){rnorm(1, meaT_list[a], 0)}))))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()

png("./plot/plot_relation/C8.png", width = 200, height = 200)
meaT_list <- seq(0, 2, length.out= 200)
ggplot(data.frame(t = 1:200, y = unlist(lapply(1:200, function(a){rnorm(1, meaT_list[a], 0.3)}))))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()

png("./plot/plot_relation/C9.png", width = 200, height = 200)
meaT_list <- seq(0, 4, length.out= 200)
SDT_list <-  seq(0, 1.2, length.out= 200)
ggplot(data.frame(t = 1:200, y = unlist(lapply(1:200, function(a){rnorm(1, meaT_list[a], SDT_list[a])}))))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()

png("./plot/plot_relation/C10.png", width = 200, height = 200)
meaT_list <- seq(0, 4, length.out= 200)
SDT_list <-  rev(seq(0, 1.2, length.out= 200))
ggplot(data.frame(t = 1:200, y = (unlist(lapply(1:200, function(a){rnorm(1, meaT_list[a], SDT_list[a])})))))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void() 
dev.off()


png("./plot/plot_relation/C11.png", width = 200, height = 200)
ggplot(data.frame(t = 1:200, y = (noise(kind = "red", duration = 200 )@left + seq(0, 4, length.out= 200))))+
  aes(x = t, y = y)  + geom_line(size = 2) +theme_void()
dev.off()



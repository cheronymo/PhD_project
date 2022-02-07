## libraries
library(tidyverse)
library(broom)
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





#Same GENTREND_______________________________________________________________________________________
out_K_trait_bind <- readRDS( file = "./output/Output_changepackage/Outpout_K_selected_strategies1.rds")
out_K_trait_bind$LHS <- "ExtSlow"
out_r_trait_bind <- readRDS( file = "./output/Output_changepackage/Outpout_r_selected_strategies.rds")
out_r_trait_bind$LHS <- "ExtFast"
out_rMix_trait_bind <- readRDS( file = "./output/Output_changepackage/Outpout_rMix_selected_strategies.rds")
out_rMix_trait_bind$LHS <- "Fast"
out_KMix_trait_bind <- readRDS( file = "./output/Output_changepackage/Outpout_KMix_selected_strategies.rds")
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



data_final <- data_output_gentrend %>% 
  unite("scenario_tot", scenario_trait,scenario, remove = FALSE) %>% 
  mutate(Sce_trait_x_demo = case_when(scenario_tot == "LinPo_1" ~ 'S1', scenario_tot == "LinNeg_1" ~ 'S2', scenario_tot == "SigPo_1" ~ 'S3', scenario_tot == "SigNeg_1" ~ 'S4',
                                      scenario_tot == "BellShape_1" ~ 'S11', scenario_tot == "LinPo_2" ~ 'S2', scenario_tot == "LinNeg_2" ~ 'S1', scenario_tot == "SigPo_2" ~ 'S4',
                                      scenario_tot == "SigNeg_2" ~ 'S3', scenario_tot == "BellShape_2" ~ 'S12', scenario_tot == "LinPo_3" ~ 'S3', scenario_tot == "LinNeg_3" ~ 'S4',
                                      scenario_tot == "SigPo_3" ~ 'S5', scenario_tot == "SigNeg_3" ~ 'S6', scenario_tot == "BellShape_3" ~ 'S13', scenario_tot == "LinPo_4" ~ 'S4', 
                                      scenario_tot == "LinNeg_4" ~ 'S3', scenario_tot == "SigPo_4" ~ 'S6', scenario_tot == "SigNeg_4" ~ 'S5', scenario_tot == "BellShape_4" ~ 'S14',
                                      scenario_tot == "LinPo_5" ~ 'S9', scenario_tot == "LinNeg_5" ~ 'S10', scenario_tot == "SigPo_5" ~ 'S7', scenario_tot == "SigNeg_5" ~ 'S8',
                                      scenario_tot == "BellShape_5" ~ 'S15'))  %>% 
  mutate(Sce_trait_x_demo_shape = case_when(scenario_tot == "LinPo_1" ~ 'Linear', scenario_tot == "LinNeg_1" ~ 'Linear', scenario_tot == "SigPo_1" ~ 'Sigmoid', scenario_tot == "SigNeg_1" ~ 'Sigmoid',
                                            scenario_tot == "BellShape_1" ~ 'BellShape', scenario_tot == "LinPo_2" ~ 'Linear', scenario_tot == "LinNeg_2" ~ 'Linear', scenario_tot == "SigPo_2" ~ 'Sigmoid',
                                            scenario_tot == "SigNeg_2" ~ 'Sigmoid', scenario_tot == "BellShape_2" ~ 'BellShape', scenario_tot == "LinPo_3" ~ 'Sigmoid', scenario_tot == "LinNeg_3" ~ 'Sigmoid',
                                            scenario_tot == "SigPo_3" ~ 'Sigmoid', scenario_tot == "SigNeg_3" ~ 'Sigmoid', scenario_tot == "BellShape_3" ~ 'BellShape', scenario_tot == "LinPo_4" ~ 'Sigmoid', 
                                            scenario_tot == "LinNeg_4" ~ 'Sigmoid', scenario_tot == "SigPo_4" ~ 'Sigmoid', scenario_tot == "SigNeg_4" ~ 'Sigmoid', scenario_tot == "BellShape_4" ~ 'BellShape',
                                            scenario_tot == "LinPo_5" ~ 'BellShape', scenario_tot == "LinNeg_5" ~ 'BellShape', scenario_tot == "SigPo_5" ~ 'Sigmoid', scenario_tot == "SigNeg_5" ~ 'Sigmoid',
                                            scenario_tot == "BellShape_5" ~ 'BellShape'))  %>% 
  mutate(Shape_trait = case_when(scenario_trait == "LinPo" ~ 'Linear', scenario_trait == "LinNeg" ~ 'Linear', 
                                 scenario_trait == "SigPo" ~ 'Sigmoid', scenario_trait == "SigNeg" ~ 'Sigmoid', 
                                 scenario_trait == "BellShape" ~ 'BellShape'
  )) %>% 
  mutate(Cat_climat = case_when(climate_scenario == "nothing" ~ 'No change in mean', climate_scenario == "Sdt0.2" ~ 'No change in mean', climate_scenario == "Sdt0.5" ~ 'No change in mean', climate_scenario == "Inc0.3" ~ 'No change in mean', climate_scenario == "Deac0.3" ~ 'No change in mean', climate_scenario == "red" ~ 'No change in mean',
                                climate_scenario == "MeanIn" ~ 'Change in mean', climate_scenario == "MeanIn.0.3" ~ 'Change in mean', climate_scenario == "M.SdInc" ~ 'Change in mean', climate_scenario == "M.SdDeac" ~ 'Change in mean', climate_scenario == "M.In.Red" ~ 'Change in mean' )) %>% 
  
  mutate(Shape_demo = case_when(scenario_demo == "Linear1" ~ 'Linear', scenario_demo == "Linear2" ~ 'Linear', 
                                scenario_demo == "Sigmoid1" ~ 'Sigmoid', scenario_demo == "Sigmoid2" ~ 'Sigmoid', 
                                scenario_demo == "BellShape" ~ 'BellShape')) %>% 
  mutate(Climate = case_when(climate_scenario == "nothing" ~ "Baseline", climate_scenario == "Sdt0.2" ~ "LowFlu.", climate_scenario == "Sdt0.5" ~ "HighFlu.", climate_scenario == "Inc0.3" ~ "Inc.Flu.",
                             climate_scenario == "Deac0.3" ~ "Deac.Flu.", climate_scenario == "MeanIn" ~ "MeanInc.", climate_scenario == "MeanIn.0.3" ~ "MeanInc.Flu", climate_scenario == "M.SdInc" ~ "MeanInc.IncFlu",
                             climate_scenario == "M.SdDeac" ~ "MeanInc.DeacFlu", climate_scenario == "M.In.Red" ~ "MeanInc.Auto", climate_scenario == "red" ~ "Temp.Auto"))



data_final$Sce_trait_x_demo = factor(data_final$Sce_trait_x_demo, levels=c('S1','S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12', 'S13', 'S14', 'S15'))
data_final$Sce_trait_x_demo_shape = factor(data_final$Sce_trait_x_demo_shape, levels=c('Linear', 'Sigmoid','BellShape'))
data_final$Shape_trait = factor(data_final$Shape_trait, levels=c('Linear', 'Sigmoid','BellShape'))
data_final$Shape_demo = factor(data_final$Shape_demo, levels=c('Linear', 'Sigmoid','BellShape'))
data_final$Cat_climat = factor(data_final$Cat_climat, levels=c('No change in mean', 'Change in mean'))
data_final$Climate = factor(data_final$Climate, 
                            levels = c("Baseline", "LowFlu.", "HighFlu.", "Inc.Flu.", "Deac.Flu.", "Temp.Auto",
                                       "MeanInc.", "MeanInc.Flu", "MeanInc.IncFlu", "MeanInc.DeacFlu", "MeanInc.Auto"))


## fit lm's to each setup
prep1 <- data_final %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo, EXT_time) %>%
  dplyr::summarize(n = n()) %>%
  #right_join(expand(., t_extinct = 1:520)) %>% 
  #replace_na(list(n = 0)) %>% 
  #filter(t_extinct > quantile(t_extinct, 0.025),
  #       t_extinct < quantile(t_extinct, 0.975)) %>% 
  mutate(cumsum = cumsum(n),
         P0 = cumsum / 150,
         ln_oneminusP0 = -log(1 - P0)) %>%
  mutate(ln_oneminusP0 = replace(ln_oneminusP0, is.infinite(ln_oneminusP0), NA)) %>% 
  tidyr::drop_na(EXT_time)  %>% 
  filter(n > 3)


fits1 <- prep1 %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo) %>%
  do(broom::tidy(lm(ln_oneminusP0 ~ EXT_time, data = ., na.action = "na.omit")))

fits2 <- prep1 %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo) %>%
  do(fit = lm(ln_oneminusP0 ~ EXT_time, data = ., na.action = "na.omit"))

## calculate c1 and Tm
fits_Tm1 <- fits1 %>% 
  dplyr::select(LHS, Cat_climat, Sce_trait_x_demo, term, estimate) %>%
  spread(term, estimate) %>%
  dplyr::rename(intercept = `(Intercept)`, slope = EXT_time) %>% 
  mutate(c1 = exp(abs(intercept)), Tm = 1 / slope) %>% 
  ungroup()

## keep info on standard error to remove NaN's
error <- fits1 %>% 
  filter(term == "EXT_time") %>% 
  dplyr::select(LHS, Cat_climat,Sce_trait_x_demo, std.error)

## keep info on number of points
ns <- prep1 %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo) %>% 
  summarize(n_all = max(cumsum))

## extract R2
R_sqrd <- map_dbl(c(1:nrow(fits2)), ~ as.numeric(summary(fits2[4][[.x,1]])$r.squared))

## final table
fits_final <- fits_Tm1 %>% 
  ungroup() %>% 
  mutate(R2 = R_sqrd) %>% 
  full_join(error) %>%
  #full_join(ns) %>%
  mutate(c1 = replace(c1, R2==0, NA), 
         Tm = replace(Tm, R2==0, NA),
         c1_yrs = c1 / 52, 
         Tm_yrs = Tm / 52) %>% 
  dplyr::select(LHS, Cat_climat, Sce_trait_x_demo, R2, c1_yrs, Tm_yrs) %>% 
  arrange(-R2)


plot_Extintion_time_Tm <- fits_final %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo) %>% 
  ggplot() + aes(x = LHS, y = Tm_yrs) +
  facet_grid(~ Cat_climat) +
  geom_boxplot() +
  geom_jitter(data = fits_final, aes(x = LHS , y = Tm_yrs, colour = Sce_trait_x_demo ))+
  labs(title = "Tm - Intrinsect mean \n time to extinction", 
       x = "Life history strategies", y = "Extinction time") +
  theme(legend.position = "none")
  

plot_Extintion_time_1Tm <- fits_final %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo) %>% 
  ggplot() + aes(x = LHS, y = (1/Tm_yrs)) +
  facet_grid(~ Cat_climat) +
  geom_boxplot() +
  geom_jitter(data = fits_final, aes(x = LHS , y = (1/Tm_yrs), colour = Sce_trait_x_demo ))+
  labs(title = "1/Tm - Risk of extinction", 
       x = "Life history strategies", y = "Extinction time") +
  theme(legend.position = "none")


plot_Extintion_time_c1 <- fits_final %>% 
  group_by(LHS, Cat_climat, Sce_trait_x_demo) %>% 
  ggplot() + aes(x = LHS, y = c1_yrs) +
  facet_grid(~ Cat_climat) +
  geom_boxplot() +
  geom_jitter(data = fits_final, aes(x = LHS , y = c1_yrs, colour = Sce_trait_x_demo ))+
  ylim (0,1)+
  labs(title = "c1 - Probability for reaching \n the established phase", 
       x = "Life history strategies", y = "Extinction time", colour = "Shape") 


pdf("./plot/plot_hypothese/Intrinsect_mean_ExtinctionTime.pdf", 15, 5)
ggarrange(plot_Extintion_time_Tm, plot_Extintion_time_1Tm
          , plot_Extintion_time_c1,  nrow = 1, ncol = 3)
dev.off()  

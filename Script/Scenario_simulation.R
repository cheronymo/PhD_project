####################################################################################################################
#################         CODE SIMULATION                ###########################################################
####################################################################################################################
#This code contains the code for simulate the population model. 

#set the emplacement for the GIS server
setwd("U:/GUEST/Abteilung6/People/Chero_Guillaume/Outputcluster/Output_update")

library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library(dplyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)
library(popbio)
library(raster)

#Parameters
runs <- 100
#nyear <- 200
number_generation <- 100

#List of scenario for the trait-demographic rate relationships
scenario_dem <- t(matrix ( ncol = 6, nrow = 7,
                           #shape       x2Sj  xSj  x2Sa  xSa  x2F  xF    (x2 = x square)
                           c("quadratic", 0,   1,   0,    1,   0, -1,
                             "quadratic", 0,   -1,  0,   -1,   0,  1,
                             "sigmoid", 0,      1,  0,    1,   0, -1,
                             "sigmoid", 0,      -1, 0,   -1,   0,  1,
                             "quadratic",0.5,    0, 0.5,   0, -0.5, 0,
                             "quadratic",-0.5,   0, -0.5,  0,  0.5, 0
                          ))) #curve

#List of scenario for the reaction norm
scenario_trait <- t(matrix(ncol = 5, nrow = 3, 
                            #shape       x   x2
                           c("quadratic", 0, 1,   #linear po
                             "quadratic", 0, -1,  #linear neg
                             "sigmoid", 0, 1,     #sigmoid po
                             "sigmoid", 0, -1,    #sigmoid neg  
                             "quadratic", -0.5, 0 #quadratic
                           )))

#List of climatic scenario
#                                          mean SDT phi  noise    dynamic    mean
VAR_list <- t(matrix(ncol = 11, nrow = 6, c(0,   0,  0, "white", "constant", "constant",
                                            0, 0.2,  0,  "white", "constant","constant",
                                            0, 0.5,  0, "white", "constant","constant",
                                            0, 0.006, 0,  "white", "increase","constant",
                                            0, 0.006, 0,  "white", "decrease","constant",
                                            0, 0.35,  0.9, "white", "constant", "constant",
                                            0.02, 0,  0,  "white", "constant", "increase",
                                            0.02, 0.35, 0,  "white", "constant", "increase",
                                            0.02, 0.006, 0, "white", "increase", "increase",
                                            0.02, 0.006, 0, "white", "decrease", "increase", 
                                            0.02, 0.35,  0.9,  "red", "red", "increase" )))

#                                     GenLenght   SjInter SaInter  FecInter Trans
LHS_list <- t(matrix(ncol = 4, nrow = 5, c( 17.2,  0.081,   3.15,   -1.18,  0.3,  #extra slow
                                            6.23,  -0.33,   1.49,    0.02,  0.4,  #slow
                                            2.43,  -1.33,  -0.96,    1.62,  0.7,  #fast               
                                            2.02,  -2.15,  -4.57,     2.4,  0.9   #extra fast
                                            )))
#___________________________________________________________________________________________________________________
#####Function output

# PLot function to check results 
function_plot <- function(dataset){
  
  plot(1,1,pch = "",ylim = c(0,500), xlim = c(0,200+1), xlab = "Years", ylab="Abundance", xaxt="n")  # set up blank plot
  cols <- rainbow(length(VAR))    # set up colors to use
  legend(0, 4000, legend=c("VAR 0", "VAR 1", "VAR 2"),
         col=c(cols[1], cols[2], cols[3], cols[4], cols[5]), lty=1, cex=0.5)
  for(var in 1:length(VAR)){
    data_var <- dataset[[var]]
    for(s in 1:runs){
      points(data_var$rep[s]$data_tot$Abundance, type = "l",lwd = 1, col = cols[var])     # plot out each life stage abundance, one at a time
    }} 
  
}

#Create summary !!!this one is the function needed for multi-simulation!!!
function_summary <- function(dataset){
  
  summary_var <- lapply(1:dim(VAR_list)[1], function(var){lapply(1:runs, function(laps){data.frame(
    Mean = mean(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    SD = sd(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    SD_sdt = sd(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]) /  mean(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    CV = cv(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear], na.rm = TRUE),
    GR = mean(log(dataset[[var]]$rep[laps]$data_tot$Growth_rate[(nyear/2):nyear])),
    GR_sdt = mean(log(dataset[[var]]$rep[laps]$data_tot$Growth_rate[(nyear/2):nyear])) / mean(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    
    EXT = sum(do.call(rbind, (lapply(1:runs , function(tour){
      tail(dataset[[var]]$rep[tour]$data_tot$Abundance, n=1) <= 0.99 })))) / runs, 
    
    EXT_time = lapply((min(which(dataset[[var]]$rep[laps]$data_tot$Abundance < 1))), function(x) replace(x, is.infinite(x),NA)),
    

    Clim_Var = var)}
    
  )})
  
  summary_multi_var <-  as.data.frame(t(matrix(unlist(summary_var), nrow = 9)))
  setnames(summary_multi_var, old=c("V1","V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9"), new=c("Abundance", "SDT", "SDT_sdt", "CV","log_GR", "log_GR_sdt", "EXT", "EXT_time", "i"))
  
  return(summary_multi_var)
}


# PLot summary
function_plotSum <- function(dataset_sum){
  
  #_______________________________________________________________________________________________________________
  #Graphe
  #AbundanceAB <- ggplot(outpout_ab, aes(x=outpout_ab$i , y=outpout_ab$Abundance,group = outpout_ab$i))+
  AB <- ggplot(dataset_sum, aes(x=dataset_sum$i , y=dataset_sum$Abundance, group = interaction(dataset_sum$LHS, dataset_sum$i)))+
    geom_boxplot(aes(colour = factor(dataset_sum$LHS ))) +
    theme(legend.title = element_blank()) +
    labs(title = "Abundance function of climatic variation") +
    theme(legend.position="bottom")+
    xlab("Climatic Variation") + 
    ylab("Abundance") 
  AB
  
  StD <- ggplot(dataset_sum, aes(x=dataset_sum$i , y=dataset_sum$SDT, group = interaction(dataset_sum$LHS, dataset_sum$i)))+
    geom_boxplot(aes(colour = factor(dataset_sum$LHS ))) +
    theme(legend.title = element_blank()) +
    labs(title = "Standard deviation of the population size function of \n climatic variation") +
    theme(legend.position="bottom")+
    xlab("Climatic Variation") + 
    ylab("Standard deviation of \n the population size") 
  StD
  
  GR <- ggplot(dataset_sum, aes(x=dataset_sum$i , y=dataset_sum$log_GR, group = interaction(dataset_sum$LHS, dataset_sum$i)))+
    geom_boxplot(aes(colour = factor(dataset_sum$LHS ))) +
    theme(legend.title = element_blank()) +
    labs(title = "Growth rate function of climatic variation") +
    theme(legend.position="bottom")+
    xlab("Climatic Variation") + 
    ylab("Growth rate") 
  GR
  
  EXT <- ggplot(dataset_sum, aes(x=dataset_sum$i , y=dataset_sum$EXT, group = interaction(dataset_sum$LHS, dataset_sum$i)))+
    geom_boxplot(aes(colour = factor(dataset_sum$LHS ))) +
    theme(legend.title = element_blank()) +
    labs(title = "Extinction probability function of climatic variation") +
    theme(legend.position="bottom")+
    xlab("Climatic Variation") + 
    ylab("Extinction probability") 
  EXT
  
  
  figure <- plot_grid(AB,StD,GR, EXT, ncol = 2, nrow = 2)
  return(figure)
  
}


#____________________________________________________________________________________
#Simulation extreme slow

nyear = number_generation * LHS_list[1,1] # 17.2 the average lenght of generation calulcated by hand.
#nyear = 100
  
out_K_trait <- lapply(1:dim(scenario_trait)[1], function(parametre_trait){
  para_list_trait <- scenario_trait[parametre_trait,]
  
  out_K_dem <- lapply(1:dim(scenario_dem)[1], function(parametre_dem){
    para_list <- scenario_dem[parametre_dem,]
    
    multi_var_K_EXTR <- lapply(1:dim(VAR_list)[1], function(i){tibble(rep = replicate(runs, model_base(
      nYears = nyear,
      meanT = as.numeric(VAR_list[i,1]),
      SDT = as.numeric(VAR_list[i,2]),
      phi = as.numeric(VAR_list[i,3]),
      Noise = VAR_list[i,4], climatic.fluctuation = VAR_list[i,5], climatic.mean =  VAR_list[i,6], 
      #Intercept
      inter_surv_juv = LHS_list[1,2],
      inter_surv_adu = LHS_list[1,3],
      inter_fec_adu = LHS_list[1,4],
      
      transition.proba = LHS_list[1,5],
      #TRAIT
      trait.function = para_list_trait[1], #quadratic / sigmoid
      
      beta_2trait_juv = as.numeric(para_list_trait[2]) ,      # beta square in the relation of T on trait mean in Juvenil
      beta_trait_juv =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Juvenil
      
      beta_2trait_ad =  as.numeric(para_list_trait[2]),  
      beta_trait_ad =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Adult
      
      #DEMOGRAPHIC RATE
      demo.function = para_list[1], 
      
      beta_2surv_juv = as.numeric(para_list[2]), 
      beta_2surv_adu = as.numeric(para_list[4]),
      beta_2fec_adu = as.numeric(para_list[6]),
      
      beta_surv_juv = as.numeric(para_list[3]),
      beta_surv_adu = as.numeric(para_list[5]),
      beta_fec_adu = as.numeric(para_list[7])
      
      
      
      
      
    ))) })
  
  KExt_curve <- function_summary(multi_var_K_EXTR)
  KExt_curve$dem <- parametre_dem
  KExt_curve$trait <- parametre_trait
  
  return(KExt_curve)
})
  
  return(out_K_dem)
  
  
}  )

out_K_trait

#bind all the list
out_K_trait_bind <- do.call(rbind,do.call(rbind,out_K_trait))

#add column name for scenario 
out_K_trait_bind <- out_K_trait_bind %>%
  mutate(scenario_demo = case_when(dem == "1" ~ 'Linear1', dem == "2" ~ 'Linear2',       #
                                   dem == "3" ~ 'Sigmoid1', dem == "4" ~ 'Sigmoid2', 
                                   dem == "5" ~ 'BellShape1', dem == "6" ~ 'BellShape2'))
out_K_trait_bind <- out_K_trait_bind %>%
  mutate(scenario_trait = case_when(trait == "1" ~ 'LinPo', trait == "2" ~ 'LinNeg', 
                                    trait == "3" ~ 'SigPo', trait == "4" ~ 'SigNeg',  
                                    trait == "5" ~ 'BellShape'))
out_K_trait_bind <- out_K_trait_bind %>%
  mutate(climate_scenario = case_when(i == "1" ~ 'nothing', i == "2" ~ 'Sdt0.2', i == "3" ~ 'Sdt0.5', 
                                      i == "4" ~ 'Inc0.3', i == "5" ~ 'Deac0.3', i == "6" ~ 'red',
                                      i == "7" ~ 'MeanIn', i == "8" ~ 'MeanIn.0.3',i == "9" ~ 'M.SdInc', 
                                      i == "10" ~ 'M.SdDeac', i == "11" ~ 'M.In.Red'
  ))

#save
saveRDS(out_K_trait_bind , file = "Outpout_K_selected_strategies_100years.rds")
out_K_trait_bind <- readRDS( file = "./output/output2/Outpout_K_selected_strategies_100years.rds")

#plot just for checking
tot_plot <- ggplot(out_K_trait_bind, aes(x=climate_scenario , y=EXT, color = climate_scenario))+
  geom_boxplot() + 
  facet_grid(scenario_trait ~ scenario_demo)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot


#____________________________________________________________________________________
#Simulation slow
nyear = number_generation * LHS_list[2,1]
nyear = 100

out_Kmix_trait <- lapply(1:dim(scenario_trait)[1], function(parametre_trait){
  para_list_trait <- scenario_trait[parametre_trait,]
  
  
  out_Kmix_dem <- lapply(1:dim(scenario_dem)[1], function(parametre_dem){
    para_list <- scenario_dem[parametre_dem,]
    
    multi_var_K_mix <- lapply(1:dim(VAR_list)[1], function(i){tibble(rep = replicate(runs, model_base(
      nYears = nyear,
      meanT = as.numeric(VAR_list[i,1]),
      SDT = as.numeric(VAR_list[i,2]),
      phi = as.numeric(VAR_list[i,3]),
      Noise = VAR_list[i,4], climatic.fluctuation = VAR_list[i,5], climatic.mean =  VAR_list[i,6],  
      #Intercept
      inter_surv_juv = LHS_list[2,2],
      inter_surv_adu =  LHS_list[2,3],
      inter_fec_adu =  LHS_list[2,4],
      
      transition.proba =  LHS_list[2,5],
      #TRAIT
      trait.function = para_list_trait[1], #quadratic / sigmoid
      
      beta_2trait_juv = as.numeric(para_list_trait[2]) ,      # beta square in the relation of T on trait mean in Juvenil
      beta_trait_juv =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Juvenil
      
      beta_2trait_ad =  as.numeric(para_list_trait[2]),  
      beta_trait_ad =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Adult
      
      #DEMOGRAPHIC RATE
      demo.function = para_list[1], 
      
      beta_2surv_juv = as.numeric(para_list[2]), 
      beta_2surv_adu = as.numeric(para_list[4]),
      beta_2fec_adu = as.numeric(para_list[6]),
      
      beta_surv_juv = as.numeric(para_list[3]),
      beta_surv_adu = as.numeric(para_list[5]),
      beta_fec_adu = as.numeric(para_list[7])
      
      
      
      
      
    ))) })
    
    Kmix_curve <- function_summary(multi_var_K_mix)
    Kmix_curve$dem <- parametre_dem
    Kmix_curve$trait <- parametre_trait
    
    return(Kmix_curve)
  })
  
  return(out_Kmix_dem)
  
  
}  )

out_Kmix_trait
out_Kmix_trait_bind <- do.call(rbind,do.call(rbind, out_Kmix_trait))

out_Kmix_trait_bind <- out_Kmix_trait_bind %>%
  mutate(scenario_demo = case_when(dem == "1" ~ 'Linear1', dem == "2" ~ 'Linear2', 
                                   dem == "3" ~ 'Sigmoid1', dem == "4" ~ 'Sigmoid2', 
                                   dem == "5" ~ 'BellShape1', dem == "6" ~ 'BellShape2'))

out_Kmix_trait_bind <- out_Kmix_trait_bind %>%
  mutate(scenario_trait = case_when(trait == "1" ~ 'LinPo', trait == "2" ~ 'LinNeg', 
                                    trait == "3" ~ 'SigPo', trait == "4" ~ 'SigNeg',  
                                    trait == "5" ~ 'BellShape'))

out_Kmix_trait_bind <- out_Kmix_trait_bind %>%
  mutate(climate_scenario = case_when(i == "1" ~ 'nothing', i == "2" ~ 'Sdt0.2', i == "3" ~ 'Sdt0.5', 
                                      i == "4" ~ 'Inc0.3', i == "5" ~ 'Deac0.3', i == "6" ~ 'red',
                                      i == "7" ~ 'MeanIn', i == "8" ~ 'MeanIn.0.3',i == "9" ~ 'M.SdInc', 
                                      i == "10" ~ 'M.SdDeac', i == "11" ~ 'M.In.Red'
  ))


saveRDS(out_Kmix_trait_bind , file = "Outpout_Kmix_selected_strategiesyear_100years.rds")
out_Kmix_trait_bind <- readRDS( file = "./output/output2/Outpout_Kmix_selected_strategies_100years.rds")

tot_plot <- ggplot(out_Kmix_trait_bind, aes(x=climate_scenario , y=EXT, color = climate_scenario))+
  geom_boxplot() + 
  facet_grid(scenario_trait ~ scenario_demo)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot

#____________________________________________________________________________________
#Simulation fast
nyear = number_generation  *  LHS_list[3,1]
nyear = 100

out_rmix_trait <- lapply(1:dim(scenario_trait)[1], function(parametre_trait){
  para_list_trait <- scenario_trait[parametre_trait,]
  
  
  out_rmix_dem <- lapply(1:dim(scenario_dem)[1], function(parametre_dem){
    para_list <- scenario_dem[parametre_dem,]
    
    multi_var_r_mix <- lapply(1:dim(VAR_list)[1], function(i){tibble(rep = replicate(runs, model_base(
      nYears = nyear,
      meanT = as.numeric(VAR_list[i,1]),
      SDT = as.numeric(VAR_list[i,2]),
      phi = as.numeric(VAR_list[i,3]),
      Noise = VAR_list[i,4], climatic.fluctuation = VAR_list[i,5], climatic.mean =  VAR_list[i,6], 
      #Intercept
      inter_surv_juv = LHS_list[3,2],
      inter_surv_adu = LHS_list[3,3],
      inter_fec_adu = LHS_list[3,4],
      
      transition.proba = LHS_list[3,5],
      #TRAIT
      trait.function = para_list_trait[1], #quadratic / sigmoid
      
      beta_2trait_juv = as.numeric(para_list_trait[2]) ,      # beta square in the relation of T on trait mean in Juvenil
      beta_trait_juv =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Juvenil
      
      beta_2trait_ad =  as.numeric(para_list_trait[2]),  
      beta_trait_ad =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Adult
      
      #DEMOGRAPHIC RATE
      demo.function = para_list[1], 
      
      beta_2surv_juv = as.numeric(para_list[2]), 
      beta_2surv_adu = as.numeric(para_list[4]),
      beta_2fec_adu = as.numeric(para_list[6]),
      
      beta_surv_juv = as.numeric(para_list[3]),
      beta_surv_adu = as.numeric(para_list[5]),
      beta_fec_adu = as.numeric(para_list[7])
      
      
      
      
      
    ))) })
    
    rmix_curve <- function_summary(multi_var_r_mix)
    rmix_curve$dem <- parametre_dem
    rmix_curve$trait <- parametre_trait
    
    return(rmix_curve)
  })
  
  return(out_rmix_dem)
  
  
}  )

out_rmix_trait
out_rmix_trait_bind <- do.call(rbind,do.call(rbind, out_rmix_trait))

out_rmix_trait_bind <- out_rmix_trait_bind %>%
  mutate(scenario_demo = case_when(dem == "1" ~ 'Linear1', dem == "2" ~ 'Linear2', 
                                   dem == "3" ~ 'Sigmoid1', dem == "4" ~ 'Sigmoid2', 
                                   dem == "5" ~ 'BellShape1', dem == "6" ~ 'BellShape2'))

out_rmix_trait_bind <- out_rmix_trait_bind %>%
  mutate(scenario_trait = case_when(trait == "1" ~ 'LinPo', trait == "2" ~ 'LinNeg', 
                                    trait == "3" ~ 'SigPo', trait == "4" ~ 'SigNeg',  
                                    trait == "5" ~ 'BellShape'))

out_rmix_trait_bind <- out_rmix_trait_bind %>%
  mutate(climate_scenario = case_when(i == "1" ~ 'nothing', i == "2" ~ 'Sdt0.2', i == "3" ~ 'Sdt0.5', 
                                      i == "4" ~ 'Inc0.3', i == "5" ~ 'Deac0.3', i == "6" ~ 'red',
                                      i == "7" ~ 'MeanIn', i == "8" ~ 'MeanIn.0.3',i == "9" ~ 'M.SdInc', 
                                      i == "10" ~ 'M.SdDeac', i == "11" ~ 'M.In.Red'
  ))



saveRDS(out_rmix_trait_bind , file = "Outpout_rmix_selected_strategies_100years.rds")
out_rmix_trait_bind <- readRDS( file = "./output/output2/Outpout_rmix_selected_strategies_100years.rds")

tot_plot <- ggplot(out_rmix_trait_bind, aes(x=climate_scenario , y=EXT, color = climate_scenario))+
  geom_boxplot() + 
  facet_grid(scenario_trait ~ scenario_demo)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot



#____________________________________________________________________________________
#Simulation extreme fast
nyear = number_generation  * LHS_list[4,1]
nyear = 100

out_r_trait <- lapply(1:dim(scenario_trait)[1], function(parametre_trait){
  para_list_trait <- scenario_trait[parametre_trait,]
  
  
  out_r_dem <- lapply(1:dim(scenario_dem)[1], function(parametre_dem){
    para_list <- scenario_dem[parametre_dem,]
    
    multi_var_r_EXTR <- lapply(1:dim(VAR_list)[1], function(i){tibble(rep = replicate(runs, model_base(
      nYears = nyear,
      meanT = as.numeric(VAR_list[i,1]),
      SDT = as.numeric(VAR_list[i,2]),
      phi = as.numeric(VAR_list[i,3]),
      Noise = VAR_list[i,4], climatic.fluctuation = VAR_list[i,5], climatic.mean =  VAR_list[i,6], 
      #Intercept
      inter_surv_juv = LHS_list[4,2],
      inter_surv_adu = LHS_list[4,3],
      inter_fec_adu = LHS_list[4,4],
      
      transition.proba = LHS_list[4,5],
      #TRAIT
      trait.function = para_list_trait[1], #quadratic / sigmoid
      
      beta_2trait_juv = as.numeric(para_list_trait[2]) ,      # beta square in the relation of T on trait mean in Juvenil
      beta_trait_juv =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Juvenil
      
      beta_2trait_ad =  as.numeric(para_list_trait[2]),  
      beta_trait_ad =  as.numeric(para_list_trait[3]),      # beta in the relation of T on trait mean in Adult
      
      #DEMOGRAPHIC RATE
      demo.function = para_list[1], 
      
      beta_2surv_juv = as.numeric(para_list[2]), 
      beta_2surv_adu = as.numeric(para_list[4]),
      beta_2fec_adu = as.numeric(para_list[6]),
      
      beta_surv_juv = as.numeric(para_list[3]),
      beta_surv_adu = as.numeric(para_list[5]),
      beta_fec_adu = as.numeric(para_list[7])
      
      
      
      
      
    ))) })
    
    rExt_curve <- function_summary(multi_var_r_EXTR)
    rExt_curve$dem <- parametre_dem
    rExt_curve$trait <- parametre_trait
    
    return(rExt_curve)
  })
  
  return(out_r_dem)
  
  
}  )

out_r_trait
out_r_trait_bind <- do.call(rbind,do.call(rbind, out_r_trait))

out_r_trait_bind <- out_r_trait_bind %>%
  mutate(scenario_demo = case_when(dem == "1" ~ 'Linear1', dem == "2" ~ 'Linear2', 
                                   dem == "3" ~ 'Sigmoid1', dem == "4" ~ 'Sigmoid2', 
                                   dem == "5" ~ 'BellShape1', dem == "6" ~ 'BellShape2'))

out_r_trait_bind <- out_r_trait_bind %>%
  mutate(scenario_trait = case_when(trait == "1" ~ 'LinPo', trait == "2" ~ 'LinNeg', 
                                    trait == "3" ~ 'SigPo', trait == "4" ~ 'SigNeg',  
                                    trait == "5" ~ 'BellShape'))

out_r_trait_bind <- out_r_trait_bind %>%
  mutate(climate_scenario = case_when(i == "1" ~ 'nothing', i == "2" ~ 'Sdt0.2', i == "3" ~ 'Sdt0.5', 
                                      i == "4" ~ 'Inc0.3', i == "5" ~ 'Deac0.3', i == "6" ~ 'red',
                                      i == "7" ~ 'MeanIn', i == "8" ~ 'MeanIn.0.3',i == "9" ~ 'M.SdInc', 
                                      i == "10" ~ 'M.SdDeac', i == "11" ~ 'M.In.Red'
  ))

saveRDS(out_r_trait_bind , file = "Outpout_r_selected_strategies_100years.rds")
out_r_trait_bind <- readRDS( file = "./output/output2/Outpout_r_selected_strategies_100years.rds")

tot_plot <- ggplot(out_r_trait_bind, aes(x=climate_scenario , y=EXT, color = climate_scenario))+
  geom_boxplot() + 
  facet_grid(scenario_trait ~ scenario_demo)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot


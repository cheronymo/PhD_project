####################################################################################################################
#################         CODE SIMULATION TIMESERIES                ###########################################################
####################################################################################################################
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
library(gridExtra )
library(ggpubr)

#Parameters
runs <- 2
#nyear <- 500
number_generation <- 100

scenario_dem <- t(matrix ( ncol = 5, nrow = 7,
                           c("quadratic", 0, 1, 0, 1, 0, -1,
                             "quadratic", 0, -1, 0, -1, 0, 1,
                             "sigmoid", 0, 1, 0, 1, 0, -1,
                             "sigmoid", 0, -1, 0, -1, 0, 1,
                             "quadratic", -0.5, 0, -0.5, 0, -0.5, 0
                           ))) #curve

scenario_trait <- t(matrix(ncol = 5, nrow = 3, 
                           c("quadratic", 0, 1,   #linear po
                             "quadratic", 0, -1,  #linear neg
                             "sigmoid", 0, 1,     #sigmoid po
                             "sigmoid", 0, -1,    #sigmoid neg  
                             "quadratic", -0.5, 0 #quadratic
                           )))

#                                          mean SDT phi  noise    dynamic    mean
VAR_list <- t(matrix(ncol = 11, nrow = 6, c(0,   0,  0, "white", "constant", "constant",
                                            0, 0.2,  0,  "white", "constant","constant",
                                            0, 0.5,  0, "white", "constant","constant",
                                            0, 0.006, 0,  "white", "increase","constant",
                                            0, 0.006, 0,  "white", "decrease","constant",
                                            0, 0.35,  0.9, "red", "constant", "constant",
                                            0.02, 0,  0,  "white", "constant", "increase",
                                            0.02, 0.35, 0,  "white", "constant", "increase",
                                            0.02, 0.006, 0, "white", "increase", "increase",
                                            0.02, 0.006, 0, "white", "decrease", "increase", 
                                            0.02, 0.35,  0.9,  "red", "red", "increase" )))


#___________________________________________________________________________________________________________________
##### Sigmoid on slow
runs <- 10


strategies <- t(matrix(ncol = 4, nrow = 5, 
                       c(1750, 0.081, 3.15, -1.18, 0.3, #ext slow
                         670, -0.33, 1.49,  0.02, 0.4, #slow
                         250, -1.33, -0.96, 1.62, 0.7, #fast
                         200, -2.15, -4.57, 2.4,  0.9  #ext fast
                       )))


loop <- lapply(1:dim(strategies)[1], function(a){
  stra <- strategies[a,]

multi_var_K_EXTR <- tibble(rep = (replicate(runs, model_base(
      nYears = stra[1],
      meanT = 0.02,
      SDT = 0.35,
      phi = 0,
      Noise = "white", climatic.fluctuation = "constant", climatic.mean =  "increase",
      #Intercept
      inter_surv_juv = stra[2],
      inter_surv_adu = stra[3],
      inter_fec_adu = stra[4],
      
      transition.proba = stra[5],
      #TRAIT
      trait.function = "Sigmoid", #quadratic / sigmoid
      
      beta_2trait_juv = 0 ,      # beta square in the relation of T on trait mean in Juvenil
      beta_trait_juv =  1,      # beta in the relation of T on trait mean in Juvenil
      
      beta_2trait_ad =  0,  
      beta_trait_ad =  1,      # beta in the relation of T on trait mean in Adult
      
      #DEMOGRAPHIC RATE
      demo.function = "quadratic", 
      
      beta_2surv_juv = 0, 
      beta_2surv_adu = 0,
      beta_2fec_adu = 0,
      
      beta_surv_juv = -1,
      beta_surv_adu = -1,
      beta_fec_adu = 1
    ))))
})


loop

ext_slow_data <- loop[[1]]
slow_data <- loop[[2]]
fast_data <- loop[[3]]
ext_fast_data <- loop[[4]]

plot_time <- function(data){
  
data_timeseries <- lapply(1:runs, function(i){ 
  data$rep[i]$data_tot$Abundance
})
data_timeseries <- data.frame(do.call(cbind, data_timeseries))
data_timeseries <- cbind(data.frame(time = c(1:dim(data_timeseries))), data_timeseries)

ggplot(data_timeseries, aes(x = time, y = X1)) +
  geom_line(col = "blue") + 
  ylab("Abundance") +
  geom_line(aes(y = X2), col = "red")+ 
  geom_line(aes(y = X3), col = "pink")+ 
  geom_line(aes(y = X4), col = "yellow")+ 
  geom_line(aes(y = X6), col = "orange")+ 
  geom_line(aes(y = X8), col = "green")+ 
  geom_line(aes(y = X9), col = "brown")+ 
  geom_line(aes(y = X10), col = "purple")
} 

ggarrange(
plot_time(ext_slow_data),
plot_time(slow_data),
plot_time(fast_data),
plot_time(ext_fast_data), labels = c("E-K", "K", "r", "E-r"))

#plot slow

VAR_list <- t(matrix(ncol = 5, nrow = 6, c(0.02, 0,  0,  "white", "constant", "increase",
                                           0.02, 0.35, 0,  "white", "constant", "increase",
                                           0.02, 0.006, 0, "white", "increase", "increase",
                                           0.02, 0.006, 0, "white", "decrease", "increase", 
                                           0.02, 0.35,  0.9,  "red", "red", "increase" )))


loop <- lapply(1:dim(VAR_list)[1], function(a){
  stra <- VAR_list[a,]
  
  multi_var_K_EXTR <- tibble(rep = (replicate(runs, model_base(
    nYears = 670,
    meanT = as.numeric(stra[1]),
    SDT = as.numeric(stra[2]),
    phi = as.numeric(stra[3]),
    Noise = as.numeric(stra[4]), climatic.fluctuation = stra[5], climatic.mean =  stra[6],
    #Intercept
    inter_surv_juv = -0.33,
    inter_surv_adu = 1.49,
    inter_fec_adu = 0.02,
    
    transition.proba = 0.4,
    #TRAIT
    trait.function = "quadratic", #quadratic / sigmoid
    
    beta_2trait_juv = 0 ,      # beta square in the relation of T on trait mean in Juvenil
    beta_trait_juv =  1,      # beta in the relation of T on trait mean in Juvenil
    
    beta_2trait_ad =  0,  
    beta_trait_ad =  1,      # beta in the relation of T on trait mean in Adult
    
    #DEMOGRAPHIC RATE
    demo.function = "quadratic", 
    
    beta_2surv_juv = 0, 
    beta_2surv_adu = 0,
    beta_2fec_adu = 0,
    
    beta_surv_juv = 1,
    beta_surv_adu = 1,
    beta_fec_adu = -1
  ))))
})


Mean.Inc <- loop[[1]]
Mean.Inc.Fl <- loop[[2]]
Mean.Inc.FlIn <- loop[[3]]
Mean.Inc.FlDeac <- loop[[4]]
Mean.Inc.Red <- loop[[5]]

Mean.Inc.Red$rep[5]$data_tot$Abundance

plot_time <- function(data){
  
  data_timeseries <- lapply(1:runs, function(i){ 
    data$rep[i]$data_tot$Abundance
  })
  data_timeseries <- data.frame(do.call(cbind, data_timeseries))
  data_timeseries <- cbind(data.frame(time = c(1:dim(data_timeseries))), data_timeseries)
  
  ggplot(data_timeseries, aes(x = time, y = X1)) +
    geom_line(col = "blue") + 
    ylab("Abundance") +
    geom_line(aes(y = X2), col = "red")+ 
    geom_line(aes(y = X3), col = "pink")+ 
    geom_line(aes(y = X4), col = "yellow")+ 
    geom_line(aes(y = X6), col = "orange")+ 
    geom_line(aes(y = X8), col = "green")+ 
    geom_line(aes(y = X9), col = "brown")+ 
    geom_line(aes(y = X10), col = "purple")
} 


pdf("./plot/plot_sup/plot_supplement_ClimateComp.pdf", 7, 5)
ggarrange(
  plot_time(Mean.Inc),
  plot_time(Mean.Inc.Fl),
  plot_time(Mean.Inc.FlIn),
  plot_time(Mean.Inc.Red), labels = c("A", "B", "C", "D"))
dev.off()

##########################################################
#                POPULATION MODEL                        #
##########################################################
#This code contains the simulation model. 

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "grid", "gtable", "tibble", "plyr", "data.table",
              "tuneR", "GeneCycle", "sigmoid", "tidyverse", "colorednoise",
              "gtable", "dplyr", "cowplot", "popbio", "raster")
ipak(packages)

###PACKAGES__________________________________________________________________________________________________
#Packages needed for the simulation model. 

library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(sigmoid)
library(tidyverse)
library(colorednoise) 

rm(list = ls())

#______________________________________________________________________________________________________________________________
###Function used in the model
#I created a function to caclulate population dynamics or summurize them after the simulation

# Function generation time (not usefull anymore)
# Calcul_trend_gentime <- function(table_model){
#   table_model[is.na(table_model)] <- 0
#   generation_calculation <- lapply(1:nyear,  function(x) {
#     gen_matrix <- matrix(c((table_model$Sj[x] * (1- table_model$Trans[x])),
#                            (table_model$Trans[x]*table_model$Sj[x]),
#                            table_model$Fec[x], table_model$Sa[x]),
#                          ncol = 2, nrow = 2)
#     gentime <- generation.time(gen_matrix)
#     
#     return(gentime)
#   }
#   )
#   
#   list_gen <- do.call(rbind, generation_calculation)
#   list_gen <- as.data.frame(list_gen)
#   list_gen$V2 <- c(1:nyear)
#   trend_gentime <- summary(lm(list_gen$V1 ~ list_gen$V2, data = list_gen))$coefficients[2]
#   return(trend_gentime)
# }


# PLot (create plot of one simulation, just for testing)
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

#Create summuray (Summurize the time series for each replicates)
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

# PLot summary (PLot the summury for testing)
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

#Reaction norm calculation function
function_calculationTrait <- function(shape, beta, beta.sqrt, climatic.value, intercept.trait){
  if (shape == "quadratic"){
    trait.calc <- function(beta = beta, beta.sqrt = beta.sqrt, climate.value = climatic.value, intercept.trait = intercept.trait){intercept.trait +  (beta * climate.value) + (beta.sqrt * climate.value^2)}
  } else { if (shape == "sigmoid"){
    trait.calc <- function(beta = beta, climate.value = climate.value,...){(1/(1+exp(-5*beta*climate.value)) - 0.5 )} # "5" is the strengh of the sigmoid to give a better shape
  }}
  trait <- trait.calc(beta = beta, beta.sqrt = beta.sqrt, climate.value = climatic.value, intercept.trait = intercept.trait)
return(trait)
  }

#Trait-demographic rate calculation function
function_calculationDemo <- function(trait.valu, shape, beta, beta.sqrt, intercept.demo){
  if (shape == "quadratic"){
    demo.calc <- function(beta = beta , beta.sqrt = beta.sqrt, trait.valu = trait.valu, intercept.demo = intercept.demo){intercept.demo +  (beta * trait.valu) + (beta.sqrt * trait.valu^2)}
  } else { if (shape == "sigmoid"){
    demo.calc <- function(beta = beta, trait.valu = trait.valu, intercept.demo = intercept.demo,...){(1/(1+exp(-5*beta*trait.valu)) - 0.5 + intercept.demo)} # "5" is the strengh of the sigmoid to give a better shape
  }}
  demographic_rate <- demo.calc(beta = beta , beta.sqrt = beta.sqrt, trait.valu = trait.valu, intercept.demo = intercept.demo)
  return(demographic_rate)
}


#______________________________________________________________________________________________________________________________
### Model Calculation 
#Population dynamics model is summurized into a function

#The parametrization is set up for some parameters (they can be modify later for specific runs)

model_base <- function(  n = c(100, 100),                      # initial pop size (FIX parameter)
                         nYears = 200,                         # number of years
                         #Climate parameters 
                         meanT = 0,                            #trend of the mean climate (0 means no trend and fix value on O for standardisation) (trend is always positive)
                         SDT = 0,                              #standard deviation or trend of the standard deviastion if mean climate increase over time (trend is always positive)
                         phi = 0,                              #autocorrelated noise parameter                    
                         climatic.mean = "constant",           #dynamics of mean( can be constant or increase)
                         climatic.fluctuation = "constant",    #dynamics of the climatic fluctuation (constant - increase - decrease)
                         Noise = "white",                      #noise (White or red)
                         Observe_sdt = 0.35 ,                  #observe value of climatic variation (use for decrease climatic fluctuation) (FIX parameter)
                         #Pop parameter
                         Dd = "Ricker",                        #density dependence (Beverton-Holt - Ricker) (FIX parameter)
                         b = 0.0001,                           #strengh of the density dependence (FIX parameter)
                         
                         ##Trait
                         #Cimate-trait relationships (reaction norms)
                         trait.function = "quadratic",         #trait shape ("quadratic": linear/auqdratic or "sigmoid": sigmoid)
                         inter_trait_juv = 0,      # intercept in the relation of T on trait mean in Juvenil
                         beta_trait_juv =  1,      # beta in the relation of T on trait mean in Juvenil
                         beta_2trait_juv =  0,      # beta square in the relation of T on trait mean in Juvenil
                         
                         inter_trait_ad = 0,      # intercept in the relation of T on trait mean in Adult
                         beta_trait_ad =  1,      # beta in the relation of T on trait mean in Adult
                         beta_2trait_ad =  0,      # beta square in the relation of T on trait mean in Adult
                         
                         ##Demographic rate
                         #Trait-demographic rates relationships
                         demo.function = "quadratic", # demographic rate shape ("quadratic": linear/auqdratic or "sigmoid": sigmoid)
                         #Juvenil
                         beta_2surv_juv = 0,     # trend in the relation of trait on survival in Juvenil
                         beta_surv_juv = 0,     # trend in the relation of trait on survival in Juvenil
                         inter_surv_juv = 0,      # intercept in the relation of triat on survival in Juvenil
                         
                         #Transition
                        transition.proba = 0.5,       #fix valu of the transition probability
                        #beta_2transi = 0,            # trend effect of climate on calculation of transition probability
                        #beta_transi = 0,             # trend effect of climate on calculation of transition probability
                        #inter_transi = 0,            # intercept effect of climate on calculation of transition probability
                         
                         #Adult survival
                         beta_2surv_adu = 0,     # trend in the relation of trait on survival in Adult
                         beta_surv_adu = 0,     # trend in the relation of trait on survival in Adult
                         inter_surv_adu = 0,      # intercept in the relation of triat on survival in Adult
                         
                         #Adult fecundity
                         beta_2fec_adu = 0 ,     # trend in the relation of trait on fecundity in Adult
                         beta_fec_adu = 0 ,     # trend in the relation of trait on fecundity in Adult
                         inter_fec_adu = 0      # intercept in the relation of trait on fecundity in Adult
                         
                         
){
  
  #I generate storage objects
  allYears <- matrix(0, nrow = 2, ncol = nYears)                                         # storage array for abundances of each classes for each replicate
  pop_total <- rep(0, nYears)                                                            # storage array for total abundances for each replicate
  
  data_tot <- data.frame("Time" = double(), "Climate" =  double(),
                         "Abundance" =  double(), "Growth_rate" =  double(), 
                         "trait_juv" =  double(), "trait_adu" = double(),
                         "Sj" = double() , "Sa" = double() , "Fec" = double(), "Trans" = double() )
  
  
  #______________________________________________________________________________________________________________________________
  #FUNCTION
  
  ### Climate generator
  ##1.Mean constant____________________________________________________________________
  if ( climatic.mean == "constant") {
  
  #1.1Constant climatic fluctuation !!!!!!!!! can create white and red noise depending on phi!!!!
  if (climatic.fluctuation == "constant") {
    
      climate <- colored_noise(nYears, meanT, SDT, phi ) 
    
    #1.2Increase climatic fluctuation
  } else { 
    if (climatic.fluctuation == "increase") {
      trend = SDT
      last_sdt_climate <- trend * nYears

      climate <- (unlist(lapply(seq(0,last_sdt_climate, last_sdt_climate/nYears), function(a){colored_noise(1, meanT, a, phi )})))   ## increase SDT

     #1.3Decrease climatic fluctuation
    } else { (climatic.fluctuation == "decrease")
      trend = SDT
      last_sdt_climate <- -trend * nYears
      
      climate <- (unlist(lapply(seq(Observe_sdt ,last_sdt_climate, last_sdt_climate/nYears), function(a){colored_noise(1, meanT, a, phi )})))   ## increase SDT
      climate[is.nan(climate)] <- 0
      
    } }
  } else { #2.Increase of mean____________________________________________________________________
     trend = meanT
     last_climate = trend * nYears
     
     #2.1Constant climatic fluctuation
    if (climatic.fluctuation == "constant") {
      meaT_list <- seq(0, last_climate, length.out= nYears)
      climate <- unlist(lapply(1:nYears, function(a){colored_noise(1, meaT_list[a], SDT, phi)})) ## increase SDT
      
      #2.2Increase climatic fluctuation
    } else { 
      if (climatic.fluctuation == "increase") {
        trend = SDT
        last_sdt_climate <- trend * nYears
        meaT_list <- seq(0, last_climate, length.out= nYears)
        SDT_list <-  seq(0,last_sdt_climate, length.out= nYears)
        climate <- unlist(lapply(1:nYears, function(a){colored_noise(1, meaT_list[a], SDT_list[a], phi)})) ## increase SDT
        climate[is.nan(climate)] <- 0
        
        #2.3Decrease climatic fluctuation
      } else { 
        if (climatic.fluctuation == "decrease") {
        trend = SDT
        last_sdt_climate <- -trend * nYears
        meaT_list <- seq(0, last_climate, length.out= nYears)
        SDT_list <-  (seq(Observe_sdt, last_sdt_climate, length.out= nYears))
        SDT_list[SDT_list < 0] <- 0
        climate <- unlist(lapply(1:nYears, function(a){colored_noise(1, meaT_list[a], SDT_list[a], phi)})) ## increase SDT
        
        } else {
          
        #2.4Temporal autocorrelation
          meaT_list <- seq(0, last_climate, length.out= nYears)
          climate <- unlist(lapply(1:nYears, function(a){colored_noise(1, meaT_list[a], SDT, phi)})) ## increase SDT
          
        
          
        } } } }
  

  #______________________________________________________________________________________________________________________________
  #MODEL
  
  allYears[ ,1] <- n
  pop_total[1] <- allYears[1 ,1] + allYears[2 ,1]
  for (t in 2:nYears) {
    #Temperature
    Temp <- climate[t]                           # select climate value
    
    ### TRAIT ######################################################################################################
    #Trait calculation
    Juv_Trait_mean <- function_calculationTrait(climatic.value = Temp, shape = trait.function, beta = beta_trait_juv,  beta.sqrt = beta_2trait_juv, intercept.trait = inter_trait_juv)      # calcul Juvenile trait mean as function of temperature
    
    Adu_Trait_mean <- function_calculationTrait(climatic.value = Temp, shape = trait.function, beta = beta_trait_ad,  beta.sqrt = beta_2trait_ad, intercept.trait = inter_trait_ad)         # calcul Adult trait mean as function of temperature


    
    ### DEMOGRAPHIC RATE ##############################################################################################
    ## Calculation of Demographic rate
    #Juvenile
      
    Surv_lin_Juv <- function_calculationDemo(trait.valu = Juv_Trait_mean, shape = demo.function, beta = beta_surv_juv, beta.sqrt = beta_2surv_juv, intercept.demo = inter_surv_juv)


    #Transition (here fixed)
    #Trans_juv_to_ad <- demo.calc(beta = beta_transi, beta.sqrt = beta_2transi, trait.valu = Juv_Trait_mean, intercept.demo = inter_transi)

    #Adult
    Surv_lin_Adu <- function_calculationDemo(trait.valu = Adu_Trait_mean, shape = demo.function, beta = beta_surv_adu, beta.sqrt = beta_2surv_adu, intercept.demo = inter_surv_adu)
        
    #Fec
    Fec_lin_Adu <-  function_calculationDemo(trait.valu = Adu_Trait_mean, shape = demo.function, beta = beta_fec_adu, beta.sqrt = beta_2fec_adu, intercept.demo = inter_fec_adu)

    
    
    ##Link function
    #The link function is here to transform each value into a rate. Logit transform into a value between 0 and 1.
    Surv_Juv_trans <- plogis(Surv_lin_Juv)                                               #logit link fuction for survival
    Surv_Adu_trans <- plogis(Surv_lin_Adu)                                               #logit link fuction for survival
    #Trans_to_ad_trans <- plogis(Trans_juv_to_ad)
    Fec_Adu_trans <- exp(Fec_lin_Adu)                                                    #exp link function for fecundity
    
    ##Density dependence choice
    #After transformation, I apply density dependance on a rate (not in the other way because the formula applied on a rate).
    #I can choose between 2 density dependence function:
    #Risker
    if (Dd =="Ricker")  {
      nsum <- sum(allYears[ , t-1])                        #calculation abundance in previous state for density-dependence
      N_adult <- allYears[2, t-1]
      surv_Juv <- Surv_Juv_trans * exp(-b * nsum)                      #DD juvenil survival
      surv_Adul <- Surv_Adu_trans * exp(-b * N_adult)                     #DD adult survival
      fec_Adu <-   Fec_Adu_trans * exp(-b * N_adult)                       #DD Fecundity
      #Trans <-   Trans_to_ad_trans                      #DD Transfert juvenile to adult
    } else if (Dd =="Beverton-Holt") {
      #Beverton-Holt
      nsum <- sum(allYears[ ,t-1])                        #calculation abundance in previous state for density-dependence
      surv_Juv <- Surv_lin_Juv / (1 + (b * nsum))                     #DD juvenil survival
      surv_Adul <- Surv_lin_Adu / (1+ (b * N_adult))                     #DD adult survival
      fec_Adu <-  Fec_lin_Adu / (1+ (b * N_adult))                       #DD Fecundity
      #Trans <-  Trans_juv_to_ad                     #DD Transfert juvenile to adult
    }

    
    ### POPULATION DYNAMICS ##############################################################################################
    #The matrix is constructed based on the demographic rates with density dependence.
    A <- matrix(
      c( surv_Juv*(1-transition.proba)   ,fec_Adu ,  
         surv_Juv*transition.proba   ,   surv_Adul     ) ,nrow=2,ncol=2,byrow=T)
    
    ## Metrics
    #Abundance is calculated based on previous abundance. 
    allYears[ ,t] <- allYears[ ,t-1] %*% A
    
    #To avoid potential error, I replace NA and NaN by 0
    allYears[is.nan(allYears)] <- 0
    if (allYears[1, t] < 1) {(allYears[1, t] = 0)} else {allYears[1, t] = (allYears[1, t])}
    if (allYears[2, t] < 1) {(allYears[2, t] = 0)} else {allYears[2, t] = (allYears[2, t])}
    
    #Everything is stored for each step of time. 
    pop_total[t] <- sum(allYears[ ,t])
    data_tot[t, "Climate"] <- climate[t]
    data_tot[t, "Time"] <- t
    data_tot[t, "Abundance"] <- sum(allYears[ ,t])
    data_tot[t, "Growth_rate"] <- if (pop_total[t-1] == 0) {NA} else {pop_total[t]  /  pop_total[t-1]}
    data_tot[t, "trait_juv"] <- if (pop_total[t] == 0) {NA} else {Juv_Trait_mean}
    data_tot[t, "trait_adu"] <- if (pop_total[t] == 0) {NA} else {Adu_Trait_mean}
    data_tot[t, "Sj"] <- if (pop_total[t] == 0) {NA} else {surv_Juv} 
    data_tot[t, "Sa"] <- if (pop_total[t] == 0) {NA} else {surv_Adul}
    data_tot[t, "Fec"] <- if (pop_total[t] == 0) {NA} else {fec_Adu}
    data_tot[t, "Trans"] <- if (pop_total[t] == 0) {NA} else {transition.proba}
    
    
  }  ## years close
  
  #Replace first line
  data_tot$Time[1] = 1
  data_tot$Climate [1] = 0
  data_tot$Abundance[1] = sum(n[1], n[2])
  data_tot$trait_juv[1] = 0
  data_tot$trait_adu[1] = 0
  data_tot$Sj[1] = plogis(inter_surv_juv)*exp(-b * nsum) 
  data_tot$Sa[1] = plogis(inter_surv_adu)*exp(-b * nsum)
  data_tot$Fec[1] = (exp(inter_fec_adu))*exp(-b * nsum) 
  data_tot$Trans[1] = transition.proba
  
    #
  return(tibble(data_tot))
}
#_____________________________________________________________________________________________________________________________________________

#One simulation
#Test simulation in order to look at the time series
annee = 200
data <- model_base(nYears = annee ,
                   meanT = 0,  SDT = 0.1, phi = 0,
                   climatic.mean = "increase",   
                   climatic.fluctuation = "constant",    
                   Noise = "white",
                   trait.function = "quadratic", 
                   
                   beta_2trait_juv = 0 ,      # beta square in the relation of T on trait mean in Juvenil
                   beta_trait_juv =  1,      # beta in the relation of T on trait mean in Juvenil
                   
                   beta_2trait_ad =  0,  
                   beta_trait_ad =  1, 
                   
                   demo.function =  "quadratic", 
                   
                   beta_2surv_juv = 0, 
                   beta_2surv_adu = 0,
                   beta_2fec_adu = 0,
                   
                   beta_surv_juv = 1,
                   beta_surv_adu = 1,
                   beta_fec_adu = 1,
                   
                   inter_surv_juv = -1.33,  #parametrization for fast life strategies
                   inter_surv_adu = -0.96,
                   inter_fec_adu = 1.62,
                   transition.proba = 0.7
)

ggplot(data, aes(x = data$data_tot$Time, y = data$data_tot$Abundance)) +
  ylim(0, max(data$data_tot$Abundance)) + 
  geom_line()

#_____________________________________________________________________________________________________________________________________________
#MULTI SIMULATION
#Test of multi simulation
runs <- 10
multi_run <- replicate(runs, model_base())

#Multi-var-simulation 
#Multi simulation with different parametrization
nyear = 200
VAR <- c(0.1, 0.006)
runs <- 1
multi_var <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i, meanT = 0,
                                                                             Noise = "white", climatic.fluctuation = "decrease",  climatic.mean = "constant",
                                                                             phi = 0, 
                                                                             nYears = nyear,                         # number of years
                                                                             b = 0.0001,
                                                                             n = c(100, 100),
                                                                             trait.function = "sigmoid", #quadratic / sigmoid
                                                                             
                                                                             demo.function = "sigmoid", #quadratic / sigmoid
                                                                             inter_surv_juv = 0.081,
                                                                             inter_surv_adu = 3.15,
                                                                             inter_fec_adu = -1.18,
                                                                             
                                                                             transition.proba = 0.3,
                                                                             
                                                                             beta_surv_juv = 1,
                                                                             beta_surv_adu = 1,
                                                                             beta_fec_adu = 1

                                                   ))) })

#Plot of the different timeseries
plot(1,1,pch = "",ylim = c(-2,4), xlim = c(0,nyear+1), xlab = "Years", ylab = "Abundance", xaxt = "n")  # set up blank plot
cols <- rainbow(length(VAR))    # set up colors to use
legend(0, 4000, legend=c("VAR 0", "VAR 1", "VAR 2"),
       col=c(cols[1], cols[2], cols[3], cols[4], cols[5]), lty=1, cex=0.5)
for(var in 1:length(VAR)){
  data_var <- multi_var[[var]]
  for(s in 1:runs){
    points(data_var$rep[s]$data_tot$Climate, type = "l",lwd = 1, col = cols[var])     # plot out each life stage abundance, one at a time
  }} 

(multi_var[[1]]$rep[1]$data_tot)


function_summary(multi_var)


#Create summuray
function_summary <- function(dataset){
  
  summary_var <- lapply(1:length(VAR), function(var){lapply(1:runs, function(laps){data.frame(
    Mean = mean(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    SD = sd(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    SD_sdt = sd(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]) /  mean(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
    GR = mean(log(dataset[[var]]$rep[laps]$data_tot$Growth_rate[(nyear/2):nyear])),
    GR_sdt = mean(log(dataset[[var]]$rep[laps]$data_tot$Growth_rate[(nyear/2):nyear])) / mean(dataset[[var]]$rep[laps]$data_tot$Abundance[(nyear/2):nyear]),
   
    EXT = sum(do.call(rbind, (lapply(1:runs , function(tour){
      tail(dataset[[var]]$rep[tour]$data_tot$Abundance, n=1) <= 0.99 })))) / runs, 
    
    EXT_time = lapply((min(which(multi_var[[var]]$rep[laps]$data_tot$Abundance < 1))), function(x) replace(x, is.infinite(x),NA)),
    
    Trend_gen = Calcul_trend_gentime(dataset[[var]]$rep[runs]$data_tot),
    
    Clim_Var = var)}
    
    )})
  
  summary_multi_var <-  as.data.frame(t(matrix(unlist(summary_var), nrow = 9)))
  setnames(summary_multi_var, old=c("V1","V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9"), new=c("Abundance", "SDT", "SDT_sdt", "log_GR", "log_GR_sdt", "EXT", "EXT_time","Trend_gen", "i"))
  
  return(summary_multi_var)
}

nyear = 200
(multi_var[[1]]$rep[1]$data_tot)
min(which(multi_var[[4]]$rep[9]$data_tot$Abundance < 1))


function_summary(multi_var)



#####################################################################################################################
#################         LIFE HISTORY STRATEGIES         ###########################################################
####################################################################################################################

library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)

#Parameters
VAR <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
runs <- 10

#___________________________________________________________________________________________________________________
#####Function output

# Function generation time
Calcul_trend_gentime <- function(table_model){
  table_model[is.na(table_model)] <- 0
  generation_calculation <- lapply(1:nyear,  function(x) {
    gen_matrix <- matrix(c((table_model$Sj[x] * (1- table_model$Trans[x])),
                           (table_model$Trans[x]*table_model$Sj[x]),
                           table_model$Fec[x], table_model$Sa[x]),
                         ncol = 2, nrow = 2)
    gentime <- generation.time(gen_matrix)
    
    return(gentime)
  }
  )
  
  list_gen <- do.call(rbind, generation_calculation)
  list_gen <- as.data.frame(list_gen)
  list_gen$V2 <- c(1:nyear)
  trend_gentime <- summary(lm(list_gen$V1 ~ list_gen$V2, data = list_gen))$coefficients[2]
  
  #plot a retirer dans la fontion final
  plot(list_gen$V2, list_gen$V1)
  abline(lm(list_gen$V1 ~ list_gen$V2, data = list_gen ))
  
  return(trend_gentime)
}

# PLot
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

#Create summuray
function_summary <- function(dataset){
  
  summary_var <- lapply(1:length(VAR), function(var){lapply(1:runs, function(runs){data.frame(
    Mean = mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    SD = sd(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    SD_sdt = sd(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]) /  mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    GR = mean(log(dataset[[var]]$rep[runs]$data_tot$Growth_rate[100:200])),
    GR_sdt = mean(log(dataset[[var]]$rep[runs]$data_tot$Growth_rate[100:200])) / mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    EXT = sum(dataset[[var]]$rep[runs]$data_tot$Abundance[200] <= 0.99) / runs,
    
    Trend_gen = Calcul_trend_gentime(dataset[[var]]$rep[runs]$data_tot),
    
    Clim_Var = var)})})
  
  summary_multi_var <-  as.data.frame(t(matrix(unlist(summary_var), nrow = 8)))
  setnames(summary_multi_var, old=c("V1","V2", "V3", "V4", "V5", "V6", "V7", "V8"), new=c("Abundance", "SDT", "SDT_sdt", "log_GR", "log_GR_sdt", "EXT", "Trend_gen", "i"))
  
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



#___________________________________________________________________________________________________________________
#r EXTREM
multi_var_r_EXTR <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                               Noise = "white", climatic.fluctuation = "constant",
                                                                               inter_surv_juv = -2.0,
                                                                               inter_transi = 0.047,
                                                                               inter_surv_adu = -4.40,
                                                                               inter_fec_adu = 2.82,
                                                                               #TRAIT
                                                                               Trait.function = "curve", #(curve - sigmoid - linear)
                                                                               
                                                                               beta_trait_juv = 0.6,
                                                                               beta_trait_ad = 0.6,
                                                                               
                                                                               #DEMOGRAPHIC RATE
                                                                               beta_surv_juv = 1,
                                                                               beta_surv_adu = 1,
                                                                               beta_fec_adu = -1,
                                                                               beta_transi = 1, 
                                                                               
                                                                               beta_2surv_juv = 0, 
                                                                               beta_2surv_adu = 0,
                                                                               beta_2fec_adu = 0,
                                                                               beta_2transi = 0
                                                                               
                                                                               
                                                                               
))) })

multi_var_r_EXTR[[1]]$rep[1]
function_plot(multi_var_r_EXTR)
rExt_curve_1 <- function_summary(multi_var_r_EXTR)
rExt_curve_1$Scena <- "1"

rExt_curve <- rbind(rExt_curve_1, rExt_curve_2, rExt_curve_3, rExt_curve_4, rExt_curve_5,rExt_curve_6,rExt_curve_7,rExt_curve_8,rExt_curve_9,rExt_curve_10,rExt_curve_11,rExt_curve_12,rExt_curve_13)
rExt_curve$trait_relation <- "curve"
rExt_LinPo <- rbind(rExt_LinPo_1, rExt_LinPo_2, rExt_LinPo_3, rExt_LinPo_4, rExt_LinPo_5,rExt_LinPo_6,rExt_LinPo_7,rExt_LinPo_8,rExt_LinPo_9,rExt_LinPo_10,rExt_LinPo_11,rExt_LinPo_12,rExt_LinPo_13)
rExt_LinPo$trait_relation <- "LinPo" 
rExt_LinNeg <- rbind(rExt_LinNeg_1, rExt_LinNeg_2, rExt_LinNeg_3, rExt_LinNeg_4, rExt_LinNeg_5,rExt_LinNeg_6,rExt_LinNeg_7,rExt_LinNeg_8,rExt_LinNeg_9,rExt_LinNeg_10,rExt_LinNeg_11,rExt_LinNeg_12,rExt_LinNeg_13)
rExt_LinNeg$trait_relation <- "LinNeg"
rExt_SigPo <- rbind(rExt_SigPo_1, rExt_SigPo_2, rExt_SigPo_3, rExt_SigPo_4, rExt_SigPo_5,rExt_SigPo_6,rExt_SigPo_7,rExt_SigPo_8,rExt_SigPo_9,rExt_SigPo_10,rExt_SigPo_11,rExt_SigPo_12,rExt_SigPo_13)
rExt_SigPo$trait_relation <- "SigPo"
rExt_SigNeg <- rbind(rExt_SigNeg_1, rExt_SigNeg_2, rExt_SigNeg_3, rExt_SigNeg_4, rExt_SigNeg_5,rExt_SigNeg_6,rExt_SigNeg_7,rExt_SigNeg_8,rExt_SigNeg_9,rExt_SigNeg_10,rExt_SigNeg_11,rExt_SigNeg_12,rExt_SigNeg_13)
rExt_SigNeg$trait_relation <- "SigNeg" 

rExt_tot <- rbind(rExt_curve, rExt_LinPo, rExt_LinNeg, rExt_SigPo, rExt_SigNeg)
rExt_tot$LHS <- "rExtrem"
rExt_tot$i <- as.character(rExt_tot$i)

saveRDS(rExt_tot , file = "rExt_tot.rds")
rExt_tot <- readRDS( file = "rExt_tot.rds")

rExt_tot_plot <- ggplot(rExt_tot, aes(x=rExt_tot$i , y=rExt_tot$log_GR, color =rExt_tot$trait_relation))+
  geom_boxplot() + 
  facet_grid(rExt_tot$Scena~rExt_tot$trait_relation)+
  theme(legend.title = element_blank()) +
  labs(title = "Abundance function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
rExt_tot_plot

#___________________________________________________________________________________________________________________
#r
multi_var_r <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                               Noise = "white", climatic.fluctuation = "constant",
                                                                               inter_surv_juv = -0.27,
                                                                               inter_transi = -0.059,
                                                                               inter_surv_adu = -0.70,
                                                                               inter_fec_adu = 1.235,
                                                                               #TRAIT
                                                                               Trait.function = "curve", #(constant - sigmoid - linear)
                                                                               
                                                                               beta_trait_juv = 1,
                                                                               beta_trait_ad = 1,
                                                                               
                                                                               #DEMOGRAPHIC RATE
                                                                               beta_surv_juv = 1,
                                                                               beta_surv_adu = 1,
                                                                               beta_fec_adu = 1,
                                                                               beta_transi = 1, 
                                                                               
                                                                               beta_2surv_juv = 0, 
                                                                               beta_2surv_adu = 0,
                                                                               beta_2fec_adu = 0,
                                                                               beta_2transi = 0
                                                                               
                                                                               
                                                                               
))) })

multi_var_r[[1]]$rep[1]
function_plot(multi_var_r)
Sum_r <- function_summary(multi_var_r)



#___________________________________________________________________________________________________________________
#Mixte

multi_var_K <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                               Noise = "white", climatic.fluctuation = "constant",
                                                                               inter_surv_juv = -0.265,
                                                                               inter_transi = -0.389,
                                                                               inter_surv_adu = 0.55,
                                                                               inter_fec_adu = 0.83,
                                                                               #TRAIT
                                                                               Trait.function = "curve", #(constant - sigmoid - linear)
                                                                               
                                                                               beta_trait_juv = 1,
                                                                               beta_trait_ad = 1,
                                                                               
                                                                               #DEMOGRAPHIC RATE
                                                                               beta_surv_juv = 1,
                                                                               beta_surv_adu = 1,
                                                                               beta_fec_adu = 1,
                                                                               beta_transi = 1, 
                                                                               
                                                                               beta_2surv_juv = 0, 
                                                                               beta_2surv_adu = 0,
                                                                               beta_2fec_adu = 0,
                                                                               beta_2transi = 0
                                                                               
                                                                               
                                                                               
))) })

multi_var_K[[1]]$rep[1]
function_plot(multi_var_K)
Sum_m <- function_summary(multi_var_K)

#___________________________________________________________________________________________________________________
#K

multi_var_K_EXTR <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                               Noise = "white", climatic.fluctuation = "constant",
                                                                               inter_surv_juv = -1.36,
                                                                               inter_transi = 0.85,
                                                                               inter_surv_adu = 2.94,
                                                                               inter_fec_adu = -0.90,
                                                                               #TRAIT
                                                                               Trait.function = "sigmoid", #( sigmoid - linear - curve)
                                                                               
                                                                               beta_trait_juv = -1,
                                                                               beta_trait_ad = -1,
                                                                               
                                                                               #DEMOGRAPHIC RATE
                                                                               beta_surv_juv = 1,
                                                                               beta_surv_adu = 1,
                                                                               beta_fec_adu = 1,
                                                                               beta_transi = -1, 
                                                                               
                                                                               beta_2surv_juv = 0, 
                                                                               beta_2surv_adu = 0,
                                                                               beta_2fec_adu = 0,
                                                                               beta_2transi = 0
                                                                               
                                                                               
                                                                                
))) })

multi_var_K_EXTR[[1]]$rep[1]
function_plot(multi_var_K_EXTR)
#Sum_K <- function_summary(multi_var_K_EXTR)

KExt_SigNeg_4 <- function_summary(multi_var_K_EXTR)
KExt_SigNeg_4$Scena <- "4"

KExt_curve <- rbind(KExt_curve_1, KExt_curve_2, KExt_curve_3, KExt_curve_4, KExt_curve_5,KExt_curve_6,KExt_curve_7,KExt_curve_8,KExt_curve_9,KExt_curve_10,KExt_curve_11,KExt_curve_12,KExt_curve_13)
KExt_curve$trait_relation <- "curve"
KExt_LinPo <- rbind(KExt_LinPo_1, KExt_LinPo_2, KExt_LinPo_3, KExt_LinPo_4, KExt_LinPo_5,KExt_LinPo_6,KExt_LinPo_7,KExt_LinPo_8,KExt_LinPo_9,KExt_LinPo_10,KExt_LinPo_11,KExt_LinPo_12,KExt_LinPo_13)
KExt_LinPo$trait_relation <- "LinPo" 
KExt_LinNeg <- rbind(KExt_LinNeg_1, KExt_LinNeg_2, KExt_LinNeg_3, KExt_LinNeg_4, KExt_LinNeg_5,KExt_LinNeg_6,KExt_LinNeg_7,KExt_LinNeg_8,KExt_LinNeg_9,KExt_LinNeg_10,KExt_LinNeg_11,KExt_LinNeg_12,KExt_LinNeg_13)
KExt_LinNeg$trait_relation <- "LinNeg"
KExt_SigPo <- rbind(KExt_SigPo_1, KExt_SigPo_2, KExt_SigPo_3, KExt_SigPo_4, KExt_SigPo_5,KExt_SigPo_6,KExt_SigPo_7,KExt_SigPo_8,KExt_SigPo_9,KExt_SigPo_10,KExt_SigPo_11,KExt_SigPo_12,KExt_SigPo_13)
KExt_SigPo$trait_relation <- "SigPo"
KExt_SigNeg <- rbind(KExt_SigNeg_1, KExt_SigNeg_2, KExt_SigNeg_3, KExt_SigNeg_4, KExt_SigNeg_5,KExt_SigNeg_6,KExt_SigNeg_7,KExt_SigNeg_8,KExt_SigNeg_9,KExt_SigNeg_10,KExt_SigNeg_11,KExt_SigNeg_12,KExt_SigNeg_13)
KExt_SigNeg$trait_relation <- "SigNeg" 

KExt_tot <- rbind(KExt_curve, KExt_LinPo, KExt_LinNeg, KExt_SigPo, KExt_SigNeg)
KExt_tot$LHS <- "KExtrem"
KExt_tot$i <- as.character(KExt_tot$i)

saveRDS(KExt_tot , file = "KExt_tot.rds")
KExt_tot <- readRDS( file = "KExt_tot.rds")

KExt_tot_plot <- ggplot(KExt_tot, aes(x=KExt_tot$i , y=KExt_tot$log_GR, color =KExt_tot$trait_relation))+
  geom_boxplot() + 
  facet_grid(KExt_tot$Scena~KExt_tot$trait_relation)+
  theme(legend.title = element_blank()) +
  labs(title = "Abundance function of climatic variation in Extrem Long life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
KExt_tot_plot

#___________________________________________________________________________________________________________________
#####Bind results

Sum_r$LHS = "r"
Sum_m$LHS = "m"
Sum_K$LHS = "K"

Sum_tot <- rbind(Sum_r, Sum_m, Sum_K)
#saveRDS(tibble(multi_var, summary_multi_var), paste0("Results_Run_", "", ".rds"))
function_plotSum(Sum_tot)

function_plotSum(Sum_tot_NonLin)
function_plotSum(Sum_tot_lin)

jpeg('Non_linear.jpg', width = 1000, height = 1000, units = "px", pointsize = 14, quality = 75)
function_plotSum(Sum_tot_NonLin)
dev.off()


jpeg('Linear.jpg', width = 1000, height = 1000, units = "px", pointsize = 14, quality = 75)
function_plotSum(Sum_tot_lin)
dev.off()

Sum_tot_NonLin <- Sum_tot
Sum_tot_lin <- Sum_tot

###### Blind totalt results
Sum_tot_NonLin$relation <- "Non-linear"
Sum_tot_lin$relation <- "Linear"

Sum_total <- rbind(Sum_tot_NonLin, Sum_tot_lin)

saveRDS(Sum_total, file = "Sum_total_poster.rds")
Sum_total <- readRDS(file = "Sum_total_poster.rds")

Sum_tot_r <- subset(Sum_total, Sum_total$LHS == "r" )
Sum_tot_K <- subset(Sum_total, Sum_total$LHS == "K" )
Sum_total <- rbind(Sum_tot_r, Sum_tot_K)

StD <- ggplot(Sum_total, aes(x=Sum_total$i , y=Sum_total$SDT, group = interaction(Sum_total$LHS, Sum_total$i)))+
  geom_boxplot(aes(colour = factor(Sum_total$LHS ))) +
  facet_grid(~relation) + 
  theme(legend.title = element_blank()) +
  labs(title = "Standard deviation of the population size function of \n climatic variation") +
  theme(legend.position="none")+
  xlab("Climatic Variation \n (generated from a normal distribution)") + 
  ylab("Standard deviation of the population size") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5 ),
    label = c("0", "0.1", "0.2", "0.3", "0.4"))+
  #scale_colour_discrete(name="Life history strategies",
   #                   breaks=c("K", "m", "r"),
    #                  labels=c("Long", "Intermediate", "Short"))+
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 30, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"))
StD

jpeg('effect of nonlinearity.jpg', width = 1400, height = 1400, units = "px", pointsize = 20, quality = 1000)
StD
dev.off()

tiff("effect of nonlinearity name.tif", height = 13, width = 13, units = 'in', pointsize = 100,res=600)
StD
dev.off()

#Graphe density dependence
N <- data_var$rep[1]$data_tot$Abundance
G_N <- unlist(lapply(1:200, function(time){data_var$rep[1]$data_tot$Abundance[time]/data_var$rep[1]$data_tot$Abundance[time-1]}))
DensDep <- cbind(N[1:199], G_N)
plot(DensDep)

#___________________________________________________________________________________________________________________
#####Scenario


scenario_dem <- t(matrix ( ncol = 13,nrow = 8,
                c(1, 0, 1, 0, 1, 0, -1, 0,   #linear
                  1, 0, 1, 0, -1, 0, -1, 0,  #linear
                  -1, 0, -1, 0, 1, 0, 1, 0,  #linear
                  -1, 0, -1, 0, -1, 0, 1, 0,  #linear
                  -0.25, 0.5, -0.25, 0.5, -0.25, 0.5, 0.25, -0.5,  #up
                  -0.25, 0.5, -0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #up
                  0.25, -0.5, 0.25, -0.5, 0.25, -0.5, -0.25, 0.5,  #up
                  0.25, -0.5, 0.25, -0.5, -0.25, 0.5, -0.25, 0.5,  #up
                  0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, -0.5,  #down
                  0.25, 0.5, 0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #down
                  0.25, -0.5, 0.25, -0.5, 0.25, 0.5, 0.25, 0.5,  #down
                  0.25, -0.5, 0.25, -0.5, 0.25, -0.5, 0.25, 0.5,  #down
                  -0.5, -0, -0.5, 0, -0.5, 0, -0.5, 0 ))) #curve

scenario_trait <- t(matrix(ncol = 5, nrow = 2, 
                           c("gaussian", 1, "linPo", 1,  "LinNeg", -1, "SigmPo", 1, "SigmNeg", -1)))

                                    #     mean SDT noise dynamic
VAR_list <- t(matrix(ncol = 6, nrow = 4, c(0, 0, "white", "constant", 
                                           0, 0.3, "white", "constant",
                                           0, 0.5, "white", "constant",
                                           0, 0.5, "white", "increase",
                                           0, 0.5, "white", "decrease",
                                           0, 1, "red", "constant"
                                                                    )))


# (gaussian, linPo, LinNeg, SigmPo, SigmNeg)

##R extrem
R_scenario <-lapply(1:5, function(parametre_trait){
    trait_family <- scenario_trait[parametre_trait,1]
    trait_valu <- as.numeric(scenario_trait[parametre_trait,2])
  
  
out_R <-  lapply(1:dim(scenario_dem)[1], function(parametre){
  para_list <- scenario_dem[parametre,]

multi_var_r_EXTR <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                                    Noise = "white", climatic.fluctuation = "constant",
                                                                                    inter_surv_juv = -2.17,
                                                                                    inter_transi = -0.12,
                                                                                    inter_surv_adu = -4.58,
                                                                                    inter_fec_adu = 3,
                                                                                    #TRAIT
                                                                                    Trait.function = trait_family, #(curve - sigmoid - linear)
                                                                                    
                                                                                    beta_trait_juv = 1,
                                                                                    beta_trait_ad = 1,
                                                                                    
                                                                                    #DEMOGRAPHIC RATE
                                                                                    beta_surv_juv = para_list[2],
                                                                                    beta_surv_adu = para_list[4],
                                                                                    beta_fec_adu = para_list[8],
                                                                                    beta_transi = para_list[6], 
                                                                                    
                                                                                    beta_2surv_juv = para_list[1], 
                                                                                    beta_2surv_adu = para_list[3],
                                                                                    beta_2fec_adu = para_list[7],
                                                                                    beta_2transi = para_list[5]
                                                                                    
                                                                                    
                                                                                    
))) })

rExt_curve <- function_summary(multi_var_r_EXTR)
rExt_curve$scenario <- parametre

}  )

out_R_bind <- do.call(rbind, out_R)

return(out_R_bind)

  })

R_scenario
R_scenario <- do.call(rbind,R_scenario)

R_scenario[[1]][[1]]$rep

saveRDS(out_R_bind , file = "out_R_bind_tot.rds")
rExt_tot <- out_R_bind( file = "out_R_bind_tot.rds")



############################################################################################################
## SIMULATION CLUSTER

##K extrem
setwd("U:/GUEST/Abteilung6/People/Chero_Guillaume/Outputcluster")
library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)

#Parameters
VAR <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
runs <- 100

scenario_dem <- t(matrix ( ncol = 13,nrow = 8,
                           c(1, 0, 1, 0, 1, 0, -1, 0,   #linear
                             1, 0, 1, 0, -1, 0, -1, 0,  #linear
                             -1, 0, -1, 0, 1, 0, 1, 0,  #linear
                             -1, 0, -1, 0, -1, 0, 1, 0,  #linear
                             -0.25, 0.5, -0.25, 0.5, -0.25, 0.5, 0.25, -0.5,  #up
                             -0.25, 0.5, -0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #up
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, -0.25, 0.5,  #up
                             0.25, -0.5, 0.25, -0.5, -0.25, 0.5, -0.25, 0.5,  #up
                             0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, -0.5,  #down
                             0.25, 0.5, 0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, 0.5, 0.25, 0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, 0.25, 0.5,  #down
                             -0.5, -0, -0.5, 0, -0.5, 0, -0.5, 0 ))) #curve

out_K <- lapply(1:dim(scenario_dem)[1], function(parametre){
  para_list <- scenario_dem[parametre,]
  
  multi_var_K_EXTR <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                                      Noise = "white", climatic.fluctuation = "constant",
                                                                                      inter_surv_juv = -1.36,
                                                                                      inter_transi = 0.84,
                                                                                      inter_surv_adu = 2.93,
                                                                                      inter_fec_adu = -0.90,
                                                                                      #TRAIT
                                                                                      Trait.function = "sigmoid", #(curve - sigmoid - linear)
                                                                                      
                                                                                      beta_trait_juv = -1,
                                                                                      beta_trait_ad = -1,
                                                                                      
                                                                                      #DEMOGRAPHIC RATE
                                                                                      beta_surv_juv = para_list[2],
                                                                                      beta_surv_adu = para_list[4],
                                                                                      beta_fec_adu = para_list[8],
                                                                                      beta_transi = para_list[6], 
                                                                                      
                                                                                      beta_2surv_juv = para_list[1], 
                                                                                      beta_2surv_adu = para_list[3],
                                                                                      beta_2fec_adu = para_list[7],
                                                                                      beta_2transi = para_list[5]
                                                                                      
                                                                                      
                                                                                      
  ))) })
  
  KExt_curve <- function_summary(multi_var_K_EXTR)
  
  KExt_curve$scenario <- parametre
  
  return(KExt_curve)
  
}  )

out_K
out_K_bind <- do.call(rbind,out_K)
out_K_bind$trait <- "SigNeg" # (gaussian, linPo, LinNeg, SigmPo, SigmNeg)

saveRDS(out_K_bind , file = "out_K_bind_SigNeg.rds")



k_gaus <- readRDS( file = "out_K_bind_gaussian.rds")
k_linpo <- readRDS( file = "out_K_bind_LinPo.rds")
k_linneg <- readRDS( file = "out_K_bind_LinNeg.rds")
k_sigpo <- readRDS( file = "out_K_bind_SigPo.rds")
k_signeg <- readRDS( file = "out_K_bind_SigNeg.rds")

KExt_tot <- rbind(k_gaus, k_linpo, k_linneg, k_sigpo, k_signeg)
KExt_tot$i <- as.character(KExt_tot$i)

saveRDS(KExt_tot , file = "Outpout_K_selected_strategies.rds")
KExt_tot <- readRDS( file = "Outpout_K_selected_strategies.rds")


KExt_tot_plot <- ggplot(KExt_tot, aes(x=KExt_tot$i , y=KExt_tot$SDT, color =KExt_tot$trait))+
  geom_boxplot() + 
  facet_grid(KExt_tot$scenario~KExt_tot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Abundance function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
KExt_tot_plot

##################################################################
##R extrem
setwd("U:/GUEST/Abteilung6/People/Chero_Guillaume/Outputcluster")
library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)

#Parameters
VAR <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
runs <- 100

scenario_dem <- t(matrix ( ncol = 13,nrow = 8,
                           c(1, 0, 1, 0, 1, 0, -1, 0,   #linear
                             1, 0, 1, 0, -1, 0, -1, 0,  #linear
                             -1, 0, -1, 0, 1, 0, 1, 0,  #linear
                             -1, 0, -1, 0, -1, 0, 1, 0,  #linear
                             -0.25, 0.5, -0.25, 0.5, -0.25, 0.5, 0.25, -0.5,  #up
                             -0.25, 0.5, -0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #up
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, -0.25, 0.5,  #up
                             0.25, -0.5, 0.25, -0.5, -0.25, 0.5, -0.25, 0.5,  #up
                             0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, -0.5,  #down
                             0.25, 0.5, 0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, 0.5, 0.25, 0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, 0.25, 0.5,  #down
                             -0.5, -0, -0.5, 0, -0.5, 0, -0.5, 0 ))) #curve

out_r <- lapply(1:dim(scenario_dem)[1], function(parametre){
  para_list <- scenario_dem[parametre,]
  
  multi_var_r_EXTR <- lapply(VAR, function(i){tibble(rep = replicate(runs, model_base(SDT = i,
                                                                                      Noise = "white", climatic.fluctuation = "constant",
                                                                                      inter_surv_juv = -2.17,
                                                                                      inter_transi = -0.12,
                                                                                      inter_surv_adu = -4.58,
                                                                                      inter_fec_adu = 3,
                                                                                      #TRAIT
                                                                                      Trait.function = "sigmoid", #(curve - sigmoid - linear)
                                                                                      
                                                                                      beta_trait_juv = -1,
                                                                                      beta_trait_ad = -1,
                                                                                      
                                                                                      #DEMOGRAPHIC RATE
                                                                                      beta_surv_juv = para_list[2],
                                                                                      beta_surv_adu = para_list[4],
                                                                                      beta_fec_adu = para_list[8],
                                                                                      beta_transi = para_list[6], 
                                                                                      
                                                                                      beta_2surv_juv = para_list[1], 
                                                                                      beta_2surv_adu = para_list[3],
                                                                                      beta_2fec_adu = para_list[7],
                                                                                      beta_2transi = para_list[5]
                                                                                      
                                                                                      
                                                                                      
  ))) })
  
  rExt_curve <- function_summary(multi_var_r_EXTR)
  
  rExt_curve$scenario <- parametre
  
  return(rExt_curve)
  
}  )

 
out_r_bind <- do.call(rbind,out_r)
out_r_bind$trait <- "SigNeg" # (gaussian, linPo, LinNeg, SigmPo, SigmNeg)

saveRDS(out_r_bind , file = "out_r_bind_SigNeg.rds")



#Output
r_gaus <- readRDS( file = "out_r_bind_gaussian.rds")
r_linpo <- readRDS( file = "out_r_bind_LinPo.rds")
r_linneg <- readRDS( file = "out_r_bind_LinNeg.rds")
r_sigpo <- readRDS( file = "out_r_bind_SigPo.rds")
r_signeg <- readRDS( file = "out_r_bind_SigNeg.rds")

rExt_tot <- rbind(r_gaus, r_linpo, r_linneg, r_sigpo, r_signeg)
rExt_tot$i <- as.character(rExt_tot$i)

saveRDS(rExt_tot , file = "Outpout_r_selected_strategies.rds")
rExt_tot <- readRDS( file = "Outpout_r_selected_strategies.rds")


rExt_tot_plot <- ggplot(rExt_tot, aes(x=rExt_tot$i , y=rExt_tot$log_GR, color =rExt_tot$trait))+
  geom_boxplot() + 
  facet_grid(rExt_tot$scenario~rExt_tot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Abundance function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
rExt_tot_plot

#______________________________________________________________________________________________________
## PlOt total
rExt_tot <- readRDS( file = "Outpout_r_selected_strategies.rds")
KExt_tot <- readRDS( file = "Outpout_K_selected_strategies.rds")

rExt_tot$LHS <- "fast"
KExt_tot$LHS <- "slow"
data_tot <- rbind(rExt_tot, KExt_tot)

tot_plot <- ggplot(data_tot, aes(x=data_tot$i , y=data_tot$log_GR, color = data_tot$LHS))+
  geom_boxplot() + 
  facet_grid(data_tot$scenario ~ data_tot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot


#Explore


data_tot_sub_fast1 <- subset(data_tot, trait == "LinPo")
data_pres <- rbind(data_tot_sub_fast1, data_tot_sub_fast)
data_tot_sub1 <- subset(data_pres, scenario == 1)
data_tot_sub5 <- subset(data_tot, scenario == 5)
data_tot_sub9 <- subset(data_tot, scenario == 9)
data_tot_sub13 <- subset(data_pres, scenario == 13)
data_tot_subtot <- rbind(data_tot_sub1, data_tot_sub13)

tot_plot_sub <- ggplot(data_tot_subtot, aes(x=data_tot_subtot$i , y=data_tot_subtot$log_GR, color = data_tot_subtot$LHS))+
  geom_boxplot() + 
  facet_grid(data_tot_subtot$scenario ~ data_tot_subtot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_plot_sub





############################################################################################################
## SIMULATION CLUSTER with different component of climate

##################################################################
##K extrem
setwd("C:/Users/Chero/Documents/Population _model/Population_model_matrix")
library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)

function_summary <- function(dataset){
  
  summary_var <- lapply(1:dim(VAR_compo), function(var){lapply(1:runs, function(runs){data.frame(
    Mean = mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    SD = sd(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    SD_sdt = sd(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]) /  mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    GR = mean(log(dataset[[var]]$rep[runs]$data_tot$Growth_rate[100:200])),
    GR_sdt = mean(log(dataset[[var]]$rep[runs]$data_tot$Growth_rate[100:200])) / mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    EXT = sum(dataset[[var]]$rep[runs]$data_tot$Abundance[200] <= 0.99) / runs,
    Clim_Var = var)})})
  
  summary_multi_var <-  as.data.frame(t(matrix(unlist(summary_var), nrow = 7)))
  setnames(summary_multi_var, old=c("V1","V2", "V3", "V4", "V5", "V6", "V7"), new=c("Abundance", "SDT", "SDT_sdt", "log_GR", "log_GR_sdt", "EXT", "i"))
  
  return(summary_multi_var)
}


#Parameters
VAR_compo <- t(matrix ( ncol = 4,nrow = 3,
                        c("white", "constant", 0.5,
                          "white", "increase", 0.5,
                          "white", "decrease", 0.5,
                          "red", "constant", 0.5)))
VAR[1,1]

runs <- 10

scenario_dem <- t(matrix ( ncol = 13,nrow = 8,
                           c(1, 0, 1, 0, 1, 0, -1, 0,   #linear
                             1, 0, 1, 0, -1, 0, -1, 0,  #linear
                             -1, 0, -1, 0, 1, 0, 1, 0,  #linear
                             -1, 0, -1, 0, -1, 0, 1, 0,  #linear
                             -0.25, 0.5, -0.25, 0.5, -0.25, 0.5, 0.25, -0.5,  #up
                             -0.25, 0.5, -0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #up
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, -0.25, 0.5,  #up
                             0.25, -0.5, 0.25, -0.5, -0.25, 0.5, -0.25, 0.5,  #up
                             0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, -0.5,  #down
                             0.25, 0.5, 0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, 0.5, 0.25, 0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, 0.25, 0.5,  #down
                             -0.5, -0, -0.5, 0, -0.5, 0, -0.5, 0 ))) #curve

out_k_compo <- lapply(1:dim(scenario_dem)[1], function(parametre){
  para_list <- scenario_dem[parametre,]
  
  multi_var_k_EXTR <- lapply(1:dim(VAR_compo)[1], function(i){tibble(rep = replicate(runs, model_base(SDT = 0.5,
                                                                                                      Noise = VAR_compo[i, 1], climatic.fluctuation = VAR_compo[i, 2],
                                                                                                      inter_surv_juv = -1.36,
                                                                                                      inter_transi = 0.84,
                                                                                                      inter_surv_adu = 2.93,
                                                                                                      inter_fec_adu = -0.90,
                                                                                                      #TRAIT
                                                                                                      Trait.function = "sigmoid", #(curve - sigmoid - linear)
                                                                                                      
                                                                                                      beta_trait_juv = -1,
                                                                                                      beta_trait_ad = -1,
                                                                                                      
                                                                                                      #DEMOGRAPHIC RATE
                                                                                                      beta_surv_juv = para_list[2],
                                                                                                      beta_surv_adu = para_list[4],
                                                                                                      beta_fec_adu = para_list[8],
                                                                                                      beta_transi = para_list[6], 
                                                                                                      
                                                                                                      beta_2surv_juv = para_list[1], 
                                                                                                      beta_2surv_adu = para_list[3],
                                                                                                      beta_2fec_adu = para_list[7],
                                                                                                      beta_2transi = para_list[5]
                                                                                                      
                                                                                                      
                                                                                                      
  ))) })
  
kExt_curve <- function_summary(multi_var_k_EXTR)
kExt_curve$scenario <- parametre
  
  return(kExt_curve)
  
}  )


out_k_compo <- do.call(rbind,out_k_compo)
out_k_compo$i <- as.factor(out_k_compo$i)
out_k_compo$scenario <- as.factor(out_k_compo$scenario)

out_k_compo_SigNeg <- out_k_compo

out_k_compo_gaussian$trait <- "gaussian"
out_k_compo_LinPo$trait <- "LinPo"
out_k_compo_LinNeg$trait <- "LinNeg"
out_k_compo_SigPo$trait <- "SigPo"
out_k_compo_SigNeg$trait <- "SigNeg"

out_k_compo_tot <- rbind(out_k_compo_gaussian, out_k_compo_LinPo, out_k_compo_LinNeg, out_k_compo_SigPo, out_k_compo_SigNeg)
out_k_compo_tot$LHS <- "slow"

saveRDS(out_k_compo_tot , file = "out_k_compo_tot_fastTrial.rds")
out_k_compo_tot <- readRDS( file = "out_k_compo_tot_fastTrial.rds")

out_k_compo_tot1 <- subset(out_k_compo_tot, scenario == 1)
out_k_compo_tot5 <- subset(out_k_compo_tot, scenario == 5)
out_k_compo_tot9 <- subset(out_k_compo_tot, scenario == 9)
out_k_compo_tot13 <- subset(out_k_compo_tot, scenario == 13)
out_k_compo_tot_subtot <- rbind(out_k_compo_tot1, out_k_compo_tot5,  out_k_compo_tot9, out_k_compo_tot13)

kExt_tot_compo_plot <- ggplot(out_k_compo_tot, aes(x=out_k_compo_tot$i , y=out_k_compo_tot$log_GR, color = out_k_compo_tot$i))+
  geom_boxplot() + 
  facet_grid(out_k_compo_tot$scenario ~ out_k_compo_tot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
kExt_tot_compo_plot

##################################################################
##R extrem
setwd("C:/Users/Chero/Documents/Population _model/Population_model_matrix")
library(ggplot2)
library(grid)
library(gtable)
library(tibble)
library (plyr)
library(data.table)
library(tuneR)
library(GeneCycle)
library(cowplot)

function_summary <- function(dataset){
  
  summary_var <- lapply(1:dim(VAR_compo), function(var){lapply(1:runs, function(runs){data.frame(
    Mean = mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    SD = sd(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    SD_sdt = sd(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]) /  mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    GR = mean(log(dataset[[var]]$rep[runs]$data_tot$Growth_rate[100:200])),
    GR_sdt = mean(log(dataset[[var]]$rep[runs]$data_tot$Growth_rate[100:200])) / mean(dataset[[var]]$rep[runs]$data_tot$Abundance[100:200]),
    EXT = sum(dataset[[var]]$rep[runs]$data_tot$Abundance[200] <= 0.99) / runs,
    Clim_Var = var)})})
  
  summary_multi_var <-  as.data.frame(t(matrix(unlist(summary_var), nrow = 7)))
  setnames(summary_multi_var, old=c("V1","V2", "V3", "V4", "V5", "V6", "V7"), new=c("Abundance", "SDT", "SDT_sdt", "log_GR", "log_GR_sdt", "EXT", "i"))
  
  return(summary_multi_var)
}


#Parameters
VAR_compo <- t(matrix ( ncol = 4,nrow = 3,
                        c("white", "constant", 0.5,
                          "white", "increase", 0.5,
                          "white", "decrease", 0.5,
                          "red", "constant", 0.5)))
VAR[1,1]

runs <- 10

scenario_dem <- t(matrix ( ncol = 13,nrow = 8,
                           c(1, 0, 1, 0, 1, 0, -1, 0,   #linear
                             1, 0, 1, 0, -1, 0, -1, 0,  #linear
                             -1, 0, -1, 0, 1, 0, 1, 0,  #linear
                             -1, 0, -1, 0, -1, 0, 1, 0,  #linear
                             -0.25, 0.5, -0.25, 0.5, -0.25, 0.5, 0.25, -0.5,  #up
                             -0.25, 0.5, -0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #up
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, -0.25, 0.5,  #up
                             0.25, -0.5, 0.25, -0.5, -0.25, 0.5, -0.25, 0.5,  #up
                             0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, -0.5,  #down
                             0.25, 0.5, 0.25, 0.5, 0.25, -0.5, 0.25, -0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, 0.5, 0.25, 0.5,  #down
                             0.25, -0.5, 0.25, -0.5, 0.25, -0.5, 0.25, 0.5,  #down
                             -0.5, -0, -0.5, 0, -0.5, 0, -0.5, 0 ))) #curve

out_r_compo <- lapply(1:dim(scenario_dem)[1], function(parametre){
  para_list <- scenario_dem[parametre,]
  
  multi_var_r_EXTR <- lapply(1:dim(VAR_compo)[1], function(i){tibble(rep = replicate(runs, model_base(SDT = 0.5,
                                                                                                      Noise = VAR_compo[i, 1], climatic.fluctuation = VAR_compo[i, 2],
                                                                                                      inter_surv_juv = -2.17,
                                                                                                      inter_transi = -0.12,
                                                                                                      inter_surv_adu = -4.58,
                                                                                                      inter_fec_adu = 3,
                                                                                                      #TRAIT
                                                                                                      Trait.function = "sigmoid", #(curve - sigmoid - linear)
                                                                                                      
                                                                                                      beta_trait_juv = -1,
                                                                                                      beta_trait_ad = -1,
                                                                                                      
                                                                                                      #DEMOGRAPHIC RATE
                                                                                                      beta_surv_juv = para_list[2],
                                                                                                      beta_surv_adu = para_list[4],
                                                                                                      beta_fec_adu = para_list[8],
                                                                                                      beta_transi = para_list[6], 
                                                                                                      
                                                                                                      beta_2surv_juv = para_list[1], 
                                                                                                      beta_2surv_adu = para_list[3],
                                                                                                      beta_2fec_adu = para_list[7],
                                                                                                      beta_2transi = para_list[5]
                                                                                                      
                                                                                                      
                                                                                                      
  ))) })
  
  rExt_curve <- function_summary(multi_var_r_EXTR)
  rExt_curve$scenario <- parametre
  
  return(rExt_curve)
  
}  )


out_r_compo <- do.call(rbind,out_r_compo)
out_r_compo$i <- as.factor(out_r_compo$i)
out_r_compo$scenario <- as.factor(out_r_compo$scenario)

out_r_compo_SigNeg <- out_r_compo

out_r_compo_gaussian$trait <- "gaussian"
out_r_compo_LinPo$trait <- "LinPo"
out_r_compo_LinNeg$trait <- "LinNeg"
out_r_compo_SigPo$trait <- "SigPo"
out_r_compo_SigNeg$trait <- "SigNeg"

out_r_compo_tot <- rbind(out_r_compo_gaussian, out_r_compo_LinPo, out_r_compo_LinNeg, out_r_compo_SigPo, out_r_compo_SigNeg)
out_r_compo_tot$LHS <- "fast"

saveRDS(out_r_compo_tot , file = "out_r_compo_tot_fastTrial.rds")
out_r_compo_tot <- readRDS( file = "out_r_compo_tot_fastTrial.rds")

out_r_compo_tot1 <- subset(out_r_compo_tot, scenario == 1)
out_r_compo_tot5 <- subset(out_r_compo_tot, scenario == 5)
out_r_compo_tot9 <- subset(out_r_compo_tot, scenario == 9)
out_r_compo_tot13 <- subset(out_r_compo_tot, scenario == 13)
out_r_compo_tot_subtot <- rbind(out_r_compo_tot1, out_r_compo_tot5,  out_r_compo_tot9, out_r_compo_tot13)

rExt_tot_compo_plot <- ggplot(out_r_compo_tot_subtot, aes(x=out_r_compo_tot_subtot$i , y=out_r_compo_tot_subtot$log_GR, color = out_r_compo_tot_subtot$i))+
  geom_boxplot() + 
  facet_grid(out_r_compo_tot_subtot$scenario ~ out_r_compo_tot_subtot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
rExt_tot_compo_plot

#_______________________________________
#total
out_r_compo_tot <- readRDS( file = "out_r_compo_tot_fastTrial.rds")
out_k_compo_tot <- readRDS( file = "out_k_compo_tot_fastTrial.rds")
out_compo_tot <- rbind(out_r_compo_tot, out_k_compo_tot)

saveRDS(out_compo_tot , file = "out_compo_tot_fastTrial.rds")
out_compo_tot <- readRDS( file = "out_compo_tot_fastTrial.rds")

out_compo_tot1 <- subset(out_compo_tot, scenario == 1)
out_compo_tot5 <- subset(out_compo_tot, scenario == 5)
out_compo_tot9 <- subset(out_compo_tot, scenario == 9)
out_compo_tot13 <- subset(out_compo_tot, scenario == 13)
out_compo_tot_sum <- rbind(out_compo_tot1, out_compo_tot5,  out_compo_tot9, out_compo_tot13)

tot_compo_plot <- ggplot(out_compo_tot, aes(x=out_compo_tot$i , y=out_compo_tot$log_GR, color = out_compo_tot$LHS))+
  geom_boxplot() + 
  facet_grid(out_compo_tot$scenario ~ out_compo_tot$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_compo_plot

tot_compo_plot_sum <- ggplot(out_compo_tot_sum, aes(x=out_compo_tot_sum$i , y=out_compo_tot_sum$log_GR, color = out_compo_tot_sum$LHS))+
  geom_boxplot() + 
  facet_grid(out_compo_tot_sum$scenario ~ out_compo_tot_sum$trait)+
  theme(legend.title = element_blank()) +
  labs(title = "Growth rate function of climatic variation in Extrem fast life species") +
  theme(legend.position="bottom")+
  xlab("Climatic Variation") + 
  ylab("Growth rate") 
tot_compo_plot_sum

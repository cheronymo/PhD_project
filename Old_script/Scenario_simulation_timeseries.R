####################################################################################################################
#################         CODE SIMULATION TIMESERIES                ###########################################################
####################################################################################################################
setwd("U:/GUEST/Abteilung6/People/Chero_Guillaume/Outputcluster/Same_time/..")

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
library(magick)

#Parameters
runs <- 1
#nyear <- 500
number_generation <- 100

#List of scenario for the trait-demographic rate relationships
scenario_dem <- t(matrix ( ncol = 6, nrow = 7,
                           c("quadratic", 0, 1, 0, 1, 0, -1,
                             "quadratic", 0, -1, 0, -1, 0, 1,
                             "sigmoid", 0, 1, 0, 1, 0, -1,
                             "sigmoid", 0, -1, 0, -1, 0, 1,
                             "quadratic", 0.5, 0, 0.5, 0, -0.5, 0,
                             "quadratic", -0.5, 0, -0.5, 0, 0.5, 0
                           ))) #curve

#List of scenario for the reaction norm
scenario_trait <- t(matrix(ncol = 5, nrow = 3, 
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


#___________________________________________________________________________________________________________________
#####Function output

# PLot
function_plot_timeseries <- function(dataset){
  
  lapply(1:5, function(trait){
    data_used_trait <- dataset[[trait]]
    trait_condition <- ifelse(trait == 1, "Linear positive", 
                       ifelse(trait == 2, "Linear Negative",
                       ifelse(trait == 3, "Sigmoid positive", 
                       ifelse(trait == 4, "Sigmoid negative" , 
                       ifelse(trait == 5, "Bell-shape" ))))) 
    
      lapply(1:6, function(demo){
        data_used_demo <- data_used_trait[demo]
        data_used_demo <- cbind(time = data.frame(1:nyear), data_used_demo)
        demo_condition <- ifelse(demo == 1, "Linear 1", 
                                  ifelse(demo == 2, "Linear 2",
                                         ifelse(demo == 3, "Sigmoid 1", 
                                                ifelse(demo == 4, "Sigmoid 2" , 
                                                       ifelse(demo == 5, "Bell-shape 1",
                                                              ifelse(demo == 6, "Bell-shape 2"))))))  
        
        plot_OUT <- ggplot(data = data_used_demo, aes(x = data_used_demo$X1.nyear)) + ylim(0, 2000)+
                 geom_line(aes(y = data_used_demo$`1`, colour = "A.Base")) + 
                 geom_line(aes(y = data_used_demo$`2`, colour = "B.MC LF ")) + 
                 geom_line(aes(y = data_used_demo$`3`, colour = "C.MC HF")) + 
                 geom_line(aes(y = data_used_demo$`4`, colour = "D.MC IF")) + 
                 geom_line(aes(y = data_used_demo$`5`, colour = "E.MC DF")) + 
                 geom_line(aes(y = data_used_demo$`6`, colour = "F.MC R")) + 
                 geom_line(aes(y = data_used_demo$`7`, colour = "G.MI")) + 
                 geom_line(aes(y = data_used_demo$`8`, colour = "G.MI F")) + 
                 geom_line(aes(y = data_used_demo$`9`, colour = "H.MI IF")) + 
                 geom_line(aes(y = data_used_demo$`10`, colour = "I.MI DF")) + 
                 geom_line(aes(y = data_used_demo$`11`, colour = "J.MI R"))+
          ylab("Population size") + xlab("Time")+
          scale_colour_discrete(name = "Climate") 
        return(plot_OUT)
        
      })
    
       
  })
  
 }

#Create summuray
outpout_timeseries <- function(data_in){
  output <- lapply(1:dim(VAR_list)[1], function(i){
    truc <- lapply(1:runs, function(laps){ 
      timeseries <- data_in[[i]]$rep[laps]$data_tot$Abundance
      timeseries <- as.data.frame(timeseries)
      colnames(timeseries) <- i
      
      return(timeseries)
    })
    truc2 <- do.call(cbind, truc)
  })
  output_out <- do.call(cbind, output)
  return(output_out)
}



#____________________________________________________________________________________
#Simulation K extrem
nyear = number_generation * 17.6

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
      inter_surv_juv = 0.081,
      inter_surv_adu = 3.15,
      inter_fec_adu = -1.18,
      
      transition.proba = 0.3,
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
    
   
    
    KExt_curve <- outpout_timeseries(multi_var_K_EXTR)

    
    return(KExt_curve)
  })
  
  return(out_K_dem)
  
  
}  )

out_K_trait[[5]][[6]]$`8`

output_plot_k <- function_plot_timeseries(out_K_trait)

for (i in 1:length(output_plot_k)) {
  output_plot_truc <- output_plot_k[[i]]
  jpeg(paste0("./plot/timeseries/", i,"plot_Timeseries_K_.jpg"), width = 7, height = 14, units = 'in', res = 300)
  grid.arrange(output_plot_truc[[1]], output_plot_truc[[2]], output_plot_truc[[3]], 
                       output_plot_truc[[4]], output_plot_truc[[5]], nrow = 5)
  dev.off()
}


a <- grid.arrange(output_plot_k[[1]][[1]], output_plot_k[[1]][[2]], output_plot_k[[1]][[3]], 
             output_plot_k[[1]][[4]], output_plot_k[[1]][[5]], output_plot_k[[1]][[6]], nrow = 6) 
b <- grid.arrange(output_plot_k[[2]][[1]], output_plot_k[[2]][[2]], output_plot_k[[2]][[3]], 
             output_plot_k[[2]][[4]], output_plot_k[[2]][[5]], output_plot_k[[1]][[6]], nrow = 6)
c <- grid.arrange(output_plot_k[[3]][[1]], output_plot_k[[3]][[2]], output_plot_k[[3]][[3]], 
             output_plot_k[[3]][[4]], output_plot_k[[3]][[5]], output_plot_k[[1]][[6]], nrow = 6)
d <- grid.arrange(output_plot_k[[4]][[1]], output_plot_k[[4]][[2]], output_plot_k[[4]][[3]], 
             output_plot_k[[4]][[4]], output_plot_k[[4]][[5]], output_plot_k[[1]][[6]], nrow = 6)
e <- grid.arrange(output_plot_k[[5]][[1]], output_plot_k[[5]][[2]], output_plot_k[[5]][[3]], 
             output_plot_k[[5]][[4]], output_plot_k[[5]][[5]], output_plot_k[[1]][[6]], nrow = 6)
first_col <- grid.arrange(textGrob("K - selected strategies"),
                          textGrob("Dem \n Linear 1"),textGrob("Dem \n Linear 2"),
                          textGrob("Dem \n Sigmoid 1"),textGrob("Dem \n Sigmoid 2"),
                          textGrob("Dem \n Gaussian 2"), textGrob("Dem \n Gaussian 2"), nrow = 6)

pdf("./plot/timeseries/plot_sum_Timeseries_K_.pdf", 60, 60)
grid.arrange(a, b, c, d, e, nrow = 1)
dev.off()

saveRDS(output_plot_k , file = "./plot/timeseries/output_plot_k_plot.rds")


#____________________________________________________________________________________
#Simulation k
nyear = number_generation * 6.23

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
      inter_surv_juv = -0.33,
      inter_surv_adu = 1.49,
      inter_fec_adu = 0.02,
      
      transition.proba = 0.4,
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
    
    Kmix_curve <- outpout_timeseries(multi_var_K_mix)
    
    
    return(Kmix_curve)
  })
  
  return(out_Kmix_dem)
  
  
}  )

out_Kmix_trait

output_plot_kimx <- function_plot_timeseries(out_Kmix_trait)

for (i in 1:length(output_plot_kimx)) {
  output_plot_truc <- output_plot_kimx[[i]]
  jpeg(paste0("./plot/timeseries/", i,"plot_Timeseries_Kmix_.jpg"), width = 7, height = 14, units = 'in', res = 300)
  grid.arrange(output_plot_truc[[1]], output_plot_truc[[2]], output_plot_truc[[3]], 
               output_plot_truc[[4]], output_plot_truc[[5]], nrow = 5)
  dev.off()
}


f <- grid.arrange(output_plot_kimx[[1]][[1]], output_plot_kimx[[1]][[2]], output_plot_kimx[[1]][[3]], 
                  output_plot_kimx[[1]][[4]], output_plot_kimx[[1]][[5]], output_plot_kimx[[1]][[6]], nrow = 6) 
g <- grid.arrange(output_plot_kimx[[2]][[1]], output_plot_kimx[[2]][[2]], output_plot_kimx[[2]][[3]], 
                  output_plot_kimx[[2]][[4]], output_plot_kimx[[2]][[5]], output_plot_kimx[[2]][[6]], nrow = 6)
h <- grid.arrange(output_plot_kimx[[3]][[1]], output_plot_kimx[[3]][[2]], output_plot_kimx[[3]][[3]], 
                  output_plot_kimx[[3]][[4]], output_plot_kimx[[3]][[5]], output_plot_kimx[[3]][[6]], nrow = 6)
i <- grid.arrange(output_plot_kimx[[4]][[1]], output_plot_kimx[[4]][[2]], output_plot_kimx[[4]][[3]], 
                  output_plot_kimx[[4]][[4]], output_plot_kimx[[4]][[5]], output_plot_kimx[[4]][[6]], nrow = 6)
j <- grid.arrange(output_plot_kimx[[5]][[1]], output_plot_kimx[[5]][[2]], output_plot_kimx[[5]][[3]], 
                  output_plot_kimx[[5]][[4]], output_plot_kimx[[5]][[5]], output_plot_kimx[[5]][[6]], nrow = 6)

pdf("./plot/timeseries/plot_sum_Timeseries_Kmix_.pdf", 60, 60)
grid.arrange(f, g, h, i, j, nrow = 1)
dev.off()

saveRDS(output_plot_kimx , file = "./plot/timeseries/output_plot_kimx_plot.rds")

#____________________________________________________________________________________
#Simulation r MIXTE
nyear = number_generation  * 2.43

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
      inter_surv_juv = -1.33,
      inter_surv_adu = -0.96,
      inter_fec_adu = 1.62,
      
      transition.proba = 0.7,
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
    
    rmix_curve <- outpout_timeseries(multi_var_r_mix)
    
    
    return(rmix_curve)
  })
  
  return(out_rmix_dem)
  
  
}  )

out_rmix_trait

output_plot_rmix <- function_plot_timeseries(out_rmix_trait)

for (i in 1:length(output_plot_rmix)) {
  output_plot_truc <- output_plot_rmix[[i]]
  jpeg(paste0("./plot/timeseries/", i,"plot_Timeseries_rmix_.jpg"),  width = 7, height = 14, units = 'in', res = 300)
  grid.arrange(output_plot_truc[[1]], output_plot_truc[[2]], output_plot_truc[[3]], 
               output_plot_truc[[4]], output_plot_truc[[5]], nrow = 5)
  dev.off()
}

k <- grid.arrange(output_plot_rmix[[1]][[1]], output_plot_rmix[[1]][[2]], output_plot_rmix[[1]][[3]], 
                  output_plot_rmix[[1]][[4]], output_plot_rmix[[1]][[5]], output_plot_rmix[[1]][[6]], nrow = 6) 
l <- grid.arrange(output_plot_rmix[[2]][[1]], output_plot_rmix[[2]][[2]], output_plot_rmix[[2]][[3]], 
                  output_plot_rmix[[2]][[4]], output_plot_rmix[[2]][[5]], output_plot_rmix[[2]][[6]], nrow = 6)
m <- grid.arrange(output_plot_rmix[[3]][[1]], output_plot_rmix[[3]][[2]], output_plot_rmix[[3]][[3]], 
                  output_plot_rmix[[3]][[4]], output_plot_rmix[[3]][[5]], output_plot_rmix[[3]][[6]], nrow = 6)
n <- grid.arrange(output_plot_rmix[[4]][[1]], output_plot_rmix[[4]][[2]], output_plot_rmix[[4]][[3]], 
                  output_plot_rmix[[4]][[4]], output_plot_rmix[[4]][[5]], output_plot_rmix[[4]][[6]], nrow = 6)
o <- grid.arrange(output_plot_rmix[[5]][[1]], output_plot_rmix[[5]][[2]], output_plot_rmix[[5]][[3]], 
                  output_plot_rmix[[5]][[4]], output_plot_rmix[[5]][[5]], output_plot_rmix[[5]][[6]], nrow = 6)

pdf("./plot/timeseries/plot_sum_Timeseries_rmix_.pdf", 60, 60)
grid.arrange(k, l, m, n, o, nrow = 1)
dev.off()

saveRDS(output_plot_rmix , file = "./plot/timeseries/output_plot_rmix_plot.rds")


#____________________________________________________________________________________
#Simulation r ext
nyear = number_generation  * 2.02


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
      inter_surv_juv = -2.15,
      inter_surv_adu = -4.57,
      inter_fec_adu = 2.4,
      
      transition.proba = 0.9,
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
    
    rExt_curve <- outpout_timeseries(multi_var_r_EXTR)
    
    
    return(rExt_curve)
  })
  
  return(out_r_dem)
  
  
}  )

out_r_trait

output_plot_r <- function_plot_timeseries(out_r_trait)

for (i in 1:length(output_plot_r)) {
  output_plot_truc <- output_plot_r[[i]]
  jpeg(paste0("./plot/timeseries/", i,"plot_Timeseries_r_.jpg"),  width = 7, height = 14, units = 'in', res = 300)
  grid.arrange(output_plot_truc[[1]], output_plot_truc[[2]], output_plot_truc[[3]], 
               output_plot_truc[[4]], output_plot_truc[[5]], nrow = 5)
  dev.off()
}


p <- grid.arrange(output_plot_r[[1]][[1]], output_plot_r[[1]][[2]], output_plot_r[[1]][[3]], 
                  output_plot_r[[1]][[4]], output_plot_r[[1]][[5]], output_plot_r[[1]][[6]], nrow = 6) 
q <- grid.arrange(output_plot_r[[2]][[1]], output_plot_r[[2]][[2]], output_plot_r[[2]][[3]], 
                  output_plot_r[[2]][[4]], output_plot_r[[2]][[5]], output_plot_r[[2]][[6]], nrow = 6)
r <- grid.arrange(output_plot_r[[3]][[1]], output_plot_r[[3]][[2]], output_plot_r[[3]][[3]], 
                  output_plot_r[[3]][[4]], output_plot_r[[3]][[5]], output_plot_r[[3]][[6]], nrow = 6)
s <- grid.arrange(output_plot_r[[4]][[1]], output_plot_r[[4]][[2]], output_plot_r[[4]][[3]], 
                  output_plot_r[[4]][[4]], output_plot_r[[4]][[5]], output_plot_r[[4]][[6]], nrow = 6)
t <- grid.arrange(output_plot_r[[5]][[1]], output_plot_r[[5]][[2]], output_plot_r[[5]][[3]], 
                  output_plot_r[[5]][[4]], output_plot_rmix[[5]][[5]], output_plot_rmix[[5]][[6]], nrow = 6)

pdf("./plot/timeseries/plot_sum_Timeseries_r_.pdf", 60, 60)
grid.arrange(p, q, r, s, t, nrow = 1)
dev.off()


saveRDS(output_plot_r , file = "./plot/timeseries/output_plot_r_plot.rds")


#####################################################################################################################
#PLot summurize______________________________________________________________________________________________________
output_plot_k <- readRDS( file = "./plot/timeseries/output_plot_k_plot.rds")
output_plot_kimx <- readRDS( file = "./plot/timeseries/output_plot_kimx_plot.rds")
output_plot_rmix <- readRDS( file = "./plot/timeseries/output_plot_rmix_plot.rds")
output_plot_r <- readRDS( file = "./plot/timeseries/output_plot_r_plot.rds")



Whale <- ggdraw() + draw_image("./plot/picture/whale.png")
colK <- grid.arrange(Whale, output_plot_k[[1]][[1]], output_plot_k[[2]][[1]], output_plot_k[[3]][[1]], output_plot_k[[4]][[1]], output_plot_k[[5]][[1]], 
                     output_plot_k[[5]][[2]], output_plot_k[[4]][[3]], output_plot_k[[5]][[3]], output_plot_k[[3]][[4]], output_plot_k[[4]][[4]], 
                     output_plot_k[[5]][[4]], output_plot_k[[3]][[5]], output_plot_k[[4]][[5]], output_plot_k[[5]][[5]], 
                     output_plot_k[[3]][[6]], output_plot_k[[4]][[6]], output_plot_k[[5]][[6]]
                     , nrow = 18)

Roe <- ggdraw() + draw_image("./plot/picture/roeDeer.png")
colKmix <- grid.arrange(Roe, output_plot_kimx[[1]][[1]], output_plot_kimx[[2]][[1]], output_plot_kimx[[3]][[1]], output_plot_kimx[[4]][[1]], output_plot_kimx[[5]][[1]], 
                        output_plot_kimx[[5]][[2]], output_plot_kimx[[4]][[3]], output_plot_kimx[[5]][[3]], output_plot_kimx[[3]][[4]], output_plot_kimx[[4]][[4]], 
                        output_plot_kimx[[5]][[4]], output_plot_kimx[[3]][[5]], output_plot_kimx[[4]][[5]], output_plot_kimx[[5]][[5]], 
                        output_plot_kimx[[3]][[6]], output_plot_kimx[[4]][[6]], output_plot_kimx[[5]][[6]]
                        , nrow = 18)

rabbit <- ggdraw() + draw_image("./plot/picture/rabbit.png")
colrmix <- grid.arrange(rabbit, output_plot_rmix[[1]][[1]], output_plot_rmix[[2]][[1]], output_plot_rmix[[3]][[1]], output_plot_rmix[[4]][[1]], output_plot_rmix[[5]][[1]], 
                        output_plot_rmix[[5]][[2]], output_plot_rmix[[4]][[3]], output_plot_rmix[[5]][[3]], output_plot_rmix[[3]][[4]], output_plot_rmix[[4]][[4]], 
                        output_plot_rmix[[5]][[4]], output_plot_rmix[[3]][[5]], output_plot_rmix[[4]][[5]], output_plot_rmix[[5]][[5]], 
                        output_plot_rmix[[3]][[6]], output_plot_rmix[[4]][[6]], output_plot_rmix[[5]][[6]]
                        , nrow = 18)

rat <- ggdraw() + draw_image("./plot/picture/rats.png")
colr <- grid.arrange(rat, output_plot_r[[1]][[1]], output_plot_r[[2]][[1]], output_plot_r[[3]][[1]], output_plot_r[[4]][[1]], output_plot_r[[5]][[1]], 
                     output_plot_r[[5]][[2]], output_plot_r[[4]][[3]], output_plot_r[[5]][[3]], output_plot_r[[3]][[4]], output_plot_r[[4]][[4]], 
                     output_plot_r[[5]][[4]], output_plot_r[[3]][[5]], output_plot_r[[4]][[5]], output_plot_r[[5]][[5]], 
                     output_plot_r[[3]][[6]], output_plot_r[[4]][[6]], output_plot_r[[5]][[6]]
                     , nrow = 18)


pdf("./plot/timeseries/plot_sum_Timeseries_total.pdf", 40, 60)
grid.arrange(colK, colKmix, colrmix, colr, nrow = 1)
dev.off()



mat = matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
               2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
),17 , 2)
print(mat)
layout(mat)

grid.arrange(colK, colKmix, colrmix, colr, nrow = 1)

curve(1*x, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 1")
curve(-1*x, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-1*x, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 2")
curve(1*x, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 3")
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 4")
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 5")
curve(0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 6")
curve(-0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )

par(1,1)
 
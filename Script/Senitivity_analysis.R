##########################################################
#               SENSITIVITY ANALYSIS                     #
##########################################################

library(multisensi)
library(raster)


#______________________________________________________________
##Simulation sensitivity

parameters <- expand.grid(b = c(0.00011, 0.00010, 0.00009), 
                          SDT = c(0, 0.5, 1), 
                          beta_trait = c(1, -1, 0),
                          beta_dem = c(1, -1, 0)
                          )

nYears = 200 
runs = 100

sensi <- rep(time = runs, lapply(1:dim(parameters)[1], function(i){
  model <- model_base(nYears = nYears, 
             b = parameters$b[i],
             SDT = parameters$SDT[i],
             beta_trait_ad = parameters$beta_trait[i],
             beta_trait_juv = parameters$beta_trait[i],
             
             climatic.mean = "increase",   
             climatic.fluctuation = "constant",    
             Noise = "white",
             trait.function = "quadratic", 
             
             demo.function =  "quadratic", 
             
             beta_surv_juv = parameters$beta_dem[i],
             beta_surv_adu = parameters$beta_dem[i],
             beta_fec_adu = - parameters$beta_dem[i],
             
             inter_surv_juv = -1.33,  #parametrization for fast life strategies
             inter_surv_adu = -0.96,
             inter_fec_adu = 1.62,
             transition.proba = 0.7
  )
  
  Ab <- mean(model$data_tot$Abundance)
  
  Gr_list <- (unlist(lapply(1:nYears, function(t){log(model$data_tot$Abundance[t]/model$data_tot$Abundance[t-1])})))
  Gr_list[is.nan(Gr_list)] <- 0
  Gr_list[!is.finite(Gr_list)] <- 0
  Gr <- mean(Gr_list)
  
  CV <- cv(model$data_tot$Abundance)
  
  
  
  
  data = data.frame(Ab = Ab,
                    GR = Gr,
                    CV = CV,
                    b = parameters$b[i],
                    SDT = parameters$SDT[i],
                    beta_trait = parameters$beta_trait[i],
                    beta_dem = parameters$beta_dem[i])
  return(data)
  
  
}))



sensi_data <- do.call(rbind, sensi)

#______________________________________________________________
##Data manipulation

#Sensitivity on Growth Rate


sensiGR_min_b <- mean(sensi_data$GR[sensi_data$b == 0.00009]) - mean(sensi_data$GR[sensi_data$b == 0.00010])
sensiGR_max_b <- mean(sensi_data$GR[sensi_data$b == 0.00010]) - mean(sensi_data$GR[sensi_data$b == 0.00011])

sensiGR_min_SDT <- mean(sensi_data$GR[sensi_data$SDT == 0.00]) - mean(sensi_data$GR[sensi_data$SDT == 0.5])
sensiGR_max_SDT <- mean(sensi_data$GR[sensi_data$SDT == 0.5]) - mean(sensi_data$GR[sensi_data$SDT == 1])

sensiGR_min_Btrait <- mean(sensi_data$GR[sensi_data$beta_trait == -1]) - mean(sensi_data$GR[sensi_data$beta_trait == 0])
sensiGR_max_Btrait <- mean(sensi_data$GR[sensi_data$beta_trait == 0]) - mean(sensi_data$GR[sensi_data$beta_trait == 1])

sensiGR_min_Bdem <- mean(sensi_data$GR[sensi_data$beta_dem == -1]) - mean(sensi_data$GR[sensi_data$beta_dem == 0])
sensiGR_max_Bdem <- mean(sensi_data$GR[sensi_data$beta_dem == 0]) - mean(sensi_data$GR[sensi_data$beta_dem == 1])


data_sensiGR <-data.frame(parameters = c("b", "b", "SDT", "SDT", "Bt", "Bt", "Bd", "Bd"),
                          group = c("min", "max","min", "max","min", "max","min", "max"),
                          GR = c(sensiGR_min_b, sensiGR_max_b,
                                     sensiGR_min_SDT, sensiGR_max_SDT,
                                     sensiGR_min_Btrait, sensiGR_max_Btrait,
                                     sensiGR_min_Bdem, sensiGR_max_Bdem))

ggplot(data = data_sensiGR, aes(x = parameters, y = GR, fill = group)) +
  geom_bar(stat="identity", position=position_dodge())




#Sensitivity on CV
sensiCV_min_b <- mean(sensi_data$CV[sensi_data$b == 0.00009]) - mean(sensi_data$CV[sensi_data$b == 0.00010])
sensiCV_max_b <- mean(sensi_data$CV[sensi_data$b == 0.00010]) - mean(sensi_data$CV[sensi_data$b == 0.00011])

sensiCV_min_SDT <- mean(sensi_data$CV[sensi_data$SDT == 0.00]) - mean(sensi_data$CV[sensi_data$SDT == 0.5])
sensiCV_max_SDT <- mean(sensi_data$CV[sensi_data$SDT == 0.5]) - mean(sensi_data$CV[sensi_data$SDT == 1])

sensiCV_min_Btrait <- mean(sensi_data$CV[sensi_data$beta_trait == -1]) - mean(sensi_data$CV[sensi_data$beta_trait == 0])
sensiCV_max_Btrait <- mean(sensi_data$CV[sensi_data$beta_trait == 0]) - mean(sensi_data$CV[sensi_data$beta_trait == 1])

sensiCV_min_Bdem <- mean(sensi_data$CV[sensi_data$beta_dem == -1]) - mean(sensi_data$CV[sensi_data$beta_dem == 0])
sensiCV_max_Bdem <- mean(sensi_data$CV[sensi_data$beta_dem == 0]) - mean(sensi_data$CV[sensi_data$beta_dem == 1])


data_sensiCV <-data.frame(parameters = c("b", "b", "SDT", "SDT", "Bt", "Bt", "Bd", "Bd"),
                          group = c("min", "max","min", "max","min", "max","min", "max"),
                          CV = c(sensiCV_min_b, sensiCV_max_b,
                                 sensiCV_min_SDT, sensiCV_max_SDT,
                                 sensiCV_min_Btrait, sensiCV_max_Btrait,
                                 sensiCV_min_Bdem, sensiCV_max_Bdem))

ggplot(data = data_sensiCV, aes(x = parameters, y = CV, fill = group)) +
  geom_bar(stat="identity", position=position_dodge())

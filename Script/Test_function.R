##########################################################
#                TEST FUNCTION                           #
##########################################################
#This code contains the different test on function into the population model. 

#__________________________________________________________________________________________________________________
#Climate generator
library(multiplex)
library(colorednoise)

nYears = 200
meanT = 0
SDT = 0.5

time = (1:nYears)

climate_white0.5 <- rnorm(200, 0, 0.3) # generate climate list
plot(climate_white0.5, type = "l", ylim = c(-1,1))

plot(climate_red <- noise(kind = "red", duration = 200)@left , type = "l")

noise(kind = "red", duration = 200)@left 

climate_whiteInc0.5 <- (unlist(lapply(seq(0,SDT, SDT/nYears), function(a){rnorm(1, 0, a)})))   ## increase SDT
climate_whiteDec0.5 <- (unlist(lapply(rev(seq(0,SDT, SDT/nYears)), function(a){rnorm(1, 0, a)})))  ## decrease SDT

  
plot(noise(kind = "red", duration = 200 )@left , type ="l", ylab = "Climate")

trend_clim <- 2/100
first_climate <- 0 * trend_clim
last_climate <- nYears * trend_clim
meaT_list <- seq(first_climate, last_climate, length.out= nYears)
SDT_list <- rev(seq(0,0, length.out= nYears))
climate_inLess <- unlist(lapply(1:nYears, function(a){rnorm(1, meaT_list[a], SDT_list[a])})) ## increase SDT
plot(climate_inLess, type = "l")

plot(rnorm(200, 0, 0.2), type = "l")  
plot(diffinv(rnorm(200, 0, 0.2)), type = "l")
plot(filter(rnorm(200, 0, 0.2), filter=rep(1,10), circular=TRUE))

#Better function (Ruokolainen et al. (2009) <doi:10.1016/j.tree.2009.04.009>)
plot(colored_noise(200, 0, 0, 0), type = "l", ylab = "", xlab = "", main = NULL)


#plot figure scenario
trend = 0.5
last_sdt_climate <- -trend * 200
meaT_list <- seq(0, 2, length.out= 200)
SDT_list <-  (seq(0.5,0, length.out= 200))
SDT_list[SDT_list < 0] <- 0
climate <- unlist(lapply(1:nYears, function(a){colored_noise(1, meaT_list[a], SDT_list[a], 0)})) ## increase SDT

#color climate =   
#'#B22222','#DC143C', '#FF4500', '#FF8C00', '#B8860B', '#DAA520','#ADD8E6', '#87CEEB', '#00BFFF', '#6495ED', '#1E90FF'
plot <- ggplot(data = data.frame(time = c(1:200),
                                 climate = climate))+  ## increase SDT
  geom_line(aes(time, climate), color = '#00BFFF') + 
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.x=element_blank())

pdf("./plot/figure_scenario/9.pdf", 7, 5)
ggarrange(plot)
dev.off()

#RELATION

pdf("./plot/figure_scenario/bel.pdf", 7, 5)
curve(-0.5*x*x, xlim = c(-1,1), ylab = " ", xlab = " "  )
dev.off()


############################################
#NOISE (without the function, not used)
climate.temporal.auto <- function(Mean.T = 0, SdT.T = 0.5, tau = 1, nYears = 200, dt = 0.5){
   xi0 = 0

   dW = sqrt(dt)*rnorm(nYears, Mean.T, SdT.T)         # Brownian increments
   W = cumsum(dW)                 # discretized Brownian path 
   
   xi = matrix(0,1,nYears)                  # preallocate for efficiency
   
   xi[1] = xi0 - dt*xi0/tau + dW[1] 
   
   for (j in 2:nYears){
     xi[j] = xi[j-1] - dt*xi[j-1]/tau + dW[j];
   }
   
   #plot(1,1,pch = "",ylim = c(-1,1), xlim = c(0,nYears), xlab = "Years", ylab="Climate", xaxt="n")  # set up blank plot
   #points(t(xi) , type = "l",lwd = 1)     # plot out each life stage abundance, one at a time
   #points(dW, type = "l",lwd = 1, col = "red")
   
   return(as.numeric(xi))
   }
 
a <- climate.temporal.auto(Mean.T = 0, SdT.T = 0.3, tau = 1, nYears = 200, dt = 0.1)
plot(a, type = "l",lwd = 1, col = "red", ylim = c(-1,1))

INC <- climate.temporal.auto(Mean.T = 0, SdT.T = 0.5, tau = 1, nYears = 200, dt = 0.1) + seq(0, 2, length.out= 200)
plot(INC, type = "l",lwd = 1)

##Better function (Ruokolainen et al. (2009) <doi:10.1016/j.tree.2009.04.009>)
plot(colored_noise(200, 0, 0.5, 0.9), type = "l")


#___________________________________________________________________________________________________________________
#Trait calculation
library(sigmoid)
Temp <- climate[1] 

curve( (-beta_TM_Juv * x^2) + inter_TM_Juv, xlim = c(-1,1) ) #relation climate - traits

SD = 0.6
linear = 1

curve( (1/((SD) * sqrt(2 * pi))) *  exp(-0.5*((x-0)/(SD))^2) - (1/((SD*0.8) * sqrt(2 * pi)))  , xlim = c(-2,2), ylab = "Trait", xlab = "Temperature"  ) #relation climate - traits
trait_curve <- function(x){(1/(SD * sqrt(2 * pi))) *  exp(-0.5*((x-0)/SD)^2) - (1/(SD * sqrt(2 * pi)))}
trait_curve(-1)

curve(x * 1, xlim = c(-1,1), ylab = "Trait", xlab = "Temperature"   )
trait_lin <- function(x){x * linear}
trait_lin(0)

curve(1/(1+exp(-5*(1/(1+exp(-5*x)) - 0.5))) - 0.5, xlim = c(-1,1), ylab = "Trait", xlab = "Temperature"  )

trait_lin <- function(x){(1/(1+exp(-5*x)) - 0.5)}
trait_lin(-2)



trait <- lapply(climate, function(Temp){
  Juv_Trait_mean <- (beta_2TM_Juv * Temp * Temp ) + (beta_TM_Juv * Temp ) + inter_TM_Juv       # calcul Juvenile trait mean as function of temperature
})
trait <- unlist(trait)
plot(trait, type = "l") #trait time series


#function trait calculation
if (trait.function == "quadratic"){
  trait.calc <- function(beta, beta.sqrt, climate.value, intercept.trait){intercept.trait +  (beta * climate.value) + (beta.sqrt * climate.value^2)}
} else {
  trait.calc <- function(beta, climate.value, intercept.trait){(1/(1+exp(-beta*climate.value)) - intercept.trait)}
}

trait.calc(beta = 1, beta.sqrt = 1, climate.value = 0, intercept.trait = 0)       # calcul Juvenile trait mean as function of temperature


#__________________________________________________________________________________________________________________
#Demographic rate calcualtion
inter_surv_juv = 0
beta_surv_juv = 0.5
beta_2surv_juv =  0.25
Juv_Trait_mean = 0
Surv_lin_Juv <-  inter_surv_juv +  (beta_surv_juv * Juv_Trait_mean) +(beta_2surv_juv * Juv_Trait_mean^2) #function

curve(beta_2surv_juv*x^2 + beta_surv_juv*x, xlim = c(-2,2), ylab = "Demographic rate", xlab = "Trait"  ) #relation climate - traits

dem <- lapply(trait, function(Juv_Trait_mean){
  Surv_lin_Juv <-  inter_surv_juv +  (beta_surv_juv * Juv_Trait_mean) +(beta_2surv_juv * Juv_Trait_mean) #function
})
dem <- unlist(dem)
plot(dem, type = "l") #trait time series

curve((1/(1+exp(-5*x)) - 0.5) , xlim = c(-1,1), ylab = "Trait", xlab = "Temperature"  )

#function demographic rate calculation
if (demo.function == "quadratic"){
  demo.calc <- function(beta, beta.sqrt, trait.valu, intercept.demo){intercept.trait +  (beta * trait.valu) + (beta.sqrt * trait.valu^2)}
} else {
  demo.calc <- function(beta, trait.valu, intercept.demo){(1/(1+exp(-beta*trait.valu)) - intercept.trait)}
}

#________________________________________________________________________________________________________________
####Population dynamics
##Link function###################
dem = 5
plogis(5)
dem_prob <- 1 / (1 + exp(-dem))                                                    #logit link fuction
plot(dem_prob, type = "l") #trait time series

##Density dep####################
nsum = 200
b = 0.01
#______________________________
#BH
dem_prob_DD_BH <- dem_prob / (1 + (b * nsum))                    #DD juvenil survival
plot(dem_prob_DD_BH, type = "l") #trait time series

plot(dem_prob, dem_prob_DD_BH, type = "l") #trait time series

#Test N
survival = 0.5
test_BH <- lapply(150:250, function(nsum){
  dem_rate <- survival / (1 + (b * nsum))                    #DD juvenil survival
  
})
test_BH <- unlist(test_BH)
plot(150:250, test_BH)

#Test b
survival = 0.5
test_BH <- lapply(c(0.0001, 0.001, 0.01, 0.1), function(b){
  dem_rate <- survival / (1 + (b * nsum))                    #DD juvenil survival
  
})
test_BH <- unlist(test_BH)
plot(c(0.0001, 0.001, 0.01, 0.1), test_BH, type = "l")

#______________________________
#Ricker
dem_prob_DD_R <- dem_prob * exp(-b * nsum)                      #DD juvenil survival
plot(dem_prob_DD_R, type = "l") #trait time series

plot(dem_prob, dem_prob_DD_R, type = "l") #trait time series

#Test N
survival = 0.5
test_R <- lapply(150:250, function(nsum){
  dem_rate <- survival * exp(-b * nsum)                      #DD juvenil survival
  
})
test_R <- unlist(test_R)
plot(150:250, test_R)

#Test b
survival = 0.5
test_R <- lapply(c(0.0001, 0.001, 0.01, 0.1), function(b){
  dem_rate <- survival * exp(-b * nsum)                      #DD juvenil survival
  
})
test_R <- unlist(test_R)
plot(c(0.0001, 0.001, 0.01, 0.1), test_R, type = "l")

#_______________________________________________________________________________________________________________
#Matrix
library(gtools)
surv_Juv = 0.2
fec_Adu = 4.9
Trans_juv_to_ad = 0.9
surv_Adul = 0.1
n = c(100, 100)

#prob
-10.24341 * exp(-0.01 * 200)                       
plogis(-1.386295)
inv.logit(-1.386295)

#fec
11.74295 * exp(-0.01 * 200)                       
exp(1.589235)


A <- matrix(
  c( surv_Juv*(1-Trans_juv_to_ad)   ,fec_Adu ,  
     surv_Juv*Trans_juv_to_ad   ,   surv_Adul     ) ,nrow=2,ncol=2,byrow=T)

n_1 <- n %*% A
 
#loop
allYears <- matrix(0, nrow = 2, ncol = 200)                                         # storage array for abundances for each replicate
allYears[ ,1] <- n

for (t in (2:200)) {
  allYears[ ,t] <- allYears[ ,t-1] %*% A
}

pop = allYears[1,] + allYears[2,]
plot(pop, type = "l")
 
life_history(survi_juv = 0.4, survi_adu = 0.95, fec = 0.2, trans = 0.5) 


#__________________________________________________________________________________________________________________
#FUNCTION TREND GENERATION TIME
library(popbio)
nyear <- 100

data_base <- data.frame(Sj = abs(rnorm(nyear, 0.4, 0.01)),
                        Sa = abs(rnorm(nyear, 0.9, 0.001 )),
                        Fec = abs(rnorm(nyear, 1, 1)),
                        Trans = abs(rnorm(nyear, 0.2, 0.1)))
Calcul_trend_gentime(data_base)

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

#____________________________________________________________________
#TEST FUNCTION
generation_calculation <- lapply(1:nyear,  function(x) {
  gen_matrix <- matrix(c((data_base$Sj[x] * (1- data_base$Trans[x])),
                         (data_base$Trans[x]*data_base$Sj[x]),
                         data_base$Fec[x], data_base$Sa[x]),
                       ncol = 2, nrow = 2)
  gentime <- generation.time(gen_matrix)
  
  return(gentime)
})
list_gen <- do.call(rbind, generation_calculation)
list_gen <- as.data.frame(list_gen)
list_gen$V2 <- c(1:nyear)
plot(list_gen$V2, list_gen$V1)
abline(lm(list_gen$V1 ~ list_gen$V2, data = list_gen ))
summary(lm(list_gen$V1 ~ list_gen$V2))$coefficients[2]

gen_matrix <- matrix(c(data_base$Sj[1], data_base$Trans[1],
                       data_base$Fec[1], data_base$Sa[1]),
                     ncol = 2, nrow = 2)
generation.time(gen_matrix)

gen_matrix <- matrix(c(0, 0.99,
                       20, 0.001),
                     ncol = 2, nrow = 2)
generation.time(gen_matrix)

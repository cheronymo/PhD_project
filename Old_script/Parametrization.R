library(popbio)
library(ggplot2)
library(boot)

############################################################################################################
# MATRIX MODEL

nYears <- 300

allYears <- matrix(0, nrow = 2, ncol = nYears)                                      
allYears[ ,1] <- c(100, 100) 
A <- matrix(
  c( 0.0001   ,500 ,  
     0.0001    ,   0.0001     ) ,nrow=2,ncol=2,byrow=T)
generation.time(A)

for(t in 2:(nYears)){   
  allYears[,t] <-  A %*% allYears[,t-1]
}
 
pop_timeseries <- allYears[1, ] +  allYears[2, ]
plot(1,1,pch="",ylim=c(0,2*max(allYears)),xlim=c(0,nYears+1),xlab="Years",ylab="Abundance",xaxt="n")  # set up blank plot
points(pop_timeseries, type = "l",lwd = 1)     # plot out each life stage abundance, one at a time



################################################################################################################
# SENSITIVITY ANALYSIS 
life_history <- function(survi_juv = 0.1, survi_adu = 0.5, fec = 1.2, trans = 0.4){ ## function which generate growth rate for each matrix

A <- matrix(
  c( survi_juv*(1-trans) , fec    ,  
     survi_juv*trans       , survi_adu) ,nrow=2,ncol=2,byrow=T)

GR = lambda(A)
Gen = generation.time(A)

output <- tibble(matrix=A, GR = GR, Gen = Gen)
return(output)
 }

life_history() # test of function


library(sensitivity)

# Create list of parameter by using the morris function (temporarly just to try)
Q=9  # number of levels of the design
x <- morris(model=NULL, factors=3, r=100, scale=F, binf=0, bsup=1, design=list(type="oat", levels=Q, grid.jump=1)) # other possible type = 'simplex'
dim(x$X)
sort(unique(x$X[,1]))
length(unique(x$X[,1]))

# true parameter ranges
range1 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)    #Surv_juv
range2 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)       #Surv_adul
range3 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)       #Fec


Parameter <- rbind(range1, range2, range3)


# search and replace
gsr <- function(Source, Search, Replace)
{
  if (length(Search) != length(Replace))
    stop("Search and Replace Must Have Equal Number of Items\n")
  Changed <- Source
  for (i in 1:length(Search))
  {
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
    Changed <- replace(Changed, Changed == Search[i], Replace[i])
  }
  cat("\n")
  Changed
}

key <- round(sort(unique(x$X[,1])),2)
length(key)  ## 5

Surv_juv <- gsr(round(x$X[,1],2),key,sort(range1))
Surv_adul <- gsr(round(x$X[,2],2),key,sort(range2))
Fec <- gsr(round(x$X[,3],2),key,sort(range3))

DemRate <- cbind(Surv_juv, Surv_adul, Fec)


## Function to generate growth rate with different combinaison of demographic rates
growth <- lapply(1:dim(DemRate)[1], function(a){
  life_history( survi_juv = x$X[a,1] ,
                survi_adu = x$X[a,2] ,
                fec =  x$X[a,3]      
                  )
})

result <- cbind(DemRate, unlist(growth))
result <- as.data.frame(result)
colnames(result) <- c("Surv_juv","Surv_adu", "Fec", "Growth_rate")

# Subset set of parameters with GR between 0.97 and 1.03 (define if this interval is good?)
sub <- subset(result, result$Growth_rate == 1)
 
# plot
library("plot3Drgl")

scatter3D(x = result$Fec, y = result$Surv_adu, z = result$Growth_rate,  phi = -60, bty ="g",
          xlab = "Fec", ylab = "Surv_adu", zlab = "Surv_juv", 
          main = "Life history strategies", cex = 5)
plotrgl()

 
ggplot(result, aes(x = result$Surv_adu, y = result$Fec, color = result$Growth_rate))+
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))+
  geom_density_2d()

ggplot(sub, aes(x = sub$Surv_adu, y = sub$Fec, color = sub$Growth_rate))+
  geom_point() +
  geom_density_2d()

  scale_color_gradientn(colours = rainbow(5))
ggplot(sub, aes(x = sub$Surv_juv, y = sub$Fec, color = sub$Growth_rate))+
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))
ggplot(sub, aes(x = sub$Surv_juv, y = sub$Surv_adu, color = sub$Growth_rate))+
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))

###############################################################################################################
# GENERATION TIME 
matrice.gen <- matrix(
  c( 0.1   , 20 ,  
     0.47    ,   0.01     ) ,nrow=2,ncol=2,byrow=T)
generation.time(matrice.gen)

life_history <- function(survi_juv = 0.1, survi_adu = 0.5, fec = 1.2, trans = 0.4){ ## function which generate growth rate for each matrix
  
  A <- matrix(
    c( survi_juv*(1-trans) , fec    ,  
       survi_juv*trans       , survi_adu) ,nrow=2,ncol=2,byrow=T)
  
  GR = lambda(A)
  Gen = generation.time(A)
  
  output <- tibble(matrix=A, GR = GR, Gen = Gen)
  return(output)
}


# K ext
life_history(survi_juv = 0.4, survi_adu = 0.94, fec = 0.3, trans = 0.4)

# K 
life_history(survi_juv = 0.3, survi_adu = 0.8, fec = 2, trans = 0.26) 

# r
life_history(survi_juv = 0.4, survi_adu = 0.2, fec = 5, trans = 0.29) 

# r ext
life_history(survi_juv = 0.1, survi_adu = 0.1, fec = 14.5, trans = 0.6) 

#SURVI
##Back transform ______
0.6 / exp(-0.0001 * 100) 
qlogis(0.6060301)         # <- Values as intercept
##Transform ______
plogis(2.932861)
0.9494472 * exp(-0.0001 * 100) 

#FEC
##Back transform ______
14.5 / exp(-0.0001 * 100)
log(14.64573)             # <- Values as intercept
##Transform ______
exp(2.684149)
14.64573 * exp(-0.0001 * 100) 

function_parameter <- function(S_j = 0.1, S_a = 0.1, fec = 20, trans = 0.9, b = 0.0001, N = 200) {
  Sj = qlogis(S_j / exp(-b * (2*N)))
  Sa = qlogis(S_a / exp(-b * (N)))
  Trans = trans
  Fec = log(fec / exp(-b * N))
  return(data.frame(Sj = Sj, Sa = Sa, Fec = Fec, Trans = Trans,
                    Gen = generation.time(matrix(c(S_j*(1-Trans), S_j*Trans, fec, S_a),nrow=2,ncol=2)),
                    A = lambda(matrix(c(S_j*(1-Trans), S_j*Trans, fec, S_a),nrow=2,ncol=2)))) }
  
function_parameter(S_j = 0.5, S_a = 0.94, fec = 0.30, trans = 0.3, b = 0.0001, N = 200) 

###############################################################################################################


data("teasel")
R0 <- net.reproductive.rate(matrice.gen )
LB <- lambda(matrice.gen)
log(R0)/log(LB)
generation.time(matrice.gen )



A <- matrix(
  c(0.01, 500, 
    0.99 , 0.000   
    ) ,nrow=2,ncol=2,byrow=T)
generation.time(A)
net.reproductive.rate(A)
lambda(A)

log(10.1)/log(3.05)

R0 <- net.reproductive.rate(A)
LB <- lambda(A)
log(R0)/log(LB)
generation.time(A)


A1 <- splitA(A)
Fmat <- A1[[2]]
Tmat <- A1[[1]]
s <- length(diag(Tmat))
N <- solve(diag(s) - Tmat)
R <- Fmat %*% N
r <- lambda(R)
eigen(R)
#N <- fundamental.matrix(A)
#Tmat <- N$N
#lambda(Fmat %*% Tmat)

log(r)/log(lambda(A))

MS <- matrix(
  c(0.55, 0.6    ,  1.00,
    0.25,0.25      , 0,
    0, 0.4 , 0.65) ,nrow=3,ncol=3,byrow=T)
net.reproductive.rate(MS)
lambda(MS)
generation.time(MS)

###############################################################################################################
# BACK TRANSFORM

####Density
Y <- X * exp(-0.01 * 200)                      
X <- Y / (exp(-0.01 * 200))

valu_prob <- list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
# Link function 
plogis(0.84)
qlogis(0.6984652) 

#density
0.6058 * exp(-0.01 * 1)    
#reverse
0.9 / exp(-0.0001 * 200)




lapply(0.94, function(x){qlogis((x) / exp(-0.0001 * 100))
 })

lapply(2.93, function(x){plogis(x)*exp(-b * nsum) })

####Fec 
#link
exp(3)
log(2.02)

#density
2 * exp(-0.0001 * 200)                       
#reverse
2 / exp(-0.0001 * 100)



lapply(2, function(x){log((x) / exp(-0.0001 * 100))
})


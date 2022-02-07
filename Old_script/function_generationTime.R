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

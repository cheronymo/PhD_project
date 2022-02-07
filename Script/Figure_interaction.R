####################################################################################################################
#################        Figure interaction                ###########################################################
####################################################################################################################

mat = matrix(c(1, 79, 79, 79, 79, 79, 79, 
               1, 2, 4, 6, 8, 10, 12, 
               1, 3, 5, 7, 9, 11, 13, 
               74, 14, 16, 18, 20, 22, 24, 
               74, 15, 17, 19, 21, 23, 25, 
               75, 26, 28, 30, 32, 34, 36, 
               75, 27, 29, 31, 33, 35, 37, 
               76, 38, 40, 42, 44, 46, 48, 
               76, 39, 41, 43, 45, 47, 49, 
               77, 50, 52, 54, 56, 58, 60, 
               77, 51, 53, 55, 57, 59, 61, 
               78, 62, 64, 66, 68, 70, 72, 
               78, 63, 65, 67, 69, 71, 73,
               1, 80, 80, 80, 80, 80 ,80
            
               ), 7, 14)
print(mat)

layout(mat)


#first column: trait-demographic rate relationship
plot.new()
curve(1*x, from=-1, to=1, xlab="Trait", ylab="Surv", col = "blue", main = "S")
curve(-1*x, from=-1, to=1, xlab="Trait", ylab="Fec", col = "blue", main = "F")
curve(-1*x, from=-1, to=1, xlab="Trait", ylab="Surv", col = "blue", main = "S")
curve(1*x, from=-1, to=1, xlab="Trait", ylab="Fec", col = "blue", main = "F")
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Trait", ylab="Surv", col = "blue", main = "S")
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Trait", ylab="Fec", col = "blue", main = "F")
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Trait", ylab="Surv", col = "blue", main = "S")
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Trait", ylab="Fec", col = "blue", main = "F")
curve(-0.5*x^2, from=-1, to=1, xlab="Trait", ylab="Surv", col = "blue", main = "S")
curve(0.5*x^2, from=-1, to=1, xlab="Trait", ylab="Fec", col = "blue", main = "F")
curve(0.5*x^2, from=-1, to=1, xlab="Trait", ylab="Surv", col = "blue", main = "S")
curve(-0.5*x^2, from=-1, to=1, xlab="Trait", ylab="Fec", col = "blue", main = "F")

#Second column: linear positive reaction norm
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

#Third column: linear negative reaction norm
curve(-1*x, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 2")
curve(1*x, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(1*x, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 1")
curve(-1*x, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 4")
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 3")
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 6")
curve(-0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 5")
curve(0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )

#Fourth column: Sigmoid positive reaction norm
curve(1*(1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 3")
curve(-1*(1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-1*(1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 4")
curve(1*(1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(-5*(1/(1+exp(-5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 6")
curve((1/(1+exp(5*(1/(1+exp(-5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(5*(1/(1+exp(-5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 9")
curve((1/(1+exp(-5*(1/(1+exp(-5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-0.5*(1/(1+exp(-5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 12")
curve(0.5*(1/(1+exp(-5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(0.5*(1/(1+exp(-5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 15")
curve(-0.5*(1/(1+exp(-5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )


#Fifth column: Sigmoid negative reaction norm
curve(1*(1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 4")
curve(-1*(1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-1*(1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 3")
curve(1*(1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(-5*(1/(1+exp(5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 7")
curve((1/(1+exp(5*(1/(1+exp(5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(5*(1/(1+exp(5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 10")
curve((1/(1+exp(-5*(1/(1+exp(5*x)) - 0.5))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-0.5*(1/(1+exp(5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 13")
curve(0.5*(1/(1+exp(5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(0.5*(1/(1+exp(5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 16")
curve(-0.5*(1/(1+exp(5*x)) - 0.5)^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )

#Six column: bell-shaped reaction norm
curve(1*(-0.5*x^2), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 5")
curve(-1*(-0.5*x^2), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-1*(-0.5*x^2), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 6")
curve(1*(-0.5*x^2), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(-5*(-0.5*x^2))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 8")
curve((1/(1+exp(5*(-0.5*x^2))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve((1/(1+exp(5*(-0.5*x^2))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 11")
curve((1/(1+exp(-5*(-0.5*x^2))) - 0.5), from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(-0.5*(-0.5*x^2)^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 14")
curve(0.5*(-0.5*x^2)^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )
curve(0.5*(-0.5*x^2)^2, from=-1, to=1, xlab="Climate", ylab="Surv", col = "red", main = "Scenario 17")
curve(-0.5*(-0.5*x^2)^2, from=-1, to=1, xlab="Climate", ylab="Fec", col = "red" )


#Reaction norm plot
curve(1*x, from=-1, to=1, xlab="Climate", ylab="Trait", col = "blue")
curve(-1*x, from=-1, to=1, xlab="Climate", ylab="Trait", col = "blue")
curve((1/(1+exp(-5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Trait", col = "blue", main = "Reaction norm")
curve((1/(1+exp(5*x)) - 0.5), from=-1, to=1, xlab="Climate", ylab="Trait", col = "blue")
curve(-0.5*x^2, from=-1, to=1, xlab="Climate", ylab="Trait", col = "blue")

#Label
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, label = "Trait-demographic rate relationship", srt = 90,
     cex = 2, col = "black")

#Last column
plot.new()

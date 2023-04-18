# Parameters
k = 100
n0 = 50
#............................

# Functions

#Creating a function for the Ricker model
rkr = function(nt,r = 1.5, k = 100){
  nt1 = nt*exp(r*(1-(nt/k)))
  return(nt1)
}

#Creating a function to iterate over the Ricker function (rkr)
itr = function(tf = 100, rt){
  Nn0 = length(rt)
  n = matrix(NA, nrow = tf, ncol = Nn0) #This matrix stores values of the population over time as it changes according to ricker model
  n[1,] = n0
  for(t in 1:(tf-1)){
    n[t+1,] = rkr(n[t,],r = rt)
  }
  return(n)
}
#............................

# Main

#Generating the N vs. t plot for r = 1.5, 2.3, 2.6, 3
rate1 = c(1.5,2.3,2.6,3)
m1 = itr(tf = 100,rt = rate1) # m is matrix that now stores values of population size over time for the four r values
print(m1) # To test that everything looks right 
# Generating plots using subplot
mf = par(mfrow=c(2,2)) # create a 2x2 set of subplots
for(step in 1:4) {
  plot(1:100,m1[,step],type="l",xlab="Time",ylab="Pop Size", main=paste("r = ",rate[step]))
}
par(mforw=mf)

#Generating the bifurcation diagram

rate2 = seq(1.5, 3.6, by = 0.01) #Initializing vector of rates
len = length(rate2)
m2 = itr(tf = 500,rt = rate2)
l = m2[400:500,]#For the bifurcation plot, we only want population sizes from the last 100 time steps. 
#Plotting bifurcation diagram
matplot(rate2,t(l), xlab="R value", ylab = "Steady State Population",col="blue", pch=16, cex=.4, main="Bifurfaction Diagram of Ricker Model")

#.............................












#............................
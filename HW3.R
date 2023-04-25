setwd("/Users/admin-sqkhan/Downloads/CompMethods")

# Parameters
#Reading the teasel file and generating the teasel matrix
ts = as.matrix(read.table("teasel_stage.txt"))#Turning the table into a matrix
tf = 500 #Number of timesteps
n0 = c(2,rep(0,nstg-1))#Initial stage structure
f = seq(0,75, by = 0.01) # Sequence of fecundity values
#a = 0.01 # For the density dependant model (mod2)

#.................................................


# Functions

# Function to generate stage structure matrix for model 2
# Takes in values for fecundity and no. of timesteps
ssm2 = function(fec = 0.1, tf = 500, a = 0.01){
  m2 = ts
  nstg = nrow(ts)
  nt = matrix(NA,nstg,tf)
  nt[,1] = n0 # Initial age structure distribution
  for(t in 1:(tf-1)){
    div = 1 + (a*sum(nt[,t]))
    m2[1,7] = fec/div
    nt[,t+1] = m2%*%nt[,t]
  }
  return(nt)
}

# Function to find leading eigen value
leval = function(l){
  Re(eigen(l)$values[1])
}



#.................................................


# Main
mod1 = ts #Initial model1 matrix before replacing [1,7] for f value
len = length(f)
vle = rep(NA,len) # Vector that stores values of the leading eigen vector for model 1
for(t1 in 1:len){
  mod1[1,7] = f[t1]# Replacing values of f in ts matrix and calculating the leading eigen value of matrix
  vle[t1] = leval(mod1)
}

m = ssm2()

vpop = rep(NA,len) # Vector that stores values of the total population size at stable equilibrium
for(t2 in 1:len){
  mod2 = ssm2(fec = f[t2]) # Finding the age structure distribution for 500 timesteps for each value of f
  vpop[t2] = sum(mod2[,tf])
}

# Making plots 1(eigen value vs f) and 2(total population at stable equuilibrium vs f)

mf = par(mfrow=c(1,2))
plot(f,vle,type = 'l', xlab = "Fecundity", ylab = "Expected gf (leading eigen)")
lines(f,rep(1,len),col = 'red')

plot(f,vpop,type = 'l',xlab = "Fecundity", ylab = "Total pop")
par(mforw=mf)


# Making plot 2 - total population vs. f 
#plot(f,vpop,type = 'l', xlabel = "Fecundity", ylabel = "Total population at stable equilibrium")


# Making plot 1 mod1 eigen value vs. f (with red line at growth rate = 1)
#plot(f,vle,type = 'l', xlabel = "Fecundity", ylabel = "Expected gf (leading eigen)")
#lines(f,rep(1,len),col = 'red') # expected growth rate as a horizontal red line


#.................................................

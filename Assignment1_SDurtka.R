# ---
# title: "Assignment 1: R Basics"
# subtitle: ECNS 460/560 Fall 2024
# date: Due Thursday, August 29
# Name:  Steven Durtka
# ---

# Part I

#1. Vectors

Temp=c(35, 88, 42, 84, 81, 50)
Temp

#Calculate difference between each temperature and the lowest temperature in the vector

TempDiff=Temp-min(Temp)   
TempDiff

#Calculate the mean of the last three entries

mean(Temp[4:6])
mean(Temp[c(4, 5, 6)])

# 2. Matrices

#a. Create a matrix
row=c(0,1,2,3)
my_matrix = matrix(c(row, row*2, row*3, row*4, row*5), nrow=5, ncol = 4, byrow = TRUE)
my_matrix

#b. Transpose the 3rd and 4th columns to create a 2x5 matrix

NewMatrix = t(my_matrix[1:5,3:4])
NewMatrix

#c. How many elements of the NewMatrix are equal to 6

count=0

for (i in 1:2) 
  {
  for (j in 1:5) 
    {
    if (NewMatrix[i, j]==6) 
        {count = count + 1}
  }
}

count


# Part II

library(MASS)
set.seed(12345)

z1=seq(0.1,10,0.1)
e=rnorm(100,0,3)
a=2
b=0.5
y=a+b*z1+e

plot(z1, y)

col1=rep(1, times=100)

X<-cbind(col1, z1)

est=ginv(t(X)%*%X)%*%t(X)%*%y

betahat1=est[2,1]
betahat1

#The true value of beta is 0.5, but the OLS estimate for this data is 0.578,
#which is close but not exact due to random variation (random error, epsilon).

#If this simulation were repeated many times, the average of the betahat's found
#will be very close to the actual value of beta.

# repeat the simulation 1000 times

betahats=c()

for(i in 1:1000){
  e=rnorm(100,0,3)
  y=a+b*z1+e
  est=ginv(t(X)%*%X)%*%t(X)%*%y
  betahats[i]=est[2,1]
}

#Find the average of the estimates of betahat from 1000 simulations
betahatmean=mean(betahats)
betahatmean

#The average of the estimated betahat values is very close to the known value for beta
#and much closer to the known value than the initial estimate from one simulation

#560 extension - imprecise measures of z

#Use original vector of observations for z to create imprecise observations
#using random noise

z.noisy = z1 + rnorm(100, 0, 1)

y2=a+b*z1+e

col1=rep(1, times=100)

X=cbind(col1, z.noisy)

est=ginv(t(X)%*%X)%*%t(X)%*%y2

betahat2=est[2,1]

#The OLS estimate of betahat when values for z are noisy is lower than
#for precise values of z, and lower than the actual value.

#Run 1000 simulations for various levels of "noise" in z


for(i in 1:1000){
  e=rnorm(100,0,3)
  y=a+b*z1+e
  est=ginv(t(X)%*%X)%*%t(X)%*%y
  betahats[i]=est[2,1]
}

#Find the average of the estimates of betahat from 1000 simulations
betahatmeans2=mean(betahats)
betahatmeans2

#The average of the OLS estimates of beta from 1000 simulations is lower than
#the actual value.  The difference is much greater than the difference of the
#previous simulated sampling distribution.

#The outputs of the model, y_i, are already assumed to be noisy.  The model
#assumes random error in the values of the output expressed by the epsilon
#term.



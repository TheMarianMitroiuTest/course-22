# install.packages("pocrm")
library("pocrm")

# Design Stage (points (a)--(f))

#Specify the possible orderings 
orders<-matrix(nrow= 6, # number of ordering
               ncol= 6) # number of combination/regimens) 
orders[1,]<-c(1, 2, 3, 4, 5 ,6)
orders[2,]<-c(1, 2, 3, 5, 4, 6)
orders[3,]<-c(1, 2, 4, 3, 5 ,6)
orders[4,]<-c(1, 2, 5, 4, 3, 6)
orders[5,]<-c(1, 2, 5, 3, 4, 6)
orders[6,]<-c(1, 2, 5, 4, 3, 6)

# Specifying skeleton
skeleton<-c(0.10, 0.21, 0.24, 0.30, 0.35, 0.40)


# Generating working model (ordering-specific skeletons)
alpha<-getwm(orders= orders,skeleton= skeleton)

# Prior probability of each ordering
prior.o<-c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

# Initial escalation scheme before any DLTs are observed
x0<-c(0.10, 0.21, 0.24, 0.30, 0.35, 0.40)

# Specify scenario
r<-c()
tox.range <- c(0.25, 0.35)

# Run simulations
pocrm.sim(r= r, # simulation scenario (as a vector)
          alpha= alpha, # matrix of working model
          prior.o= prior.o, # prior probability of each ordering (vector)
          x0= x0, # initial escalation scheme (vector) 
          stop= 30, # number of patients at one regimen to stop trial earlier (scalar)
          n= 30, #maximum number of patients (scalar)
          theta= 0.3, # target toxicity level  (scalar)
          nsim= 10, # number of simulations (scalar)
          tox.range= tox.range) # acceptable toxicity margin (scalar)


#pocrm.sim



######
######
######


# Implementation Stage (point (g))

# Insert combinations tried (1 entry = 1 patients)
combos<-c(1,1,1,2,2,2)
# Insert the corresonding DLT outcomes for each patients
y<-     c(0,0,0,1,0,0)
# Fit CRM
fit<-pocrm.imp(alpha=, # matrix of working model
               prior.o=, # prior probability of each ordering (vector)
               theta= , # target toxicity level  (scalar)
               y= , # DLT data (vector)
               combos=) # combination tried (vector)



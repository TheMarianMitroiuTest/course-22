## Practical with crmPack - template to start from
#install.packages('Hmisc')
## load the package:
library(crmPack)
library(Hmisc)

## define the dose grid: please insert the correct numbers
doseGrid <- seq(from= 40  , to= 200 , by= 10)

data <- Data(x=c(40, 40, 50, 50),
             y=c(0, 0, 1, 0),
             doseGrid = doseGrid)

data@ID
data@cohort
data@placebo
summary(data)
View(data)
#describe(data)
str(data)
plot(data)
#par(mfrow=c(2,2))


sigma0 <- 1.0278
sigma1 <- 1.65
rho <- 0.5
cov <- matrix(c(sigma0^2, rho * sigma0 * sigma1,
              rho * sigma0 * sigma1, sigma1^2),
              nrow = 2, ncol = 2)



model <- LogisticLogNormal(mean = c(-4.47, 0.0033),
                           cov = cov, refDose = 1)


summary(model)

mcmcoptions <- McmcOptions()

emptyData <- Data(doseGrid = doseGrid)

set.seed(12)

priorSamples <- mcmc(emptyData, model, mcmcoptions)


set.seed(92)
postSamples <- mcmc(data, model, mcmcoptions)
plot(priorSamples, 
     model, emptyData)

plot(postSamples, 
     model, emptyData)


increments <- IncrementsRelative(interval = 0,
                                 increments = 1)

maxDose(increments, data)


ncrm <- NextBestNCRM(target = c(0.16, 0.33),
                     overdose = c(0.33, 1),
                     maxOverdoseProb = 0.25)


doseRec <- nextBest(ncrm, 
                    doselimit = maxDose(increments, data),
                    postSamples, model, data)
doseRec$value

doseRec$plot

cohort <- CohortSizeConst(size = 3)

stop1 <- StoppingCohortsNearDose(nCohorts = 2, percentage = 0)

stop2 <- StoppingMinCohorts(nCohorts = 6)

stop3 <- StoppingMinPatients(nPatients = 30)

stopRule <- (stop1 & stop2) | stop3

stopTrial(stopRule, doseRec$value, postSamples, model, data)

design <- Design(model = model,
                 stopping = stopRule,
                 increments = increments,
                 nextBest= ncrm,
                 cohortSize = cohort, 
                 data = emptyData,
                 startingDose = 40)
                 
scenario <- function(dose, ED50, alpha1) {
  alpha0 <- qlogis(0.5) - alpha1 * log(ED50)
  model@prob(dose, alpha0 = alpha0, alpha1 = alpha1)
  }


curve(scenario(dose, ED50 = 100, alpha1 = 25), from = 40, to = 200,
      xname = "dose", ylab = "Scenario")

curve(scenario(dose, ED50 = 200, alpha1 = 25), from = 40, to = 200,
      xname = "dose", add = TRUE, col="red")

lines(fit(priorSamples, model, emptyData), col= "blue")

sims1 <- simulate(design, nsim = 10,
                  seed = 456, truth = scenario, args = list(ED50 = 100, alpha1 = 25),
                  mcmcoptions = mcmcoptions,
                  parallel = TRUE)


sims2 <- simulate(design, nsim = 10,
                  seed = 457, truth = scenario, args = list(ED50 = 200, alpha1 = 25),
                  mcmcoptions = mcmcoptions,
                  parallel = TRUE)


plot(sims1@data[[3]])
plot(sims1)


summary(sims1, truth = scenario,
        target = ncrm@target, ED50 = 100, alpha1 = 25)


PL <- 0.001
data2 <- DataDual(x = c(PL, 25, 25, 25, PL, 50, 50, 50,
                        PL, 100, 100, 100),
                  y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
                  w = c(0.02, 0.42, 0.59, 0.45, 0.03, 0.7, 0.6, 0.52,
                        0.01, 0.71, 0.54, 0.45),
                  cohort = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
                  doseGrid=c(PL, seq(25, 300, 25)), ID= 1:12, placebo = TRUE)

plot(data2)

emptydata2 <- DataDual(doseGrid = data@doseGrid, placebo = TRUE)


DLTmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8),
                              DLEweights = c(3, 3), DLEdose = c(25, 300),
                              data = emptydata2)

Effmodel <- Effloglog(Eff = c(1.223, 2.513),
                      Effdose = c(25, 300),
                      nu = c(a = 1, b = 0.025),
                      data = emptydata2, c = 2)


newDLTmodel <- update(object = DLTmodel, data = data2)
newEffmodel <- update(object = Effmodel, data = data2)

GainNextBest <- NextBestMaxGain(DLEDuringTrialtarget = 0.35,
                                DLEEndOfTrialtarget = 0.3)



myIncrements <- IncrementsRelative(intervals = c(0, 125, 200),
                                   increments = c(1, 0.75, 0.5))


nextMaxDose <- maxDose(myIncrements, data2)


doseRecGain <- nextBest()


## get the model and prior for the parameters:
## please insert the correct numbers into the c(,) notation 
## - have a look at the help page for Quantiles2LogisticNormal
set.seed(92)
## let this next command run while you edit further below - 
## it may take a few minutes...
res <- Quantiles2LogisticNormal(dosegrid=c( , ),
                                refDose=1,
                                lower=c( , ),
                                median=c( , ),
                                upper=c( , ),
                                logNormal=TRUE,
                                control=list(max.time=10))

## look at the result:
res

## get the model
model <- res$model

## generate samples from the prior and plot it:
emptyData <- Data(doseGrid=doseGrid)
mcmcOptions <- McmcOptions()
set.seed(12)
## look at the example1.R code to complete the following two lines:
priorSamples <- 
plot(  )
## how does it look? ok?

## NCRM rule: please complete with the correct numbers
ncrm <- NextBestNCRM(target = c( , ),
                     overdose = c( , ),
                     maxOverdoseProb = )

## cohort size: please insert the correct vectors
## have a look at the help pages for CohortSizeDLT
## and CohortSizeRange
cohortSize1 <- CohortSizeDLT(DLTintervals= c( , ),
                             cohortSize= c( , ))
cohortSize2 <- CohortSizeRange(intervals= c( , ),
                               cohortSize= c( , ))
cohortSize <- maxSize(cohortSize1, cohortSize2)

## stopping rule construction:
## please complete with the right numbers
stop1 <- StoppingPatientsNearDose(nPatients =  , percentage =  )
stop2 <- StoppingMinPatients(nPatients =  )
stop3 <- StoppingMinPatients(nPatients =  )
stopRule <- (stop1 & stop2) | stop3

## specify maximum increments
## please complete with the right vectors
increments <- IncrementsRelative(intervals= c( , ),
                                 increments= c( , ))

## bundle everything together in the Design object:
## please complete
design <- Design(model= ,
                 stopping= ,
                 increments= ,
                 nextBest= ,
                 cohortSize= ,
                 data= ,
                 startingDose= )

## evaluate single trial behavior first!!
ex <- examine(design, 
              mcmcOptions=
                  McmcOptions(burnin=20000,
                             step=3,
                             samples=50000))
ex  
## how many cohorts at least until max. dose is reached?

## define scenarios:

## first look at the prior means 
## (for the intercept alpha0 and for the log slope log(alpha1)): 
model@mean
exp(model@mean[2])

## this one shall be similar to the prior assumptions:
## complete with the right numbers 
## taken from the two previous lines
scen1 <- function(dose)
{
    model@prob(dose, alpha0= , alpha1= )
}

## this one shall be more toxic:
## choose higher numbers here for the intercept and slope
## prior means, in order to make it more toxic
scen2 <- function(dose)
{
  model@prob(dose, alpha0= , alpha1= )
}

## and this one shall be less toxic:
## choose lower numbers here for the intercept and slope
## prior means, in orderto make it less toxic
scen3 <- function(dose)
{
  model@prob(dose, alpha0= , alpha1= )
}

## try it out:
curve(scen1(dose),
      from=1, to=150, xname="dose", ylim=c(0, 1))
curve(scen2(dose),
      from=1, to=150, xname="dose", 
      add=TRUE, col="red")
curve(scen3(dose),
      from=1, to=150, xname="dose",
      add=TRUE, col="blue")
## check if intended ordering of scenarios is kept

## add the fit from the empty data => prior model:
lines(fit(priorSamples,
          model,
          emptyData),
      col="green")
## does the green line stay close to the black line?

## run simulations:
## please complete
sims1 <- simulate(design, 
                  nsim= ,
                  seed= ,
                  truth= ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims2 <- simulate(design, 
                  nsim= ,
                  seed= ,
                  truth= ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims3 <- simulate(design, 
                  nsim= ,
                  seed= ,
                  truth= ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

## numerical summaries
summary(sims1,
        scen1,
        target=ncrm@target)
summary(sims2,
        scen2,
        target=ncrm@target)
summary(sims3,
        scen3,
        target=ncrm@target)

## compare with 3+3 design:
## not clear which doses to pick -
## all of them will be very slow ->
## take 1, 10 (starting dose), the maximum 150,
## and four more in between respecting the max. increment
## please insert the doseGrid vector accordingly:
emptydata2 <- Data(doseGrid = c( ... ))
three <- RuleDesign(nextBest=NextBestThreePlusThree(),
                    data=emptydata2,
                    cohortSize=CohortSizeConst(size=3),
                    startingDose=10)
## need more code because starting dose shall not be 
## equal to the lowest dose 
## (may be improved in a next version of crmPack...)

## single trial behavior
examine(three)

## simulations:
simThree1 <-
  simulate(three, 
         nsim=100,
         seed=20,
         truth=scen1)
simThree2 <-
  simulate(three, 
           nsim=100,
           seed=200,
           truth=scen2)
simThree3 <-
  simulate(three, 
           nsim=100,
           seed=201,
           truth=scen3)

## summary statistics
## please comment
summary(simThree1,
        scen1,
        target=ncrm@target)

summary(simThree2,
        scen2,
        target=ncrm@target)

summary(simThree3,
        scen3,
        target=ncrm@target)


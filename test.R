library(data.table)
source('parVals.R')
source('simObs.R')
source('getTruth.R')
source('sine_analyze.R')

price = 15
daySupp = 30
followUp = 50
truth <- getTruth.sine(N=1000,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followUp)

data <- simObs.sine(N=1000,FOLLOWUP=followUp)
analyze(DATA=data,BAND=365,NUMSIM=4000,
        DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followUp)

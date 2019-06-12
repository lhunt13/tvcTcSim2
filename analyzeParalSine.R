# analyze data on cluster
library(getopt)
library(data.table)
library(splines)
source('parVals.R')
source('simObs.R')
source('sine_analyze.R')
source('analyze_noU.R')
source('sine_analyze_noG.R')
source('analyze_noUG.R')

# set seed for run SGE_TASK_ID
# grab value of SGE_TASK_ID 
# (which is given by "-t" in the corresponding bash script)
args <- commandArgs(trailingOnly = TRUE)
boot.index <- as.numeric(args[1])

# pass sensitivity parameter values
n        <- as.numeric(args[2])
followup <- as.numeric(args[3])
band     <- as.numeric(args[4])
numsim   <- as.numeric(args[5])
daySupp  <- as.numeric(args[6])
price    <- as.numeric(args[7])
r        <- as.numeric(args[8])
tval     <- as.numeric(args[9])

# set initial seed for reproducibility 
set.seed(123)
boot.seed <- sample(1e6, size = tval, replace = F)[boot.index]
set.seed(boot.seed)

# simulate data
data <- simObs.sine(N=n,FOLLOWUP=followup)

# perform analysis
both <- analyze(DATA=data,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
noU  <- analyze_noU(DATA=data,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
noG  <- analyze_noG(DATA=data,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
noUG <- analyze_noUG(DATA=data,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)

# perform bootstrap
ci_both <- bootstrap(DATA=data,R=r,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
ci_noU  <- bootstrap_noU(DATA=data,R=r,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
ci_noG  <- bootstrap_noG(DATA=data,R=r,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
ci_noUG <- bootstrap_noUG(DATA=data,R=r,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)

results <- c(both$rmdiff,ci_both,
             noU$rmdiff,ci_noU,
             noG$rmdiff,ci_noG,
             noUG$rmdiff,ci_noUG)

# store results
# save file in "truth" directory with file name "run-<boot.index>.rds"
results.file <- file.path("results_sine", paste0("run-", boot.index, ".rds"))
saveRDS(results, results.file)
# quit R and don't save workspace
quit('no')

# analyze data on cluster
library(getopt)
library(data.table)
source('parVals.R')
source('simObs.R')
source('analyze.R')
source('analyze_noU.R')
source('analyze_noG.R')
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
data <- simObs(N=n,FOLLOWUP=followup)

# perform analysis
rmdiff_UG <- analyze(DATA=data,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
rmdiff_G  <- analyze_noU(DATA=data,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
rmdiff_U  <- analyze_noG(DATA=data,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
rmdiff    <- analyze_noUG(DATA=data,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)

# perform bootstrap
ci_UG <- bootstrap(DATA=data,R=r,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
ci_G  <- bootstrap_noU(DATA=data,R=r,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
ci_U  <- bootstrap_noG(DATA=data,R=r,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
ci    <- bootstrap_noUG(DATA=data,R=r,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)

results <- c(rmdiff_UG,ci_UG,rmdiff_G,ci_G,rmdiff_U,ci_U,rmdiff,ci)

# store results
# save file in "truth" directory with file name "run-<boot.index>.rds"
results.file <- file.path("results", paste0("run-", boot.index, ".rds"))
saveRDS(results, results.file)
# quit R and don't save workspace
quit('no')

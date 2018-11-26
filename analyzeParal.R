# analyze data on cluster
library(getopt)
library(data.table)
source('parVals.R')
source('simObs.R')
source('analyze.R')

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
rmdiff <- analyze(DATA=data,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)

# perform bootstrap
ci <- bootstrap(DATA=data,R=r,BAND=band,NUMSIM=numsim,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)

results <- c(rmdiff,ci)

# store results
# save file in "truth" directory with file name "run-<boot.index>.rds"
results.file <- file.path("results", paste0("run-", boot.index, ".rds"))
saveRDS(results, results.file)
# quit R and don't save workspace
quit('no')

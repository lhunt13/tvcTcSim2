# Parallelize the procedure of getting the truth.
# Simulate datasets of size n, tval times to get
# an effective sample size of n*tval from which to 
# estimate the truth.
library(getopt)
library(data.table)
source('parVals.R')
source('simObs.R')
source('getTruth.R')

# set seed for run SGE_TASK_ID
# grab value of SGE_TASK_ID 
# (which is given by "-t" in the corresponding bash script)
args <- commandArgs(trailingOnly = TRUE)
boot.index <- as.numeric(args[1])

# pass sensitivity parameter values
n        <- as.numeric(args[2])
daySupp  <- as.numeric(args[3])
price    <- as.numeric(args[4])
followup <- as.numeric(args[5])
tval     <- as.numeric(args[6])

# set initial seed for reproducibility 
set.seed(123)
boot.seed <- sample(1e6, size = tval, replace = F)[boot.index]
set.seed(boot.seed)
truth   <- getTruth(N=n,DAYSUPP=daySupp,PRICE=price,FOLLOWUP=followup)
rmdiff  <- truth$rmdiff
brand   <- truth$brand
generic <- truth$generic

# store results
# save file in "truth" directory with file name "run-<boot.index>.rds"
rmdiff.file  <- file.path("rmdiff",  paste0("run-", boot.index, ".rds"))
brand.file   <- file.path("brand",   paste0("run-", boot.index, ".rds"))
generic.file <- file.path("generic", paste0("run-", boot.index, ".rds"))

saveRDS(rmdiff, rmdiff.file)
saveRDS(brand, brand.file)
saveRDS(generic, generic.file)

# quit R and don't save workspace
quit('no')

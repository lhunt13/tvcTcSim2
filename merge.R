# rscript to merge all .rds files to a chosen output 
# in a given folder with a given pattern
main <- function(){
  args <- commandArgs(trailingOnly = TRUE)
  folder <- args[1]
  pattern <- args[2]
  output <- args[3]
  
  files <- list.files(path = folder,
                      pattern = pattern, 
                      full.names = TRUE)
  
  numfiles <- length(files)

  merged <- numeric(0)
  for(i in 1:numfiles){
    if(file.size(files[i]) > 1){merged[i] <- readRDS(files[i])}
    else{merged[i] <- NA}
  }
  
  #merged <- do.call('rbind', 
  #                  lapply(list.files(path = folder,
  #                                    pattern = pattern, 
  #                                    full.names = TRUE), readRDS))
  
  saveRDS(merged,paste0(folder,output))
  
}

main()
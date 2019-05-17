# get truth
adherant_patient <- function(id,L1,U,Era,DAYSUPP,PRICE,FOLLOWUP){
  
  L <- V <- D <- C <- cc <- S <- numeric(0)
  
  # day 1
  L[1]  <- L1
  V[1]  <- "adhere"
  cc[1] <- PRICE
  S[1]  <- simS(V=V[1],cc=cc[1],L=L[1],U=U,t=1,era=Era)
  
  t <- 2
  repeat{
    L[t]  <- simL(V_lag=V[t-1],cc_lag=cc[t-1],L_lag=L[t-1],era=Era)
    V[t]  <- "adhere"
    cc[t] <- ceiling(t/DAYSUPP)*PRICE
    S[t]  <- simS(V=V[t],cc=cc[t],L=L[t],U=U,t=t,era=Era)
    
    if(S[t]==1|t >= FOLLOWUP){break}
    else{t <- t+1}
    
  }
  return(data.frame(id=id,day=1:t,S=S))
}

getTruth <- function(N,DAYSUPP,PRICE,FOLLOWUP){
  brand   <- data.frame(id=NA,day=NA,S=NA)
  generic <- data.frame(id=NA,day=NA,S=NA)
  
  # sample baseline variable L from empirical distributions
  L1b <- rbinom(N, 1, pL1b)
  L1g <- rbinom(N, 1, pL1g)
  
  # create adherant patients timeline
  for(i in 1:N){
    brand <- rbind(brand,adherant_patient(id=i,L1=L1b[i],U=u_star,Era=0,
                                            DAYSUPP=DAYSUPP,PRICE=PRICE,FOLLOWUP=FOLLOWUP))
    generic <- rbind(generic,adherant_patient(id=i,L1=L1g[i],U=u_star,Era=1,
                                              DAYSUPP=DAYSUPP,PRICE=PRICE,FOLLOWUP=FOLLOWUP))
  }

  setDT(brand)
  setDT(generic)
  brand <- brand[-1]
  generic <- generic[-1]
  
  # sum_t {P(S(b) > t) - P(S(g) > t)} t going from 0 to 180
  # P(S(z) > 0) = 1, always
  brand_curve   <- c(brand[,.(.N/N),.(day)]$V1,   brand[day==FOLLOWUP & S==0,.(.N/N)]$V1)
  generic_curve <- c(generic[,.(.N/N),.(day)]$V1, generic[day==FOLLOWUP & S==0,.(.N/N)]$V1)
  rmdiff <- sum(brand_curve - generic_curve)
  
  return(list(rmdiff))
}


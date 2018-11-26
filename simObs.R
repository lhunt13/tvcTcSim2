# simulate observed data
simL <- function(V_lag,cc_lag,L_lag,era){
  vars <- c(1,V_lag=="none",V_lag=="other",cc_lag,L_lag, 
            era*c(1,V_lag=="none",V_lag=="other",cc_lag,L_lag))
  return(rbinom(1,1,prob = expit(vars %*% l)))
}

simV <- function(t,D,V,L,era){
  if(t <= sum(D,na.rm=T)){
    return(V)
  }
  else{
    vars <- c(1,L, era*c(1,L))
    phik <- exp(c(v %*% vars))
    pV1 <- 1/(1 + sum(phik))
    pV <- pV1*phik
    return(sample(c("adhere","none","other"),size=1,prob=c(pV1,pV)))
  }
}

simD <- function(t,D,V,era){
  if(t <= sum(D,na.rm=T)){
    return(NA)
  }
  else if(V=="none"){return(1)}
  else{
    vars <- c(1,V=="other",era*c(1,V=="other"))
    phik <- exp(c(d %*% vars))
    pDk1 <- 1/(1 + sum(phik))
    pDk <- pDk1*phik
    return(sample(c(1,15,30,60,90),size=1,prob=c(pDk1,pDk)))
  }
}

simC <- function(t,D,V,era){
  if(is.na(D[t])){
    return(0)
  }
  else if(V=="none"){return(0)}
  else{
    vars <- c(1,D[t]==c(15,30,60,90),V=="other",era*c(1,D[t]==c(15,30,60,90),V=="other"))
    out <- rnorm(1, mean=sum(cpar*vars), sd=sum(c(1,era)*sigma))
    if(is.na(out)){
      print(vars)
    }
    return(out)
  }
}

simS <- function(V,cc,L,U,t,era){
  vars <- c(1,V=="none",V=="other",cc,L,U,t,
            era*c(1,V=="none",V=="other",cc,L,U,t))
  return(rbinom(1,1,prob = expit(vars %*% s)))
}

create_patient <- function(id,L1,U,FOLLOWUP){
  
  Era <- (U >= u_star)
  L <- V <- D <- C <- cc <- S <- numeric(0)
  
  # day 1
  L[1]  <- L1
  V[1]  <- "adhere"
  D[1]  <- simD(t=1,D=0,V=V[1],era=Era)
  C[1]  <- simC(t=1,D=D[1],V=V[1],era=Era)
  cc[1] <- (exp(2*C) - 1)/(2*exp(C))
  S[1]  <- simS(V=V[1],cc=cc[1],L=L[1],U=U,t=1,era=Era)
  
  t <- 2
  repeat{
    L[t]  <- simL(V_lag=V[t-1],cc_lag=cc[t-1],L_lag=L[t-1],era=Era)
    V[t]  <- simV(t=t,D=D,V=V[t-1],L=L[t],era=Era)
    D[t]  <- simD(t=t,D=D[1:(t-1)],V=V[t],era=Era)
    C[t]  <- simC(t=t,D=D[1:(t-1)],V=V[t],era=Era)
    
    cc[t] <- sum((exp(2*C) - 1)/(2*exp(C)))
    S[t]  <- simS(V=V[t],cc=cc[t],L=L[t],U=U,t=t,era=Era)

    if(S[t]==1|t >= FOLLOWUP){break}
    else{t <- t+1}

  }
  return(data.frame(id=id,day=1:t,U=U,V=V,D=D,C=C,cc=cc,L=L,S=S))
}

simObs <- function(N,FOLLOWUP){
  data <- data.frame(id=NA, day=NA, U=NA, V=NA, D=NA, C=NA, cc=NA, L=NA, S=NA)
  
  # sample baseline variables (U, L) from empirical distributions
  U <- predict(ssU, runif(N))$y
  L1 <- rbinom(N, 1, pL1)
  
  # create each patients timeline
  for(i in 1:N){
    data <- rbind(data,create_patient(id=i,L1=L1[i],U=U[i],FOLLOWUP=FOLLOWUP))
  }
  
  setDT(data)
  lagcols <- c("V","cc","L")
  data[,(paste0(lagcols,"_lag")) := shift(.SD),id,.SDcols=lagcols]
  
  return(data[-1])
}



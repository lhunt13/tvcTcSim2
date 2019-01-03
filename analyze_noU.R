# analyze data without adjusting for U

# fit the models for the nuisance parameters
fit_models_noU <- function(DAT){
  L <- glm(L ~ I(V_lag=="none") + I(V_lag=="other") + cc_lag + L_lag,
           data=DAT, family=binomial(logit))
  
  S <- glm(S ~ I(V=="none") + I(V=="other") + cc + L + day,
           data=DAT, family=binomial(logit))
  return(list(L=L,S=S))
}

# compute the g-computation integral numerically
g_comp_noU <- function(BSLN,MODELS,DAYSUPP,PRICE,FOLLOWUP){
  n <- BSLN[,.N]
  
  L <- matrix(NA,nrow=n,ncol=FOLLOWUP)
  S <- matrix(NA,nrow=n,ncol=FOLLOWUP)
  
  L[,1] <- BSLN[,L]
  
  # baseline survival
  X <- cbind(rep(1,n),rep(0,n),rep(0,n),rep(PRICE,n),L[,1],rep(1,n))
  S[,1] <- rbinom(n,1,expit(X %*% MODELS[["S"]]$coefficients))
  
  for(t in 2:FOLLOWUP){
    ###### L
    X <- cbind(rep(1,n),rep(0,n),rep(0,n),rep(ceiling((t-1)/DAYSUPP)*PRICE,n),L[,t-1])
    L[,t] <- rbinom(n,1,expit(X %*% MODELS[["L"]]$coefficients))
    
    ###### S
    X <- cbind(rep(1,n),rep(0,n),rep(0,n),rep(ceiling(t/DAYSUPP)*PRICE,n),
               L[,t],rep(t,n))
    S[,t] <- ifelse(S[,t-1]==1,1,
                    rbinom(n,1,expit(X %*% MODELS[["S"]]$coefficients)))
  }
  
  # the vector 1-colMeans(S) starts with P(S > 1). Need to start with P(S > 0) = 1
  survival <- c(1,1 - colMeans(S))
  return(survival)
}

# compute the restricted mean difference
analyze_noU <- function(DATA,NUMSIM,DAYSUPP,PRICE,FOLLOWUP){
  # fit models
  b_mods <- fit_models_noU(DAT = DATA[U < u_star])
  
  g_mods <- fit_models_noU(DAT = DATA[U >= u_star])
  
  # perform g computation
  bsln <- DATA[day==1][sample(.N, NUMSIM, replace=TRUE)] 
  
  brand   <- g_comp_noU(bsln[U < u_star],b_mods, DAYSUPP=DAYSUPP, PRICE=PRICE, FOLLOWUP=FOLLOWUP)
  generic <- g_comp_noU(bsln[U >= u_star],g_mods, DAYSUPP=DAYSUPP, PRICE=PRICE, FOLLOWUP=FOLLOWUP)
  
  # get restricted mean difference
  rmdiff <- sum(brand - generic)
  
  return(rmdiff)
}

# compute a 95% CI using bootstrap
bootstrap_noU <- function(DATA,R,NUMSIM,DAYSUPP,PRICE,FOLLOWUP){
  boots <- numeric(0)
  setkey(DATA,id)
  for(r in 1:R){
    ids_resampled <- sample(unique(DATA$id), length(unique(DATA$id)), replace = TRUE)
    data_resampled <- DATA[J(ids_resampled), allow.cartesian=TRUE]
    boots[r] <- analyze_noU(DATA = data_resampled,NUMSIM,DAYSUPP,PRICE,FOLLOWUP)
  }
  return(quantile(boots,probs=c(.025,.975)))
}
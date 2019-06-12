# analyze data without adjusting for time varying confounders

# fit the models for the nuisance parameters
fit_models_noG <- function(DAT,SPL){
  S <- glm(S ~ I(A==1) + cc + SPL[,1] + SPL[,2] + SPL[,3] + SPL[,4] + SPL[,5] + day,
           data=DAT, family=binomial(logit))
  return(list(S=S))
}

# compute the g-computation integral numerically
g_comp_noG <- function(BSLN,MODELS,DAYSUPP,PRICE,FOLLOWUP,SPL_STAR){
  n <- BSLN[,.N]
  
  S <- matrix(NA,nrow=n,ncol=FOLLOWUP)
  
  # baseline survival
  X <- cbind(rep(1,n),rep(1,n),rep(PRICE,n),
             rep(SPL_STAR[1],n),
             rep(SPL_STAR[2],n),
             rep(SPL_STAR[3],n),
             rep(SPL_STAR[4],n),
             rep(SPL_STAR[5],n),rep(1,n))
  S[,1] <- rbinom(n,1,expit(X %*% MODELS[["S"]]$coefficients))
  
  for(t in 2:FOLLOWUP){
    ###### S
    X <- cbind(rep(1,n),rep(1,n),rep(ceiling(t/DAYSUPP)*PRICE,n),
               rep(SPL_STAR[1],n),
               rep(SPL_STAR[2],n),
               rep(SPL_STAR[3],n),
               rep(SPL_STAR[4],n),
               rep(SPL_STAR[5],n),rep(t,n))
    S[,t] <- ifelse(S[,t-1]==1,1,
                    rbinom(n,1,expit(X %*% MODELS[["S"]]$coefficients)))
  }
  
  # the vector 1-colMeans(S) starts with P(S > 1). Need to start with P(S > 0) = 1
  survival <- c(1,1 - colMeans(S))
  return(survival)
}

# compute the restricted mean difference
analyze_noG <- function(DATA,BAND,NUMSIM,DAYSUPP,PRICE,FOLLOWUP){
  # get natural cubic splines
  b_spl <- ns(DATA[U < u_star]$U, 5)
  g_spl <- ns(DATA[U >= u_star]$U, 5)
  
  b_spl_star <- predict(b_spl,u_star)
  g_spl_star <- predict(g_spl,u_star)
  
  # fit models
  b_mods <- fit_models_noG(DAT = DATA[U < u_star], SPL = b_spl)
  
  g_mods <- fit_models_noG(DAT = DATA[U >= u_star], SPL = g_spl)
  
  # perform g computation
  bsln <- DATA[day==1][sample(.N, NUMSIM, replace=TRUE,
                              prob = dnorm(DATA[day==1]$U, mean=u_star, sd=BAND))] 
  
  brand   <- g_comp_noG(bsln[U < u_star],b_mods, DAYSUPP=DAYSUPP, PRICE=PRICE, FOLLOWUP=FOLLOWUP,SPL_STAR=b_spl_star)
  generic <- g_comp_noG(bsln[U >= u_star],g_mods, DAYSUPP=DAYSUPP, PRICE=PRICE, FOLLOWUP=FOLLOWUP,SPL_STAR=g_spl_star)
  
  # get restricted mean difference
  rmdiff <- sum(brand - generic)
  
  return(list(rmdiff=rmdiff, 
              brand=brand, 
              generic=generic))
}

# compute a 95% CI using bootstrap
bootstrap_noG <- function(DATA,R,BAND,NUMSIM,DAYSUPP,PRICE,FOLLOWUP){
  boots <- numeric(0)
  setkey(DATA,id)
  for(r in 1:R){
    ids_resampled <- sample(unique(DATA$id), length(unique(DATA$id)), replace = TRUE)
    data_resampled <- DATA[J(ids_resampled), allow.cartesian=TRUE]
    boots[r] <- analyze_noG(DATA = data_resampled,BAND,NUMSIM,DAYSUPP,PRICE,FOLLOWUP)$rmdiff
  }
  return(quantile(boots,probs=c(.025,.975)))
}
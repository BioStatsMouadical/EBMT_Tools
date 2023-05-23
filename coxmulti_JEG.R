
### extraction model de Cox

extractCox <- function(cox_model,j,result, pvaldig = 2){

  if(is.null(cox_model)) return(NA)
  cox_model_bu <- cox_model
  cox_model <- summary(cox_model)
  
  # First, get and store the levels and the reference
  
  # Remove frailty terms
  frailty <- any(grepl("frailty", rownames(cox_model$coefficients)))
  vars <- rep(names(cox_model_bu$assign) , lengths(cox_model_bu$assign))
  a <- names(cox_model_bu$assign)
  b <- lapply(cox_model_bu$xlevels, "[", -1)
  vars <- vars[!grepl("strata", vars)]
  a <- a[!grepl("strata", a)]
  b <- b[!grepl("strata", b)]    # Get rid of any strata term!
  
  ## vars <- vars[!grepl("frailty", vars)]
  ## a <- a[!grepl("frailty", a)]
  ## b <- b[!grepl("frailty", b)]    # Get rid of any frailty term!
  
  lvls <- unlist(sapply(a, function(z) paste0(z, "-", b[[z]])) , use.names = FALSE)
  c <- lapply(cox_model_bu$xlevels, "[", 1)
  refs <- sapply(vars, function(t) c[[t]])  ;  refs[lengths(refs) == 0] <- NA
  refs <- unlist(refs, use.names = FALSE)

  nCoeff <- nrow(cox_model$coefficients)
  beta <- round(cox_model$coef[ , 1], 2);    # coeficient beta (multiple)
  
  nCoeff ; beta
  
  if(frailty)  HR <- round(exp(cox_model$coef[ , 1]) , 2)
  if(!frailty) HR <- round(cox_model$coef[ , 2], 2);     # exp(beta)
  
  HR.confint.lower <- round(cox_model$conf.int[,"lower .95"], 2)
  HR.confint.upper <- round(cox_model$conf.int[,"upper .95"], 2)
  
  HR_txt <- paste0(HR, " (",HR.confint.lower, "-", HR.confint.upper, ")")
  
  if(frailty) beta_pr <- pvalformat(signif(cox_model$coef[ , "p"], digits=pvaldig))
  if(!frailty) beta_pr <- pvalformat(signif(cox_model$coef[ , 5], digits=pvaldig)) # This is Pr(>|z|) after z-scoring each coeffient
  
  # Do this only if the wald test result exists....
  if(length(cox_model$wald) > 0){
    wald.test <- pvalformat(signif(cox_model$wald["test"], digits=pvaldig))     # # Same for single coefficient
    p.value <- pvalformat(signif(cox_model$wald["pvalue"], digits=pvaldig))    # Same for single coefficient
    test <- wald.test
  }

  # Do this only if the log test result exists.... (when using frailty term)
  if(length(cox_model$logtest) > 0){
    log.test <- pvalformat(signif(cox_model$logtest["test"], digits=pvaldig))     # # Same for single coefficient
    p.value <- pvalformat(signif(cox_model$logtest["pvalue"], digits=pvaldig))     # Same for single coefficient
    test <- log.test
  }
  
  n_total <- cox_model$n
  n_event <- cox_model$nevent
  n_nonevent <- cox_model$n-cox_model$nevent
  n_missing <- length(cox_model$na.action)
  
  if(j==1) {
  
  res <- cbind(vars, lvls, refs,HR_txt ,beta_pr)
  endpoint<-c(" "," "," ",rep(my.Event[j],2))
  res<-rbind(endpoint,res)
  res<-data.frame(res)
  
  colnames(res) <- c("variable","level","reference","HR (95% CI)" ,"p value")
  res$level <- gsub(".*-", "", res$level)
  
  }
  
  if(j>1) {
  
  res <- cbind(HR_txt ,beta_pr)
  endpoint<-c(rep(my.Event[j],2))
  res<-rbind(endpoint,res)
  res<-data.frame(res)
  
  colnames(res) <- c("HR (95% CI)","p value")
  res<-try(cbind(result,res))
  
  }
  return(res)
}



pvalformat<-function(pval){
  pval<-pval
for(i in 1:length(pval)){
    if(pval[i] > 0.05){
      pval[i]<-round(pval[i],2)
    }else{if(pval[i] >0.045){
        pval[i]<-round(pval[i],3)
    }else{if(pval[i] >0.01){
        pval[i]<-round(pval[i],2)
    }else{if(pval[i] >0.001){
        pval[i]<-round(pval[i],3)
    }else{pval[i]<-0.001}}}}}
  
    return(pval)
    }

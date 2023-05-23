library(foreign)
library(cmprsk)

univ_outcomes<-function(d,covars.uni,t=c(2,3),tpnn=60,outcome=NULL,savedCSV=FALSE){
  res<-NULL
  
  ### Ne prendre que les variables en facteurs ###
  for(r in 1:length(covars.uni)){
    if(is.factor(d[,which(names(d)==covars.uni[r])])==TRUE) {
      nomvar=covars.uni[r]
      
      ### Choix des outcomes à étudier
      nomev0<-  c("STATE","STATEPFS","STATEPFS2","CRELAPSE","CTOXI","CSECOND","CAGVH2" ,"CAGVH3" ,"GRFS"    ,"CCGVH"  ,"CCGVHEXT"  ,"STATEGVHD", "STATECONCEPT","CPOLY100","CRESOL")
      nomtime0<-c("SURVY","PFSY"    ,"PFS2"     ,"PFSY"    ,"PFSY" ,"BMTSECOND","BMTAGV2","BMTAGV3","BMTGRFSY","BMTCGVH","BMTCGVHEXT","SURVY","BMTCONCEPT","DPOLY100","DELAIRESOL")
      if (!is.null((outcome))){nomev<-nomev0[which(nomev0 %in% outcome)] ; nomtime<-nomtime0[which(nomev0 %in% outcome)]
      }else{nomev<-nomev0 ; nomtime<-nomtime0}
      
      ### Analyse pour chaque temps choisi
      for(tt in t){rest<-rap16(repe,d,nomvar,nomtime,nomev,tt,r,tpnn)
        ### Mise en forme des résultats et odification du format de pvalue
        for(p in 2:dim(rest)[2]){
          rest[length(levels(d[,nomvar]))+2,p]<-ifelse(as.numeric(as.character(rest[length(levels(d[,nomvar]))+2,p]))<0.0001,
                                                       "<0.0001",rest[length(levels(d[,nomvar]))+2,p])}
        res<-try(rbind(res,cbind(Variable=c(nomvar,rep("",length(levels(d[,nomvar]))+1)),
                                 "Time (y)"=c(rep(tt,c(length(levels(d[,nomvar])))),rep("",2)),
                                 rest[1:(length(levels(d[,nomvar]))+2),])))}}
    }
  return(res) ; if(savedCSV==TRUE){write.csv2(res, file ="univ.csv", row.names=F,na ="")}}

rap16 <-function(repe,d,nomvar,nomtime,nomev,t,r,tpnn) {
  fmethod="log-log" ; k=0.95 ;z <- qnorm(1-(1-0.95)/2) ; h=1 ;nr=0 ; nrcol=0 ;  colname<-c("Levels")
  for(i in 1:length(nomvar)){
    if (is.factor(d[,which(names(d)==nomvar[i])])) {nr=nr+nlevels(d[,which(names(d)==nomvar[i])])} else {nr=nr+1}}
  
  for(e in 1:length(nomev)){if (nomev[e] %in% names(d) == TRUE)  {nrcol=nrcol+1}}
  
  ### Matrice de résultats
  result <- data.frame(matrix(data=NA, nrow = nr+length(nomvar)+2, ncol = nrcol+1, byrow = FALSE))
  for (e in 1: length(nomev)) {colname<-c(colname,nomev[e])}
  colnames(result)=colname
  
  ##########################
  ### Analyses #############
  ##########################
  
  for(i in 1:length(nomvar)){
    for (e in 1: length(nomev)) {
      if (nomev[e]=="CRELAPSE") {
        fitri=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitri.t<-timepoints(fitri,t)
      }
      if (nomev[e]=="CSECOND") {
        fitsec=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitsec.t<-timepoints(fitsec,t*365.25)
      }
      if (nomev[e]=="CTOXI") {
        fitnrm=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitnrm.t<-timepoints(fitnrm,t)
      }  
      if (nomev[e]=="STATEGVHD") {
        fitgvhd=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitgvhd.t<-timepoints(fitgvhd,t)
      }  
      if (nomev[e]=="STATECONCEPT") {
        fitconc=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        print(fitconc)
        fitconc.t<-timepoints(fitconc,t)
        print(fitconc.t)
      }  
      if (nomev[e]=="STATEPFS") {
        pfs<-survfit(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d,conf.type=fmethod,conf.int=k)
        fitpfs<-summary(pfs,t)
        testpfs <- survdiff(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d)
        ptestpfs <- pchisq(testpfs$chisq,df=nrow(table(d[,which(names(d)==nomvar[i])]))-1,lower.tail=FALSE)
      }
      if (nomev[e]=="STATEPFS2") {
        pfs2<-survfit(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d,conf.type=fmethod,conf.int=k)
        fitpfs2<-summary(pfs2,t*365.25)
        testpfs2 <- survdiff(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d)
        ptestpfs2 <- pchisq(testpfs2$chisq,df=nrow(table(d[,which(names(d)==nomvar[i])]))-1,lower.tail=FALSE)
      }      
      if (nomev[e]=="STATE") {
        os<-survfit(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d,conf.type=fmethod,conf.int=k)
        fitos<-summary(os,t)
        testos <- survdiff(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d)
        ptestos <- pchisq(testos$chisq,df=nrow(table(d[,which(names(d)==nomvar[i])]))-1,lower.tail=FALSE)
      }
      if (nomev[e]=="GRFS") {
        grf<-survfit(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d,conf.type=fmethod,conf.int=k)
        fitgrf<-summary(grf,t)
        testgrf <- survdiff(Surv(d[,which(names(d)==nomtime[e])],d[,which(names(d)==nomev[e])])~ d[,which(names(d)==nomvar[i])], data=d)
        ptestgrf <- pchisq(testgrf$chisq,df=nrow(table(d[,which(names(d)==nomvar[i])]))-1,lower.tail=FALSE)
      }
      if (nomev[e]=="CAGVH2") {
        fitagv2=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitagv2.t<-timepoints(fitagv2,100)
      }
      if (nomev[e]=="CAGVH3") {
        fitagv3=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitagv3.t<-timepoints(fitagv3,100)
      }
      if (nomev[e]=="CCGVH") {
        fitcgv=cuminc(d[,which(names(d)==nomtime[e])]/365.25, d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitcgv.t<-timepoints(fitcgv,t)
      }
      if (nomev[e]=="CCGVHEXT") {
        fitcgvex=cuminc(d[,which(names(d)==nomtime[e])]/365.25, d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitcgvex.t<-timepoints(fitcgvex,t)
      }
      if (nomev[e]=="CRESOL") {
        fitcresol=cuminc(d[,which(names(d)==nomtime[e])]/365.25, d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitcresol.t<-timepoints(fitcresol,t)
      }
      if (nomev[e]=="CPOLY100") {
        fitpmn=cuminc(d[,which(names(d)==nomtime[e])], d[,which(names(d)==nomev[e])], d[,which(names(d)==nomvar[i])])
        fitpmn.t<-timepoints(fitpmn,tpnn)
      }
    }
    
    ##############################################
    ### Remplissage de la matrice de réslutats ###
    ##############################################
    nclas=nlevels(d[,which(names(d)==nomvar[i])])
    for(j in 1:nclas)   {
      result[j,1]<-paste(unlist(strsplit(levels(d[,which(names(d)==nomvar[i])]), "  ")[j]))
      
      if ("STATE" %in% names(result) == TRUE) {
        result[j,which(names(result)=="STATE")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
       paste(round(fitos$surv[j]*100,1)," % [",
             round(fitos$lower[j]*100,1),"-",
             round(fitos$upper[j]*100,1),"]")))
      }
      if ("STATEPFS" %in% names(result) == TRUE) {
        result[j,which(names(result)=="STATEPFS")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
        paste(round(fitpfs$surv[j]*100,1)," % [",
              round(fitpfs$lower[j]*100,1),"-",
              round(fitpfs$upper[j]*100,1),"]")))
      }
      if ("STATEPFS2" %in% names(result) == TRUE) {
        result[j,which(names(result)=="STATEPFS2")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
                                                                          paste(round(fitpfs2$surv[j]*100,1)," % [",
                                                                                round(fitpfs2$lower[j]*100,1),"-",
                                                                                round(fitpfs2$upper[j]*100,1),"]")))
      }      
      if ("CRELAPSE" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CRELAPSE")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
      paste(round(fitri.t$est[j]*100,1)," % [",
            round(fitri.t$est[j] ^ exp(-z*sqrt(fitri.t$var[j])/(fitri.t$est[j]*log(fitri.t$est[j])))*100,1),"-",
            round(fitri.t$est[j] ^ exp(z*sqrt(fitri.t$var[j])/(fitri.t$est[j]*log(fitri.t$est[j])))*100,1),"]")))
      }
      if ("CTOXI" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CTOXI")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
       paste(round(fitnrm.t$est[j]*100,1)," % [",
             round(fitnrm.t$est[j] ^ exp(-z*sqrt(fitnrm.t$var[j])/(fitnrm.t$est[j]*log(fitnrm.t$est[j])))*100,1),"-",
             round(fitnrm.t$est[j] ^ exp(z*sqrt(fitnrm.t$var[j])/(fitnrm.t$est[j]*log(fitnrm.t$est[j])))*100,1),"]")))
      }
      if ("CSECOND" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CSECOND")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
      paste(round(fitsec.t$est[j]*100,1)," % [",
            round(fitsec.t$est[j] ^ exp(-z*sqrt(fitsec.t$var[j])/(fitsec.t$est[j]*log(fitsec.t$est[j])))*100,1),"-",
            round(fitsec.t$est[j] ^ exp(z*sqrt(fitsec.t$var[j])/(fitsec.t$est[j]*log(fitsec.t$est[j])))*100,1),"]")))
      }
      if ("STATEGVHD" %in% names(result) == TRUE) {
        result[j,which(names(result)=="STATEGVHD")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
       paste(round(fitgvhd.t$est[j]*100,1)," % [",
             round(fitgvhd.t$est[j] ^ exp(-z*sqrt(fitgvhd.t$var[j])/(fitgvhd.t$est[j]*log(fitgvhd.t$est[j])))*100,1),"-",
             round(fitgvhd.t$est[j] ^ exp(z*sqrt(fitgvhd.t$var[j])/(fitgvhd.t$est[j]*log(fitgvhd.t$est[j])))*100,1),"]")))
      }
      if ("STATECONCEPT" %in% names(result) == TRUE) {
        result[j,which(names(result)=="STATECONCEPT")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
        paste(round(fitconc.t$est[j]*100,1)," % [",
              round(fitconc.t$est[j] ^ exp(-z*sqrt(fitconc.t$var[j])/(fitconc.t$est[j]*log(fitconc.t$est[j])))*100,1),"-",
              round(fitconc.t$est[j] ^ exp(z*sqrt(fitconc.t$var[j])/(fitconc.t$est[j]*log(fitconc.t$est[j])))*100,1),"]")))
      }
      if ("GRFS" %in% names(result) == TRUE) {
        result[j,which(names(result)=="GRFS")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
        paste(round(fitgrf$surv[j]*100,1)," % [",
              round(fitgrf$lower[j]*100,1),"-",
              round(fitgrf$upper[j]*100,1),"]")))
      }
      if ("CAGVH2" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CAGVH2")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
      paste(round(fitagv2.t$est[j]*100,1)," % [",
            round(fitagv2.t$est[j] ^ exp(-z*sqrt(fitagv2.t$var[j])/(fitagv2.t$est[j]*log(fitagv2.t$est[j])))*100,1),"-",
            round(fitagv2.t$est[j] ^ exp(z*sqrt(fitagv2.t$var[j])/(fitagv2.t$est[j]*log(fitagv2.t$est[j])))*100,1),"]")))
      }
      if ("CAGVH3" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CAGVH3")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
      paste(round(fitagv3.t$est[j]*100,1)," % [",
            round(fitagv3.t$est[j] ^ exp(-z*sqrt(fitagv3.t$var[j])/(fitagv3.t$est[j]*log(fitagv3.t$est[j])))*100,1),"-",
            round(fitagv3.t$est[j] ^ exp(z*sqrt(fitagv3.t$var[j])/(fitagv3.t$est[j]*log(fitagv3.t$est[j])))*100,1),"]")))
      }
      if ("CCGVH" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CCGVH")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
       paste(round(fitcgv.t$est[j]*100,1)," % [",
             round(fitcgv.t$est[j] ^ exp(-z*sqrt(fitcgv.t$var[j])/(fitcgv.t$est[j]*log(fitcgv.t$est[j])))*100,1),"-",
             round(fitcgv.t$est[j] ^ exp(z*sqrt(fitcgv.t$var[j])/(fitcgv.t$est[j]*log(fitcgv.t$est[j])))*100,1),"]")))
      }
      if ("CCGVHEXT" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CCGVHEXT")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
        paste(round(fitcgvex.t$est[j]*100,1)," % [",
              round(fitcgvex.t$est[j] ^ exp(-z*sqrt(fitcgvex.t$var[j])/(fitcgvex.t$est[j]*log(fitcgvex.t$est[j])))*100,1),"-",
              round(fitcgvex.t$est[j] ^ exp(z*sqrt(fitcgvex.t$var[j])/(fitcgvex.t$est[j]*log(fitcgvex.t$est[j])))*100,1),"]")))
      }
      if ("CRESOL" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CRESOL")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
        paste(round(fitcresol.t$est[j]*100,1)," % [",
             round(fitcresol.t$est[j] ^ exp(-z*sqrt(fitcresol.t$var[j])/(fitcresol.t$est[j]*log(fitcresol.t$est[j])))*100,1),"-",
             round(fitcresol.t$est[j] ^ exp(z*sqrt(fitcresol.t$var[j])/(fitcresol.t$est[j]*log(fitcresol.t$est[j])))*100,1),"]")))
      }
      if ("CPOLY100" %in% names(result) == TRUE) {
        result[j,which(names(result)=="CPOLY100")]<-gsub("\\s+", "", gsub("^\\s+|\\s+$", "", 
        paste(round(fitpmn.t$est[j]*100,1)," % [",
              round(fitpmn.t$est[j] ^ exp(-z*sqrt(fitpmn.t$var[j])/(fitpmn.t$est[j]*log(fitpmn.t$est[j])))*100,1),"-",
              round(fitpmn.t$est[j] ^ exp(z*sqrt(fitpmn.t$var[j])/(fitpmn.t$est[j]*log(fitpmn.t$est[j])))*100,1),"]")))
      }
    }
    result[j+1,1]<-paste("Missing N (%)")
    if ("CRELAPSE" %in% names(result) == TRUE) {result[j+1,which(names(result)=="CRELAPSE")]<-paste0(sum(is.na(d$CRELAPSE) | is.na(d$PFSY) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CRELAPSE) | is.na(d$PFSY) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CTOXI" %in% names(result) == TRUE) {   result[j+1,which(names(result)=="CTOXI")]<-paste0(sum(is.na(d$CRELAPSE) | is.na(d$PFSY) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CRELAPSE) | is.na(d$PFSY) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CSECOND" %in% names(result) == TRUE) {result[j+1,which(names(result)=="CSECOND")]<-paste0(sum(is.na(d$CSECOND) | is.na(d$BMTSECOND) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CSECOND) | is.na(d$BMTSECOND) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("STATEGVHD" %in% names(result) == TRUE){result[j+1,which(names(result)=="STATEGVHD")]<-paste0(sum(is.na(d$STATEGVHD) | is.na(d$SURVY) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$STATEGVHD) | is.na(d$SURVY) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("STATECONCEPT" %in% names(result) == TRUE){result[j+1,which(names(result)=="STATECONCEPT")]<-paste0(sum(is.na(d$STATECONCEPT) | is.na(d$BMTCONCEPT) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$STATECONCEPT) | is.na(d$BMTCONCEPT) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("STATEPFS" %in% names(result) == TRUE) {result[j+1,which(names(result)=="STATEPFS")]<-paste0(dim(d)[1]-sum(fitpfs$n)," (",(round(100*(dim(d)[1]-sum(fitpfs$n))/dim(d)[1],0)),"%)")}
    if ("STATEPFS2" %in% names(result) == TRUE) {result[j+1,which(names(result)=="STATEPFS2")]<-paste0(dim(d)[1]-sum(fitpfs2$n)," (",(round(100*(dim(d)[1]-sum(fitpfs2$n))/dim(d)[1],0)),"%)")}
    if ("STATE" %in% names(result) == TRUE) {   result[j+1,which(names(result)=="STATE")]<-paste0(dim(d)[1]-sum(fitos$n)," (",(round(100*(dim(d)[1]-sum(fitos$n))/dim(d)[1],0)),"%)")}
    if ("GRFS" %in% names(result) == TRUE) {    result[j+1,which(names(result)=="GRFS")]<-paste0(dim(d)[1]-sum(fitgrf$n)," (",(round(100*(dim(d)[1]-sum(fitgrf$n))/dim(d)[1],0)),"%)")}
    if ("CAGVH2" %in% names(result) == TRUE) {  result[j+1,which(names(result)=="CAGVH2")]<-paste0(sum(is.na(d$CAGVH2) | is.na(d$BMTAGV2) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CAGVH2) | is.na(d$BMTAGV2) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CAGVH3" %in% names(result) == TRUE) {  result[j+1,which(names(result)=="CAGVH3")]<-paste0(sum(is.na(d$CAGVH3) | is.na(d$BMTAGV3) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CAGVH3) | is.na(d$BMTAGV3) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CCGVH" %in% names(result) == TRUE) {   result[j+1,which(names(result)=="CCGVH")]<-paste0(sum(is.na(d$CCGVH) | is.na(d$BMTCGVH) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CCGVH) | is.na(d$BMTCGVH) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CCGVHEXT" %in% names(result) == TRUE) {result[j+1,which(names(result)=="CCGVHEXT")]<-paste0(sum(is.na(d$CCGVHEXT) | is.na(d$BMTCGVHEXT) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CCGVHEXT) | is.na(d$BMTCGVHEXT) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CRESOL" %in% names(result) == TRUE) {result[j+1,which(names(result)=="CRESOL")]<-paste0(sum(is.na(d$CRESOL) | is.na(d$DELAIRESOL) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CRESOL) | is.na(d$DELAIRESOL) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    if ("CPOLY100" %in% names(result) == TRUE) {  result[j+1,which(names(result)=="CPOLY100")]<-paste0(sum(is.na(d$CPOLY100) | is.na(d$DPOLY100) | is.na(d[,nomvar[i]]))," (",round(100*sum(is.na(d$CPOLY100) | is.na(d$DPOLY100) | is.na(d[,nomvar[i]]))/dim(d)[1],0),"%)")}
    
    result[j+2,1]<-paste("P value")  
    if ("CRELAPSE" %in% names(result) == TRUE) {result[j+2,which(names(result)=="CRELAPSE")]<-format(as.numeric(fitri$Tests[1,2]),digit=3)}
    if ("CTOXI" %in% names(result) == TRUE) {   result[j+2,which(names(result)=="CTOXI")]<-format(as.numeric(fitnrm$Tests[1,2]),digit=3)}
    if ("CSECOND" %in% names(result) == TRUE) {result[j+2,which(names(result)=="CSECOND")]<-format(as.numeric(fitsec$Tests[1,2]),digit=3)}
    if ("STATEGVHD" %in% names(result) == TRUE){result[j+2,which(names(result)=="STATEGVHD")]<-format(as.numeric(fitgvhd$Tests[1,2]),digit=3)}
    if ("STATECONCEPT" %in% names(result) == TRUE){result[j+2,which(names(result)=="STATECONCEPT")]<-format(as.numeric(fitconc$Tests[1,2]),digit=3)}
    if ("STATEPFS" %in% names(result) == TRUE) {result[j+2,which(names(result)=="STATEPFS")]<-format(as.numeric(ptestpfs),digit=3)}
    if ("STATEPFS2" %in% names(result) == TRUE) {result[j+2,which(names(result)=="STATEPFS2")]<-format(as.numeric(ptestpfs2),digit=3)}
    if ("STATE" %in% names(result) == TRUE) {   result[j+2,which(names(result)=="STATE")]<-format(as.numeric(ptestos),digit=3)}
    if ("GRFS" %in% names(result) == TRUE) {    result[j+2,which(names(result)=="GRFS")]<-format(as.numeric(ptestgrf),digit=3)}
    if ("CAGVH2" %in% names(result) == TRUE) {  result[j+2,which(names(result)=="CAGVH2")]<-format(as.numeric(fitagv2$Tests[1,2]),digit=3)}
    if ("CAGVH3" %in% names(result) == TRUE) {  result[j+2,which(names(result)=="CAGVH3")]<-format(as.numeric(fitagv3$Tests[1,2]),digit=3)}
    if ("CCGVH" %in% names(result) == TRUE) {   result[j+2,which(names(result)=="CCGVH")]<-format(as.numeric(fitcgv$Tests[1,2]),digit=3)}
    if ("CCGVHEXT" %in% names(result) == TRUE) {result[j+2,which(names(result)=="CCGVHEXT")]<-format(as.numeric(fitcgvex$Tests[1,2]),digit=3)}
    if ("CCGVHEXT" %in% names(result) == TRUE) {result[j+2,which(names(result)=="CCGVHEXT")]<-format(as.numeric(fitcgvex$Tests[1,2]),digit=3)}
    if ("CRESOL" %in% names(result) == TRUE) {result[j+2,which(names(result)=="CRESOL")]<-format(as.numeric(fitcresol$Tests[1,2]),digit=3)}
    if ("CPOLY100" %in% names(result) == TRUE) {  result[j+2,which(names(result)=="CPOLY100")]<-format(as.numeric(fitpmn$Tests[1,2]),digit=3)}
  }
  result
}  

# d<-read.spss("C:\\Users\\g-sat-oebmt01.WPROD\\Desktop\\code_en_cours\\mrc.sav",use.value.labels=T,to.data.frame=T)
# colnames(d)<-toupper(colnames(d)) ### Mettre tout les noms de variables en majuscule
# nomvar<-dput(names(d)) ### créer un vecteur de tout les noms de variable de la bdd
# d <- droplevels(d) ### REtirer les "levels" vides
# 
# covars.uni <- c("CLASSGRIM","PATSEX","TYPDON") ### Définition des variable que l'on souhaite étudier
# res<-univ_outcomes(data=d,covars.uni = covars.uni)

# univ_outcomes<-function(data,covars.uni,t=c(2,3)){
#   data=d
#   res<-NULL
#   for(r in 1:length(covars.uni)){
#     if(is.factor(d[,which(names(d)==covars.uni[r])])==TRUE) {
#       nomvar=covars.uni[r]
#       nomev<-c("CRELAPSE","CTOXI","STATEPFS","STATE","GRFS","CAGVH2","CAGVH3","CCGVH","CCGVHEXT")
#       nomtime<-c("PFSY","PFSY","PFSY","SURVY","BMTGRFSY","BMTAGV2","BMTAGV3","BMTCGVH","BMTCGVHEXT")
#       for(tt in t){
#       rest<-rap16(repe,d,nomvar,nomtime,nomev,tt,r)
#       for(p in 2:dim(rest)[2]){
#         rest[length(levels(d[,nomvar]))+1,p]<-ifelse(as.numeric(as.character(rest[length(levels(d[,nomvar]))+1,p]))<0.0001,
#                                               "<0.0001",
#                                               rest[length(levels(d[,nomvar]))+1,p])
#       }
#       res<-try(rbind(res,
#                      cbind(time=rep(tt,length(levels(d[,nomvar]))+1),
#                            Variable=rep(nomvar,length(levels(d[,nomvar]))+1),
#                            rest[1:(length(levels(d[,nomvar]))+1),])))
#       }
#     }
#   }
#   res
# }
# 

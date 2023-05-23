
survcum<-function(data,t=2){
indata2<-data  
z <- qnorm(1-(1-0.95)/2)


stat_os<-paste0(round(summary(survfit(Surv(indata2$SURVY,indata2$STATE)~1,conf.type="log-log"),t)$surv*100,1)," ( ",
                round(summary(survfit(Surv(indata2$SURVY,indata2$STATE)~1,conf.type="log-log"),t)$lower*100,1)," - ",
                round(summary(survfit(Surv(indata2$SURVY,indata2$STATE)~1,conf.type="log-log"),t)$upper*100,1)," ) ")

stat_pfs<-paste0(round(summary(survfit(Surv(indata2$PFSY,indata2$STATEPFS)~1,conf.type="log-log"),t)$surv*100,1)," ( ",
                 round(summary(survfit(Surv(indata2$PFSY,indata2$STATEPFS)~1,conf.type="log-log"),t)$lower*100,1)," - ",
                 round(summary(survfit(Surv(indata2$PFSY,indata2$STATEPFS)~1,conf.type="log-log"),t)$upper*100,1)," ) ")

grfs<-try(summary(survfit(Surv(indata2$BMTGRFSY,indata2$GRFS)~1,conf.type="log-log")))

if(class(grfs)!="try-error"){
 stat_grfs<-paste0(round(summary(survfit(Surv(indata2$BMTGRFSY,indata2$GRFS)~1,conf.type="log-log"),t)$surv*100,1)," ( ",
                   round(summary(survfit(Surv(indata2$BMTGRFSY,indata2$GRFS)~1,conf.type="log-log"),t)$lower*100,1)," - ",
                   round(summary(survfit(Surv(indata2$BMTGRFSY,indata2$GRFS)~1,conf.type="log-log"),t)$upper*100,1)," ) ")
}

# aGHVD II 100 jours
cum_agvh02<-try(cuminc(indata2$BMTAGV2,indata2$CAGVH2,rho=0,cencode = 0))

if(class(cum_agvh02)!="try-error"){
cum_agvh2<-timepoints(cum_agvh02,100)
stat_cum_agvh2 <- paste0(round(cum_agvh2$est[1]*100,1),
                         " ( ",round((cum_agvh2$est ^ exp(-z*sqrt(cum_agvh2$var)/(cum_agvh2$est*log(cum_agvh2$est))))[1]*100,1)," - ",
                         round((cum_agvh2$est ^ exp(z*sqrt(cum_agvh2$var)/(cum_agvh2$est*log(cum_agvh2$est))))[1]*100,1)," ) ")
}

# aGHVD III 100 jours
cum_agvh03<-try(cuminc(indata2$BMTAGV3,indata2$CAGVH3,rho=0,cencode = 0))
if(class(cum_agvh03)!="try-error"){
cum_agvh3<-timepoints(cum_agvh03,100)
stat_cum_agvh3 <- paste0(round(cum_agvh3$est[1]*100,1),
                         " ( ",round((cum_agvh3$est ^ exp(-z*sqrt(cum_agvh3$var)/(cum_agvh3$est*log(cum_agvh3$est))))[1]*100,1)," - ",
                         round((cum_agvh3$est ^ exp(z*sqrt(cum_agvh3$var)/(cum_agvh3$est*log(cum_agvh3$est))))[1]*100,1)," ) ")
}

# DPOLY
cum_dpoply030<-try(cuminc(indata2$DPOLY1,indata2$CPOLY1,rho=0,cencode = 0),silent = TRUE)

# DPOLY 30 jours
if(class(cum_dpoply030)!="try-error"){
cum_dpoply30<-timepoints(cum_dpoply030,30)
stat_cum_dpoly30 <- paste0(round(cum_dpoply30$est[1]*100,1),
                           " ( ",round((cum_dpoply30$est ^ exp(-z*sqrt(cum_dpoply30$var)/(cum_dpoply30$est*log(cum_dpoply30$est))))[1]*100,1)," - ",
                           round((cum_dpoply30$est ^ exp(z*sqrt(cum_dpoply30$var)/(cum_dpoply30$est*log(cum_dpoply30$est))))[1]*100,1)," ) ")

# DPLOY 42 jours
cum_dpoply42<-timepoints(cum_dpoply030,42)
stat_cum_dpoly42 <- paste0(round(cum_dpoply42$est[1]*100,1),
                           " ( ",round((cum_dpoply42$est ^ exp(-z*sqrt(cum_dpoply42$var)/(cum_dpoply42$est*log(cum_dpoply42$est))))[1]*100,1)," - ",
                           round((cum_dpoply42$est ^ exp(z*sqrt(cum_dpoply42$var)/(cum_dpoply42$est*log(cum_dpoply42$est))))[1]*100,1)," ) ")


# DPLOY 60 jours
cum_dpoply60<-timepoints(cum_dpoply030,60)
stat_cum_dpoly60 <- paste0(round(cum_dpoply60$est[1]*100,1),
                           " ( ",round((cum_dpoply60$est ^ exp(-z*sqrt(cum_dpoply60$var)/(cum_dpoply60$est*log(cum_dpoply60$est))))[1]*100,1)," - ",
                           round((cum_dpoply60$est ^ exp(z*sqrt(cum_dpoply60$var)/(cum_dpoply60$est*log(cum_dpoply60$est))))[1]*100,1)," ) ")

}

# DPLAT
cum_dplat<-try(cuminc(indata2$DPL20,indata2$CPLAT20,rho=0,cencode = 0),silent=TRUE)

# DPOLY 30 jours
if(class(cum_dplat)!="try-error"){
  
cum_dplat30<-timepoints(cum_dplat,60)
stat_cum_dplat30 <- paste0(round(cum_dplat30$est[1]*100,1),
                           " ( ",round((cum_dplat30$est ^ exp(-z*sqrt(cum_dplat30$var)/(cum_dplat30$est*log(cum_dplat30$est))))[1]*100,1)," - ",
                           round((cum_dplat30$est ^ exp(z*sqrt(cum_dplat30$var)/(cum_dplat30$est*log(cum_dplat30$est))))[1]*100,1)," ) ")

# DPOLY 60 jours
cum_dplat60<-timepoints(cum_dplat,180)
stat_cum_dplat60 <- paste0(round(cum_dplat60$est[1]*100,1),
                           " ( ",round((cum_dplat60$est ^ exp(-z*sqrt(cum_dplat60$var)/(cum_dplat60$est*log(cum_dplat60$est))))[1]*100,1)," - ",
                           round((cum_dplat60$est ^ exp(z*sqrt(cum_dplat60$var)/(cum_dplat60$est*log(cum_dplat60$est))))[1]*100,1)," ) ")
}

# DPLAT
cum_dplat50<-try(cuminc(indata2$DPL50,indata2$CPLAT50,rho=0,cencode = 0),silent=TRUE)

# DPOLY 30 jours
if(class(cum_dplat50)!="try-error"){
  
  cum_dplat5030<-timepoints(cum_dplat50,60)
  stat_cum_dplat5030 <- paste0(round(cum_dplat5030$est[1]*100,1),
                             " ( ",round((cum_dplat5030$est ^ exp(-z*sqrt(cum_dplat5030$var)/(cum_dplat5030$est*log(cum_dplat5030$est))))[1]*100,1)," - ",
                             round((cum_dplat5030$est ^ exp(z*sqrt(cum_dplat5030$var)/(cum_dplat5030$est*log(cum_dplat5030$est))))[1]*100,1)," ) ")
  
  # DPOLY 60 jours
  cum_dplat5060<-timepoints(cum_dplat50,180)
  stat_cum_dplat5060 <- paste0(round(cum_dplat5060$est[1]*100,1),
                             " ( ",round((cum_dplat5060$est ^ exp(-z*sqrt(cum_dplat5060$var)/(cum_dplat5060$est*log(cum_dplat5060$est))))[1]*100,1)," - ",
                             round((cum_dplat5060$est ^ exp(z*sqrt(cum_dplat5060$var)/(cum_dplat5060$est*log(cum_dplat5060$est))))[1]*100,1)," ) ")
}


cum_setx<-try(timepoints(cuminc(indata2$BMTSECOND,indata2$CSECOND,rho=0,cencode = 0),t*365.25))


if(class(cum_setx)!="try-error"){
  
  stat_cum_setx <- paste0(round(cum_setx$est[1]*100,1),
                               " ( ",round((cum_setx$est ^ exp(-z*sqrt(cum_setx$var)/(cum_setx$est*log(cum_setx$est))))[1]*100,1)," - ",
                               round((cum_setx$est ^ exp(z*sqrt(cum_setx$var)/(cum_setx$est*log(cum_setx$est))))[1]*100,1)," ) ")
}


cum_nrm<-timepoints(cuminc(indata2$PFSY,indata2$CRELAPSE,rho=0,cencode = 0),t)

stat_ri<-paste0(round(cum_nrm$est[1]*100,1)," ( ",
                round((cum_nrm$est ^ exp(-z*sqrt(cum_nrm$var)/(cum_nrm$est*log(cum_nrm$est))))[1]*100,1)," - ",
                round((cum_nrm$est ^ exp(z*sqrt(cum_nrm$var)/(cum_nrm$est*log(cum_nrm$est))))[1]*100,1)," ) ")


stat_nrm<-paste0(round(cum_nrm$est[2]*100,1)," ( ",
                 round((cum_nrm$est ^ exp(-z*sqrt(cum_nrm$var)/(cum_nrm$est*log(cum_nrm$est))))[2]*100,1)," - ",
                 round((cum_nrm$est ^ exp(z*sqrt(cum_nrm$var)/(cum_nrm$est*log(cum_nrm$est))))[2]*100,1)," ) ")

if("STATEGVHD" %in% names(indata2)){
# # DC du a la GvHD 
 cum_gvhd_dc<-timepoints(cuminc(indata2$SURVY,indata2$STATEGVHD,rho=0,cencode = 0),t)
 stat_gvhd_dc<-paste0(round(cum_gvhd_dc$est[1]*100,1)," ( ",
                      round((cum_gvhd_dc$est ^ exp(-z*sqrt(cum_gvhd_dc$var)/(cum_gvhd_dc$est*log(cum_gvhd_dc$est))))[1]*100,1)," - ",
                      round((cum_gvhd_dc$est ^ exp(z*sqrt(cum_gvhd_dc$var)/(cum_gvhd_dc$est*log(cum_gvhd_dc$est))))[1]*100,1)," ) ")
}

if("STATECONCEPT" %in% names(indata2)){
  # # DC du a la GvHD 
  print("a")
  cum_conc<-timepoints(cuminc(indata2$BMTCONCEPT,indata2$STATECONCEPT,rho=0,cencode = 0),t)
  print(cum_conc)
  stat_conc<-paste0(round(cum_conc$est[1]*100,1)," ( ",
                       round((cum_conc$est ^ exp(-z*sqrt(cum_conc$var)/(cum_conc$est*log(cum_conc$est))))[1]*100,1)," - ",
                       round((cum_conc$est ^ exp(z*sqrt(cum_conc$var)/(cum_conc$est*log(cum_conc$est))))[1]*100,1)," ) ")
}
 
# CGVHD 
cum_cgvhd<-try(timepoints(cuminc(indata2$BMTCGVH,indata2$CCGVH,rho=0,cencode = 0),t*365.25))

if(class(cum_cgvhd)!="try-error"){
stat_cgvhd<-paste0(round(cum_cgvhd$est[1]*100,1)," ( ",
                   round((cum_cgvhd$est ^ exp(-z*sqrt(cum_cgvhd$var)/(cum_cgvhd$est*log(cum_cgvhd$est))))[1]*100,1)," - ",
                   round((cum_cgvhd$est ^ exp(z*sqrt(cum_cgvhd$var)/(cum_cgvhd$est*log(cum_cgvhd$est))))[1]*100,1)," ) ")
}

# CGVHD extensive
cum_cgvhd_ext<-try(timepoints(cuminc(indata2$BMTCGVHEXT,indata2$CCGVHEXT,rho=0,cencode = 0),t*365.25))

if(class(cum_cgvhd_ext)!="try-error"){
stat_cgvhd_ext<-paste0(round(cum_cgvhd_ext$est[1]*100,1)," ( ",
                       round((cum_cgvhd_ext$est ^ exp(-z*sqrt(cum_cgvhd_ext$var)/(cum_cgvhd_ext$est*log(cum_cgvhd_ext$est))))[1]*100,1)," - ",
                       round((cum_cgvhd_ext$est ^ exp(z*sqrt(cum_cgvhd_ext$var)/(cum_cgvhd_ext$est*log(cum_cgvhd_ext$est))))[1]*100,1)," ) ")
}
### Mediane de suivi par reverse KM
medianfu<-summary(survfit(Surv(indata2$SURV,indata2$STATE==0)~1,conf.type="log-log"))$table
medianfu_stat<-paste0( round(medianfu["median"]/365.25,1), " ( ",
                       round(medianfu["0.95LCL"]/365.25,1), " - ",
                       round(medianfu["0.95UCL"]/365.25,1), " ) ")

###########################################################
########### Mise en forme des results   ###################
###########################################################


  res_surv_desc_indata2<-as.data.frame(rbind(c("Median FU (y)",survfit(Surv(indata2$SURV,indata2$STATE)~1,conf.type="log-log")$n,"",medianfu_stat),
                                             c(paste0("OS (" ,t," y)"),survfit(Surv(indata2$SURV,indata2$STATE)~1,conf.type="log-log")$n,
                                               summary(survfit(Surv(indata2$SURVY,indata2$STATE)~1,conf.type="log-log"),t)$n.event,stat_os),
                                             c(paste0("PFS (" ,t," y)"),survfit(Surv(indata2$PFS,indata2$STATEPFS)~1,conf.type="log-log")$n,
                                               summary(survfit(Surv(indata2$PFSY,indata2$STATEPFS)~1,conf.type="log-log"),t)$n.event,stat_pfs),
                                             c("Poly recovery (30 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, sum(indata2$CPOLY1==1 & indata2$DPOLY1 <31,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, stat_cum_dpoly30)),
                                             c("Poly recovery (42 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, sum(indata2$CPOLY1==1 & indata2$DPOLY1 <42+1,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, stat_cum_dpoly42)),
                                             c("Poly recovery (60 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA,sum(indata2$CPOLY1==1 & indata2$DPOLY1 <61,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA,stat_cum_dpoly60)),
                                             c("Platelet recovery (>=20) (60 d)",
                                               ifelse(class(cum_dplat)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL20))),
                                               ifelse(class(cum_dplat)=="try-error",NA,sum(indata2$CPLAT20==1 & indata2$DPL20 <61,na.rm=T)),
                                               ifelse(class(cum_dplat)=="try-error",NA,stat_cum_dplat30)),
                                             c("Platelet recovery (>=20) (180 d)",
                                               ifelse(class(cum_dplat)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL20))),
                                               ifelse(class(cum_dplat)=="try-error",NA,sum(indata2$CPLAT20==1 & indata2$DPL20 <181,na.rm=T)),
                                               ifelse(class(cum_dplat)=="try-error",NA,stat_cum_dplat60)),
                                             c("Platelet recovery (>=50) (60 d)",
                                               ifelse(class(cum_dplat50)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL50))),
                                               ifelse(class(cum_dplat50)=="try-error",NA,sum(indata2$CPLAT50==1 & indata2$DPL50 <61,na.rm=T)),
                                               ifelse(class(cum_dplat50)=="try-error",NA,stat_cum_dplat5030)),
                                             c("Platelet recovery (>=50) (180 d)",
                                               ifelse(class(cum_dplat50)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL50))),
                                               ifelse(class(cum_dplat50)=="try-error",NA,sum(indata2$CPLAT50==1 & indata2$DPL50 <181,na.rm=T)),
                                               ifelse(class(cum_dplat50)=="try-error",NA,stat_cum_dplat5060)),
                                             c("aGVH-II/IV (100 d)",
                                               ifelse(class(cum_agvh02)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTAGV2))),
                                               ifelse(class(cum_agvh02)=="try-error",NA,sum(indata2$CAGVH2==1 & indata2$BMTAGV2 <101,na.rm=T)),
                                               ifelse(class(cum_agvh02)=="try-error",NA,stat_cum_agvh2)),
                                             c("aGVH-III/IV (100 d)",
                                               ifelse(class(cum_agvh03)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTAGV3))),
                                               ifelse(class(cum_agvh03)=="try-error",NA,sum(indata2$CAGVH3==1 & indata2$BMTAGV3 <101,na.rm=T)),
                                               ifelse(class(cum_agvh03)=="try-error",NA,stat_cum_agvh3)),
                                             c(paste0("GRFS (" ,t," y)"),
                                               ifelse(class(grfs)=="try-error",NA,survfit(Surv(indata2$BMTGRFS,indata2$GRFS)~1)$n),
                                               ifelse(class(grfs)=="try-error",NA,summary(survfit(Surv(indata2$BMTGRFSY,indata2$GRFS)~1),t)$n.event),
                                               ifelse(class(grfs)=="try-error",NA,stat_grfs)),    
                                             c(paste0("RI (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$PFS)),
                                               sum(indata2$CRELAPSE==1 & indata2$PFSY < t,na.rm=T),stat_ri),
                                             c(paste0("NRM (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$PFS)),
                                               sum(indata2$CRELAPSE==2 & indata2$PFSY < t,na.rm=T),stat_nrm),
                                             c(paste0("Second TX (" ,t," y)"),
                                               ifelse(class(cum_setx)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTSECOND))),
                                               ifelse(class(cum_setx)=="try-error",NA,sum(indata2$CSECOND==1 & indata2$BMTSECOND < t*365.25,na.rm=T)),
                                               ifelse(class(cum_setx)=="try-error",NA,stat_cum_setx)),
                                             c(paste0("CGVHD (" ,t," y)"),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTCGVH) | is.na(indata2$CCGVH))),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,sum(indata2$CCGVH==1 & indata2$BMTCGVH <365.25*t,na.rm=T)),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,stat_cgvhd)),
                                             c(paste0("CGVHD Ext (" ,t," y)"),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTCGVHEXT) | is.na(indata2$CCGVHEXT))),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,sum(indata2$CCGVHEXT==1 & indata2$BMTCGVHEXT <365.25*t,na.rm=T)),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,stat_cgvhd_ext))))


if("STATEGVHD" %in% names(indata2)){
  
  res_surv_desc_indata2<-as.data.frame(rbind(c("Median FU (y)",dim(indata2)[1],"",medianfu_stat),
                                             c(paste0("OS (" ,t," y)"),survfit(Surv(indata2$SURV,indata2$STATE)~1)$n,
                                               summary(survfit(Surv(indata2$SURVY,indata2$STATE)~1),t)$n.event,stat_os),
                                             c(paste0("PFS (" ,t," y)"),survfit(Surv(indata2$PFS,indata2$STATEPFS)~1)$n,
                                               summary(survfit(Surv(indata2$PFSY,indata2$STATEPFS)~1),t)$n.event,stat_pfs),
                                             c("Poly recovery (30 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, sum(indata2$CPOLY1==1 & indata2$DPOLY1 <31,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, stat_cum_dpoly30)),
                                             c("Poly recovery (60 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA,sum(indata2$CPOLY1==1 & indata2$DPOLY1 <61,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA,stat_cum_dpoly60)),
                                             c("Platelet recovery (>=20) (30 d)",
                                               ifelse(class(cum_dplat)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL20))),
                                               ifelse(class(cum_dplat)=="try-error",NA,sum(indata2$CPLAT20==1 & indata2$DPL20 <31,na.rm=T)),
                                               ifelse(class(cum_dplat)=="try-error",NA,stat_cum_dplat30)),
                                             c("Platelet recovery (>=20) (60 d)",
                                               ifelse(class(cum_dplat)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL20))),
                                               ifelse(class(cum_dplat)=="try-error",NA,sum(indata2$CPLAT20==1 & indata2$DPL20 <61,na.rm=T)),
                                               ifelse(class(cum_dplat)=="try-error",NA,stat_cum_dplat60)),
                                             c("aGVH-II/IV (100 d)",
                                               ifelse(class(cum_agvh02)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTAGV2))),
                                               ifelse(class(cum_agvh02)=="try-error",NA,sum(indata2$CAGVH2==1 & indata2$BMTAGV2 <101,na.rm=T)),
                                               ifelse(class(cum_agvh02)=="try-error",NA,stat_cum_agvh2)),
                                             c("aGVH-III/IV (100 d)",
                                               ifelse(class(cum_agvh03)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTAGV3))),
                                               ifelse(class(cum_agvh03)=="try-error",NA,sum(indata2$CAGVH3==1 & indata2$BMTAGV3 <101,na.rm=T)),
                                               ifelse(class(cum_agvh03)=="try-error",NA,stat_cum_agvh3)),
                                             c(paste0("GRFS (" ,t," y)"),
                                               ifelse(class(grfs)=="try-error",NA,survfit(Surv(indata2$BMTGRFS,indata2$GRFS)~1)$n),
                                               ifelse(class(grfs)=="try-error",NA,summary(survfit(Surv(indata2$BMTGRFSY,indata2$GRFS)~1),t)$n.event),
                                               ifelse(class(grfs)=="try-error",NA,stat_grfs)),    
                                             c(paste0("RI (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$PFS)),
                                               sum(indata2$CRELAPSE==1 & indata2$PFSY < t,na.rm=T),stat_ri),
                                             c(paste0("NRM (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$PFS)),
                                               sum(indata2$CRELAPSE==2 & indata2$PFSY < t,na.rm=T),stat_nrm),
                                             c(paste0("CGVHD (" ,t," y)"),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTCGVH) | is.na(indata2$CCGVH))),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,sum(indata2$CCGVH==1 & indata2$BMTCGVH <365.25*t,na.rm=T)),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,stat_cgvhd)),
                                             c(paste0("CGVHD Ext (" ,t," y)"),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTCGVHEXT) | is.na(indata2$CCGVHEXT))),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,sum(indata2$CCGVHEXT==1 & indata2$BMTCGVHEXT <365.25*t,na.rm=T)),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,stat_cgvhd_ext)),
                                             c(paste0("GVHD Death (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$SURV) | is.na(indata2$STATEGVHD)),
                                               sum(indata2$STATEGVHD==1 & indata2$SURV <365.25*t,na.rm=T),stat_gvhd_dc)))
  
  
}

if("STATECONCEPT" %in% names(indata2)){
  
  res_surv_desc_indata2<-as.data.frame(rbind(c("Median FU (y)",dim(indata2)[1],"",medianfu_stat),
                                             c(paste0("OS (" ,t," y)"),survfit(Surv(indata2$SURV,indata2$STATE)~1)$n,
                                               summary(survfit(Surv(indata2$SURVY,indata2$STATE)~1),t)$n.event,stat_os),
                                             c(paste0("PFS (" ,t," y)"),survfit(Surv(indata2$PFS,indata2$STATEPFS)~1)$n,
                                               summary(survfit(Surv(indata2$PFSY,indata2$STATEPFS)~1),t)$n.event,stat_pfs),
                                             c("Poly recovery (30 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, sum(indata2$CPOLY1==1 & indata2$DPOLY1 <31,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA, stat_cum_dpoly30)),
                                             c("Poly recovery (60 d)",
                                               ifelse(class(cum_dpoply030)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPOLY1))),
                                               ifelse(class(cum_dpoply030)=="try-error",NA,sum(indata2$CPOLY1==1 & indata2$DPOLY1 <61,na.rm=T)),
                                               ifelse(class(cum_dpoply030)=="try-error",NA,stat_cum_dpoly60)),
                                             c("Platelet recovery (>=20) (30 d)",
                                               ifelse(class(cum_dplat)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL20))),
                                               ifelse(class(cum_dplat)=="try-error",NA,sum(indata2$CPLAT20==1 & indata2$DPL20 <31,na.rm=T)),
                                               ifelse(class(cum_dplat)=="try-error",NA,stat_cum_dplat30)),
                                             c("Platelet recovery (>=20) (60 d)",
                                               ifelse(class(cum_dplat)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$DPL20))),
                                               ifelse(class(cum_dplat)=="try-error",NA,sum(indata2$CPLAT20==1 & indata2$DPL20 <61,na.rm=T)),
                                               ifelse(class(cum_dplat)=="try-error",NA,stat_cum_dplat60)),
                                             c("aGVH-II/IV (100 d)",
                                               ifelse(class(cum_agvh02)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTAGV2))),
                                               ifelse(class(cum_agvh02)=="try-error",NA,sum(indata2$CAGVH2==1 & indata2$BMTAGV2 <101,na.rm=T)),
                                               ifelse(class(cum_agvh02)=="try-error",NA,stat_cum_agvh2)),
                                             c("aGVH-III/IV (100 d)",
                                               ifelse(class(cum_agvh03)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTAGV3))),
                                               ifelse(class(cum_agvh03)=="try-error",NA,sum(indata2$CAGVH3==1 & indata2$BMTAGV3 <101,na.rm=T)),
                                               ifelse(class(cum_agvh03)=="try-error",NA,stat_cum_agvh3)),                                             c(paste0("RI (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$PFS)),
                                               sum(indata2$CRELAPSE==1 & indata2$PFSY < t,na.rm=T),stat_ri),
                                             c(paste0("NRM (" ,t," y)"),dim(indata2)[1]- sum(is.na(indata2$PFS)),
                                               sum(indata2$CRELAPSE==2 & indata2$PFSY < t,na.rm=T),stat_nrm),
                                             c(paste0("CGVHD (" ,t," y)"),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTCGVH) | is.na(indata2$CCGVH))),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,sum(indata2$CCGVH==1 & indata2$BMTCGVH <365.25*t,na.rm=T)),
                                               ifelse(class(cum_cgvhd)=="try-error",NA,stat_cgvhd)),                                             
                                             c(paste0("CGVHD Ext (" ,t," y)"),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,dim(indata2)[1]- sum(is.na(indata2$BMTCGVHEXT) | is.na(indata2$CCGVHEXT))),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,sum(indata2$CCGVHEXT==1 & indata2$BMTCGVHEXT <365.25*t,na.rm=T)),
                                               ifelse(class(cum_cgvhd_ext)=="try-error",NA,stat_cgvhd_ext)),
                                             c(paste0("Conception Incidence (" ,t," y)"),
                                               dim(indata2)[1]- sum(is.na(indata2$BMTCONCEPT) | is.na(indata2$STATECONCEPT)),
                                               sum(indata2$STATECONCEPT==1 & indata2$BMTCONCEPT < t,na.rm=T),stat_conc)))
}


names(res_surv_desc_indata2)<-c("Outcomes","N","Event (N)","Estimation (IC95%)")
return(res_surv_desc_indata2)
}
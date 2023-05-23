########################################################
# Les outcomes. ########################################
########################################################

compute_outcomes<-function(baz,compete_relapse=TRUE,impute_relapse=FALSE,compete_consecTX_engneut=FALSE){
  
names(baz)<-toupper(names(baz))

if(class(baz$DATBMT)!="Date")       suppressWarnings(baz$DATBMT <- as.Date(as.numeric(as.character(baz$DATBMT))/86400, origin = "1582-10-14"))
if(class(baz$DATPATBD)!="Date")     suppressWarnings(baz$DATPATBD <- as.Date(as.numeric(as.character(baz$DATPATBD))/86400, origin = "1582-10-14"))
if(class(baz$DATLASTFU)!="Date")    suppressWarnings(baz$DATLASTFU<- as.Date(as.numeric(as.character(baz$DATLASTFU))/86400, origin = "1582-10-14"))
if(class(baz$DATAGVH)!="Date")      suppressWarnings(baz$DATAGVH<- as.Date(as.numeric(as.character(baz$DATAGVH))/86400, origin = "1582-10-14"))
if(class(baz$DATLASTALIVE)!="Date") suppressWarnings(try(baz$DATLASTALIVE<- as.Date(as.numeric(as.character(baz$DATLASTALIVE))/86400, origin = "1582-10-14"),silent=TRUE))

if(class(baz$DTRANS1)!="Date" & "DTRANS1" %in% names(baz))      suppressWarnings(baz$DTRANS1<- as.Date(as.numeric(as.character(baz$DTRANS1))/86400, origin = "1582-10-14"))
if(class(baz$DTRANS2)!="Date" & "DTRANS2" %in% names(baz))      suppressWarnings(baz$DTRANS2<- as.Date(as.numeric(as.character(baz$DTRANS2))/86400, origin = "1582-10-14"))
if(class(baz$DTRANS3)!="Date" & "DTRANS3" %in% names(baz))      suppressWarnings(baz$DTRANS3<- as.Date(as.numeric(as.character(baz$DTRANS3))/86400, origin = "1582-10-14"))
if(class(baz$DTRANS4)!="Date" & "DTRANS4" %in% names(baz))      suppressWarnings(baz$DTRANS3<- as.Date(as.numeric(as.character(baz$DTRANS4))/86400, origin = "1582-10-14"))
if(class(baz$DTRANS5)!="Date" & "DTRANS5" %in% names(baz))      suppressWarnings(baz$DTRANS3<- as.Date(as.numeric(as.character(baz$DTRANS5))/86400, origin = "1582-10-14"))


baz$DATLASTFU[which(baz$DATLASTFU=="1809-12-31")]<-NA
baz$DATBMT[which(baz$DATBMT=="1809-12-31")]<-NA
baz$DATPATBD[which(baz$DATPATBD=="1809-12-31")]<-NA
baz$DATAGVH[which(baz$DATAGVH=="1809-12-31")]<-NA
if("DATLASTALIVE" %in% names(baz)) baz$DATLASTALIVE[which(baz$DATLASTALIVE=="1809-12-31")]<-NA

############################################
#Comment no event after transplant. ########
############################################

# Relapse
if(class(baz$DATRELAPSE)!="Date") suppressWarnings(baz$DATRELAPSE<- as.Date(as.numeric(as.character(baz$DATRELAPSE))/86400, origin = "1582-10-14"))
if(class(baz$LDATNOREL)!="Date")  suppressWarnings(baz$LDATNOREL<- as.Date(as.numeric(as.character(baz$LDATNOREL))/86400, origin = "1582-10-14"))

baz$DATRELAPSE[which(baz$DATRELAPSE=="1809-12-31")]<-NA
baz$LDATNOREL[which(baz$LDATNOREL=="1809-12-31")]<-NA


# VRELPROG --> variable rechute ou progression
baz$VRELPROG<-as.character(baz$VRELPROG)
baz$VRELPROG[which(is.na(baz$VRELPROG) & baz$LDATNOREL==baz$DATLASTFU)]<-"No"
baz$VRELPROG<-as.factor(baz$VRELPROG)

# Chronic GVHD
if(class(baz$LDATNOCGVH)!="Date") baz$LDATNOCGVH<- suppressWarnings(as.Date(as.numeric(as.character(baz$LDATNOCGVH))/86400, origin = "1582-10-14"))
baz$LDATNOCGVH[which(baz$LDATNOCGVH=="1809-12-31")]<-NA

# VCGVHDG --> Extent of CGVHD
baz$VCGVHDG<-as.character(baz$VCGVHDG)
baz$VCGVHDG[which(is.na(baz$VCGVHDG) & baz$LDATNOCGVH==baz$DATLASTFU)]<-"None"
baz$VCGVHDG<-as.factor(baz$VCGVHDG)

# GRAVHOSD_1ST --> c GvHD after HSCT ( Y/N)?
baz$GRAVHOSD_1ST<-as.character(baz$GRAVHOSD_1ST)
baz$GRAVHOSD_1ST[which(is.na(baz$GRAVHOSD_1ST) & baz$LDATNOCGVH==baz$DATLASTFU)]<-"No"
baz$GRAVHOSD_1ST<-as.factor(baz$GRAVHOSD_1ST)

# GRAVHOSD --> Chronic graft versus host disease (cGvHD)
baz$GRAVHOSD<-as.character(baz$GRAVHOSD)
baz$GRAVHOSD[which(is.na(baz$GRAVHOSD) & baz$LDATNOCGVH==baz$DATLASTFU)]<-"No"
baz$GRAVHOSD<-as.factor(baz$GRAVHOSD)

#############################
###### Death variable #######
#############################

# Variable source: VPATSTAT et DATLASTFU

baz$STATE<-NA
baz$STATE[which(baz$VPATSTAT=="Dead")]<-1
baz$STATE[which(baz$VPATSTAT=="Died before HSCT but after conditioning was initiated")]<-1 # Attention a ces patients
baz$STATE[which(baz$VPATSTAT=="Alive")]<-0
baz$STATE[which(baz$VPATSTAT=="Lost to follow up")]<-0

baz$SURV<-as.numeric(difftime(baz$DATLASTFU,baz$DATBMT,unit="days")+1)
# baz$SURV[which(baz$SURV < 0)]<-NA

#############################
###### Relapse variable #####
#############################

# Variable source: VRELPROG et DATRELAPSE

print(paste("VRELPROG and VPATSTAT descriptions:"))
print(table(baz$VRELPROG,baz$VPATSTAT,useNA="ifany"))

baz$RELAPSE<-NA
if (impute_relapse==TRUE) baz$RELAPSE[which(is.na(baz$VRELPROG) | baz$VRELPROG=="No")]<-0 #### Attention, NA comme no RELAPSE !!!
if (impute_relapse==FALSE) baz$RELAPSE[which(baz$VRELPROG=="No")]<-0 

baz$RELAPSE[which(baz$VRELPROG=="Yes"| 
                    baz$VRELPROG=="Yes, before this date"|
                    baz$VRELPROG=="Continuous progression")]<-1 # Continuous progression date ????

# PFS: Delai entre greffe et rechute, dc ou lastfu

baz$PFS<-NA
baz$PFS[which(baz$RELAPSE==1)]<-as.numeric(difftime(baz$DATRELAPSE,baz$DATBMT,unit="days")+1)[which(baz$RELAPSE==1)]
baz$PFS[which(baz$RELAPSE==0)]<-as.numeric(difftime(baz$DATLASTFU,baz$DATBMT,unit="days")+1)[which(baz$RELAPSE==0)]

baz$PFS[which(baz$VRELPROG=="Continuous progression" & is.na(baz$DATRELAPSE))]<-28 # Imputation a 28 j comme CIBMTR

# STATEPFS: 0 vivant sans rechute, 1 rechute et/ou dc

baz$STATEPFS<-NA
baz$STATEPFS[which(baz$STATE==1 | baz$RELAPSE==1)]<-1
baz$STATEPFS[which(baz$STATE==0 & baz$RELAPSE==0)]<-0

# STATEPFS: 0 vivant, 1 mort dans rechute

baz$STATETRM<-NA
baz$STATETRM[which(baz$STATE==1 & baz$RELAPSE==0)]<-1
baz$STATETRM[which(baz$STATE==0 | baz$RELAPSE==1)]<-0

# CRELAPSE: 0 vivant sans rechute, 1 rechute, 2 mort sans rechute

baz$CRELAPSE<-baz$RELAPSE
baz$CRELAPSE[which(baz$STATETRM==1 & baz$RELAPSE==0)]<-2

# CTOXI: 0 vivant sans rechute, 1 mort sans rechute, 2 rechute

baz$CTOXI<-NA
baz$CTOXI[which(baz$CRELAPSE==0)]<-0
baz$CTOXI[which(baz$CRELAPSE==2)]<-1
baz$CTOXI[which(baz$CRELAPSE==1)]<-2

###############################################################
# Acute GVHD ##################################################
###############################################################

# Variable source: AGVHGRMX et DATAGVH (maximum grade [witin 100 days after HSCT record])

baz$AGVHGRMX[which(baz$AGVHGRMX=="Not evaluated" | 
                   baz$AGVHGRMX=="unknown" )]<-NA

baz$AGVH<- ifelse(baz$AGVHGRMX=="No aGvHD present (Grade 0)",0,1)

baz$AGVH1<- baz$AGVH
baz$AGVH1[which(baz$AGVHGRMX=="Present, grade unknown")]<-NA

baz$AGVH2<-baz$AGVH1
baz$AGVH2[which(baz$AGVHGRMX=="Grade I")]<-0

baz$AGVH3<-baz$AGVH2
baz$AGVH3[which(baz$AGVHGRMX=="Grade II")]<-0

baz$BMTAGVH<-suppressWarnings(as.numeric(difftime(baz$DATAGVH,baz$DATBMT,units="days")+1))
if(compete_relapse==TRUE) baz$BMTAGVH[which(baz$AGVH==0)]<-baz$PFS[which(baz$AGVH==0)]
if(compete_relapse==FALSE) baz$BMTAGVH[which(baz$AGVH==0)]<-baz$SURV[which(baz$AGVH==0)]

baz$BMTAGV1<-NA
if(compete_relapse==TRUE) baz$BMTAGV1[which(baz$AGVH1==0)]<-baz$PFS[which(baz$AGVH1==0)]
if(compete_relapse==FALSE) baz$BMTAGV1[which(baz$AGVH1==0)]<-baz$SURV[which(baz$AGVH1==0)]
baz$BMTAGV1[which(baz$AGVH1==1)]<-baz$BMTAGVH[which(baz$AGVH1==1)]

baz$BMTAGV2<-NA
if(compete_relapse==TRUE) baz$BMTAGV2[which(baz$AGVH2==0)]<-baz$PFS[which(baz$AGVH2==0)]
if(compete_relapse==FALSE) baz$BMTAGV2[which(baz$AGVH2==0)]<-baz$SURV[which(baz$AGVH2==0)]
baz$BMTAGV2[which(baz$AGVH2==1)]<-baz$BMTAGVH[which(baz$AGVH2==1)]

baz$BMTAGV3<-NA
if(compete_relapse==TRUE) baz$BMTAGV3[which(baz$AGVH3==0)]<-baz$PFS[which(baz$AGVH3==0)]
if(compete_relapse==FALSE) baz$BMTAGV3[which(baz$AGVH3==0)]<-baz$SURV[which(baz$AGVH3==0)]
baz$BMTAGV3[which(baz$AGVH3==1)]<-baz$BMTAGVH[which(baz$AGVH3==1)]

baz$CAGVH<-baz$AGVH
if(compete_relapse==TRUE) baz$CAGVH[which(baz$AGVH==0 & (baz$STATE==1 | baz$RELAPSE==1))]<-2
if(compete_relapse==FALSE) baz$CAGVH[which(baz$AGVH==0 & (baz$STATE==1))]<-2

baz$CAGVH1<-baz$AGVH1
if(compete_relapse==TRUE) baz$CAGVH1[which(baz$AGVH1==0 & (baz$STATE==1 | baz$RELAPSE==1))]<-2
if(compete_relapse==FALSE) baz$CAGVH1[which(baz$AGVH1==0 & (baz$STATE==1))]<-2

baz$CAGVH2<-baz$AGVH2
if(compete_relapse==TRUE) baz$CAGVH2[which(baz$AGVH2==0 & (baz$STATE==1 | baz$RELAPSE==1))]<-2
if(compete_relapse==FALSE) baz$CAGVH2[which(baz$AGVH2==0 & (baz$STATE==1))]<-2

baz$CAGVH3<-baz$AGVH3
if(compete_relapse==TRUE) baz$CAGVH3[which(baz$AGVH3==0 & (baz$STATE==1 | baz$RELAPSE==1))]<-2
if(compete_relapse==FALSE) baz$CAGVH3[which(baz$AGVH3==0 & (baz$STATE==1))]<-2

###############################################################
# Chronic GVHD ################################################
###############################################################

if(class(baz$CGVHDD_1ST)!="Date")    suppressWarnings(baz$CGVHDD_1ST<- as.Date(as.numeric(as.character(baz$CGVHDD_1ST))/86400, origin = "1582-10-14"))
if(class(baz$CGVHDDAT_LIM)!="Date")  suppressWarnings(baz$CGVHDDAT_LIM<- as.Date(as.numeric(as.character(baz$CGVHDDAT_LIM))/86400, origin = "1582-10-14"))
if(class(baz$CGVHDDAT_EXT)!="Date")  suppressWarnings(baz$CGVHDDAT_EXT<- as.Date(as.numeric(as.character(baz$CGVHDDAT_EXT))/86400, origin = "1582-10-14"))

if(class(baz$CGVHDDAT_MILD)!="Date") suppressWarnings(baz$CGVHDDAT_MILD<- as.Date(as.numeric(as.character(baz$CGVHDDAT_MILD))/86400, origin = "1582-10-14"))
if(class(baz$CGVHDDAT_MOD)!="Date")  suppressWarnings(baz$CGVHDDAT_MOD<- as.Date(as.numeric(as.character(baz$CGVHDDAT_MOD))/86400, origin = "1582-10-14"))
if(class(baz$CGVHDDAT_SEV)!="Date")  suppressWarnings(baz$CGVHDDAT_SEV<- as.Date(as.numeric(as.character(baz$CGVHDDAT_SEV))/86400, origin = "1582-10-14"))

# Variables sources:
   # Evenement: GRAVHOSD_1ST (ancienne variable: GRAVHOSD)
   # Date du first event: CGVHDD_1ST (ancienne variable: DATCGVHD)
   # Grade du first event: VCGVHDG_1ST (EBMT) et MAXNIHSC_1ST (NIH)

baz$CGVH<-NA
baz$CGVH[which(baz$GRAVHOSD_1ST==1 | baz$GRAVHOSD_1ST=="No")]<-0
baz$CGVH[which(baz$GRAVHOSD_1ST=="Yes")]<-1

baz$CGVH[which(is.na(baz$CGVH) & baz$GRAVHOSD=="No")]<-0
baz$CGVH[which(is.na(baz$CGVH) & baz$GRAVHOSD=="Yes, first episode")]<-1
baz$CGVH[which(is.na(baz$CGVH) & baz$GRAVHOSD=="Yes, before this date")]<-1
baz$CGVH[which(is.na(baz$CGVH) & baz$GRAVHOSD=="Yes, recurrence")]<-1
baz$CGVH[which(is.na(baz$CGVH) & baz$GRAVHOSD=="Continuous since last reported episode")]<-1

# Variable source date selon grade EBMT: 
  # VCGVHDG_LIM (old: GRAVHOSD_LIM) ou VCGVHDG_EXT (old: GRAVHOSD_EXT)
  # CGVHDDAT_LIM ou CGVHDDAT_EXT

baz$CGVHGR<-NA
baz$CGVHGR[which(baz$CGVH==1 & baz$VCGVHDG_1ST=="Limited")]<-1
baz$CGVHGR[which(baz$CGVH==1 & baz$GRAVHOSD_LIM=="Yes")]<-1
baz$CGVHGR[which(baz$CGVH==1 & baz$VCGVHDG_LIM=="Limited")]<-1

baz$CGVHGR[which(baz$CGVH==1 & baz$VCGVHDG_1ST=="Extensive")]<-2
baz$CGVHGR[which(baz$CGVH==1 & baz$GRAVHOSD_EXT=="Yes")]<-2
baz$CGVHGR[which(baz$CGVH==1 & baz$VCGVHDG_EXT=="Extensive")]<-2

# Variable source date selon grade NIH:
  # MAXNIHSC_MILD (GRAVHOSD_MILD) ou MAXNIHSC_MOD (GRAVHOSD_MOD) ou MAXNIHSC_SEV (GRAVHOSD_SEV)
  # CGVHDDAT_MILD ou CGVHDDAT_MOD ou CGVHDDAT_SEV

baz$CGVHGRNIH<-NA
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$MAXNIHSC_1ST=="Mild")]<-1
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$GRAVHOSD_MILD=="Yes")]<-1
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$MAXNIHSC_MILD=="Mild")]<-1

baz$CGVHGRNIH[which(baz$CGVH==1 & baz$MAXNIHSC_1ST=="Moderate")]<-2
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$GRAVHOSD_MOD=="Yes")]<-2
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$MAXNIHSC_MOD=="Moderate")]<-2

baz$CGVHGRNIH[which(baz$CGVH==1 & baz$MAXNIHSC_1ST=="Severe")]<-3
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$GRAVHOSD_SEV=="Yes")]<-3
baz$CGVHGRNIH[which(baz$CGVH==1 & baz$MAXNIHSC_SEV=="Severe")]<-3

baz$CGVH[which(baz$CGVHGR >=1 | baz$CGVHGRNIH >=1 )]<-1 # cette ligne ne sert plus a rien (?)

# Extensive GVHD: Date difference de GVHD si premier episode non extensive
# Hypothese extensive GVHD: Moderate considere comme non extensive

baz$CGVHEXT<-NA
baz$CGVHEXT[which(baz$CGVH==0 | baz$CGVHGR==1 | baz$CGVHGRNIH==1 | baz$CGVHGRNIH==2)]<-0
baz$CGVHEXT[which(baz$CGVH==1 & (baz$CGVHGR==2 | baz$CGVHGRNIH==3))]<-1

# Delai transplant a first event de CGVH
baz$BMTCGVH<-NA
baz$BMTCGVH[which(baz$CGVH==1)]<-as.numeric(difftime(baz$CGVHDD_1ST,
                                                     baz$DATBMT,units="days")+1)[which(baz$CGVH==1)]

### Delai transplant a limited CGVH
baz$BMTCGVHLIM<-NA
baz$BMTCGVHLIM[which(baz$CGVH==1 & baz$CGVHGR==1)]<-
  as.numeric(difftime(baz$CGVHDDAT_LIM,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGR==1)]
baz$BMTCGVHLIM[which(baz$CGVH==1 & baz$CGVHGRNIH==1)]<-
  as.numeric(difftime(baz$CGVHDDAT_MILD,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGRNIH==1)]
baz$BMTCGVHLIM[which(baz$CGVH==1 & baz$CGVHGRNIH==2)]<-
  as.numeric(difftime(baz$CGVHDDAT_MOD,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGRNIH==2)]
### Extensive CGVHD
baz$BMTCGVHEXT<-NA
baz$BMTCGVHEXT[which(baz$CGVH==1 & baz$CGVHGR==2)]<-
  as.numeric(difftime(baz$CGVHDDAT_EXT,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGR==2)]
baz$BMTCGVHEXT[which(baz$CGVH==1 & baz$CGVHGRNIH==3)]<-
  as.numeric(difftime(baz$CGVHDDAT_SEV,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGRNIH==3)]

### Moderate CGVHD
baz$BMTCGVHMOD<-NA
baz$BMTCGVHMOD[which(baz$CGVH==1 & baz$CGVHGRNIH==2)]<-
  as.numeric(difftime(baz$CGVHDDAT_MOD,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGRNIH==2)]
baz$BMTCGVHMOD[which(baz$CGVH==1 & baz$CGVHGRNIH==2)]<-
  as.numeric(difftime(baz$CGVHDDAT_EXT,
                      baz$DATBMT,units="days")+1)[which(baz$CGVH==1 & baz$CGVHGRNIH==2)]

#### Si rechute en competition: Recodage des patients avec cgvhd apres relapse comme n'ayant pas eu de CGVHD ##

baz$CGVH_NO_IMPUTED<-baz$CGVH
baz$CGVHEXT_NO_IMPUTED<-baz$CGVHEXT

if(compete_relapse==TRUE) baz$CGVH[which(baz$CGVH==1 & 
                                         baz$RELAPSE==1 & 
                                         (baz$BMTCGVH > baz$PFS))]<-0
if(compete_relapse==TRUE) baz$CGVHEXT[which(baz$CGVHEXT==1 & 
                                            baz$RELAPSE==1 & 
                                            (baz$BMTCGVHEXT > baz$PFS))]<-0

#### Imputation des patients avec PFS <= 100 days avec donnees manquantes sur CGVH

if(compete_relapse==TRUE) print(paste(length(which(is.na(baz$CGVH) & (baz$PFS <= 100)))," CGVH imputed as 0"))
if(compete_relapse==TRUE) print(paste(length(which(is.na(baz$CGVHEXT) & (baz$PFS <= 100)))," CGVHEXT imputed as 0"))

if(compete_relapse==FALSE) print(paste(length(which(is.na(baz$CGVH) & (baz$SURV <= 100)))," CGVH imputed as 0"))
if(compete_relapse==FALSE) print(paste(length(which(is.na(baz$CGVHEXT) & (baz$SURV <= 100)))," CGVHEXT imputed as 0"))

if(compete_relapse==TRUE) baz$CGVH[which(is.na(baz$CGVH) & (baz$PFS <= 100))]<-0       
if(compete_relapse==TRUE) baz$CGVHEXT[which(is.na(baz$CGVHEXT) & (baz$PFS <= 100))]<-0

if(compete_relapse==FALSE) baz$CGVH[which(is.na(baz$CGVH) & (baz$SURV <= 100))]<-0       
if(compete_relapse==FALSE) baz$CGVHEXT[which(is.na(baz$CGVHEXT) & (baz$SURV <= 100))]<-0

if(compete_relapse==TRUE) baz$BMTCGVH[which(baz$CGVH==0)]<-baz$PFS[which(baz$CGVH==0)]
if(compete_relapse==TRUE) baz$BMTCGVHEXT[which(baz$CGVHEXT==0)]<-baz$PFS[which(baz$CGVHEXT==0)]

if(compete_relapse==FALSE) baz$BMTCGVH[which(baz$CGVH==0)]<-baz$SURV[which(baz$CGVH==0)] 
if(compete_relapse==FALSE) baz$BMTCGVHEXT[which(baz$CGVHEXT==0)]<-baz$SURV[which(baz$CGVHEXT==0)] 

### Imputation des extensives par delai CGVH (normalement pas possible)
baz$BMTCGVHEXT[which(baz$CGVHEXT==1 & is.na(baz$BMTCGVHEXT))]<-baz$BMTCGVH[which(baz$CGVHEXT==1 & is.na(baz$BMTCGVHEXT))]

baz$CGVHGR[which(baz$CGVH==0)]<-0
baz$CGVHGR[which(baz$CGVHEXT==1)]<-2

### Evenement en competition
baz$CCGVH<-baz$CGVH 
if(compete_relapse==TRUE)  baz$CCGVH[which(baz$CGVH ==0 & (baz$STATE==1 | baz$RELAPSE==1))]<-2
if(compete_relapse==FALSE) baz$CCGVH[which(baz$CGVH ==0 & baz$STATE==1)]<-2

baz$CCGVHEXT<-baz$CGVHEXT
if(compete_relapse==TRUE)  baz$CCGVHEXT[which(baz$CGVHEXT ==0 & (baz$STATE==1 | baz$RELAPSE==1))]<-2
if(compete_relapse==FALSE) baz$CCGVHEXT[which(baz$CGVHEXT ==0 & baz$STATE==1)]<-2

################################################################
# Engraftment. #################################################
################################################################

if(class(baz$DATCRGR2)!="Date") suppressWarnings(baz$DATCRGR2<- as.Date(as.numeric(as.character(baz$DATCRGR2))/86400, origin = "1582-10-14"))
if ("DLEUCEN" %in% names(baz)) if(class(baz$DLEUCEN)!="Date") suppressWarnings(baz$DLEUCEN<- as.Date(as.numeric(as.character(baz$DLEUCEN))/86400, origin = "1582-10-14"))
if(class(baz$DPLAT20)!="Date") suppressWarnings(baz$DPLAT20<- as.Date(as.numeric(as.character(baz$DPLAT20))/86400, origin = "1582-10-14"))
if(class(baz$DPLAT50)!="Date") suppressWarnings(baz$DPLAT50<- as.Date(as.numeric(as.character(baz$DPLAT50))/86400, origin = "1582-10-14"))

baz$DPOLY1<-as.numeric(difftime(baz$DATCRGR2,baz$DATBMT,units="days")+1)
if ("DLEUCEN" %in% names(baz)) baz$LEUC1<-as.numeric(difftime(baz$DLEUCEN,baz$DATBMT,units="days")+1)
baz$DPL20<-as.numeric(difftime(baz$DPLAT20,baz$DATBMT,units="days")+1)
baz$DPL50<-as.numeric(difftime(baz$DPLAT50,baz$DATBMT,units="days")+1)

# VPLAT20 --> Platelets >= 20 x 10^9/L reached?
baz$EPLAT20<-NA
baz$EPLAT20[which(baz$VPLAT20A=="No")]<-0
baz$EPLAT20[which(baz$VPLAT20A=="Yes" |baz$VPLAT20A=="Never below")]<-1 # Attention date pour never below

# VPLAT50 --> Platelets >= 50 x 10^9/L reached?
baz$EPLAT50<-NA
baz$EPLAT50[which(baz$VPLAT50A=="No")]<-0
baz$EPLAT50[which(baz$VPLAT50A=="Yes" |baz$VPLAT50A=="Never below")]<-1 # Attention date pour never below

# ENGRAFC variable construite
baz$ENGRAFC<-NA
baz$ENGRAFC[which(baz$ENGNEUT=="No")]<-1
baz$ENGRAFC[which(baz$ENGNEUT=="Yes" |baz$ENGNEUT=="Never below")]<-2
#baz$ENGRAFC[which(baz$EARGRLOSS=="Yes" |baz$ENGRAF=="Lost graft")]<-3

baz$ENGRAFC2<-NA
baz$ENGRAFC2[which(baz$ENGNEUT=="No")]<-1
baz$ENGRAFC2[which(baz$ENGNEUT=="Yes" |baz$ENGNEUT=="Never below")]<-2
baz$ENGRAFC2[which(baz$EARGRLOSS=="Yes" |baz$ENGRAF=="Lost graft")]<-3

# ENGNEUT --> Neutrophils >= 0.5 x 10^9/L reached?
# ENGRAF --> Engraftment (ANC recovery) ###### Old variable
# EARGRLOSS --> Early graft loss 

baz$LOSSGRAFT<-NA
baz$LOSSGRAFT[which(baz$ENGRAFC==2)]<-0
baz$LOSSGRAFT[which(baz$ENGRAFC==2 & (baz$EARGRLOSS=="Yes" |baz$ENGRAF=="Lost graft"))]<-1

if(class(baz$DLOSTEN)!="Date") suppressWarnings(baz$DLOSTEN<- as.Date(as.numeric(as.character(baz$DLOSTEN))/86400, origin = "1582-10-14"))

baz$DELGRAFTLOSS<-as.numeric(difftime(baz$DLOSTEN,baz$DATBMT,units="days")+1)

#comment graft failure no considered as no engraftment.
baz$EPOLY1<-NA
baz$EPOLY1[which(baz$ENGRAFC==1)]<-0
# baz$EPOLY1[which(baz$ENGRAFC==1 |baz$ENGRAFC==3)]<-0
baz$EPOLY1[which(baz$ENGRAFC==2|baz$ENGRAFC==3)]<-1

baz$DPOLY1[which(baz$EPOLY1==0)]<-baz$SURV[which(baz$EPOLY1==0)]
baz$DPOLY1[which(baz$ENGNEUT=="Never below")]<-1

baz$DPL20[which(baz$EPLAT20==0)]<-baz$SURV[which(baz$EPLAT20==0)]
baz$DPL20[which(baz$VPLAT20A=="Never below")]<-1

baz$DPL50[which(baz$EPLAT50==0)]<-baz$SURV[which(baz$EPLAT50==0)]
baz$DPL50[which(baz$VPLAT50A=="Never below")]<-1

baz$CPOLY1<-baz$EPOLY1
baz$CPOLY1[which(baz$EPOLY1==0 & baz$STATE==1)]<-2

baz$CPLAT50<-baz$EPLAT50
baz$CPLAT50[which(baz$EPLAT50==0 & baz$STATE==1)]<-2

baz$CPLAT20<-baz$EPLAT20
baz$CPLAT20[which(baz$EPLAT20==0 & baz$STATE==1)]<-2

### DPOLY100

baz$DPOLY100<-pmin(baz$DPOLY1,100)
baz$EPOLY100<-ifelse(baz$DPOLY1 <=100,baz$EPOLY1,0)
baz$CPOLY100<-ifelse(baz$DPOLY1 <=100,baz$CPOLY1,0)

### DPOLY30
baz$DPOLY30<-pmin(baz$DPOLY1,30)
baz$EPOLY30<-ifelse(baz$DPOLY1 <=30,baz$EPOLY1,0)
baz$CPOLY30<-ifelse(baz$DPOLY1 <=30,baz$CPOLY1,0)


baz$CONSECTX<-NA
baz$DELCONSECTX<-NA

for(i in 1: dim(baz)[1]){
  if(!is.na(baz$DTRANS1[i]) & baz$DTRANS1[i]==baz$DATBMT[i]){
    baz$CONSECTX[i]<-ifelse(is.na(baz$DTRANS2[i]),0,1)
    baz$DELCONSECTX[i]<-as.numeric(difftime(baz$DTRANS2[i],baz$DATBMT[i],units = "days"))
  }
  
  if(!is.na(baz$DTRANS2[i]) & baz$DTRANS2[i]==baz$DATBMT[i]){
    baz$CONSECTX[i]<-ifelse(is.na(baz$DTRANS3[i]),0,1)
    baz$DELCONSECTX[i]<-as.numeric(difftime(baz$DTRANS3[i],baz$DATBMT[i],units = "days"))
  }
  
  if(!is.na(baz$DTRANS3[i]) & baz$DTRANS3[i]==baz$DATBMT[i]){
    baz$CONSECTX[i]<-ifelse(is.na(baz$DTRANS4[i]),0,1)
    baz$DELCONSECTX[i]<-as.numeric(difftime(baz$DTRANS4[i],baz$DATBMT[i],units = "days"))
  }
  
  if(!is.na(baz$DTRANS4[i]) & baz$DTRANS4[i]==baz$DATBMT[i]){
    baz$CONSECTX[i]<-ifelse(is.na(baz$DTRANS5[i]),0,1)
    baz$DELCONSECTX[i]<-as.numeric(difftime(baz$DTRANS5[i],baz$DATBMT[i],units = "days"))
  }
  
  if(!is.na(baz$DTRANS5[i]) & baz$DTRANS5[i]==baz$DATBMT[i]){
    baz$CONSECTX[i]<-ifelse(is.na(baz$DTRANS6[i]),0,1)
    baz$DELCONSECTX[i]<-as.numeric(difftime(baz$DTRANS6[i],baz$DATBMT[i],units = "days"))
  }
}


##### Competition for consecutive TX #########
if(compete_consecTX_engneut==TRUE){
  
  baz$CPOLY1<-baz$EPOLY1
  baz$CPOLY1[which(baz$EPOLY1==0 & baz$STATE==1)]<-2
  baz$CPOLY1[which(baz$EPOLY1==0 & baz$CONSECTX==1)]<-2
  baz$CPOLY1[which(baz$EPOLY1==1 & baz$CONSECTX==1 & (baz$DPOLY1 >= baz$DELCONSECTX))]<-2
  
  baz$DPOLY1[which(baz$EPOLY1==0 & baz$CONSECTX==1)]<-baz$DELCONSECTX[which(baz$EPOLY1==0 & baz$CONSECTX==1)]
  baz$DPOLY1[which(baz$EPOLY1==1 & baz$CONSECTX==1 & (baz$DPOLY1 >= baz$DELCONSECTX))]<-baz$DELCONSECTX[which(baz$EPOLY1==1 & baz$CONSECTX==1 & (baz$DPOLY1 >= baz$DELCONSECTX))]
  
  baz$CPLAT50<-baz$EPLAT50
  baz$CPLAT50[which(baz$CPLAT50==0 & baz$STATE==1)]<-2
  baz$CPLAT50[which(baz$CPLAT50==0 & baz$CONSECTX==1)]<-2
  baz$CPLAT50[which(baz$CPLAT50==1 & baz$CONSECTX==1 & (baz$DPL50 >= baz$DELCONSECTX))]<-2
  
  baz$DPL50[which(baz$EPLAT50==0 & baz$CONSECTX==1)]<-baz$DELCONSECTX[which(baz$EPLAT50==0 & baz$CONSECTX==1)]
  baz$DPL50[which(baz$EPLAT50==1 & baz$CONSECTX==1 & (baz$DPL50 >= baz$DELCONSECTX))]<-baz$DELCONSECTX[which(baz$EPLAT50==1 & baz$CONSECTX==1 & (baz$DPL50 >= baz$DELCONSECTX))]
  
  
  baz$CPLAT20<-baz$EPLAT20
  baz$CPLAT20[which(baz$EPLAT20==0 & baz$STATE==1)]<-2
  baz$CPLAT20[which(baz$EPLAT20==0 & baz$CONSECTX==1)]<-2
  baz$CPLAT20[which(baz$EPLAT20==1 & baz$CONSECTX==1 & (baz$DPL20 >= baz$DELCONSECTX))]<-2
  
  baz$DPL20[which(baz$EPLAT20==0 & baz$CONSECTX==1)]<-baz$DELCONSECTX[which(baz$EPLAT20==0 & baz$CONSECTX==1)]
  baz$DPL20[which(baz$EPLAT20==1 & baz$CONSECTX==1 & (baz$DPL20 >= baz$DELCONSECTX))]<-baz$DELCONSECTX[which(baz$EPLAT20==1 & baz$CONSECTX==1 & (baz$DPL20 >= baz$DELCONSECTX))]
  
}

### DPOLY100

baz$DPOLY100<-pmin(baz$DPOLY1,100)
baz$EPOLY100<-ifelse(baz$DPOLY1 <=100,baz$EPOLY1,0)
baz$CPOLY100<-ifelse(baz$DPOLY1 <=100,baz$CPOLY1,0)

### DPOLY30
baz$DPOLY30<-pmin(baz$DPOLY1,30)
baz$EPOLY30<-ifelse(baz$DPOLY1 <=30,baz$EPOLY1,0)
baz$CPOLY30<-ifelse(baz$DPOLY1 <=30,baz$CPOLY1,0)


baz$SURVY<-baz$SURV/365.25
baz$SURVM<-baz$SURV/30.425
baz$PFSY<-baz$PFS/365.25
baz$PFSM<-baz$PFS/30.425

baz$BMTCGVHM<-baz$BMTCGVH/30.425
baz$BMTCGVHY<-baz$BMTCGVH/365.25

baz$BMTCGVHEXTM<-baz$BMTCGVHEXT/30.425
baz$BMTCGVHEXTY<-baz$BMTCGVHEXT/365.25

if(dim(baz[which(baz$DATLASTFU < baz$DATRELAPSE | baz$DATLASTFU < baz$DATAGVH | baz$DATLASTFU < baz$CGVHDD_1ST), c("IDAA","ID")])[1] > 0){

toshow<-c("IDAA","ID","IDPROMISE","CENTRE","DATBMT","DATLASTFU","DATLASTALIVE","DATRELAPSE","LDATNOREL","DATAGVH","CGVHDD_1ST")
toshow2<-toshow[which(toshow %in% names(baz))]
  
print(paste("Nbr patient avec dates incohérentes :",
dim(baz[which(baz$DATLASTFU < baz$DATRELAPSE | baz$DATLASTFU < baz$DATAGVH | baz$DATLASTFU < baz$CGVHDD_1ST),toshow2])[1]))

print(baz[which(baz$DATLASTFU < baz$DATRELAPSE | baz$DATLASTFU < baz$DATAGVH | baz$DATLASTFU < baz$CGVHDD_1ST),toshow2])
           }

return(baz)
}


#########################################################
############## Convertion des dates #####################
#########################################################

convertdatespss<-function(data=baz,varplus=NULL){
  baz<-data
  names(baz)<-toupper(names(baz))
  
  date_comp_inf<-c("DATVOD", "DATNOVOD",
                   "DATASSCOMP.1","DBEGCOM.1","DATASSCOMP.2","DBEGCOM.2", "DATASSCOMP.3","DBEGCOM.3",
                   "DATASSCOMP.4","DBEGCOM.4","DATASSCOMP.5","DBEGCOM.5", "DATASSCOMP.6","DBEGCOM.6",
                   "DATASSCOMP.7","DBEGCOM.7","DATASSCOMP.8","DBEGCOM.8", "DATASSCOMP.9","DBEGCOM.9",
                   "DATASSCOMP.10","DBEGCOM.10","DATASSCOMP.11","DBEGCOM.11", "DATASSCOMP.12","DBEGCOM.12",
                   "DATASSCOMP.13","DBEGCOM.13","DATASSCOMP.14","DBEGCOM.14", "DATASSCOMP.15","DBEGCOM.15",
                   "DATASSCOMP.16","DBEGCOM.16","DATASSCOMP.17","DBEGCOM.17", "DATASSCOMP.18","DBEGCOM.18",
                   "DATASSCOMP.19","DBEGCOM.19","DATASSCOMP.20","DBEGCOM.20", "DATASSCOMP.21","DBEGCOM.21",
                   "DATASSCOMP.22","DBEGCOM.22","DATASSCOMP.23","DBEGCOM.23", "DATASSCOMP.24","DBEGCOM.24",
                   "DATASSCOMP.25","DBEGCOM.25","DATASSCOMP.26","DBEGCOM.26", "DATASSCOMP.27","DBEGCOM.27",
                   "DATASSCOMP.28","DBEGCOM.28","DATASSCOMP.29","DBEGCOM.29", "DATASSCOMP.30","DBEGCOM.30",
                   "DATASSCOMP.31","DBEGCOM.31","DATASSCOMP.32","DBEGCOM.32", "DATASSCOMP.33","DBEGCOM.33",
                   "DATASSCOMP.34","DBEGCOM.34",
                   "DATASSINF.1","BEGINFEP.1", "ENDINFEP.1", "DATASSINF.2","BEGINFEP.2", "ENDINFEP.2",
                   "DATASSINF.3","BEGINFEP.3","ENDINFEP.3", "DATASSINF.4", "BEGINFEP.4", "ENDINFEP.4",
                   "DATASSINF.5","BEGINFEP.5", "ENDINFEP.5","DATASSINF.6", "BEGINFEP.6", "ENDINFEP.6",
                   "DATASSINF.7","BEGINFEP.7", "ENDINFEP.7","DATASSINF.8","BEGINFEP.8","ENDINFEP.8",
                   "DATASSINF.9","BEGINFEP.9", "ENDINFEP.9", "DATASSINF.10","BEGINFEP.10", "ENDINFEP.10",
                   "DATASSINF.11", "BEGINFEP.11", "ENDINFEP.11", "DATASSINF.12","BEGINFEP.12", "ENDINFEP.12",
                   "DATASSINF.13","BEGINFEP.13", "ENDINFEP.13","DATASSINF.14", "BEGINFEP.14", "ENDINFEP.14",
                   "DATASSINF.15","BEGINFEP.15", "ENDINFEP.15", "DATASSINF.16","BEGINFEP.16", "ENDINFEP.16",
                   "DATASSINF.17","BEGINFEP.17", "ENDINFEP.17", "DATASSINF.18","BEGINFEP.18", "ENDINFEP.18",
                   "DATASSINF.19","BEGINFEP.19", "ENDINFEP.19","DATASSINF.20","BEGINFEP.20", "ENDINFEP.20",
                   "DATASSINF.21","BEGINFEP.21", "ENDINFEP.21", "DATASSINF.22","BEGINFEP.22", "ENDINFEP.22",
                   "DATASSINF.23","BEGINFEP.23", "ENDINFEP.23", "DATASSINF.24","BEGINFEP.24", "ENDINFEP.24",
                   "DATASSINF.25","BEGINFEP.25", "ENDINFEP.25","DATASSINF.26","BEGINFEP.26", "ENDINFEP.26",
                   "DATASSINF.27","BEGINFEP.27", "ENDINFEP.27", "DATASSINF.28","BEGINFEP.28", "ENDINFEP.28",
                   "DATASSINF.29","BEGINFEP.29", "ENDINFEP.29", "DATASSINF.30","BEGINFEP.30", "ENDINFEP.30",
                   "DATASSINF.31","BEGINFEP.31", "ENDINFEP.31","DATASSINF.32","BEGINFEP.32", "ENDINFEP.32",
                   "DATASSINF.33","BEGINFEP.33", "ENDINFEP.33", "DATASSINF.34","BEGINFEP.34", "ENDINFEP.34",
                   "DATASSINF.35","BEGINFEP.35", "ENDINFEP.35", "DATASSINF.36","BEGINFEP.36", "ENDINFEP.36",
                   "DATASSINF.37","BEGINFEP.37", "ENDINFEP.37","DATASSINF.38","BEGINFEP.38", "ENDINFEP.38",
                   "DATASSINF.39","BEGINFEP.39", "ENDINFEP.39", "DATASSINF.40","BEGINFEP.40", "ENDINFEP.40",
                   "DATASSINF.41","BEGINFEP.41", "ENDINFEP.41", "DATASSINF.42","BEGINFEP.42", "ENDINFEP.42",
                   "DATASSINF.43","BEGINFEP.43", "ENDINFEP.43","DATASSINF.44","BEGINFEP.44", "ENDINFEP.44",
                   "DATASSINF.45","BEGINFEP.45", "ENDINFEP.45", "DATASSINF.46","BEGINFEP.46", "ENDINFEP.46",
                   "DATASSINF.47","BEGINFEP.47", "ENDINFEP.47", "DATASSINF.48","BEGINFEP.48", "ENDINFEP.48",
                   "DATASSINF.49","BEGINFEP.49", "ENDINFEP.49","DATASSINF.50","BEGINFEP.50", "ENDINFEP.50",
                   "DATASSINF.51","BEGINFEP.51", "ENDINFEP.51", "DATASSINF.52","BEGINFEP.52", "ENDINFEP.52",
                   "DATASSINF.53", "BEGINFEP.53", "ENDINFEP.53", "DATASSINF.54","BEGINFEP.54", "ENDINFEP.54",
                   "DATASSINF.55","BEGINFEP.55", "ENDINFEP.55","DATASSINF.56", "BEGINFEP.56", "ENDINFEP.56", 
                   "DATASSINF.57","BEGINFEP.57", "ENDINFEP.57", "DATASSINF.58","BEGINFEP.58", "ENDINFEP.58",
                   "DATASSINF.59","BEGINFEP.59", "ENDINFEP.59", "DATASSINF.60","BEGINFEP.60", "ENDINFEP.60",
                   "DATASSINF.61","BEGINFEP.61", "ENDINFEP.61","DATASSINF.62","BEGINFEP.62", "ENDINFEP.62",
                   "DATASSINF.63","BEGINFEP.63", "ENDINFEP.63", "DATASSINF.64","BEGINFEP.64", "ENDINFEP.64",
                   "DATASSINF.65","BEGINFEP.65", "ENDINFEP.65", "DATASSINF.66","BEGINFEP.66", "ENDINFEP.66",
                   "DATASSINF.67","BEGINFEP.67", "ENDINFEP.67","DATASSINF.68","BEGINFEP.68", "ENDINFEP.68",
                   "DATASSINF.69","BEGINFEP.69", "ENDINFEP.69", "DATASSINF.70", "BEGINFEP.70", "ENDINFEP.70",
                   "DATASSINF.71","BEGINFEP.71", "ENDINFEP.71", "DATASSINF.72","BEGINFEP.72", "ENDINFEP.72",
                   "DATASSINF.73","BEGINFEP.73", "ENDINFEP.73","DATASSINF.74","BEGINFEP.74", "ENDINFEP.74",
                   "DATASSINF.75","BEGINFEP.75", "ENDINFEP.75", "DATASSINF.76","BEGINFEP.76", "ENDINFEP.76",
                   "DATASSINF.77","BEGINFEP.77", "ENDINFEP.77", "DATASSINF.78","BEGINFEP.78", "ENDINFEP.78", 
                   "DATASSINF.79","BEGINFEP.79", "ENDINFEP.79")
  
  datedate<-c("DATPATBD_MEDC","DATPATBD","DATBMT","TMADIAGDATE_MEDC","ASSDGLOSS.1","ASSDGLOSS.2","ASSDGLOSS.3",
              "PTCYDATE_MEDC","PTCYDATE2_MEDC","DATAGVH_MEDC",
              "DATAGVH","DATLASTFU_MEDC","DATLASTFU","TMARESOLDATE_MEDC",
              "DATEINFECTION_MEDC","DATEFUNGAL_MEDC",
              "INFECTIONDATE_MEDC","INFECTIONDATE2_MEDC",
              "IDAABB","DATRELAPSE_MEDC","PRECHSCTDAT_MEDC",
              "DATBMTPRE","DATAGVHPRE","SUBHSCTDATPRE",
              "SUBHSCTDAT2_MEDCPRE","DATCGVHD","CGVHDD_1ST","CGVHDDAT_LIM","CGVHDDAT_MILD","CGVHDDAT_EXT",
              "CGVHDDAT_SEV","CGVHDDAT_MOD","DATCRGR2","DLEUCEN","DPLAT20","DPLAT50","DLOSTEN",
              "DTRANS1","DTRANS2","DTRANS3","DTRANS4","DTRANS5","DTRANS6","DTRANSAML","DCHIM",
              "DATLASTALIVE","EBVIDAT_EP","PTLDDATE_EP","DATRESP","DATRELAPSE",
              "DATASS_1STYFUHEMOG",
              "DATASS_LNOFUHEMOG", 
              "DATASS_FUHEMOG.1",
              "DATASS_FUHEMOG.2", 
              "DATASS_FUHEMOG.3",
              "DATASS_FUHEMOG.4",
              "DATASS_FUHEMOG.5", 
              "DATASS_FUHEMOG.6", 
              "DATASS_FUHEMOG.7", 
              "DATASS_FUHEMOG.8",
              "DATASS_FUHEMOG.9",
              "DATASS_FUHEMOG.10",
              "DATASS_FUHEMOG.11",
              "DATASS_FUHEMOG.12",
              "DATASS_FUHEMOG.13",
              "DATASS_FUHEMOG.14",
              "DATASS_FUHEMOG.15", 
              "DATASS_FUHEMOG.16",
              "DATASS_FUHEMOG.17",
              "DATASS_FUHEMOG.18",
              "DATASS_FUHEMOG.19",
              "DATASS_FUHEMOG.20",
              "DATASS_FUHEMOG.21",
              "DATASS_FUHEMOG.22",
              "DATASS_FUHEMOG.23", 
              "DATASS_FUHEMOG.24", 
              "DATASS_FUHEMOG.25", 
              "DATASS_FUHEMOG.26",
              "DATASS_FUHEMOG.27",
              "DATASS_FUHEMOG.28",
              "DATASS_FUHEMOG.29",
              "DATASS_FUHEMOG.30",
              "DATASS_FUHEMOG.31","IDAABB_FIRST","IDAABB_LASTPREV","IDAABB_MAIN",
              "LDATNOSECMALIG","LDATNOADDTREAT","DATADCEL_PRDLI","IDAABC_PRDLI","DATRELAPSE_BEFDLI","DATRELPRE","VPREVDOG","SUBHSCTDAT",
              "DATDLI1","DATDLI2","DATDLI3","DATDLI4","DATNODLI","DATVOD","DATNOVOD","DATSECMALIGN_POSTHSCT","LDATNOADCELLT",
              "DATDONBD1","DATDONBD2","DATCRPRE","DATCR1P","DATREL1P","DATRELPTX_BIS",
              "DATLINE1","DATGCSF","DATE_LINEPOSTH1","DATE_LINEPOSTH2","DATE_LINEPOSTH3","VPREVDOG","IDAABB","DATRESP",
              "DATLRLPS","DCHIM","DATVRELLEUK","DATVRELLEU3","DATVRELLEU5","LFUDISCLID","LFUDISCYTD","LFUDISMOLD","VSIDERR",
              "DCYTREL","DMOLREL","DATCR1P","DATREL1P","DATCRPRE","DATRELPTX_BIS",
              "DATLINE1","DATLINE2","DATLINE2","DATLINE3","DATDLI1","DATDLI2","DATTRAN","MOBDATE","MOBDATE1","MOBDATE2","MOBDATE3","DLATE_GRAFTL",
              "DLOSTEN.1","DLOSTEN.2","DLOSTEN.3","DLOSTEN.4","DATDIAG_DIAGCT","ICFCTDATE",
              "IDAABB_CTDIAG","DTRANS1_HISTORY","DTRANS2_HISTORY","DTRANS3_HISTORY","DTRANS4_HISTORY","DTRANS5_HISTORY","DTRANS6_HISTORY","DATADCEL_CT",
              "DATPREVCINF_CT","IDAABE_CT","CIUCOLLDAT_UCT1","IDAABCCQ_ECT1","DAT_PREVDIAG1","DAT_PREVDIAG2","DAT_CT1","DAT_PREVAUTO1_CT1","MOBDATE_PREVAUTO1_CT1",
              "DATCRGR2_PREVAUTO1_CT1","DPLAT50_PREVAUTO1_CT1","DATREL_1_PREVAUTO1_CT1","DATRESP_PREVAUTO1_CT1","DAT_PREVAUTO2_CT1","DAT_PREVAUTO2_CT1","MOBDATE_PREVAUTO2_CT1",
              "DATCRGR2_PREVAUTO2_CT1","DPLAT50_PREVAUTO2_CT1","DATREL_1_PREVAUTO2_CT1","DATRESP_PREVAUTO2_CT1","DAT_PREVAUTO3_CT1","MOBDATE_PREVAUTO3_CT1",
              "DATCRGR2_PREVAUTO3_CT1","DPLAT50_PREVAUTO3_CT1","DATREL_1_PREVAUTO3_CT1","DATRESP_PREVAUTO3_CT1","DAT_PREVALLO1_CT1","DATCRGR2_PREVALLO1_CT1",
              "DPLAT50_PREVALLO1_CT1","DLOSTEN_PREVALLO1_CT1","DATAGVH_PREVALLO1_CT1","DATCGVHD_PREVALLO1_CT1","DATREL_1_PREVALLO1_CT1","DATRESP_PREVALLO1_CT1",
              "DAT_PREVALLO2_CT1","DATCRGR2_PREVALLO2_CT1",
              "DPLAT50_PREVALLO2_CT1","DLOSTEN_PREVALLO2_CT1","DATAGVH_PREVALLO2_CT1","DATCGVHD_PREVALLO2_CT1","DATREL_1_PREVALLO2_CT1","DATRESP_PREVALLO2_CT1",
              "DAT_PREVALLO3_CT1","DATCRGR2_PREVALLO3_CT1",
              "DPLAT50_PREVALLO3_CT1","DLOSTEN_PREVALLO3_CT1","DATAGVH_PREVALLO3_CT1","DATCGVHD_PREVALLO3_CT1","DATREL_1_PREVALLO3_CT1","DATRESP_PREVALLO3_CT1",
              "DATLASTTX_CT1","DAT_CT1_PRE1","DATRESP_CT1_PRE1","START1_CT1_PRE1","TRETSTAR1_CT1_PRE1","END1_CT1_PRE1","START2_CT1_PRE1","TRETSTAR2_CT1_PRE1",
              "END2_CT1_PRE1","START3_CT1_PRE1","TRETSTAR3_CT1_PRE1","END3_CT1_PRE1","START4_CT1_PRE1","TRETSTAR4_CT1_PRE1","END4_CT1_PRE1",
              "START5_CT1_PRE1","TRETSTAR5_CT1_PRE1","END5_CT1_PRE1",
              "DAT_CT1_PRE2","DATRESP_CT1_PRE2","START1_CT1_PRE2","TRETSTAR1_CT1_PRE2","END1_CT1_PRE2",
              "START2_CT1_PRE2","TRETSTAR2_CT1_PRE2","END2_CT1_PRE2",
              "START3_CT1_PRE2","TRETSTAR3_CT1_PRE2","END3_CT1_PRE2",
              "DAT_CT1_PRE3","DAT_CT2","DAT_CT3","DAT_CT4",
              "DAT_CT1_PRE4",
              "DATCYTGEN_CT1_PRES1","DATIMMPHENO_CT1_PRES1",
              "DATCRGR2_CT1","DNOENGR_CT1","DPLAT20_CT1","DPLAT50_CT1","DLASTPLT_CT1","DATHIDEREG_CT1",
              "CIUCOLLDAT_CELLINFUNIT1_CT1","DATPREVCINF_CT1","DATNEAR_D100_CT1","DATRESP_D100_CT1","DATRESP_6M_CT1","TRETSTAR1_CT1_POST1","END1_CT1_POST1",
              "DATNEXTTX_CT1","DATAGVH_D100_CT1","CXCRS_DAT_D100_CT1","PRSTSTDATE_D100_CT1","DATNEAR_6M_CT1"
)
  
  datepool<-c(varplus,date_comp_inf,datedate)
  datepool2<-datepool[which(datepool %in% names(baz))]
  
  for(i in datepool2){
    if(class(baz[,i])!="Date") suppressWarnings(baz[,i] <- as.Date(as.numeric(as.character(baz[,i]))/86400, origin = "1582-10-14"))
   baz[which(baz[,i]=="1809-12-31"),i]<-NA
   baz[which(baz[,i]=="1809-12-31"),i]<-NA
  }
  return(baz)
}


censure_outcomes<-function(baz,t){
  baz$STATE<-ifelse(baz$SURV > t*365.25,0,baz$STATE)
  baz$STATEPFS<-ifelse(baz$PFS > t*365.25,0,baz$STATEPFS)
  baz$STATETRM<-ifelse(baz$PFS > t*365.25,0,baz$STATETRM)
  baz$GRFS<-ifelse(baz$BMTGRFS > t*365.25,0,baz$GRFS)
  
  baz$RELAPSE<-ifelse(baz$PFS > t*365.25,0,baz$RELAPSE)
  baz$CRELAPSE<-ifelse(baz$PFS > t*365.25,0,baz$CRELAPSE)
  baz$CTOXI<-ifelse(baz$PFS > t*365.25,0,baz$CTOXI)
  baz$CGVH<-ifelse(baz$SURV > t*365.25,0,baz$CGVH)
  baz$CCGVH<-ifelse(baz$BMTCGVH > t*365.25,0,baz$CCGVH)
  baz$CGVHEXT<-ifelse(baz$SURV > t*365.25,0,baz$CGVHEXT)
  baz$CCGVHEXT<-ifelse(baz$BMTCGVHEXT > t*365.25,0,baz$CCGVHEXT)
  baz$AGVH2<-ifelse(baz$BMTAGV2 > t*365.25,0,baz$AGVH2)
  baz$CAGVH2<-ifelse(baz$BMTAGV2 > t*365.25,0,baz$CAGVH2)
  baz$AGVH3<-ifelse(baz$BMTAGV3 > t*365.25,0,baz$AGVH3)
  baz$CAGVH3<-ifelse(baz$BMTAGV3 > t*365.25,0,baz$CAGVH3)
  baz$CSECOND<-ifelse(baz$BMTSECOND > t*365.25,0,baz$CSECOND)
  
  baz$SURV<-pmin(baz$SURV,t*365.25)
  baz$SURVM<-pmin(baz$SURVM,t*12)
  baz$SURVY<-pmin(baz$SURVY,t)
  
  baz$PFS<-pmin(baz$PFS,t*365.25)
  baz$PFSM<-pmin(baz$PFSM,t*12)
  baz$PFSY<-pmin(baz$PFSY,t)
  
  baz$BMTAGVH<-pmin(baz$BMTAGVH,t*365.25)
  baz$BMTAGV1<-pmin(baz$BMTAGV1,t*365.25)
  baz$BMTAGV2<-pmin(baz$BMTAGV2,t*365.25)
  baz$BMTAGV3<-pmin(baz$BMTAGV3,t*365.25)
  
  baz$BMTCGVH<-pmin(baz$BMTCGVH,t*365.25)
  baz$BMTCGVHM<-pmin(baz$BMTCGVHM,t*12)
  baz$BMTCGVHY<-pmin(baz$BMTCGVHY,t)
  
  baz$BMTCGVHEXT<-pmin(baz$BMTCGVHEXT,t*365.25)
  baz$BMTCGVHEXTM<-pmin(baz$BMTCGVHEXTM,t*12)
  baz$BMTCGVHEXTY<-pmin(baz$BMTCGVHEXTY,t)
  
  baz$BMTSECOND<-pmin(baz$BMTSECOND,t*365.25)
  
  
  baz$BMTGRFS<-pmin(baz$BMTGRFS,t*365.25)
#  baz$BMTGRFSM<-pmin(baz$BMTGRFSM,t*12)
  baz$BMTGRFSY<-pmin(baz$BMTGRFSY,t)
  

  return(baz)
}

secondTX<-function(baz){

  baz$CSECOND<-0
  baz$CSECOND<-baz$CONSECTX
  baz$CSECOND[which(baz$CONSECTX==0)]<-baz$STATE[which(baz$CONSECTX==0)]*2
  
  baz$BMTSECOND<-as.numeric(baz$DELCONSECTX)
  baz$BMTSECOND[which(baz$CONSECTX==0)]<-baz$SURV[which(baz$CONSECTX==0)]

  return(baz)
  }




###############################################################################################
###############################################################################################
####################### Cause of Death  #######################################################
###############################################################################################
###############################################################################################


dcfunction<-function(data=NULL,compete_relapse=TRUE){
  baz<-data
  baz$DCREL<-NA
  baz$DCREL[which(baz$VCAUSDTH=="Relapse or progression of original disease")]<-1
  baz$DCSECOND<-NA
  baz$DCSECOND[which(baz$VCAUSDTH=="Secondary malignancy")]<-1
  baz$DCSECOND<-NA
  baz$DCSECOND[which(baz$VCAUSDTH=="Secondary malignancy")]<-1
  baz$DCHSCT<-NA
  baz$DCHSCT[which(baz$VCAUSDTH=="HSCT related")]<-1
  baz$DCGVH<-NA
  baz$DCGVH[which(baz$VCSDTGVH=="Yes")]<-1
  baz$DCIP<-NA
  baz$DCIP[which(baz$VCSDTINP=="Yes")]<-1
  baz$DCPULM<-NA
  baz$DCPULM[which(baz$VCSDTPTX=="Yes")]<-1
  baz$DCINF<-NA
  baz$DCINF[which(baz$VCSDTINF=="Yes")]<-1
  baz$DCBACT<-NA
  baz$DCBACT[which(baz$VCSDTBAC=="Yes")]<-1
  baz$DCVIR<-NA
  baz$DCVIR[which(baz$VCSDTVIR=="Yes")]<-1
  baz$DCFUNG<-NA
  baz$DCFUNG[which(baz$VCSDTFUN=="Yes")]<-1
  baz$DCPARA<-NA
  baz$DCPARA[which(baz$VCSDTPAR=="Yes")]<-1
  baz$DCREJ<-NA
  baz$DCREJ[which(baz$VCSDTREJ=="Yes")]<-1
  baz$DCVOD<-NA
  baz$DCVOD[which(baz$VCSDTVOD=="Yes")]<-1
  baz$DCHEMO<-NA
  baz$DCHEMO[which(baz$VCSDTHMR=="Yes")]<-1
  baz$DCCARD<-NA
  baz$DCCARD[which(baz$VCSDTCTX=="Yes")]<-1
  baz$DCLYMPH<-NA
  baz$DCLYMPH[which(baz$VCSDTEBV=="Yes")]<-1
  baz$DCCNS<-NA
  baz$DCCNS[which(baz$VCSDTCNS=="Yes")]<-1
  baz$DCGI<-NA
  baz$DCGI[which(baz$VCSDTGIT=="Yes")]<-1
  baz$DCMOF<-NA
  baz$DCMOF[which(baz$VCSDTMOF=="Yes")]<-1
  baz$DCREIN<-NA
  baz$DCREIN[which(baz$VCSDTREN=="Yes")]<-1
  baz$DCSKIN<-NA
  baz$DCSKIN[which(baz$VCSDTSKI=="Yes")]<-1
  
  # Myriam Labopin based
  
  baz$DC<-NA
  baz$DC[which(baz$DCREL==1)]<-'Original disease' # code 8
  baz$DC[which(baz$DCPULM==1 |baz$DCLYMPH==1 |baz$DCCNS==1 |baz$DCGI==1 |
                 baz$DCMOF==1 |baz$DCREIN==1 |baz$DCSKIN==1)]<-'other transp related' # code 10
  baz$DC[which(baz$DCCARD==1)]<-'Cardiac toxicity' # code 1
  baz$DC[which(baz$DCHEMO==1)]<-'haemorhage' # code 2
  baz$DC[which(baz$DCREJ==1)]<-'Failure/Rejection' # 3
  baz$DC[which(baz$DCINF==1 |baz$DCBACT==1 |baz$DCVIR==1 |baz$DCFUNG==1 |
                 baz$DCPARA==1)]<-'Infection' # 5
  baz$DC[which(baz$DCIP==1)]<-'IP' # code 6
  baz$DC[which(baz$DCGVH==1)]<-'GVHD'# code 7
  baz$DC[which(baz$DCVOD==1)]<-'VOD'# code 4
  baz$DC[which(baz$DCSECOND==1)]<-'second malignancy' # code 9
  if(compete_relapse==TRUE) baz$DC[which(is.na(baz$DC) & baz$STATE==1 & baz$RELAPSE==1)]<-'Original disease'
  baz$DC[which(is.na(baz$DC) & baz$STATE==1 & baz$VCAUSDTH=="HSCT related")]<-'other transp related'
  baz$DC[which(baz$STATE==0)]<-"Alive"
  baz$DC<-as.factor(baz$DC)
  
  baz$DC_TOT<-paste(ifelse(baz$DCREL==1 | baz$DC=="Original disease",'OrigDis',""),
                    ifelse(baz$DCSECOND==1,'SecMal',""),
                    ifelse((baz$DCPULM==1 |baz$DCLYMPH==1 |baz$DCCNS==1 |baz$DCGI==1 |
                              baz$DCMOF==1 |baz$DCREIN==1 |baz$DCSKIN==1) | 
                             (baz$STATE==1 & baz$VCAUSDTH=="HSCT related"),'OtherTxRel',""),
                    ifelse(baz$DCCARD==1,'CardTox',""),
                    ifelse(baz$DCHEMO==1,'Haemo',""),
                    ifelse(baz$DCREJ==1,'FailRej',""),
                    ifelse(baz$DCINF==1 |baz$DCBACT==1 |
                             baz$DCVIR==1 |baz$DCFUNG==1 |
                             baz$DCPARA==1,'Inf',""),
                    ifelse(baz$DCVOD==1,'VOD',""),
                    ifelse(baz$DCIP==1,'IP',""),
                    ifelse(baz$DCGVH==1,'GVHD',""),
                    ifelse(baz$STATE==0,'Alive',""),"stop",sep="_")
  baz$DC_TOT[which(baz$DC_TOT=="NA_NA_NA_NA_NA_NA_NA_NA_NA_NA__stop")]<-NA
  baz$DC_TOT<-gsub("NA_","",baz$DC_TOT)
  baz$DC_TOT<-gsub("__stop","",baz$DC_TOT)
  baz$DC_TOT<-gsub("_stop","",baz$DC_TOT)
  baz$DC_TOT<-as.factor(baz$DC_TOT)
  
  baz$DC_TOT_SNA<-as.character(baz$DC_TOT)
  baz$DC_TOT_SNA[which(baz$DC_TOT=="Alive")]<-NA
  baz$DC_TOT_SNA<-factor(baz$DC_TOT_SNA)
  
  baz$DC_TOT2<-as.character(baz$DC_TOT_SNA)
  baz$DC_TOT2<-gsub("OtherTxRel_","",baz$DC_TOT2)
  baz$DC_TOT2<-gsub("Inf_","",baz$DC_TOT2)
  baz$DC_TOT2<-gsub("Haemo_","",baz$DC_TOT2)
  baz$DC_TOT2<-gsub("_OtherTxRel","",baz$DC_TOT2)
  baz$DC_TOT2<-gsub("_Inf","",baz$DC_TOT2)
  baz$DC_TOT2<-gsub("_Haemo","",baz$DC_TOT2)
  baz$DC_TOT2<-as.factor(baz$DC_TOT2)
  
  baz$DC_TOT_all<-paste0(ifelse(baz$VCAUSDTH=="Relapse or progression of original disease","OrigDisease",
                         ifelse(baz$VCAUSDTH=="unknown","NA",as.character(baz$VCAUSDTH))),"__",
                     ifelse(baz$VCSDTGVH=="Yes","GVHD","NA"),"__",
                     ifelse(baz$VCSDTINP=="Yes","IP","NA"),"__",
                     ifelse(baz$VCSDTPTX=="Yes","PulmonTox","NA"),"__",
                     ifelse(baz$VCSDTINF=="Yes","Inf","NA"),"__",
                     ifelse(baz$VCSDTBAC=="Yes","Bact","NA"),"__",
                     ifelse(baz$VCSDTVIR=="Yes","Vir","NA"),"__",
                     ifelse(baz$VCSDTFUN=="Yes","Fung","NA"),"__",
                     ifelse(baz$VCSDTPAR=="Yes","Parasite","NA"),"__",
                     ifelse(baz$VCSDTREJ=="Yes","Rej","NA"),"__",
                     ifelse(baz$VCSDTVOD=="Yes","VOD","NA"),"__",
                     ifelse(baz$VCSDTHMR=="Yes","Haemo","NA"),"__",
                     ifelse(baz$VCSDTCTX=="Yes","CardTox","NA"),"__",
                     ifelse(baz$VCSDTCNS=="Yes","CNSTox","NA"),"__",
                     ifelse(baz$VCSDTGIT=="Yes","GITox","NA"),"__",
                     ifelse(baz$VCSDTSKI=="Yes","SkinTox","NA"),"__",
                     ifelse(baz$VCSDTREN=="Yes","ReinalFail","NA"),"__",
                     ifelse(baz$VCSDTMOF=="Yes","MOF","NA"),"__",
                     ifelse(baz$VCSDTEBV=="Yes","LPD","NA"),"__",
                     str_trim(baz$DEACSBMR),"__",
                     str_trim(baz$DEACSBMU),"__",
                     "NA")
  
  baz$DC_TOT_all<-gsub("______NA","",baz$DC_TOT_all)
  baz$DC_TOT_all<-gsub("____NA","",baz$DC_TOT_all)
  baz$DC_TOT_all<-gsub("__NA","",baz$DC_TOT_all)
  baz$DC_TOT_all<-gsub("NA__","",baz$DC_TOT_all)
  baz$DC_TOT_all<-gsub("____","__",baz$DC_TOT_all)
  baz$DC_TOT_all[which(baz$DC_TOT_all=="NA")]<-NA
  
  baz$VCAUSDTH2<-as.character(baz$VCAUSDTH)
  baz$VCAUSDTH2[which(baz$VCAUSDTH=="unknown")]<-NA
  baz$VCAUSDTH2[which(baz$VCAUSDTH=="Relapse or progression of original disease")]<-"OrigDisease"
  if(compete_relapse==TRUE)   baz$VCAUSDTH2[which(baz$RELAPSE==1 & baz$STATE==1)]<-"OrigDisease"

  
  baz$DC_TOT_all2<-paste0(as.character(baz$VCAUSDTH2),"__",
                         ifelse(baz$VCSDTGVH=="Yes","GVHD","NA"),"__",
                         ifelse(baz$VCSDTINP=="Yes","IP","NA"),"__",
                         ifelse(baz$VCSDTPTX=="Yes","PulmonTox","NA"),"__",
                         ifelse(baz$VCSDTINF=="Yes","Inf","NA"),"__",
                         ifelse(baz$VCSDTBAC=="Yes","Bact","NA"),"__",
                         ifelse(baz$VCSDTVIR=="Yes","Vir","NA"),"__",
                         ifelse(baz$VCSDTFUN=="Yes","Fung","NA"),"__",
                         ifelse(baz$VCSDTPAR=="Yes","Parasite","NA"),"__",
                         ifelse(baz$VCSDTREJ=="Yes","Rej","NA"),"__",
                         ifelse(baz$VCSDTVOD=="Yes","VOD","NA"),"__",
                         ifelse(baz$VCSDTHMR=="Yes","Haemo","NA"),"__",
                         ifelse(baz$VCSDTCTX=="Yes","CardTox","NA"),"__",
                         ifelse(baz$VCSDTCNS=="Yes","CNSTox","NA"),"__",
                         ifelse(baz$VCSDTGIT=="Yes","GITox","NA"),"__",
                         ifelse(baz$VCSDTSKI=="Yes","SkinTox","NA"),"__",
                         ifelse(baz$VCSDTREN=="Yes","ReinalFail","NA"),"__",
                         ifelse(baz$VCSDTMOF=="Yes","MOF","NA"),"__",
                         ifelse(baz$VCSDTEBV=="Yes","LPD","NA"),"__",
                         str_trim(baz$DEACSBMR),"__",
                         str_trim(baz$DEACSBMU),"__",
                         "NA")
  
  baz$DC_TOT_all2<-gsub("______NA","",baz$DC_TOT_all2)
  baz$DC_TOT_all2<-gsub("____NA","",baz$DC_TOT_all2)
  baz$DC_TOT_all2<-gsub("__NA","",baz$DC_TOT_all2)
  baz$DC_TOT_all2<-gsub("NA__","",baz$DC_TOT_all2)
  baz$DC_TOT_all2<-gsub("____","__",baz$DC_TOT_all2)
  baz$DC_TOT_all2<-gsub("__1","",baz$DC_TOT_all2)
  baz$DC_TOT_all2[which(baz$DC_TOT_all2=="NA")]<-NA
  
  return(baz)
}




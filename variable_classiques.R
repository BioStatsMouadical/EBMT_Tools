

variableclassique<-function(baz){

names(baz)<-toupper(names(baz))
  
if(class(baz$DATBMT)=="Date"){
  
baz$YEARTX<-as.numeric(format(baz$DATBMT,"%Y"))
    
baz$DIAGRFM<-as.numeric(difftime(baz$DATBMT,baz$IDAABB,units = "days"))/30.4375
baz$AGETX<-as.numeric(difftime(baz$DATBMT,baz$DATPATBD,units = "days"))/365.25
baz$AGETX[which(baz$DATPATBD=="1809-01-01")]<-NA
baz$AGEDIAG<-as.numeric(difftime(baz$IDAABB,baz$DATPATBD,units = "days"))/365.25
baz$AGEDIAG[which(baz$AGEDIAG <= 0 )]<-0
baz$AGEDON1<-as.numeric(difftime(baz$DATBMT,baz$DATDONBD1,units = "days"))/365.25
baz$AGEDON2<-as.numeric(difftime(baz$DATBMT,baz$DATDONBD2,units = "days"))/365.25
}

baz$RELATIVE_DO1[which(baz$RELATIVE_DO1=="unknown")]<-NA
baz$RELATIVE_DO2[which(baz$RELATIVE_DO2=="unknown")]<-NA

baz$VCMVPAT[which(baz$VCMVPAT=="unknown" | baz$VCMVPAT=="Not evaluated")]<-NA
baz$VCMVDON1[which(baz$VCMVDON1=="unknown" | baz$VCMVDON1=="Not evaluated")]<-NA
baz$VCMVDON2[which(baz$VCMVDON2=="unknown" | baz$VCMVDON2=="Not evaluated")]<-NA

baz$VEBVPAT[which(baz$VEBVPAT=="unknown" | baz$VEBVPAT=="Not evaluated")]<-NA
baz$VEBVDON1[which(baz$VEBVDON1=="unknown" | baz$VEBVDON1=="Not evaluated")]<-NA
baz$VEBVDON2[which(baz$VEBVDON2=="unknown" | baz$VEBVDON2=="Not evaluated")]<-NA

baz$PATSEX[which(baz$PATSEX=="unknown" | baz$PATSEX=="Not evaluated")]<-NA
baz$DONSEX1[which(baz$DONSEX1=="unknown" | baz$DONSEX1=="Not evaluated")]<-NA
baz$DONSEX2[which(baz$DONSEX2=="unknown" | baz$DONSEX2=="Not evaluated")]<-NA

if ("VHEMOGLO_DIAG" %in% names(baz)) baz$VHEMOGLO_DIAG[which(baz$VHEMOGLO_DIAG=="unknown" | baz$VHEMOGLO_DIAG=="Not evaluated")]<-NA

baz$FM1<-NA
baz$FM1[which(baz$PATSEX=="Female")]<-"No"
baz$FM1[which(baz$PATSEX=="Male" & baz$DONSEX1=="Male")]<-"No"
baz$FM1[which(baz$PATSEX=="Male" & baz$DONSEX1=="Female")]<-"Yes"
baz$FM1<-as.factor(baz$FM1)

baz$FM2<-NA
baz$FM2[which(baz$PATSEX=="Female")]<-"No"
baz$FM2[which(baz$PATSEX=="Male" & baz$DONSEX2=="Male")]<-"No"
baz$FM2[which(baz$PATSEX=="Male" & baz$DONSEX2=="Female")]<-"Yes"
baz$FM2<-as.factor(baz$FM2)

baz$CMVDP1<-NA
baz$CMVDP1[which(baz$VCMVDON1=="Negative" & baz$VCMVPAT=="Negative")]<-"Neg to Neg"
baz$CMVDP1[which(baz$VCMVDON1=="Negative" & baz$VCMVPAT=="Positive")]<-"Neg to Pos"
baz$CMVDP1[which(baz$VCMVDON1=="Positive" & baz$VCMVPAT=="Negative")]<-"Pos to Neg"
baz$CMVDP1[which(baz$VCMVDON1=="Positive" & baz$VCMVPAT=="Positive")]<-"Pos to Pos"
baz$CMVDP1<-as.factor(baz$CMVDP1)

baz$CMVDP2<-NA
baz$CMVDP2[which(baz$VCMVDON2=="Negative" & baz$VCMVPAT=="Negative")]<-"Neg to Neg"
baz$CMVDP2[which(baz$VCMVDON2=="Negative" & baz$VCMVPAT=="Positive")]<-"Neg to Pos"
baz$CMVDP2[which(baz$VCMVDON2=="Positive" & baz$VCMVPAT=="Negative")]<-"Pos to Neg"
baz$CMVDP2[which(baz$VCMVDON2=="Positive" & baz$VCMVPAT=="Positive")]<-"Pos to Pos"
baz$CMVDP2<-as.factor(baz$CMVDP2)


baz$EBVDP1<-NA
baz$EBVDP1[which(baz$VEBVDON1=="Negative" & baz$VEBVPAT=="Negative")]<-"Neg to Neg"
baz$EBVDP1[which(baz$VEBVDON1=="Negative" & baz$VEBVPAT=="Positive")]<-"Neg to Pos"
baz$EBVDP1[which(baz$VEBVDON1=="Positive" & baz$VEBVPAT=="Negative")]<-"Pos to Neg"
baz$EBVDP1[which(baz$VEBVDON1=="Positive" & baz$VEBVPAT=="Positive")]<-"Pos to Pos"
baz$EBVDP1<-as.factor(baz$EBVDP1)

baz$EBVDP2<-NA
baz$EBVDP2[which(baz$VEBVDON2=="Negative" & baz$VEBVPAT=="Negative")]<-"Neg to Neg"
baz$EBVDP2[which(baz$VEBVDON2=="Negative" & baz$VEBVPAT=="Positive")]<-"Neg to Pos"
baz$EBVDP2[which(baz$VEBVDON2=="Positive" & baz$VEBVPAT=="Negative")]<-"Pos to Neg"
baz$EBVDP2[which(baz$VEBVDON2=="Positive" & baz$VEBVPAT=="Positive")]<-"Pos to Pos"
baz$EBVDP2<-as.factor(baz$EBVDP2)


baz$BMTKARNOFSK[which(baz$BMTKARNOFSK=="unknown" | baz$BMTKARNOFSK=="Not evaluated")]<-NA
baz$PERFSTAT[which(baz$PERFSTAT=="unknown" | baz$PERFSTAT=="Not evaluated")]<-NA

baz$BMTKARNOFSK[which(is.na(baz$BMTKARNOFSK) & baz$PERFSTAT=="Good")]<-"Good (Karnofsky >=80)"
baz$BMTKARNOFSK[which(is.na(baz$BMTKARNOFSK) & baz$PERFSTAT=="Poor")]<-"Poor (Karnofsky <80)"

baz$KARN80<-NA
baz$KARN80[which(baz$BMTKARNOFSK=="Dead")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Moribund / Doesn`t play")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Very sick / Sleeping often; play limited to passive activities")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Severely disabled / Bedbound; needs assistance for quiet play")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Disabled / Mainly in bed; participates in quiet activities")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Requires assistance / Lying around much of the day; no active playing")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Requires occasional assistance / Up and around; active play minimal")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Cares for self / Greater restriction of play")]<-"< 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Normal with effort / Active, but tired more quickly")]<-">= 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Normal activity / Minor restrictions in strenous physical activity")]<-">= 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Normal, NED")]<-">= 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Good (Karnofsky >=80)")]<-">= 80"
baz$KARN80[which(baz$BMTKARNOFSK=="Poor (Karnofsky <80)")]<-"< 80"
baz$KARN80<-as.factor(baz$KARN80)

baz$KARN90<-NA
baz$KARN90[which(baz$BMTKARNOFSK=="Dead")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Moribund / Doesn`t play")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Very sick / Sleeping often; play limited to passive activities")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Severely disabled / Bedbound; needs assistance for quiet play")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Disabled / Mainly in bed; participates in quiet activities")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Requires assistance / Lying around much of the day; no active playing")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Requires occasional assistance / Up and around; active play minimal")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Cares for self / Greater restriction of play")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Normal with effort / Active, but tired more quickly")]<-"< 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Normal activity / Minor restrictions in strenous physical activity")]<-">= 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Normal, NED")]<-">= 90"
baz$KARN90[which(baz$BMTKARNOFSK=="Poor (Karnofsky <80)")]<-"< 90"
baz$KARN90<-as.factor(baz$KARN90)

#### Sorror ####
baz$COMORBID[which(baz$COMORBID == "unknown")] <- NA
baz$COMORBID <- factor(baz$COMORBID)

# Solid tumour,previously present: Treated at any time point in the patient's past history, excluding non-melanoma skin cancer
baz$COMOR_MALIGN[which(baz$COMOR_MALIGN == "unknown")] <- NA
baz$COMOR_MALIGN[which(baz$COMOR_MALIGN == "Not evaluated")] <- NA
baz$COMOR_MALIGN <- factor(baz$COMOR_MALIGN, levels = c("No","Yes"))

# Infammatory bowel disease: Crohn's disease or ulcerative colitis
baz$COMOR_INBWDIS[which(baz$COMOR_INBWDIS == "unknown")] <- NA
baz$COMOR_INBWDIS[which(baz$COMOR_INBWDIS == "Not evaluated")] <- NA
baz$COMOR_INBWDIS <- factor(baz$COMOR_INBWDIS, levels = c("No","Yes"))

# Rheumatologic: SLE, RA, polymyositis, mixed CTD, or polymyalgia rheumatica
baz$COMOR_RHEUMAT[which(baz$COMOR_RHEUMAT == "unknown")] <- NA
baz$COMOR_RHEUMAT[which(baz$COMOR_RHEUMAT == "Not evaluated")] <- NA
baz$COMOR_RHEUMAT <- factor(baz$COMOR_RHEUMAT, levels = c("No","Yes"))

# Infection: Requiring continuation of antimicrobial treatment after day 0
baz$COMOR_INFECPRE[which(baz$COMOR_INFECPRE == "unknown")] <- NA
baz$COMOR_INFECPRE[which(baz$COMOR_INFECPRE == "Not evaluated")] <- NA
baz$COMOR_INFECPRE <- factor(baz$COMOR_INFECPRE, levels = c("No","Yes"))

# Diabetes: Requiring treatment with insulin or oral hypoglycaemics but not diet alone
baz$COMOR_TRTDEPDB[which(baz$COMOR_TRTDEPDB == "unknown")] <- NA
baz$COMOR_TRTDEPDB[which(baz$COMOR_TRTDEPDB == "Not evaluated")] <- NA
baz$COMOR_TRTDEPDB <- factor(baz$COMOR_TRTDEPDB, levels = c("No","Yes"))

# Renal:       moderate/severe: Serum creatinine > 2 mg/dL or >177 ??mol/L, on dialysis, or prior renal transplantation
baz$COMOR_KIDNEYCO[which(baz$COMOR_KIDNEYCO == "unknown")] <- NA
baz$COMOR_KIDNEYCO[which(baz$COMOR_KIDNEYCO == "Not evaluated")] <- NA
baz$COMOR_KIDNEYCO <- factor(baz$COMOR_KIDNEYCO, levels = c("No","Yes"))

# Hepatic:   mild: Chronic hepatitis, bilirubin between Upper Limit Normal (ULN) and 1.5 x the ULN, or AST/ALT between ULN and 2.5 × ULN
#           moderate/severe: Liver cirrhosis, bilirubin greater than 1.5 × ULN, or AST/ALT greater than 2.5 × ULN
baz$COMOR_HEPATIC[which(baz$COMOR_HEPATIC == "unknown")] <- NA
baz$COMOR_HEPATIC[which(baz$COMOR_HEPATIC == "Unknown")] <- NA
baz$COMOR_HEPATIC[which(baz$COMOR_HEPATIC == "Not evaluated")] <- NA
baz$COMOR_HEPATIC[which(baz$COMOR_HEPATIC == "Not eval")] <- NA
baz$COMOR_HEPATIC = factor(baz$COMOR_HEPATIC, levels = c("No","Mild","Moderate/severe"))

# Arrhythmia: Atrial fibrillation or flutter, sick sinus syndrome, or ventricular arrhythmias
baz$COMOR_ARRYTHBL[which(baz$COMOR_ARRYTHBL == "unknown")] <- NA
baz$COMOR_ARRYTHBL[which(baz$COMOR_ARRYTHBL == "Not evaluated")] <- NA
baz$COMOR_ARRYTHBL <- factor(baz$COMOR_ARRYTHBL, levels = c("No","Yes"))

# Cardiac: Coronary artery disease, congestive heart failure, myocardial infarction, EF ??? 50%, or shortening fraction in children (<28%)
baz$COMOR_CARDIAC[which(baz$COMOR_CARDIAC == "unknown")] <- NA
baz$COMOR_CARDIAC[which(baz$COMOR_CARDIAC == "Not evaluated")] <- NA
baz$COMOR_CARDIAC <- factor(baz$COMOR_CARDIAC, levels = c("No","Yes"))

# Cerebrovascular disease: Transient ischemic attack or cerebrovascular accident
baz$COMOR_STROKE <- as.character(baz$COMOR_STROKE)
baz$COMOR_STROKE[which(baz$COMOR_STROKE == "unknown")] <- NA
baz$COMOR_STROKE[which(baz$COMOR_STROKE == "Not evaluated")] <- NA
baz$COMOR_STROKE[which(baz$COMOR_STROKE == "Present")] <- "Yes"
baz$COMOR_STROKE[which(baz$COMOR_STROKE == "Absent")] <- "No"
baz$COMOR_STROKE <- factor(baz$COMOR_STROKE, levels = c("No","Yes"))

# Heart valve disease: Except mitral valve prolapse
baz$COMOR_VALVE[which(baz$COMOR_VALVE == "unknown")] <- NA
baz$COMOR_VALVE[which(baz$COMOR_VALVE == "Not evaluated")] <- NA
baz$COMOR_VALVE <- factor(baz$COMOR_VALVE, levels = c("No","Yes"))

# Pulmonary:   moderate : DLco and/or FEV1 66-80% or dyspnoea on slight activity
#              severe : DLco and/or FEV1 ??? 65% or dyspnoea at rest or requiring oxygen
baz$COMOR_PULMONC <- as.character(baz$COMOR_PULMONC)
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == "unknown")] <- NA
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == "Not evaluated")] <- NA
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == "Absent")] <- "No"
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == "Present")] <- "Yes (grade unknown)"
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == " - Mild")] <- "Mild"
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == " - Moderate")] <- "Moderate"
baz$COMOR_PULMONC[which(baz$COMOR_PULMONC == " - Severe")] <- "Severe"
baz$COMOR_PULMONC <- factor(baz$COMOR_PULMONC, levels = c("No","Yes (grade unknown)","Mild","Moderate","Severe"))

# Obesity: Patients with a body mass index > 35 kg/m2
baz$COMOR_OBESITY[which(baz$COMOR_OBESITY == "unknown")] <- NA
baz$COMOR_OBESITY[which(baz$COMOR_OBESITY == "Not evaluated")] <- NA
baz$COMOR_OBESITY <- factor(baz$COMOR_OBESITY, levels = c("No","Yes"))

# Peptic ulcer: Requiring treatment
baz$COMOR_PEPTICU[which(baz$COMOR_PEPTICU == "unknown")] <- NA
baz$COMOR_PEPTICU[which(baz$COMOR_PEPTICU == "Not evaluated")] <- NA
baz$COMOR_PEPTICU <- factor(baz$COMOR_PEPTICU, levels = c("No","Yes"))

# Psychiatric disturbance: Depression or anxiety requiring psychiatric consultation or treatment
baz$COMOR_PSYCH[which(baz$COMOR_PSYCH == "unknown")] <- NA
baz$COMOR_PSYCH[which(baz$COMOR_PSYCH == "Not evaluated")] <- NA
baz$COMOR_PSYCH <- factor(baz$COMOR_PSYCH, levels = c("No","Yes"))

baz$COMOR_MALIGN_SOURCE<-baz$COMOR_MALIGN
baz$COMOR_INBWDIS_SOURCE<-baz$COMOR_INBWDIS
baz$COMOR_RHEUMAT_SOURCE<-baz$COMOR_RHEUMAT
baz$COMOR_INFECPRE_SOURCE<-baz$COMOR_INFECPRE
baz$COMOR_TRTDEPDB_SOURCE<-baz$COMOR_TRTDEPDB
baz$COMOR_KIDNEYCO_SOURCE<-baz$COMOR_KIDNEYCO
baz$COMOR_HEPATIC_SOURCE<-baz$COMOR_HEPATIC
baz$COMOR_ARRYTHBL_SOURCE<-baz$COMOR_ARRYTHBL
baz$COMOR_CARDIAC_SOURCE<-baz$COMOR_CARDIAC
baz$COMOR_STROKE_SOURCE<-baz$COMOR_STROKE
baz$COMOR_VALVE_SOURCE<-baz$COMOR_VALVE
baz$COMOR_PULMONC_SOURCE<-baz$COMOR_PULMONC
baz$COMOR_OBESITY_SOURCE<-baz$COMOR_OBESITY
baz$COMOR_PEPTICU_SOURCE<-baz$COMOR_PEPTICU
baz$COMOR_PSYCH_SOURCE<-baz$COMOR_PSYCH


# If COMORBID is No, I put No in all individual comorbidities that are missing
baz$COMOR_MALIGN[which(is.na(baz$COMOR_MALIGN) & baz$COMORBID == "No")] <- "No"
baz$COMOR_INBWDIS[which(is.na(baz$COMOR_INBWDIS) & baz$COMORBID == "No")] <- "No"
baz$COMOR_RHEUMAT[which(is.na(baz$COMOR_RHEUMAT) & baz$COMORBID == "No")] <- "No"
baz$COMOR_INFECPRE[which(is.na(baz$COMOR_INFECPRE) & baz$COMORBID == "No")] <- "No"
baz$COMOR_TRTDEPDB[which(is.na(baz$COMOR_TRTDEPDB) & baz$COMORBID == "No")] <- "No"
baz$COMOR_KIDNEYCO[which(is.na(baz$COMOR_KIDNEYCO) & baz$COMORBID == "No")] <- "No"
baz$COMOR_HEPATIC[which(is.na(baz$COMOR_HEPATIC) & baz$COMORBID == "No")] <- "No"
baz$COMOR_ARRYTHBL[which(is.na(baz$COMOR_ARRYTHBL) & baz$COMORBID == "No")] <- "No"
baz$COMOR_CARDIAC[which(is.na(baz$COMOR_CARDIAC) & baz$COMORBID == "No")] <- "No"
baz$COMOR_STROKE[which(is.na(baz$COMOR_STROKE) & baz$COMORBID == "No")] <- "No"
baz$COMOR_VALVE[which(is.na(baz$COMOR_VALVE) & baz$COMORBID == "No")] <- "No"
baz$COMOR_PULMONC[which(is.na(baz$COMOR_PULMONC) & baz$COMORBID == "No")] <- "No"
baz$COMOR_OBESITY[which(is.na(baz$COMOR_OBESITY) & baz$COMORBID == "No")] <- "No"
baz$COMOR_PEPTICU[which(is.na(baz$COMOR_PEPTICU) & baz$COMORBID == "No")] <- "No"
baz$COMOR_PSYCH[which(is.na(baz$COMOR_PSYCH) & baz$COMORBID == "No")] <- "No"

# Binary version of hepatic comorbidity
baz$COMOR_HEPATIC_YN <- as.character(baz$COMOR_HEPATIC)
baz$COMOR_HEPATIC_YN[which(baz$COMOR_HEPATIC_YN == "Moderate/severe")] <- "Yes"
baz$COMOR_HEPATIC_YN[which(baz$COMOR_HEPATIC_YN == "Mild")] <- "Yes"
baz$COMOR_HEPATIC_YN <- factor(baz$COMOR_HEPATIC_YN, levels = c("No","Yes"))

baz$COMOR_HEPATIC_YN_SOURCE <- as.character(baz$COMOR_HEPATIC_SOURCE)
baz$COMOR_HEPATIC_YN_SOURCE[which(baz$COMOR_HEPATIC_YN_SOURCE == "Moderate/severe")] <- "Yes"
baz$COMOR_HEPATIC_YN_SOURCE[which(baz$COMOR_HEPATIC_YN_SOURCE == "Mild")] <- "Yes"
baz$COMOR_HEPATIC_YN_SOURCE <- factor(baz$COMOR_HEPATIC_YN_SOURCE, levels = c("No","Yes"))


# Binary version of pulmonary comorbidity
baz$COMOR_PULMONC_YN <- as.character(baz$COMOR_PULMONC)
baz$COMOR_PULMONC_YN[which(baz$COMOR_PULMONC_YN == "Yes (grade unknown)")] <- "Yes"
baz$COMOR_PULMONC_YN[which(baz$COMOR_PULMONC_YN == "Mild")] = "Yes"
baz$COMOR_PULMONC_YN[which(baz$COMOR_PULMONC_YN == "Moderate")] = "Yes"
baz$COMOR_PULMONC_YN[which(baz$COMOR_PULMONC_YN == "Severe")] = "Yes"
baz$COMOR_PULMONC_YN <- factor(baz$COMOR_PULMONC_YN, levels = c("No","Yes"))

baz$COMOR_PULMONC_YN_SOURCE <- as.character(baz$COMOR_PULMONC_SOURCE)
baz$COMOR_PULMONC_YN_SOURCE[which(baz$COMOR_PULMONC_YN_SOURCE == "Yes (grade unknown)")] <- "Yes"
baz$COMOR_PULMONC_YN_SOURCE[which(baz$COMOR_PULMONC_YN_SOURCE == "Mild")] = "Yes"
baz$COMOR_PULMONC_YN_SOURCE[which(baz$COMOR_PULMONC_YN_SOURCE == "Moderate")] = "Yes"
baz$COMOR_PULMONC_YN_SOURCE[which(baz$COMOR_PULMONC_YN_SOURCE == "Severe")] = "Yes"
baz$COMOR_PULMONC_YN_SOURCE <- factor(baz$COMOR_PULMONC_YN_SOURCE, levels = c("No","Yes"))

baz$SORROR <- 0
baz$SORROR_SOURCE <- 0

# Sorror is unknown if not all individual comorbidities are available
baz$SORROR[which(is.na(baz$COMOR_MALIGN) |
                    is.na(baz$COMOR_INBWDIS) |
                    is.na(baz$COMOR_RHEUMAT) |
                    is.na(baz$COMOR_INFECPRE) |
                    is.na(baz$COMOR_TRTDEPDB) |
                    is.na(baz$COMOR_KIDNEYCO) |
                    is.na(baz$COMOR_HEPATIC) |
                    is.na(baz$COMOR_ARRYTHBL) |
                    is.na(baz$COMOR_CARDIAC) |
                    is.na(baz$COMOR_STROKE) |
                    is.na(baz$COMOR_VALVE) |
                    is.na(baz$COMOR_PULMONC) |
                    is.na(baz$COMOR_OBESITY) |
                    is.na(baz$COMOR_PEPTICU) |
                    is.na(baz$COMOR_PSYCH))] = NA
# Also the grade must be available for pulmonary comorbidity
baz$SORROR[which(baz$COMOR_PULMONC == "Yes (grade unknown)")] = NA

baz$SORROR_SOURCE[which(is.na(baz$COMOR_MALIGN_SOURCE) |
                   is.na(baz$COMOR_INBWDIS_SOURCE) |
                   is.na(baz$COMOR_RHEUMAT_SOURCE) |
                   is.na(baz$COMOR_INFECPRE_SOURCE) |
                   is.na(baz$COMOR_TRTDEPDB_SOURCE) |
                   is.na(baz$COMOR_KIDNEYCO_SOURCE) |
                   is.na(baz$COMOR_HEPATIC_SOURCE) |
                   is.na(baz$COMOR_ARRYTHBL_SOURCE) |
                   is.na(baz$COMOR_CARDIAC_SOURCE) |
                   is.na(baz$COMOR_STROKE_SOURCE) |
                   is.na(baz$COMOR_VALVE_SOURCE) |
                   is.na(baz$COMOR_PULMONC_SOURCE) |
                   is.na(baz$COMOR_OBESITY_SOURCE) |
                   is.na(baz$COMOR_PEPTICU_SOURCE) |
                   is.na(baz$COMOR_PSYCH_SOURCE))] = NA
# Also the grade must be available for pulmonary comorbidity
baz$SORROR_SOURCE[which(baz$COMOR_PULMONC_SOURCE == "Yes (grade unknown)")] = NA

baz$SORROR[which(baz$COMOR_MALIGN == "Yes")] = baz$SORROR[which(baz$COMOR_MALIGN == "Yes")] + 3
baz$SORROR[which(baz$COMOR_INBWDIS == "Yes")] = baz$SORROR[which(baz$COMOR_INBWDIS == "Yes")] + 1
baz$SORROR[which(baz$COMOR_RHEUMAT == "Yes")] = baz$SORROR[which(baz$COMOR_RHEUMAT == "Yes")] + 2
baz$SORROR[which(baz$COMOR_INFECPRE == "Yes")] = baz$SORROR[which(baz$COMOR_INFECPRE == "Yes")] + 1
baz$SORROR[which(baz$COMOR_TRTDEPDB == "Yes")] = baz$SORROR[which(baz$COMOR_TRTDEPDB == "Yes")] + 1
baz$SORROR[which(baz$COMOR_KIDNEYCO == "Yes")] = baz$SORROR[which(baz$COMOR_KIDNEYCO == "Yes")] + 2
baz$SORROR[which(baz$COMOR_HEPATIC == "Mild")] = baz$SORROR[which(baz$COMOR_HEPATIC == "Mild")] + 1
baz$SORROR[which(baz$COMOR_HEPATIC == "Moderate/severe")] = baz$SORROR[which(baz$COMOR_HEPATIC == "Moderate/severe")] + 3
baz$SORROR[which(baz$COMOR_ARRYTHBL == "Yes")] = baz$SORROR[which(baz$COMOR_ARRYTHBL == "Yes")] + 1
baz$SORROR[which(baz$COMOR_CARDIAC == "Yes")] = baz$SORROR[which(baz$COMOR_CARDIAC == "Yes")] + 1
baz$SORROR[which(baz$COMOR_STROKE == "Yes")] = baz$SORROR[which(baz$COMOR_STROKE == "Yes")] + 1
baz$SORROR[which(baz$COMOR_VALVE == "Yes")] = baz$SORROR[which(baz$COMOR_VALVE == "Yes")] + 3
baz$SORROR[which(baz$COMOR_PULMONC == "Moderate")] = baz$SORROR[which(baz$COMOR_PULMONC == "Moderate")] + 2
baz$SORROR[which(baz$COMOR_PULMONC == "Severe")] = baz$SORROR[which(baz$COMOR_PULMONC == "Severe")] + 3
baz$SORROR[which(baz$COMOR_OBESITY == "Yes")] = baz$SORROR[which(baz$COMOR_OBESITY == "Yes")] + 1
baz$SORROR[which(baz$COMOR_PEPTICU == "Yes")] = baz$SORROR[which(baz$COMOR_PEPTICU == "Yes")] + 2
baz$SORROR[which(baz$COMOR_PSYCH == "Yes")] = baz$SORROR[which(baz$COMOR_PSYCH == "Yes")] + 1

baz$SORROR_SOURCE[which(baz$COMOR_MALIGN_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_MALIGN_SOURCE == "Yes")] + 3
baz$SORROR_SOURCE[which(baz$COMOR_INBWDIS_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_INBWDIS_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_RHEUMAT_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_RHEUMAT_SOURCE == "Yes")] + 2
baz$SORROR_SOURCE[which(baz$COMOR_INFECPRE_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_INFECPRE_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_TRTDEPDB_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_TRTDEPDB_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_KIDNEYCO_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_KIDNEYCO_SOURCE == "Yes")] + 2
baz$SORROR_SOURCE[which(baz$COMOR_HEPATIC_SOURCE == "Mild")] = baz$SORROR_SOURCE[which(baz$COMOR_HEPATIC_SOURCE == "Mild")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_HEPATIC_SOURCE == "Moderate/severe")] = baz$SORROR_SOURCE[which(baz$COMOR_HEPATIC_SOURCE == "Moderate/severe")] + 3
baz$SORROR_SOURCE[which(baz$COMOR_ARRYTHBL_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_ARRYTHBL_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_CARDIAC_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_CARDIAC_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_STROKE_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_STROKE_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_VALVE_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_VALVE_SOURCE == "Yes")] + 3
baz$SORROR_SOURCE[which(baz$COMOR_PULMONC_SOURCE == "Moderate")] = baz$SORROR_SOURCE[which(baz$COMOR_PULMONC_SOURCE == "Moderate")] + 2
baz$SORROR_SOURCE[which(baz$COMOR_PULMONC_SOURCE == "Severe")] = baz$SORROR_SOURCE[which(baz$COMOR_PULMONC_SOURCE == "Severe")] + 3
baz$SORROR_SOURCE[which(baz$COMOR_OBESITY_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_OBESITY_SOURCE == "Yes")] + 1
baz$SORROR_SOURCE[which(baz$COMOR_PEPTICU_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_PEPTICU_SOURCE == "Yes")] + 2
baz$SORROR_SOURCE[which(baz$COMOR_PSYCH_SOURCE == "Yes")] = baz$SORROR_SOURCE[which(baz$COMOR_PSYCH_SOURCE == "Yes")] + 1

baz$SORROR.CL = NA
baz$SORROR.CL[which(baz$SORROR == 0)] = "0"
baz$SORROR.CL[which(baz$SORROR == 1 | baz$SORROR == 2)] = "1-2"
baz$SORROR.CL[which(baz$SORROR >= 3)] = ">=3"
baz$SORROR.CL = factor(baz$SORROR.CL, levels = c("0","1-2",">=3"))

baz$SORROR.CL_SOURCE = NA
baz$SORROR.CL_SOURCE[which(baz$SORROR_SOURCE == 0)] = "0"
baz$SORROR.CL_SOURCE[which(baz$SORROR_SOURCE == 1 | baz$SORROR_SOURCE == 2)] = "1-2"
baz$SORROR.CL_SOURCE[which(baz$SORROR_SOURCE >= 3)] = ">=3"
baz$SORROR.CL_SOURCE = factor(baz$SORROR.CL_SOURCE, levels = c("0","1-2",">=3"))



if ("TDVIVO" %in% names(baz)) baz$TDVIVO<-as.factor(ifelse(baz$TDVIVO==1,"Yes","No"))

if ("EXVIMANI" %in% names(baz)){
baz$TDEPLVITROAUTO<-as.character(baz$EXVIMANI)
baz$TDEPLVITROAUTO[which(baz$TDEPLVITROAUTO==" - Marrow")]<-"Yes"
baz$TDEPLVITROAUTO[which(baz$TDEPLVITROAUTO==" - PB")]<-"Yes"
baz$TDEPLVITROAUTO[which(baz$TDEPLVITROAUTO==" - Marrow+PB")]<-"Yes"
baz$TDEPLVITROAUTO<-as.factor(baz$TDEPLVITROAUTO)

baz$TDEPLVITRO_D1<-NA
baz$TDEPLVITRO_D1[which(baz$EXVIMAND1_D1P1=="No" & baz$EXVIMAND2_D1P2=="No")]<-"No"
baz$TDEPLVITRO_D1[which(baz$EXVIMAND1_D1P1=="Yes" | baz$EXVIMAND2_D1P2=="Yes")]<-"Yes"
baz$TDEPLVITRO_D1[which(is.na(baz$EXVIMAND1_D1P1) & baz$EXVIMAND2_D1P2=="Yes")]<-"Yes"
baz$TDEPLVITRO_D1[which(is.na(baz$EXVIMAND1_D1P1) & baz$EXVIMAND2_D1P2=="No")]<-"No"
baz$TDEPLVITRO_D1[which(is.na(baz$EXVIMAND2_D1P2) & baz$EXVIMAND1_D1P1=="Yes")]<-"Yes"
baz$TDEPLVITRO_D1[which(is.na(baz$EXVIMAND2_D1P2) & baz$EXVIMAND1_D1P1=="No")]<-"No"

baz$TDEPLVITRO_D2<-NA
baz$TDEPLVITRO_D2[which(baz$EXVIMAND1_D2P1=="No" & baz$EXVIMAND2_D2P2=="No")]<-"No"
baz$TDEPLVITRO_D2[which(baz$EXVIMAND1_D2P1=="Yes" | baz$EXVIMAND2_D2P2=="Yes")]<-"Yes"
baz$TDEPLVITRO_D2[which(is.na(baz$EXVIMAND1_D2P1) & baz$EXVIMAND2_D2P2=="Yes")]<-"Yes"
baz$TDEPLVITRO_D2[which(is.na(baz$EXVIMAND1_D2P1) & baz$EXVIMAND2_D2P2=="No")]<-"No"
baz$TDEPLVITRO_D2[which(is.na(baz$EXVIMAND2_D2P2) & baz$EXVIMAND1_D2P1=="Yes")]<-"Yes"
baz$TDEPLVITRO_D2[which(is.na(baz$EXVIMAND2_D2P2) & baz$EXVIMAND1_D2P1=="No")]<-"No"

baz$TDEPLVITROJEG<-NA
baz$TDEPLVITROJEG[which(baz$TDEPLVITRO_D1=="No" & baz$TDEPLVITRO_D2 == "No")]<-"No"
baz$TDEPLVITROJEG[which(baz$TDEPLVITRO_D1=="Yes" | baz$TDEPLVITRO_D2 == "Yes")]<-"Yes"
baz$TDEPLVITROJEG[which(baz$TDEPLVITRO_D1=="Yes" & is.na(baz$TDEPLVITRO_D2))]<-"Yes"
baz$TDEPLVITROJEG[which(baz$TDEPLVITRO_D1=="No" & is.na(baz$TDEPLVITRO_D2))]<-"No"
baz$TDEPLVITROJEG[which(is.na(baz$TDEPLVITRO_D1) & baz$TDEPLVITRO_D2=="Yes")]<-"Yes"
baz$TDEPLVITROJEG[which(is.na(baz$TDEPLVITRO_D1) & baz$TDEPLVITRO_D2=="No")]<-"No"
baz$TDEPLVITROJEG[which(baz$VTRANTYP=="Autologous")]<-baz$TDEPLVITROAUTO[which(baz$VTRANTYP=="Autologous")]
baz$TDEPLVITROJEG<-as.factor(baz$TDEPLVITROJEG)
}

if("STEMCEDO1_D1P1" %in% names(baz)){
# baz$STEMCEDO_DO1[which(baz$STEMCEDO_DO1==99)]<-NA
# baz$STEMCEDO_DO2[which(baz$STEMCEDO_DO2==99)]<-NA


baz$NBRPROD_D1<-(1-is.na(baz$STEMCEDO1_D1P1))*1000+(1-is.na(baz$STEMCEDO2_D1P2))*100
baz$NBRPROD_D1[which(baz$NBRPROD_D1==0 & !is.na(baz$STEMCEDO1_D1P1))]<-1000
baz$NBRPROD_D1[which(baz$NBRPROD_D1==0 & ((baz$VOTSC=="Yes") | (baz$VPBSC=="Yes") | 
                                                  (baz$VBMSC=="Yes") | (baz$VCBSC=="Yes")))]<-1000
baz$NBRPROD_D1[which(baz$NBRPROD_D1==100)]<-1000

baz$NBRPROD_D2<-(1-is.na(baz$STEMCEDO1_D2P1))*20+(1-is.na(baz$STEMCEDO2_D2P2))*2
baz$NBRPROD_D2[which(baz$NBRPROD_D2==0 & !is.na(baz$STEMCEDO1_D2P1))]<-20

baz$DONPROD<-baz$NBRPROD_D1+baz$NBRPROD_D2
baz$DONPROD2<-baz$DONPROD
baz$DONPROD2[baz$DONPROD==1000]<-"1donor_1product"
baz$DONPROD2[baz$DONPROD==1100]<-"1donor_2product"
baz$DONPROD2[baz$DONPROD==1020]<-"2donor_2product"
baz$DONPROD2[baz$DONPROD==1022]<-"2donor_3product"
baz$DONPROD2[baz$DONPROD==1120]<-"2donor_3product"
baz$DONPROD2[baz$DONPROD==1122]<-"2donor_4product"
baz$DONPROD2[baz$DONPROD==0]<-NA
baz$DONPROD2<-as.factor(baz$DONPROD2)
}

if("CENTRE_NUMERIC" %in% names(baz)) baz$CENTREJEG<-as.factor(paste0(baz$CENTRE,", ",baz$CENTRE_NUMERIC))
if("CENTRE_NUMERIC" %in% names(baz)) baz$CENTREJEG[which(baz$CENTREJEG=="NA, NA")]<-NA
if("CENTRE_NUMERIC" %in% names(baz)) baz$CENTREJEGEP<-as.factor(paste0(baz$CENTRE_NUMERIC,"__",baz$CENTRE))


######### HSCTYPE 2 #######

baz$CELLSOURCE[which(baz$CELLSOURCE=="Unknown")]<-NA

baz$CELLSOURCE2<-as.character(baz$CELLSOURCE)
baz$CELLSOURCE2[baz$CELLSOURCE=="Double CB"]<-"CB"
baz$CELLSOURCE2[baz$CELLSOURCE=="BM+PB"]<-"PB"
baz$CELLSOURCE2<-as.factor(baz$CELLSOURCE2)


baz$ABOPAT[which(baz$ABOPAT=="unknown")]<-NA
baz$ABODOND1[which(baz$ABODOND1=="unknown")]<-NA
baz$ABODOND1[which(baz$ABODOND1=="Not evaluated")]<-NA
baz$ABODOND2[which(baz$ABODOND2=="unknown")]<-NA
baz$ABODOND2[which(baz$ABODOND2=="Not evaluated")]<-NA
baz$ENGNEUT[which(baz$ENGNEUT=="unknown")]<-NA

baz$ABOMATCH1[which(baz$ABOMATCH1=="Not evaluated")]<-NA
baz$ABOMATCH1[which(baz$ABOMATCH1=="unknown")]<-NA
baz$ABOPAT[which(baz$ABOPAT=="unknown")]<-NA
baz$ABOPAT[which(baz$ABOPAT=="Not evaluated")]<-NA
baz$ABODOND1[which(baz$ABODOND1=="unknown")]<-NA
baz$ABODOND1[which(baz$ABODOND1=="Not evaluated")]<-NA

baz$ABOPAT<-as.character(baz$ABOPAT)
baz$ABOPAT[which(baz$ABOPAT=="A1")]<-"A"
baz$ABOPAT<-as.factor(baz$ABOPAT)

baz$ABODOND1<-as.character(baz$ABODOND1)
baz$ABODOND1[which(baz$ABODOND1=="A1")]<-"A"
baz$ABODOND1<-as.factor(baz$ABODOND1)

baz$ABOMATCH1[which(baz$ABOMATCH1=="9")]<-NA
baz$ABOMATCH1[which(baz$ABOMATCH1=="9")]<-NA



baz$ABOJEG<-NA

baz$ABOJEG[which(baz$ABODOND1=="A" & baz$ABOPAT=="O")]<-"Major incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="B" & baz$ABOPAT=="O")]<-"Major incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="AB" & baz$ABOPAT=="O")]<-"Major incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="O" & baz$ABOPAT=="O")]<-"Compatible"

baz$ABOJEG[which(baz$ABODOND1=="A" & baz$ABOPAT=="A")]<-"Compatible"
baz$ABOJEG[which(baz$ABODOND1=="B" & baz$ABOPAT=="A")]<-"Bidirectional" # Decision Selim
baz$ABOJEG[which(baz$ABODOND1=="AB" & baz$ABOPAT=="A")]<-"Major incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="O" & baz$ABOPAT=="A")]<-"Minor incompatibility"

baz$ABOJEG[which(baz$ABODOND1=="A" & baz$ABOPAT=="B")]<-"Bidirectional"# Decision Selim
baz$ABOJEG[which(baz$ABODOND1=="B" & baz$ABOPAT=="B")]<-"Compatible"
baz$ABOJEG[which(baz$ABODOND1=="AB" & baz$ABOPAT=="B")]<-"Major incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="O" & baz$ABOPAT=="B")]<-"Minor incompatibility"

baz$ABOJEG[which(baz$ABODOND1=="A" & baz$ABOPAT=="AB")]<-"Minor incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="B" & baz$ABOPAT=="AB")]<-"Minor incompatibility"
baz$ABOJEG[which(baz$ABODOND1=="AB" & baz$ABOPAT=="AB")]<-"Compatible"
baz$ABOJEG[which(baz$ABODOND1=="O" & baz$ABOPAT=="AB")]<-"Minor incompatibility"



# Donor 1
baz$HLASEROAF[which(baz$HLASEROAF=="-1")]<-NA
baz$HLASEROBF[which(baz$HLASEROBF=="-1")]<-NA
baz$HLAMISMDR[which(baz$HLAMISMDR=="-1")]<-NA
baz$HLAMISMDRF<-baz$HLAMISMDR
baz$HLAMISMDRF[which(is.na(baz$HLAMISMDR) & baz$MMALLDRD1=="None")]<-0
baz$HLAMISMDRF[which(is.na(baz$HLAMISMDR) & baz$MMALLDRD1=="1")]<-1
baz$HLAMISMDRF[which(is.na(baz$HLAMISMDR) & baz$MMALLDRD1=="2")]<-2

baz$NRMISM66CB1JEG<-baz$HLASEROAF+baz$HLASEROBF+baz$HLAMISMDRF

# Donor 2 # Manque la comparaison sero donneur 2
if("HLASEROAF_2" %in% names(baz)){
baz$HLASEROAF_2[which(baz$HLASEROAF_2=="-1")]<-NA
baz$HLASEROBF_2[which(baz$HLASEROBF_2=="-1")]<-NA
baz$HLAMISMDR_2[which(baz$HLAMISMDR_2=="-1")]<-NA
baz$HLAMISMDR_2[which(baz$HLAMISMDR_2=="-1")]<-NA
baz$HLAMISMDRF_2<-baz$HLAMISMDR_2
baz$HLAMISMDRF_2[which(is.na(baz$HLAMISMDR_2) & baz$MMALLDRD2=="None")]<-0
baz$HLAMISMDRF_2[which(is.na(baz$HLAMISMDR_2) & baz$MMALLDRD2=="1")]<-1
baz$HLAMISMDRF_2[which(is.na(baz$HLAMISMDR_2) & baz$MMALLDRD2=="2")]<-2

baz$NRMISM66CB2JEG<-baz$HLASEROAF_2+baz$HLASEROBF_2+baz$HLAMISMDRF_2
}
baz$AMP1<-str_trim(baz$AMP1)
baz$AMP2<-str_trim(baz$AMP2)
baz$HLAAP1<-str_trim(baz$HLAAP1)
baz$HLAAP2<-str_trim(baz$HLAAP2)
baz$HLAAD1<-str_trim(baz$HLAAD1)
baz$HLAAD2<-str_trim(baz$HLAAD2)

baz$BMP1<-str_trim(baz$BMP1)
baz$BMP2<-str_trim(baz$BMP2)
baz$HLABP1<-str_trim(baz$HLABP1)
baz$HLABP2<-str_trim(baz$HLABP2)
baz$HLABD1<-str_trim(baz$HLABD1)
baz$HLABD2<-str_trim(baz$HLABD2)

baz$CMP1<-str_trim(baz$CMP1)
baz$CMP2<-str_trim(baz$CMP2)
baz$HLACP1<-str_trim(baz$HLACP1)
baz$HLACP2<-str_trim(baz$HLACP2)
baz$HLACD1<-str_trim(baz$HLACD1)
baz$HLACD2<-str_trim(baz$HLACD2)

###### Patients HLA A
baz$AMP1[which(baz$AMP1=="")]<-NA
baz$ANMDP1<-str_trim(baz$ANMDP1)
baz$AMP1[which(baz$AMP1=="MAC")]<-baz$ANMDP1[which(baz$AMP1=="MAC")]
baz$ASP1<-str_trim(baz$ASP1)
baz$ASP1[which(baz$ASP1=="")]<-NA
baz$AMP1[which(is.na(baz$AMP1) & !is.na(baz$ASP1))]<-baz$ASP1[which(is.na(baz$AMP1) & !is.na(baz$ASP1))]
baz$AMP1[which(baz$AMP1=="NE" & !is.na(baz$ASP1))]<-baz$ASP1[which(baz$AMP1=="NE" & !is.na(baz$ASP1))]

baz$AMP2[which(baz$AMP2=="")]<-NA
baz$ANMDP2<-str_trim(baz$ANMDP2)
baz$AMP2[which(baz$AMP2=="MAC")]<-baz$ANMDP2[which(baz$AMP2=="MAC")]
baz$ASP2<-str_trim(baz$ASP2)
baz$ASP2[which(baz$ASP2=="")]<-NA
baz$AMP2[which(is.na(baz$AMP2) & !is.na(baz$ASP2))]<-baz$ASP2[which(is.na(baz$AMP2) & !is.na(baz$ASP2))]
baz$AMP2[which(baz$AMP2=="NE" & !is.na(baz$ASP2))]<-baz$ASP2[which(baz$AMP2=="NE" & !is.na(baz$ASP2))]

#### Patient HLA B
baz$BMP1[which(baz$BMP1=="")]<-NA
baz$BNMDP1<-str_trim(baz$BNMDP1)
baz$BMP1[which(baz$BMP1=="MAC")]<-baz$BNMDP1[which(baz$BMP1=="MAC")]
baz$BSP1<-str_trim(baz$BSP1)
baz$BSP1[which(baz$BSP1=="")]<-NA
baz$BMP1[which(is.na(baz$BMP1) & !is.na(baz$BSP1))]<-baz$BSP1[which(is.na(baz$BMP1) & !is.na(baz$BSP1))]
baz$BMP1[which(baz$BMP1=="NE" & !is.na(baz$BSP1))]<-baz$BSP1[which(baz$BMP1=="NE" & !is.na(baz$BSP1))]

baz$BMP2[which(baz$BMP2=="")]<-NA
baz$BNMDP2<-str_trim(baz$BNMDP2)
baz$BMP2[which(baz$BMP2=="MAC")]<-baz$BNMDP2[which(baz$BMP2=="MAC")]
baz$BSP2<-str_trim(baz$BSP2)
baz$BSP2[which(baz$BSP2=="")]<-NA
baz$BMP2[which(is.na(baz$BMP2) & !is.na(baz$BSP2))]<-baz$BSP2[which(is.na(baz$BMP2) & !is.na(baz$BSP2))]
baz$BMP2[which(baz$BMP2=="NE" & !is.na(baz$BSP2))]<-baz$BSP2[which(baz$BMP2=="NE" & !is.na(baz$BSP2))]

#### Patient HLA C
baz$CMP1[which(baz$CMP1=="")]<-NA
baz$CNMDP1<-str_trim(baz$CNMDP1)
baz$CMP1[which(baz$CMP1=="MAC")]<-baz$CNMDP1[which(baz$CMP1=="MAC")]
baz$CSP1<-str_trim(baz$CSP1)
baz$CSP1[which(baz$CSP1=="")]<-NA
baz$CMP1[which(is.na(baz$CMP1) & !is.na(baz$CSP1))]<-baz$CSP1[which(is.na(baz$CMP1) & !is.na(baz$CSP1))]
baz$CMP1[which(baz$CMP1=="NE" & !is.na(baz$CSP1))]<-baz$CSP1[which(baz$CMP1=="NE" & !is.na(baz$CSP1))]

baz$CMP2[which(baz$CMP2=="")]<-NA
baz$CNMDP2<-str_trim(baz$CNMDP2)
baz$CMP2[which(baz$CMP2=="MAC")]<-baz$CNMDP2[which(baz$CMP2=="MAC")]
baz$CSP2<-str_trim(baz$CSP2)
baz$CSP2[which(baz$CSP2=="")]<-NA
baz$CMP2[which(is.na(baz$CMP2) & !is.na(baz$CSP2))]<-baz$CSP2[which(is.na(baz$CMP2) & !is.na(baz$CSP2))]
baz$CMP2[which(baz$CMP2=="NE" & !is.na(baz$CSP2))]<-baz$CSP2[which(baz$CMP2=="NE" & !is.na(baz$CSP2))]


##### Patient DRB1
baz$DRBNMDP1<-str_trim(baz$DRBNMDP1)
baz$DRB1MP1<-str_trim(baz$DRB1MP1)
baz$DRB1MP1[which(baz$DRB1MP1=="")]<-NA
baz$DRB1MP1[which(baz$DRB1MP1=="MAC")]<-baz$DRBNMDP1[which(baz$DRB1MP1=="MAC")]
baz$DRB1SP1<-str_trim(baz$DRB1SP1)
baz$DRB1SP1[which(baz$DRB1SP1=="")]<-NA
baz$DRB1MP1[which(is.na(baz$DRB1MP1) & !is.na(baz$DRB1SP1))]<-baz$DRB1SP1[which(is.na(baz$DRB1MP1) & !is.na(baz$DRB1SP1))]
baz$DRB1MP1[which(baz$DRB1MP1=="NE" & !is.na(baz$DRB1SP1))]<-baz$DRB1SP1[which(baz$DRB1MP1=="NE" & !is.na(baz$DRB1SP1))]

baz$DRBNMDP2<-str_trim(baz$DRBNMDP2)
baz$DRB1MP2<-str_trim(baz$DRB1MP2)
baz$DRB1MP2[which(baz$DRB1MP2=="")]<-NA
baz$DRB1MP2[which(baz$DRB1MP2=="MAC")]<-baz$DRBNMDP2[which(baz$DRB1MP2=="MAC")]
baz$DRB1SP2<-str_trim(baz$DRB1SP2)
baz$DRB1SP2[which(baz$DRB1SP2=="")]<-NA
baz$DRB1MP2[which(is.na(baz$DRB1MP2) & !is.na(baz$DRB1SP2))]<-baz$DRB1SP2[which(is.na(baz$DRB1MP2) & !is.na(baz$DRB1SP2))]
baz$DRB1MP2[which(baz$DRB1MP2=="NE" & !is.na(baz$DRB1SP2))]<-baz$DRB1SP2[which(baz$DRB1MP2=="NE" & !is.na(baz$DRB1SP2))]


##### Patient DQB1
baz$DQBNMDP1<-str_trim(baz$DQBNMDP1)
baz$DQB1MP1<-str_trim(baz$DQB1MP1)
baz$DQB1MP1[which(baz$DQB1MP1=="")]<-NA
baz$DQB1MP1[which(baz$DQB1MP1=="MAC")]<-baz$DQBNMDP1[which(baz$DQB1MP1=="MAC")]
baz$DQB1SP1<-str_trim(baz$DQB1SP1)
baz$DQB1SP1[which(baz$DQB1SP1=="")]<-NA
baz$DQB1MP1[which(is.na(baz$DQB1MP1) & !is.na(baz$DQB1SP1))]<-baz$DQB1SP1[which(is.na(baz$DQB1MP1) & !is.na(baz$DQB1SP1))]
baz$DQB1MP1[which(baz$DQB1MP1=="NE" & !is.na(baz$DQB1SP1))]<-baz$DQB1SP1[which(baz$DQB1MP1=="NE" & !is.na(baz$DQB1SP1))]

baz$DQBNMDP2<-str_trim(baz$DQBNMDP2)
baz$DQB1MP2<-str_trim(baz$DQB1MP2)
baz$DQB1MP2[which(baz$DQB1MP2=="")]<-NA
baz$DQB1MP2[which(baz$DQB1MP2=="MAC")]<-baz$DQBNMDP2[which(baz$DQB1MP2=="MAC")]
baz$DQB1SP2<-str_trim(baz$DQB1SP2)
baz$DQB1SP2[which(baz$DQB1SP2=="")]<-NA
baz$DQB1MP2[which(is.na(baz$DQB1MP2) & !is.na(baz$DQB1SP2))]<-baz$DQB1SP2[which(is.na(baz$DQB1MP2) & !is.na(baz$DQB1SP2))]
baz$DQB1MP2[which(baz$DQB1MP2=="NE" & !is.na(baz$DQB1SP2))]<-baz$DQB1SP2[which(baz$DQB1MP2=="NE" & !is.na(baz$DQB1SP2))]


##### Patient DPB1
baz$DPBNMDP1<-str_trim(baz$DPBNMDP1)
baz$DPB1MP1<-str_trim(baz$DPB1MP1)
baz$DPB1MP1[which(baz$DPB1MP1=="")]<-NA
baz$DPB1MP1[which(baz$DPB1MP1=="MAC")]<-baz$DPBNMDP1[which(baz$DPB1MP1=="MAC")]
baz$DPB1SP1<-str_trim(baz$DPB1SP1)
baz$DPB1SP1[which(baz$DPB1SP1=="")]<-NA
baz$DPB1MP1[which(is.na(baz$DPB1MP1) & !is.na(baz$DPB1SP1))]<-baz$DPB1SP1[which(is.na(baz$DPB1MP1) & !is.na(baz$DPB1SP1))]
baz$DPB1MP1[which(baz$DPB1MP1=="NE" & !is.na(baz$DPB1SP1))]<-baz$DPB1SP1[which(baz$DPB1MP1=="NE" & !is.na(baz$DPB1SP1))]

baz$DPBNMDP2<-str_trim(baz$DPBNMDP2)
baz$DPB1MP2<-str_trim(baz$DPB1MP2)
baz$DPB1MP2[which(baz$DPB1MP2=="")]<-NA
baz$DPB1MP2[which(baz$DPB1MP2=="MAC")]<-baz$DPBNMDP2[which(baz$DPB1MP2=="MAC")]
baz$DPB1SP2<-str_trim(baz$DPB1SP2)
baz$DPB1SP2[which(baz$DPB1SP2=="")]<-NA
baz$DPB1MP2[which(is.na(baz$DPB1MP2) & !is.na(baz$DPB1SP2))]<-baz$DPB1SP2[which(is.na(baz$DPB1MP2) & !is.na(baz$DPB1SP2))]
baz$DPB1MP2[which(baz$DPB1MP2=="NE" & !is.na(baz$DPB1SP2))]<-baz$DPB1SP2[which(baz$DPB1MP2=="NE" & !is.na(baz$DPB1SP2))]



##### Donor 1 HLA A
baz$AMD1_DO1<-str_trim(baz$AMD1_DO1)
baz$AMD1_DO1[which(baz$AMD1_DO1=="")]<-NA
baz$ANMDD1_DO1<-str_trim(baz$ANMDD1_DO1)
baz$AMD1_DO1[which(baz$AMD1_DO1=="MAC")]<-baz$ANMDD1_DO1[which(baz$AMD1_DO1=="MAC")]
baz$ASD1_DO1<-str_trim(baz$ASD1_DO1)
baz$ASD1_DO1[which(baz$ASD1_DO1=="")]<-NA
baz$AMD1_DO1[which(is.na(baz$AMD1_DO1) & !is.na(baz$ASD1_DO1))]<-baz$ASD1_DO1[which(is.na(baz$AMD1_DO1) & !is.na(baz$ASD1_DO1))]
baz$AMD1_DO1[which(baz$AMD1_DO1=="NE" & !is.na(baz$ASD1_DO1))]<-baz$ASD1_DO1[which(baz$AMD1_DO1=="NE" & !is.na(baz$ASD1_DO1))]

baz$AMD2_DO1<-str_trim(baz$AMD2_DO1)
baz$AMD2_DO1[which(baz$AMD2_DO1=="")]<-NA
baz$ANMDD2_DO1<-str_trim(baz$ANMDD2_DO1)
baz$AMD2_DO1[which(baz$AMD2_DO1=="MAC")]<-baz$ANMDD2_DO1[which(baz$AMD2_DO1=="MAC")]
baz$ASD2_DO1<-str_trim(baz$ASD2_DO1)
baz$ASD2_DO1[which(baz$ASD2_DO1=="")]<-NA
baz$AMD2_DO1[which(is.na(baz$AMD2_DO1) & !is.na(baz$ASD2_DO1))]<-baz$ASD2_DO1[which(is.na(baz$AMD2_DO1) & !is.na(baz$ASD2_DO1))]
baz$AMD2_DO1[which(baz$AMD2_DO1=="NE" & !is.na(baz$ASD2_DO1))]<-baz$ASD2_DO1[which(baz$AMD2_DO1=="NE" & !is.na(baz$ASD2_DO1))]

##### Donor 2 HLA A
baz$AMD1_DO2<-str_trim(baz$AMD1_DO2)
baz$AMD1_DO2[which(baz$AMD1_DO2=="")]<-NA
baz$ANMDD1_DO2<-str_trim(baz$ANMDD1_DO2)
baz$AMD1_DO2[which(baz$AMD1_DO2=="MAC")]<-baz$ANMDD1_DO2[which(baz$AMD1_DO2=="MAC")]
baz$ASD1_DO2<-str_trim(baz$ASD1_DO2)
baz$ASD1_DO2[which(baz$ASD1_DO2=="")]<-NA
baz$AMD1_DO2[which(is.na(baz$AMD1_DO2) & !is.na(baz$ASD1_DO2))]<-baz$ASD1_DO2[which(is.na(baz$AMD1_DO2) & !is.na(baz$ASD1_DO2))]
baz$AMD1_DO2[which(baz$AMD1_DO2=="NE" & !is.na(baz$ASD1_DO2))]<-baz$ASD1_DO2[which(baz$AMD1_DO2=="NE" & !is.na(baz$ASD1_DO2))]

baz$AMD2_DO2<-str_trim(baz$AMD2_DO2)
baz$AMD2_DO2[which(baz$AMD2_DO2=="")]<-NA
baz$ANMDD2_DO2<-str_trim(baz$ANMDD2_DO2)
baz$AMD2_DO2[which(baz$AMD2_DO2=="MAC")]<-baz$ANMDD2_DO2[which(baz$AMD2_DO2=="MAC")]
baz$ASD2_DO2<-str_trim(baz$ASD2_DO2)
baz$ASD2_DO2[which(baz$ASD2_DO2=="")]<-NA
baz$AMD2_DO2[which(is.na(baz$AMD2_DO2) & !is.na(baz$ASD2_DO2))]<-baz$ASD2_DO2[which(is.na(baz$AMD2_DO2) & !is.na(baz$ASD2_DO2))]
baz$AMD2_DO2[which(baz$AMD2_DO2=="NE" & !is.na(baz$ASD2_DO2))]<-baz$ASD2_DO2[which(baz$AMD2_DO2=="NE" & !is.na(baz$ASD2_DO2))]

##### Donor 1 HLA B
baz$BMD1_DO1<-str_trim(baz$BMD1_DO1)
baz$BMD1_DO1[which(baz$BMD1_DO1=="")]<-NA
baz$BNMDD1_DO1<-str_trim(baz$BNMDD1_DO1)
baz$BMD1_DO1[which(baz$BMD1_DO1=="MAC")]<-baz$BNMDD1_DO1[which(baz$BMD1_DO1=="MAC")]
baz$BSD1_DO1<-str_trim(baz$BSD1_DO1)
baz$BSD1_DO1[which(baz$BSD1_DO1=="")]<-NA
baz$BMD1_DO1[which(is.na(baz$BMD1_DO1) & !is.na(baz$BSD1_DO1))]<-baz$BSD1_DO1[which(is.na(baz$BMD1_DO1) & !is.na(baz$BSD1_DO1))]
baz$BMD1_DO1[which(baz$BMD1_DO1=="NE" & !is.na(baz$BSD1_DO1))]<-baz$BSD1_DO1[which(baz$BMD1_DO1=="NE" & !is.na(baz$BSD1_DO1))]

baz$BMD2_DO1<-str_trim(baz$BMD2_DO1)
baz$BMD2_DO1[which(baz$BMD2_DO1=="")]<-NA
baz$BNMDD2_DO1<-str_trim(baz$BNMDD2_DO1)
baz$BMD2_DO1[which(baz$BMD2_DO1=="MAC")]<-baz$BNMDD2_DO1[which(baz$BMD2_DO1=="MAC")]
baz$BSD2_DO1<-str_trim(baz$BSD2_DO1)
baz$BSD2_DO1[which(baz$BSD2_DO1=="")]<-NA
baz$BMD2_DO1[which(is.na(baz$BMD2_DO1) & !is.na(baz$BSD2_DO1))]<-baz$BSD2_DO1[which(is.na(baz$BMD2_DO1) & !is.na(baz$BSD2_DO1))]
baz$BMD2_DO1[which(baz$BMD2_DO1=="NE" & !is.na(baz$BSD2_DO1))]<-baz$BSD2_DO1[which(baz$BMD2_DO1=="NE" & !is.na(baz$BSD2_DO1))]

##### Donor 2 HLA B
baz$BMD1_DO2<-str_trim(baz$BMD1_DO2)
baz$BMD1_DO2[which(baz$BMD1_DO2=="")]<-NA
baz$BNMDD1_DO2<-str_trim(baz$BNMDD1_DO2)
baz$BMD1_DO2[which(baz$BMD1_DO2=="MAC")]<-baz$BNMDD1_DO2[which(baz$BMD1_DO2=="MAC")]
baz$BSD1_DO2<-str_trim(baz$BSD1_DO2)
baz$BSD1_DO2[which(baz$BSD1_DO2=="")]<-NA
baz$BMD1_DO2[which(is.na(baz$BMD1_DO2) & !is.na(baz$BSD1_DO2))]<-baz$BSD1_DO2[which(is.na(baz$BMD1_DO2) & !is.na(baz$BSD1_DO2))]
baz$BMD1_DO2[which(baz$BMD1_DO2=="NE" & !is.na(baz$BSD1_DO2))]<-baz$BSD1_DO2[which(baz$BMD1_DO2=="NE" & !is.na(baz$BSD1_DO2))]

baz$BMD2_DO2<-str_trim(baz$BMD2_DO2)
baz$BMD2_DO2[which(baz$BMD2_DO2=="")]<-NA
baz$BNMDD2_DO2<-str_trim(baz$BNMDD2_DO2)
baz$BMD2_DO2[which(baz$BMD2_DO2=="MAC")]<-baz$BNMDD2_DO2[which(baz$BMD2_DO2=="MAC")]
baz$BSD2_DO2<-str_trim(baz$BSD2_DO2)
baz$BSD2_DO2[which(baz$BSD2_DO2=="")]<-NA
baz$BMD2_DO2[which(is.na(baz$BMD2_DO2) & !is.na(baz$BSD2_DO2))]<-baz$BSD2_DO2[which(is.na(baz$BMD2_DO2) & !is.na(baz$BSD2_DO2))]
baz$BMD2_DO2[which(baz$BMD2_DO2=="NE" & !is.na(baz$BSD2_DO2))]<-baz$BSD2_DO2[which(baz$BMD2_DO2=="NE" & !is.na(baz$BSD2_DO2))]


##### Donor 1 HLA C
baz$CMD1_DO1<-str_trim(baz$CMD1_DO1)
baz$CMD1_DO1[which(baz$CMD1_DO1=="")]<-NA
baz$CNMDD1_DO1<-str_trim(baz$CNMDD1_DO1)
baz$CMD1_DO1[which(baz$CMD1_DO1=="MAC")]<-baz$CNMDD1_DO1[which(baz$CMD1_DO1=="MAC")]
baz$CSD1_DO1<-str_trim(baz$CSD1_DO1)
baz$CSD1_DO1[which(baz$CSD1_DO1=="")]<-NA
baz$CMD1_DO1[which(is.na(baz$CMD1_DO1) & !is.na(baz$CSD1_DO1))]<-baz$CSD1_DO1[which(is.na(baz$CMD1_DO1) & !is.na(baz$CSD1_DO1))]
baz$CMD1_DO1[which(baz$CMD1_DO1=="NE" & !is.na(baz$CSD1_DO1))]<-baz$CSD1_DO1[which(baz$CMD1_DO1=="NE" & !is.na(baz$CSD1_DO1))]

baz$CMD2_DO1<-str_trim(baz$CMD2_DO1)
baz$CMD2_DO1[which(baz$CMD2_DO1=="")]<-NA
baz$CNMDD2_DO1<-str_trim(baz$CNMDD2_DO1)
baz$CMD2_DO1[which(baz$CMD2_DO1=="MAC")]<-baz$CNMDD2_DO1[which(baz$CMD2_DO1=="MAC")]
baz$CSD2_DO1<-str_trim(baz$CSD2_DO1)
baz$CSD2_DO1[which(baz$CSD2_DO1=="")]<-NA
baz$CMD2_DO1[which(is.na(baz$CMD2_DO1) & !is.na(baz$CSD2_DO1))]<-baz$CSD2_DO1[which(is.na(baz$CMD2_DO1) & !is.na(baz$CSD2_DO1))]
baz$CMD2_DO1[which(baz$CMD2_DO1=="NE" & !is.na(baz$CSD2_DO1))]<-baz$CSD2_DO1[which(baz$CMD2_DO1=="NE" & !is.na(baz$CSD2_DO1))]


##### Donor 1 HLA DR
baz$DRB1MD1_DO1<-str_trim(baz$DRB1MD1_DO1)
baz$DRB1MD1_DO1[which(baz$DRB1MD1_DO1=="")]<-NA
baz$DRBNMDD1_DO1<-str_trim(baz$DRBNMDD1_DO1)
baz$DRB1MD1_DO1[which(baz$DRB1MD1_DO1=="MAC")]<-baz$DRBNMDD1_DO1[which(baz$DRB1MD1_DO1=="MAC")]
baz$DRB1SD1_DO1<-str_trim(baz$DRB1SD1_DO1)
baz$DRB1SD1_DO1[which(baz$DRB1SD1_DO1=="")]<-NA
baz$DRB1MD1_DO1[which(is.na(baz$DRB1MD1_DO1) & !is.na(baz$DRB1SD1_DO1))]<-baz$DRB1SD1_DO1[which(is.na(baz$DRB1MD1_DO1) & !is.na(baz$DRB1SD1_DO1))]
baz$DRB1MD1_DO1[which(baz$DRB1MD1_DO1=="NE" & !is.na(baz$DRB1SD1_DO1))]<-baz$DRB1SD1_DO1[which(baz$DRB1MD1_DO1=="NE" & !is.na(baz$DRB1SD1_DO1))]

baz$DRB1MD2_DO1<-str_trim(baz$DRB1MD2_DO1)
baz$DRB1MD2_DO1[which(baz$DRB1MD2_DO1=="")]<-NA
baz$DRBNMDD2_DO1<-str_trim(baz$DRBNMDD2_DO1)
baz$DRB1MD2_DO1[which(baz$DRB1MD2_DO1=="MAC")]<-baz$DRBNMDD2_DO1[which(baz$DRB1MD2_DO1=="MAC")]
baz$DRB1SD2_DO1<-str_trim(baz$DRB1SD2_DO1)
baz$DRB1SD2_DO1[which(baz$DRB1SD2_DO1=="")]<-NA
baz$DRB1MD2_DO1[which(is.na(baz$DRB1MD2_DO1) & !is.na(baz$DRB1SD2_DO1))]<-baz$DRB1SD2_DO1[which(is.na(baz$DRB1MD2_DO1) & !is.na(baz$DRB1SD2_DO1))]
baz$DRB1MD2_DO1[which(baz$DRB1MD2_DO1=="NE" & !is.na(baz$DRB1SD2_DO1))]<-baz$DRB1SD2_DO1[which(baz$DRB1MD2_DO1=="NE" & !is.na(baz$DRB1SD2_DO1))]


##### Donor 1 HLA DQ
baz$DQB1MD1_DO1<-str_trim(baz$DQB1MD1_DO1)
baz$DQB1MD1_DO1[which(baz$DQB1MD1_DO1=="")]<-NA
baz$DQBNMDD1_DO1<-str_trim(baz$DQBNMDD1_DO1)
baz$DQB1MD1_DO1[which(baz$DQB1MD1_DO1=="MAC")]<-baz$DQBNMDD1_DO1[which(baz$DQB1MD1_DO1=="MAC")]
baz$DQB1SD1_DO1<-str_trim(baz$DQB1SD1_DO1)
baz$DQB1SD1_DO1[which(baz$DQB1SD1_DO1=="")]<-NA
baz$DQB1MD1_DO1[which(is.na(baz$DQB1MD1_DO1) & !is.na(baz$DQB1SD1_DO1))]<-baz$DQB1SD1_DO1[which(is.na(baz$DQB1MD1_DO1) & !is.na(baz$DQB1SD1_DO1))]
baz$DQB1MD1_DO1[which(baz$DQB1MD1_DO1=="NE" & !is.na(baz$DQB1SD1_DO1))]<-baz$DQB1SD1_DO1[which(baz$DQB1MD1_DO1=="NE" & !is.na(baz$DQB1SD1_DO1))]

baz$DQB1MD2_DO1<-str_trim(baz$DQB1MD2_DO1)
baz$DQB1MD2_DO1[which(baz$DQB1MD2_DO1=="")]<-NA
baz$DQBNMDD2_DO1<-str_trim(baz$DQBNMDD2_DO1)
baz$DQB1MD2_DO1[which(baz$DQB1MD2_DO1=="MAC")]<-baz$DQBNMDD2_DO1[which(baz$DQB1MD2_DO1=="MAC")]
baz$DQB1SD2_DO1<-str_trim(baz$DQB1SD2_DO1)
baz$DQB1SD2_DO1[which(baz$DQB1SD2_DO1=="")]<-NA
baz$DQB1MD2_DO1[which(is.na(baz$DQB1MD2_DO1) & !is.na(baz$DQB1SD2_DO1))]<-baz$DQB1SD2_DO1[which(is.na(baz$DQB1MD2_DO1) & !is.na(baz$DQB1SD2_DO1))]
baz$DQB1MD2_DO1[which(baz$DQB1MD2_DO1=="NE" & !is.na(baz$DQB1SD2_DO1))]<-baz$DQB1SD2_DO1[which(baz$DQB1MD2_DO1=="NE" & !is.na(baz$DQB1SD2_DO1))]

##### Donor 1 HLA DP
baz$DPB1MD1_DO1<-str_trim(baz$DPB1MD1_DO1)
baz$DPB1MD1_DO1[which(baz$DPB1MD1_DO1=="")]<-NA
baz$DPBNMDD1_DO1<-str_trim(baz$DPBNMDD1_DO1)
baz$DPB1MD1_DO1[which(baz$DPB1MD1_DO1=="MAC")]<-baz$DPBNMDD1_DO1[which(baz$DPB1MD1_DO1=="MAC")]
baz$DPB1SD1_DO1<-str_trim(baz$DPB1SD1_DO1)
baz$DPB1SD1_DO1[which(baz$DPB1SD1_DO1=="")]<-NA
baz$DPB1MD1_DO1[which(is.na(baz$DPB1MD1_DO1) & !is.na(baz$DPB1SD1_DO1))]<-baz$DPB1SD1_DO1[which(is.na(baz$DPB1MD1_DO1) & !is.na(baz$DPB1SD1_DO1))]
baz$DPB1MD1_DO1[which(baz$DPB1MD1_DO1=="NE" & !is.na(baz$DPB1SD1_DO1))]<-baz$DPB1SD1_DO1[which(baz$DPB1MD1_DO1=="NE" & !is.na(baz$DPB1SD1_DO1))]

baz$DPB1MD2_DO1<-str_trim(baz$DPB1MD2_DO1)
baz$DPB1MD2_DO1[which(baz$DPB1MD2_DO1=="")]<-NA
baz$DPBNMDD2_DO1<-str_trim(baz$DPBNMDD2_DO1)
baz$DPB1MD2_DO1[which(baz$DPB1MD2_DO1=="MAC")]<-baz$DPBNMDD2_DO1[which(baz$DPB1MD2_DO1=="MAC")]
baz$DPB1SD2_DO1<-str_trim(baz$DPB1SD2_DO1)
baz$DPB1SD2_DO1[which(baz$DPB1SD2_DO1=="")]<-NA
baz$DPB1MD2_DO1[which(is.na(baz$DPB1MD2_DO1) & !is.na(baz$DPB1SD2_DO1))]<-baz$DPB1SD2_DO1[which(is.na(baz$DPB1MD2_DO1) & !is.na(baz$DPB1SD2_DO1))]
baz$DPB1MD2_DO1[which(baz$DPB1MD2_DO1=="NE" & !is.na(baz$DPB1SD2_DO1))]<-baz$DPB1SD2_DO1[which(baz$DPB1MD2_DO1=="NE" & !is.na(baz$DPB1SD2_DO1))]





for(i in c("HLAMISMA","HLAMISMB","HLAMISMC","HLAMISMDR","HLAMISMDQ","HLAMISMDP")){
  temp<-baz[,i]
  baz[which(temp==-1),i]<-NA
}

baz$HSCTYPE2<-as.character(baz$HSCTYPE)
baz$HSCTYPE2[which(baz$CELLSOURCE=="CB" & (baz$DONRL1=="Identical sibling"))]<-"CB (MSD)"
baz$HSCTYPE2[which(baz$CELLSOURCE=="CB" & (baz$DONRL1=="Syngeneic"))]<-"CB (syng)"
baz$HSCTYPE2[which(baz$CELLSOURCE=="CB" & (baz$DONRL1=="Matched other relative"))]<-"CB (MOR)"
baz$HSCTYPE2[which(baz$CELLSOURCE=="CB" & (baz$DONRL1=="Mismatched relative"))]<-"CB (MMR)"

baz$HSCTYPE2[which(baz$CELLSOURCE=="CB" & (baz$DONRL1=="Matched unrelated"|
                                                   baz$DONRL1=="Mismatched unrelated"|
                                                   baz$DONRL1=="Unrelated"))]<-"Unrelated CB"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB" & baz$NRMISM66CB1JEG==0)]<-"UCB 6/6"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB" & baz$NRMISM66CB1JEG==1)]<-"UCB 5/6"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB" & baz$NRMISM66CB1JEG >=2)]<-"UCB <=4/6"
# baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB" & (!is.na(baz$NRMISM8DQ) | 
#                                                            !is.na(baz$NRMISM8C) | 
#                                                            !is.na(baz$NRMISM66)))]<-"UCB code < 10 loci"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB" & baz$DONRL1=="Matched unrelated")]<-"Match UCB (missing HLA)"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB" & baz$DONRL1=="Mismatched unrelated")]<-"Mismatch UCB (missing HLA)"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated CB")]<-"UCB (missing HLA)"

baz$HSCTYPE2[which(baz$CELLSOURCE=="Double CB" & (baz$DONRL1=="Matched unrelated"|
                                                          baz$DONRL1=="Mismatched unrelated"|
                                                          baz$DONRL1=="Unrelated") &
                        (baz$DONRL2=="Matched unrelated"|
                           baz$DONRL2=="Mismatched unrelated"|
                           baz$DONRL2=="Unrelated")
)]<-"Double UCB"

baz$HSCTYPE2[which(baz$HSCTYPE=="Unrelated (not HLA typing to check (old code))" & (!is.na(baz$NRMISM66)|
                                                                                            !is.na(baz$NRMISM8C)|
                                                                                            !is.na(baz$NRMISM8DQ)))]<-"Unrelated < 10 loci"

baz$HSCTYPE2[which(baz$HSCTYPE=="Mismatched relative (cross over [only one mismatch] or missing nb of mismatches)" &
                        baz$ALLMISRLD1=="1 HLA locus mismatch")]<-"Mismatched relative 1 mismatch"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Mismatched relative (cross over [only one mismatch] or missing nb of mismatches)")]<-"Mismatched relative (missing HLA)"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Matched Unrelated 10/10 (A, B, C, DRb1 & DQb1)")]<-"UD 10/10"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Mismatched Unrelated 9/10 (A, B, C, DRb1 & DQb1)")]<-"UD 9/10"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Mismatched unrelated >=8/10  (A, B, C, DRb1 & DQb1)")]<-"UD <=8/10"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated (not HLA typing to check (old code))" & baz$DONRL1=="Matched unrelated")]<-"Match UD (missing HLA)"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated (not HLA typing to check (old code))"& baz$DONRL1=="Mismatched unrelated")]<-"Mismatch UD (missing HLA)"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated (not HLA typing to check (old code))")]<-"UD (missing HLA)"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Identical sibling")]<-"MSD"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Mismatched relative (missing HLA)")]<-"MMR (missing HLA)"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Mismatched relative 1 mismatch")]<-"MMR 1 locus"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Haploidentical HSCT")]<-"Haplo"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Unrelated < 10 loci")]<-"UD < 10 loci"
baz$HSCTYPE2[which(baz$HSCTYPE2=="Matched other relative")]<-"MOR"

baz$HSCTYPE2[which(baz$HSCTYPE2=="Single CB-HSCT" & is.na(baz$DONRL1))]<-"CB (missing donor type)"


baz$HSCTYPE3<-baz$HSCTYPE2

UCB <- c("Unrelated CB","UCB 6/6","UCB 5/6","UCB <=4/6",
         "Match UCB (missing HLA)",
         "Mismatch UCB (missing HLA)","UCB (missing HLA)")

baz$HSCTYPE3[which(baz$HSCTYPE2 %in% UCB )]<-"UCB"

RCB<- c("CB (MSD)","CB (syng)","CB (MOR)","CB (MMR)")

baz$HSCTYPE3[which(baz$HSCTYPE2 %in% RCB )]<-"CB relative"
baz$HSCTYPE3[which(baz$HSCTYPE2=="UD < 10 loci")]<-"UD (missing HLA)"
baz$HSCTYPE3[which(baz$HSCTYPE2=="Match UD (missing HLA)")]<-"UD (missing HLA)"
baz$HSCTYPE3[which(baz$HSCTYPE2=="Mismatch UD (missing HLA)")]<-"UD (missing HLA)"
baz$HSCTYPE3[which(baz$HSCTYPE2=="Double UCB")]<-"UCB"
baz$HSCTYPE3[which(baz$HSCTYPE2=="Syngeneic")]<-"MSD"

baz$DONRL1JEG<-as.character(baz$DONRL1)
baz$DONRL1JEG[which(baz$DONRL1=="unknown")]<-NA
baz$DONRL1JEG[which(baz$DONRL1=="Identical sibling")]<-"MSD"
baz$DONRL1JEG[which(baz$DONRL1=="Syng")]<-"Syng"
baz$DONRL1JEG[which(baz$DONRL1=="Matched other relative")]<-"MOR"
baz$DONRL1JEG[which(baz$DONRL1=="Mismatched relative")]<-"MMR"
baz$DONRL1JEG[which(baz$DONRL1=="Matched unrelated")]<-"UD"
baz$DONRL1JEG[which(baz$DONRL1=="Mismatched unrelated")]<-"UD"
baz$DONRL1JEG[which(baz$DONRL1=="Unrelated")]<-"UD"

baz$VDIAGTX<-str_trim(baz$VDIAGTX)
baz$VDIAGTX[which(baz$VDIAGTX=="")]<-NA


baz$VACLEUK[which(baz$VACLEUK=="unknown")]<-NA

baz$DISJEG<-as.character(baz$DISMCLFD)
baz$DISJEG[which(baz$DISMCLFD=="Acute leukaemia")]<-"AL others"
baz$DISJEG[which(baz$DISMCLFD=="Acute leukaemia" & baz$VACLEUK=="AML & Related Precursor Neoplasms")]<-"AML"
baz$DISJEG[which(baz$DISMCLFD=="Acute leukaemia" & baz$VACLEUK=="Precursor Lymphoid Neoplasms (old ALL)")]<-"ALL"
baz$DISJEG[which(baz$DISMCLFD=="Acute leukaemia" & is.na(baz$VACLEUK))]<-"AL (missing classification)"

# Variable Malignancy

baz$MALIGNANCY<- NA
baz$MALIGNANCY[baz$DISMCLFD=="Acute leukaemia"] <-"Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Chronic leukaemia"] <-"Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Lymphoma"] <-"Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Plasma cell disorders"] <-"Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Solid tumours"] <-"Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Myelodysplastic/Myeloproliferative"] <-"Malignant"

baz$MALIGNANCY[baz$DISMCLFD=="Bone marrow failure"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Inherited disorders"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Histiocytic disorders"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Auto-immune diseases"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Hemoglobinopathies"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Neurologic disorder"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Heart (cardiovascular) disease"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Infection"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Muscoskeletal disorder"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Ocular disease"] <-"Non-Malignant"
baz$MALIGNANCY[baz$DISMCLFD=="Pulmonary disease"] <-"Non-Malignant"
baz$MALIGNANCY<-as.factor(baz$MALIGNANCY)


baz$BMTDISESTA<-as.character(baz$BMTDISESTA)
baz$BMTDISESTA[baz$BMTDISESTA=="unknown"]<-NA
baz$BMTDISESTA[baz$BMTDISESTA=="Not evaluated"]<-NA
baz$BMTDISESTA[baz$BMTDISESTA=="Not evaluable"]<-NA
baz$BMTDISESTA<-as.factor(baz$BMTDISESTA)

baz$BMTNUMSTM<-as.character(baz$BMTNUMSTM)
baz$BMTNUMSTM[baz$BMTNUMSTM=="unknown"]<-NA
baz$BMTNUMSTM[baz$BMTNUMSTM=="Not evaluated"]<-NA
baz$BMTNUMSTM<-as.factor(baz$BMTNUMSTM)

baz$BMTMOLECREJEG<-as.character(baz$BMTMOLECRE)
baz$BMTMOLECREJEG[baz$BMTMOLECRE=="unknown"]<-NA
baz$BMTMOLECREJEG[baz$BMTMOLECRE=="Not evaluated"]<-NA
baz$BMTMOLECREJEG[baz$BMTMOLECRE=="Not applicable: no previous abnormalities detected"]<-NA
baz$BMTMOLECREJEG<-as.factor(baz$BMTMOLECREJEG)

baz$ETAT<-NA
baz$ETAT[which(baz$BMTDISESTA=="Complete remission (CR)" & baz$BMTNUMSTM=="1st")]<-"CR1"
baz$ETAT[which(baz$BMTDISESTA=="Complete remission (CR)" & baz$BMTNUMSTM=="2nd")]<-"CR2"
baz$ETAT[which(baz$BMTDISESTA=="Complete remission (CR)" & baz$BMTNUMSTM=="3rd or higher")]<-"CR>=3"
baz$ETAT[which(baz$BMTDISESTA=="Primary induction failure / Primary refractory")]<-"advanced"
baz$ETAT[which(baz$BMTDISESTA=="Relapse")]<-"advanced"
baz$ETAT[which(baz$BMTDISESTA=="Progression / Progressive disease (PD)")]<-"advanced"
baz$ETAT<-as.factor(baz$ETAT)

baz$BMTDISESTAJEG<-paste(as.character(baz$BMTDISESTA),as.character(baz$BMTNUMSTM),sep="__")
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Complete remission (CR)__1st")]<-"CR1"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Complete remission (CR)__2nd")]<-"CR2"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Complete remission (CR)__3rd or higher")]<-"CR>=3"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Complete remission (CR)__NA")]<-"CR (missing)"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Complete remission (CR)__>1st")]<-"CR (missing)"

baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Relapse__1st")]<-"Rel1"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Relapse__2nd")]<-"Rel2"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Relapse__3rd or higher")]<-"Rel>=3"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Relapse__NA")]<-"Rel (missing)"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Relapse__>1st")]<-"Rel (missing)"

baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Progression / Progressive disease (PD)__1st")]<-"PD1"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Progression / Progressive disease (PD)__2nd")]<-"PD2"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Progression / Progressive disease (PD)__3rd or higher")]<-"PD>=3"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Progression / Progressive disease (PD)__NA")]<-"PD (missing)"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Progression / Progressive disease (PD)__>1st")]<-"PD (missing)"

baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="First partial remission (PR1)")]<-"PR1"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Partial remission __1st")]<-"PR1"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Partial remission __2nd")]<-"PR2"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Partial remission __3rd or higher")]<-"PR>=3"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Partial remission __NA")]<-"PR (missing)"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Partial remission __>1st")]<-"PR (missing)"

baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Never treated / Untreated / Upfront ")]<-"Never treated / Untreated / Upfront"

baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Primary induction failure / Primary refractory")]<-"Primary induct fail"

baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Chronic phase")]<-"Chronic phase"

baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Never in CR")]<-"Never/not in CR"
baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Not in CR")]<-"Never/not in CR"


baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Very good PR (VGPR)")]<-"Very good PR (VGPR)"
baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Response / Improvement (no CR)")]<-"Response / Improvement (no CR)"

baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Stable disease (no change, no response)__1st")]<-"Stable disease (no change, no response)"
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Stable disease (no change, no response)__NA")]<-"Stable disease (no change, no response)"


baz$BMTDISESTAJEG[which(baz$BMTDISESTA=="Other")]<-"Other"


baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="NA__1st")]<-NA
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="NA__2nd")]<-NA
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="NA__3rd or higher")]<-NA
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="NA__NA")]<-NA
baz$BMTDISESTAJEG[which(baz$BMTDISESTAJEG=="Not applicable__NA")]<-NA



baz$BMTDISESTAJEGBIS<-baz$BMTDISESTAJEG
baz$BMTDISESTAJEGBIS[which(baz$ETAT=="advanced")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Response / Improvement (no CR)")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Never treated / Untreated / Upfront")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Not in CR")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Not in CR")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Other")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="PR (missing)")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="PR (missing)")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Stable disease (no change, no response)")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Accelerated phase")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Chemorefractory relapse or progression, including primary refractory disease")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Chronic phase")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Never in CR")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="Never in CR")]<-"active disease"
baz$BMTDISESTAJEGBIS[which(baz$BMTDISESTAJEGBIS=="PR1")]<-"active disease"




baz$BMTDISESTAJEG2022<-as.character(baz$BMTDISESTAJEG)
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Untreated relapse (from a previous CR) or progression (from a previous PR)")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Secondary progressive")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Relapsing/remitting")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Relapse")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Progressive relapsing (malignant)")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Progression / Progressive disease (PD)")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Primary progressive")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Treatment not aimed at remission ")]<-"Treatment not aimed at remission"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Chemorefractory relapse or progression, including primary refractory disease")]<-"Rel/Prog"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Very good PR (VGPR)")]<-"PR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Partial remission ")]<-"PR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Nodular partial remission (nPR)")]<-"PR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Minor response (MR)")]<-"PR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Partial remission > 1 without a previous CR")]<-"PR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="First partial remission (PR1)")]<-"PR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Accelerated phase")]<-"Accelerated phase"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Blast crisis")]<-"Blast crisis"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Chronic phase")]<-"Chronic phase"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Never treated / Untreated / Upfront ")]<-"Upfront"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Stable disease (no change, no response)")]<-"Stable disease"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="stringent Complete remission (sCR)")]<-"sCR"
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTAJEG2022=="NA__>1st")]<-NA
baz$BMTDISESTAJEG2022[which(baz$BMTDISESTA=="Adjuvant")]<-NA

baz$MDSJEG<-as.character(baz$MDSSTAG)
baz$MDSJEG[which(baz$MDSSTAG=="RA with ring sideroblasts (RARS) ")]<-"RARS"
baz$MDSJEG[which(baz$MDSSTAG=="RA with excess of blasts-1 (RAEB-1) ")]<-"RAEB1"
baz$MDSJEG[which(baz$MDSSTAG=="RA with excess of blasts-1 (RAEB-1)")]<-"RAEB1"
baz$MDSJEG[which(baz$MDSSTAG=="RA with excess of blasts-2 (RAEB-2) ")]<-"RAEB2"
baz$MDSJEG[which(baz$MDSSTAG=="RA with excess of blasts-2 (RAEB-2)")]<-"RAEB2"
baz$MDSJEG[which(baz$MDSSTAG=="Childhood myelodysplastic syndrome (Refractory cytopenia of childhood (RCC))")]<-"RCC"
baz$MDSJEG[which(baz$MDSSTAG=="Old Refractory anaemia. No information on ring sideroblasts.")]<-"RA (old code)"
baz$MDSJEG[which(baz$MDSSTAG=="Refractory anaemia  (without ring sideroblasts) (RA)")]<-"RA"
baz$MDSJEG[which(baz$MDSSTAG=="RCMD with ring sideroblasts (RCMD-RS)")]<-"RCMD-RS"
baz$MDSJEG[which(baz$MDSSTAG=="Refractory cytopenia with multilineage dysplasia (RCMD)")]<-"RCMD"
baz$MDSJEG[which(baz$MDSSTAG=="MDS Unclassifiable (MDS-U) or other")]<-"MDS-U"
baz$MDSJEG[which(baz$MDSSTAG=="Myelodysplastic syndrome associated with isolated del(5q)")]<-"Isol. del5q"
baz$MDSJEG[which(baz$MDSSTAG=="Transformed to AML")]<-"sAML"
baz$MDSJEG[which(baz$MDSSTAG=="unknown")]<-NA

baz$MDSJEG[which(is.na(baz$MDSJEG) & baz$VMDS=="RA without ring sideroblasts")]<-"RA"
baz$MDSJEG[which(is.na(baz$MDSJEG) & baz$VMDS=="RA without ring sideroblasts ")]<-"RA"
baz$MDSJEG[which(is.na(baz$MDSJEG) & baz$VMDS=="RA with ring sideroblasts (RARS)")]<-"RARS"

baz$MDSJEG[which(is.na(baz$MDSJEG) & baz$VMDS=="RA with excess of blasts (RAEB)")]<-"RAEB"
baz$MDSJEG[which(is.na(baz$MDSJEG) & baz$VMDS=="RAEB in transformation (RAEB-t)")]<-"RAEB"
baz$MDSJEG[which(is.na(baz$MDSJEG) & baz$VMDS=="Transformed to AML (`secondary` acute leukaemia)")]<-"sAML"


baz$DISEASE<-as.character(baz$AACOD21)
baz$DISEASE[which(baz$DISEASE=="Myelodysplastic syndrome")]<-"MDS"

baz$DISEASE[which(baz$DISEASE=="MDS or MPN" & baz$VMDSMPS=="Myelodysplastic syndrome (MDS)")]<-"MDS"


baz$DISEASE[which(baz$DISEASE=="Myeloproliferative neoplasia")]<-"MPN"
baz$DISEASE[which(baz$DISEASE=="AML & Related Precursor Neoplasms")]<-"AML"
baz$DISEASE[which(baz$DISEASE=="Precursor Lymphoid Neoplasms")]<-"ALL"
baz$DISEASE[which(baz$DISEASE=="Multiple myeloma")]<-"MM"

baz$DISEASE[which(baz$DISEASE=="Secondary acute leukaemia" & 
                    baz$VACLEUK=="AML & Related Precursor Neoplasms")]<-"AML"

baz$DISEASE[which(baz$DISEASE=="Acute leukaemia" & 
                    baz$VACLEUK=="AML & Related Precursor Neoplasms")]<-"AML"

baz$DISEASE[which(baz$DISEASE=="Acute leukaemia" & 
                    baz$VACLEUK=="Precursor Lymphoid Neoplasms (old ALL)")]<-"ALL"


baz$DISEASE[which(baz$DISEASE=="Lymphoma" & 
                    baz$WHOLYCLS_DIAG=="Diffuse large B-cell lymphoma (DLBCL), NOS")]<-"NHL"

baz$DISEASE[which(baz$DISEASE %in% c("Lymphoma","NHL") & 
                    baz$WHOLYCLS_DIAG=="B-cell lymphoma, unclassifiable, diffuse large B-cell and classical Hodgkin (Gray zone lymphoma)")]<-"NHL/HL"

baz$DISEASE[which(baz$DISEASE %in% c("Lymphoma","NHL") & 
                    baz$WHOLYCLS_DIAG=="B-cell lymphoma, unclassifiable, diffuse large B-cell lymphoma and Burkitt lymphoma (Intermediate DLBCL/BL)")]<-"DLBCL/BL"

baz$DISEASE[which(baz$DISEASE %in% c("Lymphoma","NHL") & 
                    baz$WHOLYCLS_DIAG=="B-cell lymphoma, unclassifiable, diffuse large B-cell lymphoma and Burkitt lymphoma (Intermediate DLBCL/BL)")]<-"DLBCL/BL"


baz$DISEASE[which(baz$DISEASE %in% c("Primary immune deficiency"))]<-"PID"
baz$DISEASE[which(baz$DISEASE %in% c("Plasma cell leukaemia"))]<-"PCL"
baz$DISEASE[which(baz$DISEASE %in% c("Acute undifferentiated leukaemia"))]<-"AL undiff"

baz$MDSRISK<-NA
baz$MDSRISK[which(baz$MDSJEG %in% c("RAEB","RAEB2",
                                     "RAEB1") & baz$DISEASE=="MDS")]<-"High"
baz$MDSRISK[which(is.na(baz$MDSRISK) & baz$DISEASE=="MDS")]<-"Low"



if ("WHOLYCLS_DIAG" %in% names(baz) ){
baz$NHLSUBTYP<-as.character(baz$WHOLYCLS_DIAG)
baz$NHLSUBTYP[which(baz$WHOLYCLS_DIAG %in% c("Diffuse large B-cell lymphoma (DLBCL), NOS",
                                                   "Activated B-cell type (ABC or non-GCB) DLBCL",
                                                   "DLBCL associated with chronic inflammation",
                                                   "Germinal center B-cell type (GCB) DLBCL",
                                                   "HHV8 positive DLBCL, NOS ",
                                                   "High grade B-cell lymphoma with MYC and BCL2 and/or BCL6 rearrangements",
                                                   "High grade B-cell lymphoma NOS",
                                                   "Old High grade B-cell lymphoma, Burkitt-like (prov.). No longer in use",
                                                   "Primary cutaneous DLBCL, leg type",
                                                   "Intravascular large B-cell lymphoma",
                                                   "Primary mediastinal large B-cell lymphoma (PMBCL)",
                                                   "Primary DLBCL of the CNS ",
                                                   "Extranodal marginal zone lymphoma of mucosa associated lymphoid tissue (MALT) ",
                                                   "T-cell/histiocyte rich large B cell lymphoma",
                                                   "Plasmablastic lymphoma",
                                                   "Post-transplant lymphoproliferative disorders (P",
                                                   "ALK positive large B-cell lymphoma ",
                                                   "Primary effusion lymphoma (PEL)"
) )]<-"B-cell Aggressive"
baz$NHLSUBTYP[which(baz$WHOLYCLS_DIAG %in% c("Follicular lymphoma",
                                                   "Lymphoplasmacytic lymphoma (LPL)",
                                                   "Waldenstrom macroglobulinaemia (LPL with monoclonal IgM)",
                                                   "Splenic marginal zone lymphoma ",
                                                   "Primary cutaneous follicle centre lymphoma",
                                                   "Nodal marginal zone lymphoma "
) )]<-"B-cell Indolent"
baz$NHLSUBTYP[which(baz$WHOLYCLS_DIAG %in% c("Peripheral T-cell lymphoma, NOS (PTCL)",
                                                   "Angioimmunoblastic T-cell lymphoma",
                                                   "Anaplastic large-cell lymphoma (ALCL), ALK-negative",
                                                   "Anaplastic large-cell lymphoma (ALCL), ALK-positive",
                                                   "Hepatosplenic T-cell lymphoma",
                                                   "Enteropathy-associated T-cell lymphoma",
                                                   "Primary cutaneous CD8 positive aggressive epidermotropic cytotoxic T-cell lymphoma",
                                                   "Primary cutaneous CD4 positive small/medium T-cell lymphoma",
                                                   "Primary cutaneous gamma-delta T-cell lymphoma",
                                                   "Primary cutaneous anaplastic large cell lymphoma",
                                                   "Subcutaneous panniculitis-like T-cell lymphoma",
                                                   "Aggressive NK-cell leukaemia"
) )]<-"T/NK-cell Aggressive"
baz$NHLSUBTYP[which(baz$WHOLYCLS_DIAG %in% c("Adult T-cell leukaemia/lymphoma","Mycosis fungoides (MF) ",
                                                   "Extranodal NK/T-cell lymphoma, nasal type",
                                                   "Sezary syndrome",
                                                   "T-cell large granular lymphocytic leukaemia"
) )]<-"T/NK-cell Indolent"
baz$NHLSUBTYP[which(baz$WHOLYCLS_DIAG %in% c("EBV positive DLBCL, NOS",
                                                   "Lymphomatoid granulomatosis",
                                                   "B-cell lymphoma, unclassifiable, diffuse large B-cell lymphoma and Burkitt lymphoma (Intermediate DLBCL/BL)",
                                                   "B-cell lymphoma, unclassifiable, diffuse large B-cell and classical Hodgkin (Gray zone lymphoma)",
                                                   "B-cell lymphoma, other or NOS"
) )]<-"B-cell undertermined"
baz$NHLSUBTYP[which(baz$WHOLYCLS_DIAG %in% c("Systemic EBV positive T-cell lymphoproliferative disease of childhood",
                                                   "T-cell lymphoma, other or NOS"
) )]<-"T-cell undertermined"
}


baz$TRANSFORM<-NA
baz$TRANSFORM[which(baz$VMDS=="Transformed to AML (`secondary` acute leukaemia)")]<-"VMDS"
baz$TRANSFORM[which(baz$VMDS_BMT=="Transformed to AML (`secondary` acute leukaemia)")]<-"VMDS_BMT"
baz$TRANSFORM[which(baz$MDSSTAG=="Transformed to AML")]<-"MDSSTAG"
baz$TRANSFORM[which(baz$MDSSTAG_BMT=="Transformed to AML")]<-"MDSSTAG_BMT"
baz$TRANSFORM[which(baz$MDSSTAG_TRANSF=="Transformed to AML")]<-"MDSSTAG_TRANSF"
baz$TRANSFORM[which(baz$VMPS=="Transformed to AML")]<-"VMPS"
baz$TRANSFORM[which(baz$VMPS_BMT=="Transformed to AML")]<-"VMPS_BMT"
baz$TRANSFORM[which(baz$VMPS_TRANSF=="Transformed to AML")]<-"VMPS_TRANSF"
baz$TRANSFORM[which(baz$MDSSTAG=="Transformed to AML")]<-"MDSSTAG"
baz$TRANSFORM[which(baz$MDSSTAG_BMT=="Transformed to AML")]<-"MDSSTAG_BMT"
baz$TRANSFORM[which(baz$MDSSTAG_TRANSF=="Transformed to AML")]<-"MDSSTAG_TRANSF"


baz$AGVHDSKI[baz$AGVHDSKI %in% c("Not evaluated","unknown")] <- NA
baz$AGVHDLIV[baz$AGVHDLIV %in% c("Not evaluated","unknown")] <- NA
baz$AGVHDLGI[baz$AGVHDLGI %in% c("Not evaluated","unknown")] <- NA
baz$AGVHDUGI[baz$AGVHDUGI %in% c("Not evaluated","unknown")] <- NA
baz$AGVHDGUT[baz$AGVHDGUT %in% c("Not evaluated","unknown")] <- NA


######## Nbr de cellules ######

if( "INFNUC_DO1" %in% names(baz)){

baz$INFNUC_DO1[which(baz$INFNUC_DO1==8888.88)]<-NA  
baz$INFNUC_DO1[which(baz$INFNUC_DO1==9999.99)]<-NA  

baz$INFNUC_DO2[which(baz$INFNUC_DO2==8888.88)]<-NA  
baz$INFNUC_DO2[which(baz$INFNUC_DO2==9999.99)]<-NA  

baz$INF34PC_DO1[which(baz$INF34PC_DO1==8888.88)]<-NA  
baz$INF34PC_DO1[which(baz$INF34PC_DO1==9999.99)]<-NA  

baz$INF34PC_DO2[which(baz$INF34PC_DO2==8888.88)]<-NA  
baz$INF34PC_DO2[which(baz$INF34PC_DO2==9999.99)]<-NA

baz$INF3PC_DO1[which(baz$INF3PC_DO1==8888.88)]<-NA  
baz$INF3PC_DO1[which(baz$INF3PC_DO1==9999.99)]<-NA  

baz$INF3PC_DO2[which(baz$INF3PC_DO2==8888.88)]<-NA  
baz$INF3PC_DO2[which(baz$INF3PC_DO2==9999.99)]<-NA

}




if ("INFNUC1_D1P1" %in% names(baz)){
  
baz$INFNUC1_D1P1<-as.numeric(as.character(baz$INFNUC1_D1P1))
baz$INF34PC1_D1P1<-as.numeric(as.character(baz$INF34PC1_D1P1))
baz$INF3PC1_D1P1<-as.numeric(as.character(baz$INF3PC1_D1P1))

baz$INFNUC2_D1P2<-as.numeric(as.character(baz$INFNUC2_D1P2))
baz$INF34PC2_D1P2<-as.numeric(as.character(baz$INF34PC2_D1P2))
baz$INF3PC2_D1P2<-as.numeric(as.character(baz$INF3PC2_D1P2))

baz$INFNUC1_D2P1<-as.numeric(as.character(baz$INFNUC1_D2P1))
baz$INF34PC1_D2P1<-as.numeric(as.character(baz$INF34PC1_D2P1))
baz$INF3PC1_D2P1<-as.numeric(as.character(baz$INF3PC1_D2P1))

baz$INFNUC2_D2P2<-as.numeric(as.character(baz$INFNUC2_D2P2))
baz$INF34PC2_D2P2<-as.numeric(as.character(baz$INF34PC2_D2P2))
baz$INF3PC2_D2P2<-as.numeric(as.character(baz$INF3PC2_D2P2))


#####################
#### D1P1 ###########
#####################

#### CD34 donor 1, produit 1 ##### Total number of Cells Infused (per kg of recipient body weight) 
baz$CD34_D1P1_JEG<-NA
baz$CD34_D1P1_JEG[which(baz$CD34UNIT1_D1P1=="10e5 / Kg")]<-baz$INF34PC1_D1P1[which(baz$CD34UNIT1_D1P1=="10e5 / Kg")]/10
baz$CD34_D1P1_JEG[which(baz$CD34UNIT1_D1P1=="10e6 / Kg")]<-baz$INF34PC1_D1P1[which(baz$CD34UNIT1_D1P1=="10e6 / Kg")]
baz$CD34_D1P1_JEG[which(baz$CD34UNIT1_D1P1=="10e7 / Kg")]<-baz$INF34PC1_D1P1[which(baz$CD34UNIT1_D1P1=="10e7 / Kg")]*10
baz$CD34_D1P1_JEG[which(baz$CD34UNIT1_D1P1=="10e8 / Kg")]<-baz$INF34PC1_D1P1[which(baz$CD34UNIT1_D1P1=="10e8 / Kg")]*100
baz$CD34_D1P1_JEG[which(baz$CD34UNIT1_D1P1=="10e9 / Kg")]<-baz$INF34PC1_D1P1[which(baz$CD34UNIT1_D1P1=="10e9 / Kg")]*1000

#### CD3 donor 1, produit 1 ##### Total number of Cells Infused (per kg of recipient body weight) 
baz$CD3_D1P1_JEG<-NA
baz$CD3_D1P1_JEG[which(baz$CD3UNIT1_D1P1=="10e5 / Kg")]<-baz$INF3PC1_D1P1[which(baz$CD3UNIT1_D1P1=="10e5 / Kg")]/10
baz$CD3_D1P1_JEG[which(baz$CD3UNIT1_D1P1=="10e6 / Kg")]<-baz$INF3PC1_D1P1[which(baz$CD3UNIT1_D1P1=="10e6 / Kg")]
baz$CD3_D1P1_JEG[which(baz$CD3UNIT1_D1P1=="10e7 / Kg")]<-baz$INF3PC1_D1P1[which(baz$CD3UNIT1_D1P1=="10e7 / Kg")]*10
baz$CD3_D1P1_JEG[which(baz$CD3UNIT1_D1P1=="10e8 / Kg")]<-baz$INF3PC1_D1P1[which(baz$CD3UNIT1_D1P1=="10e8 / Kg")]*100
baz$CD3_D1P1_JEG[which(baz$CD3UNIT1_D1P1=="10e9 / Kg")]<-baz$INF3PC1_D1P1[which(baz$CD3UNIT1_D1P1=="10e9 / Kg")]*1000

#### TNC donor 1, produit 1 ##### Total number of Cells Infused (per kg of recipient body weight) 
baz$TNC_D1P1_JEG<-NA
baz$TNC_D1P1_JEG[which(baz$NUCUNIT1_D1P1=="10e5 / Kg")]<-baz$INFNUC1_D1P1[which(baz$NUCUNIT1_D1P1=="10e5 / Kg")]/1000
baz$TNC_D1P1_JEG[which(baz$NUCUNIT1_D1P1=="10e6 / Kg")]<-baz$INFNUC1_D1P1[which(baz$NUCUNIT1_D1P1=="10e6 / Kg")]/100
baz$TNC_D1P1_JEG[which(baz$NUCUNIT1_D1P1=="10e7 / Kg")]<-baz$INFNUC1_D1P1[which(baz$NUCUNIT1_D1P1=="10e7 / Kg")]/10
baz$TNC_D1P1_JEG[which(baz$NUCUNIT1_D1P1=="10e8 / Kg")]<-baz$INFNUC1_D1P1[which(baz$NUCUNIT1_D1P1=="10e8 / Kg")]
baz$TNC_D1P1_JEG[which(baz$NUCUNIT1_D1P1=="10e9 / Kg")]<-baz$INFNUC1_D1P1[which(baz$NUCUNIT1_D1P1=="10e9 / Kg")]*10

#####################
#### D1P2 ###########
#####################

baz$CD34_D1P2_JEG<-NA
baz$CD34_D1P2_JEG[which(baz$CD34UNIT2_D1P2=="10e5 / Kg")]<-baz$INF34PC2_D1P2[which(baz$CD34UNIT2_D1P2=="10e5 / Kg")]/10
baz$CD34_D1P2_JEG[which(baz$CD34UNIT2_D1P2=="10e6 / Kg")]<-baz$INF34PC2_D1P2[which(baz$CD34UNIT2_D1P2=="10e6 / Kg")]
baz$CD34_D1P2_JEG[which(baz$CD34UNIT2_D1P2=="10e7 / Kg")]<-baz$INF34PC2_D1P2[which(baz$CD34UNIT2_D1P2=="10e7 / Kg")]*10
baz$CD34_D1P2_JEG[which(baz$CD34UNIT2_D1P2=="10e8 / Kg")]<-baz$INF34PC2_D1P2[which(baz$CD34UNIT2_D1P2=="10e8 / Kg")]*100
baz$CD34_D1P2_JEG[which(baz$CD34UNIT2_D1P2=="10e9 / Kg")]<-baz$INF34PC2_D1P2[which(baz$CD34UNIT2_D1P2=="10e9 / Kg")]*1000

baz$CD3_D1P2_JEG<-NA
baz$CD3_D1P2_JEG[which(baz$CD3UNIT2_D1P2=="10e5 / Kg")]<-baz$INF3PC2_D1P2[which(baz$CD3UNIT2_D1P2=="10e5 / Kg")]/10
baz$CD3_D1P2_JEG[which(baz$CD3UNIT2_D1P2=="10e6 / Kg")]<-baz$INF3PC2_D1P2[which(baz$CD3UNIT2_D1P2=="10e6 / Kg")]
baz$CD3_D1P2_JEG[which(baz$CD3UNIT2_D1P2=="10e7 / Kg")]<-baz$INF3PC2_D1P2[which(baz$CD3UNIT2_D1P2=="10e7 / Kg")]*10
baz$CD3_D1P2_JEG[which(baz$CD3UNIT2_D1P2=="10e8 / Kg")]<-baz$INF3PC2_D1P2[which(baz$CD3UNIT2_D1P2=="10e8 / Kg")]*100
baz$CD3_D1P2_JEG[which(baz$CD3UNIT2_D1P2=="10e9 / Kg")]<-baz$INF3PC2_D1P2[which(baz$CD3UNIT2_D1P2=="10e9 / Kg")]*1000

# Nucl
baz$TNC_D1P2_JEG<-NA
baz$TNC_D1P2_JEG[which(baz$NUCUNIT2_D1P2=="10e5 / Kg")]<-baz$INFNUC2_D1P2[which(baz$NUCUNIT2_D1P2=="10e5 / Kg")]/1000
baz$TNC_D1P2_JEG[which(baz$NUCUNIT2_D1P2=="10e6 / Kg")]<-baz$INFNUC2_D1P2[which(baz$NUCUNIT2_D1P2=="10e6 / Kg")]/100
baz$TNC_D1P2_JEG[which(baz$NUCUNIT2_D1P2=="10e7 / Kg")]<-baz$INFNUC2_D1P2[which(baz$NUCUNIT2_D1P2=="10e7 / Kg")]/10
baz$TNC_D1P2_JEG[which(baz$NUCUNIT2_D1P2=="10e8 / Kg")]<-baz$INFNUC2_D1P2[which(baz$NUCUNIT2_D1P2=="10e8 / Kg")]
baz$TNC_D1P2_JEG[which(baz$NUCUNIT2_D1P2=="10e9 / Kg")]<-baz$INFNUC2_D1P2[which(baz$NUCUNIT2_D1P2=="10e9 / Kg")]*10

#####################
#### D2P1 ###########
#####################

baz$CD34_D2P1_JEG<-NA
baz$CD34_D2P1_JEG[which(baz$CD34UNIT1_D2P1=="10e5 / Kg")]<-baz$INF34PC1_D2P1[which(baz$CD34UNIT1_D2P1=="10e5 / Kg")]/10
baz$CD34_D2P1_JEG[which(baz$CD34UNIT1_D2P1=="10e6 / Kg")]<-baz$INF34PC1_D2P1[which(baz$CD34UNIT1_D2P1=="10e6 / Kg")]
baz$CD34_D2P1_JEG[which(baz$CD34UNIT1_D2P1=="10e7 / Kg")]<-baz$INF34PC1_D2P1[which(baz$CD34UNIT1_D2P1=="10e7 / Kg")]*10
baz$CD34_D2P1_JEG[which(baz$CD34UNIT1_D2P1=="10e8 / Kg")]<-baz$INF34PC1_D2P1[which(baz$CD34UNIT1_D2P1=="10e8 / Kg")]*100
baz$CD34_D2P1_JEG[which(baz$CD34UNIT1_D2P1=="10e9 / Kg")]<-baz$INF34PC1_D2P1[which(baz$CD34UNIT1_D2P1=="10e9 / Kg")]*1000

baz$CD3_D2P1_JEG<-NA
baz$CD3_D2P1_JEG[which(baz$CD3UNIT1_D2P1=="10e5 / Kg")]<-baz$INF3PC1_D2P1[which(baz$CD3UNIT1_D2P1=="10e5 / Kg")]/10
baz$CD3_D2P1_JEG[which(baz$CD3UNIT1_D2P1=="10e6 / Kg")]<-baz$INF3PC1_D2P1[which(baz$CD3UNIT1_D2P1=="10e6 / Kg")]
baz$CD3_D2P1_JEG[which(baz$CD3UNIT1_D2P1=="10e7 / Kg")]<-baz$INF3PC1_D2P1[which(baz$CD3UNIT1_D2P1=="10e7 / Kg")]*10
baz$CD3_D2P1_JEG[which(baz$CD3UNIT1_D2P1=="10e8 / Kg")]<-baz$INF3PC1_D2P1[which(baz$CD3UNIT1_D2P1=="10e8 / Kg")]*100
baz$CD3_D2P1_JEG[which(baz$CD3UNIT1_D2P1=="10e9 / Kg")]<-baz$INF3PC1_D2P1[which(baz$CD3UNIT1_D2P1=="10e9 / Kg")]*1000


# Nucl
baz$TNC_D2P1_JEG<-NA
baz$TNC_D2P1_JEG[which(baz$NUCUNIT1_D2P1=="10e5 / Kg")]<-baz$INFNUC1_D2P1[which(baz$NUCUNIT1_D2P1=="10e5 / Kg")]/1000
baz$TNC_D2P1_JEG[which(baz$NUCUNIT1_D2P1=="10e6 / Kg")]<-baz$INFNUC1_D2P1[which(baz$NUCUNIT1_D2P1=="10e6 / Kg")]/100
baz$TNC_D2P1_JEG[which(baz$NUCUNIT1_D2P1=="10e7 / Kg")]<-baz$INFNUC1_D2P1[which(baz$NUCUNIT1_D2P1=="10e7 / Kg")]/10
baz$TNC_D2P1_JEG[which(baz$NUCUNIT1_D2P1=="10e8 / Kg")]<-baz$INFNUC1_D2P1[which(baz$NUCUNIT1_D2P1=="10e8 / Kg")]
baz$TNC_D2P1_JEG[which(baz$NUCUNIT1_D2P1=="10e9 / Kg")]<-baz$INFNUC1_D2P1[which(baz$NUCUNIT1_D2P1=="10e9 / Kg")]*10

#####################
#### D2P2 ###########
#####################

baz$CD34_D2P2_JEG<-NA
baz$CD34_D2P2_JEG[which(baz$CD34UNIT2_D2P2=="10e5 / Kg")]<-baz$INF34PC2_D2P2[which(baz$CD34UNIT2_D2P2=="10e5 / Kg")]/10
baz$CD34_D2P2_JEG[which(baz$CD34UNIT2_D2P2=="10e6 / Kg")]<-baz$INF34PC2_D2P2[which(baz$CD34UNIT2_D2P2=="10e6 / Kg")]
baz$CD34_D2P2_JEG[which(baz$CD34UNIT2_D2P2=="10e7 / Kg")]<-baz$INF34PC2_D2P2[which(baz$CD34UNIT2_D2P2=="10e7 / Kg")]*10
baz$CD34_D2P2_JEG[which(baz$CD34UNIT2_D2P2=="10e8 / Kg")]<-baz$INF34PC2_D2P2[which(baz$CD34UNIT2_D2P2=="10e8 / Kg")]*100
baz$CD34_D2P2_JEG[which(baz$CD34UNIT2_D2P2=="10e9 / Kg")]<-baz$INF34PC2_D2P2[which(baz$CD34UNIT2_D2P2=="10e9 / Kg")]*1000

baz$CD3_D2P2_JEG<-NA
baz$CD3_D2P2_JEG[which(baz$CD3UNIT2_D2P2=="10e5 / Kg")]<-baz$INF3PC2_D2P2[which(baz$CD3UNIT2_D2P2=="10e5 / Kg")]/10
baz$CD3_D2P2_JEG[which(baz$CD3UNIT2_D2P2=="10e6 / Kg")]<-baz$INF3PC2_D2P2[which(baz$CD3UNIT2_D2P2=="10e6 / Kg")]
baz$CD3_D2P2_JEG[which(baz$CD3UNIT2_D2P2=="10e7 / Kg")]<-baz$INF3PC2_D2P2[which(baz$CD3UNIT2_D2P2=="10e7 / Kg")]*10
baz$CD3_D2P2_JEG[which(baz$CD3UNIT2_D2P2=="10e8 / Kg")]<-baz$INF3PC2_D2P2[which(baz$CD3UNIT2_D2P2=="10e8 / Kg")]*100
baz$CD3_D2P2_JEG[which(baz$CD3UNIT2_D2P2=="10e9 / Kg")]<-baz$INF3PC2_D2P2[which(baz$CD3UNIT2_D2P2=="10e9 / Kg")]*1000


# Nucl
baz$TNC_D2P2_JEG<-NA
baz$TNC_D2P2_JEG[which(baz$NUCUNIT2_D2P2=="10e5 / Kg")]<-baz$INFNUC2_D2P2[which(baz$NUCUNIT2_D2P2=="10e5 / Kg")]/1000
baz$TNC_D2P2_JEG[which(baz$NUCUNIT2_D2P2=="10e6 / Kg")]<-baz$INFNUC2_D2P2[which(baz$NUCUNIT2_D2P2=="10e6 / Kg")]/100
baz$TNC_D2P2_JEG[which(baz$NUCUNIT2_D2P2=="10e7 / Kg")]<-baz$INFNUC2_D2P2[which(baz$NUCUNIT2_D2P2=="10e7 / Kg")]/10
baz$TNC_D2P2_JEG[which(baz$NUCUNIT2_D2P2=="10e8 / Kg")]<-baz$INFNUC2_D2P2[which(baz$NUCUNIT2_D2P2=="10e8 / Kg")]
baz$TNC_D2P2_JEG[which(baz$NUCUNIT2_D2P2=="10e9 / Kg")]<-baz$INFNUC2_D2P2[which(baz$NUCUNIT2_D2P2=="10e9 / Kg")]*10


}


##### Cells for PB auto
baz$PBNUCL1<-as.character(baz$PBNUCL1)
baz$PBNUCL1[which(baz$PBNUCL1 %in% c("unknown","Not evaluated"))]<-NA
baz$PBNUCL1<-as.numeric(baz$PBNUCL1)




baz$ALLMISRLD1[which(baz$ALLMISRLD1=="unknown")]<-NA

baz$PHIEP[which(baz$PHIEP=="Unknown")]<-NA
baz$PHIEP[which(baz$PHIEP=="Not evaluated")]<-NA
baz$PHIEP[which(baz$PHIEP=="To be confirmed")]<-NA

baz$WHOALL_JEG<-as.character(baz$WHOALL)
baz$WHOALL_JEG[which(baz$WHOALL=="T lymphoblastic leukaemia/lymphoma (Precursor T-cell ALL)")]<-"T"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymphoblastic leukaemia/lymphoma NOS (Precursor B-cell ALL)  " & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymphoblastic leukaemia/lymphoma NOS (Precursor B-cell ALL)  " & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymphoblastic leukaemia/lymphoma NOS (Precursor B-cell ALL)  " & is.na(baz$PHIEP))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(9;22)(q34;q11.2); BCR-ABL1")]<-"B phi+"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(v;11q23); MLL rearranged" & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(v;11q23); MLL rearranged" & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(v;11q23); MLL rearranged" & is.na(baz$PHI))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(12;21)(p13;q22); TEL-AML1" & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(12;21)(p13;q22); TEL-AML1" & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(12;21)(p13;q22); TEL-AML1" & is.na(baz$PHIEP))]<-"B phi NA"


baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(1;19)(q23;p13.3); E2A-PBX1" & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(1;19)(q23;p13.3); E2A-PBX1" & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(1;19)(q23;p13.3); E2A-PBX1" & is.na(baz$PHIEP))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with hyperdiploidy" & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with hyperdiploidy" & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with hyperdiploidy" & is.na(baz$PHIEP))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with hypodiploidy" & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with hypodiploidy" & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with hypodiploidy" & is.na(baz$PHIEP))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymphoblastic leukaemia/lymphoma NOS (Precursor B-cell ALL)  " & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymphoblastic leukaemia/lymphoma NOS (Precursor B-cell ALL)  " & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymphoblastic leukaemia/lymphoma NOS (Precursor B-cell ALL)  " & is.na(baz$PHIEP))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(v;11q23); MLL rearranged" & baz$PHIEP=="Positive")]<-"B phi+"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(v;11q23); MLL rearranged" & baz$PHIEP=="Negative")]<-"B phi-"
baz$WHOALL_JEG[which(baz$WHOALL=="B lymph leuk/lymph with t(v;11q23); MLL rearranged" & is.na(baz$PHIEP))]<-"B phi NA"

baz$WHOALL_JEG[which(baz$WHOALL=="Precursor lymphoid neoplasm, other")]<-"Other"
baz$WHOALL_JEG[which(baz$WHOALL=="unknown")]<-NA

baz$WHOALL_JEG<-as.factor(baz$WHOALL_JEG)





baz$PREVMDS_MAIN[which(baz$PREVMDS_MAIN=="unknown")]<-NA

baz$SECAML2021<-NA
baz$SECAML2021[which(baz$VACLEUK=="AML & Related Precursor Neoplasms")]<-"de novo"
baz$SECAML2021[which((baz$DISMCLFD=="Acute leukaemia" &
                      baz$VACLEUK=="AML & Related Precursor Neoplasms" & 
                      !is.na(baz$DISMCLFD_LASTPREV)) |
                      baz$PREVMDS_MAIN=="Yes" )]<-"secAML"

baz$SECAML2021[which(baz$DISMCLFD=="Acute leukaemia" &
                        baz$VACLEUK=="AML & Related Precursor Neoplasms" & 
                        baz$WHOAML == "Therapy related myeloid neoplasm (old Secondary AML)")]<-"secAML"

baz$SECAML2021[which(  is.na(baz$PREVMDS_MAIN) &
                       !is.na(baz$DISMCLFD_LASTPREV) & 
                       baz$WHOAML == "AML w MDS type changes (incl AML transf from MDS or overlap MD/MPN)")]<-"secAML"

baz$SECAML2021[which(baz$DISMCLFD=="Acute leukaemia" &
                       baz$VACLEUK=="AML & Related Precursor Neoplasms" & 
                       baz$WHOAML == "AML w MDS type changes (incl AML transf from MDS or overlap MD/MPN)" &
                       baz$PREVMDS_MAIN=="Yes")]<-"secAML"

baz$SECAML2021[which(baz$DISMCLFD=="Acute leukaemia" &
                       baz$VACLEUK=="AML & Related Precursor Neoplasms" & 
                       baz$WHOAML == "AML w MDS type changes (incl AML transf from MDS or overlap MD/MPN)" &
                       baz$AACOD21_LASTPREV %in% c("MDS or MPN",
                                                   "Myelodysplastic syndrome",
                                                   "Myeloproliferative neoplasia",
                                                   "MDS->ac leuk",
                                                   "MDS & MPN"))]<-"secAML"

baz$CYTOANO<-str_trim(baz$CYTOANO)
baz$CYTOANO[which(baz$CYTOANO=="")]<-NA

if("MOLECABNOR" %in% names(baz)){
  baz$MOLECABNOR<-str_trim(baz$MOLECABNOR)
  baz$MOLECABNOR[which(baz$MOLECABNOR=="")]<-NA
}


if("CYTOAML" %in% names(baz)){
baz$CYTOAML2<-as.character(baz$CYTOAML)
baz$CYTOAML2[which(baz$CYTOAML=="NA/failed")]<-NA
baz$CYTOAML2<-as.factor(baz$CYTOAML2)
}

# Chromosome analysis at diagnosis
baz$VCHROMOS[which(baz$VCHROMOS=="unknown")]<-NA

# Are there 3 or more abnormalities (complex kariotype)?
baz$MORE3AB[which(baz$MORE3AB=="unknown")]<-NA

# Monosomal karyotype
baz$MONOSKAR[which(baz$MONOSKAR=="unknown")]<-NA



return(baz)

}
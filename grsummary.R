library(arsenal)

# Make Exhaustif Labels Dictionary (Check Christophe Label, or mega SPSS file Name/Label)
## Enrich the Following
labels <- c(AGETX = 'Age at Transplant, yrs',
            AGEDIAG= 'Age at Diagnosis, yrs', 
            PATSEX  = "Patient Gender", 
            DIAGRFM="Duration Diagnostic to HSCT, months", 
            "factor(YEARTX)"="Transplant Year", 
            YEARTX ="Transplant Year, yrs", 
            VRADICON = "TBI",
            "fct_infreq(DISMCLFD_MAIN)" ="Hematological Malignancies (Classification 1)",
            SAMEDONO = "Same Donor as 1st HSCT", 
            PURGE = "Ex-vivo T-cell depletion",
            "fct_infreq(DISEASE)" ="Hematological Malignancies (Classification 2)", 
            MYELOABRJEG = "Myeloablative Conditioning", 
            DISEASE_SPE = "Disease Specific Remission",
            "fct_infreq(DISEASE)" ="Hematological Malignancies", 
            "fct_infreq(DISEASE_SPE)" = "Disease Specific Remission Status" ,
            "fct_infreq(DISEASE_SPE1)" = "Disease Specific Remission Status" ,
            KARN90 = "Karnofsky =>90", 
            PREVENTION_REGIMEN = 'Prevention Regimen',
            "fct_infreq(CONDITIONING_REGIMEN)"	 = 'Conditioning Regimen',
            DONSEX1 = "Donor Sex", 
            "fct_infreq(PREVENTION_REGIMEN)" = "GVHD Prevention Regimen", 
            "fct_infreq(VCENLAND)" = "Center Country","
            VCENLAND"= "Center Country",
            SORROR.CL = "SORROR Comorbidity Index", 
            HSCTYPE = "Type of Donor" , 
            CMVDP1 = "Donor to Patient CMV positivity", 
            DRI= "Disease Risk Index (DRI)" )
 
mycontrols  <- tableby.control(test=TRUE, total=TRUE,digits = 1,digits.pct = 1,digits.p = 2,
                               numeric.test="kwt", 
                               cat.test="chisq",
                               numeric.stats=c( "medianq1q3","range","Nmiss"),
                               cat.stats=c("countpct","Nmiss"),
                               stats.labels=list(Nmiss='Missing count', medianq1q3=' median [Q1, Q3]', range='[Min, Max]'))

tab <- tableby( ~ PATSEX  + DONSEX1+                  
                  AGETX + AGEDIAG+YEARTX + DIAGRFM+
                 SAMEDONO+ CMVDP1 +PURGE+             
                  KARN90 + SORROR.CL + DRI + HSCTYPE+
                  MYELOABRJEG + VRADICON +
               
               notest(fct_infreq(DISMCLFD_MAIN))+  notest(fct_infreq(DISEASE)) + notest(fct_infreq(DISEASE_SPE1))+ 
               notest(fct_infreq(PREVENTION_REGIMEN)) + notest(fct_infreq(CONDITIONING_REGIMEN)) +  notest( fct_infreq( VCENLAND) )     
              , data= Data_HSCT, control=mycontrols  )

summary(tab,
        labelTranslations=labels
        , pfootnote=TRUE, 
        title="\\\  Table. Baseline patient-, donor- and transplant-related characteristics at **XXXXX**. ")

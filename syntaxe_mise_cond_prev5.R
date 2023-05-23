###############################################################################################
###############################################################################################
####################### Fonction recodant le conditionnement, cause de DC et ##################
######################## prevention de la GVHD                              ###################
###############################################################################################

prevcond<-function(data=NULL){
  baz<-data
  
###############################################################################################
###############################################################################################
################### Correction TBI  ###########################################################
###############################################################################################

baz$VTBIDOSE<-as.numeric(as.character(baz$VTBIDOSE))
baz$VTBIDOSE[which(baz$VTBIDOSE==0.2 & baz$MYELOABR=="No")]<-2
baz$VTBIDOSE[which(baz$VTBIDOSE==0.12 & baz$MYELOABR=="Yes")]<-12
baz$VTBIDOSE[which(baz$VTBIDOSE==1.2 & baz$MYELOABR=="Yes")]<-12

baz$VRADICON[which(baz$VRADICON=="unknown")]<-NA
baz$VRADICON[which(baz$VTBIDOSE > 0)]<-"Yes"

if("VNUMFRAC" %in% names(baz)) baz$VNUMFRAC[which(baz$VNUMFRAC=="unknown")]<-NA
if("VNUMFRAC" %in% names(baz)) baz$VNUMFRAC[which(baz$VNUMFRAC=="Not evaluated")]<-NA

baz$DOSETBINBIN<-NA
baz$DOSETBINBIN[which(baz$VRADICON=="Yes" & baz$VTBIDOSE > 6)]<-"TBI(>6)"
baz$DOSETBINBIN[which(baz$VRADICON=="Yes" & baz$VTBIDOSE <= 6)]<-"TBI(<=6)"
baz$DOSETBINBIN[which(baz$VRADICON=="Yes" & is.na(baz$VTBIDOSE))]<-"TBI(NA)"
baz$DOSETBINBIN[which(baz$VRADICON=="No")]<-"No"


###############################################################################################
################################## Renomage ###################################################
###############################################################################################  
  
  for(i in c("CONDDRUG1","CONDDRUG2","CONDDRUG3","CONDDRUG4","CONDDRUG5","CONDDRUG6","CONDDRUG7",
             "PREVDRUG1","PREVDRUG2","PREVDRUG3","PREVDRUG4","PREVDRUG5","PREVDRUG6","PREVDRUG7")){
    vartemp<-baz[,i]
    vartemp[which(vartemp=="No more")]<-NA
    vartemp[which(vartemp=="- PROTOCOLS / Combination chemo")]<-NA
    vartemp[which(vartemp=="- PROTOCOLS/ Combination chemo")]<-NA
    
    vartemp[which(vartemp=="-SINGLE DRUG")]<-NA
    vartemp[which(vartemp=="Analgesics")]<-NA    
    vartemp[which(vartemp=="Hypomethylating agents")]<-NA
    vartemp[which(vartemp=="Weekly inten")]<-NA
    vartemp[which(vartemp=="Trialdrug\\/Placebo")]<-NA
    vartemp[which(vartemp=="Trialdrug\\/ Placebo")]<-NA
    vartemp[which(vartemp=="Trialdrug/ Placebo")]<-NA
    vartemp[which(vartemp=="Trialdrug/Placebo")]<-NA
    
    baz[,i]<-vartemp
    
    baz[,i]<-gsub("Ciclosporin / Cyclosporin / Neoral","CSA",baz[,i])
    baz[,i]<-gsub("Ciclosporin/ Cyclosporin/ Neoral","CSA",baz[,i])
    
    baz[,i]<-gsub("Methotrexate","MTX",baz[,i])
    baz[,i]<-gsub("Mycophenolate mofetil","MMF",baz[,i])
    baz[,i]<-gsub("Myfortic","MMF",baz[,i])
    baz[,i]<-gsub("Sirolimus / Rapamune","SIRO",baz[,i])
    baz[,i]<-gsub("Sirolimus/ Rapamune","SIRO",baz[,i])
    
    baz[,i]<-gsub("Tacrolimus","TACRO",baz[,i])
    baz[,i]<-gsub("Methyl prednisone/solone","STER",baz[,i])
    baz[,i]<-gsub("Methyl prednisone/ solone","STER",baz[,i])
    
    baz[,i]<-gsub("Prednisone/solone","STER",baz[,i])
    baz[,i]<-gsub("Prednisone/ solone","STER",baz[,i])
    
    baz[,i]<-gsub("Corticosteroids","STER",baz[,i])
    baz[,i]<-gsub("Alemtuzumab \\(MabCampath,CD52\\)","CAMP",baz[,i])
    baz[,i]<-gsub("ATG or ALG \\(ATS or ALS\\)","ATG",baz[,i])
    baz[,i]<-gsub("ATG \\(anti-thymocyte globulin/serum\\)","ATG",baz[,i])
    baz[,i]<-gsub("ATG \\(anti-thymocyte globulin/ serum\\)","ATG",baz[,i])
    
    baz[,i]<-gsub("ALG \\(anti-lymphocyte globulin/serum\\)","ATG",baz[,i])
    baz[,i]<-gsub("ALG \\(anti-lymphocyte globulin/ serum\\)","ATG",baz[,i])
    
    baz[,i]<-gsub("anti rIL2 \\(CD25\\)","DACLI",baz[,i])
    baz[,i]<-gsub("Etanercept \\(anti TNF\\)","Etanercept",baz[,i])
    baz[,i]<-gsub("Infliximab \\(anti TNF\\)","Infliximab",baz[,i])
    baz[,i]<-gsub("Lenalidomide / Revlimid","Revlimid",baz[,i])
    baz[,i]<-gsub("Lenalidomide/ Revlimid","Revlimid",baz[,i])
    
    baz[,i]<-gsub("Daratumumab (Darzalex)","Daratumumab",baz[,i])
    baz[,i]<-gsub("Venetoclax / Venclexta  / ABT-199","Venetoclax",baz[,i])
    baz[,i]<-gsub("Venetoclax/ Venclexta/ ABT-199","Venetoclax",baz[,i])
    
    baz[,i]<-gsub("Ruxolitinib / Jakafi","Ruxolitinib",baz[,i])
    baz[,i]<-gsub("Ruxolitinib/ Jakafi","Ruxolitinib",baz[,i])
    
    baz[,i]<-gsub("2-CdA / Cladribine","Cladribine",baz[,i])
    baz[,i]<-gsub("2-CdA/ Cladribine","Cladribine",baz[,i])
    
    baz[,i]<-gsub("FCM or FMC","Flu__Cyclo__Mitoxantrone",baz[,i])
    baz[,i]<-gsub("ACE","Adriamycine__Cyclo__Etoposide",baz[,i])
    baz[,i]<-gsub("APO","Adriamycine__Prednisone__Vincristine",baz[,i])
    
    
    baz[,i]<-gsub("Daclizumab \\(Zenapax\\)","DACLI",baz[,i])
    baz[,i]<-gsub("Topotecan","TOPOT",baz[,i])
    baz[,i]<-gsub("CD34","CD34",baz[,i])
    baz[,i]<-gsub("CD66","CD66",baz[,i])
    baz[,i]<-gsub("CD3","CD3",baz[,i])
    
    baz[,i]<-gsub("Basiliximab","DACLI",baz[,i])
    baz[,i]<-gsub("Temozolomide \\(Temodal\\)","Temo",baz[,i]) 
    baz[,i]<-gsub("Other 1","Other",baz[,i])
    baz[,i]<-gsub("Other 2","Other",baz[,i])
    baz[,i]<-gsub("Other 3","Other",baz[,i])
    baz[,i]<-gsub("Other 4","Other",baz[,i])   
    baz[,i]<-gsub("Carboplatin","Carbo",baz[,i])
    baz[,i]<-gsub("Clofarabin","Clofa",baz[,i])
    baz[,i]<-gsub("Bendamustin\\(Ribomustin\\)","Benda",baz[,i])
    baz[,i]<-gsub("Amsacrine","Amsa",baz[,i])
    baz[,i]<-gsub("mini BEAM","BEAM",baz[,i])
    
    baz[,i]<-gsub("MIBG \\(Meta Iodo Benzyl Guanidine, radiolabelled ","MIBG",baz[,i])
    baz[,i]<-gsub("AraC - Platinum like","Arac",baz[,i])
    baz[,i]<-gsub("IV donor AB \\(IVIG, intravenous immune globulin\\)","IVIG",baz[,i])
    baz[,i]<-gsub("Brentuximab \\(Adcetris\\)","Brentux",baz[,i])
    baz[,i]<-gsub("Nivolumab \\(Opdivo\\)","Nivolumab",baz[,i])
    
    baz[,i]<-gsub("Cyclophosphamide / Endoxan","Cyclo",baz[,i])
    baz[,i]<-gsub("Cyclophosphamide/ Endoxan","Cyclo",baz[,i])
    
    baz[,i]<-gsub("Etoposide / VP16","Eto",baz[,i])
    baz[,i]<-gsub("Etoposide/ VP16","Eto",baz[,i])
    
    baz[,i]<-gsub("Busulfan / Busulphan","Bu",baz[,i])
    baz[,i]<-gsub("Busulfan/ Busulphan","Bu",baz[,i])
    
    baz[,i]<-gsub("Busulfex / Busilvex","Bu",baz[,i])
    baz[,i]<-gsub("Busulfex/ Busilvex","Bu",baz[,i])
    
    baz[,i]<-gsub("ARA-C / Cytarabine","Arac",baz[,i])
    baz[,i]<-gsub("ARA-C/ Cytarabine","Arac",baz[,i])
    
    baz[,i]<-gsub("BCNU / Carmustine","BCNU",baz[,i])
    baz[,i]<-gsub("BCNU/ Carmustine","BCNU",baz[,i])
    
    baz[,i]<-gsub("CD20\\(rituximab,mabthera\\)","Ritux",baz[,i])
    
    baz[,i]<-gsub("Arsenic trioxide \\(Trisenox\\)","ArsenicTrioxide",baz[,i])
    baz[,i]<-gsub("Retinoic acid \\(unspecified\\)","RetinoicAcid",baz[,i])
    baz[,i]<-gsub("Neorecormon \\(Epoetin\\)","Neorecormon",baz[,i])
    baz[,i]<-gsub("Intrathecal chemotherapy \\(any\\)","IntrathecalChemo",baz[,i])
    baz[,i]<-gsub("Teniposide \\(VM-26\\)","Teniposide",baz[,i])
    baz[,i]<-gsub("Monoclonal AB, unspecified","MonoclonalAB",baz[,i])
    baz[,i]<-gsub("CD33 \\(gemtuzumab\\)","CD33",baz[,i])
    baz[,i]<-gsub("Zevalin \\(CD20,radiolabelled Ytrium\\)","Zevalin",baz[,i])
    baz[,i]<-gsub("Daratumumab \\(Darzalex\\)","Daratumumab",baz[,i])
    
    baz[,i]<-gsub("Bortezomib \\(Velcade\\)","Bortezomib",baz[,i])
    baz[,i]<-gsub("Paclitaxel / Taxol","Taxo",baz[,i])
    baz[,i]<-gsub("Paclitaxel/ Taxol","Taxo",baz[,i])
    baz[,i]<-gsub("Imatinib mesylate","Imatinib",baz[,i])
    
    baz[,i]<-gsub("Teniposide (VM-26)","Teniposide",baz[,i])
    baz[,i]<-gsub("Fludarabine","Flu",baz[,i])
    baz[,i]<-gsub("Melphalan","Mel",baz[,i])
    baz[,i]<-gsub("Treosulfan","Treo",baz[,i])
    baz[,i]<-gsub("Thiotepa","Thio",baz[,i])
    baz[,i]<-gsub("Bexxar \\(CD20,radiolabelled Iodine\\)","Bexxar",baz[,i])
    baz[,i]<-gsub("Monoclonal AB, unspecified","MonoclonalAB",baz[,i])
    baz[,i]<-gsub("BuCy2","BuCy",baz[,i])
    baz[,i]<-gsub("BuCy4","BuCy",baz[,i])
    baz[,i]<-gsub("Eprex \\(Epoetin\\)","Epoetin",baz[,i])
    baz[,i]<-gsub("Prostanoids/Prostaglandins","Prostaglandins",baz[,i])
    baz[,i]<-gsub("Prostanoids/ Prostaglandins","Prostaglandins",baz[,i])
    baz[,i]<-gsub("Aranesp (Darbepoetin)","Darbepoetin",baz[,i])
    baz[,i]<-gsub("Nelarabine/ Atriance","Nelarabine",baz[,i])
    baz[,i]<-gsub("Daratumumab (Darzalex)","Daratumumab",baz[,i])
    baz[,i]<-gsub("Tocilizumab/ Actemra","Tocilizumab",baz[,i])
    baz[,i]<-gsub("Vedolizumab/ Entyvio","Vedolizumab",baz[,i])
  
    baz[,i]<-gsub("IV donor AB \\(IVIG, intravenous immune globulin\\)","IVIG",baz[,i])
    baz[,i]<-gsub("FLAG","Flu__Arac__G-CSF",baz[,i])
  }


condi<-c("Eto","BuCy","Bu","Flu","Arac","BCNU","CCNU",
         "mini BEAM","BEAM","Clofa","BAVC","Thio","Mel","Treo",
         "Amsa","Mesna","ABVD","CBV","Mitoxantrone","Idarubicine",
         "Ritux","Cisplatin","Carbo","R-Benda","ChlVPP/PABlOE", "R-GIfOx",
         "ITT AMH\\(intrathecal\\)","ACBV","ACVB","Chlorambucil","Cladribine",
         "Benda","CEP","Vincristine","ICE","VBMCP/VBAD")

preven<-c("ATG","CAMP","MTX","CSA",
          "MMF","SIRO","TACRO","DACLI",
          "TOPOT","CD34",
          "CD66","Everolimus","Dexamethasone")

antibio<-c("Metronidazole","Ciprofloxacin")

mobi<-c("G-CSF","GM-CSF","Growth factors","Plerixafor","Tevagastrim", "Neorecormon",
        "Stem cell factor","Epoetin","Filgrastim","Pegfilgrastim","Granocyte","EPO","Myelostim",
        "Nivestim","Darbepoetin")

steroid<-c("STER","Prednisone","Dexamethasone")
    
pascondi<-  c(preven,antibio,mobi,steroid,
              "Etanercept",
              "Dexamethasone",
              "Acyclovir",
              "Hydroxyurea",
              "Prostaglandins",
              "antiLFA1",
              "CD2","CD41","CD19",
              "IVIG","L-asparaginase",
              "MIBG", # radiolabel agent
              "MonoclonalAB",
              "Heparin",
              "Midostaurin","Gancyclovir",
              "Eculizumab","Decitabine","Ruxolitinib",
              "Fluorouracil","Azacytidine ","Tocilizumab")

pascondi2<- c("Itrakonazol", # Anti-fungal agent
              "TDT", # ???
              "Anti-malarial", # Anti malarial
              "Mesna" # Decrease risk of bleeding
              )

pasprev <- c(condi,antibio,mobi,steroid,
             "Bortezomib",
             "Hydroxyurea",
             "IVIG",
             "IL-11",
             "Adriamycine",
             "Azacytidine ",
             "Acyclovir",
             "Heparin","Vindesine",
             "Gancyclovir",
             "ChlVPP/ PABlOE",
             "Midostaurin",
             "Ciprofloxacin",
             "Prostaglandins","Bleomycine",
             "CBV","Daunorubicin",
             "IntrathecalChemo","L-asparaginase",
             "FLAG","MIBG","Fluorouracil",
             "Imatinib","Epirubicine","Bexxar","Revlimid",
             "Teniposide","Mercaptapur6","Venetoclax",
             "Gemcitabine","Eculizumab","Ruxolitinib","Decitabine","Tocilizumab")

  #############################################################
  ######################## Joli label #########################
  #############################################################
  
  doublelabel<-function(baz,var){
    baz[,var]<-gsub("ATG__ATG","ATG",baz[,var])
    baz[,var]<-gsub("STER__STER","STER",baz[,var])
    baz[,var]<-gsub("BuCy__BuCy","BuCy",baz[,var])
    baz[,var]<-gsub("Arac__Arac","Arac",baz[,var])
    baz[,var]<-gsub("Bu__Bu","Bu",baz[,var])
    baz[,var]<-gsub("Eto__Eto","Eto",baz[,var])
    baz[,var]<-gsub("Treo__Treo","Treo",baz[,var])
    baz[,var]<-gsub("Ritux__Ritux","Ritux",baz[,var])
    baz[,var]<-gsub("Thio__Thio","Thio",baz[,var])
    baz[,var]<-gsub("Clofa__Clofa","Clofa",baz[,var])
    baz[,var]<-gsub("Mel__Mel","Mel",baz[,var])
    baz[,var]<-gsub("Flu__Flu","Flu",baz[,var])
    baz[,var]<-gsub("Mesna__Mesna","Mesna",baz[,var])
    baz[,var]<-gsub("Bortezomib__Bortezomib","Bortezomib",baz[,var])
    
    baz[,var]<-gsub("-Bu","Bu",baz[,var])
  }

# 1-Bu (ou Treo) 2-Cy 3-Flu 4-Mel 5-Flamsa (Flu Asma Arac) 6-Eto 7-Thio 8-Arac 9-Amsa
# 10-CCNU 11-BCNU 12-Clofa 13-Carbo 14-Cisplatine 15-Idarubicine 16-Ifosfamide 17-Ritux
# 17-Anthracycline (dans solide tumeur)

 bucylabel<-function(baz,var){
    ### BuCy based
    
    ## 6 drugs
    baz[,var]<-gsub("Amsa__Arac__Bu__Cy__Flu__Thio","BuCy__Flamsa__Thio",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Cy__Eto__Flu__Thio","BuCy__Flu__Eto__Thio__Arac",baz[,var])
    baz[,var]<-gsub("Adriamycine__Daunorubicin__Bu__Cy__Mel","BuCy__Eto__Mel__Daunorubicin__Adriamycine",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Cy__Flu__Thio","BuCy__Flu__Thio__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__Thio__Bu__Cy__Flu__Thio","BuCy__Flu__Thio__Azathioprine",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Eto__Flu__Thio__Flu__Thio","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bu__Cy__Eto__Flu__Idarubicine","BuCy__Flasma__Eto__Idarubincine",baz[,var])
    
    
    
    ## 5 drugs
    baz[,var]<-gsub("Bu__Eto__Flu__Cy__Eto__Flu__Thio","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Eto__Flu__Thio","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Cy__Eto__Thio","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Cy__Thio__Eto","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Eto__Cy__Eto__Flu__Thio","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Cy__Eto__Flu__Thio","BuCy__Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Carbo__Eto__Flu__Mel","Bu__Flu__Mel__Eto__Carbo",baz[,var])
    baz[,var]<-gsub("BuCy__Arac__Daunorubicin__Flu__Eto","BuCy__Flu__Eto__Arac__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Arac__BuCy__Daunorubicin__Eto__Flu","BuCy__Flu__Eto__Arac__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Cy__Daunorubicin__Eto__Flu","BuCy__Flu__Eto__Arac__Daunorubicin",baz[,var])
    baz[,var]<-gsub("ADE__Bu__Cy","BuCy__Eto__Arac__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Bu__Cy__CD3__Flu__Mel","BuCy__Flu__Mel__CD3",baz[,var])
    baz[,var]<-gsub("Bu__CD3__Cy__Flu__Mel","BuCy__Flu__Mel__CD3",baz[,var])
    baz[,var]<-gsub("Adriamycine__Arac__Bu__CCNU__Cy","BuCy__Arac__CCNU__Adriamycine",baz[,var])
    
    
    baz[,var]<-gsub("Bu__Cisplatin__Cy__Flu__Thio","BuCy__Flu__Thio__Cisplatin",baz[,var])
    baz[,var]<-gsub("Amsa__Bu__Cy__Arac__CCNU","BuCy__Arac__Amsa__CCNU",baz[,var])
    baz[,var]<-gsub("BCNU__Bu__Cy__Flu__Thio","BuCy__Flu__Thio__BCNU",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Cy__Eto__Flu","BuCy__Flu__Eto__Arac",baz[,var])
    
    baz[,var]<-gsub("Amsa__Arac__Bu__Cy__Flu","BuCy__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bu__Cy__Mel","BuCy__Mel__Arac__Amsa",baz[,var])
    baz[,var]<-gsub("Amsa__Bu__Cy__Mel__Arac","BuCy__Mel__Arac__Amsa",baz[,var])
    baz[,var]<-gsub("Arac__CCNU__Bu__Cy__CCNU","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Flu__Arac__Thio","BuCy__Flu__Thio__Arac",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Cy__Flu__Thio","BuCy__Flu__Thio__Azathioprine",baz[,var])
    baz[,var]<-gsub("Arac__Bu__CCNU__Cy__Flu","BuCy__Flu__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Amsa__BCNU__Bu__Cy__Flu","BuCy__Flu__Amsa__BCNU",baz[,var])
    
    ## 4 drugs
    baz[,var]<-gsub("Amsa__BuCy__Arac__CCNU","Bu__Cy__Arac__Amsa__CCNU",baz[,var])
    baz[,var]<-gsub("BCNU__BuCy__Flu__Thio","Bu__Cy__Flu__Thio__BCNU",baz[,var])
    
    baz[,var]<-gsub("Bu__Cy__Eto__Flu","BuCy__Flu__Eto",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Cy__Eto","BuCy__Flu__Eto",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Thio__Cy__Flu__Thio","BuCy__Flu__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Arac__Arac__Mel","BuCy__Mel__Arac",baz[,var])
    baz[,var]<-gsub("Bu__Carbo__Cy__Thio","Bu__Cy__Thio__Carbo",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Cy__CCNU","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Arac__Cy__CCNU","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Arac__Chlorambucil__Cy","Bu__Cy__Arac__Chlorambucil",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Chlorambucil__Cy","Bu__Cy__Arac__Chlorambucil",baz[,var])
    baz[,var]<-gsub("Amsa__Bu__Cy__CCNU","BuCy__Amsa__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__CD19__Cy__Flu","BuCy__Flu__CD19",baz[,var])
    
    baz[,var]<-gsub("Arac__Bu__CCNU__Cy","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Clofa__Cy","BuCy__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__CCNU__BuCy__CCNU","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Arac__BuCy__CCNU","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Cy__CCNU","BuCy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Arac__Flu","BuCy__Flu__Arac",baz[,var])
    baz[,var]<-gsub("BuCy__Arac__Flu","BuCy__Flu__Arac",baz[,var])
    baz[,var]<-gsub("BuCy__Arac__Flu","BuCy__Flu__Arac",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Arac__Eto","BuCy__Eto__Arac",baz[,var])
    baz[,var]<-gsub("BuCy__IVIG__Mel","BuCy__Mel__IVIG",baz[,var])
    baz[,var]<-gsub("BuCy__Ritux__Thio","BuCy__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("BCNU__Bu__Cy__Mel","BuCy__Mel__BCNU",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Cy__Flu","BuCy__Flu__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Cy__Eto","BuCy__Eto__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Cy__Thio","BuCy__Thio__Azathioprine",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bu__Cy","BuCy__Arac__Amsa",baz[,var])
    baz[,var]<-gsub("BuFlu__Arac__BCNU__CCNU","BuCy__Arac__BCNU__CCNU",baz[,var])
    
    
    
    ## 3 drugs
    baz[,var]<-gsub("Adriamycine__BuCy","BuCy__Adriamycine",baz[,var])
    
    baz[,var]<-gsub("Bu__Thio__Cy__Thio","BuCy__Thio",baz[,var])
    baz[,var]<-gsub("Bu__CD3__Cy","BuCy__CD3",baz[,var])
    
    baz[,var]<-gsub("Bu__Flu__Cy","BuCy__Flu",baz[,var])
    baz[,var]<-gsub("Bu3__Cy__Flu","BuCy__Flu",baz[,var])
    baz[,var]<-gsub("Bu__Mel__Cy","BuCy__Mel",baz[,var])
    baz[,var]<-gsub("Bu__Eto__Cy","BuCy__Eto",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Cy__Eto","BuCy__Eto",baz[,var])
    baz[,var]<-gsub("Bu__CCNU__Cy","BuCy__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Clofa__Cy","BuCy__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Cy","BuCy__Arac",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Cy","BuCy__Azathioprine",baz[,var])
    
    baz[,var]<-gsub("BuCy__Cy__Mel","BuCy__Mel",baz[,var])
    baz[,var]<-gsub("BuCy__Cy__Eto","BuCy__Eto",baz[,var])
    baz[,var]<-gsub("Bleomycine__Bu__Cy","BuCy__Bleomycine",baz[,var])
    baz[,var]<-gsub("Bleomycine__Bu__Cy","BuCy__Bleomycine",baz[,var])
    baz[,var]<-gsub("Amsa__Bu__Cy__Flu","BuCy__Flu__Amsa",baz[,var])
    baz[,var]<-gsub("Arac__BuCy","BuCy__Arac",baz[,var])
    baz[,var]<-gsub("Bu__Carbo__Cy","BuCy__Carbo",baz[,var])
    baz[,var]<-gsub("Amsa__Bu__Cy","BuCy__Amsa",baz[,var])
    baz[,var]<-gsub("BCNU__Bu__Cy","BuCy__BCNU",baz[,var])
    
    
    ## 2 drugs
    baz[,var]<-gsub("CCNU__Cy","Cy__CCNU",baz[,var])
    baz[,var]<-gsub("BEAM__BuCy","BuCy__BEAM",baz[,var])
    
    baz[,var]<-gsub("Bu__Cy","BuCy",baz[,var])
    baz[,var]<-gsub("Bu__Cy__Cy","BuCy",baz[,var])
    baz[,var]<-gsub("BuCy__Cy","BuCy",baz[,var])
    baz[,var]<-gsub("Bu__BuCy","BuCy",baz[,var])
    baz[,var]<-gsub("BuCy__Bu","BuCy",baz[,var])
    baz[,var]<-gsub("BuCy__BuCy","BuCy",baz[,var])
  }

  flamsalabel<-function(baz,var){
    # Flamsa
    ## 6 drugs
    baz[,var]<-gsub("Amsa__Arac__BuCy__Flu__Thio","BuCy__Flamsa__Thio",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__CCNU__Cy__Flu__Mel","Cy__Mel__Flamsa__CCNU",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Cy__CCNU__Flu__Mel","Cy__Mel__Flamsa__CCNU",baz[,var])
    
    
    ## 5 drugs
    baz[,var]<-gsub("Amsa__Arac__BuCy__Flu","BuCy__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bu__Flu__Mel","BuMel__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bleomycine__Cy__Flu","Cy__Flamsa__Bleomycine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Amsa__Arac__Cy__Flu","Cy__Flamsa__Adriamycine",baz[,var])
    baz[,var]<-gsub("Amsa__Flu__Arac__Cy__Flu","Cy__Flamsa",baz[,var])
    
    
    
    baz[,var]<-gsub("Amsa__Arac__Cy__Flu__Mel","Cy__Mel__Flamsa",baz[,var])
    baz[,var]<-gsub("Cy__Flamsa__Treo","Treo__Cy__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Flu__Arac__Cy__Flu","Cy__Flamsa",baz[,var])
    
    
    ## 4 drugs
    baz[,var]<-gsub("Amsa__Arac__Bu__Flu","Bu__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Cy__Flu","Cy__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Flu__Mel__Arac","Mel__Flamsa",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Flu__Mel","Mel__Flamsa",baz[,var])
    
    ## 3 drugs
    baz[,var]<-gsub("Amsa__Arac__Flu","Flamsa",baz[,var])
    
  }
  
  
  # 1-Bu (ou Treo) 2-Cy 3-Flu 4-Mel 5-Flamsa (Flu Asma Arac) 6-Eto 7-Thio 8-Arac 9-Amsa
  # 10-CCNU 11-BCNU 12-Clofa 13-Carbo 14-Cisplatine 15-Idarubicine 16-Ifosfamide 17-Ritux
  # 17-Anthracycline (dans solide tumeur)
  
  treolabel<-function(baz,var){
    # Treo
    ### TreoFlu based
    
    
    baz[,var]<-gsub("Bortezomib__Mel__Ritux__Flu__Mel__Ritux__Treo","Treo__Flu__Mel__Ritux__Bortezomib",baz[,var])
    

    ##6 drugs
    baz[,var]<-gsub("Bexxar__Bortezomib__Flu__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__Bortezomib__Bexxar",baz[,var])
    
    baz[,var]<-gsub("Bortezomib__Bortezomib__Flu__IL-6__Ritux__Treo__Thio","Treo__Flu__Thio__Ritux__Bortezomib__IL-6",baz[,var])
    baz[,var]<-gsub("Bortezomib__Bortezomib__Flu__IL-6__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__Bortezomib__IL-6",baz[,var])
    baz[,var]<-gsub("Bexxar__Bortezomib__Treo__Flu__Thio__Ritux","Treo__Flu__Thio__Ritux__Bortezomib__Bexxar",baz[,var])
    baz[,var]<-gsub("Bortezomib__Bortezomib__Ritux__Treo__Flu__Ritux","Treo__Flu__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Ritux__Thio__Tocilizumab__Treo","Treo__Flu__Thio__Ritux__Bortezomib__Tocilizumab",baz[,var])
    baz[,var]<-gsub("Bortezomib__Ritux__Tocilizumab__Flu__Thio__Treo","Treo__Flu__Thio__Ritux__Bortezomib__Tocilizumab",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Ritux__Thio__Tocilizumab__Treo","Treo__Flu__Thio__Ritux__Bortezomib__Tocilizumab",baz[,var])
    baz[,var]<-gsub("Bortezomib__Mel__Ritux__Flu__Mel__Ritux__Treo ","Treo__Flu__Mel__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Arac__Clofa__Flu__Mel__Ritux__Treo ","Treo__Flu__Mel__Arac__Clofa__Ritux",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__IL-6__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__IL-6__Bortezomib",baz[,var])
    baz[,var]<-gsub("Arac__Clofa__Flu__Mel__Ritux__Treo","Treo__Flu__Mel__Arac__Clofa__Ritux",baz[,var])
    baz[,var]<-gsub("Bortezomib__IL-6__Flu__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__IL-6__Bortezomib",baz[,var])
    baz[,var]<-gsub("Daratumumab__Flu__IL-6 __Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__IL-6__Daratumumab",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__IL-6__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__IL-6__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__IL-6__Flu__IL-6__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__IL-6__Bortezomib",baz[,var])
    
    baz[,var]<-gsub("Amsa__Arac__Cy__Flu__Taxo__Treo","Treo__Cy__Flamsa__Taxo",baz[,var])
    
    baz[,var]<-gsub("Flu__IL-6 __Treo__Thio__Ritux__Venetoclax","Treo__Flu__Thio__Ritux__IL-6__Venetoclax",baz[,var])
    
    ##5 drugs
    baz[,var]<-gsub("Bortezomib__Bortezomib__Flu__Ritux__Treo__Thio","Treo__Flu__Thio__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Bortezomib__Flu__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Treo__Cy__Flu__Thio__Ritux","Treo__Cy__Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Arac__Treo__Flu__Mel__Clofa","Treo__Flu__Mel__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Flu__Mel__Ritux__Ruxolitinib__Treo","Treo__Flu__Mel__Ritux__Ruxolitinib",baz[,var])
    baz[,var]<-gsub("Treo__Flu__Arac__Treo__Flu","Treo__Flu__Arac",baz[,var])
    
    baz[,var]<-gsub("Arac__Flu__Treo__Flu__Treo","Treo__Flu__Arac",baz[,var])
    baz[,var]<-gsub("Bexxar__Treo__Flu__Mel__Ritux","Treo__FluMel__Ritux__Bexxar",baz[,var])
    baz[,var]<-gsub("Arac__Treo__Flu__Mel__Clofa","Treo__Flu__Mel__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__Clofa__Flu__Mel__Treo","Treo__Flu__Mel__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__IL-6__Treo__Thio__Ritux","Treo__Flu__Thio__Ritux__IL-6__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__IL-6__Flu__IL-6__Treo__Thio__Ritux","Treo__Flu__Thio__Ritux__IL-6__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Thio__Ritux__Tocilizumab__Treo","Treo__Flu__Thio__Ritux__Tocilizumab__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Mel__Ritux__Treo","Treo__Flu__Mel__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Ritux__Mel__Ritux__Treo","Treo__Mel__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bortezomib__Ritux__Flu__Ritux__Treo","Treo__Flu__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Arac__Flu__Mel__Mitoxantrone__Treo","Treo__Flu__Mel__Arac__Mitoxantrone",baz[,var])
    baz[,var]<-gsub("Cy__Flu__Arac__Idarubicine__Treo","Treo__Cy__Flu__Arac__Idarubicine",baz[,var])
    baz[,var]<-gsub("Arac__Cy__Flu__Idarubicine__Treo","Treo__Cy__Flu__Arac__Idarubicine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Vincristine__Mel__Ritux__Treo","Treo__Mel__Ritux__Adriamycine__Vincristine",baz[,var])
    
    
    
    
    ##4 drugs
    baz[,var]<-gsub("Cy__Flamsa__Taxo__Treo","Treo__Cy__Flamsa__Taxo",baz[,var])
    baz[,var]<-gsub("Adriamycine__Vincristine__Treo__Flu","Treo__Flu__Adriamycine__Vincristine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Clofa__Ritux__Treo","Treo__Clofa__Ritux__Adriamycine",baz[,var])
    
    baz[,var]<-gsub("Cy__Flu__Thio__Treo","Treo__Cy__Flu__Thio",baz[,var])
    baz[,var]<-gsub("Arac__Cy__Treo__Clofa","Treo__Cy__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__Treo__Cy__Clofa","Treo__Cy__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__Clofa__Cy__Treo","Treo__Cy__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Cy__Eto__Flu__Treo","Treo__Cy__Flu__Eto",baz[,var])
    baz[,var]<-gsub("Treo__Cy__Eto__Flu","Treo__Cy__Flu__Eto",baz[,var])
    baz[,var]<-gsub("Arac__Treo__Cy__Flu","Treo__Cy__Flu__Arac",baz[,var])
    baz[,var]<-gsub("Arac__Treo__CyFlu","Treo__Cy__Flu__Arac",baz[,var])
    baz[,var]<-gsub("Arac__CCNU__Treo__Cy","Treo__Cy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Arac__Cy__CCNU__Treo","Treo__Cy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Treo__Flu","Treo__Flu__Arac__Amsa",baz[,var])
    baz[,var]<-gsub("CD3__Treo__Flu__Thio","Treo__Flu__Thio__CD3",baz[,var])
    baz[,var]<-gsub("Flu__Imatinib__Thio__Treo","Treo__Flu__Thio__Imatinib",baz[,var])
    baz[,var]<-gsub("Treo__Cy__Flu__Ritux","Treo__Flu__Cy__Ritux",baz[,var])
    baz[,var]<-gsub("Cy__Mel__Treo__Thio","Treo__Cy__Mel__Thio",baz[,var])
    baz[,var]<-gsub("Cy__Mel__Thio__Treo","Treo__Cy__Mel__Thio",baz[,var])
    baz[,var]<-gsub("Flu__Mel__Ritux__Treo","Treo__Flu__Mel__Ritux",baz[,var])
    baz[,var]<-gsub("Cy__Mel__Ritux__Treo","Treo__Cy__Mel__Ritux",baz[,var])
    baz[,var]<-gsub("CD19__Treo__Flu__Thio","Treo__Flu__Thio__CD19",baz[,var])
    baz[,var]<-gsub("Arac__Nelarabine__Treo__Thio","Treo__Thio__Arac__Nelarabine",baz[,var])
    baz[,var]<-gsub("Arac__Treo__Flu__Eto","Treo__Flu__Arac__Eto",baz[,var])
    baz[,var]<-gsub("Azathioprine__Treo__Flu__Thio","Treo__Flu__Thio__Azathioprine",baz[,var])
    baz[,var]<-gsub("Arac__Nelarabine__Treo__Thio","Treo__Thio__Arac__Nelarabine",baz[,var])
    baz[,var]<-gsub("Arac__Nelarabine__Treo__Thio","Treo__Thio__Arac__Nelarabine",baz[,var])
    baz[,var]<-gsub("Arac__Flu__Thio__Treo","Treo__Flu__Thio__Arac",baz[,var])
    baz[,var]<-gsub("Flu__Mel__Thio__Treo","Treo__Flu__Mel__Thio",baz[,var])
    baz[,var]<-gsub("Clofa__Mel__Ritux__Treo","Treo__Mel__Clofa__Ritux",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Mel__Treo","Treo__Flu__Mel__Bortezomib",baz[,var])
    baz[,var]<-gsub("Arac__Nelarabine__Treo__Thio","Treo__Thio__Arac__Nelarabine",baz[,var])
    baz[,var]<-gsub("Treo__Flu__Adriamycine__Thio","Treo__Flu__Thio__Adriamycine",baz[,var])
    baz[,var]<-gsub("Arac__Nelarabine__Thio__Treo","Treo__Thio__Arac__Nelarabine",baz[,var])
    
    
    baz[,var]<-gsub("Treo__Flu__Cy__Mel","Treo__Cy__Flu__Mel",baz[,var])
    baz[,var]<-gsub("Treo__Flu__Cy__Thio","Treo__Cy__Flu__Thio",baz[,var])
    baz[,var]<-gsub("Carbo__Treo__Flu__Thio","Treo__Flu__Thio__Carbo",baz[,var])
    baz[,var]<-gsub("Flu__Ritux__Ritux__Treo__Thio","Treo__Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Flu__Ritux__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Bortezomib__Bortezomib__FluMel__Ritux__Treo","Treo__FluMel__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Arac__Eto__Flu__Treo","Treo__Flu__Eto__Arac",baz[,var])
    baz[,var]<-gsub("CD3__Cy__Mel__Treo","Treo__Cy__Mel__CD3",baz[,var])
    baz[,var]<-gsub("Arac__Mel__Mitoxantrone__Treo","Treo__Mel__Arac__Mitoxantrone",baz[,var])
    baz[,var]<-gsub("Bortezomib__Mel__Ritux__Treo","Treo__Mel__Ritux__Bortezomib",baz[,var])
    
    
    
    baz[,var]<-gsub("Carbo__Treo__Cy__Flu","Treo__Cy__Flu__Carbo",baz[,var])
    baz[,var]<-gsub("Amsa__Flu__Thio__Treo","Treo__Flu__Thio__Amsa",baz[,var])
    baz[,var]<-gsub("Cisplatin__Treo__Flu__Thio","Treo__Flu__Thio__Cisplatin",baz[,var])
    baz[,var]<-gsub("Arac__Treo__Clofa__Mel","Treo__Mel__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Flu__Ritux__Treo__Thio","Treo__Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Flu__Ritux__Thio__Treo","Treo__Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Flu__L-asparaginase__Thio__Treo__Thio__Treo","Treo__Flu__Thio__L-asparaginase",baz[,var])

    baz[,var]<-gsub("Cy__Mel__Treo__Thio","Treo__Cy__Mel__Thio",baz[,var])
    baz[,var]<-gsub("Amsa__Treo__Flu__Venetoclax","Treo__Flu__Amsa__Venetoclax",baz[,var])
    baz[,var]<-gsub("Flu__Mel__Temsirolimus__Treo","Treo__Flu__Mel__Temsirolimus",baz[,var])
    baz[,var]<-gsub("Bu__Clofa__Cy__Treo","Bu__Treo__Cy__Clofa",baz[,var])
    
    ## 3 drugs
    baz[,var]<-gsub("Flu__Midostaurin__Treo","Treo__Flu__Midostaurin",baz[,var])
    
    baz[,var]<-gsub("Arac__Treo__Flu","Treo__Flu__Arac",baz[,var])
    baz[,var]<-gsub("Flu__Ritux__Treo","Treo__Flu__Ritux",baz[,var])
    baz[,var]<-gsub("Adriamycine__Treo__Flu","Treo__Flu__Adriamycine",baz[,var])
    baz[,var]<-gsub("Flu__IVIG__Treo","Treo__Flu__IVIG",baz[,var])
    baz[,var]<-gsub("Flu__Thio__Treo","Treo__Flu__Thio",baz[,var])
    baz[,var]<-gsub("Flu__Treo__Mel","Treo__Flu__Mel",baz[,var])
    baz[,var]<-gsub("Treo__Flu__Cy","Treo__Cy__Flu",baz[,var])
    baz[,var]<-gsub("Ifosfamide__Treo__Mel","Treo__Mel__Ifosfamide",baz[,var])
    baz[,var]<-gsub("BEAM__Thio__Treo","Treo__Thio__BEAM",baz[,var])
    baz[,var]<-gsub("BEAM__Treo__Thio","Treo__Thio__BEAM",baz[,var])
    baz[,var]<-gsub("Eto__Treo__Thio","Treo__Eto__Thio",baz[,var])  
    baz[,var]<-gsub("Arac__Eto__Treo","Treo__Eto__Arac",baz[,var])  
    baz[,var]<-gsub("Ritux__Treo__Thio","Treo__Thio__Ritux",baz[,var])  
    baz[,var]<-gsub("CD19__Treo__Clofa","Treo__Clofa__CD19",baz[,var])  
    baz[,var]<-gsub("CD33__Idarubicine__Treo","Treo__Idarubicine__CD33",baz[,var])  
    baz[,var]<-gsub("Adriamycine__Treo__Cy","Treo__Cy__Adriamycine",baz[,var])  
    baz[,var]<-gsub("Adriamycine__Treo__Mel","Treo__Mel__Adriamycine",baz[,var])  
    baz[,var]<-gsub("Flu__Midostaurin__Treo","Treo__Flu__Midostaurin",baz[,var])  
    
    baz[,var]<-gsub("Cy__Ritux__Treo","Treo__Cy__Ritux",baz[,var])  
    
    baz[,var]<-gsub("Flu__Mel__Treo","Treo__Flu__Mel",baz[,var])  
    baz[,var]<-gsub("Cy__Mel__Treo","Treo__Cy__Mel",baz[,var])
    baz[,var]<-gsub("Cy__Eto__Treo","Treo__Cy__Eto",baz[,var]) 
    baz[,var]<-gsub("Cy__Thio__Treo","Treo__Cy__Thio",baz[,var]) 
    baz[,var]<-gsub("Clofa__Thio__Treo","Treo__Thio__Clofa",baz[,var]) 
    baz[,var]<-gsub("Clofa__Ritux__Treo","Treo__Clofa__Ritux",baz[,var]) 
    baz[,var]<-gsub("Treo__Clofa__Flu","Treo__Flu__Clofa",baz[,var]) 
    baz[,var]<-gsub("Treo__Clofa__Mel","Treo__Mel__Clofa",baz[,var]) 
    baz[,var]<-gsub("Eto__Treo__Flu","Treo__Flu__Eto",baz[,var])
    baz[,var]<-gsub("Eto__Treo__Thio","Treo__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Eto__Thio__Treo","Treo__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Ritux__Treo__Thio","Treo__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Ritux__Thio__Treo","Treo__Thio__Ritux",baz[,var])
    
    baz[,var]<-gsub("Adriamycine__Cy__Treo","Treo__Cy__Adriamycine",baz[,var]) 
    baz[,var]<-gsub("Bexxar__Flu__Treo","Treo__Flu__Bexxar",baz[,var]) 
    baz[,var]<-gsub("CD33__Mel__Treo","Treo__Mel__CD33",baz[,var]) 
    
    baz[,var]<-gsub("Adriamycine__Treo__Flu","Treo__Flu__Adriamycine",baz[,var]) 
    baz[,var]<-gsub("Amsa__Flu__Treo","Treo__Flu__Amsa",baz[,var]) 
    
    baz[,var]<-gsub("Chlorambucil__Clofa__Treo","Treo__Clofa__Chlorambucil",baz[,var]) 
    baz[,var]<-gsub("Flu__Thalidomide__Treo","Treo__Flu__Thalidomide",baz[,var]) 
    baz[,var]<-gsub("Mel__Thio__Treo","Treo__Thio__Mel",baz[,var]) 
    baz[,var]<-gsub("R-GIfOx__Thio__Treo","Treo__Thio__R-GIfOx",baz[,var]) 
    
    ## 2 drugs
    baz[,var]<-gsub("Flu__Treo","Treo__Flu",baz[,var]) 
    baz[,var]<-gsub("ABVD__Flu","Flu__ABVD",baz[,var])    
    
    baz[,var]<-gsub("Mel__Treo","Treo__Mel",baz[,var])    
    baz[,var]<-gsub("Cy__Treo","Treo__Cy",baz[,var])  
    baz[,var]<-gsub("Clofa__Treo","Treo__Clofa",baz[,var])  
    baz[,var]<-gsub("Eto__Treo","Treo__Eto",baz[,var])  
    baz[,var]<-gsub("CD33__Treo","Treo__CD33",baz[,var])  
    baz[,var]<-gsub("Ritux__Treo","Treo__Ritux",baz[,var])  
    baz[,var]<-gsub("Fluorouracil__Treo","Treo__Fluorouracil",baz[,var])  
    
    
    
  }
  
  buflulabel<-function(baz,var){
    ### BuFlu based
    ## 5 drugs
    baz[,var]<-gsub("Bu__Eto__Flu__Mel__Thio","BuFlu__Mel__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Flu__IVIG__Ritux__Thio","BuFlu__Thio__Ritux__IVIG",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Eto__Flu__Bu__Eto__Flu","BuFlu__Eto__Arac",baz[,var])
    
    ## 4 drugs
    baz[,var]<-gsub("Bu__Flu__Clofa__Thio","BuFlu__Thio__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Daunorubicin__Flu","BuFlu__Arac__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Clofa__Flu","BuFlu__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Bu__CD8__Flu__Thio","BuFlu__Thio__CD8",baz[,var])
    baz[,var]<-gsub("Bu__Daunorubicin__Flu__Thio","BuFlu__Thio__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Bleomycine__Bu__Flu__Mel","BuFlu__Mel__Bleomycine",baz[,var])
    
    baz[,var]<-gsub("Bu__Eto__Flu__Thio","BuFlu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Idarubicine__Thio","BuFlu__Thio__Idarubicine",baz[,var])
    baz[,var]<-gsub("BuFlu__Arac__Thio","BuFlu__Thio__Arac",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Ritux__Thio","Bu__Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Gemcitabine__Ritux__Thio","Bu__Flu__Thio__Ritux__Gemcitabine",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Clofa__Thio","BuFlu__Thio__Clofa",baz[,var])
    baz[,var]<-gsub("Bu__Flu__IntrathecalChemo__Thio","BuFlu__Thio__IntrathecalChemo",baz[,var])
    baz[,var]<-gsub("Bu__Flu__IVIG__Thio","BuFlu__Thio__IVIG",baz[,var])
    baz[,var]<-gsub("BuFlu__Clofa__Thio","BuFlu__Thio__Clofa",baz[,var])
    
    baz[,var]<-gsub("Adriamycine__Bu__Flu__Thio","Bu__Flu__Thio__Adriamycine",baz[,var])
    baz[,var]<-gsub("Bu__CD3__Flu__Thio","Bu__Flu__Thio__CD3",baz[,var])
    baz[,var]<-gsub("Bu__Flu__IVIG__Mel","Bu__Flu__Mel__IVIG",baz[,var])
    baz[,var]<-gsub("Bu__Flu__CD3__Mel","Bu__Flu__Mel__CD3",baz[,var])
    baz[,var]<-gsub("Bu__CD3__Flu__Mel","Bu__Flu__Mel__CD3",baz[,var])
    baz[,var]<-gsub("Bu__CD3__CD3__Flu__Mel","BuFlu__Mel__CD3",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Mercaptapur6__Thio","BuFlu__Thio__Mercaptapur6",baz[,var])
    baz[,var]<-gsub("Adriamycine__Amsa__Bu__Flu","BuFlu__Amsa__Adriamycine",baz[,var])
    baz[,var]<-gsub("Arac__Bu__CCNU__Flu","BuFlu__Arac__CCNU",baz[,var])
    
    
    ## 3 drugs
    baz[,var]<-gsub("Bu__Flu__Thio__Flu__Thio","BuFlu__Thio",baz[,var])
    baz[,var]<-gsub("Bleomycine__Bu__Flu","BuFlu__Bleomycine",baz[,var])
    
    baz[,var]<-gsub("Bu__Flu__Ritux__Ritux","BuFlu__Ritux",baz[,var])
    baz[,var]<-gsub("Bu__CEP__Flu__Thio","BuFlu__Thio__CEP",baz[,var])
    baz[,var]<-gsub("Bu__CD3__CD3__Flu","BuFlu__CD3",baz[,var])
    baz[,var]<-gsub("Bu__Flu__Mel__Flu__Mel","BuFlu__Mel",baz[,var])
    baz[,var]<-gsub("Bu__Eto__Flu","BuFlu__Eto",baz[,var])
    baz[,var]<-gsub("Arac__Bu__Flu","BuFlu__Arac",baz[,var])
    baz[,var]<-gsub("Bu__CCNU__Flu","BuFlu__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Clofa__Flu","BuFlu__Clofa",baz[,var])
    baz[,var]<-gsub("ABVD__BuFlu","BuFlu__ABVD",baz[,var])
    baz[,var]<-gsub("Bu__Carbo__Flu","BuFlu__Carbo",baz[,var])
    baz[,var]<-gsub("Bu__Cisplatin__Flu","BuFlu__Cisplatin",baz[,var])
    baz[,var]<-gsub("Bu__Dactinomycin__Flu","BuFlu__Dactinomycin",baz[,var])
    baz[,var]<-gsub("Amsa__Bu__Flu","BuFlu__Amsa",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Flu","BuFlu__Azathioprine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Bu__Flu","BuFlu__Adriamycine",baz[,var])
    baz[,var]<-gsub("Bu__Daunorubicin__Flu","BuFlu__Daunorubicin",baz[,var])
    
    
    ## 2 drugs
    baz[,var]<-gsub("Bu__Flu","BuFlu",baz[,var])
    baz[,var]<-gsub("BuFlu__BuFlu","BuFlu",baz[,var])
  }
  
  cyflulabel<-function(baz,var){
    
    baz[,var]<-gsub("Adriamycine__Vincristine__Cy__Flu","Cy__Flu__Adriamycine__Vincristine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Arac__Carbo__Cisplatin__Cy__Eto__Flu","Cy__Flu__Eto__Arac__Carbo__Cisplatin__Adriamycine",baz[,var])
    
    
    
    ## 3 drugs
    baz[,var]<-gsub("Cy__Flu__Mel","Cy__Flu__Mel",baz[,var])
    baz[,var]<-gsub("Cy__Daunorubicin__Flu","Cy__Flu__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Arac__Cy__Flu","Cy__Flu__Arac",baz[,var])
    baz[,var]<-gsub("Clofa__Cy__Flu","Cy__Flu__Clofa",baz[,var])
    
    ## 2 drugs
    baz[,var]<-gsub("Clofa__Cy","Cy__Clofa",baz[,var])
  }
  
  jolilabel<-function(baz,var){
    
    ### Beam
    baz[,var]<-gsub("Arac__BCNU__Eto__Mel","BEAM",baz[,var])
    baz[,var]<-gsub("Arac__BCNU__Eto__Flu__Mel","BEAM__Flu",baz[,var])
    
    baz[,var]<-gsub("Arac__BEAM","BEAM",baz[,var])
    baz[,var]<-gsub("Arac__BCNU__BEAM__Eto__Mel","BEAM",baz[,var])
    baz[,var]<-gsub("Arac__Eto__Fotemustine__Mel","FEAM",baz[,var])
    # baz[,var]<-gsub("BCNU__Cy__Eto","CBV",baz[,var])
    baz[,var]<-gsub("Arac__Benda__Eto__Mel","BeEAM",baz[,var])
    baz[,var]<-gsub("BEAM__Eto__Fotemustine__Treo__Mel","BEAM__Treo__Fotemustine",baz[,var])
    baz[,var]<-gsub("BEAM__Eto__Fotemustine__Mel","BEAM__Fotemustine",baz[,var])
    
    # baz[,var]<-gsub("mini BEAM","BEAM",baz[,var])
    
    #### 6 drugs
    baz[,var]<-gsub("Bortezomib__Flu__IL-6 __Ritux__Thio__Venetoclax","Flu__Thio__Ritux__Bortezomib__IL-6 __Venetoclax",baz[,var])
    baz[,var]<-gsub("Arac__Cy__Daunorubicin__Teniposide__Vinblastine__Vincristine","Cy__Arac__Daunorubicin__Teniposide__Vinblastine__Vincristine",baz[,var])
    baz[,var]<-gsub("Carbo__Flu__Mitoxantrone__Ritux__Taxo__Thio","Flu__Thio__Ritux__Carbo__Mitoxantrone__Taxo",baz[,var])
    
    baz[,var]<-gsub("Arac__Cy__Daunorubicin__Teniposide__Vincristine","Cy__Arac__Daunorubicin__Teniposide__Vincristine",baz[,var])
    
    
    
    baz[,var]<-gsub("Cy__Clofa__Eto__Flu__Mel__Thio","Flu__Cy__Mel__Eto__Thio__Clofa",baz[,var])
    baz[,var]<-gsub("BCNU__Flu__Mel__BCNU__Flu__Mel","Flu__Mel__BCNU",baz[,var])
    baz[,var]<-gsub("BCNU__BCNU__Flu__Flu__Mel__Mel","Flu__Mel__BCNU",baz[,var])
    ### 5 drugs
    baz[,var]<-gsub("Bortezomib__Flu__IL-6__Ritux__Thio","Flu__Thio__Ritux__Bortezomib__IL-6",baz[,var])
    baz[,var]<-gsub("Bortezomib__IL-6__Flu__Ritux__Thio","Flu__Thio__Ritux__Bortezomib__IL-6",baz[,var])
    
    
    
    ### 4 drugs
    baz[,var]<-gsub("Bu__Flu__Clofa__Thio","Bu__Flu__Thio__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__BCNU__Eto__Ifosfamide","Eto__Arac__BCNU__Ifosfamide",baz[,var])
    baz[,var]<-gsub("Arac__BCNU__Flu__Thio","Flu__Thio__Arac__BCNU",baz[,var])
    baz[,var]<-gsub("Flu__ICE__Mel__Thio","Flu__Mel__Thio__ICE",baz[,var])
    baz[,var]<-gsub("BCNU__Flu__Mel__Flu__Mel","Flu__Mel__BCNU",baz[,var])
    baz[,var]<-gsub("Arac__BCNU__Flu__Mel","Flu__Mel__Arac__BCNU",baz[,var])
    
    baz[,var]<-gsub("Arac__Eto__Flu__Mel","Flu__Mel__Eto__Arac",baz[,var])
    baz[,var]<-gsub("Arac__CCNU__Eto__Mel","Mel__Eto__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Arac__Cy__CCNU__Eto","Cy__Eto__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Flu__Arac__Idarubicine__Mel","Flu__Mel__Arac__Idarubicine",baz[,var])
    baz[,var]<-gsub("CD3__Flu__Mel__Thio","Flu__Mel__Thio__CD3",baz[,var])
    baz[,var]<-gsub("BCNU__Flu__IVIG__Mel","Flu__Mel__BCNU__IVIG",baz[,var])
    baz[,var]<-gsub("Flu__Bleomycine__Ritux__Thio","Flu__Thio__Ritux__Bleomycine",baz[,var])
    baz[,var]<-gsub("FluMel__Ritux__Thio","Flu__Mel__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Adriamycine__BuMel__IVIG","BuMel__Adriamycine__IVIG",baz[,var])
    baz[,var]<-gsub("Arac__Cy__Daunorubicin__Vincristine","Cy__Arac__Daunorubicin__Vincristine",baz[,var])
    baz[,var]<-gsub("Cisplatin__Flu__Mel__Thio","Flu__Mel__Thio__Cisplatin",baz[,var])
    baz[,var]<-gsub("Eto__Flu__Cy__Mitoxantrone","Cy__Flu__Eto__Mitoxantrone",baz[,var])
    baz[,var]<-gsub("Bortezomib__Flu__Ritux__Thio","Flu__Thio__Ritux__Bortezomib",baz[,var])
    baz[,var]<-gsub("Bleomycine__Flu__Ritux__Thio","Flu__Thio__Ritux__Bleomycine",baz[,var])
    baz[,var]<-gsub("Bortezomib__Cy__Flu__Thio","Cy__Flu__Thio__Bortezomib",baz[,var])
    baz[,var]<-gsub("Eto__Flu__Mel__Thio","Flu__Mel__Thio__Eto",baz[,var])
    baz[,var]<-gsub("BCNU__Cy__Flu__Mel","Cy__Flu__Mel__BCNU",baz[,var])
    baz[,var]<-gsub("BCNU__CCNU__Flu__Mel","Flu__Mel__BCNU__CCNU",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bu__Eto","Bu__Eto__Arac__Amsa",baz[,var])
    baz[,var]<-gsub("Adriamycine__Vincristine__Bu__Thio","Bu__Thio__Adriamycine__Vincristine",baz[,var])
    
  
    
    ### 3 drugs
    baz[,var]<-gsub("Arac__CCNU__Cy","Cy__Arac__CCNU",baz[,var])
    baz[,var]<-gsub("Arac__Flu__Mel","Flu__Mel__Arac",baz[,var])
    baz[,var]<-gsub("BCNU__Flu__Mel","Flu__Mel__BCNU",baz[,var])
    baz[,var]<-gsub("Eto__Flu__Thio","Flu__Eto__Thio",baz[,var])
    baz[,var]<-gsub("Arac__Flu__Idarubicine","Flu__Arac__Idarubicine",baz[,var])
    baz[,var]<-gsub("Bu__AracMel","Bu__Mel__Arac",baz[,var])
    baz[,var]<-gsub("Bu__IVIG__Mel","Bu__Mel__IVIG",baz[,var])
    baz[,var]<-gsub("Adriamycine__Bu__Clofa","Bu__Clofa__Adriamycine",baz[,var])
    baz[,var]<-gsub("Arac__ITT AMH\\(intrathecal\\)__Eto","Eto__Arac__ITT AMH\\(intrathecal\\)",baz[,var])
    baz[,var]<-gsub("Flu__IVIG__Thio","Flu__Thio__IVIG",baz[,var])
    baz[,var]<-gsub("Amsa__Flu__Mel","Flu__Mel__Amsa",baz[,var])
    baz[,var]<-gsub("FLAC__Flu__Mel","Cy__Flu__Mel__Adriamycine__Fluorouracil",baz[,var])
    
    baz[,var]<-gsub("Arac__BCNU__Mel","Mel__Arac__BCNU",baz[,var])
    baz[,var]<-gsub("Mel__Arac__Clofa","Mel__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Bu__Arac__Eto","Bu__Eto__Arac",baz[,var])
    baz[,var]<-gsub("Arac__Eto__Mel","Mel__Eto__Arac",baz[,var])
    baz[,var]<-gsub("Carbo__Cy__Eto","Cy__Eto__Carbo",baz[,var])
    baz[,var]<-gsub("Carbo__Cy__Vincristine","Cy__Carbo__Vincristine",baz[,var])
    baz[,var]<-gsub("Carbo__Eto__Mel","Mel__Eto__Carbo",baz[,var])
    baz[,var]<-gsub("Carbo__Eto__Thio","Eto__Thio__Carbo",baz[,var])
    baz[,var]<-gsub("Cy__BCNU__Eto","Cy__Eto__BCNU",baz[,var])
    baz[,var]<-gsub("Cy__Ritux__Ritux__Thio","Cy__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("CD3__Cy__Flu","Cy__Flu__CD3",baz[,var])
    baz[,var]<-gsub("Cy__Ritux__Thio","Cy__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Mel__Clofa__Eto","Mel__Eto__Clofa",baz[,var])
    
    
    
    baz[,var]<-gsub("Eto__FluMel","Flu__Mel__Eto",baz[,var])
    baz[,var]<-gsub("Cy__Eto__BCNU","CBV",baz[,var])
    baz[,var]<-gsub("Arac__Flu__Eto","Flu__Eto__Arac",baz[,var])
    baz[,var]<-gsub("Flu__Bleomycine__Thio","Flu__Thio__Bleomycine",baz[,var])
    baz[,var]<-gsub("Arac__Daunorubicin__Flu","Flu__Arac__Daunorubicin",baz[,var])
    baz[,var]<-gsub("Bexxar__Eto__Ritux","Eto__Ritux__Bexxar",baz[,var])
    
    
    baz[,var]<-gsub("Clofa__Mel__Thio","Mel__Thio__Clofa",baz[,var])
    baz[,var]<-gsub("CCNU__Flu__Mel","Flu__Mel__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Clofa__Mel","Bu__Mel__Clofa",baz[,var])
    baz[,var]<-gsub("Mel__BCNU__Eto","Mel__Eto__BCNU",baz[,var])
    baz[,var]<-gsub("Mel__Ritux__Thio","Mel__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("Adriamycine__Bu__Mel","Bu__Mel__Adriamycine",baz[,var])
    baz[,var]<-gsub("Azathioprine__Bu__Flu","Bu__Flu__Azathioprine",baz[,var])
    
    baz[,var]<-gsub("CD33__Flu__Mel","Flu__Mel__CD33",baz[,var])
    
    
    baz[,var]<-gsub("Arac__Cy__Clofa","Cy__Arac__Clofa",baz[,var])
    baz[,var]<-gsub("Bleomycine__Cy__Mel","Cy__Mel__Bleomycine",baz[,var])
    
    baz[,var]<-gsub("Arac__Flu__Mitoxantrone","Flu__Arac__Mitoxantrone",baz[,var])
    baz[,var]<-gsub("BCNU__Flu__Eto","Flu__Eto__BCNU",baz[,var])
    baz[,var]<-gsub("BCNU__Flu__Thio","Flu__Thio__BCNU",baz[,var])
    baz[,var]<-gsub("Bu__CCNU__Mel","Bu__Mel__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__Mel__Clofa","Bu__Mel__CCNU",baz[,var])
    baz[,var]<-gsub("Bu__IVIG__Thio","Bu__Thio__IVIG",baz[,var])
    baz[,var]<-gsub("Arac__Azathioprine__Bu","Bu__Arac__Azathioprine",baz[,var])
    
    baz[,var]<-gsub("Flu__Ritux__Thio","Flu__Thio__Ritux",baz[,var])
    baz[,var]<-gsub("BCNU__Eto__Mel","Mel__Eto__BCNU",baz[,var])
    baz[,var]<-gsub("Clofa__Flu__Mel","Flu__Mel__Clofa",baz[,var])
    baz[,var]<-gsub("Arac__Flu__Thio","Flu__Thio__Arac",baz[,var])
    baz[,var]<-gsub("Amsa__Arac__Bu","Bu__Arac__Amsa",baz[,var])
    
    
    ### 2 drugs
    baz[,var]<-gsub("Bu__Mel","BuMel",baz[,var])
    baz[,var]<-gsub("BuFluorouracil","Bu__Fluorouracil",baz[,var])
    baz[,var]<-gsub("ACVB__Bu","Bu__ACVB",baz[,var])
    baz[,var]<-gsub("Flu__Mel","FluMel",baz[,var])
    baz[,var]<-gsub("Arac__Bu","Bu__Arac",baz[,var])
    baz[,var]<-gsub("Arac__Mel","Mel__Arac",baz[,var])
    baz[,var]<-gsub("Arac__Flu","Flu__Arac",baz[,var])
    baz[,var]<-gsub("R-GIfOx__Thio","Thio__R-GIfOx",baz[,var])
    baz[,var]<-gsub("Adriamycine__Eto","Eto__Adriamycine",baz[,var])
    baz[,var]<-gsub("ChlVPP/ PABlOE__Flu","Flu__ChlVPP/ PABlOE",baz[,var])
    
    baz[,var]<-gsub("Benda__Flu","Flu__Benda",baz[,var])
    baz[,var]<-gsub("Chlorambucil__Flu","Flu__Chlorambucil",baz[,var])
    baz[,var]<-gsub("Carbo__Eto","Eto__Carbo",baz[,var])
    baz[,var]<-gsub("Carbo__Mel","Mel__Carbo",baz[,var])
    baz[,var]<-gsub("Cladribine__Mel","Mel__Cladribine",baz[,var])
    baz[,var]<-gsub("Carbo__Thio","Thio__Carbo",baz[,var])
    baz[,var]<-gsub("Amsa__Cy","Cy__Amsa",baz[,var])
    baz[,var]<-gsub("Daunorubicin__Eto","Eto__Daunorubicin",baz[,var])
    
    baz[,var]<-gsub("CCNU__Flu","Flu__CCNU",baz[,var])
    
    baz[,var]<-gsub("BCNU__Cy","Cy__BCNU",baz[,var])
    baz[,var]<-gsub("BCNU__Mel","Mel__BCNU",baz[,var])
    baz[,var]<-gsub("BCNU__Thio","Thio__BCNU",baz[,var])
    baz[,var]<-gsub("BCNU__Flu","Flu__BCNU",baz[,var])
    baz[,var]<-gsub("Bleomycine__Flu","Flu__Bleomycine",baz[,var])
    baz[,var]<-gsub("Eto__Flu","Flu__Eto",baz[,var])
    baz[,var]<-gsub("Eto__Mel","Mel__Eto",baz[,var])
    baz[,var]<-gsub("Amsa__Flu","Flu__Amsa",baz[,var])
    baz[,var]<-gsub("Amsa__Cy","Cy__Amsa",baz[,var])
    baz[,var]<-gsub("Carbo__Cy","Cy__Carbo",baz[,var])
    baz[,var]<-gsub("Clofa__Mel","Mel__Clofa",baz[,var])
    
    baz[,var]<-gsub("Bleomycine__Cy","Cy__Bleomycine",baz[,var])
    baz[,var]<-gsub("Arac__Cy","Cy__Arac",baz[,var])
    baz[,var]<-gsub("Arac__Eto","Eto__Arac",baz[,var])
    baz[,var]<-gsub("Adriamycine__Cy","Cy__Adriamycine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Bu","Bu__Adriamycine",baz[,var])
    baz[,var]<-gsub("Adriamycine__Flu","Flu__Adriamycine",baz[,var])
    baz[,var]<-gsub("Carbo__Flu","Flu__Carbo",baz[,var])
    baz[,var]<-gsub("Epirubicine__Flu","Flu__Epirubicine",baz[,var])
    
    baz[,var]<-gsub("Thio__Treo","Treo__Thio",baz[,var])
    baz[,var]<-gsub("Anthracycline__BuMel","BuMel__Anthracycline",baz[,var])
    baz[,var]<-gsub("Daunorubicin__Eto","Eto__Daunorubicin",baz[,var])
    
    baz[,var]<-gsub("TBI__NA","TBI alone",baz[,var])
    
    return(baz[,var])
  }    

###############################################################################################
###############################################################################################
####################### Conditioning regimen in GVHD prevention ###############################
###############################################################################################
###############################################################################################

baz$condinprev<-NA
for (i in 1:dim(baz)[1]){
  if(sum(is.na(baz$PREVDRUG1[i])+
         is.na(baz$PREVDRUG2[i])+
         is.na(baz$PREVDRUG3[i])+
         is.na(baz$PREVDRUG4[i])+
         is.na(baz$PREVDRUG5[i])+
         is.na(baz$PREVDRUG6[i])+
         is.na(baz$PREVDRUG7[i]))==7){
    baz$condinprev[i]<-NA
  }else{
    baz$condinprev[i]<-paste(sort(c( as.character(baz$PREVDRUG1[i]),
                                     as.character(baz$PREVDRUG2[i]),
                                     as.character(baz$PREVDRUG3[i]),
                                     as.character(baz$PREVDRUG4[i]),
                                     as.character(baz$PREVDRUG5[i]),
                                     as.character(baz$PREVDRUG6[i]),
                                     as.character(baz$PREVDRUG7[i]))),collapse='__') }
}

baz$condinprev<-gsub("^\\s+|\\s+$","",baz$condinprev)

baz$condinprev<-gsub(paste0("Cyclo","__"),"",baz$condinprev)
baz$condinprev<-gsub(paste0("__","Cyclo"),"",baz$condinprev)
baz$condinprev<-gsub("Cyclo",NA,baz$condinprev)

## Retrait de pas condi

for( i in pascondi){
  
  baz$condinprev<-gsub(paste0(i,"__"),"",baz$condinprev)
  baz$condinprev<-gsub(paste0("__",i),"",baz$condinprev)
  baz$condinprev<-gsub(i,NA,baz$condinprev)
}

baz$condinprev<-gsub("Other__","",baz$condinprev)
baz$condinprev<-gsub("__Other","",baz$condinprev)
baz$condinprev<-gsub("Other",NA,baz$condinprev)

###############################################################################################
###############################################################################################
####################### Conditioning regimen  #################################################
###############################################################################################
###############################################################################################

tempTBI<-baz$VRADICON
tempTBI[is.na(baz$VRADICON)]<-"No"

  baz$cond<-NA
  for (i in 1:dim(baz)[1]){
    if(sum(is.na(baz$condinprev[i])+
           is.na(baz$CONDDRUG1[i])+
           is.na(baz$CONDDRUG2[i])+
           is.na(baz$CONDDRUG3[i])+
           is.na(baz$CONDDRUG4[i])+
           is.na(baz$CONDDRUG5[i])+
           is.na(baz$CONDDRUG6[i])+
           is.na(baz$CONDDRUG7[i]))==8){
      baz$cond[i]<-NA
    }else{
      baz$cond[i]<-paste(sort(c(
        baz$condinprev[i],
        as.character(baz$CONDDRUG1[i]),
        as.character(baz$CONDDRUG2[i]),
        as.character(baz$CONDDRUG3[i]),
        as.character(baz$CONDDRUG4[i]),
        as.character(baz$CONDDRUG5[i]),
        as.character(baz$CONDDRUG6[i]),
        as.character(baz$CONDDRUG7[i]))),collapse='__') }
  }

baz$cond<-gsub("Cyclo","Cy",baz$cond)
baz$cond<-doublelabel(baz,"cond")

baz$cond2<-baz$cond

baz$cond3<-baz$cond2

baz$cond<-ifelse(tempTBI=="Yes",paste0("TBI__",baz$cond),baz$cond)
baz$cond<-gsub("__","+",baz$cond)
  
baz$cond2<-ifelse(tempTBI=="Yes",paste0("TBI__",baz$cond2),baz$cond2)
baz$cond2<-gsub("__","+",baz$cond2)
  
## Retrait de pascondi
  
  for( i in pascondi){
    
    baz$cond3<-gsub(paste0(i,"__"),"",baz$cond3)
    baz$cond3<-gsub(paste0("__",i),"",baz$cond3)
    baz$cond3<-gsub(i,NA,baz$cond3)
  }

baz$cond3<-gsub("Other__","",baz$cond3)
baz$cond3<-gsub("__Other","",baz$cond3)

baz$cond4<-baz$cond3

baz$cond3<-ifelse(tempTBI=="Yes",paste0("TBI__",baz$cond3),baz$cond3)
baz$cond3<-gsub("__","+",baz$cond3)
baz$cond3<-as.factor(baz$cond3)

# Retrait de pascondi2
  
  for ( i in pascondi2){
    baz$cond4<-gsub(paste0(i,"__"),"",baz$cond4)
    baz$cond4<-gsub(paste0("__",i),"",baz$cond4)
    baz$cond4<-gsub(i,NA,baz$cond4)
  }
  
  baz$cond4<-doublelabel(baz,"cond4")
  baz$cond4<-bucylabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  baz$cond4<-bucylabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  
  baz$cond4<-flamsalabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  baz$cond4<-flamsalabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  
  baz$cond4<-treolabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  baz$cond4<-treolabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  
  baz$cond4<-buflulabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  baz$cond4<-buflulabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")  
  
  baz$cond4<-cyflulabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4")
  baz$cond4<-cyflulabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4") 
  
  baz$cond4<-jolilabel(baz,"cond4")
  baz$cond4<-doublelabel(baz,"cond4") 
  baz$cond4<-jolilabel(baz,"cond4")

  baz$cond4<-gsub("Arac__BCNU__Brentux__Eto__Mel","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("Arac__BCNU__Brentux__Eto__Mel__Nivolumab__Vinorelbine","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("Arac__BCNU__Ciprofloxacin__Eto__Mel__Metronidazole","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Treo__Fotemustine","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Fotemustine","BEAM+Other",baz$cond4)
  
  baz$cond4<-gsub("Arac__BCNU__Eto__GM-CSF__Mel","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Eto__Fotemustine__Mel__Treo","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Eto__Ifosfamide","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Flu__Treo","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Flu","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__ITT AMH\\(intrathecal\\)","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Pegfilgrastim","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Tevagastrim","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("BEAM__Zevalin","BEAM+Other",baz$cond4)
  baz$cond4<-gsub("R-DEXA-BEAM","BEAM+Other",baz$cond4)
  
  baz$cond4<-gsub("Cisplatin__Eto__Ifosfamide","ICE (cis or carbo)",baz$cond4)
  baz$cond4<-gsub("Eto__Cisplatin__Ifosfamide","ICE (cis or carbo)",baz$cond4)
  
  baz$cond4<-gsub("Carbo__Eto__Ifosfamide","ICE (cis or carbo)",baz$cond4)
  baz$cond4<-gsub("Eto__Carbo__Ifosfamide","ICE (cis or carbo)",baz$cond4)
  
  baz$cond5<-baz$cond4
  
  baz$cond4dose<-ifelse(tempTBI=="Yes",paste(baz$DOSETBINBIN,baz$cond4,sep="__"),baz$cond4)
  baz$cond4dose<-jolilabel(baz,"cond4dose")
  
  baz$cond4<-ifelse(tempTBI=="Yes",paste0("TBI__",baz$cond4),baz$cond4)
  baz$cond4<-jolilabel(baz,"cond4")
  
  baz$cond4<-gsub("__","+",baz$cond4)
  baz$cond4dose<-gsub("__","+",baz$cond4dose)
  
  baz$cond4<-as.factor(baz$cond4)
  baz$cond4dose<-as.factor(baz$cond4dose)
  
  treobased<-c("Treo",
               "Treo__CD33",
               "Treo__Clofa__CD19",
               "Treo__Idarubicine__CD33",
               "Treo__Flu__Thio__L-asparaginase","Treo__Flu__Thio__Azathioprine",
               "Treo__Cy","Treo__Cy__Mel","Treo__Cy__Mel__Thio","Treo__Cy__Eto","Treo__Cy__Thio","Treo__Cy__Ritux",
               "Treo__Cy__Flu__Clofa__Ritux","Treo__Cy__FluMel__Ritux",
               "Treo__Eto",
               "Treo__Thio","Treo__Thio__Mel","Treo__Thio__R-GIfOx",
               "Treo__Mel","Treo__Clofa","Treo__Clofa__Ritux","Treo__Clofa__Chlorambucil",
               "Treo__Cy__Flu", "Treo__Cy__Flu__Mel","Treo__Cy__FluMel","Treo__Cy__Flu__Ritux",
               "Treo__Cy__Flu__Thio__Ritux","Treo__Cy__Mel__Thio__Ritux",
               "Treo__Cy__Mel__Ritux","Treo__Cy__Adriamycine",
               "Treo__Mel__Ifosfamide","Treo__Mel__Clofa","Treo__Mel__Ritux__Bortezomib","Treo__Mel__CD33","Treo__Flu__Thio__Ritux__IL-6__Venetoclax",
               "reo__Flu__Thio__Ritux__IL-6__Bortezomib","Treo__Flu__Thio__Ritux__Bortezomib__Tocilizumab",
               "Treo__Flu","Treo__FluMel","Treo__Flu__Thio__Carbo","Treo__Flu__Thio__Cisplatin","Treo__Flu__Thio__CD3","Treo__Flu__Thalidomide","Treo__Flu__Bexxar",
               "Treo__FluMel__Ritux__Bortezomib",
               "Treo__FluMel__Arac__Clofa","Treo__FluMel__Arac",
               "Treo__FluMel__Ritux","Treo__FluMel__Ritux__Bexxar","Treo__FluMel__Ritux__Ruxolitinib",
               "Treo__Flu__Adriamycine",
               "Treo__Flu__Thio__Amsa","Treo__Flu__Thio__Ritux","Treo__Flu__Thio__Ritux__Bortezomib",
               "Treo__Flu__Thio__Ritux__Bortezomib__IL-6","Treo__Flu__Thio__Imatinib",
               "Treo__Flu__Thio__Ritux__IL-6__Bortezomib",
               "Treo__Flu__Thio__CD19",
               "Treo__Flu__Thio__Adriamycine",
               "Treo__Flu__Thio__Vindesine",
               "Treo__Flu__Arac",
               "Treo__Flu__Arac__Amsa","Treo__Flu__Adriamycine",
               "Treo__Flu__Eto","Treo__Flu__Eto__Ritux",
               "Treo__Flu__Clofa",
               "Treo__Flu__Ritux","Treo__Flu__Ritux__Bortezomib","Treo__Flu__Thio__Ritux__Bortezomib__Bexxar",
               "Treo__Eto__Arac",
               "Treo__Flu__Amsa",
               "Treo__Mel__Ritux__Adriamycine__Vincristine",
               "Treo__Eto__Thio","Treo__Mel__Arac__Mitoxantrone","Treo__FluMel__Arac__Mitoxantrone",
               "Treo__Flu__IVIG","Treo__FluMel__Thio","Treo__FluMel__Arac__Clofa__Ritux",
               "Treo__Thio__Clofa","Treo__Flu__Thio__Arac",
               "Treo__Thio__Arac__Nelarabine","Treo__Flu__Thio__Vindesine",
               "Treo__Thio__Ritux","Treo__Flu__Eto__Arac",
               "Treo__Cy__Flu__Carbo","Treo__FluMel__Bortezomib","Treo__Flu__Thio__Ritux__IL-6__Daratumumab",
               "Treo__Cy__Mel__Thio__Ritux","Treo__Cy__Mel__CD3",
               "Treo__Mel__Arac__Clofa","Treo__Mel__Clofa","Treo__Mel__Clofa__Ritux","Treo__FluMel__Thio__Ritux","Treo__FluMel__Arac",
               "Treo__Mel__Thio__Ritux","Treo__FluMel__Thio__Ritux","Treo__Mel__Thio__Ritux",
               "Treo__Cy__Flu__Arac","Treo__Flu__Arac__Amsa__IVIG", "Treo__Cy__Flu__Eto", "Treo__Flu__Thio", "Treo__Flu__Mel",
               "Treo__Cy__Arac__CCNU","Treo__Cy__Arac__Clofa","Treo__Cy__Flu__Thio","Treo__Thio__BEAM",
               "Treo__Flu__Arac__Amsa","Treo__Ritux",
               "Treo__FluMel__Temsirolimus","Treo__Fluorouracil","Treo__Flu__Midostaurin",
               "Treo__Flu__Adriamycine__Vincristine","Treo__Cy__Flu__Arac__Idarubicine","Treo__Cy__Flamsa__Taxo",
               "Treo__Clofa__Ritux__Adriamycine")
  
  baz$cond5[which(baz$cond5 %in% treobased)]<-"Treo based (-Bu)"

  butreobased<-c("BuTreo__Cy")
  
  baz$cond5[which(baz$cond5 %in% butreobased)]<-"Treo+Bu based"

  bucyflubased<-c("BuCy__Flu", 
                  "BuCy__Flu__Mitoxantrone",
                  "BuCy__Flu__Eto",
                  "BuCy__Flu__Amsa",
                  "BuCy__Flu__Amsa__BCNU",
                  "BuCy__Flu__Arac",
                  "BuCy__Flu__Arac__CCNU",
                  "BuCy__Flu__Ritux",
                  "BuCy__Flu__Daunorubicin",
                  "BuCy__Flu__CD19",
                  "BuCy__Flu__Arac__Bleomycine",
                  "BuCy__Flu__Eto__Arac",
                  "BuCy__Flu__Azathioprine",
                  "BuCy__Flu__IntrathecalChemo",
                  "BuCy__Flu__Eto__Arac__Daunorubicin",
                  "BuCy__Flu__Arac__Mitoxantrone")
  
  baz$cond5[which(baz$cond5 %in% bucyflubased)]<-"BuCyFlu based (-Mel/Thio)"

  bucyfluthiobased<-c("BuCy__Flu__Thio",
                      "BuCy__Flu__Thio__Cisplatin",
                      "BuCy__Flu__Eto__Thio",
                      "BuCy__Flu__Eto__Thio__Arac",
                      "BuCy__Flu__Thio__Vinblastine",
                      "BuCy__Flu__Thio__BCNU",
                      "BuCy__Flu__Thio__Ritux",
                      "BuCy__Flu__Thio__Azathioprine")
  
  baz$cond5[which(baz$cond5 %in% bucyfluthiobased)]<-"BuCyFluThio based (-Mel)"

  bucythiobased<-c("BuCy__Thio__Carbo","BuCy__Thio__Ritux")
  
  baz$cond5[which(baz$cond5 %in% bucythiobased)]<-"BuCyFluThio based (-Mel)"

  bufluthiobased<-c("BuFlu__Thio",
                    "BuFlu__Thio__CD3",
                    "BuFlu__Thio__CD8",
                    "BuFlu__Thio__Daunorubicin",
                    "BuFlu__Eto__Thio",
                    "BuFlu__Thio__Arac",
                    "BuFlu__Thio__Ritux",
                    "BuFlu__Thio__Clofa",
                    "BuFlu__Thio__Adriamycine",
                    "BuFlu__Thio__Ritux__Gemcitabine",
                    "BuFlu__Thio__Ritux__IVIG",
                    "BuFlu__Thio__IVIG",
                    "BuFlu__Thio__Idarubicine",
                    "BuFlu__Thio__IntrathecalChemo",
                    "BuFlu__Thio__Mercaptapur6",
                    "BuFlu__Thio__CEP")
  
  baz$cond5[which(baz$cond5 %in% bufluthiobased)]<-"BuFluThio based (-Cy/Mel)"

  buflumelthiobased<-c("BuFluMel__Thio")
  
  baz$cond5[which(baz$cond5 %in% buflumelthiobased)]<-"BuFluMelThio based (-Cy)"
  
  bucymelbased<-c("BuCy__Mel",
                  "BuCy__Mel__IVIG",
                  "BuCy__Mel__Ritux",
                  "BuCy__Mel__Arac","BuCy__Mel__Revlimid",
                  "BuCy__Mel__Thioguanine",
                  "BuCy__Mel__Ruxolitinib",
                  "BuCy__Mel__Ritux",
                  "BuCy__Mel__Eto","BuCy__Mel__Eto__Arac",
                  "BuCy__Mel__Eto__Daunorubicin__Adriamycine",
                  "BuCy__Mel__Mitoxantrone","BuCy__Mel__Arac__Amsa")
  
  baz$cond5[which(baz$cond5 %in% bucymelbased)]<-"BuCyMel based (-Flu/Thio)"

  bucyflumelbased<-c("BuCy__FluMel","BuCy__FluMel__CD3")
  
  baz$cond5[which(baz$cond5 %in% bucyflumelbased)]<-"BuCyFluMel based (-Thio)"

  bucybased<-c("BuCy",
               "BuCy__Azathioprine",
               "BuCy__Bleomycine",
               "BuCy__Clofa",
               "BuCy__Carbo",
               "BuCy__Eto",
               "BuCy__Eto__Ritux",
               "BuCy__Eto__ITT AMH(intrathecal)",
               "BuCy__Eto__IVIG",
               "BuCy__Eto__Arac__Daunorubicin",
               "BuCy__Eto__Azathioprine",
               "BuCy__Eto__Arac",
               "BuCy__Daunorubicin",
               "BuCy__Thio",
               "BuCy__Thio__Azathioprine",
               "BuCy__Taxo",
               "BuCy__BCNU",
               "BuCy__Amsa__CCNU",
               "BuCy__Amsa",
               "BuCy__Arac__Amsa",
               "BuCy__Arac__Amsa__CCNU",
               "BuCy__Arac__CCNU",
               "BuCy__Arac__CCNU__Ritux",
               "BuCy__Arac__Amsa__CCNU__Taxo",
               "BuCy__Arac__Thio",
               "BuCy__Arac__CCNU__Imatinib",
               "BuCy__Arac__Chlorambucil",
               "BuCy__Arac__Clofa",
               "BuCy__Arac__CCNU__Adriamycine",
               "BuCy__ITT AMH(intrathecal)",
               "BuCy__Idarubicine",
               "BuCy__Arac",
               "BuCy__BEAM",
               "BuCy__Arac__CCNU__Chlorambucil",
               "BuCy__Vincristine",
               "BuCy__Mercaptapur6",
               "BuCy__IL-11",
               "BuCy__CD3",
               "BuCy__CCNU",
               "BuCy__Clofa__Ritux",
               "BuCy__IntrathecalChemo",
               "BuCy__IVIG",
               "BuCy__Ritux",
               "BuCy__Adriamycine")
  
  baz$cond5[which(baz$cond5 %in% bucybased)]<-"BuCy based (-Flu/Mel)"

  buflubased<-c("BuFlu",
                "BuFlu__Amsa",
                "BuFlu__CD3",
                "BuFlu__CCNU", 
                "BuFlu__Azathioprine",
                "BuFlu__Chlorambucil",
                "BuFlu__Clofa","BuFlu__Clofa__Ritux",
                "BuFlu__Arac","BuFlu__Arac__Idarubicine","BuFlu__Arac__Daunorubicin",
                "BuFlu__Arac__Clofa",
                "BuFlu__Arac__CCNU",
                "BuFlu__Arac__BCNU__CCNU",
                "BuFlu__Adriamycine",
                "BuFlu__Amsa__Adriamycine",
                "BuFlu__Eto",
                "BuFlu__Bleomycine",
                "BuFlu__Daunorubicin",
                "BuFlu__Eto__Arac",
                "BuFlu__Vedolizumab/ Entyvio",
                "BuFlu__ITT AMH(intrathecal)",
                "BuFlu__IntrathecalChemo__ITT AMH(intrathecal)",
                "BuFlu__Gancyclovir",
                "BuFlu__Arac__Mitoxantrone",
                "BuFlu__Taxo",
                "BuFlu__Carbo",
                "BuFlu__Cisplatin",
                "BuFlu__ABVD",
                "BuFlu__IntrathecalChemo",
                "BuFlu__Dactinomycin",
                "BuFlu__IVIG",
                "BuFlu__Mercaptapur6",
                "BuFlu__Ritux","BuFlu__Ritux__Ruxolitinib")
  
  baz$cond5[which(baz$cond5 %in% buflubased)]<-"BuFlu based (-Cy/Mel/Thio)"

  buflumelbased<-c("BuFluMel",
                "Bu__Flu__Mel__Eto__Carbo",
                "BuFluMel__Eto__Carbo",
                "BuFluMel__IVIG",
                "BuFluMel__Ritux",
                "BuFluMel__CD3",
                "BuFlu__Mel__Bleomycine",
                "BuFluMel__Arac__Mitoxantrone")
  
  baz$cond5[which(baz$cond5 %in% buflumelbased)]<-"BuFluMel based (-Cy/Thio)"

  buflumelthiobased<-c("BuFluMel__Eto__Thio")
  
  baz$cond5[which(baz$cond5 %in% buflumelthiobased)]<-"BuFluMelThio based (-Cy)"

  bucymelthiobased<-c("BuCy__Mel__Eto__Thio","BuCy__Mel__Thio")
  
  baz$cond5[which(baz$cond5 %in% bucymelthiobased)]<-"BuCyMelThio based (-Flu)"

  flamsabased<-c("Bu__Flamsa", "Bu__Adriamycine__Flamsa","Bu__Flamsa__Thio",
                 "Cy__Mel__Flamsa", "Cy__Mel__Flamsa__CCNU", 
                 "Cy__Flamsa","Bu__Flamsa__IVIG","Mel__Flamsa","BuMel__Flamsa",
                 "Cy__Flamsa__Bleomycine","Cy__Flamsa__Adriamycine","Cy__Amsa__Flamsa")
  
  baz$cond5[which(baz$cond5 %in% flamsabased)]<-"Flasma based"

  BuCyflamsabased<-c("BuCy__Flamsa",
                 "BuCy__Flamsa__Thio","BuCy__Flamsa__IVIG","BuCy__Flasma__Eto__Idarubincine")
  
  baz$cond5[which(baz$cond5 %in% BuCyflamsabased)]<-"BuCy-Flasma based"
  
  Treoflamsabased<-c("Flamsa__Treo","Treo__Cy__Flamsa")
  
  baz$cond5[which(baz$cond5 %in% Treoflamsabased)]<-"Treo-Flasma based"
  
  ### CBV Cyclo BCNU VP16 (Eto)
  CBVbased<-c("CBV","CBV__Mitoxantrone","Cy__Eto__BCNU__Mitoxantrone" )
  
  baz$cond5[which(baz$cond5 %in% CBVbased)]<-"CBV based (Cy+BCNU+Eto)"

  bumelbased<-c("BuMel",
                "BuMel__Adriamycine",
                "BuMel__Anthracycline",
                "BuMel__Arac","Bu__Mel__Arac__Eto",
                "BuMel__Thio","BuMel__Thio__Ritux",
                "BuMel__CCNU__Taxo",
                "BuMel__IVIG",
                "BuMel__Adriamycine__IVIG",
                "BuMel__Arac__Eto","BuMel__CCNU",
                "BuMel__Eto","Bu__Mel__Eto__Arac")
  
  baz$cond5[which(baz$cond5 %in% bumelbased)]<-"BuMel based (-Cy/Flu)"

  flumelbased<-c("FluMel",
                 "FluMel__CD33",
                 "FluMel__Arac",
                 "FluMel__Arac__Idarubicine",
                 "FluMel__BCNU",
                 "FluMel__Ritux",
                 "FluMel__Vedolizumab/ Entyvio",
                 "FluMel__Thio__Eto",
                 "FluMel__Clofa",
                 "FluMel__BCNU__BCNU",
                 "FluMel__BCNU__CCNU",
                 "FluMel__Arac__BCNU",
                 "FluMel__Amsa",
                 "FluMel__Thio",
                 "FluMel__Thio__Ritux",
                 "FluMel__Thio__Cisplatin",
                 "FluMel__Thio__CD3",
                 "Flu__EtoMel" ,
                 "FluMel__BCNU__Ritux",
                 "FluMel__Thio__Ritux",
                 "FluMel__Eto__Arac",
                 "FluMel__Thio__ICE",
                 "FluMel__Thio__VBMCP/VBAD",
                 "FluMel__BCNU__IVIG" )
  
  baz$cond5[which(baz$cond5 %in% flumelbased)]<-"FluMel based (-Bu/Treo/Cy)"
  
  cyflubased<-c("Cy__Flu__Thio",
                 "Cy__Flu__Adriamycine__Vincristine",
                 "Cy__Flu","Cy__Flu__Ritux",
                "Cy__Flu__CD3",
                 "Cy__Flu__Eto","Cy__Flu__Eto__Mitoxantrone",
                "Cy__Flu__Amsa","Cy__Flu__Amsa__Arac",
                 "Cy__Flu__Arac__Idarubicine",
                "Cy__Flu__Arac__Mitoxantrone",
                "Cy__Flu__Arac__BCNU",
                "Cy__Flu__Eto__Arac__Carbo__Cisplatin__Adriamycine",
                 "Cy__Flu__Daunorubicin",
                "Cy__Flu__Eto__Arac",
                "Cy__Flu__Arac__BCNU",
                 "Cy__Flu__Arac",
                "Cy__Flu__Eto__Thio",
                "Cy__Flu__Idarubicine",
                "Cy__Flu__Thio__Bortezomib",
                 "Cy__Flu__Clofa")
  
  baz$cond5[which(baz$cond5 %in% cyflubased)]<-"CyFlu based (-Bu/Treo/Mel)"
  
  cyflumelbased<-c("Cy__FluMel",
                 "Cy__FluMel__Thio",
                 "Cy__FluMel__Clofa",
                 "Cy__FluMel__Arac__Clofa","Cy__Flu__Mel__Arac__Clofa",
                 "Flu__Cy__Mel__Eto__Thio__Clofa","Cy__FluMel__BCNU")
  
  baz$cond5[which(baz$cond5 %in% cyflumelbased)]<-"CyFluMel based (-Bu/Treo)"
  
  baz$cond5[which(baz$cond5 %in% c("Cy","Cy__Mitoxantrone","Cy__BCNU",
                                   "Cy__Bleomycine","Cy__Adriamycine","Cy__Arac__CCNU",
                                   "Cy__FluMel__Flu__Adriamycineorouracil",
                                   "Cy__Amsa",
                                   "Cy__Amsa__Arac",
                                   "Cy__Arac__Vincristine",
                                   "Cy__Clofa",
                                   "Cy__Carbo","Cy__Arac",
                                   "Cy__Vincristine",
                                   "Cy__Carbo__Vincristine",
                                   "Cy__IntrathecalChemo",
                                   "Cy__ITT AMH(intrathecal)",
                                   "Cy__Mel",
                                   "Cy__Mel__Thio",
                                   "Cy__Mel__Bleomycine","Cy__Idarubicine",
                                   "Cy__Eto","Cy__Eto__sIG","Cy__IVIG","Cy__Daunorubicin",
                                   "Cy__Eto__Thio","Cy__Eto__Arac__CCNU","Cy__Eto__Carbo",
                                   "Cy__Thio","Cy__Thio__Ritux",
                                   "Cy__Arac__Daunorubicin__Vincristine",
                                   "Cy__Arac__Daunorubicin__Teniposide__Vincristine",
                                   "Cy__Arac__Daunorubicin__Teniposide__Vinblastine__Vincristine"))]<-"Cy based other"


  othercomb<-c("Arac",
               "Arac__Clofa",
               "Amsa",
               "BCNU",
               "Benda",
               "Bu",
               "Bu__Eto",
               "Bu__Eto__Thio",
               "Bu__Eto__Arac",
               "Bu__Eto__Arac__Amsa",
               "Bu__Eto__IVIG",
               "Bu__Thio",
               "Bu__Thio__IVIG",
               "Bu__Thio__Adriamycine__Vincristine",
               "Bu__CCNU",
               "Bu__Clofa","Bu__Clofa__Adriamycine",
               "Bu__Arac",
               "Bu__Arac__Amsa",
               "Bu__Arac__CCNU",
               "Bu__Arac__Azathioprine",
               "Bu__ACVB",
               "Bu__Fluorouracil","Bu__Carbo","Bu__Oxaliplatin","Bu__Arac__Clofa","Bu__Adriamycine",
               "ABVD__Bu",
               "Carbo",
               "CCNU",
               "Clofa","Cladribine",
               "Daunorubicin",
               "Eto",
               "Eto__Thio","Eto__Thio__Carbo",
               "Eto__Arac__BCNU__Ifosfamide",
               "Eto__Arac",
               "Eto__Carbo",
               "Eto__Daunorubicin",
               "Eto__Ritux","Eto__Ritux__Bexxar",
               "Eto__sIG",
               "Eto__IVIG",
               "Eto__Adriamycine",
               "Epirubicine",
               "Flu",
               "Flu__Adriamycine",
               "Flu__BCNU",
               "Flu__CCNU",
               "Flu__Benda__Ritux",
               "Flu__Eto","Flu__Eto__Arac","Flu__Eto__BCNU",
               "Flu__Eto__Ritux",
               "Flu__Thio",
               "Flu__Thio__Arac",
               "Flu__Thio__Arac__BCNU","Flu__Thio__BCNU","Flu__Thio__Bleomycine",
               "Flu__Thio__Ritux",
               "Flu__Ritux",
               "Flu__Mitoxantrone",
               "Flu__Carbo",
               "Flu__ChlVPP/ PABlOE",
               "Flu__Epirubicine",
               
               "Flu__Thio__Ritux__Bortezomib",
               "Flu__Thio__Ritux__Bleomycine","Flu__Thio__Ritux__Bortezomib__IL-6 __Venetoclax",
               "Flu__Thio__Ritux__Bortezomib__IL-6","Flu__Thio__Ritux__Carbo__Mitoxantrone__Taxo",
               "Eto__Arac__ITT AMH(intrathecal)",
               "Flu__Thio__IVIG",
               "Flu__Arac",
               "Flu__Arac__Idarubicine",
               "Flu__Arac__Daunorubicin",
               "Flu__Amsa",
               "Flu__Benda",
               "Flu__Bleomycine",
               "Flu__Chlorambucil",
               "Flu__Ritux",
               "Fluorouracil",
               "Ifosfamide",
               "Ifosfamide__Vincristine",
               "IVIG",
               "Mel",
               "Mel__Eto","Mel__Eto__Arac",
               "Mel__Eto__BCNU",
               "Mel__Eto__Clofa",
               "Mel__Eto__Arac__CCNU","Mel__Eto__Carbo",
               "Mel__Thio","Mel__Thio__Clofa","Mel__Thio__Ritux",
               "Mel__Arac","Mel__Arac__BCNU",
               "Mel__Arac__Clofa",
               "Mel__BCNU",
               "Mel__Clofa",
               "Mel__Mitoxantrone",
               "Mel__Carbo",
               "Mel__Cladribine",
               "Ruxolitinib",
               "Thio",
               "Thio__BCNU",
               "Thio__R-GIfOx",
               "Thio__Carbo",
               "Arac__Daunorubicin",
               "Arac__Mitoxantrone",
               "Other")
  
  baz$cond5[which(baz$cond5 %in% othercomb)]<-"Other combinations"

baz$cond5dose<-baz$cond5
  
baz$cond5<-ifelse(tempTBI=="Yes",paste("TBI",baz$cond5,sep="+"),baz$cond5)

baz$cond5dose[which(baz$VRADICON=="Yes" & baz$DOSETBINBIN=="TBI(<=6)")]<-paste("TBI(<=6)",baz$cond5dose[which(baz$VRADICON=="Yes" & baz$DOSETBINBIN=="TBI(<=6)")],sep="+")
baz$cond5dose[which(baz$VRADICON=="Yes" & baz$DOSETBINBIN=="TBI(>6)")]<-paste("TBI(>6)",baz$cond5dose[which(baz$VRADICON=="Yes" & baz$DOSETBINBIN=="TBI(>6)")],sep="+")
baz$cond5dose[which(baz$VRADICON=="Yes" & baz$DOSETBINBIN=="TBI(NA)")]<-paste("TBI(NA)",baz$cond5dose[which(baz$VRADICON=="Yes" & baz$DOSETBINBIN=="TBI(NA)")],sep="+")

#baz$cond5dose<-jolilabel(baz,"cond5dose")
  
  
  
baz$cond6<-baz$cond5
baz$cond6[which(baz$cond5=="BuCy based (-Flu/Mel)")]<-"BuCy+/-Flu based (-Mel)"
baz$cond6[which(baz$cond5=="BuCyFlu based (-Mel)")]<-"BuCy+/-Flu based (-Mel)"
baz$cond6[which(baz$cond4=="BuCy+Flamsa")]<-"BuCy+/-Flu based (-Mel)"

baz$cond6[which(baz$cond5=="BuCyMel based (-Flu/Thio)")]<-"BuCyMel+/-Flu based (-Thio)"
baz$cond6[which(baz$cond5=="BuCyFluMel based (-Thio)")]<-"BuCyMel+/-Flu based (-Thio)"

baz$cond6[which(baz$cond5=="BuFluThio based (-Cy/Mel)")]<-"BuFluThio+/-Cy based (-Mel)"
baz$cond6[which(baz$cond5=="BuCyFluThio based (-Mel)")]<-"BuFluThio+/-Cy based (-Mel)"

baz$cond6[which(baz$cond5=="BuFlu based (-Cy/Mel/Thio)")]<-"Other combinations"
baz$cond6[which(baz$cond5=="BuFluMel based (-Cy/Thio)")]<-"Other combinations"
baz$cond6[which(baz$cond5=="BuMel based (-Cy/Flu)")]<-"Other combinations"

baz$cond6dose<-baz$cond5dose
baz$cond6dose[which(baz$cond5dose=="BuCy__Mel")]<-"BuCy based"
baz$cond6dose[which(baz$cond5dose=="FluMel and FluCy based")]<-"Other combinations"
baz$cond6dose[which(baz$cond5dose=="BuCyFlu based")]<-"BuCyFlu+/-Thio based"
baz$cond6dose[which(baz$cond5dose=="BuCyFluThio based")]<-"BuCyFlu+/-Thio based"
baz$cond6dose[which(baz$cond5dose=="BuFlu based")]<-"BuFlu+/-Thio based"
baz$cond6dose[which(baz$cond5dose=="ThioBuFlu based")]<-"BuFlu+/-Thio based"


################## Si VRADICON manquant et drug rentrées alors VRADICON comme NON
baz$VRADICON[which(is.na(baz$VRADICON) & !is.na(baz$cond4))]<-"No"

  
##########################################################################################
############################### GVHD prev ################################################
##########################################################################################
  

  doublelabelprev<-function(baz,var){
    baz[,var]<-gsub("ATG__ATG","ATG",baz[,var])
    baz[,var]<-gsub("STER__STER","STER",baz[,var])
    baz[,var]<-gsub("CD3__CD3","CD3",baz[,var])
    baz[,var]<-gsub("CSA__CSA","CSA",baz[,var])
    baz[,var]<-gsub("MTX__MTX","MTX",baz[,var])
    baz[,var]<-gsub("MMF__MMF","MMF",baz[,var])
    baz[,var]<-gsub("TACRO__TACRO","TACRO",baz[,var])
    baz[,var]<-gsub("SIRO__SIRO","SIRO",baz[,var])
    baz[,var]<-gsub("CNI__CNI","CNI",baz[,var])
    
  } 
  
# 1-CSA 2-MTX 3-MMF 4-SIRO 5-TACRO 6-DACLI 7-EVERO 8-Azathioprine 9-Etanercept 10-Infliximab 11-CD3 12-CD19

  jolilabelprev<-function(baz,var){
    
    ### 5 drugs
    baz[,var]<-gsub("CSA__DACLI__MMF__MTX__TACRO","CSA__MTX__MMF__TACRO__DACLI",baz[,var])
    baz[,var]<-gsub("Etanercept__MMF__MTX__SIRO__TACRO","MTX__MMF__SIRO__TACRO__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__MTX__Everolimus__MMF__TACRO","CSA__MTX__MMF__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("Dexamethasone__MTX__CSA__MMF__TACRO","CSA__MTX__MMF__TACRO__Dexamethasone",baz[,var])
    baz[,var]<-gsub("CSA__MTX__Everolimus__MMF__TACRO","CSA__MTX__MMF__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("CSA__Everolimus__MMF__MTX__TACRO","CSA__MTX__MMF__TACRO__Everolimus",baz[,var])
    
    ### 4 drugs
    baz[,var]<-gsub("CSA__Etanercept__MTX__TACRO","CSA__MTX__TACRO__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__Etanercept__MTX__MMF","CSA__MTX__MMF__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__Etanercept__MMF__MTX","CSA__MTX__MMF__Etanercept",baz[,var])
    baz[,var]<-gsub("Dexamethasone__CSA__MTX__MMF","CSA__MTX__MMF__Dexamethasone",baz[,var])
    baz[,var]<-gsub("Dexamethasone__MTX__CSA__MMF","CSA__MTX__MMF__Dexamethasone",baz[,var])
    baz[,var]<-gsub("CSA__MMF__CSA__MMF","CSA__MMF",baz[,var])
    baz[,var]<-gsub("CSA__Etanercept__MMF__SIRO","CSA__MMF__SIRO__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__Dexamethasone__MTX__TACRO","CSA__MTX__TACRO__Dexamethasone",baz[,var])
    
    baz[,var]<-gsub("CSA__DACLI__MMF__MTX","CSA__MTX__MMF__DACLI",baz[,var])
    baz[,var]<-gsub("Azathioprine__CSA__MMF__MTX","CSA__MTX__MMF__Azathioprine",baz[,var])
    baz[,var]<-gsub("CSA__MMF__MTX__TACRO","CSA__MTX__MMF__TACRO",baz[,var])
    baz[,var]<-gsub("CSA__Etanercept__MTX__MMF","CSA__MTX__MTX__MMF__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__DACLI__SIRO__TACRO","CSA__SIRO__TACRO__DACLI",baz[,var])
    baz[,var]<-gsub("CSA__MMF__MonoclonalAB__TACRO","CSA__MMF__TACRO__MonoclonalAB",baz[,var])
    baz[,var]<-gsub("Etanercept__MMF__MTX__TACRO","MTX__MMF__TACRO__Etanercept",baz[,var])
    baz[,var]<-gsub("Etanercept__MTX__SIRO__TACRO","MTX__SIRO__TACRO__Etanercept",baz[,var])
    baz[,var]<-gsub("MTX__MMF__TACRO__TACRO","MTX__MMF__TACRO",baz[,var])
    baz[,var]<-gsub("MTX__TACRO__MMF__TACRO","MTX__MMF__TACRO",baz[,var])
    baz[,var]<-gsub("MTX__MTX__MMF__TACRO","MTX__MMF__TACRO",baz[,var])
    baz[,var]<-gsub("TACRO__CSA__MTX__TACRO","CSA__MTX__TACRO",baz[,var])
    baz[,var]<-gsub("MTX__MTX__MMF__Everolimus","MTX__MMF__Everolimus",baz[,var])
    baz[,var]<-gsub("MTX__MMF__Dexamethasone__TACRO","MTX__MMF__TACRO__Dexamethasone",baz[,var])
    baz[,var]<-gsub("MTX__Everolimus__MMF__MTX","MTX__MMF__Everolimus",baz[,var])
    baz[,var]<-gsub("CSA__MMF__DACLI__TACRO","CSA__MMF__TACRO__DACLI",baz[,var])
    
    
    ### 3 drugs
    baz[,var]<-gsub("CSA__DACLI__MTX","CSA__MTX__DACLI",baz[,var])
    baz[,var]<-gsub("Azathioprine__CSA__MTX","CSA__MTX__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__CSA__MTX","CSA__MTX__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__CSA__TACRO","CSA__TACRO__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__CSA__MMF","CSA__MMF__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__MTX__TACRO","MTX__TACRO__Azathioprine",baz[,var])
    baz[,var]<-gsub("CSA__Etanercept__MTX","CSA__MTX__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__Etanercept__MMF","CSA__MMF__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__Dexamethasone__MTX","CSA__MTX__Dexamethasone",baz[,var])
    baz[,var]<-gsub("CSA__Fluorouracil__MMF","CSA__MMF__Fluorouracil",baz[,var])
    
    baz[,var]<-gsub("CSA__Etanercept__SIRO","CSA__SIRO__Etanercept",baz[,var])
    baz[,var]<-gsub("CSA__Infliximab__MMF","CSA__MMF__Infliximab",baz[,var])
    baz[,var]<-gsub("Etanercept__MTX__TACRO","MTX__TACRO__Etanercept",baz[,var])
    baz[,var]<-gsub("MMF__Ruxolitinib__TACRO","MMF__TACRO__Ruxolitinib",baz[,var])
    baz[,var]<-gsub("TOPOT__MTX__MMF","MTX__MMF__TOPOT",baz[,var])
    baz[,var]<-gsub("TOPOT__MMF__MTX","MTX__MMF__TOPOT",baz[,var])
    baz[,var]<-gsub("CSA__CD3__MTX","CSA__MTX__CD3",baz[,var])
    baz[,var]<-gsub("CSA__CD34__MTX","CSA__MTX__CD34",baz[,var])
    baz[,var]<-gsub("Etanercept__MMF__TACRO","MMF__TACRO__Etanercept",baz[,var])
    baz[,var]<-gsub("TACRO__MTX__TACRO","MTX__TACRO",baz[,var])
    baz[,var]<-gsub("CSA__Everolimus__TACRO","CSA__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("CSA__FLAC__MTX","CSA__MTX__FLAC",baz[,var])
    baz[,var]<-gsub("Dexamethasone__MTX__MMF","MTX__MMF__Dexamethasone",baz[,var])
    baz[,var]<-gsub("MTX__MTX__TACRO","MTX__TACRO",baz[,var])
    baz[,var]<-gsub("MMF__Everolimus__TACRO","MMF__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("Oxaliplatin__CSA__MMF","CSA__MMF__Oxaliplatin",baz[,var])
    
    baz[,var]<-gsub("MMF__MTX__TACRO","MTX__MMF__TACRO",baz[,var])
    baz[,var]<-gsub("CSA__MMF__MTX","CSA__MTX__MMF",baz[,var])
    baz[,var]<-gsub("CSA__MMF__SIRO","CSA__MMF__SIRO",baz[,var])
    baz[,var]<-gsub("TOPOT__CSA__MTX","CSA__MTX__TOPOT",baz[,var])
    baz[,var]<-gsub("CSA__DACLI__MMF","CSA__MMF__DACLI",baz[,var])
    baz[,var]<-gsub("DACLI__MMF__TACRO","MMF__TACRO__DACLI",baz[,var])
    baz[,var]<-gsub("MTX__MTX__MMF","MTX__MMF",baz[,var])
    baz[,var]<-gsub("MTX__MMF__MTX","MTX__MMF",baz[,var])
    baz[,var]<-gsub("MMF__Everolimus__MTX","MTX__MMF__Everolimus",baz[,var])
    baz[,var]<-gsub("CSA__Dactinomycin__MMF","CSA__MMF__Dactinomycin",baz[,var])
    baz[,var]<-gsub("CSA__CD19__MMF","CSA__MMF__CD19",baz[,var])
    baz[,var]<-gsub("CD66__CSA__MTX","CSA__MTX__CD66",baz[,var])
    baz[,var]<-gsub("CD66__MTX__TACRO","MTX__TACRO__CD66",baz[,var])
    baz[,var]<-gsub("Dexamethasone__MTX__TACRO","MTX__TACRO__Dexamethasone",baz[,var])
    baz[,var]<-gsub("MMF__Everolimus__TACRO","MMF__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("CSA__Fluorouracil__MMF","CSA__MMF__Fluorouracil",baz[,var])
    baz[,var]<-gsub("Everolimus__MMF__TACRO","MMF__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("CSA__CD19__MMF","CSA__MMF__CD19",baz[,var])
    baz[,var]<-gsub("Everolimus__SIRO__TACRO","SIRO__TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("CAMPguanine__CSA__MTX","Campath__CSA__MTX",baz[,var])
    
    ### 2 drugs
    baz[,var]<-gsub("Everolimus__TACRO","TACRO__Everolimus",baz[,var])
    baz[,var]<-gsub("MTX__CSA__MTX","CSA__MTX",baz[,var])
    baz[,var]<-gsub("MTX__CSA","CSA__MTX",baz[,var])
    baz[,var]<-gsub("CSA__MTX__CSA__MTX","CSA__MTX",baz[,var])
    baz[,var]<-gsub("CSA__MTX__CSA","CSA__MTX",baz[,var])
    baz[,var]<-gsub("MTX__CSA","CSA__MTX",baz[,var])
    baz[,var]<-gsub("CSA__CSA__MTX","CSA__MTX",baz[,var])
    baz[,var]<-gsub("Azathioprine__TACRO","TACRO__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__CSA","CSA__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__MMF","MMF__Azathioprine",baz[,var])
    baz[,var]<-gsub("Azathioprine__MTX","MTX__Azathioprine",baz[,var])
    baz[,var]<-gsub("Dexamethasone__MTX","MTX__Dexamethasone",baz[,var])
    
    baz[,var]<-gsub("ATG onlyorouracil__CSA__MMF","CSA__MMF",baz[,var])
    baz[,var]<-gsub("ATG onlyorouracil__MMF__TACRO","MMF__TACRO",baz[,var])
    baz[,var]<-gsub("CD19__CSA","CSA__CD19",baz[,var])
    baz[,var]<-gsub("Everolimus__MMF","MMF__Everolimus",baz[,var])
    baz[,var]<-gsub("Everolimus__MTX","MTX__Everolimus",baz[,var])
    baz[,var]<-gsub("CD34__CSA","CSA__CD34",baz[,var])
    baz[,var]<-gsub("CD41__CSA","CSA__CD41",baz[,var])
    baz[,var]<-gsub("MMF__CSA","CSA__MMF",baz[,var])
    
    
# 1-CSA 2-MTX 3-MMF 4-SIRO 5-TACRO 6-DACLI 7-EVERO 8-Azathioprine 9-Etanercept 10-Infliximab 11-CD3 12-CD19 13- Dactinomycin
    
    baz[,var]<-gsub("CSA__Dactinomycin__MMF","CSA__MMF__Dactinomycin",baz[,var])
    
    
    baz[,var]<-gsub("MMF__MTX","MTX__MMF",baz[,var])
    
    baz[,var]<-gsub("TACRO__MMF","MMF__TACRO",baz[,var])
    baz[,var]<-gsub("TACRO__MMF","MMF__TACRO",baz[,var])
    
    baz[,var]<-gsub("CD3__CSA","CSA__CD3",baz[,var])
    baz[,var]<-gsub("CD3__MMF","MMF__CD3",baz[,var])
    
    baz[,var]<-gsub("Infliximab__TACRO","TACRO__Infliximab",baz[,var])
    
  }
  
  
  jolilabelprevcni<-function(baz,var){
    
    ##4 drugs
    baz[,var]<-gsub("CNI__DACLI__MMF__MTX__CNI","CNI__MMF__MTX__DACLI",baz[,var])
    
    ## 3 drugs
    baz[,var]<-gsub("CNI__DACLI__MMF__CNI","CNI__MMF__DACLI",baz[,var])
    baz[,var]<-gsub("CNI__MMF__MTX__CNI","CNI__MMF__MTX",baz[,var])
    baz[,var]<-gsub("MMF__MTX__CNI","CNI__MMF__MTX",baz[,var])
    
    ## 2 drugs
    baz[,var]<-gsub("CNI__MMF__CNI","CNI__MMF",baz[,var])
    baz[,var]<-gsub("CNI__MTX__CNI","CNI__MTX",baz[,var])
    baz[,var]<-gsub("CNI__DACLI__CNI","CNI__DACLI",baz[,var])
    baz[,var]<-gsub("MMF__CNI","CNI__MMF",baz[,var])
    baz[,var]<-gsub("MTX__CNI","CNI__MTX",baz[,var])
    
    baz[,var]<-gsub("CNI__CNI__CNI","CNI",baz[,var])
    baz[,var]<-gsub("CNI__CNI","CNI",baz[,var])
    
    return(baz[,var])
  }    
  
    
      
###############################################################################################
###############################################################################################
####################### GVHD prevention in conditioning regimen ###############################
###############################################################################################
###############################################################################################

# baz$previncond2<-NA
# baz$previncond2<-ifelse(grepl("ATG",baz$cond2)==TRUE,"ATG",baz$previncond2)
#   for(i in sort(c("CSA","MTX","MMF","SIRO","TACRO","TOPOT","CD34","STER","CAMP","DACLI"))){
#     baz$previncond2<-ifelse(grepl(i,baz$cond2)==TRUE,
#                            ifelse(is.na(baz$previncond2),i,
#                                   paste(baz$previncond2,i,sep="+")),
#                            baz$previncond2)
#   }
# 
# baz$previncond2<-gsub("CAMP\\+CAMP","CAMP",baz$previncond2)

baz$previncond<-NA
for (i in 1:dim(baz)[1]){
  if(sum(is.na(baz$CONDDRUG1[i])+
         is.na(baz$CONDDRUG2[i])+
         is.na(baz$CONDDRUG3[i])+
         is.na(baz$CONDDRUG4[i])+
         is.na(baz$CONDDRUG5[i])+
         is.na(baz$CONDDRUG6[i])+
         is.na(baz$CONDDRUG7[i]))==7){
    baz$previncond[i]<-NA
  }else{
    baz$previncond[i]<-paste(sort(c( as.character(baz$CONDDRUG1[i]),
                                     as.character(baz$CONDDRUG2[i]),
                                     as.character(baz$CONDDRUG3[i]),
                                     as.character(baz$CONDDRUG4[i]),
                                     as.character(baz$CONDDRUG5[i]),
                                     as.character(baz$CONDDRUG6[i]),
                                     as.character(baz$CONDDRUG7[i]))),collapse='__') }
}

baz$previncond<-gsub("^\\s+|\\s+$","",baz$previncond)

baz$previncond<-gsub(paste0("Cyclo","__"),"",baz$previncond)
baz$previncond<-gsub(paste0("__","Cyclo"),"",baz$previncond)
baz$previncond<-gsub("Cyclo",NA,baz$previncond)

for( i in pasprev){
  baz$previncond<-gsub(paste0(i,"__"),"",baz$previncond)
  baz$previncond<-gsub(paste0("__",i),"",baz$previncond)
  baz$previncond<-gsub(i,NA,baz$previncond)
}

###############################################################################################
###############################################################################################
####################### GVHD prevention #######################################################
###############################################################################################
###############################################################################################
  
baz$prevtot<-NA
  for (i in 1:dim(baz)[1]){
    if(sum(is.na(baz$previncond[i])+
           is.na(baz$PREVDRUG1[i])+
           is.na(baz$PREVDRUG2[i])+
           is.na(baz$PREVDRUG3[i])+
           is.na(baz$PREVDRUG4[i])+
           is.na(baz$PREVDRUG5[i])+
           is.na(baz$PREVDRUG6[i])+
           is.na(baz$PREVDRUG7[i]))==8){
      baz$prevtot[i]<-NA
    }else{
      # baz$prevtot[i]<-paste(sort(c( unlist(str_split(as.character(baz$previncond[i]),"\\+")),
      baz$prevtot[i]<-paste(sort(c( as.character(baz$previncond[i]),
                                    as.character(baz$PREVDRUG1[i]),
                                    as.character(baz$PREVDRUG2[i]),
                                    as.character(baz$PREVDRUG3[i]),
                                    as.character(baz$PREVDRUG4[i]),
                                    as.character(baz$PREVDRUG5[i]),
                                    as.character(baz$PREVDRUG6[i]),
                                    as.character(baz$PREVDRUG7[i]))),collapse='__') }
  }
  
  baz$prevtot<-gsub("^\\s+|\\s+$", "",baz$prevtot)
  
  baz$prevtot<-gsub("Cyclo","PTCY",baz$prevtot)
  
#  baz$prevtot<-jolilabelprev(baz,"prevtot")
  
  baz$prevtot2<-gsub("STER__STER","STER",baz$prevtot)
  
  baz$prevtot2<-gsub("STER__","",baz$prevtot2)
  baz$prevtot2<-gsub("__STER","",baz$prevtot2)
  baz$prevtot2<-gsub("STER",NA,baz$prevtot2)
  
  baz$prevtot2<-gsub("ATG__","",baz$prevtot2)
  baz$prevtot2<-gsub("__ATG","",baz$prevtot2)
  baz$prevtot2<-gsub("ATG","ATG only",baz$prevtot2)
  
  baz$prevtot2<-gsub("CAMP__","",baz$prevtot2)
  baz$prevtot2<-gsub("__CAMP","",baz$prevtot2)
  baz$prevtot2<-gsub("CAMP","Campath only",baz$prevtot2)
  
  # baz$prevtot2<-jolilabelprev(baz,"prevtot2")
  
  baz$prevtot3<-baz$prevtot2
  baz$prevtot2<-gsub("__","+",baz$prevtot2)
  
  for( i in pasprev){
    baz$prevtot3<-gsub(paste0(i,"__"),"",baz$prevtot3)
    baz$prevtot3<-gsub(paste0("__",i),"",baz$prevtot3)
    baz$prevtot3<-gsub(i,NA,baz$prevtot3)
  }

  baz$prevtot3<-gsub("__Other","",baz$prevtot3)
  baz$prevtot3<-gsub("Other__","",baz$prevtot3)

  baz$PTCYJEG<-ifelse(grepl("PTCY",baz$prevtot3)==TRUE,"PTCY","")
  if ("PTCYEP" %in% names(baz)) {print(try(table(baz$PTCYEP,baz$PTCYJEG,useNA="ifany"),silent = TRUE))}
  
  baz$prevtot3<-gsub("__PTCY","",baz$prevtot3)
  baz$prevtot3<-gsub("PTCY__","",baz$prevtot3)
  
  baz$prevtot3<-doublelabelprev(baz,'prevtot3')
  baz$prevtot3<-jolilabelprev(baz,'prevtot3')
  
  baz$prevtotsansPTCy<-baz$prevtot3
  baz$prevtotsansPTCy<-gsub("PTCY","PTCY alone",baz$prevtotsansPTCy)
  baz$prevtotsansPTCy<-as.factor(gsub("__","+",baz$prevtotsansPTCy))
  
  baz$prevtot4<-baz$prevtot3
  baz$prevtot4[which(baz$PTCYJEG=="PTCY")]<-paste0("PTCY__",baz$prevtot4[which(baz$PTCYJEG=="PTCY")])
  baz$prevtot4<-gsub("PTCY__PTCY","PTCY",baz$prevtot4)
  baz$prevtot4<-as.factor(gsub("__","+",baz$prevtot4))
  
  baz$prevtot4group<-baz$prevtot3
  
  CSAbased<-c("CSA","CSA__CD34","CSA__Azathioprine","CSA__Etanercept","CSA__CD3")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSAbased)]<-"CSA based (-MTX/MMF/SIRO/TACRO/PTCY)"
  
  CSATACRObased<-c("CSA__TACRO","CSA__TACRO__Infliximab",
                   "CSA__SIRO__TACRO__DACLI","CSA__SIRO","CSA__CD34","CSA__CD341",
                   "CSA__SIRO__Etanercept","CSA__CD41")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSATACRObased)]<-"CSA+(SIRO/TACRO) based (-MTX/MMF/PTCY)"
  
  CSAMMFbased<-c("CSA__MMF","CSA__MMF__DACLI","CSA__MMF__CD19","CSA__MMF__Infliximab","CSA__SIRO__TACRO","CSA__TACRO__Everolimus")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSAMMFbased)]<-"CSA+MMF based (-MTX/SIRO/TACRO/PTCY)"
  
  CSAMMFTACRObased<-c("CSA__MMF__SIRO__TACRO","CSA__MMF__TACRO","CSA__MMF__Taxo","CSA__MMF__TACRO__MonoclonalAB","CSA__MMF__SIRO",
                      "CSA__MMF__Dactinomycin","CSA__MMF__TACRO__DACLI")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSAMMFTACRObased)]<-"CSA+MMF+(SIRO/TACRO) based (-MTX/PTCY)"
  
  CSAMTXbased<-c("CSA__MTX","CSA__MTX__Azathioprine","CSA__MTX__sIG","CSA__MTX__DACLI","CSA__MMF__Etanercept","CSA__MMF__Oxaliplatin",
                 "CSA__MTX__Etanercept","CSA__MMF__Fluorouracil","CSA__MTX__TOPOT","CSA__MTX__CD66","CSA__MTX__Everolimus")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSAMTXbased)]<-"CSA+MTX based (-MMF/SIRO/TACRO/PTCY)"
  
  CSAMTXTACRObased<-c("CSA__MTX__CD34","CSA__MTX__SIRO__TACRO","CSA__MTX__Taxo","CSA__MTX__Vedolizumab","CSA__TDT"," CSA__MTX__FLAC",
                      "CSA__MTX__TACRO","CSA__MTX__SIRO","CSA__MTX__DACLI","CSA__MTX__Etanercept")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSAMTXTACRObased)]<-"CSA+MTX+(SIRO/TACRO) based (-MMF/PTCY)"
  
  CSAMMFMTXbased<-c("CSA__MTX__MMF__Etanercept","CSA__MTX__MMF","CSA__MTX__MMF__SIRO","CSA__MTX__MMF__Everolimus",
                    "CSA__MTX__MMF__TACRO","CSA__MTX__MMF__TACRO__DACLI","CSA__MTX__TACRO__Taxo","CSA__MTX__MMF__Dexamethasone")
  
  baz$prevtot4group[which(baz$prevtot4group %in% CSAMMFMTXbased)]<-"CSA+MMF+MTX (SIRO/TACRO) based (-PTCY)"
  
  WithoutCSA<-c("MMF", "MMF__SIRO", "MMF__TACRO", "MMF__TACRO__DACLI","CSA__MTX__MMF__DACLI",
                "MMF__TACRO__Ruxolitinib","MTX__TACRO__CD66","MMF__TACRO__Vedolizumab", 
                "CSA__MTX__MMF__TACRO__Everolimus","MMF__TACRO__Taxo","MTX__MMF__Everolimus",
                "MTX", "MTX__MMF", "MTX__MMF__SIRO__TACRO__Etanercept", "MTX__MMF__TACRO", 
                "MTX__MMF__TACRO__Etanercept", "MTX__MMF__TOPOT", "MTX__SIRO__TACRO", 
                "MTX__SIRO__TACRO__Etanercept", "MTX__TACRO", "MTX__TACRO__Etanercept",
                "SIRO__TACRO","TACRO","MMF__SIRO__TACRO","MTX__MMF__Everolimus","SIRO__TACRO__Everolimus","TACRO__Everolimus",
                "MTX__MMF__TACRO__Dexamethasone","MMF__TACRO__Everolimus","MTX__MMF__Dexamethasone",
                "MTX__Everolimus","SIRO","Everolimus","TACRO__Azathioprine","MMF__CD3","MTX__SIRO","MMF__TACRO__Everolimus")
  
  baz$prevtot4group[which(baz$prevtot4group %in% WithoutCSA)]<-"Whitout CSA (-CSA/PTCY)"
  
  OTHER<-c("ATG only","Campath only","IL-6","CD19")
  
  baz$prevtot4group[which(baz$prevtot4group %in% OTHER)]<-"other (-CSA/MTX/MMF/SIRO/TACRO/PTCY)"
  
  baz$PREVTOTSANSPTCyGROUP<-baz$prevtot4group
  baz$PREVTOTSANSPTCyGROUP<-as.factor(baz$PREVTOTSANSPTCyGROUP)
  
  
  baz$prevtot4group[which(baz$PTCYJEG=="PTCY")]<-"PTCY based"
  
  baz$prevtot4group<-as.factor(baz$prevtot4group)
  
  baz$prevtot5<-baz$prevtot3
  
  baz$prevtot5<-gsub("CSA","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("__CSA","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("CSA__","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("SIRO","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("__SIRO","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("SIRO__","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("TACRO","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("__TACRO","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("TACRO__","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("Everolimus","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("__Everolimus","CNI",baz$prevtot5)
  baz$prevtot5<-gsub("Everolimus__","CNI",baz$prevtot5)
  
  baz$prevtot5<-doublelabelprev(baz,"prevtot5")
  baz$prevtot5<-jolilabelprevcni(baz,"prevtot5")
  
  baz$prevtotsansPTCy2<-baz$prevtot5
  
  baz$prevtot5[which(baz$PTCYJEG=="PTCY")]<-paste0("PTCY__",baz$prevtot5[which(baz$PTCYJEG=="PTCY")])
  baz$prevtot5<-gsub("PTCY__PTCY","PTCY",baz$prevtot5)
  
  baz$prevtotsansPTCy2<-as.factor(gsub("__","+",baz$prevtotsansPTCy2))
  baz$prevtot5<-as.factor(gsub("__","+",baz$prevtot5))

  
  baz$INVIVOJEG<-NA
  baz$INVIVOJEG[!is.na(baz$prevtot4)]<-"No"
  baz$INVIVOJEG[grepl("ATG",baz$prevtot)==TRUE]<-"ATG"
  baz$INVIVOJEG[grepl("CAMP",baz$prevtot)==TRUE]<-"Campath"
  baz$INVIVOJEG[grepl("CAMP",baz$prevtot)==TRUE & grepl("ATG",baz$prevtot)==TRUE]<-"ATG+Campath"
  baz$INVIVOJEG<-as.factor(baz$INVIVOJEG)
  
  baz$TDVIVOJEG<-as.factor(ifelse(baz$INVIVOJEG=="No","No","Yes"))
  
return(baz)
  
}

# 1-Bu (ou Treo) 2-Cy 3-Flu 4-Mel 5-Flamsa 6-Eto 7-Thio 8-Arac 9-Amsa
# 10-CCNU 11-BCNU 12-Clofa 13-Carbo 14-Cisplatine 15-Idarubicine
# Flasma = Flu Asma Arac

doseconddrug<-function(baz,hypothese_treo=FALSE){
  for(i in c("NDOSEDRUG1","NDOSEDRUG2","NDOSEDRUG3","NDOSEDRUG4",
             "NDOSEDRUG5","NDOSEDRUG6","NDOSEDRUG7")) baz[i]<-as.numeric(as.character(baz[,i]))
  
  baz$Bu_DOSEJEG<-NA
  baz$Bu_UNITJEG<-NA
  baz$Bu_ROUTADMJEG<-NA
  
  baz$Treo_DOSEJEG<-NA
  baz$Treo_UNITJEG<-NA
  baz$Treo_ROUTADMJEG<-NA
  
  baz$Cyclo_DOSEJEG<-NA
  baz$Cyclo_UNITJEG<-NA
  baz$Cyclo_ROUTADMJEG<-NA
 
  baz$Flu_DOSEJEG<-NA
  baz$Flu_UNITJEG<-NA
  baz$Flu_ROUTADMJEG<-NA
  
  baz$Mel_DOSEJEG<-NA
  baz$Mel_UNITJEG<-NA
  baz$Mel_ROUTADMJEG<-NA
  
  baz$Eto_DOSEJEG<-NA
  baz$Eto_UNITJEG<-NA
  baz$Eto_ROUTADMJEG<-NA
  
  baz$Thio_DOSEJEG<-NA
  baz$Thio_UNITJEG<-NA
  baz$Thio_ROUTADMJEG<-NA
  
  baz$Arac_DOSEJEG<-NA
  baz$Arac_UNITJEG<-NA
  baz$Arac_ROUTADMJEG<-NA
  
  baz$Amsa_DOSEJEG<-NA
  baz$Amsa_UNITJEG<-NA
  baz$Amsa_ROUTADMJEG<-NA
  
  baz$CCNU_DOSEJEG<-NA
  baz$CCNU_UNITJEG<-NA
  baz$CCNU_ROUTADMJEG<-NA
  
  baz$BCNU_DOSEJEG<-NA
  baz$BCNU_UNITJEG<-NA
  baz$BCNU_ROUTADMJEG<-NA
  
  baz$Clofa_DOSEJEG<-NA
  baz$Clofa_UNITJEG<-NA
  baz$Clofa_ROUTADMJEG<-NA
  
  baz$Carbo_DOSEJEG<-NA
  baz$Carbo_UNITJEG<-NA
  baz$Carbo_ROUTADMJEG<-NA
  
  baz$Cisplatine_DOSEJEG<-NA
  baz$Cisplatine_UNITJEG<-NA
  baz$Cisplatine_ROUTADMJEG<-NA
  
  baz$Carbo_DOSEJEG<-NA
  baz$Carbo_UNITJEG<-NA
  baz$Carbo_ROUTADMJEG<-NA
  
  baz$Idarubicine_DOSEJEG<-NA
  baz$Idarubicine_UNITJEG<-NA
  baz$Idarubicine_ROUTADMJEG<-NA

  baz$ATG_DOSEJEG<-NA
  baz$ATG_UNITJEG<-NA
  baz$ATG_ROUTADMJEG<-NA
  
  baz$Camp_DOSEJEG<-NA
  baz$Camp_UNITJEG<-NA
  baz$Camp_ROUTADMJEG<-NA
  
  baz$Ritux_DOSEJEG<-NA
  baz$Ritux_UNITJEG<-NA
  baz$Ritux_ROUTADMJEG<-NA
  
  
  for(j in c("Bu","Treo","Cyclo","Flu","Mel","Eto","Thio","Arac","Amsa","CCNU","BCNU","Clofa","Carbo","Cisplatine","Idarubicine","ATG","Camp","Ritux")){
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG1==j,baz$NDOSEDRUG1,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG1==j,as.character(baz$DOSEUNITDRUG1),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG1==j,as.character(baz$ROUTADMDRUG1),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG2==j & is.na(baz[,paste(j,"DOSEJEG",sep="_")]),baz$NDOSEDRUG2,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG2==j & is.na(baz[,paste(j,"UNITJEG",sep="_")]),as.character(baz$DOSEUNITDRUG2),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG2==j & is.na(baz[,paste(j,"ROUTADMJEG",sep="_")]),as.character(baz$ROUTADMDRUG2),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG3==j & is.na(baz[,paste(j,"DOSEJEG",sep="_")]),baz$NDOSEDRUG3,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG3==j & is.na(baz[,paste(j,"UNITJEG",sep="_")]),as.character(baz$DOSEUNITDRUG3),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG3==j & is.na(baz[,paste(j,"ROUTADMJEG",sep="_")]),as.character(baz$ROUTADMDRUG3),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG4==j & is.na(baz[,paste(j,"DOSEJEG",sep="_")]),baz$NDOSEDRUG4,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG4==j & is.na(baz[,paste(j,"UNITJEG",sep="_")]),as.character(baz$DOSEUNITDRUG4),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG4==j & is.na(baz[,paste(j,"ROUTADMJEG",sep="_")]),as.character(baz$ROUTADMDRUG4),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG5==j & is.na(baz[,paste(j,"DOSEJEG",sep="_")]),baz$NDOSEDRUG5,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG5==j & is.na(baz[,paste(j,"UNITJEG",sep="_")]),as.character(baz$DOSEUNITDRUG5),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG5==j & is.na(baz[,paste(j,"ROUTADMJEG",sep="_")]),as.character(baz$ROUTADMDRUG5),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG6==j & is.na(baz[,paste(j,"DOSEJEG",sep="_")]),baz$NDOSEDRUG6,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG6==j & is.na(baz[,paste(j,"UNITJEG",sep="_")]),as.character(baz$DOSEUNITDRUG6),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG6==j & is.na(baz[,paste(j,"ROUTADMJEG",sep="_")]),as.character(baz$ROUTADMDRUG6),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
    baz[,paste(j,"DOSEJEG",sep="_")]<-ifelse(baz$CONDDRUG7==j & is.na(baz[,paste(j,"DOSEJEG",sep="_")]),baz$NDOSEDRUG7,baz[,paste(j,"DOSEJEG",sep="_")])
    baz[,paste(j,"UNITJEG",sep="_")]<-ifelse(baz$CONDDRUG7==j & is.na(baz[,paste(j,"UNITJEG",sep="_")]),as.character(baz$DOSEUNITDRUG7),baz[,paste(j,"UNITJEG",sep="_")])
    baz[,paste(j,"ROUTADMJEG",sep="_")]<-ifelse(baz$CONDDRUG7==j & is.na(baz[,paste(j,"ROUTADMJEG",sep="_")]),as.character(baz$ROUTADMDRUG7),baz[,paste(j,"ROUTADMJEG",sep="_")])
    
  }
  
  baz$WEIGHTB<-as.numeric(as.character(baz$WEIGHTB))
  baz$HEIGHT<-as.numeric(as.character(baz$HEIGHT))
  
  # Formule de Boyd
  baz$SC_BOYD<-0.0003207*baz$WEIGHTB^(0.7285-0.0188*log10(baz$WEIGHTB))*baz$HEIGHT^0.3
  baz$SC_GEHAN<-0.0235*baz$HEIGHT^0.42246*baz$WEIGHTB^0.51456
  baz$SC_MOSTELLERS<-sqrt(baz$HEIGHT*baz$WEIGHTB/3600) ### prefered for child and adult
  baz$SC_COSTEFFPED<-(4*baz$WEIGHTB+7)/(90+baz$WEIGHTB)
  
  baz$VTBIDOSE<-as.numeric(as.character(baz$VTBIDOSE))
  
  # 1-mg 2-mg/msq 3-mg/Kg 4-g 5-g/msq 6-g/Kg 7 à 11 truc chelou avec AUC
  
  # 1-Bu (ou Treo) 2-Cy 3-Flu 4-Mel 5-Flamsa 6-Eto 7-Thio 8-Arac 9-Amsa
  # 10-CCNU 11-BCNU 12-Clofa 13-Carbo 14-Cisplatine 15-Idarubicine
  # Flasma = Flu Asma Arac
  
  #### Dose de Busulfan
  baz$Bu_DOSEMGKG<-NA
  baz$Bu_DOSEMGKG[which(baz$Bu_UNITJEG=="mg (miligrams)")]<-baz$Bu_DOSEJEG[which(baz$Bu_UNITJEG=="mg (miligrams)")]/
                                                               baz$WEIGHTB[which(baz$Bu_UNITJEG=="mg (miligrams)")]
  baz$Bu_DOSEMGKG[which(baz$Bu_UNITJEG=="mg/msq")]<-(baz$Bu_DOSEJEG[which(baz$Bu_UNITJEG=="mg/msq")]*
                                                  baz$SC_MOSTELLERS[which(baz$Bu_UNITJEG=="mg/msq")])/
                                                        baz$WEIGHTB[which(baz$Bu_UNITJEG=="mg/msq")]
  baz$Bu_DOSEMGKG[which(baz$Bu_UNITJEG=="mg/Kg")]<-baz$Bu_DOSEJEG[which(baz$Bu_UNITJEG=="mg/Kg")]
  baz$Bu_DOSEMGKG[which(baz$Bu_UNITJEG=="g (grams)")]<-(baz$Bu_DOSEJEG[which(baz$Bu_UNITJEG=="g (grams)")]/1000)/
                                                           baz$WEIGHTB[which(baz$Bu_UNITJEG=="g (grams)")]
  baz$Bu_DOSEMGKG[which(baz$Bu_UNITJEG=="g/msq")]<-(baz$Bu_DOSEJEG[which(baz$Bu_UNITJEG=="g/msq")]*1000*
                                                 baz$SC_MOSTELLERS[which(baz$Bu_UNITJEG=="g/msq")])/
                                                       baz$WEIGHTB[which(baz$Bu_UNITJEG=="g/msq")]
  baz$Bu_DOSEMGKG[which(baz$Bu_UNITJEG=="g/Kg")]<-baz$Bu_DOSEJEG[which(baz$Bu_UNITJEG=="g/Kg")]/1000
 
  #### Dose Treo
  baz$Treo_DOSEGM2<-NA
  baz$Treo_DOSEGM2[which(baz$Treo_UNITJEG=="mg (miligrams)")]<-(baz$Treo_DOSEJEG[which(baz$Treo_UNITJEG=="mg (miligrams)")]/1000)/
                                                               baz$SC_MOSTELLERS[which(baz$Treo_UNITJEG=="mg (miligrams)")]
  baz$Treo_DOSEGM2[which(baz$Treo_UNITJEG=="mg/msq")]<-(baz$Treo_DOSEJEG[which(baz$Treo_UNITJEG=="mg/msq")])/1000
  baz$Treo_DOSEGM2[which(baz$Treo_UNITJEG=="mg/Kg")]<-(baz$Treo_DOSEJEG[which(baz$Treo_UNITJEG=="mg/Kg")]*
                                                            baz$WEIGHTB[which(baz$Treo_UNITJEG=="mg/Kg")])/
                                                (1000*baz$SC_MOSTELLERS[which(baz$Treo_UNITJEG=="mg/Kg")])
  baz$Treo_DOSEGM2[which(baz$Treo_UNITJEG=="g (grams)")]<-(baz$Treo_DOSEJEG[which(baz$Treo_UNITJEG=="g (grams)")])/
                                                          baz$SC_MOSTELLERS[which(baz$Treo_UNITJEG=="g (grams)")]
  baz$Treo_DOSEGM2[which(baz$Treo_UNITJEG=="g/msq")]<-(baz$Treo_DOSEJEG[which(baz$Treo_UNITJEG=="g/msq")])
  baz$Treo_DOSEGM2[which(baz$Treo_UNITJEG=="g/Kg")]<-(baz$Treo_DOSEJEG[which(baz$Treo_UNITJEG=="g/Kg")]*
                                                           baz$WEIGHTB[which(baz$Treo_UNITJEG=="g/Kg")])/
                                                    (baz$SC_MOSTELLERS[which(baz$Treo_UNITJEG=="g/Kg")])
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2==0.036)]<-36
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2 >= 0.035 & baz$Treo_DOSEGM2 <= 0.037)]<-36
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2==3.6)]<-36
  
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2==0.042)]<-42
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2 >= 0.041 & baz$Treo_DOSEGM2 <= 0.043)]<-42
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2 >= 40 & baz$Treo_DOSEGM2 <= 43)]<-42
  
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2==4.2)]<-42
  
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2==0.39)]<-39
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2 >= 0.038 & baz$Treo_DOSEGM2 <= 0.040)]<-39
  baz$Treo_DOSEGM2[which(baz$Treo_DOSEGM2==3.9)]<-39
  
  #### Dose de Cyclo
  baz$Cyclo_DOSEMGKG<-NA
  baz$Cyclo_DOSEMGKG[which(baz$Cyclo_UNITJEG=="mg (miligrams)")]<-baz$Cyclo_DOSEJEG[which(baz$Cyclo_UNITJEG=="mg (miligrams)")]/
                                                                        baz$WEIGHTB[which(baz$Cyclo_UNITJEG=="mg (miligrams)")]
  baz$Cyclo_DOSEMGKG[which(baz$Cyclo_UNITJEG=="mg/msq")]<-(baz$Cyclo_DOSEJEG[which(baz$Cyclo_UNITJEG=="mg/msq")]*
                                                           baz$SC_MOSTELLERS[which(baz$Cyclo_UNITJEG=="mg/msq")])/
                                                                 baz$WEIGHTB[which(baz$Cyclo_UNITJEG=="mg/msq")]
  baz$Cyclo_DOSEMGKG[which(baz$Cyclo_UNITJEG=="mg/Kg")]<-baz$Cyclo_DOSEJEG[which(baz$Cyclo_UNITJEG=="mg/Kg")]
  baz$Cyclo_DOSEMGKG[which(baz$Cyclo_UNITJEG=="g (grams)")]<-(baz$Cyclo_DOSEJEG[which(baz$Cyclo_UNITJEG=="g (grams)")]/1000)/
                                                                    baz$WEIGHTB[which(baz$Cyclo_UNITJEG=="g (grams)")]
  baz$Cyclo_DOSEMGKG[which(baz$Cyclo_UNITJEG=="g/msq")]<-(baz$Cyclo_DOSEJEG[which(baz$Cyclo_UNITJEG=="g/msq")]*1000*
                                                          baz$SC_MOSTELLERS[which(baz$Cyclo_UNITJEG=="g/msq")])/
                                                                baz$WEIGHTB[which(baz$Cyclo_UNITJEG=="g/msq")]
  baz$Cyclo_DOSEMGKG[which(baz$Cyclo_UNITJEG=="g/Kg")]<-baz$Cyclo_DOSEJEG[which(baz$Cyclo_UNITJEG=="g/Kg")]/1000
  
  #### Dose de Fluda
  baz$Flu_DOSEMGM2<-NA
  baz$Flu_DOSEMGM2[which(baz$Flu_UNITJEG=="mg (miligrams)")]<-baz$Flu_DOSEJEG[which(baz$Flu_UNITJEG=="mg (miligrams)")]/
                                                            baz$SC_MOSTELLERS[which(baz$Flu_UNITJEG=="mg (miligrams)")]
  baz$Flu_DOSEMGM2[which(baz$Flu_UNITJEG=="mg/msq")]<-baz$Flu_DOSEJEG[which(baz$Flu_UNITJEG=="mg/msq")]
  baz$Flu_DOSEMGM2[which(baz$Flu_UNITJEG=="mg/Kg")]<-baz$Flu_DOSEJEG[which(baz$Flu_UNITJEG=="mg/Kg")]*
      baz$WEIGHTB[which(baz$Flu_UNITJEG=="mg/Kg")]/baz$SC_MOSTELLERS[which(baz$Flu_UNITJEG=="mg/Kg")]
  baz$Flu_DOSEMGM2[which(baz$Flu_UNITJEG=="g (grams)")]<-(baz$Flu_DOSEJEG[which(baz$Flu_UNITJEG=="g (grams)")]*1000)/
                                                        baz$SC_MOSTELLERS[which(baz$Flu_UNITJEG=="g (grams)")]
  baz$Flu_DOSEMGM2[which(baz$Flu_UNITJEG=="g/msq")]<-baz$Flu_DOSEJEG[which(baz$Flu_UNITJEG=="g/msq")]*1000
  baz$Flu_DOSEMGM2[which(baz$Flu_UNITJEG=="g/Kg")]<-baz$Flu_DOSEJEG[which(baz$Flu_UNITJEG=="g/Kg")]*1000*
                                                        baz$WEIGHTB[which(baz$Flu_UNITJEG=="g/Kg")]/
                                                  baz$SC_MOSTELLERS[which(baz$Flu_UNITJEG=="g/Kg")]
  
  baz$Flu_DOSEMGM2[which(baz$Flu_DOSEMGM2==1600)]<-160
  baz$Flu_DOSEMGM2[which(baz$Flu_DOSEMGM2 >= 1590 & baz$Flu_DOSEMGM2 <= 1610)]<-160
  baz$Flu_DOSEMGM2[which(baz$Flu_DOSEMGM2==1500)]<-150
  baz$Flu_DOSEMGM2[which(baz$Flu_DOSEMGM2 >= 1490 & baz$Flu_DOSEMGM2 <= 1510)]<-150
  
  #### Dose de Melphalan
  baz$Mel_DOSEMGM2<-NA
  baz$Mel_DOSEMGM2[which(baz$Mel_UNITJEG=="mg (miligrams)")]<-baz$Mel_DOSEJEG[which(baz$Mel_UNITJEG=="mg (miligrams)")]/
                                                            baz$SC_MOSTELLERS[which(baz$Mel_UNITJEG=="mg (miligrams)")]
  baz$Mel_DOSEMGM2[which(baz$Mel_UNITJEG=="mg/msq")]<-baz$Mel_DOSEJEG[which(baz$Mel_UNITJEG=="mg/msq")]
  baz$Mel_DOSEMGM2[which(baz$Mel_UNITJEG=="mg/Kg")]<-baz$Mel_DOSEJEG[which(baz$Mel_UNITJEG=="mg/Kg")]*
                                                         baz$WEIGHTB[which(baz$Mel_UNITJEG=="mg/Kg")]/
                                                   baz$SC_MOSTELLERS[which(baz$Mel_UNITJEG=="mg/Kg")]
  baz$Mel_DOSEMGM2[which(baz$Mel_UNITJEG=="g (grams)")]<-(baz$Mel_DOSEJEG[which(baz$Mel_UNITJEG=="g (grams)")]*1000)/
                                                        baz$SC_MOSTELLERS[which(baz$Mel_UNITJEG=="g (grams)")]
  baz$Mel_DOSEMGM2[which(baz$Mel_UNITJEG=="g/msq")]<-baz$Mel_DOSEJEG[which(baz$Mel_UNITJEG=="g/msq")]*1000
  baz$Mel_DOSEMGM2[which(baz$Mel_UNITJEG=="g/Kg")]<-baz$Mel_DOSEJEG[which(baz$Mel_UNITJEG=="g/Kg")]*1000*
                                                        baz$WEIGHTB[which(baz$Mel_UNITJEG=="g/Kg")]/
                                                  baz$SC_MOSTELLERS[which(baz$Mel_UNITJEG=="g/Kg")]
  
  #### Dose de Eto
  baz$Eto_DOSEMGKG<-NA
  baz$Eto_DOSEMGKG[which(baz$Eto_UNITJEG=="mg (miligrams)")]<-baz$Eto_DOSEJEG[which(baz$Eto_UNITJEG=="mg (miligrams)")]/
                                                                  baz$WEIGHTB[which(baz$Eto_UNITJEG=="mg (miligrams)")]
  baz$Eto_DOSEMGKG[which(baz$Eto_UNITJEG=="mg/msq")]<-(baz$Eto_DOSEJEG[which(baz$Eto_UNITJEG=="mg/msq")]*
                                                     baz$SC_MOSTELLERS[which(baz$Eto_UNITJEG=="mg/msq")])/
                                                           baz$WEIGHTB[which(baz$Eto_UNITJEG=="mg/msq")]
  baz$Eto_DOSEMGKG[which(baz$Eto_UNITJEG=="mg/Kg")]<-baz$Eto_DOSEJEG[which(baz$Eto_UNITJEG=="mg/Kg")]
  baz$Eto_DOSEMGKG[which(baz$Eto_UNITJEG=="g (grams)")]<-(baz$Eto_DOSEJEG[which(baz$Eto_UNITJEG=="g (grams)")]/1000)/
                                                              baz$WEIGHTB[which(baz$Eto_UNITJEG=="g (grams)")]
  baz$Eto_DOSEMGKG[which(baz$Eto_UNITJEG=="g/msq")]<-(baz$Eto_DOSEJEG[which(baz$Eto_UNITJEG=="g/msq")]*1000*
                                                    baz$SC_MOSTELLERS[which(baz$Eto_UNITJEG=="g/msq")])/
                                                          baz$WEIGHTB[which(baz$Eto_UNITJEG=="g/msq")]
  baz$Eto_DOSEMGKG[which(baz$Eto_UNITJEG=="g/Kg")]<-baz$Eto_DOSEJEG[which(baz$Eto_UNITJEG=="g/Kg")]/1000
  
  
  #### Dose de Thiothepa
  baz$Thio_DOSEMGKG<-NA
  baz$Thio_DOSEMGKG[which(baz$Thio_UNITJEG=="mg (miligrams)")]<-baz$Thio_DOSEJEG[which(baz$Thio_UNITJEG=="mg (miligrams)")]/
                                                                     baz$WEIGHTB[which(baz$Thio_UNITJEG=="mg (miligrams)")]
  baz$Thio_DOSEMGKG[which(baz$Thio_UNITJEG=="mg/msq")]<-(baz$Thio_DOSEJEG[which(baz$Thio_UNITJEG=="mg/msq")]*
                                                        baz$SC_MOSTELLERS[which(baz$Thio_UNITJEG=="mg/msq")])/
                                                              baz$WEIGHTB[which(baz$Thio_UNITJEG=="mg/msq")]
  baz$Thio_DOSEMGKG[which(baz$Thio_UNITJEG=="mg/Kg")]<-baz$Thio_DOSEJEG[which(baz$Thio_UNITJEG=="mg/Kg")]
  baz$Thio_DOSEMGKG[which(baz$Thio_UNITJEG=="g (grams)")]<-(baz$Thio_DOSEJEG[which(baz$Thio_UNITJEG=="g (grams)")]/1000)/
                                                                 baz$WEIGHTB[which(baz$Thio_UNITJEG=="g (grams)")]
  baz$Thio_DOSEMGKG[which(baz$Thio_UNITJEG=="g/msq")]<-(baz$Thio_DOSEJEG[which(baz$Thio_UNITJEG=="g/msq")]*1000*
                                                       baz$SC_MOSTELLERS[which(baz$Thio_UNITJEG=="g/msq")])/
                                                             baz$WEIGHTB[which(baz$Thio_UNITJEG=="g/msq")]
  baz$Thio_DOSEMGKG[which(baz$Thio_UNITJEG=="g/Kg")]<-baz$Thio_DOSEJEG[which(baz$Thio_UNITJEG=="g/Kg")]/1000
  
  #### Dose Arac
  baz$Arac_DOSEGM2<-NA
  baz$Arac_DOSEGM2[which(baz$Arac_UNITJEG=="mg (miligrams)")]<-(baz$Arac_DOSEJEG[which(baz$Arac_UNITJEG=="mg (miligrams)")]/1000)/
                                                               baz$SC_MOSTELLERS[which(baz$Arac_UNITJEG=="mg (miligrams)")]
  baz$Arac_DOSEGM2[which(baz$Arac_UNITJEG=="mg/msq")]<-(baz$Arac_DOSEJEG[which(baz$Arac_UNITJEG=="mg/msq")])/1000
  baz$Arac_DOSEGM2[which(baz$Arac_UNITJEG=="mg/Kg")]<-(baz$Arac_DOSEJEG[which(baz$Arac_UNITJEG=="mg/Kg")]*
                                                            baz$WEIGHTB[which(baz$Arac_UNITJEG=="mg/Kg")])/
                                                (1000*baz$SC_MOSTELLERS[which(baz$Arac_UNITJEG=="mg/Kg")])
  baz$Arac_DOSEGM2[which(baz$Arac_UNITJEG=="g (grams)")]<-(baz$Arac_DOSEJEG[which(baz$Arac_UNITJEG=="g (grams)")])/
                                                          baz$SC_MOSTELLERS[which(baz$Arac_UNITJEG=="g (grams)")]
  baz$Arac_DOSEGM2[which(baz$Arac_UNITJEG=="g/msq")]<-(baz$Arac_DOSEJEG[which(baz$Arac_UNITJEG=="g/msq")])
  baz$Arac_DOSEGM2[which(baz$Arac_UNITJEG=="g/Kg")]<-(baz$Arac_DOSEJEG[which(baz$Arac_UNITJEG=="g/Kg")]*
                                                           baz$WEIGHTB[which(baz$Arac_UNITJEG=="g/Kg")])/
                                                    (baz$SC_MOSTELLERS[which(baz$Arac_UNITJEG=="g/Kg")])
  
  #### Dose de Amsa
  baz$Amsa_DOSEMGKG<-NA
  baz$Amsa_DOSEMGKG[which(baz$Amsa_UNITJEG=="mg (miligrams)")]<-baz$Amsa_DOSEJEG[which(baz$Amsa_UNITJEG=="mg (miligrams)")]/
                                                                     baz$WEIGHTB[which(baz$Amsa_UNITJEG=="mg (miligrams)")]
  baz$Amsa_DOSEMGKG[which(baz$Amsa_UNITJEG=="mg/msq")]<-(baz$Amsa_DOSEJEG[which(baz$Amsa_UNITJEG=="mg/msq")]*
                                                        baz$SC_MOSTELLERS[which(baz$Amsa_UNITJEG=="mg/msq")])/
                                                              baz$WEIGHTB[which(baz$Amsa_UNITJEG=="mg/msq")]
  baz$Amsa_DOSEMGKG[which(baz$Amsa_UNITJEG=="mg/Kg")]<-baz$Amsa_DOSEJEG[which(baz$Amsa_UNITJEG=="mg/Kg")]
  baz$Amsa_DOSEMGKG[which(baz$Amsa_UNITJEG=="g (grams)")]<-(baz$Amsa_DOSEJEG[which(baz$Amsa_UNITJEG=="g (grams)")]/1000)/
                                                                 baz$WEIGHTB[which(baz$Amsa_UNITJEG=="g (grams)")]
  baz$Amsa_DOSEMGKG[which(baz$Amsa_UNITJEG=="g/msq")]<-(baz$Amsa_DOSEJEG[which(baz$Amsa_UNITJEG=="g/msq")]*1000*
                                                       baz$SC_MOSTELLERS[which(baz$Amsa_UNITJEG=="g/msq")])/
                                                             baz$WEIGHTB[which(baz$Amsa_UNITJEG=="g/msq")]
  baz$Amsa_DOSEMGKG[which(baz$Amsa_UNITJEG=="g/Kg")]<-baz$Amsa_DOSEJEG[which(baz$Amsa_UNITJEG=="g/Kg")]/1000
  
  
  #### Dose de CCNU
  baz$CCNU_DOSEMGM2<-NA
  baz$CCNU_DOSEMGM2[which(baz$CCNU_UNITJEG=="mg (miligrams)")]<-baz$CCNU_DOSEJEG[which(baz$CCNU_UNITJEG=="mg (miligrams)")]/
                                                               baz$SC_MOSTELLERS[which(baz$CCNU_UNITJEG=="mg (miligrams)")]
  baz$CCNU_DOSEMGM2[which(baz$CCNU_UNITJEG=="mg/msq")]<-baz$CCNU_DOSEJEG[which(baz$CCNU_UNITJEG=="mg/msq")]
  baz$CCNU_DOSEMGM2[which(baz$CCNU_UNITJEG=="mg/Kg")]<-baz$CCNU_DOSEJEG[which(baz$CCNU_UNITJEG=="mg/Kg")]*
                                                            baz$WEIGHTB[which(baz$CCNU_UNITJEG=="mg/Kg")]/
                                                      baz$SC_MOSTELLERS[which(baz$CCNU_UNITJEG=="mg/Kg")]
  baz$CCNU_DOSEMGM2[which(baz$CCNU_UNITJEG=="g (grams)")]<-(baz$CCNU_DOSEJEG[which(baz$CCNU_UNITJEG=="g (grams)")]*1000)/
                                                           baz$SC_MOSTELLERS[which(baz$CCNU_UNITJEG=="g (grams)")]
  baz$CCNU_DOSEMGM2[which(baz$CCNU_UNITJEG=="g/msq")]<-baz$CCNU_DOSEJEG[which(baz$CCNU_UNITJEG=="g/msq")]*1000
  baz$CCNU_DOSEMGM2[which(baz$CCNU_UNITJEG=="g/Kg")]<-baz$CCNU_DOSEJEG[which(baz$CCNU_UNITJEG=="g/Kg")]*
                                                      1000*baz$WEIGHTB[which(baz$CCNU_UNITJEG=="g/Kg")]/
                                                     baz$SC_MOSTELLERS[which(baz$CCNU_UNITJEG=="g/Kg")]
  
  #### Dose de BCNU
  baz$BCNU_DOSEMGM2<-NA
  baz$BCNU_DOSEMGM2[which(baz$BCNU_UNITJEG=="mg (miligrams)")]<-baz$BCNU_DOSEJEG[which(baz$BCNU_UNITJEG=="mg (miligrams)")]/
                                                               baz$SC_MOSTELLERS[which(baz$BCNU_UNITJEG=="mg (miligrams)")]
  baz$BCNU_DOSEMGM2[which(baz$BCNU_UNITJEG=="mg/msq")]<-baz$BCNU_DOSEJEG[which(baz$BCNU_UNITJEG=="mg/msq")]
  baz$BCNU_DOSEMGM2[which(baz$BCNU_UNITJEG=="mg/Kg")]<-baz$BCNU_DOSEJEG[which(baz$BCNU_UNITJEG=="mg/Kg")]*
                                                            baz$WEIGHTB[which(baz$BCNU_UNITJEG=="mg/Kg")]/
                                                      baz$SC_MOSTELLERS[which(baz$BCNU_UNITJEG=="mg/Kg")]
  baz$BCNU_DOSEMGM2[which(baz$BCNU_UNITJEG=="g (grams)")]<-(baz$BCNU_DOSEJEG[which(baz$BCNU_UNITJEG=="g (grams)")]*1000)/
                                                           baz$SC_MOSTELLERS[which(baz$BCNU_UNITJEG=="g (grams)")]
  baz$BCNU_DOSEMGM2[which(baz$BCNU_UNITJEG=="g/msq")]<-baz$BCNU_DOSEJEG[which(baz$BCNU_UNITJEG=="g/msq")]*1000
  baz$BCNU_DOSEMGM2[which(baz$BCNU_UNITJEG=="g/Kg")]<-baz$BCNU_DOSEJEG[which(baz$BCNU_UNITJEG=="g/Kg")]*1000*
                                                           baz$WEIGHTB[which(baz$BCNU_UNITJEG=="g/Kg")]/
                                                     baz$SC_MOSTELLERS[which(baz$BCNU_UNITJEG=="g/Kg")]
  
  #### Dose de Clofa
  baz$Clofa_DOSEMGM2<-NA
  baz$Clofa_DOSEMGM2[which(baz$Clofa_UNITJEG=="mg (miligrams)")]<-baz$Clofa_DOSEJEG[which(baz$Clofa_UNITJEG=="mg (miligrams)")]/
                                                                  baz$SC_MOSTELLERS[which(baz$Clofa_UNITJEG=="mg (miligrams)")]
  baz$Clofa_DOSEMGM2[which(baz$Clofa_UNITJEG=="mg/msq")]<-baz$Clofa_DOSEJEG[which(baz$Clofa_UNITJEG=="mg/msq")]
  baz$Clofa_DOSEMGM2[which(baz$Clofa_UNITJEG=="mg/Kg")]<-baz$Clofa_DOSEJEG[which(baz$Clofa_UNITJEG=="mg/Kg")]*
                                                               baz$WEIGHTB[which(baz$Clofa_UNITJEG=="mg/Kg")]/
                                                         baz$SC_MOSTELLERS[which(baz$Clofa_UNITJEG=="mg/Kg")]
  baz$Clofa_DOSEMGM2[which(baz$Clofa_UNITJEG=="g (grams)")]<-(baz$Clofa_DOSEJEG[which(baz$Clofa_UNITJEG=="g (grams)")]*1000)/
                                                              baz$SC_MOSTELLERS[which(baz$Clofa_UNITJEG=="g (grams)")]
  baz$Clofa_DOSEMGM2[which(baz$Clofa_UNITJEG=="g/msq")]<-baz$Clofa_DOSEJEG[which(baz$Clofa_UNITJEG=="g/msq")]*1000
  baz$Clofa_DOSEMGM2[which(baz$Clofa_UNITJEG=="g/Kg")]<-baz$Clofa_DOSEJEG[which(baz$Clofa_UNITJEG=="g/Kg")]*1000*
                                                              baz$WEIGHTB[which(baz$Clofa_UNITJEG=="g/Kg")]/
                                                        baz$SC_MOSTELLERS[which(baz$Clofa_UNITJEG=="g/Kg")]
  

  #### Dose de ATG
  baz$ATG_DOSEMGKG<-NA
  baz$ATG_DOSEMGKG[which(baz$ATG_UNITJEG=="mg (miligrams)")]<-baz$ATG_DOSEJEG[which(baz$ATG_UNITJEG=="mg (miligrams)")]/
    baz$WEIGHTB[which(baz$ATG_UNITJEG=="mg (miligrams)")]
  baz$ATG_DOSEMGKG[which(baz$ATG_UNITJEG=="mg/msq")]<-(baz$ATG_DOSEJEG[which(baz$ATG_UNITJEG=="mg/msq")]*
                                                       baz$SC_MOSTELLERS[which(baz$ATG_UNITJEG=="mg/msq")])/
    baz$WEIGHTB[which(baz$ATG_UNITJEG=="mg/msq")]
  baz$ATG_DOSEMGKG[which(baz$ATG_UNITJEG=="mg/Kg")]<-baz$ATG_DOSEJEG[which(baz$ATG_UNITJEG=="mg/Kg")]
  baz$ATG_DOSEMGKG[which(baz$ATG_UNITJEG=="g (grams)")]<-(baz$ATG_DOSEJEG[which(baz$ATG_UNITJEG=="g (grams)")]/1000)/
    baz$WEIGHTB[which(baz$ATG_UNITJEG=="g (grams)")]
  baz$ATG_DOSEMGKG[which(baz$ATG_UNITJEG=="g/msq")]<-(baz$ATG_DOSEJEG[which(baz$ATG_UNITJEG=="g/msq")]*1000*
                                                      baz$SC_MOSTELLERS[which(baz$ATG_UNITJEG=="g/msq")])/
    baz$WEIGHTB[which(baz$ATG_UNITJEG=="g/msq")]
  baz$ATG_DOSEMGKG[which(baz$ATG_UNITJEG=="g/Kg")]<-baz$ATG_DOSEJEG[which(baz$ATG_UNITJEG=="g/Kg")]/1000
    
  
###############################################################################################
###############################################################################################
################### Correction Myeloabr  ######################################################
###############################################################################################

baz$MYELOABR[which(baz$MYELOABR=="unknown")]<-NA
baz$MYELOABRJEG<-as.character(baz$MYELOABR)

baz$MYELOABRJEG[which(baz$VTBIDOSE > 6)]<-"Yes"
baz$MYELOABRJEG[which(baz$Bu_DOSEMGKG >= 9.2)]<-"Yes" # Marge d erreur de conversion
baz$MYELOABRJEG[which(baz$Bu_DOSEMGKG >= 11.5 & baz$Bu_ROUTADMJEG=="Oral")]<-"Yes"

baz$MYELOABRJEG[which(baz$Bu_DOSEMGKG <= 6 & baz$cond4=="Bu")]<-"No"

# baz$MYELOABRJEG[which(baz$cond4=="TBI+Flu" & baz$VTBIDOSE <= 6)]<-"No"
# baz$MYELOABRJEG[which(baz$cond4=="TBI+Cy" & baz$VTBIDOSE <= 6)]<-"No"



if(hypothese_treo==TRUE){
  baz$MYELOABRJEG[which(baz$Treo_DOSEGM2 >= 42)]<-"Yes"
baz$MYELOABRJEG[which(baz$cond4 == "Treo+Flu+Thio" & baz$Treo_DOSEGM2 >=36)]<-"Yes"
}



##### PTCy / Exvivo / ATG/Campth

baz$PTCYTDEPATG<-NA
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & baz$TDVIVOJEG=="Yes" & baz$PTCYEP=="Yes")]<-"Exvivo, Invivo, PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & baz$TDVIVOJEG=="Yes" & baz$PTCYEP=="No")]<-"Exvivo, Invivo"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & baz$TDVIVOJEG=="Yes" & is.na(baz$PTCYEP))]<-"Exvivo, Invivo, missing PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & baz$TDVIVOJEG=="Yes" & baz$PTCYEP=="Yes")]<-"Invivo, PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & baz$TDVIVOJEG=="Yes" & baz$PTCYEP=="No")]<-"Invivo"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & baz$TDVIVOJEG=="Yes" & is.na(baz$PTCYEP))]<-"Invivo, missing PTCy"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & baz$TDVIVOJEG=="Yes" & baz$PTCYEP=="Yes")]<-"missing Exvivo, Invivo, PTCy"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & baz$TDVIVOJEG=="Yes" & baz$PTCYEP=="No")]<-"missing Exvivo, Invivo"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & baz$TDVIVOJEG=="Yes" & is.na(baz$PTCYEP))]<-"missing Exvivo, Invivo, missing PTCy"

baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & baz$TDVIVOJEG=="No" & baz$PTCYEP=="Yes")]<-"Exvivo, PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & baz$TDVIVOJEG=="No" & baz$PTCYEP=="No")]<-"Exvivo"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & baz$TDVIVOJEG=="No" & is.na(baz$PTCYEP))]<-"Exvivo, missing PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & baz$TDVIVOJEG=="No" & baz$PTCYEP=="Yes")]<-"PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & baz$TDVIVOJEG=="No" & baz$PTCYEP=="No")]<-"No"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & baz$TDVIVOJEG=="No" & is.na(baz$PTCYEP))]<-"missing PTCy"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & baz$TDVIVOJEG=="No" & baz$PTCYEP=="Yes")]<-"missing Exvivo, PTCy"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & baz$TDVIVOJEG=="No" & baz$PTCYEP=="No")]<-"missing Exvivo"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & baz$TDVIVOJEG=="No" & is.na(baz$PTCYEP))]<-"missing Exvivo, missing PTCy"


baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & is.na(baz$TDVIVOJEG) & baz$PTCYEP=="Yes")]<-"Exvivo, missing Invivo, PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & is.na(baz$TDVIVOJEG) & baz$PTCYEP=="No")]<-"Exvivo, missing Invivo"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="Yes" & is.na(baz$TDVIVOJEG) & is.na(baz$PTCYEP))]<-"Exvivo, missing Invivo, missing PTCy"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & is.na(baz$TDVIVOJEG) & baz$PTCYEP=="Yes")]<-"PTCy, missing Invivo"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & is.na(baz$TDVIVOJEG) & baz$PTCYEP=="No")]<-"No, missing Invivo"
baz$PTCYTDEPATG[which(baz$TDEPLVITROJEG=="No" & is.na(baz$TDVIVOJEG) & is.na(baz$PTCYEP))]<-"missing PTCy, missing Invivo"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & is.na(baz$TDVIVOJEG) & baz$PTCYEP=="Yes")]<-"missing Exvivo, missing Invivo, PTCy"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & is.na(baz$TDVIVOJEG) & baz$PTCYEP=="No")]<-"missing Exvivo, missing Invivo"
baz$PTCYTDEPATG[which(is.na(baz$TDEPLVITROJEG) & is.na(baz$TDVIVOJEG) & is.na(baz$PTCYEP))]<-"missing Exvivo, missing Invivo, missing PTCy"

baz$PTCYTDEPATG2<-baz$PTCYTDEPATG
baz$PTCYTDEPATG2[which(baz$PTCYTDEPATG %in% c("Exvivo, Invivo, missing PTCy"))]<-"Exvivo, Invivo"
baz$PTCYTDEPATG2[which(baz$PTCYTDEPATG %in% c("Exvivo, missing PTCy"))]<-"Exvivo"
baz$PTCYTDEPATG2[which(baz$PTCYTDEPATG %in% c("Invivo, missing PTCy",
                                              "missing Exvivo, Invivo",
                                              "missing Exvivo, Invivo, missing PTCy"))]<-"Invivo"
baz$PTCYTDEPATG2[which(baz$PTCYTDEPATG %in% c("missing Exvivo","missing Exvivo, missing PTCy","missing PTCy"))]<-"No"
baz$PTCYTDEPATG2[which(baz$PTCYTDEPATG %in% c("missing Exvivo, Invivo, PTCy"))]<-"Invivo, PTCy"
baz$PTCYTDEPATG2[which(baz$PTCYTDEPATG %in% c("missing Exvivo, PTCy"))]<-"PTCy"

baz$PTCYTDEPATG3<-baz$PTCYTDEPATG2
baz$PTCYTDEPATG3[which(baz$PTCYTDEPATG3 %in% c("Exvivo, Invivo"))]<-"Exvivo"
baz$PTCYTDEPATG3[which(baz$PTCYTDEPATG3 %in% c("Invivo, PTCy"))]<-"PTCy"
baz$PTCYTDEPATG3[which(baz$PTCYTDEPATG3 %in% c("Exvivo, Invivo, PTCy"))]<-"Exvivo, PTCy"

return(baz)
}




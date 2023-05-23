
sectrans<-function(baz=baz){
baz<-droplevels(baz)

# baz$DATBMT <- as.Date(as.numeric(as.character(baz$DATBMT))/86400, origin = "1582-10-14")
# baz$DATPATBD <- as.Date(as.numeric(as.character(baz$DATPATBD))/86400, origin = "1582-10-14")
# baz$DATRELAPSE <- as.Date(as.numeric(as.character(baz$DATRELAPSE))/86400, origin = "1582-10-14")
# baz$DTRANS2 <- as.Date(as.numeric(as.character(baz$DTRANS2))/86400, origin = "1582-10-14")
# baz$DTRANS3 <- as.Date(as.numeric(as.character(baz$DTRANS3))/86400, origin = "1582-10-14")
# baz$DATAGVH <- as.Date(as.numeric(as.character(baz$DATAGVH))/86400, origin = "1582-10-14")

print("a")

baz$Nbr_greffe<-NA

for( i in 1:dim(baz)[1]) {
  baz$Nbr_greffe[i]<-length(baz$IDPROMISE[which(baz$IDPROMISE==baz$IDPROMISE[i])])
  print(i)
}

print("b")

baz$BMTNR6<-baz$BMTNR5<-baz$BMTNR4<-baz$BMTNR3<-baz$BMTNR2<-baz$BMTNR1<-NA
baz$DATBMT6<-baz$DATBMT5<-baz$DATBMT4<-baz$DATBMT3<-baz$DATBMT2<-baz$DATBMT1<-as.Date(NA)
baz$VTRANTYP6<-baz$VTRANTYP5<-baz$VTRANTYP4<-baz$VTRANTYP3<-baz$VTRANTYP2<-baz$VTRANTYP1<-NA
baz$BMTNRAU6<-baz$BMTNRAU5<-baz$BMTNRAU4<-baz$BMTNRAU3<-baz$BMTNRAU2<-baz$BMTNRAU1<-NA
baz$BMTNRAL6<-baz$BMTNRAL5<-baz$BMTNRAL4<-baz$BMTNRAL3<-baz$BMTNRAL2<-baz$BMTNRAL1<-NA
baz$DISMCLFD6<-baz$DISMCLFD5<-baz$DISMCLFD4<-baz$DISMCLFD3<-baz$DISMCLFD2<-baz$DISMCLFD1<-NA
baz$BMTDISESTA6<-baz$BMTDISESTA5<-baz$BMTDISESTA4<-baz$BMTDISESTA3<-baz$BMTDISESTA2<-baz$BMTDISESTA1<-NA
baz$BMTDISESTAJEG6<-baz$BMTDISESTAJEG5<-baz$BMTDISESTAJEG4<-baz$BMTDISESTAJEG3<-baz$BMTDISESTAJEG2<-baz$BMTDISESTAJEG1<-NA
baz$DATRELAPSE6<-baz$DATRELAPSE5<-baz$DATRELAPSE4<-baz$DATRELAPSE3<-baz$DATRELAPSE2<-baz$DATRELAPSE1<-as.Date(NA)
baz$DISMCLFD6<-baz$DISMCLFD5<-baz$DISMCLFD4<-baz$DISMCLFD3<-baz$DISMCLFD2<-baz$DISMCLFD1<-NA
baz$VRELPROG6<-baz$VRELPROG5<-baz$VRELPROG4<-baz$VRELPROG3<-baz$VRELPROG2<-baz$VRELPROG1<-NA
baz$AGVHGRMX6<-baz$AGVHGRMX5<-baz$AGVHGRMX4<-baz$AGVHGRMX3<-baz$AGVHGRMX2<-baz$AGVHGRMX1<-NA
baz$DATAGVH6<-baz$DATAGVH5<-baz$DATAGVH4<-baz$DATAGVH3<-baz$DATAGVH2<-baz$DATAGVH1<-as.Date(NA)
baz$ENGRAF6<-baz$ENGRAF5<-baz$ENGRAF4<-baz$ENGRAF3<-baz$ENGRAF2<-baz$ENGRAF1<-NA
baz$ENGNEUT6<-baz$ENGNEUT5<-baz$ENGNEUT4<-baz$ENGNEUT3<-baz$ENGNEUT2<-baz$ENGNEUT1<-NA
baz$DATCRGR26<-baz$DATCRGR25<-baz$DATCRGR24<-baz$DATCRGR23<-baz$DATCRGR22<-baz$DATCRGR21<-as.Date(NA)
baz$ENGNEUT6<-baz$ENGNEUT5<-baz$ENGNEUT4<-baz$ENGNEUT3<-baz$ENGNEUT2<-baz$ENGNEUT1<-NA
baz$HSCTYPE36<-baz$HSCTYPE35<-baz$HSCTYPE34<-baz$HSCTYPE33<-baz$HSCTYPE32<-baz$HSCTYPE31<-NA
baz$SAMEDONO6<-baz$SAMEDONO5<-baz$SAMEDONO4<-baz$SAMEDONO3<-baz$SAMEDONO2<-baz$SAMEDONO1<-NA
baz$COND46<-baz$COND45<-baz$COND44<-baz$COND43<-baz$COND42<-baz$COND41<-NA
baz$PREVTOT46<-baz$PREVTOT45<-baz$PREVTOT44<-baz$PREVTOT43<-baz$PREVTOT42<-baz$PREVTOT41<-NA

print("c")

listIDPROMISE<-(unique(baz$IDPROMISE))

print("d")


baz2<-NULL
k<-0
for (i in listIDPROMISE){
  k<-k+1
  data_temp<-baz[which(baz$IDPROMISE==i),]
  for( j in 1:nrow(data_temp)){
    print(j)
    print(i)
    nomvar<-paste0("BMTNR",j)
    nomvar2<-paste0("DATBMT",j)
    nomvar3<-paste0("VTRANTYP",j)
    nomvar4<-paste0("BMTNRAU",j)
    nomvar5<-paste0("BMTNRAL",j)
    nomvar6<-paste0("DISMCLFD",j)
    nomvar7<-paste0("BMTDISESTA",j)
    nomvar8<-paste0("BMTDISESTAJEG",j)
    nomvar9<-paste0("DISMCLFD",j)
    nomvar10<-paste0("VRELPROG",j)
    nomvar11<-paste0("AGVHGRMX",j)
    nomvar12<-paste0("DATAGVH",j)
    nomvar13<-paste0("ENGRAF",j)
    nomvar14<-paste0("ENGNEUT",j)
    nomvar15<-paste0("DATCRGR2",j)
    nomvar16<-paste0("HSCTYPE3",j)
    nomvar17<-paste0("SAMEDONO",j)
    nomvar18<-paste0("COND4",j)
    nomvar19<-paste0("PREVTOT4",j)
    nomvar20<-paste0("DATRELAPSE",j)
    
    
    data_temp[[nomvar]]<-data_temp$BMTNR[j]
    data_temp[[nomvar2]]<-data_temp$DATBMT[j]
    data_temp[[nomvar3]]<-data_temp$VTRANTYP[j]
    data_temp[[nomvar4]]<-data_temp$BMTNRAU[j]
    data_temp[[nomvar5]]<-data_temp$BMTNRAL[j]
    data_temp[[nomvar6]]<-data_temp$DISMCLFD[j]
    data_temp[[nomvar7]]<-data_temp$BMTDISESTA[j]
    data_temp[[nomvar8]]<-data_temp$BMTDISESTAJEG[j]
    print(j)
    data_temp[[nomvar9]]<-data_temp$DISMCLFD[j]
    data_temp[[nomvar10]]<-data_temp$VRELPROG[j]
    data_temp[[nomvar11]]<-data_temp$AGVHGRMX[j]
    data_temp[[nomvar12]]<-data_temp$DATAGVH[j]
    data_temp[[nomvar13]]<-data_temp$ENGRAF[j]
    data_temp[[nomvar14]]<-data_temp$ENGNEUT[j]
    data_temp[[nomvar15]]<-data_temp$DATCRGR2[j]
    data_temp[[nomvar16]]<-data_temp$HSCTYPE3[j]
    data_temp[[nomvar17]]<-data_temp$SAMEDONO[j]
    data_temp[[nomvar18]]<-data_temp$COND4[j]
    data_temp[[nomvar19]]<-data_temp$PREVTOT4[j]
    data_temp[[nomvar20]]<-data_temp$DATRELAPSE[j]
    
    
  }
  print(dim(data_temp))
  print(dim(baz2))
  
  baz2<-rbind(baz2,data_temp)
  print(k)
}

print("e")


# baz3<-baz2[which(baz2$BMTNR=="First"),c("ID", "IDAA", "CENTRE","IDPROMISE", 
#                                       "DATBMT", "DATPATBD","DISMCLFD", 
#                                       "BMTNR","BMTNR1", "BMTNR2", 
#                                       "BMTNR3", "DATBMT1", "DATBMT2", "DATBMT3", "VTRANTYP1", "VTRANTYP2", 
#                                       "VTRANTYP3", "BMTNRAU1", "BMTNRAU2", "BMTNRAU3", "BMTNRAL1", 
#                                       "BMTNRAL2", "BMTNRAL3", "DISMCLFD1", "DISMCLFD2", "DISMCLFD3", 
#                                       "BMTDISESTA1", "BMTDISESTA2", "BMTDISESTA3", "DATRELAPSE1", "DATRELAPSE2", 
#                                       "DATRELAPSE3", "VRELPROG1", "VRELPROG2", "VRELPROG3","DATAGVH1", "DATAGVH2", 
#                                       "DATAGVH3", "AGVHGRMX1", "AGVHGRMX2", "AGVHGRMX3")]
return(baz2)
}


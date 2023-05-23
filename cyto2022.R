
cyto2022<-function(data){

baz<-data  
baz$CYTO_CLEAN<-as.character(baz$CYTOANO)

baz$CYTO_CLEAN<-gsub("monossomy","monosomy",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("MONOSOMY","monosomy",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("monosomie","monosomy",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomi","monosomy",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("trysom","trisom",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("trisomie","trisomy",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("trisomi","trisomy",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("delétion","deletion",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("délétion","deletion",tolower(baz$CYTO_CLEAN))

### Complex K
baz$CYTO_CLEAN<-gsub("multiple \\(>=3\\),","Complex__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("complexe caryotype \\(3 or more abnormalities\\),","Complex__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("complex__, ","Complex__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("complex__complex__","Complex__",tolower(baz$CYTO_CLEAN))

### Monosomal
baz$CYTO_CLEAN<-gsub("monosomal caryotype,","Monosomal__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomal__Monosomal__","Monosomal__",tolower(baz$CYTO_CLEAN))

baz$CYTO_CLEAN<-gsub("t\\(1-3\\)","t\\(1;3\\)",tolower(baz$CYTO_CLEAN))


baz$CYTO_CLEAN<-gsub("2-31","2_31",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-32","2_32",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-33","2_33",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-34","2_34",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-35","2_35",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-36","2_36",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-37","2_37",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-38","2_38",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("2-39","2_39",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-31","3_31",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-32","3_32",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-33","3_33",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-34","3_34",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-35","3_35",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-36","3_36",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-37","3_37",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-38","3_38",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3-39","3_39",tolower(baz$CYTO_CLEAN))

### Inversion du 16
baz$CYTO_CLEAN<-gsub("inv\\(16\\)\\(p13q22\\)","inv(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(16\\)__Other abnormality\\(ies\\),, \\(p13q22\\)","inv(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(16\\)__Other abnormality\\(ies\\),Complex__Monosomal__, \\(p13q22\\)","inv(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(16;16\\)\\(p13.1;q22\\)","inv(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(16;16\\)\\(p13;q22\\)","inv(16)__",baz$CYTO_CLEAN)

### del(7q)
baz$CYTO_CLEAN<-gsub("del\\(7q\\) / 7q-,","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("7q-,","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("7q-,","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7q\\),","del(7q)__",baz$CYTO_CLEAN)


### t(8;21)
baz$CYTO_CLEAN<-gsub("t\\(8,21\\)","t(8;21)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(8 ;21","t(8;21",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(8:21\\)\\(q22;q22\\),","t(8;21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(8;21\\)\\(q22;q22\\),","t(8;21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(8.21\\),","t(8;21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(8,21\\),","t(8;21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(8;21\\),","t(8;21)__",baz$CYTO_CLEAN)

### t(15;17)
baz$CYTO_CLEAN<-gsub("t\\(15;17\\)\\(p11;q11\\)","t(15;17)__",baz$CYTO_CLEAN)

#### del(17) a laisser avant del(7) !!!!!
baz$CYTO_CLEAN<-gsub("-17 / monosomy 17,","mono(17)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 17,","mono(17)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 17","mono(17)__",tolower(baz$CYTO_CLEAN))

baz$CYTO_CLEAN<-gsub("del\\(17p,","del(17p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(17q,","del(17q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub(", 17p deletion","del(17p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(17p-\\)","del(17p)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del17p,","del(17p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del17q,","del(17q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del17 p,","del(17p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del17 q,","del(17q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del17\\(q12,","del(17q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("add\\(17\\)\\(p13\\)","add(17p)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(17\\),","mono(17)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-17","mono(17)__",baz$CYTO_CLEAN)

### del(7q)
baz$CYTO_CLEAN<-gsub("del\\(7q\\) / 7q-,","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("7q-,","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("7q-","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del 7q \\( 7q-\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7q\\),","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-7q22","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-7q","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q\\?31\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(7q22\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q2\\?2\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q2\\?3\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q21\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(Q21\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q21q31\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q21;q32\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\) \\(q21q35\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q21q35\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q32\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q33\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q34\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q35\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q36\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q11q36\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del \\(7\\)\\(q21q35\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q31q35\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q31\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q22q32\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q11\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q31\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q32\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q33\\)","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(q36\\)","del(7q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(p\\?21\\)","del(7p)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(p13p22\\)","del(7p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(p13\\)","del(7p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7\\)\\(p11\\)","del(7p)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del7q","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del \\(7\\)","del(7)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("7q deletion","del(7q)__",tolower(baz$CYTO_CLEAN))


### Monosomy or del 7
baz$CYTO_CLEAN<-gsub("-7 / monosomy 7,","mono(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 7,","mono(7)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 7","mono(7)__",tolower(baz$CYTO_CLEAN))

baz$CYTO_CLEAN<-gsub("del\\(7\\),","mono(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-7","mono(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("- 7","mono(7)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del 7","del(7)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del7","del(7)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub(",del\\(7\\)__","del(7)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, deletion du chromosome 7","del(7)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("deletion du chromosome 7","del(7)__",tolower(baz$CYTO_CLEAN))

### del(15) a laisser avant del 5 !!!!
baz$CYTO_CLEAN<-gsub("-15 / monosomy 15,","mono(15)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(15p,","del(15p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(15q,","del(15q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del15p,","del(15p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del15q,","del(15q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del15 p,","del(15p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del15 q,","del(15q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del15\\(q12,","del(15q)__",baz$CYTO_CLEAN)


baz$CYTO_CLEAN<-gsub("del\\(15\\),","mono(15)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 15,","mono(15)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 15","mono(15)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-15","mono(15)__",baz$CYTO_CLEAN)

### Del 5q
baz$CYTO_CLEAN<-gsub("del\\(5q\\) / 5q-,","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("deletion 5q","del(5q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("5q deleted","del(5q)__",tolower(baz$CYTO_CLEAN))

baz$CYTO_CLEAN<-gsub("5q-,","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("5q-","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del \\(5q\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(p13p14\\)del\\(q23q33\\)","del(5p)__del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(p13;pter\\)","del(5p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del5q","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("5q31 deletion","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("5q31deletion","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("deletion du 5q","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("delin 5q","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("deletion,, 5q","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("deletion of chromosom 5q","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("5 q-","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("5q del","del(5q)__",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q11;q35\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q12q31\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q13q15\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q13q31\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q13q32\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q13q33\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q13q34\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q\\?13q35\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q13q35\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q14q23\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q\\?q\\?\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q1\\?5q3\\?3\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q11.2q33\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q12q33\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q14)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q15q33\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q21q31\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q21q33\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q21q34\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q22q31\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q31\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5q31\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q31q35\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\) \\(q31, q34","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q12;q32\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del 5q31","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q1\\?4q3\\?4)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q2\\?3q3\\?3\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q3\\?2q3\\?3\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q2\\?2q3\\?3\\)","del(5q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q\\?11.2q\\?32-34\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q\\?11.2q\\?32-34\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5\\)\\(q12;q33\\)","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5q\\)__other abnormality\\(ies\\),","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub(", 46, xy, del\\(5q\\)__\\[6\\]\\/46, xy\\[14\\]","del(5q)__",baz$CYTO_CLEAN)


### Mono 5
baz$CYTO_CLEAN<-gsub("-5 / monosomy 5,","mono(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 5,","mono(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 5","mono(5)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(5\\),","mono(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-5","mono(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("- 5","mono(5)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del 5","del(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del5","del(5)__",baz$CYTO_CLEAN)

### del(13) Ã  laisser avant del(3)
baz$CYTO_CLEAN<-gsub("-13 / monosomy 13,","mono(13)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(13q\\) /13q-,","del(13q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(13p,","del(13p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(13q,","del(13q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del13p,","del(13p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del13q,","del(13q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del13 p,","del(13p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del13 q,","del(13q)__",baz$CYTO_CLEAN)


baz$CYTO_CLEAN<-gsub("13q-,","del(13q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(13\\),","mono(13)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 13,","mono(13)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 13","mono(13)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-13","mono(13)__",baz$CYTO_CLEAN)


### Del 3q
baz$CYTO_CLEAN<-gsub("del\\(3q\\) / 3q-,","del(3q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("3q-","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("deletion 3q","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("3q deleted","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del\\(3\\)\\(q13q25\\)","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del\\(3\\)\\(q25\\)","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del\\(3\\)q25","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del\\(3\\)\\(q\\?22,q\\?26\\)","del(3q)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del\\(3\\)\\(q21\\)","del(3q)__",tolower(baz$CYTO_CLEAN))

baz$CYTO_CLEAN<-gsub("del\\(3\\)\\(p21\\)","del(3p)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("del\\(3\\)\\(p21p25\\)","del(3p)__",tolower(baz$CYTO_CLEAN))

### 11q23

baz$CYTO_CLEAN<-gsub("abn 11q23,","abn(11q23)",baz$CYTO_CLEAN)


baz$CYTO_CLEAN<-gsub("\\+11q23","add(11q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(11\\)\\/q13q23\\)-13\\/14\\)\\(q32\\)","del(11q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(11\\)\\(q22-23\\),","del(11q23)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("t\\(4;11\\)\\(q21/q23\\)","t(4;11)(q21;q13)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(4;11\\),abn 11q23,","t(4;11)(q21;q13)",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("t\\(6;11\\)\\(q27;q23\\)","t(6;11)(q27;q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(6;11\\),Other abnormality\\(ies\\),, \\(q27;q23\\)","t(6;11)(q27;q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(6;11\\)\\(q25-27;q23\\)","t(6;11)(q25_27;q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(6;11\\)\\(q25_27;q23\\)","t(6;11)(q25_27;q23)",baz$CYTO_CLEAN)




baz$CYTO_CLEAN<-gsub("t\\(9;11\\)\\(p22;q23\\)","t(9;11)(p22;q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(10;11\\)\\(p11;q23\\)","t(10;11)(p11;q23)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(10;11\\)\\(q22;q23\\)","t(10;11)(q22;q23)",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("t\\(11:14\\)\\(q23;q11\\)","t(11;14)(q23;q11)",baz$CYTO_CLEAN)


baz$CYTO_CLEAN<-gsub("t\\(11;16\\)\\(q23;p13\\)","t(11;16)(q23;q13)",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("t\\(11;17\\)\\(q23;q1\\?2\\)","t(11;17)(q23;q1\\?2)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(11,19","t(11;19",baz$CYTO_CLEAN)


baz$CYTO_CLEAN<-gsub("t\\(11;19\\)\\(q23;p13\\)","t(11;19)(q23;p13)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t \\(11;19\\) \\(q23; p13\\)","t(11;19)(q23;p13)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(11;19\\)\\(q23;q13.1\\)","t(11;19)(q23;p13)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(11;19\\),Other abnormality\\(ies\\),, q23.3; p13.1","t(11;19)(q23;p13)",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("(9.22)","t\\(9;22)",baz$CYTO_CLEAN)


### Monosomy
baz$CYTO_CLEAN<-gsub("Monosomy,","Monosomy__",baz$CYTO_CLEAN)

### Hyperdiploid
baz$CYTO_CLEAN<-gsub("Hyperdiploid \\(>46 chromosomes\\),","Hyperdiploid__",baz$CYTO_CLEAN)

### t(1;17)
baz$CYTO_CLEAN<-gsub("t\\(1;17\\)\\(q21;p13\\)","t(1;17)__",baz$CYTO_CLEAN)


## t(8;21)
baz$CYTO_CLEAN<-gsub("t\\(8;21\\),","t(8;21)__",baz$CYTO_CLEAN)

### t(9;22)
baz$CYTO_CLEAN<-gsub("t\\(9;22\\),","t(9;22)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(9 ;22\\)","t(9;22)__",baz$CYTO_CLEAN)






### t(15;17)
baz$CYTO_CLEAN<-gsub("t\\(15;17\\),","t\\(15;17\\)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("t\\(5;12\\)\\(q23-31;p13)","t(5;12)(q23_31;p13)",baz$CYTO_CLEAN)


### t(15;22)
baz$CYTO_CLEAN<-gsub("Other abnormality(ies),, t\\(15;22\\)","t(15;22)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("t\\(3,3\\) \\(q21;q26\\)","t(3;3)(q21;q26)",baz$CYTO_CLEAN)



### Del 3q
baz$CYTO_CLEAN<-gsub("del\\(3q\\) / 3q-,","del(3q)__",baz$CYTO_CLEAN)


### Isochrome
baz$CYTO_CLEAN<-gsub("Other abnormality\\(ies\\),, isocromosoma 17","i(17)__",baz$CYTO_CLEAN)

### Inversion
baz$CYTO_CLEAN<-gsub("inv\\(1\\)/t\\(1;1\\),","inv(1)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(2\\)/t\\(2;2\\),","inv(2)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(3\\)/t\\(3;3\\),","inv(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(3;3\\),","inv(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv 3","inv(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv3","inv(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t 3;3","inv(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(3q;3q\\)","inv(3)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("inv\\(4\\)/t\\(4;4\\),","inv(4)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(5\\)/t\\(5;5\\),","inv(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(6\\)/t\\(6;6\\),","inv(6)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(7\\)/t\\(7;7\\),","inv(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(8\\)/t\\(8;8\\),","inv(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(9\\)/t\\(9;9\\),","inv(9)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(10\\)/t\\(10;10\\),","inv(10)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(11\\)/t\\(11;11\\),","inv(11)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(12\\)/t\\(12;12\\),","inv(12)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(13\\)/t\\(13;13\\),","inv(13)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(14\\)/t\\(14;14\\),","inv(14)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(15\\)/t\\(15;15\\),","inv(15)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(16\\)/t\\(16;16\\),","inv(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv \\(16","inv(16",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("inv\\(17\\)/t\\(17;17\\),","inv(17)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(18\\)/t\\(18;18\\),","inv(18)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(19\\)/t\\(19;19\\),","inv(19)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(20\\)/t\\(20;20\\),","inv(20)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(21\\)/t\\(21;21\\),","inv(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("inv\\(22\\)/t\\(22;22\\),","inv(22)__",baz$CYTO_CLEAN)

## inv (16)(p13;q21) ???


### del q
baz$CYTO_CLEAN<-gsub("del\\(20q\\) /20q-,","del(20q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(19q\\) /19q-,","del(19q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(18q\\) /18q-,","del(18q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(16q\\) /16q-,","del(16q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(14q\\) /14q-,","del(14q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(12q\\) /12q-,","del(12q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(11q\\) /11q-,","del(11q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(11\\)\\(q22-23\\),","del(11q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(10q\\) /10q-,","del(10q)__",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("del\\(9q\\) /9q-,","del(9q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(8q\\) /8q-,","del(8q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7q\\) /7q-,","del(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(6q\\) /6q-,","del(6q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5q\\) /5q-,","del(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(4q\\) /4q-,","del(4q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(3q\\) /3q-,","del(3q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(2q\\) /2q-,","del(2q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(1q\\) /1q-,","del(1q)__",baz$CYTO_CLEAN)

### del p
baz$CYTO_CLEAN<-gsub("del\\(20p\\) /20p-,","del(20p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(19p\\) /19p-,","del(19p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(18p\\) /18p-,","del(18p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(16p\\) /16p-,","del(16p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(14p\\) /14p-,","del(14p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(12p\\) /12p-,","del(12p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(11p\\) /11p-,","del(11p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(10p\\) /10p-,","del(10p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(9p\\) /9p-,","del(9p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(8p\\) /8p-,","del(8p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(7p\\) /7p-,","del(7p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(6p\\) /6p-,","del(6p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(5p\\) /5p-,","del(5p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(4p\\) /4p-,","del(4p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(3p\\) /3p-,","del(3p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(2p\\) /2p-,","del(2p)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(1p\\) /1p-,","del(1p)__",baz$CYTO_CLEAN)


### Monosomy

baz$CYTO_CLEAN<-gsub("-10 / monosomy 10,","mono(10)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(10\\),","mono(10)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 10,","mono(10)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 10","mono(10)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-10","mono(10)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-11 / monosomy 11,","mono(11)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(11\\),","mono(11)__",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("del\\(11\\),","mono(11)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 11,","mono(11)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 11","mono(11)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-11","mono(11)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-12 / monosomy 12,","mono(12)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(12\\),","mono(12)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 12,","mono(12)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 12","mono(12)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-12","mono(12)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-14 / monosomy 14,","mono(14)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(14\\),","mono(14)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 14,","mono(14)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 14","mono(14)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-14","mono(14)__",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("-16 / monosomy 16,","mono(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(16\\),","mono(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 16,","mono(16)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 16","mono(16)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-16","mono(16)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-18 / monosomy 18,","mono(18)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(18\\),","mono(18)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 18,","mono(18)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 18","mono(18)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-18","mono(18)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-19 / monosomy 19,","mono(19)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(19\\),","mono(19)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 19,","mono(19)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 19","mono(19)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-19","mono(19)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-20 / monosomy 20,","mono(20)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-20 / monosomy 20,","mono(20)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(20\\),","mono(20)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 20,","mono(20)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 20","mono(20)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-20","mono(20)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-21 / monosomy 21,","mono(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(21\\),","mono(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 21,","mono(21)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 21","mono(21)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-21","mono(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 21","mono(21)__",tolower(baz$CYTO_CLEAN))



baz$CYTO_CLEAN<-gsub("-22 / monosomy 22,","mono(22)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(22\\),","mono(22)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 22,","mono(22)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 22","mono(22)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-22","mono(22)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(Y\\),","del(Y)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-y","del(Y)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(X\\),","del(X)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-x","del(X)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-1 / monosomy 1,","mono(1)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(1\\),","mono(1)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 1,","mono(1)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 1","mono(1)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-1","mono(1)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-2 / monosomy 2,","mono(2)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(2\\),","mono(2)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 2,","mono(2)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 2","mono(2)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-2","mono(2)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-3 / monosomy 3,","mono(3)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("del\\(3\\),","mono(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 3,","mono(3)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 3","mono(3)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-3","mono(3)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-4 / monosomy 4,","mono(4)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(4\\),","mono(4)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 4,","mono(4)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 4","mono(4)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-4","mono(4)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-6 / monosomy 6,","mono(6)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(6\\),","mono(6)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 6,","mono(6)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 6","mono(6)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-6","mono(6)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-8 / monosomy 8,","mono(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(8\\),","mono(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 8,","mono(8)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 8","mono(8)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-8","mono(8)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("-9 / monosomy 9,","mono(9)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("del\\(9\\),","mono(9)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("monosomy 9,","mono(9)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("monosomy 9","mono(9)__",tolower(baz$CYTO_CLEAN))
baz$CYTO_CLEAN<-gsub("-9","mono(9)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("- y,","del(y)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("- y","del(y)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-y,","del(y)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("-y","del(y)__",baz$CYTO_CLEAN)



### Trisomy
baz$CYTO_CLEAN<-gsub("trisomy 10,","tri(10)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+10","tri(10)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 11,","tri(11)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+11","tri(11)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 12,","tri(12)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+12","tri(12)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 13,","tri(13)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+13","tri(13)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 14,","tri(14)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+14","tri(14)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 15,","tri(15)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+15","tri(15)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 16,","tri(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+16","tri(16)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 17,","tri(17)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+17","tri(17)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 18,","tri(18)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+18","tri(18)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 19,","tri(19)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+19","tri(19)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 20,","tri(20)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+20","tri(20)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 21,","tri(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+21","tri(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, tri\\(21\\)__","tri(21)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 22,","tri(22)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+22","tri(22)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, tri\\(22\\)__","tri(22)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 1,","tri(1)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+1","tri(1)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 2,","tri(2)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+2","tri(2)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 3,","tri(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+3","tri(3)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 4,","tri(4)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+4","tri(4)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 5,","tri(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+5","tri(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, trisomy 5","tri(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("trisomy 5","tri(5)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 6,","tri(6)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+6","tri(6)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, trisomy 6","tri(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("trisomy 8","tri(8)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 7,","tri(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+7","tri(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, trisomy 7","tri(7)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("trisomy 7","tri(7)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 8,","tri(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+8","tri(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, trisomy 8","tri(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("trisomy 8","tri(8)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("trisomy 9,","tri(9)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("\\+9","tri(9)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("other abnormality\\(ies\\),, trisomy 9","tri(9)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("trisomy 9","tri(9)__",baz$CYTO_CLEAN)


baz$CYTO_CLEAN<-gsub("trisomy,, +x","tri(x)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("xxx","tri(x)__",baz$CYTO_CLEAN)


### abn
baz$CYTO_CLEAN<-gsub("abn 10q,","abn(10q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 10,","abn(10)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 11q,","abn(11q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 11,","abn(11)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 12q,","abn(12q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 12,","abn(12)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 13q,","abn(13q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 13,","abn(13)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 14q,","abn(14q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 14,","abn(14)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 15q,","abn(15q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 15,","abn(15)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 16q,","abn(16q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 16,","abn(16)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 17q,","abn(17q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 17,","abn(17)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 17p,","abn(17p)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("abn 18q,","abn(18q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 18,","abn(18)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 19,","abn(19)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 19q,","abn(19q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 20,","abn(20)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 20q,","abn(20q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 21,","abn(21)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 21q,","abn(21q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 22,","abn(22)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 22q,","abn(22q)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("abn 1,","abn(1)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 1q,","abn(1q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 2,","abn(2)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 2q,","abn(2q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 3,","abn(3)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 3q,","abn(3q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 3q26 / evi1 rearrangement,","abn(3q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 3q","abn(3q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn3q","abn(3q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(3;9\\), \\(q12;p13\\)","t\\(3;9\\),\\(q12;p13\\)",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("t\\(3;5\\)\\(q21;q31\\)","t\\(3;5\\),\\(q21;p31\\)",baz$CYTO_CLEAN)



baz$CYTO_CLEAN<-gsub("abn 4,","abn(4)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 4q,","abn(4q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 5,","abn(5)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 5q,","abn(5q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 6q,","abn(6q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 6,","abn(6)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 7q,","abn(7q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 7,","abn(7)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN[which(baz$CYTO_CLEAN=="abn(7)__, monosomy")]<-"mono(7)__"
baz$CYTO_CLEAN[which(baz$CYTO_CLEAN=="abn(7)__, mono(7)__")]<-"mono(7)__"
baz$CYTO_CLEAN[which(baz$CYTO_CLEAN==", chromosome 7")]<-"abn(7)__"

baz$CYTO_CLEAN<-gsub("mono\\(7\\)__, mono\\(7\\)__","mono(7)__",baz$CYTO_CLEAN)

baz$CYTO_CLEAN<-gsub("abn 8q,","abn(8q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 8,","abn(8)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 9q,","abn(9q)__",baz$CYTO_CLEAN)
baz$CYTO_CLEAN<-gsub("abn 9,","abn(9)__",baz$CYTO_CLEAN)

baz$DELORMO7_JEG<-grepl("del\\(7\\)",baz$CYTO_CLEAN)
baz$DELORMO7_JEG[grepl("mono\\(7\\)",baz$CYTO_CLEAN)]<-TRUE
baz$DEL7Q_JEG<-grepl("del\\(7q\\)",baz$CYTO_CLEAN)
baz$DEL7Q_JEG<-grepl("del\\(7q",baz$CYTO_CLEAN)

baz$DEL7P_JEG<-grepl("del\\(7p\\)",baz$CYTO_CLEAN)

baz$DELORMONO7TOT_JEG<-ifelse(baz$DELORMO7_JEG+baz$DEL7Q_JEG+baz$DEL7P_JEG >0,"Yes","No")

baz$DELORMO5_JEG<-grepl("del\\(5\\)",baz$CYTO_CLEAN)
baz$DELORMO5_JEG[grepl("mono\\(5\\)",baz$CYTO_CLEAN)]<-TRUE
baz$DEL5Q_JEG<-grepl("del\\(5q\\)",baz$CYTO_CLEAN)
baz$DEL5P_JEG<-grepl("del\\(5p\\)",baz$CYTO_CLEAN)

baz$DELORMONO5TOT_JEG<-ifelse(baz$DELORMO5_JEG+baz$DEL5Q_JEG+baz$DEL5P_JEG >0,"Yes","No")

baz$INV16_JEG<-grepl("inv\\(16",baz$CYTO_CLEAN)
baz$INV16_JEG[grepl("t\\(16;16\\)\\(q13;1;q22\\)",baz$CYTO_CLEAN)]<-TRUE
baz$INV16_JEG[grepl("t\\(16;16\\)\\(q13;q22\\)",baz$CYTO_CLEAN)]<-TRUE
baz$INV16_JEG[grepl("t\\(16;16",baz$CYTO_CLEAN)]<-TRUE
baz$INV16_JEG[grepl("inv16",baz$CYTO_CLEAN)]<-TRUE

baz$T821_JEG<-grepl("t\\(8;21",baz$CYTO_CLEAN)
baz$T1517_JEG<-grepl("t\\(15;17",baz$CYTO_CLEAN)

baz$COMPLEX_JEG<-grepl("complex",baz$CYTO_CLEAN)
baz$MONOSOMAL_JEG<-grepl("monosomal",baz$CYTO_CLEAN)
baz$ABN3Q_JEG<-grepl("abn\\(3q",baz$CYTO_CLEAN)
baz$ABN3Q_JEG[grepl("del\\(3q",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("mono\\(3\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("del\\(3\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("inv\\(3",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("t\\(3;3\\)\\(q21;q26\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("t\\(3;9\\),\\(q12;p13\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("3q26",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("t\\(2;3\\)\\(p23;q26\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("t\\(2;3\\)\\(p22;q26\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN3Q_JEG[grepl("t\\(2;3\\)\\(p21;q26\\)",baz$CYTO_CLEAN)]<-TRUE

baz$ABN17P_JEG<-grepl("abn\\(17p",baz$CYTO_CLEAN)
baz$ABN17P_JEG[grepl("del\\(17p\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN17P_JEG[grepl("add\\(17p\\)",baz$CYTO_CLEAN)]<-TRUE
baz$ABN17P_JEG[grepl("add 17p",baz$CYTO_CLEAN)]<-TRUE
baz$ABN17P_JEG[grepl("add17p",baz$CYTO_CLEAN)]<-TRUE
baz$ABN17P_JEG[grepl("add\\(17\\)p",baz$CYTO_CLEAN)]<-TRUE
baz$ABN17P_JEG[grepl("add\\(17\\)\\(p",baz$CYTO_CLEAN)]<-TRUE
baz$ABN17P_JEG[grepl("17p",baz$CYTO_CLEAN)]<-TRUE

baz$T922_JEG<-grepl("t\\(9;22",baz$CYTO_CLEAN)
baz$T69_JEG<-grepl("t\\(6;9",baz$CYTO_CLEAN)
baz$DEL17_JEG<-grepl("mono\\(17\\)",baz$CYTO_CLEAN)

baz$T411Q23_JEG<-grepl("t\\(4;11\\)\\(q21;q23\\)",baz$CYTO_CLEAN)
baz$T611Q23_JEG<-grepl("t\\(6;11\\)\\(q27;q23\\)",baz$CYTO_CLEAN)
baz$T611Q23_JEG[grepl("t\\(6;11\\)\\(q25_27;q23\\)",baz$CYTO_CLEAN)]<-"TRUE"


baz$T911Q23_JEG<-grepl("t\\(9;11\\)\\(p22;q23\\)",baz$CYTO_CLEAN)
baz$T1011Q23_JEG<-grepl("t\\(10;11\\)\\(p11;q23",baz$CYTO_CLEAN)
baz$T1011Q23_JEG[grepl("t\\(10;11\\)\\(q22;q23",baz$CYTO_CLEAN)==TRUE]<-TRUE
baz$T1116Q23_JEG<-grepl("t\\(11;16\\)\\(q23",baz$CYTO_CLEAN)
baz$T1117Q23_JEG<-grepl("t\\(11;17\\)\\(q23",baz$CYTO_CLEAN)
baz$T1114Q23_JEG<-grepl("t\\(11;14\\)\\(q23",baz$CYTO_CLEAN)
baz$T1119Q23_JEG<-grepl("t\\(11;19\\)\\(q23",baz$CYTO_CLEAN)
baz$DEL11Q23_JEG<-grepl("del\\(11q23",baz$CYTO_CLEAN)


###
baz$MRC1_JEG<-NA
baz$MRC1_JEG[which(baz$COMPLEX_JEG==TRUE)]<-"Poor"
baz$MRC1_JEG[which(baz$DELORMO7_JEG==TRUE)]<-"Poor"
baz$MRC1_JEG[which(baz$DELORMO5_JEG==TRUE)]<-"Poor"
baz$MRC1_JEG[which(baz$DEL5Q_JEG==TRUE)]<-"Poor"
baz$MRC1_JEG[which(baz$ABN3Q_JEG==TRUE)]<-"Poor"

baz$MRC1_JEG[which(baz$VCHROMOS=="Normal" & is.na(baz$CYTO_CLEAN))]<-"Intermediate"
baz$MRC1_JEG[which(!is.na(baz$CYTOANO) & is.na(baz$MRC1_JEG))]<-"Intermediate"

baz$MRC1_JEG[which(baz$INV16_JEG==TRUE)]<-"Good"
baz$MRC1_JEG[which(baz$T821_JEG==TRUE)]<-"Good"
baz$MRC1_JEG[which(baz$T1517_JEG==TRUE)]<-"Good"

baz$CYTOAML_ML<-NA

baz$CYTOAML_ML[which(baz$VCHROMOS=="Normal" & is.na(baz$CYTO_CLEAN))]<-"Intermediate"
baz$CYTOAML_ML[which(!is.na(baz$CYTOANO) & is.na(baz$CYTOAML_ML))]<-"Intermediate"


baz$CYTOAML_ML[which(baz$COMPLEX_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$DELORMONO7TOT_JEG=="Yes")]<-"Poor"
baz$CYTOAML_ML[which(baz$DELORMONO5TOT_JEG=="Yes")]<-"Poor"
baz$CYTOAML_ML[which(baz$DEL5Q_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$ABN3Q_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$DEL7Q_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$MONOSOMAL_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T922_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T69_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$DEL17_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$ABN17P_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T411Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T611Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T1011Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T1114Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T1116Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T1117Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$T1119Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_ML[which(baz$DEL11Q23_JEG==TRUE)]<-"Poor"



baz$CYTOAML_ML[which(baz$WHOAML %in% c("AML with t(6;9)(p23;q24); DEK-NUP214",
                                             "AML with inv(3)(q21q26.2) or t(3;3)(q21;q26.2); RPN1-EVI1",
                                             "Acute promyelocytic leukaemia with t(15;17)(q22;q12); PML-RARA"))]<-"Poor"

baz$CYTOAML_ML[which(baz$INV16_JEG==TRUE)]<-"Good"
baz$CYTOAML_ML[which(baz$T821_JEG==TRUE)]<-"Good"
baz$CYTOAML_ML[which(baz$T1517_JEG==TRUE)]<-"Good"
baz$CYTOAML_ML[which(baz$WHOAML %in% c("AML w t(8;21)(q22;q22); RUNX1-RUNX1T1",
                                             "AML with inv(16)(p13.1q22) or t(16;16)(p13.1;q22); CBFB-MYH11",
                                             "Acute promyelocytic leukaemia with t(15;17)(q22;q12); PML-RARA"))]<-"Good"



#####
baz$CYTOAML_MLHYPO<-NA

baz$CYTOAML_MLHYPO[which(baz$VCHROMOS=="Normal" & is.na(baz$CYTO_CLEAN))]<-"Intermediate"
baz$CYTOAML_MLHYPO[which(!is.na(baz$CYTOANO) & is.na(baz$CYTOAML_MLHYPO))]<-"Intermediate"

baz$CYTOAML_MLHYPO[which(baz$WHOAML=="AML with 11q23 (MLL) abnormalities")]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("mll",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("11q23",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("11q23",baz$MOLECABNOR)==TRUE)]<-"Poor"

baz$CYTOAML_MLHYPO[which(grepl("\\(9;11",baz$CYTO_CLEAN)==TRUE)]<-"Intermediate"
baz$CYTOAML_MLHYPO[which(baz$WHOAML=="AML with t(9;11)(p22;q23); MLLT 3-MLL ")]<-"Intermediate"

baz$CYTOAML_MLHYPO[which(baz$COMPLEX_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$DELORMONO7TOT_JEG=="Yes")]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$DELORMONO5TOT_JEG=="Yes")]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$DEL5Q_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$ABN3Q_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$DEL7Q_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$MONOSOMAL_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T922_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T69_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$DEL17_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$ABN17P_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T411Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T611Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T1011Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T1114Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T1116Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T1117Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$T1119Q23_JEG==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(baz$DEL11Q23_JEG==TRUE)]<-"Poor"

baz$CYTOAML_MLHYPO[which(baz$WHOAML %in% c("AML with t(6;9)(p23;q24); DEK-NUP214",
                                       "AML with inv(3)(q21q26.2) or t(3;3)(q21;q26.2); RPN1-EVI1",
                                       "Acute promyelocytic leukaemia with t(15;17)(q22;q12); PML-RARA"))]<-"Poor"

baz$CYTOAML_MLHYPO[which(grepl("\\(4;11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(4,11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(5,11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(5;11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(6;11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(6,11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(7,11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(7;11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(8,11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(8;11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(10;11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(10,11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11;14",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11,14",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11;15",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11;16",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11,16",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11;17",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11,17",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11;19",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11,19",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("\\(11;21",baz$CYTO_CLEAN)==TRUE)]<-"Poor"
baz$CYTOAML_MLHYPO[which(grepl("del\\(11",baz$CYTO_CLEAN)==TRUE)]<-"Poor"

baz$CYTOAML_MLHYPO[which(grepl("evi1",baz$CYTO_CLEAN)==TRUE)]<-"Poor"

baz$CYTOAML_MLHYPO[which(baz$INV16_JEG==TRUE)]<-"Good"
baz$CYTOAML_MLHYPO[which(baz$T821_JEG==TRUE)]<-"Good"
baz$CYTOAML_MLHYPO[which(baz$T1517_JEG==TRUE)]<-"Good"
baz$CYTOAML_MLHYPO[which(baz$WHOAML %in% c("AML w t(8;21)(q22;q22); RUNX1-RUNX1T1",
                                           "AML with inv(16)(p13.1q22) or t(16;16)(p13.1;q22); CBFB-MYH11",
                                           "Acute promyelocytic leukaemia with t(15;17)(q22;q12); PML-RARA"))]<-"Good"
  
  
baz$CYTOMDS<-NA
## very good: -Y or del(11q) alone, 
baz$CYTOMDS[which(baz$CYTO_CLEAN=="del(y)__")]<-"Very good"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="del(11q)__")]<-"Very good"
## good: Normal, del(5q), del(12p), del(20q), double including del(5q)
baz$CYTOMDS[which(baz$VCHROMOS=="Normal")]<-"Good"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="mono(5)__")]<-"Good"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="del(5q)__")]<-"Good"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="del(5q)__")]<-"Good"

## intermediate: del(7q), +8, +19, i(17q), any other single or double independent clones
baz$CYTOMDS[which(baz$CYTO_CLEAN=="del(7q)__")]<-"Intermediate"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="tri(8)__del(7q)__")]<-"Intermediate"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="tri(8)__,del(7q)__")]<-"Intermediate"

baz$CYTOMDS[which(baz$CYTO_CLEAN=="tri(8)__")]<-"Intermediate"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="tri(21)__")]<-"Intermediate"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="tri(7)__")]<-"Intermediate"
baz$CYTOMDS[which(baz$CYTO_CLEAN=="mono(7)__")]<-"Intermediate"

## poor: -7, inv(3)/t(3q)/del(3q), double including -7/del(7q), Complex: 3 abnormalities
baz$CYTOMDS[grepl("mono\\(7\\)__",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("del\\(7\\)__",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("abn\\(7\\)__",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("complex",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("del\\(3q\\)__",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("inv\\(3__",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("t\\(3;5\\)\\(q",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[grepl("t\\(3;5\\),\\(q",baz$CYTO_CLEAN)==TRUE]<-"Poor / Very Poor"
baz$CYTOMDS[which(baz$ABN3Q_JEG==TRUE)]<-"Poor / Very Poor"
baz$CYTOMDS[is.na(baz$CYTOMDS) & !is.na(baz$CYTO_CLEAN)]<-"Intermediate"

## very poor: Complex: >3 abnormalities

baz$CYTOMDS_DRI<-baz$CYTOMDS
baz$CYTOMDS_DRI[is.na(baz$CYTOMDS)]<-"Intermediate"

baz$CYTOAML_MLHYPO_DRI<-baz$CYTOAML_MLHYPO
baz$CYTOAML_MLHYPO_DRI[is.na(baz$CYTOAML_MLHYPO)]<-"Intermediate"

## Disease spe

baz$DISEASE_spe<-baz$DISEASE
baz$DISEASE_spe[which(baz$DISEASE=="ALL" & baz$BMTDISESTAJEG2022 =="CR1")]<-"ALL CR1"
baz$DISEASE_spe[which(baz$DISEASE=="ALL" & baz$BMTDISESTAJEG2022 =="CR2")]<-"ALL CR2"
baz$DISEASE_spe[which(baz$DISEASE=="ALL" & baz$BMTDISESTAJEG2022 =="CR>=3")]<-"ALL CR>=3"
baz$DISEASE_spe[which(baz$DISEASE=="ALL" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Blast crisis",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase",
                                                                        "Stable disease",
                                                                        "Chronic phase"))]<-"ALL active"

baz$DISEASE_spe[which(baz$DISEASE=="ALL" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"ALL (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="ALL" & baz$BMTDISESTAJEG2022 %in% c("CR (missing)"))]<-"ALL CR(missing nbr)"
baz$DISEASE_spe[which(baz$DISEASE=="ALL" & is.na(baz$BMTDISESTAJEG2022))]<-"ALL (missing DS)"

baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTA =="Complete remission (CR)" & baz$CYTOAML_MLHYPO_DRI=="Good")]<-"AML CR Good"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTA =="Complete remission (CR)" & baz$CYTOAML_MLHYPO_DRI=="Intermediate")]<-"AML CR Intermediate"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$CYTOAML_MLHYPO_DRI))]<-"AML CR Missing"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTA =="Complete remission (CR)" & baz$CYTOAML_MLHYPO_DRI=="Poor")]<-"AML CR Poor"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & baz$CYTOAML_MLHYPO_DRI=="Good")]<-"AML active Good"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & baz$CYTOAML_MLHYPO_DRI=="Intermediate")]<-"AML active Intermediate"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & is.na(baz$CYTOAML_MLHYPO_DRI))]<-"AML active Missing"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & baz$CYTOAML_MLHYPO_DRI=="Poor")]<-"AML active Poor"

baz$DISEASE_spe[which(baz$DISEASE=="AML" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"AML (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="AML" & is.na(baz$BMTDISESTAJEG2022))]<-"AML (missing DS)"


baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTA =="Complete remission (CR)" & baz$CYTOAML_MLHYPO_DRI=="Good")]<-"MPAL CR Good"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTA =="Complete remission (CR)" & baz$CYTOAML_MLHYPO_DRI=="Intermediate")]<-"MPAL CR Intermediate"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$CYTOAML_MLHYPO_DRI))]<-"MPAL CR Missing"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTA =="Complete remission (CR)" & baz$CYTOAML_MLHYPO_DRI=="Poor")]<-"MPAL CR Poor"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & baz$CYTOAML_MLHYPO_DRI=="Good")]<-"MPAL active Good"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & baz$CYTOAML_MLHYPO_DRI=="Intermediate")]<-"MPAL active Intermediate"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & is.na(baz$CYTOAML_MLHYPO_DRI))]<-"MPAL active Missing"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR",
                                                                        "PR","Upfront","Treatment not aimed at remission",
                                                                        "Primary induct fail",
                                                                        "Rel/Prog","Stable disease","Upfront",
                                                                        "Response / Improvement (no CR)",
                                                                        "Accelerated phase","Blast crisis",
                                                                        "Chronic phase") & baz$CYTOAML_MLHYPO_DRI=="Poor")]<-"MPAL active Poor"

baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"MPAL (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="MPAL" & is.na(baz$BMTDISESTAJEG2022))]<-"MPAL (missing DS)"




baz$DISEASE_spe[which(baz$DISEASE=="Hodgkins" & baz$BMTDISESTA =="Complete remission (CR)")]<-"Hogkin CR"
baz$DISEASE_spe[which(baz$DISEASE=="Hodgkins" & baz$BMTDISESTAJEG2022 =="PR")]<-"Hogkin PR"
baz$DISEASE_spe[which(baz$DISEASE=="Hodgkins" & baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease"))]<-"Hogkin active"

baz$DISEASE_spe[which(baz$DISEASE=="Hodgkins" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"Hodgkins (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="Hodgkins" & is.na(baz$BMTDISESTAJEG2022))]<-"Hodgkins (missing DS)"

baz$DISEASE_spe[which(baz$DISEASE=="Hodgkins" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR","Chronic phase",
                                                                             "Response / Improvement (no CR)","Upfront"))]<-"Hogkin (not clear DS)"


baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Mantle cell lymphoma" & 
                        baz$BMTDISESTA =="Complete remission (CR)")]<-"Mantle cell CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Mantle cell lymphoma" & 
                        baz$BMTDISESTAJEG2022 =="PR")]<-"Mantle cell PR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Mantle cell lymphoma" &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease"))]<-"Mantle cell active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Mantle cell lymphoma" &
                        baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"Mantle cell (DS: Other)"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Mantle cell lymphoma" &
                        is.na(baz$BMTDISESTAJEG2022))]<-"Mantle cell (missing DS)"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Burkitt lymphoma (BL)" & 
                        baz$BMTDISESTA =="Complete remission (CR)")]<-"BL CR"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Burkitt lymphoma (BL)" & 
                        baz$BMTDISESTAJEG2022 =="PR")]<-"BL PR"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Burkitt lymphoma (BL)" &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease"))]<-"BL active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Burkitt lymphoma (BL)" &
                        baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"BL (DS: Other)"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$WHOLYCLS_DIAG== "Burkitt lymphoma (BL)" &
                        is.na(baz$BMTDISESTAJEG2022))]<-"BL (missing DS)"

baz$DRI<-NA
baz$DRI[which(baz$DISEASE_spe=="ALL CR1")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="ALL CR2")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="ALL CR>=3")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="ALL active")]<-"Very high"

baz$DRI[which(baz$DISEASE_spe=="AML CR Good")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="AML CR Intermediate")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="AML CR Missing")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="AML CR Poor")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="AML active Good")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="AML active Intermediate")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="AML active Missing")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="AML active Poor")]<-"Very high"

baz$DRI[which(baz$DISEASE_spe=="MPAL CR Good")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="MPAL CR Intermediate")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="MPAL CR Missing")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="MPAL CR Poor")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="MPAL active Good")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="MPAL active Intermediate")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="MPAL active Missing")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="MPAL active Poor")]<-"Very high"

baz$DRI[which(baz$DISEASE_spe=="Hogkin CR")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="Hogkin PR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="Hogkin active")]<-"High"

baz$DRI[which(baz$DISEASE=="MPN")]<-"Int"
baz$DRI[which(baz$DISEASE=="MDS & MPN")]<-"Int"
baz$DRI[which(baz$DISEASE=="JMML")]<-"Int"
baz$DRI[which(baz$DISEASE=="MDS or MPN")]<-"Int"
baz$DRI[which(baz$DISEASE=="Atypical CML")]<-"Int"
baz$DRI[which(baz$DISEASE=="CMML")]<-"Int"

baz$DRI[which(baz$DISEASE_spe=="Mantle cell CR")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="Mantle cell PR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="Mantle cell active")]<-"High"

baz$DRI[which(baz$DISEASE_spe=="BL CR")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="BL PR")]<-"Very high"
baz$DRI[which(baz$DISEASE_spe=="BL active")]<-"Very high"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("T/NK-cell Indolent","T-cell undertermined","T/NK-cell Aggressive") &
                        baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$DRI))]<-"NHL TCELL CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("T/NK-cell Indolent","T-cell undertermined","T/NK-cell Aggressive") &
                        baz$BMTDISESTAJEG2022 =="PR" & is.na(baz$DRI))]<-"NHL TCELL PR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("T/NK-cell Indolent","T-cell undertermined","T/NK-cell Aggressive") &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease") & is.na(baz$DRI))]<-"NHL TCELL active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell Indolent") &
                        baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$DRI))]<-"NHL BCELL Indolent CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell Indolent") &
                        baz$BMTDISESTAJEG2022 =="PR" & is.na(baz$DRI))]<-"NHL BCELL Indolent PR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell Indolent") &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease") & is.na(baz$DRI))]<-"NHL BCELL Indolent active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell Aggressive") &
                        baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$DRI))]<-"NHL BCELL Aggressive CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell Aggressive") &
                        baz$BMTDISESTAJEG2022 =="PR" & is.na(baz$DRI))]<-"NHL BCELL Aggressive PR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell Aggressive") &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease") & is.na(baz$DRI))]<-"NHL BCELL Aggressive active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell undertermined") &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease") & is.na(baz$DRI))]<-"NHL BCELL undertermined active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell undertermined") &
                        baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$DRI))]<-"NHL BCELL undertermined CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("B-cell undertermined") &
                        baz$BMTDISESTAJEG2022 =="PR" & is.na(baz$DRI))]<-"NHL BCELL undertermined PR"


baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("T-cell undertermined") &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease") & is.na(baz$DRI))]<-"NHL TCELL undertermined active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("T-cell undertermined") &
                        baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$DRI))]<-"NHL TCELL undertermined CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("T-cell undertermined") &
                        baz$BMTDISESTAJEG2022 =="PR" & is.na(baz$DRI))]<-"NHL TCELL undertermined PR"


baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("Other NHL or NOS") &
                        baz$BMTDISESTAJEG2022 %in% c("Primary induct fail","Rel/Prog","Stable disease") & is.na(baz$DRI))]<-"Other NHL or NOS active"

baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("Other NHL or NOS") &
                        baz$BMTDISESTA =="Complete remission (CR)" & is.na(baz$DRI))]<-"Other NHL or NOS CR"
baz$DISEASE_spe[which(baz$DISEASE=="NHL" & 
                        baz$NHLSUBTYP %in% c("Other NHL or NOS") &
                        baz$BMTDISESTAJEG2022 =="PR" & is.na(baz$DRI))]<-"Other NHL or NOS PR"

baz$DISEASE_spe[which(baz$DISEASE_spe=="NHL" & baz$WHOLYCLS_DIAG=="Working formulation")]<-"NHL unknown type"

#### T cell NHL
baz$DRI[which(baz$DISEASE_spe=="NHL TCELL CR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="NHL TCELL PR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="NHL TCELL active")]<-"High"

#### B-cell
baz$DRI[which(baz$DISEASE_spe=="NHL BCELL Indolent CR")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="NHL BCELL Indolent PR")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="NHL BCELL Indolent active")]<-"Int"

baz$DRI[which(baz$DISEASE_spe=="NHL BCELL Aggressive CR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="NHL BCELL Aggressive PR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="NHL BCELL Aggressive active")]<-"Very high"



baz$DISEASE_spe[which(baz$DISEASE=="MDS" & 
                        baz$MDSRISK=="Low" &
                        baz$BMTDISESTAJEG2022 %in% c("CR (missing)","CR>=3","CR1","CR2","Upfront","Chronic phase") & is.na(baz$DRI))]<-"MDS Low CR/Upfront"

baz$DISEASE_spe[which(baz$DISEASE=="MDS" & baz$MDSRISK=="Low" &
                        baz$CYTOMDS_DRI %in% c("Good","Very good","Intermediate") &
                        baz$BMTDISESTAJEG2022 %in% c("Never/not in CR","PR","Primary induct fail","Rel/Prog","Treatment not aimed at remission",
                                                     "Response / Improvement (no CR)","Stable disease") & is.na(baz$DRI))]<-"MDS Low Int/G/VG active"
baz$DISEASE_spe[which(baz$DISEASE=="MDS" & baz$MDSRISK=="Low" &
                        baz$CYTOMDS_DRI %in% c("Poor / Very Poor") &
                        baz$BMTDISESTAJEG2022 %in% c("Never/not in CR","PR","Primary induct fail","Rel/Prog","Treatment not aimed at remission",
                                                     "Response / Improvement (no CR)","Stable disease") & is.na(baz$DRI))]<-"MDS Low P/VP active"

baz$DISEASE_spe[which(baz$DISEASE=="MDS" & baz$MDSRISK=="High" &
                        baz$BMTDISESTAJEG2022 %in% c("Never/not in CR","PR","Primary induct fail","Rel/Prog","Treatment not aimed at remission",
                                                     "Response / Improvement (no CR)","Stable disease") & is.na(baz$DRI))]<-"MDS High active"

baz$DISEASE_spe[which(baz$DISEASE=="MDS" & baz$MDSRISK=="High" &
                        baz$CYTOMDS_DRI %in% c("Good","Very good","Intermediate") &
                        baz$BMTDISESTAJEG2022 %in% c("CR (missing)","CR>=3","CR1","CR2","Upfront","Chronic phase"))]<-"MDS High Int/G/VG CR/Upfront"

baz$DISEASE_spe[which(baz$DISEASE=="MDS" & baz$MDSRISK=="High" &
                        baz$CYTOMDS_DRI %in% c("Poor / Very Poor") &
                        baz$BMTDISESTAJEG2022 %in% c("CR (missing)","CR>=3","CR1","CR2","Upfront","Chronic phase") & is.na(baz$DRI))]<-"MDS High P/VP CR/Upfront"

baz$DISEASE_spe[which(baz$DISEASE=="MDS" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"MDS (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="MDS" & is.na(baz$BMTDISESTAJEG2022))]<-"MDS (missing DS)"



# MDS
baz$DRI[which(baz$DISEASE_spe=="MDS Low CR/Upfront")]<-"Int"

baz$DRI[which(baz$DISEASE_spe=="MDS Low Int/G/VG active")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="MDS Low P/VP active")]<-"High"


baz$DRI[which(baz$DISEASE_spe=="MDS High active")]<-"High"
baz$DRI[which(baz$DISEASE_spe=="MDS High Int/G/VG CR/Upfront")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="MDS High P/VP CR/Upfront")]<-"High"

baz$DISEASE_spe[which(baz$DISEASE=="CLL" & baz$BMTDISESTA =="Complete remission (CR)")]<-"CLL CR"
baz$DISEASE_spe[which(baz$DISEASE=="CLL" & baz$BMTDISESTAJEG2022 =="PR")]<-"CLL PR"
baz$DISEASE_spe[which(baz$DISEASE=="CLL" & baz$BMTDISESTAJEG2022 %in% c("Primary induct fail",
                                                                        "Rel/Prog","Stable disease"))]<-"CLL active"

baz$DISEASE_spe[which(baz$DISEASE=="CLL" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"CLL (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="CLL" & is.na(baz$BMTDISESTAJEG2022))]<-"CLL (missing DS)"


baz$DISEASE_spe[which(baz$DISEASE=="CML" & baz$BMTDISESTAJEG2022 %in% c("Chronic phase","CR2","CR1","CR>=3","CR (missing)"))]<-"CML CR/CP"
baz$DISEASE_spe[which(baz$DISEASE=="CML" & baz$BMTDISESTAJEG2022 %in% c("Accelerated phase"))]<-"CML Acc Phase"
baz$DISEASE_spe[which(baz$DISEASE=="CML" & baz$BMTDISESTAJEG2022 %in% c("Blast crisis"))]<-"CML Blast Crisis"

baz$DISEASE_spe[which(baz$DISEASE=="CML" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"CML (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="CML" & is.na(baz$BMTDISESTAJEG2022))]<-"CML (missing DS)"


baz$DISEASE_spe[which(baz$DISEASE=="MM" & baz$BMTDISESTAJEG2022 %in% c("sCR1","sCR","CR>=3","CR1","CR2","CR (missing)","PR"))]<-"MM CR/PR"
baz$DISEASE_spe[which(baz$DISEASE=="MM" & baz$BMTDISESTAJEG2022 %in% c("Rel/Prog","Stable disease","Primary induct fail"))]<-"MM active"

baz$DISEASE_spe[which(baz$DISEASE=="MM" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"MM (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="MM" & is.na(baz$BMTDISESTAJEG2022))]<-"MM (missing DS)"


baz$DISEASE_spe[which(baz$DISEASE=="MM" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR","Chronic phase",
                                                                             "Response / Improvement (no CR)","Upfront"))]<-"MM (not clear DS)"



baz$DISEASE_spe[which(baz$DISEASE=="PCL" & baz$BMTDISESTAJEG2022 %in% c("sCR1","sCR","CR>=3","CR1","CR2","CR (missing)","PR"))]<-"PCL CR/PR"
baz$DISEASE_spe[which(baz$DISEASE=="PCL" & baz$BMTDISESTAJEG2022 %in% c("Rel/Prog","Stable disease","Primary induct fail"))]<-"PCL active"

baz$DISEASE_spe[which(baz$DISEASE=="PCL" & baz$BMTDISESTAJEG2022 %in% c("Other"))]<-"PCL (DS: other)"
baz$DISEASE_spe[which(baz$DISEASE=="PCL" & is.na(baz$BMTDISESTAJEG2022))]<-"PCL (missing DS)"


baz$DISEASE_spe[which(baz$DISEASE=="PCL" & baz$BMTDISESTAJEG2022 %in% c("Never/not in CR","Chronic phase",
                                                                       "Response / Improvement (no CR)","Upfront"))]<-"PCL (not clear DS)"


# CLL
baz$DRI[which(baz$DISEASE_spe=="CLL CR")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="CLL PR")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="CLL active")]<-"Int"

baz$DRI[which(baz$DISEASE_spe=="CML CR/CP")]<-"Low"
baz$DRI[which(baz$DISEASE_spe=="CML Acc Phase")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="CML Blast Crisis")]<-"Very high"

baz$DRI[which(baz$DISEASE_spe=="MM CR/PR")]<-"Int"
baz$DRI[which(baz$DISEASE_spe=="MM active")]<-"High"



return(baz)
}


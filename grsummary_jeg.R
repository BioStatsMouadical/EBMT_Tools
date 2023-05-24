
require(stringr)

########################################
# Fonction issue du package "questionr"
rename.variable<-function (df, old, new){names(df)[which(names(df) == old)] <- new
  df
  }
##########################################

# DESCRIPTIVE ANALYSIS
grsummary <- function (dat, nomvarcont=NULL, nomvarcat=NULL, nomvargroup=NULL,testcat=NULL) 
{
  nbmissgr <- function(var) {sum(is.na(var))}
  nbnonmissgr <- function(var) {sum(!is.na(var))}
  
  ##########################################################
  ##### La variable "nomvargroup" doit être codé en facteur#
  ##########################################################
  
  dat<-droplevels(dat)
  
  if (class(dat[,nomvargroup])== "logical" ){dat[,nomvargroup]<-as.factor(as.numeric(dat[,nomvargroup]))}
  if (class(dat[,nomvargroup])!= "factor" ){dat[,nomvargroup]<-as.factor(as.character(dat[,nomvargroup]))}

  if (class(dat[,nomvargroup])== "factor" ){dat[,nomvargroup]<-factor(dat[,nomvargroup],sort(levels(dat[,nomvargroup])))}
  
  for(i in nomvarcat){
    # rangement des labels par ordre alphabetique
    if (class(dat[,i])== "logical" ){dat[,i]<-as.factor(as.character(dat[,i]))}
    if (class(dat[,i])== "character" ){dat[,i]<-as.factor(as.character(dat[,i]))}
    if (class(dat[,i])!= "factor" ){dat[,i]<-as.factor(as.character(dat[,i]))}
    
    if (class(dat[,i])== "factor" ){dat[,i]<-factor(dat[,i],sort(levels(dat[,i])))}
  }

  ##### Warnings ######
  if (is.null(nomvarcont) & is.null(nomvarcat)) {
    stop("At least one variable must be specified")
  }
  
  cont_ok<-NULL
  
  for(i in 1:length(nomvarcont)){
    if(nbmissgr(dat[, names(dat) == nomvarcont[i]])==nrow(dat)){
      print(paste("La variable",nomvarcont[i],"est vide."))
    }else{
      cont_ok<- c(cont_ok,nomvarcont[i])
    }
  }
  
  nomvarcont<-cont_ok

  for(i in nomvarcont) dat[,i]<-as.numeric(as.character(dat[,i]))
  
  cat_ok<-NULL
  
  for(i in 1:length(nomvarcat)){
    if(nbmissgr(dat[, names(dat) == nomvarcat[i]])==nrow(dat)){
      print(paste("La variable",nomvarcat[i],"est vide."))
    }else{
      cat_ok<- c(cat_ok,nomvarcat[i])
    }
  }
  
  nomvarcat<-cat_ok

  ###################################################
  ### Code en absence de variable "nomvargroup" #####
  ###################################################

  if (is.null(nomvargroup)){
    
    som = nrow(dat)
    if (is.null(nomvarcont) == FALSE & sum(nomvarcont %in%  names(dat)) == length(nomvarcont)) {
    
      for(i in 1:length(nomvarcont)) {
        moy = mean(dat[, names(dat) == nomvarcont[i]],  na.rm = TRUE)
        mini = suppressWarnings(min(dat[, names(dat) == nomvarcont[i]],na.rm = TRUE))
        Q1 = quantile(dat[, names(dat) == nomvarcont[i]], probs = 0.25, na.rm = TRUE)
        medi = median(dat[, names(dat) == nomvarcont[i]], na.rm = TRUE)
        Q3 = quantile(dat[, names(dat) == nomvarcont[i]], probs = 0.75, na.rm = TRUE)
        maxi = suppressWarnings(max(dat[, names(dat) == nomvarcont[i]], na.rm = TRUE))
        mat = sum(is.na(dat[, names(dat) == nomvarcont[i]]))
        mat = cbind(paste(round(medi, 2), " (", round(mini, 1),"-",round(maxi, 1),") "," [",round(Q1, 1), "-", round(Q3, 1),"] ", sep = ""),mat)
        mat = as.data.frame(mat)
        names(mat) = c(paste(nomvarcont[i], "median (range)[IQR]",sep = "____"), paste(nomvarcont[i], "missing", sep = "____"))
        
        if (i > 1) {
          toutcont = cbind(toutcont, mat)
        }
        else {
          mat = cbind(N = som, mat)
          toutcont = mat
        }
      }
    }
    else {
      if (sum(nomvarcont %in% names(dat)) != length(nomvarcont)) {
        print(paste("Undefined variable: ", nomvarcont[which(nomvarcont %in%  names(dat) == FALSE)], sep = ""))
      }
      else {
        print("No continuous variable defined")
      }
    }
 
    if (is.null(nomvarcat) == FALSE & sum(nomvarcat %in% names(dat)) == length(nomvarcat)) {

      for(i in 1:length(nomvarcat)) {
        matcat = as.matrix(t(table(dat[, names(dat) == nomvarcat[i]])))
        nonmissg = nbnonmissgr(dat[, names(dat) == nomvarcat[i]])
        missg = nbmissgr(dat[, names(dat) == nomvarcat[i]])
        matsum = matrix(paste(round(matcat,0)," (",round(matcat/nonmissg *100, 1), ")", sep = ""), 
                        ncol = ncol(matcat), nrow = nrow(matcat))
        nclas=nlevels(dat[,which(names(dat)==nomvarcat[i])])
        for(j in 1:nclas)   {colnames(matcat)[j]=colnames(matcat)[j]}  
        colnames(matsum) = colnames(matcat)
        matcat1 = cbind(matsum,missing=missg)
        colnames(matcat1)<-paste(nomvarcat[i],colnames(matcat1),sep="____")

        if (i > 1) {
          toutcat = cbind(toutcat, matcat1)
        }
        else {
          toutcat = matcat1
        }
      }
    }
    else {
      if (sum(nomvarcat %in% names(dat)) != length(nomvarcat)) {
        print(paste("Undefined variable: ", nomvarcat[which(nomvarcat %in% 
                                                              names(dat) == FALSE)], sep = ""))
      }
      else {
        print("No categorical variable defined")
      }
    }

    # Uniquement des variables quali
    if (is.null(nomvarcont) == T & is.null(nomvarcat) ==  F) {
      res<- t(toutcat)
    }
    
    # Uniquement des variables quanti
    if (is.null(nomvarcat) == T & is.null(nomvarcont) ==  F) {
      res<-t(toutcont)
    }
    
    ### Des variables quanti et quali
    
    if (is.null(nomvarcat) == F & is.null(nomvarcont) ==  F) {
      res<-t(cbind(toutcont, toutcat))
    }
    
    ### Sortie du resultat final
    res<-cbind(NA,res)
    colnames(res)<-c("Stat or level","Values")
    for (i in 1:dim(res)[1]){
      res[i,1]<-str_split(row.names(res)[i],"____")[[1]][2]
      rownames(res)[i]<-str_split(row.names(res)[i],"____")[[1]][1]
    }
    rownames(res)[duplicated(rownames(res))]<-""
    res
  }
  
  ###################################################
  ### Code en présence de variable "nomvargroup" ####
  ###################################################
  else {
    som = aggregate(rep(1, nrow(dat)), list(group = dat[,names(dat) == nomvargroup]), sum)
    names(som)[2] = "N"
    
    ### Code en présence de "nomvarcont"
    if (is.null(nomvarcont) == FALSE & sum(nomvarcont %in%  names(dat)) == length(nomvarcont)) {
      i=1
      while (i < length(nomvarcont) + 1) {
        moy = aggregate(dat[, names(dat) == nomvarcont[i]], list(group = dat[, names(dat) == nomvargroup]), mean, na.rm = TRUE)
        mini = suppressWarnings(aggregate(dat[, names(dat) == nomvarcont[i]], list(group = dat[, names(dat) == nomvargroup]), min, na.rm = TRUE))
        Q1 = aggregate(dat[, names(dat) == nomvarcont[i]], list(group = dat[, names(dat) == nomvargroup]),quantile, probs = 0.25, na.rm = TRUE)
        medi = aggregate(dat[, names(dat) == nomvarcont[i]],list(group = dat[, names(dat) == nomvargroup]),median, na.rm = TRUE)
        Q3 = aggregate(dat[, names(dat) == nomvarcont[i]],list(group = dat[, names(dat) == nomvargroup]),  quantile, probs = 0.75, na.rm = TRUE)
        maxi = suppressWarnings(aggregate(dat[, names(dat) == nomvarcont[i]],list(group = dat[, names(dat) == nomvargroup]), max, na.rm = TRUE))
        mat = aggregate(dat[, names(dat) == nomvarcont[i]], list(group = dat[, names(dat) == nomvargroup]), nbmissgr)
        summa = cbind(group = levels(medi$group), s = paste(round(medi$x, 1), " (", round(mini$x, 1), "-", round(maxi$x,1),") ", " [",round(Q1$x,1), "-", round(Q3$x,1), "] ", sep = ""))
        ###
        mat = merge(summa, by = "group",mat)
        ###
        nclas=nlevels(dat[,which(names(dat)==nomvarcat[i])])
        ###
        names(mat) = c("group", paste(nomvarcont[i], "median (range)[IQR]",sep = "____"), paste(nomvarcont[i], "missing", sep = "____"))
        ###
        if (i == 1) {
          mat = merge(som, mat, by = "group")
        }
        if (dim(table(dat[, names(dat) == nomvargroup])) >  2) {
          test = try(kruskal.test(dat[, names(dat) == nomvarcont[i]] ~  dat[, names(dat) == nomvargroup]),silent=TRUE)
          test.type.p = matrix(nrow = 1, ncol = ncol(mat))
          colnames(test.type.p) = colnames(mat)
          test.type.p[1, 1] = "Test p-value"
          test.type.p[1, 2] = ifelse(class(test)=="try-error",NA,
                                     ifelse(test$p.value < 0.0001, " < 0.0001" ,
                                            round(test$p.value,4)))
          mat = rbind(mat, test.type.p)
        }
        else {
          test = try(wilcox.test(dat[, names(dat) == nomvarcont[i]] ~  dat[, names(dat) == nomvargroup]),silent=TRUE)
          test.type.p = matrix(nrow = 1, ncol = ncol(mat))
          colnames(test.type.p) = colnames(mat)
          test.type.p[1, 1] = "Test p-value"
          test.type.p[1, 2] = ifelse(class(test)=="try-error",NA,
                                     ifelse(test$p.value < 0.0001, " < 0.0001" ,
                                            round(test$p.value,4)))
          mat = rbind(mat, test.type.p)
        }
        if (i > 1) {
          toutcont = merge(toutcont, mat, by = "group")
        }
        else {
          toutcont = mat
        }
        i <- i + 1
      }
    }
    else {
      if (sum(nomvarcont %in% names(dat)) != length(nomvarcont)) {
        print(paste("Undefined variable: ", nomvarcont[which(nomvarcont %in% 
                                                               names(dat) == FALSE)], sep = ""))
      }
      else {
        print("No continuous variable defined")
      }
    }
    
    ### Code en présence de "nomvarcat"
    if (is.null(nomvarcat) == FALSE & sum(nomvarcat %in%  names(dat)) == length(nomvarcat)) {
      
      for(i in nomvarcat) dat[,i]<-as.factor(as.character(dat[,i]))
      
      
      i = 1
      while (i < length(nomvarcat) + 1) {
        matcat = as.matrix(t(table(dat[, names(dat) ==  nomvarcat[i]], dat[, names(dat) == nomvargroup])))
        nclas=nlevels(dat[,which(names(dat)==nomvarcat[i])])
        for(j in 1:nclas)   {
          colnames(matcat)[j]=colnames(matcat)[j]
        }  

        colnames(matcat)<-paste(nomvarcat[i],levels(dat[,nomvarcat[i]]),sep="____")
        group = data.frame(rownames(matcat))
        names(group) = "group"
        ########################################
        nonmissg = aggregate(dat[, names(dat) == nomvarcat[i]], 
                             list(group = dat[, names(dat) == nomvargroup]), 
                             nbnonmissgr)
        missg = aggregate(dat[, names(dat) == nomvarcat[i]], 
                          list(group = dat[, names(dat) == nomvargroup]), 
                          nbmissgr)
        #matsum = matrix(paste(round(matcat,0), " (", round(matcat/nonmissg$x * 100, 1), ")", sep = ""), 
        matsum = matrix(paste(round(matcat,0), " (", round(matcat/nonmissg$x * 100, 1), ")", sep = ""),                 
                        ncol = ncol(matcat), nrow = nrow(matcat))
        colnames(matsum) = colnames(matcat)
        ###
        matcat1 = cbind.data.frame(missg$group,as.data.frame(matsum),missg$x)
        ##matcat1 = matcat1[,c(2:dim(matcat1)[2])]
        matcat1 <- rename.variable(matcat1, "missg$group", "group")
        matcat1 <- rename.variable(matcat1, "missg$x", paste(nomvarcat[i],"missing",sep="____"))
        ###
        test.type.p = matrix(nrow = 1, ncol = ncol(matcat1))
        colnames(test.type.p) = colnames(matcat1)
        test.type.p[1, 1] = "Test p-value"
        
        ########## Test sur les variables #############
        ###############################################
        
        ## Si plus de 5 modalité, pas de tests
        if(dim(table(dat[, names(dat) == nomvarcat[i]], dat[, names(dat) == nomvargroup]))[1] > 5 | 
           dim(table(dat[, names(dat) == nomvarcat[i]], dat[, names(dat) == nomvargroup]))[1] < 2){
          test.type.p[1, 2] = "Not done"
          print(paste0("Levels of",nomvarcat[i],">5 or < 2, no test realised"))
        }else{
        
        ## Test de chi2
        test = suppressWarnings((try(chisq.test(dat[, names(dat) == nomvarcat[i]], dat[, names(dat) == nomvargroup],correct=F),silent = TRUE)))

        test.type.p[1, 2] = ifelse(class(test)=="try-error",NA,
                                   ifelse(test$p.value < 0.0001, " < 0.0001" ,
                                          round(test$p.value,4)))

        ## Si chi2 invalide, fisher
        if(class(test)!="try-error"){
        if(sum(test$expected<5)!=0){
          test = try(fisher.test(dat[, names(dat) == nomvarcat[i]], dat[, names(dat) == nomvargroup]),silent=TRUE)
          print(paste0("Chi2 validity is not satified for: ",nomvarcat[i],", an exact Fisher test is realised"))

          if(class(test)=="try-error"){
            print(paste0("The fisher test fails for the variable: ",nomvarcat[i], 
            ", no p-value is returned, choose another option (regroupment of modalities or simulated p.value (see ?fisher.test))"))
            test.type.p[1, 2] = NA
            }else{
              test.type.p[1, 2] <-paste0(ifelse(test$p.value == 0, 0, 
                                                ifelse(test$p.value < 0.0001, " < 0.0001 ", round(test$p.value,4)))," f")
            }
          }}
        }
        matcat1 = rbind(matcat1, test.type.p)

        if (i > 1) {
          toutcat = merge(toutcat, matcat1, by = "group")
        }
        else {
          toutcat = matcat1
        }
        i <- i + 1
      }
    }
    else {
      if (sum(nomvarcat %in% names(dat)) != length(nomvarcat)) {
        print(paste("Undefined variable: ", nomvarcat[which(nomvarcat %in% 
                                                              names(dat) == FALSE)], sep = ""))
      }
      else {
        print("No categorical variable defined")
      }
    }
    
    ############################
    ### Sortie des résultats ###
    ############################
    
    ### Uniquement "nonvarcat"
    if (is.null(nomvarcont) == T & is.null(nomvarcat) == F) {
      res<- t(toutcat)
    }
    
    ### Uniquement "nonvarcont"
    if (is.null(nomvarcat) == T & is.null(nomvarcont) == F) {
      
      res<- t(toutcont)
      
    }
    
    ### Presences de "nonvarcont"et "nomvarcat"
    if (is.null(nomvarcat) == F & is.null(nomvarcont) == F) {
      res<- t(merge(toutcont, toutcat, by = "group"))
    }

    res<-cbind(NA,res)
    colnames(res)<-c("Stat or level",rep("Group:",length(levels(dat[,nomvargroup]))),"Test")
    for (i in 1:dim(res)[1]){
      res[i,1]<-str_split(row.names(res)[i],"____")[[1]][2]
      rownames(res)[i]<-str_split(row.names(res)[i],"____")[[1]][1]
    }
    res[is.na(res[,"Test"]),"Test"]<-""
    res[is.na(res[,"Stat or level"]),"Stat or level"]<-""
    rownames(res)[duplicated(rownames(res))]<-""
    res
  }
}





### Suggestion ###

library(compareGroups)
compareGrsummary <- function (dat, nomvarcont=NULL, nomvarcat=NULL, nomvargroup=NULL,testcat=NULL) {
  if (is.null(nomvargroup)) {
  
    if (is.null(nomvarcont) & is.null(nomvarcat)) {
      stop("At least one categorical or continuous variable must be specified")
    }
    
    # Create the formula for compareGroups
    vars <- c(nomvarcont, nomvarcat)
    # if nomvargroup is not specified then is.null(nomvargroup) , formula will still work
    formula <- as.formula(paste( ifelse(is.null(nomvargroup), "",nomvargroup) , " ~ ", paste(vars, collapse = " + ")))
    
    result <- compareGroups(formula, data = dat, show.all = TRUE)
  }
  
  # Display html
  export
  
  # Return a descriptive table
  return(createTable(result))
}


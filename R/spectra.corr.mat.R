# Comparison of multiple spectra with database using Pearson correlation
# No plot option
# Fist column for both db1 and db2 must be frequency data

spectra.corr.mat <- function(db1, db2, t, normal='no') {
  if(normal=='percentage'){
    for(i in 2:ncol(db1)){
      db1[,i]<-(db1[,i]/max(db1[,i], na.rm = T)*100)
    }
    for(j in 2:ncol(db2)){
      db2[,j]<-(db2[,j]/max(db2[,j], na.rm = T)*100)
    }}
  if(normal=='SNV'){
    for(i in 2:ncol(db1)){
      db1[,i]<-(db1[,i]-mean(db1[,i], na.rm=T)/sd(db1[,i], na.rm=T))}
    for(j in 2:ncol(db2)){
      db2[,j]<-(db2[,j]-mean(db2[,j], na.rm=T)/sd(db2[,j], na.rm=T))}
  }
  if(normal=='min.max'){
    for(i in 2:ncol(db1)){
      db1[,i]<-(db1[,i]-min(db1[,i], na.rm=T))/((max(db1[,i], na.rm = T))-min(db1[,i], na.rm=T))
    }
    for(j in 2:ncol(db2)){
      db2[,j]<-(db2[,j]-min(db2[,j], na.rm=T))/((max(db2[,j], na.rm = T))-min(db2[,j], na.rm=T))
    }  }
  if(normal=='no'){
    db1<-db1
    db2<-db2
  }

  if((db1[2,1]-db1[1,1]) < (db2[2,1]-db2[1,1]))#Verify which database has the higher spectral resolution

  { for(i in 1:nrow(db2)){ #Case 1 db1 has higher spectral resolution than db2
    db1[,1][db2[,1][i] >= db1[,1] - t & db2[,1][i] <= db1[,1] + t] <- db2[,1][i]} #Alignment based on the t value of tolerance
    alignment<- merge(db1, db2, by = 1, all = TRUE) #Merging the two databases
    alignment2<-imputeTS::na_interpolation(alignment, max.gap=5)
    mcor<-round(cor(alignment2, use="pairwise.complete.obs"),2) #Pairwise correlation
    cor_spectra<-mcor[,-1] #Remove column with frequency from correlation matrix
    mcor_inv <- cor_spectra[,order(ncol(cor_spectra):1)] #Inverse order of column
    mcor_mat<-as.matrix(mcor_inv[-c(1),])#Creation of matrix of correlation
    numb_un<-(ncol(db2)-1)
    numb_row<- (nrow(mcor_mat)-numb_un)+1
    mcor_fin<-mcor_mat[-(numb_row:nrow(mcor_mat)),(1:numb_un)]
    final<-as.data.frame(mcor_fin) #Final matrix
    max_index<-apply(final, 2, function(x) which.max(x))
    final2<-final
    final2$names <- rownames(final)
    value_max<-t(as.matrix(final2$names[as.vector(max_index)]))
    colnames(value_max)<-c(colnames(final))
    rownames(value_max)<-c("Maximum match")
    newList <- list("Score" = final, "Maximum_match" = value_max)
    return(newList)
  }

  else #Case 2: db1 has minor spatial resolution than db2
  {  for(i in 1:nrow(db1))
    db2[,1][db1[,1][i] >= db2[,1] - t & db1[,1][i] <= db2[,1] + t] <- db1[,1][i] #Alignment based on the t value of tolerance
  alignment <- merge(db2, db1, by = 1, all = TRUE) #Merging the two databases
  alignment2<-imputeTS::na_interpolation(alignment, max.gap=5)
  mcor<-round(cor(alignment2, use="pairwise.complete.obs"),2)
  cor_spectra<-mcor[-1,-1] #Remove column with frequency from correlation matrix
  numb_un<-(ncol(db2)-1)
  mcor_fin<-cor_spectra[-(1:numb_un),(1:numb_un)]
  final<-as.data.frame(mcor_fin) #Final matrix order by correlation values
  max_index<-apply(final, 2, function(x) which.max(x))
  final2<-final
  final2$names <- rownames(final)
  value_max<-t(as.matrix(final2$names[as.vector(max_index)]))
  colnames(value_max)<-c(colnames(final))
  rownames(value_max)<-c("Maximum match")
  newList <- list("Score" = final, "Maximum_match" = value_max)
  return(newList)

  }
}


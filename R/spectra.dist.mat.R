# Comparison of multiple spectra with database using Pearson correlation
# No plot option
# Fist column for both db1 and db2 must be frequency data

spectra.dist.mat <- function(db1, db2, t) {

  if((db1[2,1]-db1[1,1]) < (db2[2,1]-db2[1,1]))#Verify which database has the higher spectral resolution

  { for(i in 1:nrow(db2)){ #Case 1 db1 has higher spectral resolution than db2
    db1[,1][db2[,1][i] >= db1[,1] - t & db2[,1][i] <= db1[,1] + t] <- db2[,1][i]} #Alignment based on the t value of tolerance
    alignment<- merge(db1, db2, by = 1, all = TRUE) #Merging the two databases
    alignment2<-imputeTS::na_interpolation(alignment, max.gap=5)
    trasp_alignment<-as.data.frame(t(alignment2))
    euc_dist<-round(as.matrix(dist(trasp_alignment, "euclidean")),2)
    euc_spectra<-euc_dist[,-1] #Remove column with frequency from correlation matrix
    euc_inv <- euc_spectra[,order(ncol(euc_spectra):1)] #Inverse order of column

    norm_vec <- function(x) sqrt(sum(x^2))
    denom_mat<-matrix(nrow=(ncol(db1)-1), ncol=1)#Preparation of denominator
    numb_un<-ncol(db2)-1
    index_col<-ncol(alignment2)-numb_un
    for (i in 2:(index_col)){
      denom_mat[i-1,1]<-norm_vec(alignment2[,i][!is.na(alignment2[,i])])
    }
    colnames(denom_mat)<-"Norm ref_spec"
    rownames(denom_mat)<-colnames(db1[,-1])

    euc_data<-as.data.frame(euc_inv[-c(1, (nrow(euc_inv)-numb_un+1):nrow(euc_inv)),])
    euc_int<-as.data.frame(euc_data[,1:numb_un])
    HQI<-(1+((euc_int)^2/(denom_mat)^2))^-1

    max_index<-apply(HQI, 2, function(x) which.max(x))
    final2<-HQI
    final2$names <- rownames(HQI)
    value_max<-t(as.matrix(final2$names[as.vector(max_index)]))
    colnames(value_max)<-c(colnames(HQI))
    rownames(value_max)<-c("Maximum match")
    newList <- list("Score" = HQI, "Maximum_match" = value_max)
    return(newList)
  }

  else #Case 2: db1 has minor spatial resolution than db2
  {  for(i in 1:nrow(db1))
    db2[,1][db1[,1][i] >= db2[,1] - t & db1[,1][i] <= db2[,1] + t] <- db1[,1][i] #Alignment based on the t value of tolerance
  alignment <- merge(db2, db1, by = 1, all = TRUE) #Merging the two databases
  alignment2<-imputeTS::na_interpolation(alignment, max.gap=5)
  trasp_alignment<-as.data.frame(t(alignment2))
  euc_dist<-round(as.matrix(dist(trasp_alignment, "euclidean")),2)
  euc_spectra<-euc_dist[,-1] #Remove column with frequency from correlation matrix

  norm_vec <- function(x) sqrt(sum(x^2))
  denom_mat<-matrix(nrow=(ncol(db1)-1), ncol=1)#Preparation of denominator
  numb_un<-ncol(db2)-1
  numb_start<-2+numb_un
  numb_row<-numb_un+1
  for (i in (numb_start):(ncol(alignment2))){
    denom_mat[(i-numb_row),1]<-norm_vec(alignment2[,i][!is.na(alignment2[,i])])}
  colnames(denom_mat)<-"Norm ref_spec"
  rownames(denom_mat)<-colnames(db1[,-1])

  euc_data<-as.data.frame(euc_spectra[-c(1:numb_row),])
  euc_int<-as.data.frame(euc_data[,1:numb_un])
  HQI<-(1+((euc_int)^2/(denom_mat)^2))^-1

  max_index<-apply(HQI, 2, function(x) which.max(x))
  final2<-HQI
  final2$names <- rownames(HQI)
  value_max<-t(as.matrix(final2$names[as.vector(max_index)]))
  colnames(value_max)<-c(colnames(HQI))
  rownames(value_max)<-c("Maximum match")
  newList <- list("Score" = HQI, "Maximum_match" = value_max)
  return(newList)

  }
}

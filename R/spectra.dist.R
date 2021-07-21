# Spectra comparison with user-defined or provided database using Euclidean distance
# Identification of only one unknown spectra
# Fist column for both db1 and db2 must be frequency data
# Possibility to print the plot, by default it is set T

spectra.dist <- function(db1, db2, t, plot=T) {
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
    denom_mat<-matrix(nrow=(ncol(alignment2)-2), ncol=1)
    for (i in 2:(ncol(alignment2)-1)){
      denom_mat[i-1,1]<-norm_vec(alignment2[,i][!is.na(alignment2[,i])])
    }
    colnames(denom_mat)<-"Norm ref_spec"
    rownames(denom_mat)<-colnames(db1[,-1])
    euc_data<-as.data.frame(euc_inv[-c(1, nrow(euc_inv)),])
    euc_int<-as.matrix(euc_data[,1])
    colnames(euc_int)<-colnames(euc_data)[1]
    rownames(euc_int)<-colnames(db1[,-1])
    HQI<-(1+((euc_int)^2/(denom_mat)^2))^-1
    if(nrow(HQI)<10) { #Condition if number row of db1 is less than 10
      final1<-as.data.frame(HQI[order(HQI[,1], decreasing = TRUE),]) #Final matrix order by correlation values
      colnames(final1)<-c(colnames(db2))[-1] #Change column name
      final<-round(final1, 2)
      final
    }
    else #Condition if number row of db1 is major than 10
    {
      final2<-as.matrix(HQI[order(HQI[,1], decreasing = TRUE),]) #Final matrix order by correlation values
      final1<-as.matrix(final2[c(1:10),]) #Final matrix order by correlation values
      colnames(final1)<-c(colnames(db2))[-1] #Change column name
      final<-round(final1, 2)
      final }
  }

  else #Case 2: db1 has minor spatial resolution than db2
  {  for(i in 1:nrow(db1))
    db2[,1][db1[,1][i] >= db2[,1] - t & db1[,1][i] <= db2[,1] + t] <- db1[,1][i] #Alignment based on the t value of tolerance
  alignment <- merge(db2, db1, by = 1, all = TRUE) #Merging the two databases
  alignment2<-imputeTS::na_interpolation(alignment, max.gap=5)
  trasp_alignment<-as.data.frame(t(alignment2))
  euc_dist<-round(as.matrix(dist(trasp_alignment, "euclidean")),2)
  euc_spectra<-as.matrix(euc_dist[-c(1,2),2]) #Remove column with frequency from correlation matrix
  colnames(euc_spectra)<-colnames(db2)[2]
  norm_vec <- function(x) sqrt(sum(x^2))
  denom_mat<-matrix(nrow=(ncol(db1)-1), ncol=1)
  for (i in 3:(ncol(alignment2))){
    denom_mat[i-2,1]<-norm_vec(alignment2[,i][!is.na(alignment2[,i])])
  }
  colnames(denom_mat)<-"Norm ref_spec"
  rownames(denom_mat)<-colnames(db1[,-1])
  HQI2<-(1+((euc_spectra)^2/(denom_mat)^2))^-1

  if(nrow(HQI2)<10){
    final1<-as.matrix(HQI2[order(HQI2[,1], decreasing = TRUE),])
    colnames(final1)<-c(colnames(db2))[-1]
    final<-round(final1, 2)
    final
  }
  else{
    final2<-as.matrix(HQI2[order(HQI2[,1], decreasing = TRUE),])
    final1<-as.matrix(final2[c(1:10),]) #Final matrix order by correlation values
    colnames(final1)<-c(colnames(db2))[-1]
    final<-round(final1, 2)
    final}

  }
  plot_spec<-ggplot2::ggplot()+
    ggplot2::geom_path(data=db2, ggplot2::aes(x=db2[,1], y=db2[,2],color = colnames(db2)[2]), na.rm=TRUE)+
    ggplot2::geom_path(data=db1, ggplot2::aes(x=db1[,1], y=db1[,row.names(final)[1]], color = rownames(final)[1]), na.rm=TRUE)+
    ggplot2::labs(x=expression(Raman~shift~(cm^-1)), y="Counts (a.u.)")+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = ggplot2::element_text(size="16"),
          axis.title.y = ggplot2::element_text(size="16"),
          axis.text.x= ggplot2::element_text(size="14"),
          legend.position="top",
          legend.title=ggplot2::element_blank())

  if(plot==T){
    print(plot_spec)
    print(final)
  }  else{
    print(final)
  }
}

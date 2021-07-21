# Spectra comparison with user-defined or provided database using Pearson correlation
# Identification of only one unknown spectra
# Fist column for both db1 and db2 must be frequency data
# Possibility to print the plot, by default it is set T


spectra.corr <- function(db1, db2, t, normal='no', plot=T) {
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
    numb_unidenti<-ncol(db2)
    index_row<-(nrow(mcor_inv)-(ncol(db2)-1))+1
    mcor_mat<-as.matrix(mcor_inv[-c(1, nrow(mcor_inv)), 1]) #Creation of matrix of correlation
    if(nrow(mcor_mat)<10) { #Condition if number roe of db1 is less than 10
      final<-as.matrix(mcor_mat[order(mcor_mat[,1], decreasing = TRUE),]) #Final matrix order by correlation values
      colnames(final)<-c(colnames(db2))[-1] #Change column name
      final
    }
    else #Condition if number roe of db1 is major than 10
    {
      final2<-as.matrix(mcor_mat[order(mcor_mat[,1], decreasing = TRUE),]) #Final matrix order by correlation values
      final<-as.matrix(final2[c(1:10),]) #Final matrix order by correlation values
      colnames(final)<-c(colnames(db2))[-1] #Change column name
      final }
  }

  else #Case 2: db1 has minor spatial resolution than db2
  {  for(i in 1:nrow(db1))
    db2[,1][db1[,1][i] >= db2[,1] - t & db1[,1][i] <= db2[,1] + t] <- db1[,1][i] #Alignment based on the t value of tolerance
  alignment <- merge(db2, db1, by = 1, all = TRUE) #Merging the two databases
  alignment2<-imputeTS::na_interpolation(alignment, max.gap=5)
  mcor<-round(cor(alignment, use="pairwise.complete.obs"),2)
  numb_unidenti<-ncol(db2)
  mcor_mat<-as.matrix(mcor[-c(1:2),2:numb_unidenti])
  if(nrow(mcor_mat)<10){
    final<-as.matrix(mcor_mat[order(mcor_mat[,1], decreasing = TRUE),])
    colnames(final)<-c(colnames(db2))[-1]
    final
  }
  else{
    final2<-as.matrix(mcor_mat[order(mcor_mat[,1], decreasing = TRUE),])
    final<-as.matrix(final2[c(1:10),]) #Final matrix order by correlation values
    colnames(final)<-c(colnames(db2))[-1]
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


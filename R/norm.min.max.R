# Min max data normalization of spectra file with frequency as a first column and intensity
norm.min.max<-function(spectra){
  for(i in 2:ncol(spectra)){
    spectra[,i]<-(spectra[,i]-min(spectra[,i], na.rm=T))/(max(spectra[,i], na.rm = T)-min(spectra[,i], na.rm=T))
  }
  return(spectra)
}

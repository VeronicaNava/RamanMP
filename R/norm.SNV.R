#Standard normal variate (SNV) transformation of a spectra with first column frequency data
norm.SNV<-function(spectra){
  for(i in 2:ncol(spectra)){
    spectra[,i]<-(spectra[,i]-mean(spectra[,i], na.rm=T))/sd(spectra[,i], na.rm=T)  }
  return(spectra)
}

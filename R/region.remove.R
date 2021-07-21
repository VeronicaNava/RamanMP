# Remove spectral range from matrix with multiple spectra
# First column must be frequency

region.remove<-function(spectra, min.region, max.region){
  if(missing(min.region))
    stop("Argument 'min.region' is missing.")
  if(missing(max.region))
    stop("Argument 'max.region' is missing")

  new_spectra<-spectra[with(spectra, !((spectra[,1] >= min.region & spectra[,1] <= max.region) )), ]
  return(new_spectra)
}

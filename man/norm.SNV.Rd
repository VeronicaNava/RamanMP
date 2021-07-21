\name{norm.SNV}
\alias{norm.SNV}
\title{
Z-score normalisation
}
\usage{
norm.SNV(spectra)
}
\description{
The function performs a Standard normal variate (SNV) transformation of a spectra. Normalisation is performed subtracting at each peak intensity the mean intensity value of the spectra and then dividing for the standard deviation of the spectra intensities.
}
\arguments{
\item{spectra}{A dataframe/matrix with frequency values as first column and at least one column with intensity values.  }
}
\value{
Return the normalised spectra: the first column represent the frequency data, the second the intensity values normalised by Z-score}

\author{
Veronica Nava}
\examples{
data("MPdatabase")
norm.database<-norm.SNV(MPdatabase)
norm.spectra<-norm.SNV(MPdatabase[,c(1,2)])
}


\keyword{normalisation}

\name{norm.min.max}
\alias{norm.min.max}
\title{
Min-max normalisation
}
\usage{
norm.min.max(spectra)
}
\description{
The function performs a min-max normalisation on one or multiple spectra. Normalisation is performed subtracting at each peak intensity the minimum intensity value of the spectra and then dividing for the difference between the maximum and the minimum peak values of the spectra.
}
\arguments{
\item{spectra}{A dataframe/matrix with frequency values as first column and at least one column with intensity values.  }
}
\value{
Return the normalised spectra: the first column represent the frequency data, the second the intensity values normalised}

\author{
Veronica Nava}
\examples{
data("MPdatabase")
norm.database<-norm.min.max(MPdatabase)
norm.spectra<-norm.min.max(MPdatabase[,c(1,2)])
}


\keyword{normalisation}

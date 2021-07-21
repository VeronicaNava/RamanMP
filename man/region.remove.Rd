\name{region.remove}
\alias{region.remove}
\title{
Removal of spectral region
}
\usage{
region.remove(spectra, min.region, max.region)
}
\description{
The function removes a spectral region of no interest for further analysis. The user must specify range values for the region that has to be removed.
}
\arguments{
\item{spectra}{A dataframe/matrix with frequency values as first column and at least one column with intensity values. }
\item{min.region}{Numeric. Minimum frequency value of the region that should be removed.}
\item{max.region}{Numeric. Maximum frequency value of the region that should be removed.}
}

\value{
Return the spectra with the removed region. The rows corresponding to the range specified are removed.}


\examples{
data("MPdatabase")
new.spectrum<-region.remove(MPdatabase[,c(1,6)], min.region=500, max.region=1200)
new.spectra<-region.remove(MPdatabase, min.region=500, max.region=1200)
}

\keyword{peaks}

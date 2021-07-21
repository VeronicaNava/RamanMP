\name{savit.gol}
\alias{savit.gol}
\title{
Savitzky–Golay smoothing
}
\usage{
savit.gol(x, filt, filt_order = 4, der_order = 0)
}
\description{
The function applies a Savitkzy-Golay smoothing filter on the spectra file based on settings defined by the user.
}
\arguments{
\item{x}{A vector with the intensity values that should be smoothed. }
\item{filt}{Numeric.The length of the filter length, must be odd.}
\item{filt_order}{Numeric. Filter order: 2 = quadratic filter, 4 = quartic. Default is 4.}
\item{der_order}{Numeric. Derivative order: 0 = smoothing, 1 = first derivative, etc. Default is 0.}
}

\value{
Return the spectra with the removed region. The rows corresponding to the range specified are removed.}


\examples{
data("MPdatabase")
smooth.vect<-savit.gol(MPdatabase[,6], filt=11)
}

\keyword{peaks}

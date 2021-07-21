\name{peak.finder}
\alias{peak.finder}
\title{
Peaks identification
}
\usage{
peak.finder(spectrum, threshold=0, m=5, max.peak=0)
}
\description{
The function identifies peaks based on local maxima. The function returns a list of the peaks and a plot with the peaks labeled. Missing values (NA) are removed.
}
\arguments{
\item{spectrum}{A dataframe/matrix with only two columns: the first column must report the frequency values; the second  column must report the intensity values.}
\item{threshold}{Numeric. It indicates the value on y-axis that the peak intensity must exceed to be considered a peak. This can be helpful in case of noisy Raman spectrum. The default value is 0.}
\item{m}{Numeric. It indicates the interval on x-axis for the determination of the interval for the calculation of the peak. Default value is 5.}
\item{max.peak}{Numberic. It indicates the number of peaks that should be displayed. The default is 0, which indicates that all peaks are showed.}

}
\value{
Return the normalised spectra: the first column represent the frequency data, the second the intensity values normalised by Z-score}


\examples{
data("MPdatabase")
peak.data<-peak.finder(MPdatabase[,c(1,7)], threshold = 500, m=7)
}

\keyword{peaks}

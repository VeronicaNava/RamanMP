\name{spectra.dist}
\alias{spectra.dist}
\title{
Spectrum identification based on Euclidean distance
}
\usage{
spectra.dist(db1, db2, t, plot=T)
}
\description{
The function allows identification of Raman spectra of single unknown plastic polymer comparing the spectrum with a user-defined database or using the database included into the package using the Euclidean distance. The database is provided within the data of the package with the name 'MPdatabase' and includes different plastic polymers, pigments and additives.
}
\arguments{
\item{db1}{Dataframe/matrix with frequency values as first column and at least one column with intensity values. This should be the database with the known spectra of plastics. This can be a user-defined database or the database implemented in the package ('MPdatabase'). }
\item{db2}{Dataframe/matrix with frequency values as first column and one column with intensity values of the unknown spectrum that should be identified.}
\item{t}{Numeric. It indicates the tolerance for the matching of the two spectra. For a given t-value, the intensity values that range in the frequency interval (f-t, f+t) are matched with the corresponding intensity values of the database with the highest spectral resolution.}
\item{plot}{Logical. If TRUE, a plot of the unknown spectra and the spectrum of the database, for which the highest correlation value was found, are showed. This allows verification of the results obtained
}
}

\value{
Return a matrix with Hit Quality Indexes (HQI) calculated using the Euclidean distance for the unknown spectra from the database spectra following the equation 6 reported in Renner et al. (2019).The matrix reports only the top 10 polymers for which the HQI are the highest, ordered from the largest to the smallest. If the database contains less than 10 spectra, all the HQI are reported.
}
\references{
Renner, G., Schmidt, T. C., Schram, J. (2019).Analytical methodologies for monitoring micro(nano)plastics: Which are fit for purpose?.
Current Opinion in Environmental Science & Health, 1, 55-61, https://doi.org/10.1016/j.coesh.2017.11.001}

\examples{
data("MPdatabase","single_unknown")
identif_spectra<-spectra.dist(MPdatabase, single_unknown, t=0.5)
}

\keyword{polymer identification}

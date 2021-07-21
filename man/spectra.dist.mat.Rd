\name{spectra.dist.mat}
\alias{spectra.dist.mat}
\title{
Identification of multiple spectra identification based on Euclidean distance
}
\usage{
spectra.dist.mat(db1, db2, t)
}
\description{
The function allows identification of Raman spectra of multiple plastic polymers through the comparison with a user-defined database or using the database included into the package by means of Euclidean distance. The database is provided within the data of the package with the name 'MPdatabase' and includes different plastic polymers, pigments and additives.
}
\arguments{
\item{db1}{Dataframe/matrix with frequency values as first column and at least one column with intensity values. This should be the database with the known spectra of plastics. This can be a user-defined database or the database implemented in the package ('MPdatabase'). }
\item{db2}{Dataframe/matrix with frequency values as first column and columns with intensity values of the unknown spectra that should be identified.}
\item{t}{Numeric. It indicates the tolerance for the matching of the two spectra. For a given t-value, the intensity values that range in the frequency interval (f-t, f+t) are matched with the corresponding intensity values of the database with the highest spectral resolution.}
}

\value{
Return a list of two elements. The first is "Score", which reports all the Hit Quality Indexes (HQI) calculated using the Euclidean distance for the unknown spectra from the database spectra following the equation 6 reported in Renner et al. (2019). The second element of the list is "Maximum score" which reports for each unkown spectra (reported in col names) the name of the polymer for which the maximum HQI (based on Euclidean distance) was identified. }
\references{
Renner, G., Schmidt, T. C., Schram, J. (2019).Analytical methodologies for monitoring micro(nano)plastics: Which are fit for purpose?.
Current Opinion in Environmental Science & Health, 1, 55-61, https://doi.org/10.1016/j.coesh.2017.11.001}

\examples{
data("MPdatabase","matrix_unknown")
identif_spectra<-spectra.dist.mat(MPdatabase, matrix_unknown, t=0.5)
score<-identif_spectra[1]
maximum_match<-identif_spectra[2]
}

\keyword{polymer identificatio}

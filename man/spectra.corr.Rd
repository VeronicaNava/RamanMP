\name{spectra.corr}
\alias{spectra.corr}
\title{
Spectrum identification based on Pearson correlation coefficient
}
\usage{
spectra.corr(db1, db2, t, normal='no', plot=T)
}
\description{
The function allows identification of Raman spectra of single unknown plastic polymer comparing the spectrum with a user-defined database or using the database included into the package using the Pearson correlation coefficient. The database is provided within the data of the package with the name 'MPdatabase' and includes different plastic polymers, pigments and additives.
}
\arguments{
\item{db1}{Dataframe/matrix with frequency values as first column and at least one column with intensity values. This should be the database with the known spectra of plastics. This can be a user-defined database or the database implemented in the package ('MPdatabase'). }
\item{db2}{Dataframe/matrix with frequency values as first column and one column with intensity values of the unknown spectrum that should be identified.}
\item{t}{Numeric. It indicates the tolerance for the matching of the two spectra. For a given t-value, the intensity values that range in the frequency interval (f-t, f+t) are matched with the corresponding intensity values of the database with the highest spectral resolution.}
\item{normal}{This arguments indicates if the data of the database and the unknown spectra should be normalized and with which methods. Accepts the following inputs: 'percentage' divides each peak for the peak of maximum intensity and then calculate the percentage; 'SNV' performs a Standard Normal Variate transformation; 'min.max' applies a min-max normalisation; 'no' no normalisation procedure is applied. Default is 'no'.}
\item{plot}{Logical. If TRUE, a plot of the unknown spectra and the spectrum of the database, for which the highest correlation value was found, are showed. This allows verification of the results obtained
}
}

\value{
Return a matrix with Hit Quality Indexes (HQI) calculated using Pearson correlation coefficient of the unknown spectra vs spectra of the database, as reported in eq. 7 of Renner et al. (2019).The matrix reports only the top 10 polymers for which the correlation values are the highest, ordered from the largest to the smallest. If the database contains less than 10 spectra, all the correlation coefficients are reported.
}
\references{
Renner, G., Schmidt, T. C., Schram, J. (2019).Analytical methodologies for monitoring micro(nano)plastics: Which are fit for purpose?.
Current Opinion in Environmental Science & Health, 1, 55-61, https://doi.org/10.1016/j.coesh.2017.11.001}

\examples{
data("MPdatabase","single_unknown")
identif_spectra<-spectra.corr(MPdatabase, single_unknown, t=0.5, normal='min.max')
}

\keyword{polymer identification}

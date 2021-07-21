\name{spectra.alignment}
\alias{spectra.alignment}
\title{
Align spectra with different spectral resolution
}
\usage{
spectra.alignment(db1, db2, t)
}
\description{
The function merges spectra with different spectral resolution using as a reference the spectra with highest resolution. The matching is done based on a span value defined by the user.
}
\arguments{
\item{db1}{Dataframe/matrix with frequency values as first column and at least one column with intensity values. }
\item{db2}{Dataframe/matrix with frequency values as first column and at least one column with intensity values.}
\item{t}{Numeric. It indicates the tolerance for the matching of the two spectra. For a given t-value, the intensity values that range in the frequency interval (f-t, f+t) are matched with the corresponding intensity values of the database with the highest spectral resolution.}
}

\value{
Return a matrix with frequency of the database with highest spectral resolution and intensity values of the two databases matched based on the 't' parameter.}


\keyword{match}

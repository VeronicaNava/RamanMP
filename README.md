[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/RamanMP)](https://cran.r-project.org/package=RamanMP)
[![](https://cranlogs.r-pkg.org/badges/RamanMP)](https://cran.r-project.org/package=RamanMP)

# RamanMP
Pre-processing and polymer identification of Raman spectra of plastics. Pre-processing includes normalisation functions, peak identification based on local maxima, smoothing process and removal of spectral region of no interest. Polymer identification can be performed using Pearson correlation coefficient or Euclidean distance (Renner et al., 2019; doi:10.1016/j.trac.2018.12.004), and the comparison can be done with a user-defined database or with the database already implemented in the package, which currently includes 356 spectra, with several spectra of plastic colorants.

## Installation
You can install a stable version of RamanMP with:  
```r
install.packages("RamanMP")
```

## Usage
First the package must be loaded into R:
```r
library(RamanMP)
```

You can view a list of functions available in RamanMP with:
```r
help(package = "RamanMP")
```

An example of application is reported below:

```r
# Min-max normalisation
norm.min.max(single_unknown)

#Z-score normalisation
norm.SNV(single_unknown)

#Spectrum identification based on Pearson correlation coefficient
identif_spectra<-spectra.corr(MPdatabase, single_unknown, t=0.5, normal='min.max')

#Spectrum identification based on Euclidean distance
identif_spectra<-spectra.dist(MPdatabase, single_unknown, t=0.5)

```


## Contribute to the plastic database 
If you wish to contribute to the development of the database, send an email to veronicanava245@gmail.com with your Raman spectra of plastics or additives.


## Getting Help or Reporting an Issue
To report bugs/issues/feature requests, please file an [issue](https://github.com/VeronicaNava/RamanMP/issues).  
These are very welcome!

## Citation
To use the package cite:  
Nava V., Frezzotti M. L., Leoni B. (2021).
  Raman spectroscopy for the analysis of microplastics in aquatic systems. Applied Spectroscopy, 75(11), 1341-1357.
  
To use the Raman spectra database cite:  
Nava V., Frezzotti M. L., Leoni B. (2021).
  Raman spectroscopy for the analysis of microplastics in aquatic systems. Applied Spectroscopy, 75(11), 1341-1357.
Fremout W., Saverwyns S. (2012).
  Identification of synthetic organic pigments: the role of a comprehensive digital Raman spectral library, Journal of Raman Spectroscopy, 43(11): 1536-1544. https://doi.org/10.1002/jrs.4054.


## Contributors
Veronica Nava, Barbara Leoni, Maria Luce Frezzotti

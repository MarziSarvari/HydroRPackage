HydroRPackage
=============

HydroRPackage provides a set of tools for hydrological analysis in R, designed to assist researchers and practitioners in efficiently analyzing and visualizing hydrological data.

Installation
------------

To install `HydroRPackage`, first install the `devtools` package if you haven't already:

```R
install.packages("devtools")
```
Then, you can install HydroRPackage from GitHub using the following command:

```R
devtools::install_github(repo = "MarziSarvari/HydroRPackage")
```
Dependencies
------------

HydroRPackage relies on several external R packages. You can install these dependencies using the following commands:

```R
install.packages(c("ncdf4", "ncdf4.helpers", "sf"))
install.packages(c("terra", "data.table", "curl"))
install.packages("lubridate")
install.packages("CoSMoS")
```
Usage
------------

Once installed, you can use HydroRPackage in your R environment by loading it along with its dependencies:
```R
library(HydroRPackage)
library(ncdf4)
library(ncdf4.helpers)
library(terra)
library(data.table)
library(lubridate)
library(CoSMoS)
library(sf)
library(curl)
```
To perform a hydrological analysis using HydroRPackage, follow these steps:

1. Download and unzip required data:
```R
hydroPck_download_and_unzip()
```
2. Import NetCDF files:
```R  
dta <- hydroPck_import_nc_files("data/adv_r_data")
```
3. Calculate IDF models:
```R
idf <- hydroPck_calculate_idf(dta)
```
4.Plot IDF curves:
```R
hydroPck_plot_idf_curves(idf)
```

Contributing
------------
Contributions to HydroRPackage are welcome! If you encounter any issues or have suggestions for improvements, please open an issue on GitHub.


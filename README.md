`RDBESvisualise`
=========
 
`RDBESvisualise` provides functions to visualise [**Regional DataBase and Estimation System (RDBES)**](https://sboxrdbes.ices.dk/#/) data.

It is implemented as an [**R**](https://www.r-project.org) package and
available on <!-- [CRAN](https://cran.r-project.org/package=RDBESvisualise) --> 
[**GitHub**](https://github.com/ices-tools-dev/RDBESvisualise)

Installation
------------

<!--
RDBESvisualise can be installed from CRAN using the `install.packages` command:

```R
install.packages("RDBESvisualise")

```
-->

RDBESvisualise can be installed from GitHub using the `install_github`
command from the [remotes](https://remotes.r-lib.org/) package:

```R
library(remotes)

install_github("ices-tools-dev/RDBESvisualise", build_vignettes = TRUE)
```

<!--
Usage
-----

For a summary of the package see the following [Vignettes]():

```R
browseVignettes(package = "RDBESvisualise")
```
-->

References
----------

* Regional Database & Estimation System:
https://sboxrdbes.ices.dk/

* Working Group on Governance of the Regional Database & Estimation System:
https://www.ices.dk/community/groups/Pages/WGRDBESGOV.aspx

* Working Group on Estimation with the RDBES data model (WGRDBES-EST):
https://github.com/ices-tools-dev/RDBEScore/blob/main/WGRDBES-EST/references/WGRDBES-EST%20Resolutions.pdf

* see also: https://github.com/ices-tools-dev/RDBEScore/tree/main/WGRDBES-EST/references

`RDBESvisualise (Development)`
=========

RDBESvisualise is developed openly on
[GitHub](https://github.com/ices-tools-dev/RDBESvisualise).

Feel free to open an
[issue](https://github.com/ices-tools-dev/RDBESvisualise/issues) there if you
encounter problems or have suggestions for future versions.

The current development version can be installed using:

```R
library(remotes)
install_github("ices-tools-dev/RDBESvisualise@dev")

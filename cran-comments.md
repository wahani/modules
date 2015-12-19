
This is a re-submission of the package, as requested. The reason for the new
submission was the following NOTE in the previous version:

Found the following (possibly) invalid URLs:
  URL: http://www.r-pkg.org/pkg/modules
    From: inst/doc/modulesInR.html
    Status: 404
    Message: Not Found
  URL: https://cran.rstudio.com/package=modules
    From: inst/doc/modulesInR.html
    Status: 404
    Message: Not Found
    
I resolved this by removing the badges from the vignette. 

## Test environments
* ubuntu (on travis-ci), R 3.2.3
* win-builder (devel and release)
* local ubuntu 14.10, R 3.2.2

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are none, this is the first release.



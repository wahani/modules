
This is a new version of the package as requested by Professor Ripley:

> Packages: ezknitr ggEcxtra lightsout modules pander shinyjs viridis
> 
> As you can see from the CRAN results pages, fetching https:// URLs is not necessarily part of a build of pandoc, so you should either declare
>
> 'pandoc with https: support'
>
> as a SystemRequirements, or preferably use R to download the file (as from R 3.3.0 R does have https: support) and point pandoc at a local copy.
> 
> Please correct.

I resolved this by removing downloads from the vignette. So that the dependency
should not be neeeded anymore.

## Test environments
* ubuntu (on travis-ci), R 3.2.3
* win-builder (devel and release)
* local ubuntu 14.10, R 3.2.2

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
To the best of my knowledge there are none



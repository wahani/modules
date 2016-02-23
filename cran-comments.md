> Thanks, we see:
> 
> * checking R code for possible problems ... NOTE
> import : isNotInstalled: no visible global function definition for
>   'installed.packages'
> Undefined global functions or variables:
>   installed.packages
> Consider adding
>   importFrom("utils", "installed.packages")
> to your NAMESPACE file.
> 
> Please fix and resubmit.
>
> Best,
> Uwe Ligges 

has been fixed as suggested

## Test environments
* ubuntu (on travis-ci), R 3.2.3
* win-builder (devel and release)
* local ubuntu 14.10, R 3.2.2

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
To the best of my knowledge there are none

This release fixes test issues on CRAN pointed out by the CRAN team. Because tests require internet access, we are now explicitly verifying if internet access is available. If there is no internet, the tests are not performed. Also, by default, tests that require internet will be skipped on CRAN. We found that some errors could occur randomly on CRAN Debian machine and locally under Ubuntu-latest.

## Test environments

* Tested on Linux (R 4.0.4) using Travis CI
* Tested on Windows (R 4.0.4) using AppVeyor
* Tested on win-builder (both R-release and R-devel)

## R CMD check results

There is no NOTE.
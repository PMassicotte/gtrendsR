## Test environments

* Tested on Linux (R 3.3.2) using Travis CI
* Tested on Windows (R 3.3.2) using AppVeyor
* Tested on Windows (R 3.3.2) Windows 10 (personal computer)
* Tested on win-builder (both R-release and R-devel)

## R CMD check results

There were no ERRORS and not NOTES. There is one WARNING for non-ascii characters because the package uses geographical data. Some countries have non-ascii characters such as *Pinar del Río* or *Médéa*. It is easier to use UTF-8 encoding.
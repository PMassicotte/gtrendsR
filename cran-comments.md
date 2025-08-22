## Test environments

- Tested macOS-latest and ubuntu-latest using Github Action (R 4.5.1).
- Tested on win-builder (both R-release and R-devel)

## R CMD check results

- There is one note because the package includes a list of country names with non-ASCII characters.
- There is one URL generating a 403 responses. The URL is valid and correct, but may forbid requests with curl in the User-Agent header.

```
Found the following (possibly) invalid URLs:
  URL: https://unece.org/trade/cefact/UNLOCODE-Download
    From: man/countries.Rd
    Status: 403
    Message: Forbidden
```

## revdepcheck results

We checked 2 reverse dependencies (1 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

- We saw 0 new problems
- We failed to check 0 packages

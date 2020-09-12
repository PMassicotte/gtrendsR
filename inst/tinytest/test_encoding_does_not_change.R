
# Encoding UTF-8 not well supported on Windows, skip tests
if ( Sys.info()[['sysname']] == "Windows"){
  exit_file("Cannot test this on Windows")
}

if (getRversion() >= as.package_version("4.1.0")) exit_file("skip remainder")

# Japanese
kw <- "赤"
res <- gtrends(kw)
expect_identical(unique(res$interest_over_time$keyword), kw)


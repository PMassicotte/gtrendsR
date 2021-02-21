
# Encoding UTF-8 not well supported on Windows, skip tests
if (Sys.info()[["sysname"]] == "Windows") {
  exit_file("Cannot test this on Windows")
}

if (getRversion() >= as.package_version("4.1.0")) exit_file("skip remainder")

# Exit if no internet
if (!curl::has_internet()) {
  exit_file("Skipping tests for lack of internet.")
}

# Exit unless opted in
if (Sys.getenv("RunAllGtrendsRTests", unset="") == "") {
    exit_file("Skipping tests not opted into.")
}

# Japanese
kw <- "赤"
res <- gtrends(kw)
expect_identical(unique(res$interest_over_time$keyword), kw)

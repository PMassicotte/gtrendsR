
if (requireNamespace("tinytest", quietly = TRUE)) {

  # Check if we have access to internet. If not, do not perform the test. Note
  # that curl::nslookup() is cross-platform and should work on all OS.
  if (curl::has_internet()) {
    tinytest::test_package("gtrendsR")
  } 
}

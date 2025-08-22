# Exit if no internet
if (!curl::has_internet()) {
  exit_file("Skipping tests for lack of internet.")
}

# Exit unless opted in
if (Sys.getenv("RunAllGtrendsRTests", unset = "") == "") {
  exit_file("Skipping tests not opted into.")
}

# Single keyword ----------------------------------------------------------

kw <- "news"
res <- gtrends(kw)

# Check that there is data in the returned list.
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

# Check that the keyword is the same in all returned df.
expect_true(all(Vectorize(identical, "x")(
  list(
    unique(res$interest_over_time$keyword),
    unique(res$interest_by_country$keyword),
    unique(res$interest_by_dma$keyword),
    unique(res$interest_by_city$keyword),
    unique(res$related_topics$keyword),
    unique(res$related_queries$keyword)
  ),
  kw
)))

# For US
res <- gtrends("NHL", geo = "US")
expect_true(nrow(res$interest_by_region) > 0)

# Multiple keywords -------------------------------------------------------

kw <- c("NHL", "NFL")
res <- gtrends(kw)

# Check that there is data in the returned list.
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_queries) > 0)

# Check that the keyword is the same in all returned df.
expect_true(all(Vectorize(identical, "x")(
  list(
    unique(res$interest_over_time$keyword),
    unique(res$interest_by_country$keyword),
    unique(res$interest_by_dma$keyword),
    unique(res$interest_by_city$keyword),
    unique(res$related_queries$keyword)
  ),
  kw
)))

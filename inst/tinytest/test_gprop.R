# Exit if no internet
if (!curl::has_internet()) {
  exit_file("Skipping tests for lack of internet.")
}

# Exit unless opted in
if (Sys.getenv("RunAllGtrendsRTests", unset="") == "") {
    exit_file("Skipping tests not opted into.")
}

kw <- "news"

res <- gtrends(kw, gprop = "web")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, gprop = "news")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, gprop = "images")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, gprop = "froogle")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, gprop = "youtube")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

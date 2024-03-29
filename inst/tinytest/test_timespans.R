# Exit if no internet
if (!curl::has_internet()) {
  exit_file("Skipping tests for lack of internet.")
}

# Exit unless opted in
# Sys.setenv("RunAllGtrendsRTests" = TRUE)

if (Sys.getenv("RunAllGtrendsRTests", unset = "") == "") {
  exit_file("Skipping tests not opted into.")
}

kw <- "news"

res <- gtrends(kw, time = "now 1-H")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "now 4-H")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "now 1-d")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "now 7-d")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "today 1-m")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "today 3-m")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "today 12-m")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "today+5-y")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "all")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

res <- gtrends(kw, time = "2010-01-01 2010-04-03")
expect_true(nrow(res$interest_over_time) > 0)
expect_true(nrow(res$interest_by_country) > 0)
expect_true(nrow(res$interest_by_dma) > 0)
expect_true(nrow(res$interest_by_city) > 0)
expect_true(nrow(res$related_topics) > 0)
expect_true(nrow(res$related_queries) > 0)

#Check if the asked time range was returned correctly.
expect_equivalent(as.Date(min(res$interest_over_time$date)), as.Date("2010-01-01"))
expect_equivalent(as.Date(max(res$interest_over_time$date)), as.Date("2010-04-03"))

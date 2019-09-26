# Japanese
kw <- "赤"
res <- gtrends(kw)
expect_identical(unique(res$interest_over_time$keyword), kw)
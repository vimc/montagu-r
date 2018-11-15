context("cache")


test_that("cache path responds to option", {
  withr::with_options(
    list(montagu.cache_path = NULL), {
      location <- montagu_server("server", "address", global = FALSE)
      expect_is(location$cache, "storr")
      expect_equal(location$cache$driver$type(), "environment")
    })

  path <- tempfile()
  withr::with_options(
    list(montagu.cache_path = path), {
      location <- montagu_server("server", "address", global = FALSE)
      expect_is(location$cache, "storr")
      expect_equal(location$cache$driver$type(), "rds")
    })
})

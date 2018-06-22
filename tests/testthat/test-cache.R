context("cache")


test_that("cache path responds to option", {
  location <- montagu_server("server", "address", global = FALSE)
  withr::with_options(
    list(montagu.cache_path = "foo"),
    expect_equal(montagu_cache_path(location), file.path("foo", "server")))
})


test_that("cache path defaults to rappdirs", {
  location <- montagu_server("server", "address", global = FALSE)
  withr::with_options(
    list(montagu.cache_path = NULL),
    expect_equal(montagu_cache_path(location),
                 file.path(rappdirs::user_data_dir("montagu"), "server")))
})

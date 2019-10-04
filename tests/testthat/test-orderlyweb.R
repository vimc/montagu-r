context("orderlyweb")

test_that("montagu_orderlyweb_remote", {
  skip_on_windows()
  remote <- montagu_orderlyweb_remote(name = "remote", host = "localhost",
                                      port = 8888)
  expect_is(remote, "orderlyweb_remote")
  expect_true(orderly:::implements_remote(remote))

  token <- montagu_orderlyweb_token(remote)
  expect_true(is.function(token))
})

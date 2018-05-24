context("util")

test_that("path encoding", {
  expect_equal(encode_path("foo"), "foo")
  expect_equal(encode_path("foo:bar"), "foo:bar")
  expect_equal(encode_path(NA_character_), NA_character_)
  expect_equal(encode_path(letters), letters)

  expect_equal(encode_path("foo/bar"), "foo:bar")
  expect_equal(encode_path("foo/bar/baz"), "foo:bar:baz")
  expect_equal(encode_path(c("foo/bar/baz", "foo/bar")),
               c("foo:bar:baz", "foo:bar"))

  expect_equal(encode_path(c("foo/bar\\baz", "foo/bar")),
               c("foo:bar:baz", "foo:bar"))


  expect_equal(decode_path("foo"), "foo")
  expect_equal(decode_path("foo:bar"), "foo/bar")
  expect_equal(decode_path(c("foo:bar", "foo:bar:baz")),
               c("foo/bar", "foo/bar/baz"))
})

test_that("clean input text", {
  ## For VIMC-1018
  expect_equal(clean_input_text('"foo"'), "foo") # strip quotes
  expect_equal(clean_input_text('  foo'), "foo") # strip leading whitespace
  expect_equal(clean_input_text('foo  '), "foo") # strip trailing whitespace
  expect_equal(clean_input_text(' foo '), "foo") # strip all whitespace
  expect_equal(clean_input_text(' "foo" '), "foo") # strip all whitespace/quotes
  expect_equal(clean_input_text(" 'foo' "), "foo") # strip all whitespace/quotes
  ## But allow use of quotes to preserve whitespace
  expect_equal(clean_input_text(' " foo " '), " foo ")
  expect_equal(clean_input_text(" ' foo ' "), " foo ")
  expect_equal(clean_input_text("f oo"), "f oo")
})


test_that("get_option_cacade uses correct priority", {
  skip_if_not_installed("withr")

  expect_equal(
    withr::with_options(list(a = 1, b = 2), get_option_cascade(c("a", "b"), 3)),
    1)
  expect_equal(
    withr::with_options(list(b = 2), get_option_cascade(c("a", "b"), 3)),
    2)
  expect_equal(
    withr::with_options(list(), get_option_cascade(c("a", "b"), 3)),
    3)
})


test_that("get_credential follows convention", {
  skip_if_not_installed("withr")

  withr::with_options(
    list(montagu.server.key = "special",
         montagu.key = "general"),
    expect_equal(get_credential(NULL, "key", FALSE, "server"), "special"))
  withr::with_options(
    list(montagu.key = "general"),
    expect_equal(get_credential(NULL, "key", FALSE, "server"), "general"))
})

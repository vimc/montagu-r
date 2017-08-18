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

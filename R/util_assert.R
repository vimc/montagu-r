assert_scalar_character <- function(x, name = deparse(substitute(name))) {
  assert_character(x, name)
  assert_scalar(x, name)
  assert_nonmissing(x, name)
  if (!nzchar(x)) {
    stop(sprintf("'%s' must be nonempty", name), call. = FALSE)
  }
}
assert_scalar_integer <- function(x, name = deparse(substitute(name))) {
  assert_integer_like(x, name)
  assert_scalar(x, name)
  assert_nonmissing(x, name)
}
assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be a character", name), call. = FALSE)
  }
}
assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}
assert_nonmissing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("'%s' must not be NA", name), call. = FALSE)
  }
}
assert_integer_like <- function(x, name = deparse(substitute(x))) {
  if (!is.integer(x) && !isTRUE(all.equal(as.integer(x), x))) {
    stop(sprintf("'%s' is not integer like", name))
  }
}

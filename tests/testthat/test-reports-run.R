context("reports: run")

## Separate out the report running a little.  This is going to be
## essentially an integration test and I'm not really sure if it will
## go ok ok travis because I'm getting general undiagnosable failures
## with the runner within orderly (see all the 'skip_on_travis' tests
## there).

test_that("run: success", {
  skip_on_os("windows")
  server <- orderly_test_server()
  on.exit(server$stop())
  remote <- montagu_server("testing", "localhost", server$port, orderly = TRUE)
  
  p <- file.path(server$path, "src", "count", "parameters.json")
  writeLines(jsonlite::toJSON(list(time = 0.2, poll = 0.1),
                              auto_unbox = TRUE), p)
  
  ans <- montagu_reports_run("count", location = remote, poll = 0.01,
                             progress = FALSE)
  
  expect_equal(ans$name, "count")
  expect_match(
    ans$id, "^([0-9]{8}-[0-9]{6})-([[:xdigit:]]{4})([[:xdigit:]]{4})$")
  expect_equal(ans$status, "success")
})


test_that("run: error", {
  skip_on_os("windows")
  server <- orderly_test_server()
  remote <- montagu_server("testing", "localhost", server$port, orderly = TRUE)
  
  p <- file.path(server$path, "src", "count", "parameters.json")
  writeLines(jsonlite::toJSON(list(time = 0.2, poll = -1),
                              auto_unbox = TRUE), p)
  
  ans <- montagu_reports_run("count", location = remote, poll = 0.01,
                             progress = FALSE, stop_on_error = FALSE)
  
  expect_equal(ans$status, "error")
  
  ## Get the reference error message here:
  ps <- file.path(dirname(p), "script.R")
  env <- new.env(parent = .GlobalEnv)
  cmp <- tryCatch(capture.output(sys.source(ps, chdir = TRUE, envir = env)),
                  error = identity)
  
  ## And check that it turns up _somewhere_
  expect_match(ans$output$stderr, cmp$message, fixed = TRUE, all = FALSE)
})


test_that("set timeout", {
  skip_on_os("windows")
  server <- orderly_test_server("interactive")
  remote <- montagu_server("testing", "localhost", server$port, orderly = TRUE)
  
  p <- file.path(server$path, "src", "count", "parameters.json")
  writeLines(jsonlite::toJSON(list(time = 2, poll = 0.1), auto_unbox = TRUE),
             p)
  
  ans <- montagu_reports_run("count", location = remote, timeout = 1,
                             progress = FALSE)
  expect_equal(ans$status, "killed")
  expect_null(ans$url)
  
  expect_error(
    montagu_reports_run("count", location = remote, timeout = 1,
                        progress = FALSE, stop_on_error = TRUE),
    "job killed by remote server after")
})

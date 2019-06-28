montagu_test_reset <- function(clear = FALSE) {
  montagu_server_global_clear()
}

montagu_test_server_admin <- function() {
  montagu_test_server("test.admin@imperial.ac.uk", "password")
}

montagu_test_server_user <- function() {
  montagu_test_server("test.modeller@imperial.ac.uk", "password")
}

montagu_test_server <- function(username = NULL, password = NULL) {
  testthat::skip_on_travis()

  montagu_test_reset()

  if (!("testing" %in% montagu_server_global_list())) {
    host <- Sys.getenv("MONTAGU_TEST_HOST", "support.montagu.dide.ic.ac.uk")
    port <- Sys.getenv("MONTAGU_TEST_PORT", "10443")
    if (is.null(username)) {
      username <- Sys.getenv("MONTAGU_TEST_USERNAME", "")
      if (!nzchar(username)) {
        testthat::skip("MONTAGU_TEST_USERNAME is not set")
      }
    }

    if (is.null(password)) {
      password <- Sys.getenv("MONTAGU_TEST_PASSWORD", "")
      if (!nzchar(password)) {
        testthat::skip("MONTAGU_TEST_PASSWORD is not set")
      }
    }

    server <- montagu_server("testing", host, as.integer(port),
                             username = username, password = password,
                             global = TRUE, overwrite = FALSE)
    tryCatch(
      server$authorise(),
      error = function(e) testthat::skip("Failed to authorise"))
  }
  "testing"
}

orderly_test_server <- function(name = "interactive", port = 8321) {
  testthat::skip_on_travis()
  path <- orderly:::prepare_orderly_example(name)
  server <- orderly.server::orderly_server_background(path, port)
  server$start()
  server
}

EXAMPLE <- "internal-2017-population-TUV-MHL"
EXAMPLE_ID <- "20170823-113855-5091025f"

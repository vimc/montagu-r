montagu_test_reset <- function(clear = FALSE) {
  montagu_server_global_clear()
}


montagu_test_server <- function() {
  testthat::skip_on_travis()
  if (!("testing" %in% montagu_server_global_list())) {
    host <- Sys.getenv("MONTAGU_TEST_HOST", "support.montagu.dide.ic.ac.uk")
    port <- Sys.getenv("MONTAGU_TEST_PORT", "10443")
    username <- Sys.getenv("MONTAGU_TEST_USERNAME", "")
    if (!nzchar(username)) {
      testthat::skip("MONTAGU_TEST_USERNAME is not set")
    }
    password <- Sys.getenv("MONTAGU_TEST_PASSWORD", "")
    if (!nzchar(password)) {
      testthat::skip("MONTAGU_TEST_PASSWORD is not set")
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


EXAMPLE <- "internal-2017-population-TUV-MHL"
EXAMPLE_ID <- "20170823-113855-5091025f"

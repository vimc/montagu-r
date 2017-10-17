montagu_reset <- function(clear = FALSE) {
  testthat::skip_on_travis()
  if (clear) {
    montagu:::.onLoad()
  }
  montagu_add_location("testing", "localhost", 1443)
  montagu_set_default_location("testing")
  montagu_authorise("test.user@imperial.ac.uk", "password")
}

EXAMPLE <- "internal-2017-population-TUV-MHL"
EXAMPLE_ID <- "20170823-113855-5091025f"

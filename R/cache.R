montagu_cache <- function(name) {
  cache_path <- getOption("montagu.cache_path", NULL)
  if (is.null(cache_path)) {
    storr::storr_environment()
  } else {
    storr::storr_rds(file.path(cache_path, name),
                     mangle_key = TRUE,
                     mangle_key_pad = FALSE)
  }
}

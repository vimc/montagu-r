montagu_cache_path <- function(location) {
  file.path(getOption("montagu.cache_path",
                      rappdirs::user_data_dir("montagu")),
            montagu_location(location)$name)
}


montagu_cache <- function(location) {
  storr::storr_rds(montagu_cache_path(location),
                   mangle_key = TRUE,
                   mangle_key_pad = FALSE)
}

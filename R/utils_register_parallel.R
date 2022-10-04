# wrapper function to register a parallel backend (and to avoid redundant code)
register_parallel <- function(ncores) {
  cl <- parallel::makePSOCKcluster(ncores)
  doParallel::registerDoParallel(cl)
  return(cl)
}


.options_to_cluster <- function(keyword) {
  relevant_option_pos <- grep(
    pattern = keyword,
    x = names(options()),
    value = FALSE
  )
  return(options()[relevant_option_pos])
}

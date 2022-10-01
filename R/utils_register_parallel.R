# wrapper function to register a parallel backend (and to avoid redundant code)
register_parallel <- function(ncores) {
  cl <- parallel::makePSOCKcluster(ncores)
  doParallel::registerDoParallel(cl)
  return(cl)
}

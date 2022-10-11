.check_available_cores <- function(ncores) {
  available_cores <- parallel::detectCores()

  if (ncores < 0L) {
    ncores_out <- available_cores
  } else if (ncores > 1L) {
    if (ncores > available_cores) {
      message(sprintf(
        paste0(
          "Number of specified cores ('%s') > available cores ('%s')...\n",
          "Setting cores to available cores - 1, i.e. '%s'."),
        ncores, available_cores, I(available_cores - 1L)
      ))
      ncores_out <- available_cores - 1L
    } else {
      ncores_out <- ncores
    }
  } else {
    ncores_out <- 1L
  }
  return(ncores_out)
}

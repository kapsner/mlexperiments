# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks(
  args = "--no-vignettes",
  build_args = "--no-build-vignettes"
)

get_stage("install") %>%
  add_step(step_install_cran("doParallel")) %>%
  add_code_step(devtools::install(".", upgrade = "always"))


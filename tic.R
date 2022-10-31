# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

get_stage("install") %>%
  add_step(step_install_cran("doParallel")) %>%
  add_code_step(devtools::install(".", upgrade = "always"))


# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

get_stage("install") %>%
  add_code_step(remotes::install_github("kapsner/kdry", dependencies = TRUE, upgrade = "always")) %>%
  add_code_step(devtools::install(".", upgrade = "always"))


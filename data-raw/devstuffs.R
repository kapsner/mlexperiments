# nolint start
packagename <- "mlexperiments"

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
#Set your name
my_desc$set_authors(c(
  person(
    given = "Lorenz A.",
    family = "Kapsner",
    email = "lorenz.kapsner@gmail.com",
    role = c('cre', 'aut', 'cph'),
    comment = c(ORCID = "0000-0003-1866-860X")
  )))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.0.3.9009")
# The title of your package
my_desc$set(Title = "Machine Learning Experiments")
# The description of your package
my_desc$set(Description = paste0(
  "Provides 'R6' objects to perform parallelized hyperparameter ",
  "optimization and cross-validation. ",
  "Hyperparameter optimization can be performed with Bayesian ",
  "optimization (via 'ParBayesianOptimization' ",
  "<https://cran.r-project.org/package=ParBayesianOptimization>) and ",
  "grid search. The optimized hyperparameters can be validated using ",
  "k-fold cross-validation. Alternatively, hyperparameter optimization and ",
  "validation can be performed with nested cross-validation. ",
  "While 'mlexperiments' focuses on ",
  "core wrappers for machine learning experiments, additional learner ",
  "algorithms can be supplemented by inheriting from the provided learner ",
  "base class."
))
# The description of your package
my_desc$set("Date/Publication" = paste(as.character(Sys.time()), "UTC"))
# The urls
my_desc$set("URL", "https://github.com/kapsner/mlexperiments")
my_desc$set("BugReports",
            "https://github.com/kapsner/mlexperiments/issues")

# Vignette Builder
my_desc$set("VignetteBuilder" = "knitr")

# License
my_desc$set("License", "GPL-3")

# Testthat stuff
my_desc$set("Config/testthat/parallel" = "false")
my_desc$set("Config/testthat/edition" = "3")
# Roxygen
my_desc$set("Roxygen" = "list(markdown = TRUE)")

# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
usethis::use_gpl3_license()

# Depends
usethis::use_package("R", min_version = "2.10", type = "Depends")

# Imports
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
usethis::use_package("data.table", type = "Imports")
usethis::use_package("splitTools", type = "Imports")
usethis::use_package("parallel", type = "Imports")
usethis::use_package("progress", type = "Imports")
usethis::use_package("R6", type = "Imports")
usethis::use_package("kdry", type = "Imports")
usethis::use_package("stats", type = "Imports")

# Suggests
usethis::use_package("testthat", type = "Suggests", min_version = "3.0.1")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("ParBayesianOptimization", type = "Suggests")
usethis::use_package("datasets", type = "Suggests")
usethis::use_package("class", type = "Suggests")
usethis::use_package("rpart", type = "Suggests")
usethis::use_package("mlbench", type = "Suggests")
usethis::use_package("mlr3measures", type = "Suggests")
usethis::use_package("ggpubr", type = "Suggests")


# define remotes
remotes_append_vector <- NULL

# Development package 1
tag1 <- "cran" # e.g. "v0.1.7", "development" or "cran"
if (tag1 == "cran") {
  install.packages("splitTools")
} else{
  remotes::install_github(
    repo = "mayer79/splitTools",
    ref = tag1
  )
  add_remotes <- paste0(
    "github::mayer79/splitTools@", tag1
  )

  if (is.null(remotes_append_vector)) {
    remotes_append_vector <- add_remotes
  } else {
    remotes_append_vector <- c(remotes_append_vector, add_remotes)
  }
}

tag2 <- "cran" # e.g. "v0.1.7", "development" or "cran"
if (tag2 == "cran") {
  install.packages("kdry")
} else{
  remotes::install_github(
    repo = "kapsner/kdry",
    ref = tag2
  )
  add_remotes <- paste0(
    "github::kapsner/kdry@", tag2
  )

  if (is.null(remotes_append_vector)) {
    remotes_append_vector <- add_remotes
  } else {
    remotes_append_vector <- c(remotes_append_vector, add_remotes)
  }
}

# finally, add remotes (if required)
if (!is.null(remotes_append_vector)) {
  desc::desc_set_remotes(
    remotes_append_vector,
    file = usethis::proj_get()
  )
}


usethis::use_build_ignore("cran-comments.md")
usethis::use_build_ignore(".lintr")
usethis::use_build_ignore("tic.R")
usethis::use_build_ignore(".github")
usethis::use_build_ignore("NEWS.md")
usethis::use_build_ignore("README.md")
usethis::use_build_ignore("README.qmd")
usethis::use_build_ignore("docs")
usethis::use_build_ignore("Meta")

usethis::use_git_ignore("!NEWS.md")
usethis::use_git_ignore("!README.md")
usethis::use_git_ignore("!README.qmd")
usethis::use_git_ignore("docs")
usethis::use_git_ignore("Meta")

usethis::use_tidy_description()

quarto::quarto_render(input = "./README.qmd")

an <- autonewsmd::autonewsmd$new(repo_name = packagename)
an$generate()
an$write(force = TRUE)

# rcmdcheck::rcmdcheck(
#   args = c("--as-cran", "--no-vignettes"),
#   build_args = c("--no-build-vignettes")
# )

# nolint end

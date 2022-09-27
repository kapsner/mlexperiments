# nolint start
packagename <- "rpkgTemplate"

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
my_desc$set_version("0.0.0.9003")
# The title of your package
my_desc$set(Title = "Template for Creating R Packages")
# The description of your package
my_desc$set(Description = paste0(
  "A template repository for creating R packages."
))
# The description of your package
my_desc$set("Date/Publication" = paste(as.character(Sys.time()), "UTC"))
# The urls
my_desc$set("URL", "https://github.com/kapsner/rpkgTemplate")
my_desc$set("BugReports",
            "https://github.com/kapsner/rpkgTemplate/issues")

# Vignette Builder
my_desc$set("VignetteBuilder" = "knitr")

# License
my_desc$set("License", "GPL-3")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
usethis::use_gpl3_license()

# Depends
usethis::use_package("R", min_version = "2.10", type = "Depends")

# Imports
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
usethis::use_package("data.table", type = "Imports")
usethis::use_package("magrittr", type = "Imports")

# Suggests
usethis::use_package("testthat", type = "Suggests", min_version = "3.0.1")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("knitr", type = "Suggests")


# dev packages
# tag <- "master"
# devtools::install_github(repo = "r-lib/testthat", ref = tag, upgrade = "always")
# # https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
# desc::desc_set_remotes(paste0("github::r-lib/testthat@", tag), file = usethis::proj_get())

usethis::use_build_ignore("cran-comments.md")
usethis::use_build_ignore(".lintr")
usethis::use_build_ignore("tic.R")
usethis::use_build_ignore(".github")
usethis::use_build_ignore("NEWS.md")
usethis::use_build_ignore("README.md")
usethis::use_build_ignore("docs")
usethis::use_build_ignore("Meta")

usethis::use_git_ignore("!NEWS.md")
usethis::use_git_ignore("!README.md")
usethis::use_git_ignore("docs")
usethis::use_git_ignore("Meta")

usethis::use_tidy_description()




#badger::badge_cran_download("sjtable2df", "grand-total", "blue")
#badger::badge_cran_download("sjtable2df", "last-month", "blue")
#badger::badge_dependencies("sjtable2df")
badger::badge_github_actions(action = "R CMD Check via {tic}")
badger::badge_github_actions(action = "lint")
badger::badge_github_actions(action = "test-coverage")



# https://github.com/gitpython-developers/GitPython/issues/1016#issuecomment-1104114129
# system(
#  command = paste0("git config --global --add safe.directory ", getwd())
# )

# create NEWS.md using the python-package "auto-changelog" (must be installed)
# https://www.conventionalcommits.org/en/v1.0.0/
# build|ci|docs|feat|fix|perf|refactor|test
# system(
#   command = 'auto-changelog -u -t "sjtable2df NEWS" --tag-prefix "v" -o "NEWS.md"'
# )
an <- autonewsmd::autonewsmd$new(repo_name = packagename)
an$generate()
an$write()

# nolint end

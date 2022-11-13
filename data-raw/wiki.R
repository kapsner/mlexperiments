# nolint start

output_dir <- "../mlexperiments.wiki"
url <- "https://github.com/kapsner/mlexperiments.wiki.git"

tryCatch({
  git2r::clone(
    url = url,
    local_path = output_dir
  )
}, error = function(e) {
  message(e)
  git2r::pull(
    repo = output_dir
  )
})


file_list <- list.files(
  path = "vignettes",
  pattern = "\\.Rmd$"
)

for (rmd_file in file_list) {
  rmd_path <- file.path("vignettes", rmd_file)
  # prepare wiki
  rmarkdown::render(
    input = rmd_path,
    output_format = rmarkdown::md_document(
      variant = "gfm",
      toc = TRUE,
      toc_depth = 3,
      standalone = TRUE
    ),
    output_dir = output_dir
  )
}

# nolint end

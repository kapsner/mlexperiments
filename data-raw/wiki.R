# nolint start

output_dir <- "../mlexperiments.wiki"

git2r::clone(
  url = "https://github.com/kapsner/mlexperiments.wiki.git",
  local_path = output_dir
)

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

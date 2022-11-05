# nolint start

output_dir <- "data-raw/"
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

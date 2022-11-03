# nolint start

# prepare wiki
rmarkdown::render(
  input = "vignettes/mlexperiments_starter.Rmd",
  output_format = rmarkdown::md_document(
    variant = "gfm",
    toc = TRUE,
    toc_depth = 3,
    standalone = TRUE
  ),
  output_dir = "data-raw/"
)

# nolint end

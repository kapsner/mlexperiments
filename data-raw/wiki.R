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
  pattern = "\\.qmd$"
)

for (rmd_file in file_list) {
  rmd_path <- file.path("vignettes", rmd_file)
  md_file <- gsub("\\.q", ".", rmd_file)
  # prepare wiki
  quarto::quarto_render(
    input = rmd_path,
    output_format = "md"
  )
  cur_md_path <- file.path("vignettes", md_file)
  file.copy(
    from = cur_md_path,
    to = file.path(output_dir, md_file),
    overwrite = TRUE
  )
  file.remove(cur_md_path)
}
# nolint end

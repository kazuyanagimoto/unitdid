
files_orig <- file.path(
  list.files(path = "vignettes/", pattern = "*\\.Rmd\\.orig",
             recursive = TRUE, full.names = TRUE))

for (file_orig in files_orig) {
  knitr::knit(file_orig, tools::file_path_sans_ext(file_orig))
}


source_image_path <- here::here("figure")
target_image_path <- here::here("vignettes/figure")

if (dir.exists(source_image_path)) {
  if (dir.exists(target_image_path)) {
    unlink(target_image_path, recursive = TRUE)
  }
  success <- file.rename(source_image_path, target_image_path)
} else {
  success <- FALSE
}



# Clean up if successful --------------------------------------------------

if (!all(success)) {
  stop("Image files were not successfully transferred to vignettes directory")
} else {
  message("Image files were successfully transferred to vignettes directory")
}


files_orig <- file.path(
  list.files(path = "vignettes/", pattern = "*\\.Rmd\\.orig",
             recursive = TRUE, full.names = TRUE))

for (file_orig in files_orig) {
  file_path_rmd <- tools::file_path_sans_ext(file_orig)
  knitr::knit(file_orig, output = file_path_rmd)

  dir_path <- dirname(file_path_rmd)
  source_image_path <- "figure"
  target_image_path <- paste0(dir_path, "/figure")

  if (dir.exists(source_image_path)) {
    if (dir.exists(target_image_path)) {
      unlink(target_image_path, recursive = TRUE)
    }
    success <- file.rename(source_image_path, target_image_path)
  } else {
    success <- FALSE
  }

  if (!all(success)) {
    stop("Image files were not successfully transferred to vignettes directory")
  } else {
    message("Image files were successfully transferred to vignettes directory")
  }
}

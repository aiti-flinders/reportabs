read_absdata <- function(name = NULL,
                         export_dir = tempdir(),
                         .validate_name = TRUE) {

  base_url <- "https://github.com/aiti-flinders/aitidata/raw/master/data/"
  url <- paste0(base_url, name, ".rda")

  out_path <- file.path(export_dir, paste0(name, ".rda"))

  if (!file.exists(out_path)) {

    tryCatch(
        download.file(url,
                      destfile = out_path,
                      mode = "wb"),
        error = "Download failed."
      )



  } else {
    message("Reading ", name, " file found in ", export_dir)
  }

  load(out_path)

  d <- get(name)

  return(d)
}

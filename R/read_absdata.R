#' Read tidied ABS data from the aitidata GitHub repository.
#'
#' @param name The name, as a string, of the dataset to download.
#' @param refresh FALSE (the default) will load a cached version of the data if it has already been downloaded.
#' Specify TRUE to force the data to be downloaded again.
#' @param export_dir The directory to save downloaded data in. Defaults to a temporary directory.
#' @param .validate_name NYI
#'
#' @return data
#' @export
#'
read_absdata <- function(name = NULL,
                         refresh = FALSE,
                         export_dir = tempdir(),
                         .validate_name = TRUE) {

  base_url <- "https://github.com/aiti-flinders/aitidata/raw/master/data/"
  url <- paste0(base_url, name, ".rda")

  out_path <- file.path(export_dir, paste0(name, ".rda"))

  if (!file.exists(out_path) | refresh) {

    tryCatch(
        utils::download.file(url,
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

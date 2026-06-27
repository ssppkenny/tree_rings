#' Read tree-ring measurement files robustly
#'
#' Reads all .rwl files in a directory using \code{read.tucson()} with
#' \code{long = TRUE} as fallback. Skips empty files and \code{-noaa.rwl}
#' metadata companion files.
#'
#' @param dir Character. Path to the directory containing .rwl files.
#'
#' @return A named list of \code{data.frame} objects (class \code{rwl}).
#'   Names are the filenames without extension. Files that fail to read
#'   are omitted with a warning.
#' @export
#'
#' @examples
#' \dontrun{
#' rwl_list <- read_treerings("treerings")
#' }
read_treerings <- function(dir) {
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  }

  files <- list.files(dir, pattern = "\\.rwl$", full.names = TRUE, recursive = TRUE)
  files <- files[!grepl("-noaa\\.rwl$", files)]

  if (length(files) == 0) {
    stop("No .rwl files found in: ", dir)
  }

  result <- list()

  for (f in files) {
    if (file.info(f)$size == 0) {
      warning("Skipping empty file: ", basename(f))
      next
    }

    name <- file_path_sans_ext(basename(f))
    rwl <- tryCatch(
      read.tucson(f),
      error = function(e) {
        tryCatch(
          read.tucson(f, long = TRUE),
          error = function(e2) {
            warning("Failed to read ", basename(f), ": ", e2$message)
            return(NULL)
          }
        )
      }
    )

    if (!is.null(rwl)) {
      result[[name]] <- rwl
    }
  }

  invisible(result)
}

#' Download tree-ring measurement files from NOAA ITRDB
#'
#' Downloads .rwl files from the NOAA International Tree-Ring Data Bank
#' into a local directory. The directory structure mirrors the URL layout
#' (e.g. \code{africa/alge001.rwl}, \code{northamerica/canada/cana001.rwl}).
#' Uses parallel HTTP downloads via \code{curl::multi_download()} for speed.
#'
#' @param url Character. Base URL of the NOAA treering measurements page.
#' @param dest_dir Character. Path to destination directory. Created if it
#'   does not exist. Existing files are skipped.
#' @param timeout Integer. Download timeout in seconds. Default 14400 (4 hours).
#'
#' @return Invisibly returns the number of new files downloaded.
#' @export
#'
#' @examples
#' \dontrun{
#' download_treerings(dest_dir = "treerings")
#' }
download_treerings <- function(url = "https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/",
                                dest_dir = "treerings",
                                timeout = 14400) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Collect all remote .rwl paths recursively, preserving relative structure
  collect_rwl <- function(dir_url, rel_path = "") {
    page <- tryCatch(read_html(dir_url), error = function(e) return(NULL))
    if (is.null(page)) return(character(0))

    tables <- html_table(page, fill = TRUE)
    df <- tables[[1]]
    df <- df[!df$Name %in% c("", "../"), ]
    if (nrow(df) == 0) return(character(0))

    rwl <- df$Name[endsWith(df$Name, ".rwl")]
    subdirs <- df$Name[endsWith(df$Name, "/")]

    local <- if (nchar(rel_path) > 0) file.path(rel_path, rwl) else rwl
    src <- paste0(dir_url, rwl)

    for (s in subdirs) {
      s_name <- sub("/$", "", s)
      sub_rel <- if (nchar(rel_path) > 0) file.path(rel_path, s_name) else s_name
      sub_result <- collect_rwl(paste0(dir_url, s), sub_rel)
      if (length(sub_result) > 0) {
        src <- c(src, sub_result)
      }
    }
    src
  }

  # Get top-level continents
  top <- tryCatch(read_html(url), error = function(e) {
    stop("Failed to read URL: ", url, "\n", e$message)
  })
  top_tables <- html_table(top, fill = TRUE)
  top_df <- top_tables[[1]]
  top_df <- top_df[endsWith(top_df$Name, "/"), ]
  skiplist <- c("correlation-stats/", "supplemental/", "zhao2018/",
                 "miramont2025/", "miramont2025b/")
  top_df <- top_df[!top_df$Name %in% skiplist, ]

  all_src <- character(0)
  all_dest <- character(0)

  for (cont in top_df$Name) {
    cont_name <- sub("/$", "", cont)
    src_list <- collect_rwl(paste0(url, cont), cont_name)
    if (length(src_list) == 0) next

    # Derive local path from URL: remove base URL prefix
    for (s in src_list) {
      rel <- sub(url, "", s, fixed = TRUE)
      dest <- file.path(dest_dir, rel)
      if (file.exists(dest) && file.info(dest)$size > 0) next
      all_src <- c(all_src, s)
      all_dest <- c(all_dest, dest)
    }
  }

  cat("\nFound", length(all_src), "files to download\n")

  if (length(all_src) == 0) {
    cat("Nothing to download.\n")
    return(invisible(0L))
  }

  # Create destination directories
  for (d in unique(dirname(all_dest))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # Download in batches to avoid overwhelming the server
  batch_size <- 500L
  total_ok <- 0L
  pending <- seq_along(all_src)
  remaining <- data.frame(src = all_src, dest = all_dest, stringsAsFactors = FALSE)

  while (nrow(remaining) > 0) {
    batch_idx <- seq_len(min(batch_size, nrow(remaining)))
    batch <- remaining[batch_idx, ]
    # Skip any dest paths that are directories
    is_dir <- dir.exists(batch$dest)
    if (any(is_dir)) {
      batch <- batch[!is_dir, ]
      if (nrow(batch) == 0) {
        remaining <- remaining[-batch_idx, , drop = FALSE]
        next
      }
    }
    cat("Downloading batch", batch_idx[1], "-", tail(batch_idx, 1),
        "of", length(all_src), "...\n")

    res <- multi_download(
      urls = batch$src,
      destfiles = batch$dest,
      timeout = timeout
    )

    ok <- which(res$status_code >= 200 & res$status_code < 400)
    total_ok <- total_ok + length(ok)
    cat("  OK:", length(ok), "\n")

    # Keep failed files for retry
    failed_idx <- which(!(res$status_code >= 200 & res$status_code < 400) |
                         is.na(res$status_code))
    dir_idx <- dir.exists(batch$dest[failed_idx])
    if (length(failed_idx) > 0) {
      remaining <- remaining[batch_idx[failed_idx[!dir_idx]], , drop = FALSE]
      cat("  Retrying", nrow(remaining), "files...\n")
      # Also remove zero-byte files from failed downloads
      for (zf in batch$dest[failed_idx]) {
        if (file.exists(zf) && file.info(zf)$size == 0) file.remove(zf)
      }
    } else {
      remaining <- remaining[-batch_idx, , drop = FALSE]
    }
  }

  cat("Downloaded", total_ok, "files successfully.\n")

  invisible(total_ok)
}

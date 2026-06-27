#' Build a wide-format tree-ring measurements table
#'
#' Reads all .rwl files in a directory and creates a wide-format data frame
#' where each row is a core from a site and each year is a column.
#' Values of 999, -999, and 9990 are converted to NA.
#'
#' @param dir Character. Path to the directory containing .rwl files.
#' @param sites Data.frame. Optional pre-computed sites table from
#'   \code{extract_sites()}. If not provided, it is computed internally.
#' @param rwl_list List. Optional pre-read rwl list from \code{read_treerings()}.
#' @param fix Logical. Automatically detect and correct decimal outliers.
#'
#' @return A \code{data.frame} with columns:
#'   \code{site_id}, \code{core_id}, \code{species}, \code{lat}, \code{lon},
#'   \code{elevation}, \code{min_year}, \code{max_year}, and one column per year.
#' @export
#'
#' @examples
#' \dontrun{
#' meas <- build_measurements("treerings")
#' }
build_measurements <- function(dir, sites = NULL, rwl_list = NULL, fix = TRUE) {
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  }

  if (is.null(sites)) {
    sites <- extract_sites(dir, rwl_list = rwl_list)
  }

  files <- list.files(dir, pattern = "\\.rwl$", full.names = TRUE, recursive = TRUE)
  files <- files[!grepl("-noaa\\.rwl$", files)]
  names(files) <- file_path_sans_ext(basename(files))

  all_cores <- list()

  for (fname in names(files)) {
    f <- files[fname]
    if (file.info(f)$size == 0) next

    if (!is.null(rwl_list) && fname %in% names(rwl_list)) {
      rwl <- rwl_list[[fname]]
    } else {
      rwl <- tryCatch(
        read.tucson(f),
        error = function(e) tryCatch(read.tucson(f, long = TRUE), error = function(e2) NULL)
      )
    }
    if (is.null(rwl)) next
    if (fix) rwl <- fix_outliers(rwl)

    site_info <- sites[sites$filename == fname, ]
    if (nrow(site_info) == 0) next

    years <- as.integer(rownames(rwl))
    min_y <- min(years)
    max_y <- max(years)

    for (core in colnames(rwl)) {
      vals <- rwl[[core]]
      vals[vals %in% c(999, -999, 9990, -9990)] <- NA

      row_data <- list(
        site_id = site_info$site_id,
        core_id = paste0(fname, "_", core),
        species = site_info$species,
        lat = site_info$lat,
        lon = site_info$lon,
        elevation = site_info$elevation,
        min_year = min_y,
        max_year = max_y
      )

      for (y in years) {
        row_data[[as.character(y)]] <- vals[years == y]
      }

      all_cores[[length(all_cores) + 1]] <- row_data
    }
  }

  result <- bind_rows(all_cores)

  na_cols <- names(result)
  result[na_cols] <- lapply(result[na_cols], function(col) {
    if (is.numeric(col)) {
      col[is.nan(col)] <- NA
    }
    col
  })

  result
}


#' Build a long-format tree-ring measurements table
#'
#' Alternative to \code{build_measurements()} that returns data in
#' long (tidy) format with one row per site-core-year combination.
#'
#' @param dir Character. Path to the directory containing .rwl files.
#' @param sites Data.frame. Optional pre-computed sites table.
#'
#' @return A \code{data.frame} with columns:
#'   \code{site_id}, \code{core_id}, \code{species}, \code{lat}, \code{lon},
#'   \code{elevation}, \code{year}, \code{ring_width}.
#' @export
#'
#' @examples
#' \dontrun{
#' meas_long <- build_measurements_long("treerings")
#' }
build_measurements_long <- function(dir, sites = NULL, rwl_list = NULL) {
  wide <- build_measurements(dir, sites, rwl_list)
  year_cols <- setdiff(names(wide), c("site_id", "core_id", "species", "lat",
                                       "lon", "elevation", "min_year", "max_year"))

  long <- pivot_longer(wide,
                        cols = year_cols,
                        names_to = "year",
                        values_to = "ring_width")
  long$year <- as.integer(long$year)
  long <- long[!is.na(long$ring_width), ]
  long
}

#' Fix suspected decimal-error outliers in tree-ring series
#'
#' Detects extreme values per core using the IQR rule and attempts to
#' correct them by dividing by 10 (the most common data-entry error in
#' Tucson-format files where a decimal point is omitted or shifted).
#'
#' A value is flagged as an outlier if it exceeds Q3 + 3 * IQR for that
#' core. The correction is accepted only if the adjusted value falls
#' within a reasonable range (Q1 – 1.5*IQR to Q3 + 1.5*IQR).
#'
#' @param rwl A data.frame of ring widths (an rwl object).
#' @param quiet Logical. Suppress messages about corrected values.
#' @return The rwl object with corrected values.
fix_outliers <- function(rwl, quiet = TRUE) {
  for (core in colnames(rwl)) {
    vals <- rwl[[core]]
    ok <- !is.na(vals) & !vals %in% c(0, 999, -999, 9990, -9990)
    if (sum(ok) < 10) next
    qs <- quantile(vals[ok], c(0.25, 0.75), na.rm = TRUE)
    iqr <- qs[2] - qs[1]
    upper <- qs[2] + 10 * iqr
    candidates <- which(vals > upper & vals > 5 & ok)
    if (length(candidates) == 0) next
    for (idx in candidates) {
      corrected <- vals[idx] / 10
      med <- median(vals[ok])
      if (corrected >= med - 3 * iqr && corrected <= med + 3 * iqr) {
        if (!quiet) cat("  Fixed", core, "year", rownames(rwl)[idx],
                        ":", vals[idx], "->", corrected, "\n")
        rwl[idx, core] <- corrected
      }
    }
  }
  rwl
}


#' Plot a spaghetti plot of all cores at a site
#'
#' Each core is shown as a separate line with ring width over time.
#' Outliers (likely decimal-point errors) are automatically fixed.
#'
#' @param site_name Character. The site ID (filename without extension).
#' @param dir Character. Path to directory containing .rwl files.
#' @param fix Logical. Automatically detect and correct decimal outliers.
#' @param ... Additional arguments passed to \code{ggplot2::labs()}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spaghetti("alge001", "treerings")
#' }
plot_spaghetti <- function(site_name, dir, fix = TRUE, ...) {
  f <- list.files(dir, pattern = paste0(site_name, "\\.rwl$"), full.names = TRUE, recursive = TRUE)
  if (length(f) == 0) stop("File not found: ", site_name, ".rwl in ", dir)
  f <- f[!grepl("-noaa\\.rwl$", f)][1]

  rwl <- tryCatch(
    read.tucson(f),
    error = function(e) tryCatch(read.tucson(f, long = TRUE), error = function(e2) NULL)
  )
  if (is.null(rwl)) {
    stop("Failed to read: ", f)
  }

  if (fix) rwl <- fix_outliers(rwl)

  years <- as.integer(rownames(rwl))
  long <- data.frame(year = rep(years, ncol(rwl)),
                     core = rep(colnames(rwl), each = length(years)),
                     width = as.vector(as.matrix(rwl)),
                     stringsAsFactors = FALSE)

  long <- long[!is.na(long$width), ]
  long$width[long$width %in% c(999, -999, 9990, -9990)] <- NA
  long <- long[!is.na(long$width), ]

  p <- ggplot(long, aes(x = year, y = width, color = core)) +
    geom_line(na.rm = TRUE) +
    labs(x = "Year", y = "Ring Width (mm)", title = site_name, ...) +
    theme_minimal() +
    guides(color = guide_legend(title = "Core"))

  p
}


#' Plot a mean chronology for a site
#'
#' Shows the mean ring width per year with a ribbon for +/- 1 standard
#' deviation and a line for the sample depth (number of cores).
#'
#' @param site_name Character. The site ID (filename without extension).
#' @param dir Character. Path to directory containing .rwl files.
#' @param ... Additional arguments passed to \code{ggplot2::labs()}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_chronology("alge001", "treerings")
#' }
plot_chronology <- function(site_name, dir, fix = TRUE, ...) {
  f <- list.files(dir, pattern = paste0(site_name, "\\.rwl$"), full.names = TRUE, recursive = TRUE)
  if (length(f) == 0) stop("File not found: ", site_name, ".rwl in ", dir)
  f <- f[!grepl("-noaa\\.rwl$", f)][1]

  rwl <- tryCatch(
    read.tucson(f),
    error = function(e) tryCatch(read.tucson(f, long = TRUE), error = function(e2) NULL)
  )
  if (is.null(rwl)) {
    stop("Failed to read: ", f)
  }

  if (fix) rwl <- fix_outliers(rwl)
  rwl[rwl %in% c(999, -999, 9990, -9990)] <- NA

  years <- as.integer(rownames(rwl))
  n_cores <- ncol(rwl)
  mean_val <- rowMeans(rwl, na.rm = TRUE)
  sd_val <- apply(rwl, 1, sd, na.rm = TRUE)
  sample_depth <- rowSums(!is.na(rwl))

  df <- data.frame(year = years, mean = mean_val, sd = sd_val,
                   sample = sample_depth, stringsAsFactors = FALSE)

  p <- ggplot(df, aes(x = year)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) +
    geom_line(aes(y = mean), color = "darkblue", linewidth = 1) +
    labs(x = "Year", y = "Ring Width (mm)", title = site_name,
         subtitle = paste(n_cores, "cores"), ...) +
    theme_minimal()

  p
}


#' Plot a map of all sites
#'
#' Creates a map showing all sites as points colored by species with
#' size proportional to number of cores.
#'
#' @param sites Data.frame. A sites table from \code{extract_sites()}.
#' @param ... Additional arguments passed to \code{ggplot2::labs()}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' sites <- extract_sites("treerings")
#' plot_site_map(sites)
#' }
plot_site_map <- function(sites, ...) {
  sites <- sites[!is.na(sites$lat) & !is.na(sites$lon), ]

  p <- ggplot(sites, aes(x = lon, y = lat, color = species, size = n_cores)) +
    geom_point(alpha = 0.7) +
    labs(x = "Longitude", y = "Latitude",
         title = "Tree Ring Sites",
         subtitle = paste(nrow(sites), "sites"), ...) +
    theme_minimal()

  p
}

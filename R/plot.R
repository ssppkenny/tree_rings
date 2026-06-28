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
#' size proportional to number of cores. When \code{continent} is specified,
#' sites are filtered to that continent and a geographic map background
#' is added using the \code{maps} package (must be installed separately).
#'
#' @param sites Data.frame. A sites table from \code{extract_sites()} or
#'   \code{query_sites()}.
#' @param continent Character. Optional continent name to filter and zoom
#'   (e.g. \code{"europe"}). Case-insensitive.
#' @param ... Additional arguments passed to \code{ggplot2::labs()}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' sites <- extract_sites("treerings")
#' plot_site_map(sites)
#' plot_site_map(sites, continent = "europe")
#' }
plot_site_map <- function(sites, continent = NULL, ...) {
  sites <- sites[!is.na(sites$lat) & !is.na(sites$lon), ]

  if (!is.null(continent)) {
    sites <- sites[tolower(sites$continent) == tolower(continent), ]
    if (nrow(sites) == 0) stop("No sites found for continent: ", continent)
  }

  p <- ggplot(sites, aes(x = lon, y = lat, color = species, size = n_cores))

  if (!is.null(continent)) {
    world <- if (requireNamespace("maps", quietly = TRUE)) {
      tryCatch(ggplot2::map_data("world"), error = function(e) NULL)
    } else NULL
    if (!is.null(world)) {
      p <- p + geom_polygon(data = world,
                             aes(x = long, y = lat, group = group),
                             fill = "gray90", color = "gray50",
                             linewidth = 0.2, inherit.aes = FALSE)
    }
  }

  n_species <- length(unique(sites$species))
  legend_cols <- if (n_species > 20) ceiling(sqrt(n_species)) else 1

  p <- p + geom_point(alpha = 0.7) +
    scale_size_area(max_size = 3) +
    guides(color = guide_legend(ncol = legend_cols,
                                override.aes = list(size = 2, alpha = 1)),
           size = guide_legend(override.aes = list(alpha = 1))) +
    labs(x = "Longitude", y = "Latitude",
         title = if (!is.null(continent)) paste0(continent, " Tree Ring Sites")
                 else "Tree Ring Sites",
         subtitle = paste(nrow(sites), "sites"), ...) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 7))

  if (!is.null(continent)) {
    x_pad <- max(diff(range(sites$lon, na.rm = TRUE)) * 0.1, 1)
    y_pad <- max(diff(range(sites$lat, na.rm = TRUE)) * 0.1, 1)
    p <- p + coord_quickmap(
      xlim = range(sites$lon, na.rm = TRUE) + c(-x_pad, x_pad),
      ylim = range(sites$lat, na.rm = TRUE) + c(-y_pad, y_pad)
    )
  }

  p
}


#' Plot concentric tree rings for a single core
#'
#' Creates a cross-section-style plot showing the tree rings of a single
#' core as concentric circles. Each ring's radius is proportional to the
#' cumulative ring width from the center outward.
#'
#' Data can come from a DuckDB connection (preferred) or from a directory
#' of .rwl files. If both \code{con} and \code{dir} are provided,
#' \code{con} takes precedence.
#'
#' @param site_name Character. The site ID.
#' @param con A DuckDB connection from \code{connect_treerings()}. If
#'   provided, data is queried from the database.
#' @param dir Character. Path to directory containing .rwl files. Used
#'   as fallback when \code{con} is NULL.
#' @param core_id Character. Optional core column name. If NULL, uses the
#'   first core found for the site.
#' @param ... Additional arguments passed to \code{ggplot2::labs()}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # From DuckDB
#' con <- connect_treerings("treerings.duckdb")
#' plot_rings("alge001", con = con)
#'
#' # From files
#' plot_rings("alge001", dir = "treerings")
#' }
plot_rings <- function(site_name, con = NULL, dir = NULL, core_id = NULL, ...) {
  if (!is.null(con)) {
    cores <- DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT core_id FROM measurements WHERE site_id = '",
      site_name, "' ORDER BY core_id"
    ))$core_id
    if (length(cores) == 0) stop("Site '", site_name, "' not found in database")
    if (is.null(core_id)) core_id <- cores[1]
    if (!core_id %in% cores) stop("Core '", core_id, "' not found for site ", site_name)
    meas <- DBI::dbGetQuery(con, paste0(
      "SELECT year, ring_width FROM measurements WHERE core_id = '",
      core_id, "' ORDER BY year"
    ))
    vals <- meas$ring_width
    years <- meas$year
  } else if (!is.null(dir)) {
    f <- list.files(dir, pattern = paste0(site_name, ".rwl$"),
                    full.names = TRUE, recursive = TRUE)
    if (length(f) == 0) stop("File not found: ", site_name, ".rwl in ", dir)
    f <- f[!grepl("-noaa.rwl$", f)][1]
    rwl <- tryCatch(
      read.tucson(f),
      error = function(e) tryCatch(read.tucson(f, long = TRUE), error = function(e2) NULL)
    )
    if (is.null(rwl)) stop("Failed to read: ", f)
    if (is.null(core_id)) core_id <- colnames(rwl)[1]
    if (!core_id %in% colnames(rwl)) stop("Core '", core_id, "' not found in ", site_name)
    vals <- rwl[[core_id]]
    years <- as.integer(rownames(rwl))
    vals[vals %in% c(999, -999, 9990, -9990)] <- NA
  } else {
    stop("Provide either a DuckDB connection (con) or a data directory (dir)")
  }

  ok <- !is.na(vals) & vals > 0
  if (sum(ok) < 2) stop("Not enough valid measurements for core: ", core_id)
  vals <- vals[ok]
  years <- years[ok]

  radii <- cumsum(vals)
  n_rings <- length(radii)
  n_pts <- 200
  theta <- seq(0, 2 * pi, length.out = n_pts)

  ring_list <- list()
  for (i in seq_len(n_rings)) {
    r_outer <- radii[i]
    r_inner <- if (i == 1) 0 else radii[i - 1]
    ring_list[[i]] <- data.frame(
      x = c(r_outer * cos(theta), r_inner * rev(cos(theta))),
      y = c(r_outer * sin(theta), r_inner * rev(sin(theta))),
      ring = i,
      year = years[i]
    )
  }
  rings <- do.call(rbind, ring_list)

  p <- ggplot(rings, aes(x = x, y = y)) +
    geom_polygon(aes(fill = ring %% 2 == 0, group = ring), color = NA) +
    scale_fill_manual(values = c("TRUE" = "#8B7355", "FALSE" = "#D2B48C"),
                      guide = "none") +
    coord_fixed() +
    labs(title = paste0(site_name, " (", core_id, ")"),
         subtitle = paste(years[1], "-", years[n_rings], " | ", n_rings, " rings"),
         ...) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  p
}

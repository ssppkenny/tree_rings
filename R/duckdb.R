#' Parse all .rwl files into a DuckDB database
#'
#' Reads all .rwl files in a directory in parallel using multiple CPU cores,
#' extracts metadata and ring-width measurements, and stores them in a DuckDB
#' database for fast querying.
#'
#' The database contains two tables:
#' \describe{
#'   \item{\code{sites}}{Site metadata — site_id, species, lat, lon, elevation, continent, n_cores, year range}
#'   \item{\code{measurements}}{Long-format ring widths — site_id, core_id, year, ring_width}
#' }
#'
#' @param dir Character. Path to directory containing .rwl files.
#' @param db_path Character. Path for the output DuckDB database file.
#' @param cores Integer. Number of CPU cores for parallel parsing.
#'   Defaults to \code{parallel::detectCores() - 1}.
#' @param quiet Logical. Suppress progress messages. Default FALSE.
#'
#' @return Invisibly returns the DuckDB connection.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- parse_to_duckdb("treerings", "treerings.duckdb")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
parse_to_duckdb <- function(dir, db_path = "treerings.duckdb",
                             cores = max(1, parallel::detectCores() - 1),
                             quiet = FALSE) {
  if (!dir.exists(dir)) stop("Directory does not exist: ", dir)
  dir <- normalizePath(dir)

  # Find all .rwl data files recursively
  files <- list.files(dir, pattern = "\\.rwl$", full.names = TRUE, recursive = TRUE)
  files <- files[!grepl("-noaa\\.rwl$", files)]
  if (length(files) == 0) stop("No .rwl files found in: ", dir)

  if (!quiet) cat("Found", length(files), "files, processing on", cores, "cores\n")

  # Split files into chunks for parallel processing
  chunks <- split(files, seq_along(files) %% cores)

  # Process each chunk in parallel, writing to temp DuckDB files
  temp_dir <- tempfile("treeringr_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  results <- parallel::mclapply(seq_along(chunks), function(i) {
    chunk <- chunks[[i]]
    worker_db <- file.path(temp_dir, paste0("worker", i, ".duckdb"))

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = worker_db)

    DBI::dbExecute(con, "CREATE TABLE measurements (
      site_id VARCHAR, core_id VARCHAR, year INTEGER, ring_width DOUBLE
    )")
    DBI::dbExecute(con, "CREATE TABLE sites (
      site_id VARCHAR PRIMARY KEY, site_name VARCHAR, species VARCHAR,
      species_code VARCHAR, lat DOUBLE, lon DOUBLE, elevation DOUBLE,
      continent VARCHAR, n_cores INTEGER, min_year INTEGER, max_year INTEGER
    )")

    site_rows <- list()
    meas_chunks <- list()
    chunk_size <- 5000L

    for (f in chunk) {
      base_name <- tools::file_path_sans_ext(basename(f))
      noaa_file <- sub("\\.rwl$", "-noaa.rwl", f)

      rwl <- tryCatch(
        read.tucson(f),
        error = function(e) tryCatch(
          read.tucson(f, long = TRUE),
          error = function(e2) NULL
        )
      )
      if (is.null(rwl)) {
        if (!quiet) cat("  FAILED:", basename(f), "\n")
        next
      }

      # Fix decimal-point outliers before storing
      rwl <- fix_outliers(rwl, quiet = quiet)

      # Metadata extraction
      site_name <- NA_character_
      species <- NA_character_
      species_code <- NA_character_
      lat <- NA_real_
      lon <- NA_real_
      elevation <- NA_real_

      if (file.exists(noaa_file)) {
        meta <- read_noaa_metadata(noaa_file)
        site_name <- if (!is.na(meta$site_name)) meta$site_name else NA_character_
        species <- if (!is.na(meta$species)) meta$species else species
        species_code <- meta$species_code
        lat <- if (!is.na(meta$lat)) meta$lat else lat
        lon <- if (!is.na(meta$lon)) meta$lon else lon
        elevation <- if (!is.na(meta$elevation)) meta$elevation else elevation
      }

      if (is.na(species) || is.na(lat) || is.na(lon)) {
        hdr <- read_raw_header(f)
        if (is.na(species)) species <- hdr$species
        if (is.na(species_code)) species_code <- hdr$species_code
        if (is.na(lat)) lat <- hdr$lat
        if (is.na(lon)) lon <- hdr$lon
        if (is.na(elevation)) elevation <- hdr$elevation
      }

      years <- as.integer(rownames(rwl))
      n_cores <- ncol(rwl)
      continent <- file_continent(f, dir)

      # Build measurements in long format
      for (core in colnames(rwl)) {
        vals <- rwl[[core]]
        vals[vals %in% c(999, -999, 9990, -9990)] <- NA
        ok <- !is.na(vals)
        if (!any(ok)) next
        meas_chunks[[length(meas_chunks) + 1]] <- data.frame(
          site_id = base_name,
          core_id = paste0(base_name, "_", core),
          year = years[ok],
          ring_width = vals[ok],
          stringsAsFactors = FALSE
        )
      }

      site_rows[[base_name]] <- data.frame(
        site_id = base_name, site_name = site_name,
        species = species, species_code = species_code,
        lat = lat, lon = lon, elevation = elevation,
        continent = continent,
        n_cores = n_cores, min_year = min(years), max_year = max(years),
        stringsAsFactors = FALSE
      )

      # Bulk insert measurements periodically
      if (length(meas_chunks) >= chunk_size) {
        combined <- do.call(rbind, meas_chunks)
        dbAppendTable(con, "measurements", combined)
        meas_chunks <- list()
      }
    }

    # Flush remaining measurements
    if (length(meas_chunks) > 0) {
      combined <- do.call(rbind, meas_chunks)
      duckdb::dbAppendTable(con, "measurements", combined)
    }

    # Write sites
    sites_df <- do.call(rbind, site_rows)
    duckdb::dbAppendTable(con, "sites", sites_df)

    # Return worker DB path for merging
    worker_db
  }, mc.cores = cores, mc.preschedule = FALSE)

  # Merge all worker databases into the main one
  if (!quiet) cat("Merging worker databases...\n")
  main_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  DBI::dbExecute(main_con, "CREATE TABLE measurements (
    site_id VARCHAR, core_id VARCHAR, year INTEGER, ring_width DOUBLE
  )")
  DBI::dbExecute(main_con, "CREATE TABLE sites (
    site_id VARCHAR, site_name VARCHAR, species VARCHAR,
    species_code VARCHAR, lat DOUBLE, lon DOUBLE, elevation DOUBLE,
    continent VARCHAR, n_cores INTEGER, min_year INTEGER, max_year INTEGER
  )")

  for (wb in results) {
    if (is.null(wb) || !file.exists(wb)) next
    wb_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = wb, read_only = TRUE)

    DBI::dbExecute(main_con, paste0("ATTACH '", wb, "' AS w"))
    DBI::dbExecute(main_con, "INSERT INTO measurements SELECT * FROM w.measurements")
    DBI::dbExecute(main_con, "INSERT INTO sites SELECT * FROM w.sites")
    DBI::dbExecute(main_con, "DETACH w")

    DBI::dbDisconnect(wb_con, shutdown = TRUE)
    unlink(wb, recursive = TRUE)
  }

  DBI::dbExecute(main_con, "CREATE INDEX idx_meas_site ON measurements(site_id)")
  DBI::dbExecute(main_con, "CREATE INDEX idx_meas_year ON measurements(year)")

  n_sites <- DBI::dbGetQuery(main_con, "SELECT COUNT(*) FROM sites")[1, 1]
  n_meas <- DBI::dbGetQuery(main_con, "SELECT COUNT(*) FROM measurements")[1, 1]
  if (!quiet) cat("Done. Sites:", n_sites, "| Measurements:", n_meas, "rows\n")

  invisible(main_con)
}


#' Connect to an existing treeringr DuckDB database
#'
#' Opens a read-only connection to a DuckDB database created by
#' \code{parse_to_duckdb()}.
#'
#' @param db_path Character. Path to the DuckDB database file.
#' @return A DuckDB connection object.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_treerings("treerings.duckdb")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
connect_treerings <- function(db_path = "treerings.duckdb") {
  if (!file.exists(db_path)) stop("Database not found: ", db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  con
}


#' Query site chronology from DuckDB
#'
#' Computes the mean ring-width chronology for one or more sites
#' using DuckDB's fast aggregation.
#'
#' @param con A DuckDB connection from \code{connect_treerings()}.
#' @param site_ids Character vector of site IDs to query.
#'   If NULL, all sites are included.
#' @return A data.frame with columns \code{site_id}, \code{year},
#'   \code{mean_width}, \code{sd_width}, \code{n_cores}.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_treerings("treerings.duckdb")
#' chron <- query_chronology(con, "alge001")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
query_chronology <- function(con, site_ids = NULL) {
  sql <- "SELECT site_id, year,
    AVG(ring_width) AS mean_width,
    STDDEV_SAMP(ring_width) AS sd_width,
    COUNT(DISTINCT core_id) AS n_cores
  FROM measurements"
  if (!is.null(site_ids)) {
    ids <- paste0("'", site_ids, "'", collapse = ", ")
    sql <- paste0(sql, " WHERE site_id IN (", ids, ")")
  }
  sql <- paste0(sql, " GROUP BY site_id, year ORDER BY site_id, year")
  DBI::dbGetQuery(con, sql)
}


#' Query measurements in wide format from DuckDB
#'
#' Uses DuckDB's SQL PIVOT to produce a wide table with years as columns.
#' Much faster and more memory-efficient than R-side pivoting for large data.
#'
#' @param con A DuckDB connection from \code{connect_treerings()}.
#' @param site_ids Character vector of site IDs to query.
#'   If NULL, all sites are included (warning: may be very large).
#' @return A data.frame with site_id, core_id, and one column per year.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_treerings("treerings.duckdb")
#' wide <- query_wide(con, "alge001")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
query_wide <- function(con, site_ids = NULL) {
  site_filter <- if (!is.null(site_ids)) {
    ids <- paste0("'", site_ids, "'", collapse = ", ")
    paste0(" WHERE site_id IN (", ids, ")")
  } else ""

  # Get distinct years that have data for these sites (avoids empty columns)
  years <- DBI::dbGetQuery(con, paste0(
    "SELECT DISTINCT year FROM measurements", site_filter, " ORDER BY year"
  ))$year
  if (length(years) == 0) return(data.frame())

  # Build PIVOT with literal year values
  year_list <- paste(years, collapse = ", ")
  sql <- paste0("SELECT * FROM measurements PIVOT (
    AVG(ring_width) FOR year IN (", year_list, ")
  )", site_filter)
  DBI::dbGetQuery(con, sql)
}


#' Query site metadata from DuckDB
#'
#' Returns the sites table, optionally filtered by site IDs and/or continent.
#'
#' @param con A DuckDB connection from \code{connect_treerings()}.
#' @param site_ids Character vector of site IDs. If NULL, all sites.
#' @param continent Character. Filter by continent. If NULL, all continents.
#' @return A data.frame with site metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_treerings("treerings.duckdb")
#' sites <- query_sites(con, continent = "europe")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
query_sites <- function(con, site_ids = NULL, continent = NULL) {
  sql <- "SELECT * FROM sites"
  filters <- c()
  if (!is.null(site_ids)) {
    ids <- paste0("'", site_ids, "'", collapse = ", ")
    filters <- c(filters, paste0("site_id IN (", ids, ")"))
  }
  if (!is.null(continent)) {
    filters <- c(filters, paste0("continent = '", continent, "'"))
  }
  if (length(filters) > 0) {
    sql <- paste0(sql, " WHERE ", paste(filters, collapse = " AND "))
  }
  sql <- paste0(sql, " ORDER BY site_id")
  DBI::dbGetQuery(con, sql)
}

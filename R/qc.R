#' Run cross-dating quality control on a single site
#'
#' Uses dplR's cross-dating pipeline to detect potential dating errors:
#' segment-wise correlation, per-series statistics, and flagged issues.
#'
#' @param site_name Character. Site ID (filename without extension).
#' @param dir Character. Path to directory containing .rwl files.
#' @param seg.length Integer. Segment length for cross-dating (default 50 years).
#' @param pcrit Numeric. Critical p-value for correlation flags (default 0.01).
#' @return A list with components:
#'   \describe{
#'     \item{\code{issues}}{Data.frame of flagged issues (low correlation, outliers)}
#'     \item{\code{stats}}{Per-series statistics from \code{rwl.stats()}}
#'     \item{\code{interseries_cor}}{Mean interseries correlation}
#'     \item{\code{segment_cor}}{Segment-wise correlation matrix}
#'   }
#' @export
#' @importFrom dplR rwl.stats rwl.report interseries.cor corr.rwl.seg detrend chron
#'
#' @examples
#' \dontrun{
#' qc <- qc_site("alge001", "treerings")
#' }
qc_site <- function(site_name, dir, seg.length = 50, pcrit = 0.01) {
  f <- list.files(dir, pattern = paste0(site_name, "\\.rwl$"),
                  full.names = TRUE, recursive = TRUE)
  if (length(f) == 0) stop("File not found: ", site_name, ".rwl in ", dir)
  f <- f[!grepl("-noaa\\.rwl$", f)][1]

  rwl <- tryCatch(
    read.tucson(f),
    error = function(e) tryCatch(read.tucson(f, long = TRUE), error = function(e2) NULL)
  )
  if (is.null(rwl)) stop("Failed to read: ", f)

  rwl[rwl %in% c(999, -999, 9990, -9990)] <- NA

  years <- as.integer(rownames(rwl))
  n_cores <- ncol(rwl)
  issues <- list()

  # 1. Per-series statistics
  stats <- rwl.stats(rwl)

  # 2. Interseries correlation
  is_cor <- tryCatch(
    interseries.cor(rwl, method = "spearman"),
    error = function(e) NULL
  )

  # 3. Detect zero-variance series (all same value)
  for (core in colnames(rwl)) {
    vals <- rwl[[core]]
    ok <- !is.na(vals)
    if (sum(ok) < 10) {
      issues[[length(issues) + 1]] <- data.frame(
        site = site_name, core = core, issue = "too_few_measurements",
        detail = paste(sum(ok), "non-NA years"), stringsAsFactors = FALSE
      )
      next
    }
    if (sd(vals[ok], na.rm = TRUE) == 0) {
      issues[[length(issues) + 1]] <- data.frame(
        site = site_name, core = core, issue = "zero_variance",
        detail = "All values identical", stringsAsFactors = FALSE
      )
    }
  }

  # 4. Segment-wise cross-dating (COFECHA-style)
  seg_cor <- tryCatch(
    corr.rwl.seg(rwl, seg.length = seg.length, pcrit = pcrit,
                  make.plot = FALSE, prewhiten = TRUE, biweight = TRUE),
    error = function(e) NULL
  )

  # 5. Flag series with low mean correlation
  if (!is.null(is_cor) && nrow(is_cor) > 0) {
    for (i in seq_len(nrow(is_cor))) {
      if (is_cor$res.cor[i] < 0.1) {
        issues[[length(issues) + 1]] <- data.frame(
          site = site_name, core = rownames(is_cor)[i],
          issue = "low_interseries_cor",
          detail = paste0("mean r = ", round(is_cor$res.cor[i], 3)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # 6. Flag cross-dating segments with low correlation
  if (!is.null(seg_cor) && length(seg_cor$flags) > 0) {
    for (core_name in names(seg_cor$flags)) {
      issues[[length(issues) + 1]] <- data.frame(
        site = site_name, core = core_name,
        issue = "low_segment_cor",
        detail = seg_cor$flags[core_name],
        stringsAsFactors = FALSE
      )
    }
  }

  # 7. Build result
  result <- list(
    issues = if (length(issues) > 0) bind_rows(issues) else data.frame(),
    stats = stats,
    interseries_cor = is_cor,
    segment_cor = seg_cor,
    n_cores = n_cores,
    year_range = c(min(years), max(years))
  )
  class(result) <- "treeringr_qc"
  result
}


#' Print a treeringr QC report
#'
#' @param x A \code{treeringr_qc} object from \code{qc_site()}.
#' @param ... Unused.
#' @export
print.treeringr_qc <- function(x, ...) {
  cat("QC Report\n")
  cat("  Cores:", x$n_cores, "\n")
  cat("  Years:", x$year_range[1], "-", x$year_range[2], "\n")
  if (!is.null(x$interseries_cor)) {
    cat("  Mean interseries r:",
        round(mean(x$interseries_cor$res.cor, na.rm = TRUE), 3), "\n")
  }
  if (nrow(x$issues) > 0) {
    cat("  Issues found:", nrow(x$issues), "\n")
    for (i in seq_len(min(nrow(x$issues), 20))) {
      cat("    [", x$issues$issue[i], "] ", x$issues$core[i], " - ",
          x$issues$detail[i], "\n", sep = "")
    }
    if (nrow(x$issues) > 20) cat("    ... and", nrow(x$issues) - 20, "more\n")
  } else {
    cat("  No issues found.\n")
  }
  invisible(x)
}


#' Run quality control across all sites in a DuckDB database
#'
#' Scans all sites with valid data in the database and runs cross-dating
#' QC on each. Stores results in a \code{qc_issues} table.
#'
#' @param con A DuckDB connection from \code{connect_treerings()}.
#' @param dir Character. Path to the .rwl file directory.
#' @param site_ids Character vector of site IDs to check. If NULL, checks
#'   all sites with at least 5 cores (default).
#' @param min_cores Integer. Minimum number of cores to run QC (default 5).
#' @param seg.length Integer. Segment length for cross-dating (default 50).
#' @param pcrit Numeric. Critical p-value (default 0.01).
#' @param db_path Character. Path to save the DuckDB database with QC results.
#'   If NULL, results are only returned in R.
#'
#' @return A data.frame of all QC issues found.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_treerings("treerings.duckdb")
#' issues <- qc_all(con, "treerings")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
qc_all <- function(con, dir, site_ids = NULL, min_cores = 5,
                    seg.length = 50, pcrit = 0.01, db_path = NULL) {
  if (is.null(site_ids)) {
    site_ids <- DBI::dbGetQuery(con, paste0(
      "SELECT site_id FROM sites WHERE n_cores >= ", min_cores,
      " ORDER BY site_id"
    ))$site_id
  }
  if (length(site_ids) == 0) stop("No sites to check")

  all_issues <- list()
  n_checked <- 0

  for (sid in site_ids) {
    qc <- tryCatch(
      qc_site(sid, dir, seg.length = seg.length, pcrit = pcrit),
      error = function(e) NULL
    )
    if (is.null(qc)) next
    n_checked <- n_checked + 1
    if (nrow(qc$issues) > 0) {
      all_issues[[length(all_issues) + 1]] <- qc$issues
      cat("  [ISSUES] ", sid, ": ", nrow(qc$issues), " problems\n", sep = "")
    }
    if (n_checked %% 100 == 0) cat("Checked", n_checked, "/", length(site_ids), "sites\n")
  }

  cat("\nQC complete:", n_checked, "sites checked\n")

  if (length(all_issues) == 0) {
    cat("No issues found.\n")
    return(data.frame())
  }

  result <- bind_rows(all_issues)

  if (!is.null(db_path)) {
    qc_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
    DBI::dbExecute(qc_con, "CREATE TABLE IF NOT EXISTS qc_issues (
      site VARCHAR, core VARCHAR, issue VARCHAR, detail VARCHAR
    )")
    dbAppendTable(qc_con, "qc_issues", result)
    DBI::dbDisconnect(qc_con, shutdown = TRUE)
    cat("Results saved to:", db_path, "\n")
  }

  invisible(result)
}

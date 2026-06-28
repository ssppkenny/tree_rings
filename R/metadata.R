#' Parse coordinate strings from ITRDB .rwl headers into decimal degrees
#'
#' Handles DDMM, DDDMM, DDMMSS, signed, merged, and NSEW formats.
#' @param raw Character. Raw coordinate string (lat or lon) possibly with suffix.
#' @param suffix Character. N/S or E/W suffix if present.
#' @param sign Integer. 1 or -1 for sign.
#' @return Numeric decimal degrees.
parse_coord_part <- function(raw, suffix = "", sign = 1) {
  raw <- str_trim(raw)
  if (nchar(raw) == 0) return(NA_real_)
  
  raw_digits <- gsub("[^0-9]", "", raw)
  n_digits <- nchar(raw_digits)
  
  val <- as.numeric(raw)
  if (is.na(val)) return(NA_real_)
  
  abs_val <- abs(val)
  # DDMMSS (6+ digits) or DDMM (4 digits even if leading zeros)
  # or DDDMM (5 digits)
  if (n_digits >= 6) {
    deg <- floor(abs_val / 10000)
    min <- floor((abs_val - deg * 10000) / 100)
    sec <- abs_val - deg * 10000 - min * 100
    result <- deg + min / 60 + sec / 3600
  } else if (n_digits >= 5) {
    # Try DDDMM: first 3 digits degrees, last 2 minutes
    deg3 <- floor(abs_val / 100)
    min3 <- abs_val - deg3 * 100
    if (deg3 > 180) {
      # deg > 180 is likely invalid; try DDMM with implied leading zero
      deg2 <- floor(abs_val / 1000)
      min2 <- floor((abs_val - deg2 * 1000) / 10)
      sec2 <- abs_val - deg2 * 1000 - min2 * 10
      result <- deg2 + min2 / 60 + sec2 / 3600
    } else {
      result <- deg3 + min3 / 60
    }
  } else if (n_digits >= 4 || abs_val > 100) {
    deg <- floor(abs_val / 100)
    min <- abs_val - deg * 100
    result <- deg + min / 60
  } else {
    return(val)
  }
  if (toupper(suffix) == "S" || toupper(suffix) == "W" || sign < 0 || val < 0) {
    result <- -result
  }
  result
}


#' Parse lat/lon from raw header tokens into decimal degrees
#'
#' @param lat_token Character. Token containing latitude, possibly merged with lon.
#' @param lon_token Character. Token containing longitude, optional.
#' @return Numeric vector of length 2: (lat, lon).
parse_latlon_tokens <- function(lat_token, lon_token = "") {
  lat <- NA_real_
  lon <- NA_real_

  lat_token <- str_trim(lat_token)
  lon_token <- str_trim(lon_token)

  # Format: ±DDMM[NS] ±DDDMM[EW] with optional NSEW
  m <- str_match(paste(lat_token, lon_token), "^(-?\\d{2,4})\\s*([NS]?)\\s+(-?\\d{3,5})\\s*([EW]?)$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(m[2], m[3])
    lon <- parse_coord_part(m[4], m[5])
    return(c(lat, lon))
  }

  # Format: ±DDD[ -]±DDDD with hyphen separator (e.g. "-035-5312", "035 5312")
  m <- str_match(paste(lat_token, lon_token), "^([+-]?\\d{2,3})[ -]+([+-]?\\d{3,4})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(m[2])
    lon <- parse_coord_part(m[3])
    return(c(lat, lon))
  }

  # Format: ±DDD-±DDD merged with hyphen (e.g. "-035-5312" single token)
  merged <- str_trim(paste0(lat_token, lon_token))
  m <- str_match(merged, "^([+-]?\\d{2,4})[ -]+([+-]?\\d{2,5})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(m[2])
    # If separator is "-" and lon part has no sign, inherit the sign from separator
    sep_pos <- nchar(m[2]) + 1
    sep_char <- substr(merged, sep_pos, sep_pos)
    lon_sign <- if (!is.na(sep_char) && sep_char == "-" && !grepl("^[+-]", m[3])) -1 else 1
    lon <- parse_coord_part(m[3], sign = lon_sign)
    return(c(lat, lon))
  }

  # Format: ±DDD ±DDDMM (3-digit lat)
  m <- str_match(paste(lat_token, lon_token), "^(-?\\d{2,3}\\.?\\d*)\\s+(-?\\d{3,5})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat_val <- as.numeric(m[2])
    lon_val <- as.numeric(m[3])
    if (abs(lat_val) >= 90) lat_val <- lat_val / 100
    if (abs(lon_val) >= 1000) lon_val <- lon_val / 100
    return(c(lat_val, lon_val))
  }

  # Format: ±DDDDDDDDD — merged 9-digit DDMMDDDMM (e.g. "123703728" = 12°37' 037°28')
  merged <- str_trim(paste0(lat_token, lon_token))
  m <- str_match(merged, "^([+-]?\\d{4})(\\d{5})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(m[2])
    lon <- parse_coord_part(m[3])
    return(c(lat, lon))
  }

  # Format: ±DDDD±DDDD — 4+4 digit DDMM-DDMM with separator (e.g. "6652-6538", "2825-8345")
  m <- str_match(merged, "^([+-]?\\d{4})[ -]+([+-]?\\d{4})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(m[2])
    lon <- parse_coord_part(m[3])
    return(c(lat, lon))
  }

  # Format: ±DDDD±DDDDD with + sign (e.g. "-0035+03703", "+5046+10012")
  m <- str_match(merged, "^([+-]?\\d{4})([+-])(\\d{3,5})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(gsub("^\\+", "", m[2]))
    lon <- parse_coord_part(paste0(m[3], m[4]), sign = if (m[3] == "-") -1 else 1)
    return(c(lat, lon))
  }

  # Format: ±DDDD NDDDD (merged, negative lon, no space)
  m <- str_match(merged, "^(-?\\d{3,4})(-?\\d{3,5})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat_val <- as.numeric(m[2])
    lon_val <- as.numeric(m[3])
    if (abs(lat_val) >= 1000) lat_val <- lat_val / 100
    if (abs(lon_val) >= 10000) lon_val <- lon_val / 100
    return(c(lat_val, lon_val))
  }

  # Format: DDDD[NS]DDDDD[EW] merged as one (e.g. "3224S01913E")
  m <- str_match(merged, "^(\\d{2,4})([NS])(\\d{3,5})([EW])$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    lat <- parse_coord_part(m[2], m[3])
    lon <- parse_coord_part(m[4], m[5])
    return(c(lat, lon))
  }

  # Single token: just numeric
  val <- suppressWarnings(as.numeric(lat_token))
  if (!is.na(val)) {
    lat <- parse_coord_part(lat_token)
  }
  val2 <- suppressWarnings(as.numeric(lon_token))
  if (!is.na(val2)) {
    lon <- parse_coord_part(lon_token)
  }

  c(lat, lon)
}


#' Parse species name and coordinates from .rwl header line 2
#'
#' Extracts elevation, lat, lon, years from the second header line.
#' Uses position-based extraction since the Tucson format has
#' semi-fixed-width fields.
#'
#' @param line2 Character. The second line of the .rwl file.
#' @param line3 Character. The third line of the .rwl file (for species hint).
#' @return Named list with species, lat, lon, elevation, start_year, end_year
parse_line2 <- function(line2, line3 = "") {
  species <- NA_character_
  lat <- NA_real_
  lon <- NA_real_
  elevation <- NA_real_
  start_year <- NA_integer_
  end_year <- NA_integer_

  if (is.na(line2) || nchar(str_trim(line2)) == 0) {
    return(list(species = species, lat = lat, lon = lon,
                elevation = elevation, start_year = start_year, end_year = end_year))
  }

  # --- Extract years ---
  # Strip trailing non-numeric characters (e.g. "1995 -")
  line2_trim <- str_trim(line2)
  line2_trim <- str_replace(line2_trim, "[^0-9\\s-]+\\s*$", "")
  # Standard: "START END" at end of line
  m <- str_match(line2_trim, "\\b(-?\\d{3,5})\\s+(-?\\d{3,5})$")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    start_year <- as.integer(m[2])
    end_year <- as.integer(m[3])
  }
  # BCE: "-2220-1890" (no space, negative years)
  if (is.na(start_year)) {
    m <- str_match(line2_trim, "\\b(-?\\d{4})-(-?\\d{4})$")
    if (!is.na(m[1]) && nchar(m[1]) > 0) {
      start_year <- as.integer(m[2])
      end_year <- as.integer(m[3])
    }
  }

  # --- Extract elevation (M immediately follows number, ITRDB convention) ---
  matches <- gregexpr("(\\d+\\.?\\d*)M", line2, perl = TRUE)
  if (length(matches[[1]]) > 0 && matches[[1]][1] != -1) {
    all_matches <- regmatches(line2, matches)[[1]]
    candidates <- as.numeric(gsub("M", "", all_matches))
    elevation <- candidates[which.max(candidates)]
  }
  if (is.na(elevation) || elevation == 0) {
    m <- str_match(line2, "(\\d+)-(\\d+)M")
    if (!is.na(m[1]) && nchar(m[1]) > 0) {
      elevation <- (as.numeric(m[2]) + as.numeric(m[3])) / 2
    }
  }
  # No M at all: find elevation-like number before coordinates
  if (is.na(elevation) || elevation == 0) {
    m <- str_match(line2, "\\b(\\d{3,4})\\s+(?:[+-]?\\d{3,5}.*)$")
    if (!is.na(m[1]) && nchar(m[1]) > 0) {
      elev_candidate <- as.numeric(m[2])
      if (!is.na(elev_candidate) && elev_candidate > 0 && elev_candidate < 9000) {
        elevation <- elev_candidate
      }
    }
  }

  # --- Extract coordinates ---
  # Strategy: find the numeric block that looks like lat/lon before the separator/years
  line2_no_years <- line2
  if (!is.na(start_year)) {
    line2_no_years <- str_trim(str_replace(line2, "\\s*-?\\d{3,5}\\s*-?\\d{3,5}\\s*$", ""))
  }

  # Remove separator fields like __, E_, L_, N_, X_, LX
  line2_clean <- str_replace(line2_no_years, "\\s*[A-Z]?_{0,2}\\s*$", "")
  line2_clean <- str_replace(line2_clean, "\\s*LX\\s*$", "")
  # Also handle plain letter separator at end (e.g. "   D")
  line2_clean <- str_replace(line2_clean, "\\s+[A-Z]\\s*$", "")

  # Coord extraction: try after "M" first
  coord_str <- NA_character_
  m <- str_match(line2_clean, "M\\s+(.+?)\\s*$")
  if (!is.na(m[1]) && nchar(str_trim(m[2])) > 0) {
    coord_str <- str_trim(m[2])
  } else {
    # No M found — coordinates are the last numeric tokens before separator/years
    tokens <- str_split(line2_clean, "\\s+")[[1]]
    tokens <- tokens[nchar(tokens) > 0]
    # Find all tokens that look like coordinate values (3-7 digits, optional sign)
    coord_idx <- which(grepl("^[+-]?\\d{3,7}$", tokens))
    # Remove elevation-like tokens (3-4 digits that aren't valid lat/lon values)
    # Keep the LAST matching tokens (furthest right, nearest to separator/years)
    if (length(coord_idx) >= 2) {
      coord_idx <- tail(coord_idx, 2)
      coord_str <- paste(tokens[coord_idx], collapse = " ")
    }
  }

  if (!is.na(coord_str)) {
    # Split by whitespace
    parts <- str_split(coord_str, "\\s+")[[1]]
    parts <- parts[nchar(parts) > 0]

    if (length(parts) >= 2) {
      # Check for NSEW merged format in first token
      if (grepl("[NS]", parts[1]) && grepl("[EW]", parts[2])) {
        parsed <- parse_latlon_tokens(parts[1], parts[2])
        lat <- parsed[1]
        lon <- parsed[2]
      } else if (grepl("[NS]", parts[1]) && length(parts) >= 3) {
        parsed <- parse_latlon_tokens(parts[1], parts[2])
        lat <- parsed[1]
        lon <- parsed[2]
      } else if (grepl("[+-]", paste0(parts[1], parts[2]))) {
        # Handle merged signed format
        parsed <- parse_latlon_tokens(parts[1], parts[2])
        lat <- parsed[1]
        lon <- parsed[2]
      } else {
        parsed <- parse_latlon_tokens(parts[1], parts[2])
        lat <- parsed[1]
        lon <- parsed[2]
      }
    } else if (length(parts) == 1) {
      # Try to parse as merged single token
      parsed <- parse_latlon_tokens(parts[1], "")
      lat <- parsed[1]
      lon <- parsed[2]
    }
  } else {
    # Try safr001-like format: "01330  3224S01913E"
    m <- str_match(line2_clean, "\\s+(\\d+)\\s+(\\d{2,4}[NS]\\d{3,5}[EW])")
    if (!is.na(m[1]) && nchar(m[1]) > 0) {
      parsed <- parse_latlon_tokens(m[3], "")
      lat <- parsed[1]
      lon <- parsed[2]
    }
  }

  # --- Extract species name (between country/region and elevation) ---
  # Country ends at the elevation number, so find text between country start and elevation
  # Elevation pattern: number followed by M
  m <- str_match(line2_trim, "\\s([A-Z][A-Za-z\\s.,]+?)\\s+\\d+\\.?\\d*\\s*M")
  if (!is.na(m[1]) && nchar(m[1]) > 0) {
    maybe_species <- str_trim(m[2])
    # Filter out known country names
    if (!grepl("^(Algeria|Bangladesh|Bhutan|Botswana|Cameroon|China|Congo|Cote|Egypt|Ethiopia|Ivory|Kenya|Morocco|Namibia|South|Tanzania|Tunisia|Zambia|Zimbabwe|P\\.R\\.)", maybe_species, ignore.case = TRUE)) {
      species <- maybe_species
    }
  }
  if (is.na(species)) {
    m <- str_match(line2_trim, "\\s+([A-Z][a-z]+\\s+[a-z]+.*?)\\s+\\d+\\.?\\d*\\s*M")
    if (!is.na(m[1]) && nchar(m[1]) > 0) {
      species <- str_trim(m[2])
    }
  }

  list(
    species = species,
    lat = lat,
    lon = lon,
    elevation = elevation,
    start_year = start_year,
    end_year = end_year
  )
}


#' Read metadata from a -noaa.rwl companion file
#'
#' Parses standard NOAA template comment headers for site metadata.
#'
#' @param file Character. Path to the -noaa.rwl file.
#' @return Named list with site_name, species, species_code, lat, lon, elevation
read_noaa_metadata <- function(file) {
  lines <- readLines(file, warn = FALSE)
  site_name <- NA_character_
  species <- NA_character_
  species_code <- NA_character_
  lat <- NA_real_
  lon <- NA_real_
  elevation <- NA_real_

  for (line in lines) {
    m <- str_match(line, "^# Site_Name:\\s*(.+)$")
    if (!is.na(m[1]) && nchar(m[1]) > 0) site_name <- str_trim(m[2])

    m <- str_match(line, "^# Species_Name:\\s*(.+)$")
    if (!is.na(m[1]) && nchar(m[1]) > 0) species <- str_trim(m[2])

    m <- str_match(line, "^# Tree_Species_Code:\\s*(.+)$")
    if (!is.na(m[1]) && nchar(m[1]) > 0) species_code <- str_trim(m[2])

    m <- str_match(line, "^# Northernmost_Latitude:\\s*(-?\\d+\\.?\\d*)")
    if (!is.na(m[1]) && nchar(m[1]) > 0) lat <- as.numeric(m[2])

    m <- str_match(line, "^# Easternmost_Longitude:\\s*(-?\\d+\\.?\\d*)")
    if (!is.na(m[1]) && nchar(m[1]) > 0) lon <- as.numeric(m[2])

    m <- str_match(line, "^# Elevation:\\s*(\\d+\\.?\\d*)")
    if (!is.na(m[1]) && nchar(m[1]) > 0) elevation <- as.numeric(m[2])
  }

  list(
    site_name = site_name,
    species = species,
    species_code = species_code,
    lat = lat,
    lon = lon,
    elevation = elevation
  )
}


#' Extract site metadata from raw header lines of a .rwl file
#'
#' Parses the first 3 lines of a Tucson-format .rwl file for site
#' identification and coordinates.
#'
#' @param file Character. Path to the .rwl file.
#' @return Named list with site_id, species, lat, lon, elevation
read_raw_header <- function(file) {
  lines <- readLines(file, n = 3, warn = FALSE)

  site_id <- NA_character_
  species <- NA_character_
  species_code <- NA_character_
  lat <- NA_real_
  lon <- NA_real_
  elevation <- NA_real_

  if (length(lines) >= 1) {
    # Skip files that start with # (comment/CRN format, not Tucson)
    if (grepl("^#", str_trim(lines[1]))) {
      return(list(site_id = NA_character_, species = NA_character_,
                  species_code = NA_character_,
                  lat = NA_real_, lon = NA_real_, elevation = NA_real_))
    }
    m <- str_match(lines[1], "^\\s*(\\S+)\\s+")
    if (!is.na(m[1]) && nchar(m[1]) > 0) site_id <- str_trim(m[2])
    m <- str_match(lines[1], "\\b([A-Z0-9]{4})\\s*$")
    if (!is.na(m[1]) && nchar(m[1]) > 0) species_code <- str_trim(m[2])
  }

  line3 <- if (length(lines) >= 3) lines[3] else ""

  if (length(lines) >= 2) {
    parsed <- parse_line2(lines[2], line3)
    species <- parsed$species
    lat <- parsed$lat
    lon <- parsed$lon
    elevation <- parsed$elevation
  }

  list(
    site_id = site_id,
    species = species,
    species_code = species_code,
    lat = lat,
    lon = lon,
    elevation = elevation
  )
}


#' Extract continent from a file path
#'
#' The first directory component of the file path relative to the base
#' directory is used as the continent name.
#'
#' @param file_path Character. Full path to a .rwl file.
#' @param base_dir Character. Base data directory.
#' @return Character. Continent name, or NA if not detectable.
file_continent <- function(file_path, base_dir) {
  rel <- sub(paste0("^", base_dir, "/?"), "", file_path)
  parts <- str_split(rel, "/")[[1]]
  parts[1]
}


#' Extract country from a file path
#'
#' The second directory component of the file path relative to the base
#' directory is used as the country name, if present.  E.g. for
#' \code{northamerica/canada/cana001.rwl} the country is \code{canada}.
#'
#' @param file_path Character. Full path to a .rwl file.
#' @param base_dir Character. Base data directory.
#' @return Character. Country name, or NA if the path has no country
#'   subdirectory.
file_country <- function(file_path, base_dir) {
  rel <- sub(paste0("^", base_dir, "/?"), "", file_path)
  parts <- str_split(rel, "/")[[1]]
  if (length(parts) > 2) parts[2] else NA_character_
}


#' Extract site metadata from .rwl files
#'
#' Reads all .rwl files in a directory and builds a metadata table.
#' Metadata sources in priority order: -noaa.rwl companion, manual header parse.
#'
#' @param dir Character. Path to directory containing .rwl files.
#' @param rwl_list List. Optional pre-read rwl list from \code{read_treerings()}.
#'   If provided, metadata and year ranges are extracted from these objects
#'   instead of re-reading files.
#' @return A \code{data.frame} with columns:
#'   \code{site_id}, \code{site_name}, \code{species}, \code{species_code},
#'   \code{lat}, \code{lon}, \code{elevation}, \code{continent},
#'   \code{country}, \code{n_cores}, \code{min_year}, \code{max_year},
#'   \code{filename}.
#' @export
#'
#' @examples
#' \dontrun{
#' sites <- extract_sites("treerings")
#' }
extract_sites <- function(dir, rwl_list = NULL) {
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  }

  # Build a lookup: basename -> full path
  all_files <- list.files(dir, pattern = "\\.rwl$", full.names = TRUE, recursive = TRUE)
  all_files <- all_files[!grepl("-noaa\\.rwl$", all_files)]
  if (length(all_files) == 0) stop("No .rwl files found in: ", dir)

  file_map <- structure(all_files, names = file_path_sans_ext(basename(all_files)))
  # If basenames conflict (same filename in different subdirs), keep the first
  file_map <- file_map[!duplicated(names(file_map))]

  names_to_process <- if (!is.null(rwl_list)) intersect(names(rwl_list), names(file_map)) else names(file_map)
  if (length(names_to_process) == 0) stop("No matching .rwl files found")

  rows <- list()

  for (base_name in names_to_process) {
    f <- if (!is.null(rwl_list) && base_name %in% names(rwl_list)) {
      # rwl_list was provided, try to find the actual file for metadata
      if (base_name %in% names(file_map)) file_map[base_name] else file.path(dir, paste0(base_name, ".rwl"))
    } else {
      file_map[base_name]
    }
    noaa_file <- sub("\\.rwl$", "-noaa.rwl", f)

    if (!is.null(rwl_list) && base_name %in% names(rwl_list)) {
      rwl <- rwl_list[[base_name]]
    } else if (file.exists(f) && file.info(f)$size > 0) {
      rwl <- tryCatch(
        read.tucson(f),
        error = function(e) tryCatch(read.tucson(f, long = TRUE), error = function(e2) NULL)
      )
    } else {
      rwl <- NULL
    }

    if (is.null(rwl)) {
      rows[[base_name]] <- data.frame(
        site_id = base_name, site_name = NA_character_,
        species = NA_character_, species_code = NA_character_,
        lat = NA_real_, lon = NA_real_, elevation = NA_real_,
        continent = NA_character_, country = NA_character_,
        n_cores = NA_integer_, min_year = NA_integer_, max_year = NA_integer_,
        filename = base_name, stringsAsFactors = FALSE
      )
      next
    }

    years <- as.integer(rownames(rwl))
    n_cores <- ncol(rwl)
    min_year <- min(years)
    max_year <- max(years)

    site_id <- base_name
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

    country <- file_country(f, dir)
    rows[[base_name]] <- data.frame(
      site_id = site_id,
      site_name = site_name,
      species = species,
      species_code = species_code,
      lat = lat,
      lon = lon,
      elevation = elevation,
      continent = file_continent(f, dir),
      country = country,
      n_cores = n_cores,
      min_year = min_year,
      max_year = max_year,
      filename = base_name,
      stringsAsFactors = FALSE
    )
  }

  result <- bind_rows(rows)

  # Reverse-geocode missing countries from lat/lon
  na_geo <- which(is.na(result$country) & !is.na(result$lat) & !is.na(result$lon))
  if (length(na_geo) > 0 && requireNamespace("maps", quietly = TRUE)) {
    countries <- maps::map.where("world", result$lon[na_geo], result$lat[na_geo])
    countries <- sub(":.*$", "", as.character(countries))
    result$country[na_geo] <- countries
    # Clean up map.where() artifacts ("USA" -> "United States"? no, keep as-is for now)
  }

  result
}

# treeringr

Download, parse, and visualize tree-ring measurements from the NOAA International Tree-Ring Data Bank (ITRDB).

## Installation

```r
devtools::install("/path/to/treeringr")
```

## Quickstart

### 1. Download data

```r
library(treeringr)

download_treerings(dest_dir = "treerings")
```

### 2. Parse into DuckDB (parallel, with outlier correction)

```r
parse_to_duckdb(dir = "treerings", db_path = "treerings.duckdb")
```

### 3. Connect and query

```r
con <- connect_treerings("treerings.duckdb")

# Sites by continent or country
sites <- query_sites(con, continent = "europe")
sites <- query_sites(con, country = "France")

# Mean chronology for a site
chron <- query_chronology(con, "alge001")

# Wide format (years as columns)
wide <- query_wide(con, c("alge001", "chin005"))

# Find sites by country via reverse-geocoding
sites <- query_sites(con)
france <- find_sites_by_country(sites, "France")

# Disconnect
DBI::dbDisconnect(con, shutdown = TRUE)
```

### 4. Plot

```r
# Spaghetti plot of all cores
plot_spaghetti("alge001", "treerings")

# Mean chronology with SD ribbon
plot_chronology("alge001", "treerings")

# Map of all sites
sites <- extract_sites("treerings")
plot_site_map(sites)

# Map filtered to a continent with geographic background
con <- connect_treerings("treerings.duckdb")
sites <- query_sites(con)
plot_site_map(sites, continent = "europe")

# Concentric rings of a single core (from DuckDB or files)
plot_rings("alge001", con = con)
plot_rings("alge001", dir = "treerings")
```

## Data structure on disk

```
treerings/
├── africa/           ← continent
│   ├── alge001.rwl   ← site .rwl file
│   └── alge001-noaa.rwl  ← NOAA metadata companion
├── asia/
├── europe/
├── northamerica/
│   ├── canada/
│   ├── mexico/
│   └── usa/
└── ...
treerings.duckdb      ← parsed database (2.1 GB, 10K+ sites)
```

## Database schema

| Table | Rows | Description |
|-------|------|-------------|
| `sites` | 10,375 | Site metadata (id, species, lat, lon, elevation, continent, country, n_cores, year range) |
| `measurements` | 72.6M | Ring widths in long format (site_id, core_id, year, ring_width) |

## Outlier correction

Decimal-point data-entry errors are automatically detected and corrected during parsing using a per-core IQR-based rule. See `?fix_outliers` for details.

## Functions

| Function | Description |
|----------|-------------|
| `download_treerings()` | Download .rwl files from NOAA ITRDB |
| `parse_to_duckdb()` | Parallel parse into DuckDB (7 cores, ~10 min) |
| `connect_treerings()` | Read-only DuckDB connection |
| `extract_sites()` | Site metadata table from files |
| `query_sites()` | Query sites from DuckDB |
| `query_chronology()` | Mean chronology via SQL aggregation |
| `query_wide()` | Wide-format pivot (years as columns) |
| `plot_spaghetti()` | All cores overlaid |
| `plot_chronology()` | Mean ± SD chronology |
| `plot_site_map()` | Map colored by species (optional continent filter + map background) |
| `plot_rings()` | Concentric tree-ring cross-section for a single core |
| `find_sites_by_country()` | Filter sites by country name via reverse-geocoding |
| `fix_outliers()` | Detect and fix decimal errors |

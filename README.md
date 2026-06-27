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

# Sites by continent
sites <- query_sites(con, continent = "europe")

# Mean chronology for a site
chron <- query_chronology(con, "alge001")

# Wide format (years as columns)
wide <- query_wide(con, c("alge001", "chin005"))

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
| `sites` | 10,375 | Site metadata (id, species, lat, lon, elevation, continent, n_cores, year range) |
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
| `plot_site_map()` | Map colored by species |
| `fix_outliers()` | Detect and fix decimal errors |

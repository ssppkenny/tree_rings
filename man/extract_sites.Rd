\name{extract_sites}
\alias{extract_sites}
\title{Extract site metadata from .rwl files}
\description{
Reads all .rwl files in a directory and builds a metadata table with
site IDs, species names, coordinates, elevation, and year ranges.
}
\usage{
extract_sites(dir)
}
\arguments{
\item{dir}{Path to directory containing .rwl files.}
}
\value{
A data.frame with columns: site_id, species, species_code, lat, lon,
elevation, n_cores, min_year, max_year, filename.
}

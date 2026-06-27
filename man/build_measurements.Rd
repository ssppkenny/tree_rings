\name{build_measurements}
\alias{build_measurements}
\title{Build a wide-format tree-ring measurements table}
\description{
Reads all .rwl files in a directory and creates a wide-format data frame
where each row is a core from a site and each year is a column.
}
\usage{
build_measurements(dir, sites = NULL)
}
\arguments{
\item{dir}{Path to the directory containing .rwl files.}
\item{sites}{Optional pre-computed sites table from \code{extract_sites()}.}
}
\value{
A data.frame with columns: site_id, core_id, species, lat, lon, elevation,
min_year, max_year, and one column per year.
}

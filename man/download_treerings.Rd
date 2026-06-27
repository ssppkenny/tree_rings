\name{download_treerings}
\alias{download_treerings}
\title{Download tree-ring measurement files from NOAA ITRDB}
\description{
Downloads .rwl files from the NOAA International Tree-Ring Data Bank
into a local directory.
}
\usage{
download_treerings(url = "https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/",
                   dest_dir = "treerings",
                   timeout = 3600)
}
\arguments{
\item{url}{Base URL of the NOAA treering measurements page.}
\item{dest_dir}{Destination directory path. Created if it does not exist.}
\item{timeout}{Download timeout in seconds.}
}
\value{
Invisibly returns the number of files downloaded.
}

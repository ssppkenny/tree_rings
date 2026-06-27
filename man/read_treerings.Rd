\name{read_treerings}
\alias{read_treerings}
\title{Read tree-ring measurement files robustly}
\description{
Reads all .rwl files in a directory using \code{read.tucson()} with
\code{long = TRUE} as fallback. Skips empty files and \code{-noaa.rwl}
metadata companion files.
}
\usage{
read_treerings(dir)
}
\arguments{
\item{dir}{Path to the directory containing .rwl files.}
}
\value{
A named list of \code{data.frame} objects (class \code{rwl}).
Names are the filenames without extension.
}

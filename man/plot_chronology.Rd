\name{plot_chronology}
\alias{plot_chronology}
\title{Plot a mean chronology for a site}
\description{
Shows the mean ring width per year with a ribbon for +/- 1 standard
deviation.
}
\usage{
plot_chronology(site_name, dir, ...)
}
\arguments{
\item{site_name}{The site ID (filename without extension).}
\item{dir}{Path to directory containing .rwl files.}
\item{...}{Additional arguments passed to \code{ggplot2::labs()}.}
}
\value{
A ggplot object.
}

\name{plot_site_map}
\alias{plot_site_map}
\title{Plot a map of all sites}
\description{
Creates a map showing all sites as points colored by species with
size proportional to number of cores.
}
\usage{
plot_site_map(sites, ...)
}
\arguments{
\item{sites}{A sites table from \code{extract_sites()}.}
\item{...}{Additional arguments passed to \code{ggplot2::labs()}.}
}
\value{
A ggplot object.
}

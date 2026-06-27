\name{plot_spaghetti}
\alias{plot_spaghetti}
\title{Plot a spaghetti plot of all cores at a site}
\description{
Each core is shown as a separate line with ring width over time.
}
\usage{
plot_spaghetti(site_name, dir, ...)
}
\arguments{
\item{site_name}{The site ID (filename without extension).}
\item{dir}{Path to directory containing .rwl files.}
\item{...}{Additional arguments passed to \code{ggplot2::labs()}.}
}
\value{
A ggplot object.
}

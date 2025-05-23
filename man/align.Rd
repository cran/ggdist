% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_hist.R
\name{align}
\alias{align}
\alias{align_none}
\alias{align_boundary}
\alias{align_center}
\title{Break (bin) alignment methods}
\usage{
align_none(breaks)

align_boundary(breaks, at = 0)

align_center(breaks, at = 0)
}
\arguments{
\item{breaks}{<\link{numeric}> A sorted vector of breaks (bin edges).}

\item{at}{<scalar \link{numeric}> The alignment point.
\itemize{
\item For \code{\link[=align_boundary]{align_boundary()}}: align breaks so that a bin edge lines up with \code{at}.
\item For \code{\link[=align_center]{align_center()}}: align breaks so that the center of a bin lines up with \code{at}.
}}
}
\value{
A scalar numeric returning an offset to be subtracted from \code{breaks}.
}
\description{
Methods for aligning breaks (bins) in histograms, as used in the \code{align}
argument to \code{\link[=density_histogram]{density_histogram()}}.

Supports \link[=auto_partial]{automatic partial function application} with
\link[=waiver]{waived arguments}.
}
\details{
These functions take a sorted vector of equally-spaced \code{breaks} giving
bin edges and return a numeric offset which, if subtracted from \code{breaks},
will align them as desired:
\itemize{
\item \code{\link[=align_none]{align_none()}} performs no alignment (it always returns \code{0}).
\item \code{\link[=align_boundary]{align_boundary()}} ensures that a bin edge lines up with \code{at}.
\item \code{\link[=align_center]{align_center()}} ensures that a bin center lines up with \code{at.}
}

For \code{\link[=align_boundary]{align_boundary()}} (respectively \code{\link[=align_center]{align_center()}}), if no bin edge (or center) in the
range of \code{breaks} would line up with \code{at}, it ensures that \code{at} is an integer
multiple of the bin width away from a bin edge (or center).
}
\examples{
library(ggplot2)

set.seed(1234)
x = rnorm(200, 1, 2)

# If we manually specify a bin width using breaks_fixed(), the default
# alignment (align_none()) will not align bin edges to any "pretty" numbers.
# Here is a comparison of the three alignment methods on such a histogram:
ggplot(data.frame(x), aes(x)) +
  stat_slab(
    aes(y = "align_none()\nor 'none'"),
    density = "histogram",
    breaks = breaks_fixed(width = 1),
    outline_bars = TRUE,
    # no need to specify align; align_none() is the default
    color = "black",
  ) +
  stat_slab(
    aes(y = "align_center(at = 0)\nor 'center'"),
    density = "histogram",
    breaks = breaks_fixed(width = 1),
    align = align_center(at = 0),   # or align = "center"
    outline_bars = TRUE,
    color = "black",
  ) +
  stat_slab(
    aes(y = "align_boundary(at = 0)\nor 'boundary'"),
    density = "histogram",
    breaks = breaks_fixed(width = 1),
    align = align_boundary(at = 0), # or align = "boundary"
    outline_bars = TRUE,
    color = "black",
  ) +
  geom_point(aes(y = 0.7), alpha = 0.5) +
  labs(
    subtitle = "ggdist::stat_slab(density = 'histogram', ...)",
    y = "align =",
    x = NULL
  ) +
  geom_vline(xintercept = 0, linetype = "22", color = "red")
}
\seealso{
\code{\link[=density_histogram]{density_histogram()}}, \link{breaks}
}

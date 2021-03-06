% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_colour_ramp.R
\name{scale_colour_ramp}
\alias{scale_colour_ramp}
\alias{scale_colour_ramp_continuous}
\alias{scale_color_ramp}
\alias{scale_fill_ramp}
\alias{scale_color_ramp_continuous}
\alias{scale_colour_ramp_discrete}
\alias{scale_color_ramp_discrete}
\alias{scale_fill_ramp_continuous}
\alias{scale_fill_ramp_discrete}
\title{Secondary ggplot color scale that ramps from another color}
\usage{
scale_colour_ramp_continuous(
  from = "white",
  ...,
  limits = function(l) c(min(0, l[[1]]), l[[2]]),
  range = c(0, 1),
  aesthetics = "colour_ramp"
)

scale_color_ramp_continuous(
  from = "white",
  ...,
  limits = function(l) c(min(0, l[[1]]), l[[2]]),
  range = c(0, 1),
  aesthetics = "colour_ramp"
)

scale_colour_ramp_discrete(
  from = "white",
  ...,
  range = c(0.2, 1),
  aesthetics = "colour_ramp"
)

scale_color_ramp_discrete(
  from = "white",
  ...,
  range = c(0.2, 1),
  aesthetics = "colour_ramp"
)

scale_fill_ramp_continuous(..., aesthetics = "fill_ramp")

scale_fill_ramp_discrete(..., aesthetics = "fill_ramp")
}
\arguments{
\item{from}{The color to ramp from. Corresponds to \code{0} on the scale.}

\item{...}{Arguments passed to underlying scale or guide functions. E.g. \code{scale_colour_ramp_discrete()},
passes arguments to \code{\link[=discrete_scale]{discrete_scale()}}, \code{scale_colour_ramp_continuous()} passes arguments
to \code{\link[=continuous_scale]{continuous_scale()}}. See those functions for more details.}

\item{limits}{One of:
\itemize{
\item \code{NULL} to use the default scale range
\item A numeric vector of length two providing limits of the scale.
Use \code{NA} to refer to the existing minimum or maximum
\item A function that accepts the existing (automatic) limits and returns
new limits. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
Note that setting limits on positional scales will \strong{remove} data outside of the limits.
If the purpose is to zoom, use the limit argument in the coordinate system
(see \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}}).
}}

\item{range}{a numeric vector of length 2 that specifies the minimum and maximum
values after the scale transformation. These values should be between \code{0}
(the \code{from} color) and \code{1} (the color determined by the \code{fill} aesthetic).}

\item{aesthetics}{Names of aesthetics to set scales for.}
}
\value{
A \link[ggplot2:ggplot2-ggproto]{ggplot2::Scale} representing a scale for the \code{colour_ramp} and/or \code{fill_ramp}
aesthetics for \code{ggdist} geoms. Can be added to a \code{\link[=ggplot]{ggplot()}} object.
}
\description{
This scale creates a secondary scale that modifies the \code{fill} or \code{color} scale of
geoms that support it (\code{\link[=geom_lineribbon]{geom_lineribbon()}} and \code{\link[=geom_slabinterval]{geom_slabinterval()}}) to "ramp"
from a secondary color (by default white) to the primary fill color (determined
by the standard \code{color} or \code{fill} aesthetics).
}
\examples{

library(dplyr)
library(ggplot2)
library(distributional)

tibble(d = dist_uniform(0, 1)) \%>\%
  ggplot(aes(y = 0, dist = d)) +
  stat_dist_slab(aes(fill_ramp = stat(x)))

tibble(d = dist_uniform(0, 1)) \%>\%
  ggplot(aes(y = 0, dist = d)) +
  stat_dist_slab(aes(fill_ramp = stat(x)), fill = "blue") +
  scale_fill_ramp_continuous(from = "red")

# you can invert the order of `range` to change the order of the blend
tibble(d = dist_normal(0, 1)) \%>\%
  ggplot(aes(y = 0, dist = d)) +
  stat_dist_slab(aes(fill_ramp = stat(cut_cdf_qi(cdf))), fill = "blue") +
  scale_fill_ramp_discrete(from = "red", range = c(1, 0))

}
\seealso{
Other ggdist scales: 
\code{\link{scales}}
}
\author{
Matthew Kay
}
\concept{ggdist scales}

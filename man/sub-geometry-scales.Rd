% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_.R
\name{sub-geometry-scales}
\alias{sub-geometry-scales}
\alias{scale_point_colour_discrete}
\alias{scales}
\alias{scale_point_color_discrete}
\alias{scale_point_colour_continuous}
\alias{scale_point_color_continuous}
\alias{scale_point_fill_discrete}
\alias{scale_point_fill_continuous}
\alias{scale_point_alpha_continuous}
\alias{scale_point_alpha_discrete}
\alias{scale_point_size_continuous}
\alias{scale_point_size_discrete}
\alias{scale_interval_colour_discrete}
\alias{scale_interval_color_discrete}
\alias{scale_interval_colour_continuous}
\alias{scale_interval_color_continuous}
\alias{scale_interval_alpha_continuous}
\alias{scale_interval_alpha_discrete}
\alias{scale_interval_size_continuous}
\alias{scale_interval_size_discrete}
\alias{scale_interval_linetype_discrete}
\alias{scale_interval_linetype_continuous}
\alias{scale_slab_colour_discrete}
\alias{scale_slab_color_discrete}
\alias{scale_slab_colour_continuous}
\alias{scale_slab_color_continuous}
\alias{scale_slab_fill_discrete}
\alias{scale_slab_fill_continuous}
\alias{scale_slab_alpha_continuous}
\alias{scale_slab_alpha_discrete}
\alias{scale_slab_size_continuous}
\alias{scale_slab_size_discrete}
\alias{scale_slab_linewidth_continuous}
\alias{scale_slab_linewidth_discrete}
\alias{scale_slab_linetype_discrete}
\alias{scale_slab_linetype_continuous}
\alias{scale_slab_shape_discrete}
\alias{scale_slab_shape_continuous}
\alias{guide_colourbar2}
\alias{guide_colorbar2}
\title{Sub-geometry scales for geom_slabinterval (ggplot2 scales)}
\usage{
scale_point_colour_discrete(..., aesthetics = "point_colour")

scale_point_color_discrete(..., aesthetics = "point_colour")

scale_point_colour_continuous(
  ...,
  aesthetics = "point_colour",
  guide = guide_colourbar2()
)

scale_point_color_continuous(
  ...,
  aesthetics = "point_colour",
  guide = guide_colourbar2()
)

scale_point_fill_discrete(..., aesthetics = "point_fill")

scale_point_fill_continuous(
  ...,
  aesthetics = "point_fill",
  guide = guide_colourbar2()
)

scale_point_alpha_continuous(..., range = c(0.1, 1))

scale_point_alpha_discrete(..., range = c(0.1, 1))

scale_point_size_continuous(..., range = c(1, 6))

scale_point_size_discrete(..., range = c(1, 6), na.translate = FALSE)

scale_interval_colour_discrete(..., aesthetics = "interval_colour")

scale_interval_color_discrete(..., aesthetics = "interval_colour")

scale_interval_colour_continuous(
  ...,
  aesthetics = "interval_colour",
  guide = guide_colourbar2()
)

scale_interval_color_continuous(
  ...,
  aesthetics = "interval_colour",
  guide = guide_colourbar2()
)

scale_interval_alpha_continuous(..., range = c(0.1, 1))

scale_interval_alpha_discrete(..., range = c(0.1, 1))

scale_interval_size_continuous(..., range = c(1, 6))

scale_interval_size_discrete(..., range = c(1, 6), na.translate = FALSE)

scale_interval_linetype_discrete(..., na.value = "blank")

scale_interval_linetype_continuous(...)

scale_slab_colour_discrete(..., aesthetics = "slab_colour")

scale_slab_color_discrete(..., aesthetics = "slab_colour")

scale_slab_colour_continuous(
  ...,
  aesthetics = "slab_colour",
  guide = guide_colourbar2()
)

scale_slab_color_continuous(
  ...,
  aesthetics = "slab_colour",
  guide = guide_colourbar2()
)

scale_slab_fill_discrete(..., aesthetics = "slab_fill")

scale_slab_fill_continuous(
  ...,
  aesthetics = "slab_fill",
  guide = guide_colourbar2()
)

scale_slab_alpha_continuous(
  ...,
  limits = function(l) c(min(0, l[[1]]), l[[2]]),
  range = c(0, 1)
)

scale_slab_alpha_discrete(..., range = c(0.1, 1))

scale_slab_size_continuous(..., range = c(1, 6))

scale_slab_size_discrete(..., range = c(1, 6), na.translate = FALSE)

scale_slab_linewidth_continuous(..., range = c(1, 6))

scale_slab_linewidth_discrete(..., range = c(1, 6), na.translate = FALSE)

scale_slab_linetype_discrete(..., na.value = "blank")

scale_slab_linetype_continuous(...)

scale_slab_shape_discrete(..., solid = TRUE)

scale_slab_shape_continuous(...)

guide_colourbar2(...)

guide_colorbar2(...)
}
\arguments{
\item{...}{Arguments passed to underlying scale or guide functions. E.g. \code{scale_point_color_discrete}
passes arguments to \code{\link[ggplot2:scale_colour_discrete]{scale_color_discrete()}}. See those functions for more details.}

\item{aesthetics}{<\link{character}> Names of aesthetics to set scales for.}

\item{guide}{<\link[ggplot2:ggplot2-ggproto]{Guide} | \link[=character]{string}> Guide to use for legends for an aesthetic.}

\item{range}{<length-2 \link{numeric}> The minimum and maximum size of the plotting symbol
after transformation.}

\item{na.translate}{<scalar \link{logical}> In discrete scales, should we show missing values?}

\item{na.value}{<\link[ggplot2:aes_linetype_size_shape]{linetype}> When \code{na.translate} is \code{TRUE}, what value should be shown?}

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

\item{solid}{Should the shapes be solid, \code{TRUE}, or hollow,
\code{FALSE}?}
}
\value{
A \link[ggplot2:ggplot2-ggproto]{ggplot2::Scale} representing one of the aesthetics used to target the appearance of specific parts of composite
\code{ggdist} geoms. Can be added to a \code{\link[ggplot2:ggplot]{ggplot()}} object.
}
\description{
These scales allow more specific aesthetic mappings to be made when using \code{\link[=geom_slabinterval]{geom_slabinterval()}}
and stats/geoms based on it (like eye plots).
}
\details{
The following additional scales / aesthetics are defined for use with \code{\link[=geom_slabinterval]{geom_slabinterval()}} and
related geoms:

\describe{
\item{\verb{scale_point_color_* }}{Point color}
\item{\verb{scale_point_fill_* }}{Point fill color}
\item{\verb{scale_point_alpha_* }}{Point alpha level / opacity}
\item{\verb{scale_point_size_* }}{Point size}
\item{\verb{scale_interval_color_* }}{Interval line color}
\item{\verb{scale_interval_alpha_* }}{Interval alpha level / opacity}
\item{\verb{scale_interval_linetype_* }}{Interval line type}
\item{\verb{scale_slab_color_* }}{Slab outline color}
\item{\verb{scale_slab_fill_* }}{Slab fill color}
\item{\verb{scale_slab_alpha_* }}{Slab alpha level / opacity. The default settings of
\code{scale_slab_alpha_continuous} differ from \code{\link[ggplot2:scale_alpha]{scale_alpha_continuous()}}
and are designed for gradient plots (e.g. \code{\link[=stat_gradientinterval]{stat_gradientinterval()}}) by ensuring that
densities of 0 get mapped to 0 in the output.}
\item{\verb{scale_slab_linewidth_* }}{Slab outline line width}
\item{\verb{scale_slab_linetype_* }}{Slab outline line type}
\item{\verb{scale_slab_shape_* }}{Slab dot shape (for \code{\link[=geom_dotsinterval]{geom_dotsinterval()}})}
}

See the corresponding scale documentation in ggplot for more information; e.g.
\code{\link[ggplot2:scale_colour_discrete]{scale_color_discrete()}},
\code{\link[ggplot2:scale_colour_continuous]{scale_color_continuous()}}, etc.

Other scale functions can be used with the aesthetics/scales defined here by using the \code{aesthetics}
argument to that scale function. For example, to use color brewer scales with the \code{point_color} aesthetic:

\code{scale_color_brewer(..., aesthetics = "point_color")}

With continuous color scales, you may also need to provide a guide as the default guide does not work properly;
this is what \code{guide_colorbar2} is for:

\code{scale_color_distiller(..., guide = "colorbar2", aesthetics = "point_color")}

These scales have been deprecated:

\describe{
\item{\verb{scale_interval_size_* }}{Use \verb{scale_linewidth_*}}
\item{\verb{scale_slab_size_* }}{Slab \verb{scale_size_linewidth_*}}
}
}
\examples{

library(dplyr)
library(ggplot2)

# This plot shows how to set multiple specific aesthetics
# NB it is very ugly and is only for demo purposes.
data.frame(distribution = "Normal(1,2)") \%>\%
  parse_dist(distribution) \%>\%
  ggplot(aes(y = distribution, xdist = .dist, args = .args)) +
  stat_halfeye(
    shape = 21,  # this point shape has a fill and outline
    point_color = "red",
    point_fill = "black",
    point_alpha = .1,
    point_size = 6,
    stroke = 2,
    interval_color = "blue",
    # interval line widths are scaled from [1, 6] onto [0.6, 1.4] by default
    # see the interval_size_range parameter in help("geom_slabinterval")
    linewidth = 8,
    interval_linetype = "dashed",
    interval_alpha = .25,
    # fill sets the fill color of the slab (here the density)
    slab_color = "green",
    slab_fill = "purple",
    slab_linewidth = 3,
    slab_linetype = "dotted",
    slab_alpha = .5
  )

}
\seealso{
Other ggplot2 scales: \code{\link[ggplot2:scale_colour_discrete]{scale_color_discrete()}},
\code{\link[ggplot2:scale_colour_continuous]{scale_color_continuous()}}, etc.

Other ggdist scales: 
\code{\link{scale_colour_ramp}},
\code{\link{scale_side_mirrored}()},
\code{\link{scale_thickness}}
}
\author{
Matthew Kay
}
\concept{ggdist scales}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thickness.R
\name{thickness}
\alias{thickness}
\title{Thickness (datatype)}
\usage{
thickness(x = double(), lower = NA_real_, upper = NA_real_)
}
\arguments{
\item{x}{<coercible-to-\link{numeric}> A \link{numeric} vector or an object
coercible to a \link{numeric} (via \code{\link[vctrs:vec_cast]{vctrs::vec_cast()}}) representing scaled values
to be converted to a \code{thickness()} object.}

\item{lower}{<\link{numeric}> The original lower bounds of thickness values before scaling.
May be \code{NA} to indicate that this bound is not known.}

\item{upper}{<\link{numeric}> The original upper bounds of thickness values before scaling.
May be \code{NA} to indicate that this bound is not known.}
}
\value{
A \link[vctrs:new_rcrd]{vctrs::rcrd} of class \code{"ggdist_thickness"} with fields
\code{"x"}, \code{"lower"}, and \code{"upper"}.
}
\description{
A representation of the thickness of a slab: a scaled value (\code{x}) where
\code{0} is the base of the slab and \code{1} is its maximum extent, and the lower
(\code{lower}) and upper (\code{upper}) limits of the slab values in their original
data units.
}
\details{
This datatype is used by \code{\link[=scale_thickness_shared]{scale_thickness_shared()}} and \code{\link[=subscale_thickness]{subscale_thickness()}}
to represent \code{numeric()}-like objects marked as being in units of slab "thickness".

Unlike regular \code{numeric()}s, \code{thickness()} values mapped onto the \code{thickness}
aesthetic are not rescaled by \code{\link[=scale_thickness_shared]{scale_thickness_shared()}} or \code{\link[=geom_slabinterval]{geom_slabinterval()}}.
In most cases \code{thickness()} is not useful directly; though it can be used to
mark values that should not be rescaled---see the definitions of
\code{\link[=stat_ccdfinterval]{stat_ccdfinterval()}} and \code{\link[=stat_gradientinterval]{stat_gradientinterval()}} for some example usages.

\link{thickness} objects with unequal lower or upper limits may not be combined.
However, \link{thickness} objects with \code{NA} limits may be combined with
\link{thickness} objects with non-\code{NA} limits. This allows (e.g.) specifying
locations on the \link{thickness} scale that are independent of data limits.
}
\examples{
thickness(0:1)
thickness(0:1, 0, 10)
}
\seealso{
The \code{thickness} aesthetic of \code{\link[=geom_slabinterval]{geom_slabinterval()}}.

\code{\link[=scale_thickness_shared]{scale_thickness_shared()}}, for setting a \code{thickness} scale across
all geometries using the \code{thickness} aesthetic.

\code{\link[=subscale_thickness]{subscale_thickness()}}, for setting a \code{thickness} sub-scale within
a single \code{\link[=geom_slabinterval]{geom_slabinterval()}}.
}
\author{
Matthew Kay
}

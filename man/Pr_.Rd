% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr.R
\name{Pr_}
\alias{Pr_}
\alias{p_}
\title{Probability expressions in ggdist aesthetics}
\usage{
Pr_(x)

p_(x)
}
\arguments{
\item{x}{<bare \link{language}> Expressions. See \emph{Probability expressions}, below.}
}
\description{
\strong{Experimental} probability-like expressions that can be used in place of
some \code{after_stat()} expressions in aesthetic assignments in \pkg{ggdist} stats.
}
\details{
\code{\link[=Pr_]{Pr_()}} and \code{\link[=p_]{p_()}} are an \strong{experimental} mini-language for specifying aesthetic values
based on probabilities and probability densities derived from distributions
supplied to \pkg{ggdist} stats (e.g., in \code{\link[=stat_slabinterval]{stat_slabinterval()}},
\code{\link[=stat_dotsinterval]{stat_dotsinterval()}}, etc.). They generate expressions that use \code{\link[ggplot2:aes_eval]{after_stat()}}
and the computed variables of the stat (such as \code{cdf} and \code{pdf}; see e.g.
the \strong{Computed Variables} section of \code{\link[=stat_slabinterval]{stat_slabinterval()}}) to compute
the desired probabilities or densities.

For example, one way to map the density of a distribution onto the \code{alpha}
aesthetic of a slab is to use \code{after_stat(pdf)}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ggplot() +
  stat_slab(aes(xdist = distributional::dist_normal(), alpha = after_stat(pdf)))
}\if{html}{\out{</div>}}

\pkg{ggdist} probability expressions offer an alternative, equivalent syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ggplot() +
  stat_slab(aes(xdist = distributional::dist_normal(), alpha = !!p_(x)))
}\if{html}{\out{</div>}}

Where \code{p_(x)} is the probability density function. The use of \verb{!!} is
necessary to splice the generated expression into the \code{aes()} call; for
more information, see \link[rlang:topic-inject]{quasiquotation}.
}
\section{Probability expressions}{


Probability expressions consist of a call to \code{Pr_()} or \code{p_()} containing
a small number of valid combinations of operators and variable names.

Valid variables in probability expressions include:
\itemize{
\item \code{x}, \code{y}, or \code{value}: values along the \code{x} or \code{y} axis. \code{value} is the
orientation-neutral form.
\item \code{xdist}, \code{ydist}, or \code{dist}: distributions mapped along the \code{x} or \code{y}
axis. \code{dist} is the orientation-neutral form. \code{X} and \code{Y} can also be
used as synonyms for \code{xdist} and \code{ydist}.
\item \code{interval}: the smallest interval containing the current \code{x}/\code{y} value.
}

\code{Pr_()} generates expressions for probabilities, e.g. cumulative distribution
functions (CDFs). Valid operators inside \code{Pr_()} are:
\itemize{
\item \code{<}, \code{<=}, \code{>}, \code{>=}: generates values of the cumulative distribution
function (CDF) or complementary CDF by comparing one of \{\code{x}, \code{y}, \code{value}\}
to one of \{\code{xdist}, \code{ydist}, \code{dist}, \code{X}, \code{Y}\}. For example, \code{Pr_(xdist <= x)}
gives the CDF and \code{Pr_(xdist > x)} gives the CCDF.
\item \code{\%in\%}: currently can only be used with \code{interval} on the right-hand side:
gives the probability of \{\code{x}, \code{y}, \code{value}\} (left-hand side) being in the
smallest interval the stat generated that contains the value; e.g.
\code{Pr_(x \%in\% interval)}.
}

\code{p_()} generates expressions for probability density functions or probability mass
functions (depending on if the underlying distribution is continuous or
discrete). It currently does not allow any operators in the expression, and
must be passed one of \code{x}, \code{y}, or \code{value}.
}

\examples{
library(ggplot2)
library(distributional)

df = data.frame(
  d = c(dist_normal(2.7, 1), dist_lognormal(1, 1/3)),
  name = c("normal", "lognormal")
)

# map density onto alpha of the fill
ggplot(df, aes(y = name, xdist = d)) +
  stat_slabinterval(aes(alpha = !!p_(x)))

# map CCDF onto thickness (like stat_ccdfinterval())
ggplot(df, aes(y = name, xdist = d)) +
  stat_slabinterval(aes(thickness = !!Pr_(xdist > x)))

# map containing interval onto fill
ggplot(df, aes(y = name, xdist = d)) +
  stat_slabinterval(aes(fill = !!Pr_(x \%in\% interval)))

# the color scale in the previous example is not great, so turn the
# probability into an ordered factor and adjust the fill scale.
# Though, see also the `level` computed variable in `stat_slabinterval()`,
# which is probably easier to use to create this style of chart.
ggplot(df, aes(y = name, xdist = d)) +
  stat_slabinterval(aes(fill = ordered(!!Pr_(x \%in\% interval)))) +
  scale_fill_brewer(direction = -1)

}
\seealso{
The \emph{Computed Variables} section of \code{\link[=stat_slabinterval]{stat_slabinterval()}} (especially
\code{cdf} and \code{pdf}) and the \code{\link[ggplot2:aes_eval]{after_stat()}} function.
}

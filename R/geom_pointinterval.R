# Like a geom_pointrange but with sensible defaults for displaying multiple intervals
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(".width")


#' @eval rd_slabinterval_shortcut_geom("pointinterval", "point + multiple-interval")
#' @param show.legend <[logical]> Should this layer be included in the legends?
#' Default is `c(size = FALSE)`, unlike most geoms, to match its common use cases.
#' `FALSE` hides all legends, `TRUE` shows all legends, and `NA` shows only
#' those that are mapped (the default for most geoms). It can also be a named
#' logical vector to finely select the aesthetics to display.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' # orientation is detected automatically based on
#' # use of xmin/xmax or ymin/ymax
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   median_qi(.width = c(.8, .95)) %>%
#'   ggplot(aes(y = i, x = u_tau, xmin = .lower, xmax = .upper)) +
#'   geom_pointinterval()
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   median_qi(.width = c(.8, .95)) %>%
#'   ggplot(aes(x = i, y = u_tau, ymin = .lower, ymax = .upper)) +
#'   geom_pointinterval()
#'
#' @import ggplot2
#' @name geom_pointinterval
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomPointinterval = ggproto("GeomPointinterval", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval",
    side = "both"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_computed_aes = defaults(aes(
    size = -.width
  ), GeomSlabinterval$default_computed_aes),

  hidden_aes = union(c(
    "datatype",
    "side", "scale", "justification", "thickness",
    "slab_size", "slab_linewidth", "slab_colour", "slab_fill", "slab_alpha", "slab_linetype"
  ), GeomSlabinterval$hidden_aes),

  default_params = defaults(list(
    orientation = NA,
    show_slab = FALSE
  ), GeomSlabinterval$default_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "subscale", "normalize", "fill_type",
    "subguide"
  ), GeomSlabinterval$hidden_params),

  layer_args = defaults(list(
    show.legend = c(size = FALSE)
  ), GeomSlabinterval$layer_args)
)

#' @rdname geom_pointinterval
#' @export
geom_pointinterval = make_geom(GeomPointinterval)

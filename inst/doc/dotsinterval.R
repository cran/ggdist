## ----chunk_options, include=FALSE---------------------------------------------
tiny_width = 5.5
tiny_height = 3 + 2/3
small_width = med_width = 6.75
small_height = med_height = 4.5
large_width = 8
large_height = 5.25

knitr::opts_chunk$set(
  fig.width = small_width,
  fig.height = small_height
)
if (capabilities("cairo") && Sys.info()[['sysname']] != "Darwin") {
  knitr::opts_chunk$set(
    dev = "png",
    dev.args = list(type = "cairo")
  )
}

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(dplyr)
library(tidyr)
library(distributional)
library(ggdist)
library(ggplot2)
library(patchwork)
library(palmerpenguins)

theme_set(theme_ggdist())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
.old_options = options(width = 120)

## ----dotsinterval_components, echo=FALSE, fig.height=4.15, fig.width=6.5----------------------------------------------
red_ = "#d95f02"
green_ = "#1b9e77"
blue_ = "#7570b3"

bracket_ = function(..., x, xend = x, y, yend = y, color = red_) {
  annotate("segment",
    arrow = arrow(angle = 90, ends = "both", length = unit(3, "points")),
    color = color, size = 0.75,
    x = x, xend = xend, y = y, yend = yend,
    ...
  )
}
thickness_ = function(x) dnorm(x,4,1) * 0.9 / dnorm(4,4,1)
refline_ = function(..., x, xend = x, y, yend = y, color = red_, linetype = "solid", alpha = 0.5) {
  annotate("segment",
    color = color, linetype = linetype, alpha = alpha, size = 0.75,
    x = x, xend = xend, y = y, yend = yend,
    ...
  )
}
label_ = function(..., hjust = 0, color = red_) {
  annotate("text",
    color = color, hjust = hjust, lineheight = 1,
    size = 3.25,
    ...
  )
}
arrow_ = function(..., curvature = 0, x, xend = x, y, yend = y) {
  annotate("curve", 
    color = red_, arrow = arrow(angle = 45, length = unit(3, "points"), type = "closed"),
    curvature = curvature,
    x = x, xend = xend, y = y, yend = yend
  )
}


tibble(dist = dist_normal(4, 1.2)) %>%
  ggplot(aes(y = 0, xdist = dist)) +

  geom_hline(yintercept = 0:1, color = "gray95") +

  stat_dotsinterval(
    aes(interval_size = NULL),
    slab_color = "gray50", 
    .width = 1 - 2*pnorm(-1, sd = 1.2),
    fill = "gray75",
    point_size = 5,
    shape = 22,
    slab_shape = 21,
    stroke = 1.5,
    interval_size = 5,
    slab_size = 1.5
  ) +
  
  # height
  refline_(x = 0, xend = 8.4, y = 1) +
  bracket_(x = 8.4, y = 0, yend = 1) +
  label_(label = "height", x = 8.6, y = 1) +
  
  # scale
  refline_(x = 4, xend = 8.6, y = 0.9) +
  bracket_(x = 8.6, y = 0, yend = 0.9) +
  label_(label = "scale = 0.9", x = 8.8, y = 0.9) +
  
  # slab line properties
  label_(x = 2.5, y = 0.7, 
    label = 'slab_color = "gray50"\nslab_size = 1.5',
    vjust = 1, hjust = 1
  ) +
  arrow_(x = 2.52, xend = 3, y = 0.67, yend = thickness_(3.1) + 0.03, curvature = -0.2) +
  
  # slab fill
  label_(x = 5.5, y = 0.7,
    label = 'slab_fill = fill = "gray75"\nslab_alpha = alpha = 1\nslab_shape = 21',
    vjust = 1,
  ) +
  arrow_(x = 5.48, xend = 4.81, y = 0.67, yend = thickness_(3.1) + 0.01, curvature = 0.2) +

  # xmin / x / xmax
  arrow_(x = 2.65, xend = 3, y = -0.1, yend = -0.01, curvature = -0.2) +
  label_(x = 2.7, y = -0.1, label = "xmin", hjust = 1, vjust = 1) +
  arrow_(x = 4, y = -0.1, yend = -0.05) +
  label_(x = 4, y = -0.1, label = "x", hjust = 0.5, vjust = 1) +
  arrow_(x = 5.35, xend = 5, y = -0.1, yend = -0.01, curvature = 0.2) +
  label_(x = 5.3, y = -0.1, label = "xmax", hjust = 0, vjust = 1) +
  
  # interval properties
  label_(x = 3.5, y = -0.2,
    label = 'interval_color = color = "black"\ninterval_alpha = alpha = 1\ninterval_linetype = linetype = "solid"\ninterval_size = size = 5',
    vjust = 1, hjust = 1
  ) +
  arrow_(x = 3.3, xend = 3.4, y = -0.18, yend = -0.015, curvature = -0.1) +

  # point properties
  label_(x = 4.5, y = -0.2,
    label = 'point_fill = fill = "gray75"\npoint_color = color = "black"\npoint_alpha = alpha = 1\npoint_size = size = 5\nshape = 22\nstroke = 1.5',
    vjust = 1, hjust = 0
  ) +
  arrow_(x = 4.55, xend = 4.2, y = -0.18, yend = -0.03, curvature = 0.2) +
  
  coord_cartesian(xlim = c(-1, 10), ylim = c(-0.6, 1)) +
  labs(subtitle = "Properties of geom_dotsinterval", x = NULL, y = NULL)

## ----dots_components, echo=FALSE, fig.height=3.04, fig.width=6.5------------------------------------------------------
tibble(dist = dist_normal(4, 1.2)) %>%
  ggplot(aes(y = 0, xdist = dist)) +

  geom_hline(yintercept = 0:1, color = "gray95") +

  stat_dots(
    aes(interval_size = NULL),
    color = "gray50",
    fill = "gray75",
    size = 1.5,
    shape = 21
  ) +
  
  # height
  refline_(x = 0, xend = 8.4, y = 1) +
  bracket_(x = 8.4, y = 0, yend = 1) +
  label_(label = "height", x = 8.6, y = 1) +
  
  # scale
  refline_(x = 4, xend = 8.6, y = 0.9) +
  bracket_(x = 8.6, y = 0, yend = 0.9) +
  label_(label = "scale = 0.9", x = 8.8, y = 0.9) +
  
  # slab line properties
  label_(x = 2.5, y = 0.7, 
    label = 'color = "gray50"\nsize = 1.5',
    vjust = 1, hjust = 1
  ) +
  arrow_(x = 2.52, xend = 3, y = 0.67, yend = thickness_(3.1) + 0.03, curvature = -0.2) +
  
  # slab fill
  label_(x = 5.5, y = 0.7,
    label = 'fill = "gray75"\nalpha = 1\nshape = 21',
    vjust = 1,
  ) +
  arrow_(x = 5.48, xend = 4.81, y = 0.67, yend = thickness_(3.1) + 0.01, curvature = 0.2) +

  coord_cartesian(xlim = c(-1, 10), ylim = c(-0.05, 1)) +
  labs(subtitle = "Properties of geom_dots", x = NULL, y = NULL)

## ----layout_params, echo=FALSE, fig.height=3.7, fig.width=6-----------------------------------------------------------
data.frame(x = c(.4, .7, .7, 1, 1, 1)) %>%
# data.frame(x = c(.4, rep(.7, 2), rep(1, 3))) %>%
  ggplot(aes(x = x)) +
  
  geom_hline(yintercept = 0:1, color = "gray95") +
  
  # binwidth
  refline_(x = seq(.25, 1.15, by = .3), y = -0.025, yend = 0.9, color = green_) +
  bracket_(x = .25, xend = .55, y = -0.025, color = green_) +
  label_(
    label = "binwidth = NA\n=> binwidth = 0.3\n(auto-selected so that\n the tallest stack is \u2264 scale)",
    x = 0.55, y = -0.08, vjust = 1, hjust = 1, color = green_
  ) +

  geom_dots(scale = 0.9, dotsize = 1, alpha = 0.5) +
  
  # height
  refline_(x = 0, xend = 2, y = 1) +
  bracket_(x = 2, y = 0, yend = 1) +
  label_(label = "height", x = 2.05, y = 1) +
  
  # scale
  refline_(x = 0.25, xend = 2.1, y = 0.9) +
  bracket_(x = 2.1, y = 0, yend = 0.9) +
  label_(label = "scale = 0.9", x = 2.15, y = 0.9) +

  # stackratio
  refline_(x = 1, xend = 1.3, y = c(.15, .45)) +
  bracket_(x = 1.3, y = .15, yend = .45) +
  label_(label = "stackratio = 1", x = 1.35, y = .3) +

  # dotsize
  refline_(x = c(.85, 1.15), y = 0.15, yend = -0.025, color = blue_, linetype = "22", alpha = 1) +
  bracket_(x = .85, xend = 1.15, y = -0.025, color = blue_) +
  label_(
    label = "dotsize = 1\n(relative to binwidth)",
    x = 0.85, y = -0.08, vjust = 1, hjust = 0, color = blue_
  ) +
  
  scale_x_continuous(limits = c(-0.1, 2.35)) +
  scale_y_continuous(limits = c(-0.35, 1)) +
  coord_fixed() +
  labs(subtitle = "Layout parameters for dots geoms", x = NULL, y = NULL)

## ----side_layout, fig.width = small_width, fig.height = small_width * 3/2---------------------------------------------
set.seed(1234)
x = rnorm(100)

make_plot = function(layout) {
  expand.grid(
    x = x,
    side = c("topright", "both", "bottomleft"),
    stringsAsFactors = FALSE
  ) %>%
    ggplot(aes(side = side, x = x)) +
    stat_dotsinterval(layout = layout) +
    facet_grid(~ side, labeller = "label_both") +
    labs(
      subtitle = paste0("stat_dotsinterval(layout = '", layout, "')"),
      x = NULL,
      y = NULL
    )
}

make_plot("bin") /
  make_plot("weave") /
  make_plot("swarm")

## ----beeswarm_bin-----------------------------------------------------------------------------------------------------
set.seed(1234)

abc_df = data.frame(
  value = rnorm(300, mean = c(1,2,3), sd = c(1,2,2)),
  abc = c("a", "b", "c")
)

abc_df %>%
  ggplot(aes(x = abc, y = value)) +
  stat_dots(side = "both") +
  ggtitle('stat_dots(side = "both")')

## ----beeswarm---------------------------------------------------------------------------------------------------------
abc_df %>%
  ggplot(aes(x = abc, y = value)) +
  stat_dots(side = "both", layout = "swarm") +
  ggtitle('stat_dots(side = "both", layout = "swarm")')

## ----beeswarm_dodge---------------------------------------------------------------------------------------------------
set.seed(12345)

abcc_df = data.frame(
  value = rnorm(300, mean = c(1,2,3,4), sd = c(1,2,2,1)),
  abc = c("a", "b", "c", "c"),
  hi = c("h", "h", "h", "i")
)

abcc_df %>%
  ggplot(aes(y = value, x = abc, fill = hi)) +
  geom_dots(side = "both", position = "dodge") +
  scale_color_brewer(palette = "Dark2") +
  ggtitle(
    'geom_dots(side = "both", position = "dodge")',
    'aes(fill = hi)'
  )

## ----beeswarm_shape_color---------------------------------------------------------------------------------------------
abcc_df %>%
  ggplot(aes(y = value, x = abc, shape = abc, color = hi)) +
  # we'll also increase the `scale` here since we 
  # have some extra space from the dodging
  geom_dots(side = "both", position = "dodge", scale = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle(
    'geom_dots(side = "both", position = "dodge")',
    'aes(shape = abc, fill = hi)'
  )

## ----beeswarm_shape_color_together------------------------------------------------------------------------------------
abcc_df %>%
  ggplot(aes(y = value, x = abc, shape = abc, color = hi, group = NA)) +
  geom_dots() +
  scale_color_brewer(palette = "Dark2") +
  ggtitle(
    'geom_dots()',
    'aes(shape = abc, color = hi, group = NA)'
  )

## ----beeswarm_shape_color_together_stacked----------------------------------------------------------------------------
abcc_df %>%
  ggplot(aes(y = value, x = abc, shape = abc, color = hi, group = NA, order = hi)) +
  geom_dots() +
  scale_color_brewer(palette = "Dark2") +
  ggtitle(
    'geom_dots()',
    'aes(shape = abc, color = hi, group = NA, order = hi)'
  )

## ----beeswarm_shape_color_continuous----------------------------------------------------------------------------------
abcc_df %>%
  arrange(hi) %>%
  ggplot(aes(y = value, x = abc, shape = abc, color = value)) +
  geom_dots() +
  ggtitle(
    'geom_dots()',
    'aes(color = value)'
  )

## ----beeswarm_shape_color_alpha---------------------------------------------------------------------------------------
abcc_df %>%
  arrange(hi) %>%
  ggplot(aes(y = value, x = abc, shape = abc, color = abc, alpha = abs(value))) +
  geom_dots(position = "dodge") +
  ggtitle(
    'geom_dots(side = "both", layout = "swarm")',
    'aes(color = value, alpha = abs(value))'
  )

## ----dotsinterval_dist------------------------------------------------------------------------------------------------
dist_df = tibble(
  dist = c(dist_normal(1,0.25), dist_beta(3,3), dist_gamma(5,5)),
  dist_name = format(dist)
)

dist_df %>%
  ggplot(aes(y = dist_name, xdist = dist)) +
  stat_dotsinterval() +
  ggtitle(
    "stat_dotsinterval()",
    "aes(y = dist_name, xdist = dist)"
  )

## ----dotsinterval_dist_1000-------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(y = dist_name, xdist = dist)) +
  stat_dotsinterval(quantiles = 1000, point_interval = mode_hdci) +
  ggtitle(
    "stat_dotsinterval(quantiles = 1000, point_interval = mode_hdci)",
    "aes(y = dist_name, xdist = dist)"
  )

## ----dotsinterval_dist_1000_level_color-------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(y = dist_name, xdist = dist, slab_color = stat(level))) +
  stat_dotsinterval(quantiles = 1000, point_interval = mode_hdci, layout = "weave") +
  scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_color") +
  ggtitle(
    "stat_dotsinterval(quantiles = 1000, point_interval = mode_hdci)",
    "aes(y = dist_name, xdist = dist, slab_color = stat(level))"
  )

## ----dotsinterval_dist_color------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(y = dist_name, xdist = dist, slab_color = stat(x))) +
  stat_dotsinterval(slab_shape = 19, quantiles = 500) +
  scale_color_distiller(aesthetics = "slab_color", guide = "colorbar2") +
  ggtitle(
    "stat_dotsinterval(slab_shape = 19, quantiles = 500)",
    'aes(slab_color = stat(x)) +\nscale_color_distiller(aesthetics = "slab_color", guide = "colorbar2")'
  )

## ----dist_dots_shape_color, fig.width = med_width, fig.height = large_height------------------------------------------
ab_df = tibble(
  ab = c("a", "b"),
  mean = c(5, 7),
  sd = c(1, 1.5)
)

ab_df %>%
  ggplot(aes(
    y = ab, xdist = dist_normal(mean, sd),
    fill = stat(x < 6), shape = stat(x < 6)
  )) +
  stat_dots(position = "dodge", color = NA) +
  labs(
    title = "stat_dots()",
    subtitle = "aes(xdist = dist_normal(mean, sd), fill and shape = stat(x < 6))"
  ) +
  geom_vline(xintercept = 6, alpha = 0.25) +
  scale_x_continuous(breaks = 2:10) +
  # we'll use these shapes since they have fill and outlines
  scale_shape_manual(values = c(21,22))

## ----dist_dots_weave, fig.width = med_width, fig.height = large_height------------------------------------------------
ab_df %>%
  ggplot(aes(y = ab, xdist = dist_normal(mean, sd), fill = stat(x < 6))) +
  stat_dots(position = "dodge", color = NA, layout = "weave") +
  labs(
    title = 'stat_dots(layout = "weave")',
    subtitle = "aes(fill = stat(x < 6))"
  ) +
  geom_vline(xintercept = 6, alpha = 0.25) +
  scale_x_continuous(breaks = 2:10)

## ----halfeye_dotplot--------------------------------------------------------------------------------------------------
set.seed(12345) # for reproducibility

data.frame(
  abc = c("a", "b", "b", "c"),
  value = rnorm(200, c(1, 8, 8, 3), c(1, 1.5, 1.5, 1))
) %>%
  ggplot(aes(y = abc, x = value, fill = abc)) +
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle(paste0(
      'stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +\n',
      'stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA)'
    ),
    'aes(fill = abc)'
  )

## ----gentoo_raw-------------------------------------------------------------------------------------------------------
gentoo = penguins %>%
  filter(species == "Gentoo", !is.na(sex))

gentoo %>%
  ggplot(aes(x = body_mass_g, y = sex, side = ifelse(sex == "male", "bottom", "top"))) +
  geom_dots(scale = 0.5) +
  ggtitle(
    "geom_dots(scale = 0.5)",
    'aes(side = ifelse(sex == "male", "bottom", "top"))'
  ) 

## ----m_gentoo---------------------------------------------------------------------------------------------------------
m = glm(sex == "male" ~ body_mass_g, data = gentoo, family = binomial)
m

## ----logit_dotplot----------------------------------------------------------------------------------------------------
# construct a prediction grid for the fit line
prediction_grid = with(gentoo, 
  data.frame(body_mass_g = seq(min(body_mass_g), max(body_mass_g), length.out = 100))
)

prediction_grid %>%
  bind_cols(predict(m, ., se.fit = TRUE)) %>%
  mutate(
    # distribution describing uncertainty in log odds
    log_odds = dist_normal(fit, se.fit),
    # inverse-logit transform the log odds to get
    # distribution describing uncertainty in Pr(sex == "male")
    p_male = dist_transformed(log_odds, plogis, qlogis)
  ) %>%
  ggplot(aes(x = body_mass_g)) +
  geom_dots(
    aes(y = as.numeric(sex == "male"), side = ifelse(sex == "male", "bottom", "top")),
    scale = 0.4,
    data = gentoo
  ) +
  stat_lineribbon(
    aes(ydist = p_male), alpha = 1/4, fill = "#08306b"
  ) +
  labs(
    title = "logit dotplot: stat_dots() with stat_lineribbon()",
    subtitle = 'aes(side = ifelse(sex == "male", "bottom", "top"))',
    x = "Body mass (g) of Gentoo penguins",
    y = "Pr(sex = male)"
  )
  

## ----reset_options, include=FALSE---------------------------------------------
options(.old_options)


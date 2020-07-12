## ----chunk_options, include=FALSE---------------------------------------------
tiny_width = small_width = med_width = 6.75
tiny_height = small_height = med_height = 4.5
large_width = 8; large_height = 5.25

knitr::opts_chunk$set(
  fig.width = small_width,
  fig.height = small_height
)
if (capabilities("cairo") && Sys.info()[['sysname']] != "Darwin") {
  knitr::opts_chunk$set(
    dev.args = list(png = list(type = "cairo"))
  )
}

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(dplyr)
library(tidyr)
library(distributional)
library(ggdist)
library(ggplot2)
library(cowplot)

theme_set(theme_ggdist())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
.old_options = options(width = 120)

## ----slabinterval_family, fig.height = 5.5, fig.width = 7, echo = FALSE-----------------------------------------------
dists_df = tibble(
  # enforce order
  geom = rev(c(
    "halfeye", 
    "eye",
    "gradientinterval", 
    "ccdfinterval", 
    "cdfinterval",
    "interval",
    "pointinterval",
    "slab",
    "dots",
    "dotsinterval",
    "histinterval"
    )) %>%
    factor(., levels = .),
  dist = "norm",
  args = list(list(4, 1))
)

hist_df = tibble(
  geom = "histinterval",
  x = qnorm(ppoints(1000), 4, 1),
  dist = NA,
  args = NA
)

dists_plot = dists_df %>%
  ggplot(aes(y = geom, dist = dist, args = args)) +
  geom_blank() + # ensures order
  stat_dist_eye(data = . %>% filter(geom == "eye")) +
  stat_dist_halfeye(data = . %>% filter(geom == "halfeye"), position = position_nudge(y = - 0.2)) +
  stat_dist_gradientinterval(data = . %>% filter(geom == "gradientinterval"), scale = .5) +
  stat_dist_ccdfinterval(data = . %>% filter(geom == "ccdfinterval"), scale = .5) +
  stat_dist_cdfinterval(data = . %>% filter(geom == "cdfinterval"), scale = .5) +
  stat_interval(aes(x = x, y = "interval"), data = hist_df, color = "gray65", alpha = 1/3, size = 10,
    position = position_nudge(y = -.1)) +
  stat_pointinterval(aes(x = x, y = "pointinterval"), data = hist_df) +
  stat_dist_slab(data = . %>% filter(geom == "slab"), position = position_nudge(y = - 0.2)) +
  stat_dist_dotsinterval(data = . %>% filter(geom == "dotsinterval"), position = position_nudge(y = - 0.3)) +
  stat_dist_dots(data = . %>% filter(geom == "dots"), position = position_nudge(y = - 0.3)) +
  stat_histinterval(aes(x = x), data = hist_df, position = position_nudge(y = - 0.4)) +
  scale_slab_alpha_continuous(guide = FALSE) +
  scale_x_continuous(limits = c(0,8), expand = c(0,0)) +
  labs(
    subtitle = "The stat_slabinterval / geom_slabinterval family",
    x = NULL,
    y = NULL
  )

annotation_plot = tribble(
    ~geom,                 ~prefix,
    "halfeye",          c("stat_...", "stat_dist_..."),
    "eye",              c("stat_...", "stat_dist_..."),
    "gradientinterval", c("stat_...", "stat_dist_..."),
    "ccdfinterval",     c("stat_...", "stat_dist_..."),
    "cdfinterval",      c("stat_...", "stat_dist_..."),
    "interval",         c("stat_...", "stat_dist_...", "geom_..."),
    "pointinterval",    c("stat_...", "stat_dist_...", "geom_..."),
    "slab",             c("stat_...", "stat_dist_...", "geom_..."),
    "dotsinterval",     c("stat_...", "stat_dist_...", "geom_..."),
    "dots",             c("stat_...", "stat_dist_...", "geom_..."),
    "histinterval",     c("stat_..."),
  ) %>%
  unnest(prefix) %>%
  mutate(
    geom = factor(geom, levels = levels(dists_df$geom)),
    prefix = factor(prefix, levels = c("stat_...", "stat_dist_...", "geom_..."))
  ) %>%
  ggplot(aes(x = prefix, y = geom)) +
  geom_hline(aes(yintercept = as.numeric(geom) - .1), color = "gray80", data = . %>% filter(prefix == "stat_...")) +
  geom_point(size = 5, color = "gray65", position = position_nudge(y = -.1)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(breaks = NULL, expand = c(0,.6)) +
  labs(y = NULL, x = NULL) +
  theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks = element_blank()) 

plot_grid(ncol = 2, align = "h", rel_widths = c(0.65, 0.35),
  dists_plot,
  annotation_plot
)

## ----sample_data------------------------------------------------------------------------------------------------------
set.seed(1234)
df = tribble(
    ~group, ~subgroup, ~value,
    "a",          "h", rnorm(1000, mean = 5),
    "b",          "h", rnorm(1000, mean = 7, sd = 1.5),
    "c",          "h", rnorm(1000, mean = 8),
    "c",          "i", rnorm(1000, mean = 9),
    "c",          "j", rnorm(1000, mean = 7)
  ) %>%
  unnest(value)

## ----group_eye, fig.width = small_height, fig.height = small_height---------------------------------------------------
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_eye() +
  ggtitle("stat_eye()")

## ----group_halfeye, fig.width = small_height, fig.height = small_height-----------------------------------------------
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_halfeye() +
  ggtitle("stat_halfeye()")

## ----eye_side, fig.width = med_width, fig.height = small_height-------------------------------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

plot_grid(ncol = 3, align = "hv",
  p + stat_eye(side = "left") + labs(title = "stat_eye()", subtitle = "side = 'left'"),
  p + stat_eye(side = "both") + labs(subtitle = "side = 'both'"),
  p + stat_eye(side = "right")  + labs(subtitle = "side = 'right'")
)

## ----halfeyeh, fig.width = small_height, fig.height = small_height----------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value)) +
  stat_halfeye() +
  ggtitle("stat_halfeye()")

## ----eyeh_side, fig.width = med_width, fig.height = small_height------------------------------------------------------
p = df %>%
  ggplot(aes(x = value, y = group)) +
  panel_border()

plot_grid(ncol = 3, align = "hv", 
  # side = "left" would give the same result
  p + stat_eye(side = "left") + ggtitle("stat_eye()") + labs(subtitle = "side = 'bottom'"),
  p + stat_eye(side = "both") + labs(subtitle = "side = 'both'"),
  # side = "right" would give the same result
  p + stat_eye(side = "right") + labs(subtitle = "side = 'top'")
)

## ----eye_dodge--------------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_eye(position = "dodge") +
  ggtitle("stat_eye(position = 'dodge')")

## ----dist_data--------------------------------------------------------------------------------------------------------
dist_df = tribble(
    ~group, ~subgroup, ~mean, ~sd,
    "a",          "h",     5,   1,
    "b",          "h",     7,   1.5,
    "c",          "h",     8,   1,
    "c",          "i",     9,   1,
    "c",          "j",     7,   1
)

## ----dist_eye_dodge---------------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_eye(position = "dodge") +
  ggtitle("stat_dist_eye(position = 'dodge')")

## ----dist_eye_dodge_distributional------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = dist_normal(mean, sd), fill = subgroup)) +
  stat_dist_eye(position = "dodge") +
  ggtitle("stat_dist_eye(position = 'dodge')")

## ----beta_stacked-----------------------------------------------------------------------------------------------------
data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = alpha, dist = dist_beta(alpha, 10))) +
  stat_dist_halfeye() +
  labs(
    title = "stat_dist_halfeye()",
    x = "Beta(alpha,10) distribution"
  )

## ----beta_overplotted_slabh-------------------------------------------------------------------------------------------
data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = "", dist = dist_beta(alpha, 10), color = alpha)) +
  stat_dist_slab(fill = NA) +
  coord_cartesian(expand = FALSE) +
  scale_color_viridis_c() +
  labs(
    title = "stat_dist_slab(fill = NA)",
    x = "Beta(alpha,10) distribution",
    y = NULL
  )

## ----norm_vs_t, fig.width = small_height, fig.height = small_height---------------------------------------------------
tribble(
  ~ dist,      ~ args,
  "norm",      list(0, 1),
  "student_t", list(3, 0, 1)
) %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_halfeye() +
  ggtitle("stat_dist_halfeye()")

## ----priors_fake, eval=FALSE------------------------------------------------------------------------------------------
#  # NB these priors are made up!
#  priors = c(
#    prior(normal(0,1), class = b),
#    prior(lognormal(0,1), class = sigma)
#  )
#  priors

## ----priors, echo=FALSE-----------------------------------------------------------------------------------------------
# we want to avoid a brms dependency, so we fake it above and
# just show the output of brms::prior() here
priors = data.frame(
  prior = c("normal(0, 1)", "lognormal(0, 1)"),
  class = c("b", "sigma"), coef = c("", ""),
  group = c("", ""),
  resp = c("", ""),
  dpar = c("", ""),
  nlpar = c("", ""),
  bound = c("", "")
)
priors

## ----parse_dist-------------------------------------------------------------------------------------------------------
priors %>%
  parse_dist(prior)

## ----prior_dist_halfeyeh----------------------------------------------------------------------------------------------
priors %>%
  parse_dist(prior) %>%
  ggplot(aes(y = class, dist = .dist, args = .args)) +
  stat_dist_halfeye() +
  labs(
    title = "stat_dist_halfeye()",
    subtitle = "with brms::prior() and ggdist::parse_dist() to visualize priors",
    x = NULL
  )

## ----dist_halfeyeh_log_scale, fig.width = small_height, fig.height = small_height-------------------------------------
data.frame(dist = "lnorm") %>%
  ggplot(aes(y = 0, dist = dist, arg1 = log(10), arg2 = 2*log(10))) +
  stat_dist_halfeye() +
  scale_x_log10(breaks = 10^seq(-5,7, by = 2))

## ----stat_histinterval_horizontal, fig.width = med_width, fig.height = small_height-----------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
  p + stat_histinterval() + labs(title = "stat_histinterval()", subtitle = "horizontal"),
  ph + stat_histinterval() + labs(subtitle = "vertical")
)

## ----stat_histintervalh_outlines, fig.width = med_width, fig.height = small_height------------------------------------
plot_grid(ncol = 2, align = "hv",
  ph + stat_histinterval(slab_color = "gray45", outline_bars = FALSE) +
    labs(title = "stat_histinterval", subtitle = "outline_bars = FALSE (default)"),
  ph + stat_histinterval(slab_color = "gray45", outline_bars = TRUE) +
    labs(subtitle = "outline_bars = TRUE")
)

## ----cdfinterval_family, fig.width = small_width, fig.height = small_width--------------------------------------------
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
  p + stat_ccdfinterval() + labs(title = "stat_ccdfinterval()", subtitle = "vertical"),
  ph + stat_ccdfinterval() + labs(subtitle = "horizontal"),
  p + stat_cdfinterval() + labs(title = "stat_cdfinterval()", subtitle = "vertical"),
  ph + stat_cdfinterval()  + labs(subtitle = "horizontal")
)

## ----ccdf_barplot-----------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup, group = subgroup)) +
  stat_ccdfinterval(position = "dodge") +
  ggtitle("stat_ccdfinterval(position = 'dodge')") 

## ----ccdf_dodge-------------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_ccdfinterval(position = "dodge") +
  expand_limits(y = 0) +
  # plus coord_cartesian so there is no space between bars and axis
  coord_cartesian(expand = FALSE) +
  ggtitle("stat_ccdfinterval(position = 'dodge')")

## ----ccdf_justification-----------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_ccdfinterval(position = "dodge", justification = 1) +
  expand_limits(y = 0) +
  # clip = "off" needed here to ensure interval at the edge is visible
  coord_cartesian(expand = FALSE, clip = "off") +
  ggtitle("stat_ccdfinterval(position = 'dodge', justification = 1)")

## ----ccdf_side, fig.width = med_width, fig.height = tiny_height-------------------------------------------------------
p = df %>%
  ggplot(aes(x = value, y = group)) +
  expand_limits(x = 0) +
  panel_border()

plot_grid(ncol = 3, align = "hv", 
  # side = "left" would give the same result
  p + stat_ccdfinterval(side = "bottom") + ggtitle("stat_ccdfinterval()") + labs(subtitle = "side = 'bottom'"),
  p + stat_ccdfinterval(side = "both") + labs(subtitle = "side = 'both'"),
  # side = "right" would give the same result
  p + stat_ccdfinterval(side = "top") + labs(subtitle = "side = 'top'")
)

## ----dist_ccdf_dodge--------------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = dist_normal(mean, sd), fill = subgroup)) +
  stat_dist_ccdfinterval(position = "dodge") +
  expand_limits(y = 0) +
  ggtitle("stat_dist_ccdfinterval(position = 'dodge')") +
  coord_cartesian(expand = FALSE)

## ----gradient_dodge---------------------------------------------------------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_gradientinterval(position = "dodge") +
  labs(title = "stat_gradientinterval(position = 'dodge')")

## ----dist_gradient_dodge----------------------------------------------------------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = dist_normal(mean, sd), fill = subgroup)) +
  stat_dist_gradientinterval(position = "dodge") +
  labs(title = "stat_dist_gradientinterval(position = 'dodge')")

## ----dots_dodge, fig.width = med_width, fig.height = small_height-----------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_dots(position = "dodge") +
  labs(title = "stat_dots(position = 'dodge')")

## ----dots_dodge_nocolor, fig.width = med_width, fig.height = small_height---------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup, color = subgroup)) +
  stat_dots(position = "dodge") +
  labs(title = "stat_dots(slab_color = NA)")

## ----quantile_dots_dodge, fig.width = med_width, fig.height = small_height--------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_dots(position = "dodge", quantiles = 50, color = NA) +
  labs(title = "stat_dots(quantiles = 50)")

## ----dist_dots_shape_color, fig.width = med_width, fig.height = large_height------------------------------------------
dist_df %>%
  ggplot(aes(y = group, dist = dist_normal(mean, sd), fill = stat(x < 5), shape = subgroup)) +
  stat_dist_dots(position = "dodge", color = NA) +
  labs(title = "stat_dist_dots(aes(fill = stat(y < 5), shape = subgroup))") +
  # we'll use these shapes since they retain outlines
  scale_shape_manual(values = c(21,22,23))

## ----dist_dots_violin, fig.width = med_width, fig.height = small_height-----------------------------------------------
dist_df %>%
  ggplot(aes(x = group, dist = dist_normal(mean, sd), fill = subgroup)) +
  stat_dist_dotsinterval(position = "dodge", side = "both", slab_color = NA) +
  labs(title = "stat_dist_dotsinterval(side = 'both', slab_color = NA)") 

## ----ccdf_gradient, fig.width = med_width, fig.height = small_height--------------------------------------------------
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_ccdfinterval(aes(slab_alpha = stat(f)), thickness = 1, position = "dodge") +
  expand_limits(y = 0) +
  # plus coord_cartesian so there is no space between bars and axis
  coord_cartesian(expand = FALSE) +
  ggtitle("stat_ccdfinterval(aes(slab_alpha = stat(f)), thickness = 1)")

## ----norm_vs_t_2, fig.width = small_width, fig.height = small_height--------------------------------------------------
priors = tribble(
  ~ dist,      ~ args,
  "norm",      list(0, 1),
  "student_t", list(3, 0, 1)
) 

priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_halfeye() +
  ggtitle("stat_dist_halfeye()")

## ----norm_vs_t_highlight, fig.width = med_width, fig.height = small_height--------------------------------------------
priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_halfeye(aes(fill = stat(abs(x) < 1.5))) +
  ggtitle("stat_dist_halfeye(aes(fill = stat(abs(x) < 1.5)))") +
  # we'll use a nicer palette than the default for highlighting:
  scale_fill_manual(values = c("gray85", "skyblue"))

## ----norm_vs_t_gradient_eye, fig.width = med_width, fig.height = small_height-----------------------------------------
priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_eye(aes(slab_alpha = stat(f), fill = stat(x > 1))) +
  ggtitle("stat_dist_eye(aes(slab_alpha = stat(f), fill = stat(x > 1)))") +
  # we'll use a nicer palette than the default for highlighting:
  scale_fill_manual(values = c("gray75", "skyblue"))

## ----correll_gradient, fig.width = med_width, fig.height = small_height/2---------------------------------------------
priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_gradientinterval(aes(slab_alpha = stat(-pmax(abs(1 - 2*cdf), .95)))) +
  scale_slab_alpha_continuous(guide = FALSE)

## ----helske_gradient_eye, fig.width = med_width, fig.height = small_height--------------------------------------------
priors %>%
  ggplot(aes(y = dist, dist = dist, args = args)) +
  stat_dist_eye(aes(slab_alpha = stat(-pmax(abs(1 - 2*cdf), .95)))) +
  scale_slab_alpha_continuous(guide = FALSE)

## ----reset_options, include=FALSE---------------------------------------------
options(.old_options)


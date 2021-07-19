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
library(purrr)
library(ggdist)
library(ggplot2)
library(distributional)
library(cowplot)

theme_set(theme_ggdist())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
.old_options = options(width = 120)

## ----df---------------------------------------------------------------------------------------------------------------
set.seed(1234)
n = 5000

df = tibble(
  .draw = 1:n,
  intercept = rnorm(n, 3, 1),
  slope = rnorm(n, 1, 0.25),
  x = list(-4:5),
  y = map2(intercept, slope, ~ .x + .y * -4:5)
) %>%
  unnest(c(x, y))

## ----spaghetti, fig.width = tiny_width, fig.height = tiny_height------------------------------------------------------
df %>%
  filter(.draw %in% 1:100) %>%
  ggplot(aes(x = x, y = y, group = .draw)) +
  geom_line(alpha = 0.25)

## ----summarized_df----------------------------------------------------------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y)

## ----one_ribbon, fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y) %>%
  ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
  geom_lineribbon(fill = "gray65")

## ----geom_lineribbon, fig.width = tiny_width, fig.height = tiny_height------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y, .width = c(.50, .80, .95)) %>%
  ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
  geom_lineribbon() +
  scale_fill_brewer()

## ----stat_lineribbon, fig.width = tiny_width, fig.height = tiny_height------------------------------------------------
df %>%
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon() +
  scale_fill_brewer()

## ----stat_lineribbon_width, fig.width = tiny_width, fig.height = tiny_height------------------------------------------
df %>%
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon(.width = c(.66, .95)) +
  scale_fill_brewer()

## ----stat_lineribbon_gradient, fig.width = tiny_width, fig.height = tiny_height---------------------------------------
df %>%
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon(aes(fill = stat(.width)), .width = ppoints(50)) +
  scale_fill_distiller()

## ----stat_lineribbon_gradient_ramp, fig.width = tiny_width, fig.height = tiny_height----------------------------------
df %>%
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon(aes(fill_ramp = stat(.width)), .width = ppoints(50), fill = "#2171b5") +
  scale_fill_ramp_continuous(range = c(1, 0))

## ----df_2groups-------------------------------------------------------------------------------------------------------
df_2groups = rbind(
  mutate(df, g = "a"),
  mutate(df, g = "b", y = (y - 2) * 0.5)
)

## ----stat_lineribbon_2groups_brewer, fig.width = tiny_width, fig.height = tiny_height---------------------------------
df_2groups %>%
  ggplot(aes(x = x, y = y, color = g)) +
  stat_lineribbon() +
  scale_fill_brewer()

## ----stat_lineribbon_2groups_alpha, fig.width = tiny_width, fig.height = tiny_height----------------------------------
df_2groups %>%
  ggplot(aes(x = x, y = y, fill = g)) +
  stat_lineribbon(alpha = 1/4)

## ----stat_lineribbon_2groups_ramp, fig.width = tiny_width, fig.height = tiny_height-----------------------------------
df_2groups %>%
  ggplot(aes(x = x, y = y, fill = g)) +
  stat_lineribbon(aes(fill_ramp = stat(level)))

## ----analytical_df----------------------------------------------------------------------------------------------------
analytical_df = tibble(
  x = -4:5,
  y_mean = 3 + x,
  y_sd = sqrt(x^2/10 + 1),
)
analytical_df

## ----stat_dist_lineribbon, fig.width = tiny_width, fig.height = tiny_height-------------------------------------------
analytical_df %>%
  ggplot(aes(x = x, dist = dist_normal(y_mean, y_sd))) +
  stat_dist_lineribbon() +
  scale_fill_brewer()

## ----curve_draws, fig.width = tiny_width, fig.height = tiny_height----------------------------------------------------
k = 11 # number of curves
n = 501
df = tibble(
    .draw = 1:k,
    mean = seq(-5,5, length.out = k),
    x = list(seq(-15,15,length.out = n)),
  ) %>%
  unnest(x) %>%
  mutate(y = dnorm(x, mean, 3)/max(dnorm(x, mean, 3)))

df %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(group = .draw), alpha=0.2)

## ----pointwise_ribbon, fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y, .width = .5) %>%
  ggplot(aes(x = x, y = y)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_line(aes(group = .draw), alpha=0.15, data = df) +
  scale_fill_brewer() +
  ggtitle("50% pointwise intervals with point_interval()")

## ----curvewise_ribbon, fig.width = tiny_width, fig.height = tiny_height, eval = requireNamespace("posterior", quietly = TRUE)----
df %>%
  group_by(x) %>%
  curve_interval(y, .width = .5) %>%
  ggplot(aes(x = x, y = y)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_line(aes(group = .draw), alpha=0.15, data = df) +
  scale_fill_brewer() +
  ggtitle("50% curvewise intervals with curve_interval()")

## ----pointwise_curvewise, fig.width = tiny_width, fig.height = tiny_width, eval = requireNamespace("posterior", quietly = TRUE)----
k = 1000 # number of curves
large_df = tibble(
    .draw = 1:k,
    mean = seq(-5,5, length.out = k),
    x = list(seq(-15,15,length.out = n)),
  ) %>%
  unnest(x) %>%
  mutate(y = dnorm(x, mean, 3)/max(dnorm(x, mean, 3)))

pointwise_plot = large_df %>%
  group_by(x) %>%
  median_qi(y, .width = c(.5, .8, .95)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_hline(yintercept = 1, color = "gray75", linetype = "dashed") +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  scale_fill_brewer() +
  ggtitle("point_interval()")

curvewise_plot = large_df %>%
  group_by(x) %>%
  curve_interval(y, .width = c(.5, .8, .95)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_hline(yintercept = 1, color = "gray75", linetype = "dashed") +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  scale_fill_brewer() +
  ggtitle("curve_interval()")

plot_grid(nrow = 2,
  pointwise_plot, curvewise_plot
)

## ----mtcars_boot------------------------------------------------------------------------------------------------------
set.seed(1234)
n = 4000
mpg = seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 100)

mtcars_boot = tibble(
  .draw = 1:n,
  m = map(.draw, ~ loess(
    hp ~ mpg, 
    span = 0.9,
    # this lets us predict outside the range of the data
    control = loess.control(surface = "direct"), 
    data = slice_sample(mtcars, prop = 1, replace = TRUE)
  )),
  hp = map(m, predict, newdata = tibble(mpg)),
  mpg = list(mpg)
) %>%
  select(-m) %>%
  unnest(c(hp, mpg))

## ----mtcars_spaghetti, fig.width = tiny_width, fig.height = tiny_height-----------------------------------------------
mtcars_boot %>%
  filter(.draw < 400) %>%
  ggplot(aes(x = mpg, y = hp)) +
  geom_line(aes(group = .draw), alpha = 1/10) +
  geom_point(data = mtcars) +
  coord_cartesian(ylim = c(0, 400))

## ----mtcars_point_interval, fig.width = tiny_width, fig.height = tiny_height------------------------------------------
mtcars_boot %>%
  ggplot(aes(x = mpg, y = hp)) +
  stat_lineribbon(.width = c(.5, .7, .9)) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 400))

## ----mtcars_curve_interval, fig.width = tiny_width, fig.height = tiny_height, eval = requireNamespace("posterior", quietly = TRUE)----
mtcars_boot %>%
  group_by(mpg) %>%
  curve_interval(hp, .width = c(.5, .7, .9)) %>%
  ggplot(aes(x = mpg, y = hp)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 400))

## ----mtcars_curve_interval_bd, fig.width = tiny_width, fig.height = tiny_height, eval = requireNamespace("fda", quietly = TRUE) && requireNamespace("posterior", quietly = TRUE)----
mtcars_boot %>%
  group_by(mpg) %>%
  curve_interval(hp, .width = c(.5, .7, .9), .interval = "bd-mbd") %>%
  ggplot(aes(x = mpg, y = hp)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 400))

## ----reset_options, include=FALSE---------------------------------------------
options(.old_options)


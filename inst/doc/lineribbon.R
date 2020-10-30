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
library(purrr)
library(ggdist)
library(ggplot2)
library(distributional)
library(cowplot)

theme_set(theme_ggdist())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
.old_options = options(width = 120)

## ----df---------------------------------------------------------------------------------------------------------------
n = 5000

df = tibble(
  .draw = 1:n,
  intercept = rnorm(n, 3, 1),
  slope = rnorm(n, 1, 0.25),
  x = list(-4:5),
  y = map2(intercept, slope, ~ .x + .y * -4:5)
) %>%
  unnest(c(x, y))

## ----spaghetti, fig.width = med_width, fig.height = med_height--------------------------------------------------------
df %>%
  filter(.draw %in% 1:100) %>%
  ggplot(aes(x = x, y = y, group = .draw)) +
  geom_line(alpha = 0.25)

## ----summarized_df----------------------------------------------------------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y)

## ----one_ribbon-------------------------------------------------------------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y) %>%
  ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
  geom_lineribbon()

## ----geom_lineribbon, fig.width = med_width, fig.height = med_height--------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y, .width = c(.50, .80, .95)) %>%
  ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
  geom_lineribbon() +
  scale_fill_brewer()

## ----stat_lineribbon, fig.width = med_width, fig.height = med_height--------------------------------------------------
df %>%
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon() +
  scale_fill_brewer()

## ----stat_lineribbon_width, fig.width = med_width, fig.height = med_height--------------------------------------------
df %>%
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon(.width = c(.66, .95)) +
  scale_fill_brewer()

## ----analytical_df----------------------------------------------------------------------------------------------------
analytical_df = tibble(
  x = -4:5,
  y_mean = 3 + x,
  y_sd = sqrt(x^2/10 + 1),
)
analytical_df

## ----stat_dist_lineribbon---------------------------------------------------------------------------------------------
analytical_df %>%
  ggplot(aes(x = x, dist = dist_normal(y_mean, y_sd))) +
  stat_dist_lineribbon() +
  scale_fill_brewer()

## ----curve_draws, fig.width = med_width, fig.height = med_height------------------------------------------------------
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

## ----pointwise_ribbon, fig.width = med_width, fig.height = med_height-------------------------------------------------
df %>%
  group_by(x) %>%
  median_qi(y, .width = c(.5)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_line(aes(group = .draw), alpha=0.15, data = df) +
  scale_fill_brewer() +
  ggtitle("50% pointwise intervals with point_interval()")

## ----curvewise_ribbon, fig.width = med_width, fig.height = med_height-------------------------------------------------
df %>%
  group_by(x) %>%
  curve_interval(y, .width = c(.5)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_line(aes(group = .draw), alpha=0.15, data = df) +
  scale_fill_brewer() +
  ggtitle("50% curvewise intervals with curve_interval()")

## ----pointwise_curvewise, fig.width = med_width, fig.height = med_width-----------------------------------------------
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

## ----reset_options, include=FALSE---------------------------------------------
options(.old_options)


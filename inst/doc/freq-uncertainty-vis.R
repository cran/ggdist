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
library(ggdist)
library(ggplot2)
library(broom)
library(modelr)

theme_set(theme_ggdist())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
.old_options = options(width = 120)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(5)
n = 10
n_condition = 5
ABC =
  tibble(
    condition = rep(c("A","B","C","D","E"), n),
    response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  ggplot(aes(x = response, y = condition)) +
  geom_point(alpha = 0.5) +
  ylab("condition")

## ---------------------------------------------------------------------------------------------------------------------
m_ABC = lm(response ~ condition, data = ABC)

## ---------------------------------------------------------------------------------------------------------------------
summary(m_ABC)

## ---------------------------------------------------------------------------------------------------------------------
tidy(m_ABC)

## ---------------------------------------------------------------------------------------------------------------------
m_ABC %>%
  tidy() %>%
  ggplot(aes(y = term)) +
  stat_dist_halfeye(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = estimate, arg3 = std.error)
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_halfeye(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit), 
    scale = .5
  ) +
  # we'll add the data back in too (scale = .5 above adjusts the halfeye height so
  # that the data fit in as well)
  geom_point(aes(x = response), data = ABC, pch = "|", size = 2, position = position_nudge(y = -.15))

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_gradientinterval(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit), 
    scale = .5
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_ccdfinterval(
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit)
  )

## ---------------------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_dots(
    quantiles = 100,
    aes(dist = "student_t", arg1 = df.residual(m_ABC), arg2 = .fitted, arg3 = .se.fit)
  )

## ---------------------------------------------------------------------------------------------------------------------
m_mpg = lm(mpg ~ hp * cyl, data = mtcars)

## ---------------------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  augment(m_mpg, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(x = hp, fill = ordered(cyl), color = ordered(cyl))) +
  stat_dist_lineribbon(
    aes(dist = "student_t", arg1 = df.residual(m_mpg), arg2 = .fitted, arg3 = .se.fit), 
    alpha = 1/4
  ) +
  geom_point(aes(y = mpg), data = mtcars) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    color = "cyl",
    fill = "cyl",
    y = "mpg"
  )

## ----reset_options, include=FALSE---------------------------------------------
options(.old_options)


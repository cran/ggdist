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
library(ggdist)
library(ggplot2)
library(broom)
library(modelr)
library(distributional)

theme_set(theme_ggdist())

## ----hidden_options, include=FALSE------------------------------------------------------------------------------------
.old_options = options(width = 120)

## ----data_gen---------------------------------------------------------------------------------------------------------
set.seed(5)
n = 10
n_condition = 5
ABC =
  tibble(
    condition = rep(c("A","B","C","D","E"), n),
    response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
  )

## ----data_plot--------------------------------------------------------------------------------------------------------
ABC %>%
  ggplot(aes(x = response, y = condition)) +
  geom_point(alpha = 0.5) +
  ylab("condition")

## ----m_ABC------------------------------------------------------------------------------------------------------------
m_ABC = lm(response ~ condition, data = ABC)

## ----m_ABC_summary----------------------------------------------------------------------------------------------------
summary(m_ABC)

## ----m_ABC_coefs------------------------------------------------------------------------------------------------------
tidy(m_ABC)

## ----halfeye----------------------------------------------------------------------------------------------------------
m_ABC %>%
  tidy() %>%
  ggplot(aes(y = term)) +
  stat_dist_halfeye(
    aes(dist = dist_student_t(df = df.residual(m_ABC), mu = estimate, sigma = std.error))
  )

## ----halfeye_with_data------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_halfeye(
    aes(dist = dist_student_t(df = df.residual(m_ABC), mu = .fitted, sigma = .se.fit)), 
    scale = .5
  ) +
  # we'll add the data back in too (scale = .5 above adjusts the halfeye height so
  # that the data fit in as well)
  geom_point(aes(x = response), data = ABC, pch = "|", size = 2, position = position_nudge(y = -.15))

## ----gradientinterval-------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_gradientinterval(
    aes(dist = dist_student_t(df = df.residual(m_ABC), mu = .fitted, sigma = .se.fit)), 
    scale = .5, fill_type = "gradient"
  )

## ----ccdfinterval-----------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_ccdfinterval(
    aes(dist = dist_student_t(df = df.residual(m_ABC), mu = .fitted, sigma = .se.fit))
  )

## ----dotplot----------------------------------------------------------------------------------------------------------
ABC %>%
  data_grid(condition) %>%
  augment(m_ABC, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(y = condition)) +
  stat_dist_dots(
    aes(dist = dist_student_t(df = df.residual(m_ABC), mu = .fitted, sigma = .se.fit)),
    quantiles = 100
  )

## ----m_mpg------------------------------------------------------------------------------------------------------------
m_mpg = lm(mpg ~ hp * cyl, data = mtcars)

## ----lineribbon-------------------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  augment(m_mpg, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(x = hp, fill = ordered(cyl), color = ordered(cyl))) +
  stat_dist_lineribbon(
    aes(dist = dist_student_t(df = df.residual(m_mpg), mu = .fitted, sigma = .se.fit)),
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

## ----lineribbon_lightened---------------------------------------------------------------------------------------------
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  augment(m_mpg, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(x = hp, color = ordered(cyl))) +
  stat_dist_lineribbon(aes(
    dist = dist_student_t(df = df.residual(m_mpg), mu = .fitted, sigma = .se.fit),
    fill = ordered(cyl),
    fill_ramp = stat(level)
  )) +
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


# Tests for geom_slabinterval
#
# Author: mjskay
###############################################################################

library(dplyr)




# group_slab_data_by_colour -------------------------------------------------

test_that("group_slab_data_by works", {

  df = data.frame(
    x = 1:8,
    ymin = 2:9,
    ymax = 3:10,
    fill = c("a","a","a","b","b","b","b","a"),
    alpha = 1
  )

  ref = data.frame(
    x =    c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5),
    ymin = c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5) + 1,
    ymax = c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5) + 2,
    fill = c(rep("a", 8), rep("b", 12), rep("a", 4)),
    alpha = 1,
    group = c(rep(0, 8), rep(1, 12), rep(2,4)),
    y    = c(3:5, 5.5, 4.5, 4:2,   5.5, 6:9, 9.5, 8.5, 8:5, 4.5,    9.5, 10, 9, 8.5)
  )

  grouped_slab_data = arrange(group_slab_data_by(df, side = "both"), group)
  expect_equal(grouped_slab_data, ref)

  df$fill = "a"
  expect_equal(group_slab_data_by(df, side = "topright"), mutate(df, y = ymax))

})


test_that("group_slab works", {
  skip_if_no_vdiffr()


  p = tibble(
    x = seq(-4,4, length.out = 20),
    d = dnorm(x)
  ) %>%
    ggplot(aes(thickness = d))

  vdiffr::expect_doppelganger("geom_slab one group",
    p + geom_slab(aes(x = 1, y = x))
  )

  vdiffr::expect_doppelganger("geom_slabh one group",
    p + geom_slab(aes(x = x, y = 1))
  )

})



# normalization -----------------------------------------------------------

test_that("normalize works", {
  skip_if_no_vdiffr()


  p = tribble(
    ~y, ~id, ~p, ~ dist, ~ mu, ~ sigma,
    1,  "A",  1, "norm",    0,       1,
    1,  "B",  1, "norm",    8,       2,
    2,  "A",  1, "norm",    0,       2,
    2,  "A",  2, "norm",    0,       2,
    1,  "B",  2, "norm",    8,       2,
  ) %>%
    ggplot(aes(y = y, dist = dist, arg1 = mu, arg2 = sigma, fill = id)) +
    facet_grid(~p)

  vdiffr::expect_doppelganger("halfeye with normalize = all",
    p + stat_dist_halfeye(normalize = "all", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = panels",
    p + stat_dist_halfeye(normalize = "panels", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = xy",
    p + stat_dist_halfeye(normalize = "xy", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = groups",
    p + stat_dist_halfeye(normalize = "groups", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = none",
    p + stat_dist_halfeye(normalize = "none", n = 20)
  )
})


# alpha in fill colors works ----------------------------------------------

test_that("alpha channel in fill colors works", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("alpha channel in slab fill",
    data.frame(x = c(0,1), y = "a", d = c(1,2)) %>%
      ggplot(aes(x = x, y = y, thickness = d)) +
      geom_slab(fill = scales::alpha("black", 0.2))
  )
})



# side, justification, and scale can vary ---------------------------------

test_that("side and justification can vary", {
  skip_if_no_vdiffr()

  df = tibble(
    x = rep(1:10, each = 2),
    y = dnorm(x, c(4.5, 5.5), 2),
    g = rep(c("a","b"), 10)
  )

  vdiffr::expect_doppelganger("varying side",
    df %>%
      ggplot(aes(x = x, y = g, thickness = y,
        side = ifelse(g == "a", "top", "bottom"),
        scale = ifelse(g == "a", 0.5, 0.25)
      )) +
      geom_slab()
  )

  vdiffr::expect_doppelganger("varying side and just",
    df %>%
      ggplot(aes(x = x, y = g, thickness = y,
        side = ifelse(g == "a", "top", "bottom"),
        justification = ifelse(g == "a", 1, 0),
        scale = ifelse(g == "a", 0.5, 0.25)
      )) +
      geom_slab()
  )

  expect_error(
    print(
      ggplot(df, aes(x = x, y = g, thickness = y, group = g,
        side = ifelse(x < 5, "top", "bottom")
      )) +
      geom_slab(orientation = "horizontal")
    ),
    "Slab `side` cannot vary within groups"
  )

  expect_error(
    print(
      ggplot(df, aes(x = x, y = g, thickness = y, group = g,
        justification = ifelse(x < 5, 0, 1)
      )) +
      geom_slab(orientation = "horizontal")
    ),
    "Slab `justification` cannot vary within groups"
  )

  expect_error(
    print(
      ggplot(df, aes(x = x, y = g, thickness = y, group = g,
        scale = ifelse(x < 5, 0.5, 0.25)
      )) +
      geom_slab(orientation = "horizontal")
    ),
    "Slab `scale` cannot vary within groups"
  )

})


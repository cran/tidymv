## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "300px", fig.align = "center", dpi = 300
)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(itsadug)
library(tidymv)
data(simdat)

## ----gam-----------------------------------------------------------------
set.seed(10)
data <- gamSim(4)
model <- gam(
    y ~
        fac +
        s(x2) +
        s(x2, by = fac) +
        s(x0),
    data = data
)

## ----plot-1-2------------------------------------------------------------
plot_smooths(
    model = model,
    series = x2,
    comparison = fac
) +
    theme(legend.position = "top")

## ----gam-2---------------------------------------------------------------
model_2 <- gam(
    y ~
        s(x0),
    data = data
)

plot_smooths(
    model = model_2,
    series = x0
)

## ----interaction-data----------------------------------------------------
simdata <- simdat %>%
    filter(
    Subject %in% c("a01", "a08", "a15", "c01", "c08", "c15")
) %>%
    mutate(
    GroupCondition = interaction(Group, Condition)
)

model_inter <- bam(
    Y ~
        GroupCondition +
        s(Time, by = GroupCondition),
    data = simdata
)

## ----plot-interactions---------------------------------------------------
plot_smooths(
    model = model_inter,
    series = Time,
    comparison = Group,
    facet_terms = Condition,
    split = list(GroupCondition = c("Group", "Condition"))
) +
    theme(legend.position = "top")

## ----plot-interactions-2-------------------------------------------------
plot_smooths(
    model = model_inter,
    series = Time,
    comparison = Group,
    facet_terms = Condition,
    conditions = quos(Condition == -1),
    split = list(GroupCondition = c("Group", "Condition"))
) +
    theme(legend.position = "top")

## ----plot-interactions-3-------------------------------------------------
plot_smooths(
    model = model_inter,
    series = Time,
    comparison = Group,
    facet_terms = Condition,
    conditions = quos(Condition %in% c(-1, 3)),
    split = list(GroupCondition = c("Group", "Condition"))
) +
    theme(legend.position = "top")

## ----plot-diff-model-----------------------------------------------------
plot_difference(
  model,
  series = x2,
  difference = list(fac = c("1", "2"))
)

## ----plot-diff-inter-1---------------------------------------------------
plot_difference(
  model_inter,
  Time,
  difference = list(GroupCondition = c("Children.1", "Adults.1"))
)

## ----plot-diff-inter-3---------------------------------------------------
plot_difference(
  model_inter,
  Time,
  difference = list(GroupCondition = c("Children.3", "Adults.3"))
)


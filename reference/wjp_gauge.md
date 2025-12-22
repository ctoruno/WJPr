# Plot a Gauge Chart following WJP style guidelines

This function creates a gauge chart using ggplot2 based on the provided
data frame.

## Usage

``` r
wjp_gauge(
  data,
  target,
  colors,
  cvec = NULL,
  factor_order = NULL,
  labels = NULL,
  crop = c(-10, 0, 0, -8),
  ptheme = WJP_theme()
)
```

## Arguments

- data:

  A data frame containing the data to be plotted.

- target:

  A string specifying the variable in the data frame that contains the
  values to be plotted.

- colors:

  A string specifying the variable in the data frame that represents the
  color groupings for the segments.

- cvec:

  A vector of colors to apply to the segments.

- factor_order:

  A vector specifying the order in which the segments should be plotted.

- labels:

  A string specifying the variable in the data frame that contains the
  labels to be displayed. Default is NULL.

- crop:

  A numeric vector specifying the amount of space to crop from the Top,
  Right, Bottom, and Right margins, respectively. Default is
  c(-10,0,0,-8).

- ptheme:

  A ggplot2 theme object to be applied to the plot. Default is
  WJP_theme().

## Value

A ggplot object representing the gauge chart.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  target = c(10, 20, 30, 40),
  colors = c("red", "blue", "green", "yellow"),
  labels = c("A", "B", "C", "D")
)
cvec <- c("red", "blue", "green", "yellow")

wjp_gauge(data, "target", "colors", cvec, labels = "labels")
} # }

```

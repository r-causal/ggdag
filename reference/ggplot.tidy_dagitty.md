# Create a new ggplot

Create a new ggplot

## Usage

``` r
# S3 method for class 'tidy_dagitty'
ggplot(data = NULL, mapping = aes(), ...)

# S3 method for class 'dagitty'
ggplot(data = NULL, mapping = aes(), ...)
```

## Arguments

- data:

  Default dataset to use for plot. If not already a data.frame, will be
  converted to one by
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html).
  If not specified, must be supplied in each layer added to the plot.

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

- ...:

  Other arguments passed on to methods. Not currently used.

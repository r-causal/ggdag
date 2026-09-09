#' Quickly scale the size of a ggplot
#'
#' `expand_plot()` is a convenience function that expands the scales of a
#' ggplot, as the large node sizes in a DAG will often get clipped in themes
#' that don't have DAGs in mind.
#'
#' @param expand_x,expand_y Vector of range expansion constants used to add some
#'   padding around the data, to ensure that they are placed some distance away
#'   from the axes. Use the convenience function `ggplot2::expansion()` to
#'   generate the values for the expand argument. The DAG plotting functions
#'   replace this value on an axis whose nodes all share one coordinate: a
#'   multiplicative expansion of a zero-width range adds nothing, so such an
#'   axis takes an additive expansion of an eighth of the other axis's span on
#'   each side instead.
#' @export
expand_plot <- function(
  expand_x = expansion(c(0.10, 0.10)),
  expand_y = expansion(c(0.10, 0.10))
) {
  list(
    ggplot2::scale_x_continuous(expand = expand_x),
    ggplot2::scale_y_continuous(expand = expand_y)
  )
}

# `expand_plot()` for a plot of `data`, the tidy DAG the plot is drawn from.
#
# A DAG whose nodes all share one coordinate, a chain along a single line for
# instance, trains that axis to a zero-width range. A multiplicative expansion
# of a zero-width range adds nothing, so ggplot2 falls back to a placeholder a
# tenth of a unit on each side of the value under the expansion used here (a
# twentieth under ggplot2's own default), and under `coord_fixed()` the panel
# is then only millimetres tall and the node discs are clipped flat. A
# degenerate axis instead takes an additive expansion of an eighth of the
# other axis's raw span on each side, which for the unit-spaced layouts this
# package draws is plus or minus half a unit.
#
# The other axis keeps the expansion the caller asked for, and when both axes
# are degenerate there is no span to borrow from, so nothing changes. The
# adjustment is made here rather than in a coord, so that a user's own
# `coord_fixed()` still composes.
expand_dag_plot <- function(
  data,
  expand_x = expansion(c(0.10, 0.10)),
  expand_y = expansion(c(0.10, 0.10))
) {
  if (is.tidy_dagitty(data)) {
    data <- pull_dag_data(data)
  }

  range_x <- dag_axis_range(data, "x")
  range_y <- dag_axis_range(data, "y")
  flat_x <- is_zero_range(range_x)
  flat_y <- is_zero_range(range_y)

  if (flat_x && !flat_y) {
    expand_x <- expansion(mult = 0, add = diff(range_y) / 8)
  }
  if (flat_y && !flat_x) {
    expand_y <- expansion(mult = 0, add = diff(range_x) / 8)
  }

  expand_plot(expand_x = expand_x, expand_y = expand_y)
}

# The raw range of one axis of tidy DAG data, node positions and edge ends
# together, before any expansion. `NULL` when the data holds no finite value
# on that axis, which leaves its expansion alone.
dag_axis_range <- function(data, axis) {
  values <- c(data[[axis]], data[[paste0(axis, "end")]])
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(NULL)
  }
  range(values)
}

# Whether a range is zero-width, by the rule ggplot2 applies when it expands
# one: endpoints that are equal, or equal to within a relative tolerance.
is_zero_range <- function(range) {
  if (is.null(range) || anyNA(range)) {
    return(FALSE)
  }
  if (range[[1]] == range[[2]]) {
    return(TRUE)
  }
  smallest <- min(abs(range))
  if (smallest == 0) {
    return(FALSE)
  }
  abs(diff(range) / smallest) < 1000 * .Machine$double.eps
}

#' Minimalist DAG themes
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional arguments passed to `theme()`. A value given here
#'   replaces the theme's own setting for that element. `complete` is the one
#'   exception: the result takes its completeness from the base theme, so a
#'   `complete` passed here has no effect.
#'
#' @export
#'
#' @examples
#' ggdag(m_bias()) + theme_dag_blank() # the default
#'
#' @rdname theme_dag_blank
#'
#' @importFrom ggplot2 %+replace%
theme_dag_blank <- function(base_size = 12, base_family = "", ...) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) %+replace%
    dag_theme(
      list(
        strip.text = ggplot2::element_text(
          face = "bold",
          margin = ggplot2::margin(5, 5, 5, 5)
        ),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      ),
      ...
    )
}

#' @rdname theme_dag_blank
#' @export
theme_dag <- theme_dag_blank

#' @rdname theme_dag_blank
#' @export
#' @importFrom ggplot2 %+replace%
theme_dag_grid <- function(base_size = 12, base_family = "", ...) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) %+replace%
    dag_theme(
      list(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      ),
      ...
    )
}

#' Simple grey themes for DAGs
#'
#' @inheritParams ggplot2::theme_grey
#' @param ... additional arguments passed to `theme()`. A value given here
#'   replaces the theme's own setting for that element. `complete` is the one
#'   exception: the result takes its completeness from the base theme, so a
#'   `complete` passed here has no effect.
#'
#' @export
#'
#' @rdname theme_dag_grey
#'
#' @examples
#'
#' ggdag(m_bias()) + theme_dag_grey()
#'
#' @importFrom ggplot2 %+replace%
theme_dag_grey <- function(base_size = 12, base_family = "", ...) {
  ggplot2::theme_grey(
    base_size = base_size,
    base_family = base_family
  ) %+replace%
    dag_theme(
      list(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour = "grey92"),
        panel.grid.minor = ggplot2::element_line(colour = "grey92")
      ),
      ...
    )
}

#' @rdname theme_dag_grey
#' @export
theme_dag_gray <- theme_dag_grey

#' @rdname theme_dag_grey
#' @export
theme_dag_grey_grid <- function(base_size = 12, base_family = "", ...) {
  ggplot2::theme_grey(
    base_size = base_size,
    base_family = base_family
  ) %+replace%
    dag_theme(
      list(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      ),
      ...
    )
}

#' @rdname theme_dag_grey
#' @export
theme_dag_gray_grid <- theme_dag_grey_grid

# The themes document that `...` reaches `theme()`. Naming the presets as
# literal arguments alongside `...` would instead make R refuse a user value
# for any element the theme sets, so the user's value replaces the preset by
# name before the theme is built.
dag_theme <- function(presets, ...) {
  args <- c(presets, list(complete = TRUE))
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(ggplot2::theme, args)
}

#' Common scale adjustments for DAGs
#'
#' `scale_adjusted()` is a convenience function that implements ways of
#' visualizing adjustment for a variable. By convention, a square shape is used
#' to indicate adjustment and a circle when not adjusted. Arrows out of adjusted
#' variables are often eliminated or de-emphasized, and `scale_adjusted()` uses
#' a lower `alpha` for these arrows. When adjusting a collider, a dashed line is
#' sometimes used to demarcate opened pathways, and `scale_adjusted()` does this
#' whenever [geom_dag_collider_edges()] is used. `scale_dag()` is deprecated in
#' favor of `scale_adjusted()`.
#'
#' @param include_linetype Logical. Include linetype scale for dashed lines on
#'   collider edges? Default is TRUE.
#' @param include_shape Logical. Include shape scale for adjustment status
#'   (squares for adjusted, circles for unadjusted)? Default is TRUE.
#' @param include_color Logical. Include color scale for adjustment status?
#'   Default is TRUE.
#' @param include_alpha Logical. Include alpha scales for de-emphasizing edges
#'   from adjusted variables? Default is FALSE.
#' @param breaks One of:
#'
#'   - NULL for no breaks
#'
#'   - waiver() for the default breaks computed by the transformation object
#'
#'   - A numeric vector of positions
#'
#'   - A function that takes the limits as input and returns breaks as output
#'
#'
#' @export
#' @rdname scale_adjusted
scale_adjusted <- function(
  include_linetype = TRUE,
  include_shape = TRUE,
  include_color = TRUE,
  include_alpha = FALSE
) {
  # Guides that share an `order` still merge into a single legend, so the shape
  # and colour scales stay together, as do the two alpha scales. Without an
  # explicit order, ggplot2 breaks ties with a content hash of the guide, which
  # is not stable across sessions for legends that have no title, and the
  # legends swap places between otherwise identical plots.
  scales <- list(
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = "dashed",
      guide = ggplot2::guide_legend(order = 3)
    ),
    ggplot2::scale_shape_manual(
      values = c("adjusted" = 15, "unadjusted" = 19),
      limits = c("adjusted", "unadjusted"),
      guide = ggplot2::guide_legend(order = 1)
    ),
    ggplot2::scale_color_discrete(
      limits = c("adjusted", "unadjusted"),
      guide = ggplot2::guide_legend(order = 1)
    ),
    ggplot2::scale_alpha_manual(
      values = c("adjusted" = 0.30, "unadjusted" = 1),
      limits = c("adjusted", "unadjusted"),
      guide = ggplot2::guide_legend(order = 2)
    ),
    ggraph::scale_edge_alpha_manual(
      name = NULL,
      values = c("adjusted" = 0.30, "unadjusted" = 1),
      limits = c("adjusted", "unadjusted"),
      guide = ggplot2::guide_legend(order = 2)
    )
  )

  # Filter scales based on arguments
  keep_scale <- c(
    include_linetype,
    include_shape,
    include_color,
    include_alpha,
    include_alpha
  )
  scales[keep_scale]
}

breaks <- function(
  breaks = ggplot2::waiver(),
  name = ggplot2::waiver(),
  drop = TRUE
) {
  list(
    ggplot2::scale_color_discrete(name = name, breaks = breaks, drop = drop),
    ggplot2::scale_fill_discrete(name = name, breaks = breaks, drop = drop)
  )
}

#' @rdname scale_adjusted
#' @export
scale_dag <- function(breaks = ggplot2::waiver()) {
  .Deprecated("scale_adjusted")
  list(
    scale_adjusted(),
    breaks(breaks = breaks)
  )
}

#' Quickly remove plot axes and grids
#'
#' `remove_axes()` and `remove_grid()` are convenience functions that removes
#' the axes and grids from a ggplot, respectively. This is useful when you want
#' to use an existing theme, e.g. those included in `ggplot2`, for a DAG.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggdag(confounder_triangle()) +
#'   theme_bw() +
#'   remove_axes()
#'
#' @rdname remove_axes
remove_axes <- function() {
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
}

#' @rdname remove_axes
#' @export
remove_grid <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank())
}

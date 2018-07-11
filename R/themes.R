#' Quickly scale the size of a ggplot
#'
#' `expand_plot()` is a convenience function that expands the scales of a
#' ggplot, as the large node sizes in a DAG will often get clipped in themes
#' that don't have DAGs in mind.
#'
#' @param expand_x,expand_y Vector of range expansion constants used to add some
#'   padding around the data, to ensure that they are placed some distance away
#'   from the axes. Use the convenience function `expand_scale()` to
#'   generate the values for the expand argument.
#' @export
expand_plot <- function(expand_x = expand_scale(c(.10, .10)),
                        expand_y = expand_scale(c(.10, .10))) {
  list(
      ggplot2::scale_x_continuous(expand = expand_x),
      ggplot2::scale_y_continuous(expand = expand_y)
    )
}

#' Minimalist DAG themes
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional arguments passed to `theme()`
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
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   ..., complete = TRUE)
}

#' @rdname theme_dag_blank
#' @export
theme_dag <- theme_dag_blank

#' @rdname theme_dag_blank
#' @export
#' @importFrom ggplot2 %+replace%
theme_dag_grid <- function(base_size = 12, base_family = "", ...) {
      ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     ..., complete = TRUE)
}

#' Simple grey themes for DAGs
#'
#' @inheritParams ggplot2::theme_grey
#' @param ... additional arguments passed to `theme()`
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
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor = ggplot2::element_line(colour = "grey92"),
                   ..., complete = TRUE)
}

#' @rdname theme_dag_grey
#' @export
theme_dag_gray <- theme_dag_grey

#' @rdname theme_dag_grey
#' @export
theme_dag_grey_grid <- function(base_size = 12, base_family = "", ...) {
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   ..., complete = TRUE)
}

#' @rdname theme_dag_grey
#' @export
theme_dag_gray_grid <- theme_dag_grey_grid

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
scale_adjusted <- function() {
  list(
    ggplot2::scale_linetype_manual(name = NULL, values = "dashed"),
    ggplot2::scale_shape_manual(drop = FALSE, values = c("unadjusted" = 19, "adjusted" = 15)),
    ggplot2::scale_alpha_manual(drop = FALSE, values = c("adjusted" = .30, "unadjusted" = 1)),
    ggraph::scale_edge_alpha_manual(name = NULL, drop = FALSE, values = c("adjusted" = .30, "unadjusted" = 1))
  )
}

breaks <- function(breaks = ggplot2::waiver(), name = ggplot2::waiver()) {
  list(
    ggplot2::scale_color_hue(name = name, drop = FALSE, breaks = breaks),
    ggplot2::scale_fill_hue(name = name, drop = FALSE, breaks = breaks)
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
#'
#' ggdag(confounder_triangle()) +
#' theme_bw() +
#' remove_axes()
#'
#' @rdname remove_axes
remove_axes <- function() {
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())
}

#' @rdname remove_axes
#' @export
remove_grid <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank())
}

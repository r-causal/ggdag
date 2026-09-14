# Common documentation parameters for ggdag
# This file contains documentation-only parameters that define commonly used
# parameters across the ggdag package. These exist solely to be referenced by
# `@inheritParams`.

#' Common DAG parameters
#'
#' @param .tdy_dag A `tidy_dagitty` or `dagitty` object
#' @param .dag A `tidy_dagitty` or `dagitty` object
#' @param exposure A character vector, the exposure variable. Default is
#'   `NULL`, in which case it will be determined from the DAG.
#' @param outcome A character vector, the outcome variable. Default is `NULL`,
#'   in which case it will be determined from the DAG.
#' @param as_factor Logical. Should the column be a factor?
#'
#' @name dag_params
#' @keywords internal
NULL

#' Path-related parameters
#'
#' @param from A character vector with starting node name(s), or `NULL`. If
#'   `NULL`, checks DAG for exposure variable.
#' @param to A character vector with ending node name(s), or `NULL`. If `NULL`,
#'   checks DAG for outcome variable.
#' @param controlling_for A set of variables to control for. This can be a
#'   character vector of variable names, a list of the form `list(c(...))`, or
#'   `NULL`. When `NULL`, no control is applied. Default is `NULL`.
#' @param collider_lines Logical. Should paths opened by conditioning on
#'   colliders be shown? These paths are drawn as dashed ggraph curves whatever
#'   `edge_engine` is in use: they mark an association rather than an edge of
#'   the DAG, so they stay visibly apart from the arrows the engine draws.
#'
#' @name path_params
#' @keywords internal
NULL

#' Edge cap parameter
#'
#' @param edge_cap The distance, in millimetres, that each edge stops short of
#'   the center of the node at either end, scaled by `size`. When neither this
#'   argument nor the `ggdag.edge_cap` option is set, each end of an edge stops
#'   2 mm outside the outline of the node drawn there, a gap also scaled by
#'   `size`, following that node's size and shape under either edge engine, so
#'   an arrowhead keeps the same distance from a large node as from a small
#'   one, and from the side of a square node as from a circle. A number fixes
#'   the cap at every end.
#'
#' @name edge_cap_params
#' @keywords internal
NULL

#' Edge layers of the composite plotters
#'
#' @section Edge layers of the composite plotters:
#' The plotters that color or fade edges by an analysis column build their edge
#' layers themselves, and which layers they build is settled from the DAG they
#' are called with: a DAG with no bidirected edge is given no bidirected edge
#' layer. Replacing the data of the returned plot afterwards, with ggplot2's
#' `%+%`, does not bring a layer back, so a plot built for one DAG is not a
#' template for another.
#'
#' @name composite_edge_layers
#' @keywords internal
NULL

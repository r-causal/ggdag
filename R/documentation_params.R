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
#'   colliders be shown?
#'
#' @name path_params
#' @keywords internal
NULL

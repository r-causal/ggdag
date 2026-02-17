# Path-related parameters

Path-related parameters

## Arguments

- from:

  A character vector with starting node name(s), or `NULL`. If `NULL`,
  checks DAG for exposure variable.

- to:

  A character vector with ending node name(s), or `NULL`. If `NULL`,
  checks DAG for outcome variable.

- controlling_for:

  A set of variables to control for. This can be a character vector of
  variable names, a list of the form `list(c(...))`, or `NULL`. When
  `NULL`, no control is applied. Default is `NULL`.

- collider_lines:

  Logical. Should paths opened by conditioning on colliders be shown?

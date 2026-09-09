# The fifteen fields the user-facing routing object exposes: the name the
# user writes, the name the router reads it under, the rule the constructor
# validates it against, the unit the print method names it in, and whether it
# travels to `route_constants()` as an argument, which a field one of the
# router's derivations reads has to. This table is the only place a user field
# is translated into a router constant.
edge_route_option_fields <- list(
  clearance = list(
    router = "m",
    check = "mm",
    unit = "mm",
    constructor_argument = TRUE
  ),
  edge_sep = list(
    router = "sep_e",
    check = "mm",
    unit = "mm",
    constructor_argument = TRUE
  ),
  edge_sep_min = list(
    router = "sep_min",
    check = "mm",
    unit = "mm",
    constructor_argument = TRUE
  ),
  corners = list(
    router = "corners",
    check = "corners",
    unit = "",
    constructor_argument = TRUE
  ),
  corner_radius = list(
    router = "rc",
    check = "corner_radius",
    unit = "mm",
    constructor_argument = FALSE
  ),
  max_bow = list(
    router = "sagitta_max",
    check = "fraction",
    unit = "of the chord",
    constructor_argument = FALSE
  ),
  bend_penalty = list(
    router = "bend_penalty",
    check = "price",
    unit = "",
    constructor_argument = TRUE
  ),
  crossing_penalty = list(
    router = "crossing_penalty",
    check = "price",
    unit = "",
    constructor_argument = FALSE
  ),
  crossing_saturation = list(
    router = "crossing_saturation",
    check = "flag",
    unit = "",
    constructor_argument = TRUE
  ),
  congestion_penalty = list(
    router = "congestion_penalty",
    check = "price",
    unit = "",
    constructor_argument = FALSE
  ),
  head_penalty = list(
    router = "head_penalty",
    check = "price",
    unit = "",
    constructor_argument = TRUE
  ),
  tight_penalty = list(
    router = "tight_penalty",
    check = "price",
    unit = "",
    constructor_argument = TRUE
  ),
  parallel_sep = list(
    router = "sep_m",
    check = "mm",
    unit = "mm",
    constructor_argument = FALSE
  ),
  steep_angle = list(
    router = "steep_deg",
    check = "angle",
    unit = "degrees",
    constructor_argument = FALSE
  ),
  tangent_clamp = list(
    router = "tangent_clamp",
    check = "angle",
    unit = "degrees",
    constructor_argument = FALSE
  )
)

# The floor the orthogonal ladder shrinks the corner radius to. A radius
# under it is inert rather than shallow, so the constructor names it.
edge_route_rc_min <- 0.8

#' Options for the draw-time edge router
#'
#' `edge_route_options()` collects the constants the edge router draws with
#' and carries them to a plot: pass it to [geom_dag()], [ggdag()], or
#' [geom_dag_routed_arrows()], or set it once for every plot with
#' `ggdag_options_set(edge_route_options = ...)`. It is read only when
#' `edge_route` names a routing mode, `"spline"` or `"orthogonal"`.
#'
#' Every field defaults to `NULL`, meaning the router's own value. The five
#' millimetre fields and `tight_penalty` are derived from the median drawn
#' node radius, which is not known until the panel is measured, so an object
#' that leaves them unset holds at every plot size. The automatic label geoms,
#' [geom_dag_label_auto()] and [geom_dag_text_auto()], route with the same
#' object the edges are drawn with, so labels keep clear of the paths the
#' options produce.
#'
#' @param clearance The daylight in millimetres a routed path keeps beyond a
#'   node disc it is not an endpoint of. `NULL`, the router's own
#'   `max(0.5 r, 1.2)`, where `r` is the drawn node radius: 3 mm at the
#'   default `node_size` of 16. Both modes read it.
#' @param edge_sep The gap in millimetres between two routed paths that share
#'   a slot. `NULL`, the router's own `max(0.6 r, 1.5)`: 3.6 mm at the default
#'   node size. Both modes read it. In spline mode half of it is also the
#'   margin a straight edge keeps from another edge's drawn arrowhead.
#' @param edge_sep_min The floor in millimetres the orthogonal ladder may
#'   tighten `edge_sep` to in a gap too narrow for the full spacing. `NULL`,
#'   the router's own `max(0.25 r, 1.5)` clamped to `edge_sep`: 1.5 mm at the
#'   default node size. A value above a separation the router derives for
#'   itself is reduced to it: `edge_sep_min = 10` beside a separation the
#'   router derives as 3.6 mm is drawn as 3.6. It must not be greater than
#'   `edge_sep` when you set both; setting the two equal fixes the orthogonal
#'   spacing, since there is then nothing left to tighten. Raising it also
#'   raises the spread the widest rung of the ladder reaches, which pushes the
#'   outermost slots further into the neighbouring layers. Orthogonal mode
#'   only.
#' @param corners Whether orthogonal bends are `"rounded"`, the router's own
#'   value, or kept `"sharp"`. `NULL` leaves it to the router. Orthogonal mode
#'   only.
#' @param corner_radius The nominal radius in millimetres of the quadratic
#'   Bezier drawn at an orthogonal bend. `NULL`, the router's own
#'   `min(max(0.35 r, 0.8), 2.5)`: 2.1 mm at the default node size. It must be
#'   at least 0.8 mm, the floor the ladder can shrink it to. A wider radius
#'   reserves a longer stub past the edge cap, which pushes a crowded gap onto
#'   a tighter rung of the ladder. Under `corners = "sharp"` no arc is drawn,
#'   but the radius is still live: it is the tolerance within which a run
#'   counts as level, and the stub still reserves it. Orthogonal mode only.
#' @param max_bow The cap on the sagitta of a free bow, as a fraction of the
#'   chord it spans. `NULL`, the router's own `0.22`. A shallower cap makes a
#'   deep arch infeasible, so more edges take an interior slot or the clamped
#'   fallback; a deeper one restores arches that the saturating crossing price
#'   (see `crossing_saturation`) exists to avoid. Spline mode only.
#' @param bend_penalty The price of one orthogonal bend, in reference radii of
#'   displacement. `NULL`, the router's own `2`, at which two bends cost one
#'   detour. `0` buys every bend a shorter run can pay for. Orthogonal mode
#'   only.
#' @param crossing_penalty The price of one edge crossing, in reference radii
#'   of displacement. `NULL`, the router's own `16`. Both modes read it.
#' @param crossing_saturation Whether the crossing price saturates: the first
#'   crossing of a candidate costs `crossing_penalty` in full and each further
#'   one half of the last, so that in a tangle a route is not driven into a
#'   deep arch by the count alone. `NULL`, the router's own `TRUE`; `FALSE`
#'   prices every crossing in full. Spline mode only; orthogonal mode always
#'   prices crossings linearly.
#' @param congestion_penalty The price per incident edge whose far endpoint
#'   sits on the side a candidate detour takes, in reference radii of
#'   displacement. `NULL`, the router's own `2`. `0` leaves the side of a tie
#'   to the rest of the cost. Both modes read it.
#' @param head_penalty The price of a chain that passes within `edge_sep` of
#'   another edge's arrowhead zone, per zone. `NULL`, the router's own `4`.
#'   Spline mode only.
#' @param tight_penalty The price of threading a gap at the soft margin rather
#'   than at the full clearance. `NULL`, the router's own
#'   `2 (clearance - soft) / r`, where `soft`, the soft margin, is
#'   `min(1.2, clearance)`: 0.6 at the default node size, and `0` under a
#'   `clearance` of 1.2 mm or less, where the soft margin is the clearance
#'   itself and there is nothing to price. Spline mode only.
#' @param parallel_sep The translation in millimetres between the parallel
#'   edges of one node pair. `NULL`, the router's own `max(r, 2.5)`: 6 mm at
#'   the default node size. Both modes read it.
#' @param steep_angle The angle in degrees between a chord and the layer axis
#'   above which an edge takes the free-bow tier rather than an interior slot.
#'   `NULL`, the router's own `60`. Spline mode only.
#' @param tangent_clamp The bound in degrees on how far a departure or arrival
#'   tangent may turn off the chord. `NULL`, the router's own `40`. Spline
#'   mode only. An arrival crowded by other arrivals at the same node may
#'   turn further than `40` when nothing inside it separates the two
#'   arrowheads; a value you set here bounds that too, so a tight clamp
#'   keeps every tangent tight at the cost of the crowded arrivals it would
#'   have separated.
#'
#' @section Fields that price a rule:
#' `bend_penalty`, `crossing_penalty`, `congestion_penalty`, `head_penalty`,
#' and `tight_penalty` are prices, so `0` is a deliberate opt-out of the rule
#' the price enforces rather than an error. So is `crossing_saturation =
#' FALSE`. The defaults are the rules; a zero says the plot in front of you is
#' better off without one of them.
#'
#' @returns An object of class `ggdag_edge_route_options`, a list of the
#'   fifteen fields with the ones you did not set left as `NULL`.
#'
#' @seealso [geom_dag_routed_arrows()], which takes the same object and whose
#'   `clearance`, `edge_sep`, and `edge_sep_min` arguments override the
#'   object's fields for that layer, and [ggdag_options_set()] for the global
#'   `edge_route` and `edge_route_options` options.
#'
#' @examples
#' # x, m, and y sit on one line, so x -> y is blocked by m and detours
#' mediator <- dagify(
#'   y ~ x + m,
#'   m ~ x,
#'   coords = list(
#'     x = c(x = 0, m = 1, y = 2),
#'     y = c(x = 0, m = 0, y = 0)
#'   )
#' )
#'
#' # a spline detour with twice the daylight around the node it passes
#' ggdag(
#'   mediator,
#'   edge_engine = "ggarrow",
#'   edge_route = "spline",
#'   edge_route_options = edge_route_options(clearance = 6)
#' ) +
#'   theme_dag()
#'
#' fan <- dagify(
#'   b ~ a,
#'   c ~ a,
#'   d ~ a,
#'   e ~ b + c + a,
#'   coords = time_ordered_coords()
#' )
#'
#' # orthogonal runs, with the bends kept square instead of rounded
#' ggdag(
#'   fan,
#'   edge_engine = "ggarrow",
#'   edge_route = "orthogonal",
#'   edge_route_options = edge_route_options(corners = "sharp")
#' ) +
#'   theme_dag()
#'
#' @export
edge_route_options <- function(
  clearance = NULL,
  edge_sep = NULL,
  edge_sep_min = NULL,
  corners = NULL,
  corner_radius = NULL,
  max_bow = NULL,
  bend_penalty = NULL,
  crossing_penalty = NULL,
  crossing_saturation = NULL,
  congestion_penalty = NULL,
  head_penalty = NULL,
  tight_penalty = NULL,
  parallel_sep = NULL,
  steep_angle = NULL,
  tangent_clamp = NULL
) {
  given <- list(
    clearance = clearance,
    edge_sep = edge_sep,
    edge_sep_min = edge_sep_min,
    corners = corners,
    corner_radius = corner_radius,
    max_bow = max_bow,
    bend_penalty = bend_penalty,
    crossing_penalty = crossing_penalty,
    crossing_saturation = crossing_saturation,
    congestion_penalty = congestion_penalty,
    head_penalty = head_penalty,
    tight_penalty = tight_penalty,
    parallel_sep = parallel_sep,
    steep_angle = steep_angle,
    tangent_clamp = tangent_clamp
  )

  call <- rlang::current_env()
  fields <- lapply(names(given), function(name) {
    check_edge_route_field(
      given[[name]],
      name,
      edge_route_option_fields[[name]]$check,
      call = call
    )
  })
  names(fields) <- names(given)

  check_edge_sep_floor(fields$edge_sep_min, fields$edge_sep, call = call)

  structure(fields, class = "ggdag_edge_route_options")
}

# The one cross-field rule the object has. The router clamps its own derived
# floor to the separation, which is right for a number nobody typed and wrong
# for a pair the user wrote down: the two together say something the router
# cannot do. Both the constructor and the merge that folds a layer's own
# millimetres into an object check it, so whichever of the two the user wrote
# is the call the error names.
check_edge_sep_floor <- function(
  edge_sep_min,
  edge_sep,
  call = rlang::caller_env()
) {
  if (is.null(edge_sep) || is.null(edge_sep_min) || edge_sep_min <= edge_sep) {
    return(invisible(NULL))
  }

  abort(
    c(
      "{.arg edge_sep_min} must not be greater than {.arg edge_sep}.",
      "x" = "You provided {.val {edge_sep_min}} and {.val {edge_sep}}."
    ),
    error_class = "ggdag_type_error",
    call = call
  )
}

# Whether a value offered where a routing object belongs is one. Every entry
# point that takes the object checks it against its own call, so the error
# names the function the user wrote rather than the layer builder or the
# constructor it reaches.
check_edge_route_options <- function(options, call = rlang::caller_env()) {
  if (!is.null(options) && !inherits(options, "ggdag_edge_route_options")) {
    abort(
      c(
        "{.arg edge_route_options} must be an object from {.fun edge_route_options}.",
        "x" = "You provided {.obj_type_friendly {options}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  invisible(options)
}

# One field of the object, checked against the rule its kind names. `NULL` is
# the unset value at every kind: it says the router keeps its own.
check_edge_route_field <- function(
  value,
  arg,
  kind,
  call = rlang::caller_env()
) {
  if (is.null(value)) {
    return(NULL)
  }

  if (identical(kind, "flag")) {
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      abort(
        c(
          "{.arg {arg}} must be a single logical value ({.val {TRUE}} or {.val {FALSE}}).",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
    return(value)
  }

  if (identical(kind, "corners")) {
    choices <- c("rounded", "sharp")
    if (
      !is.character(value) || length(value) != 1 || !isTRUE(value %in% choices)
    ) {
      abort(
        c(
          "{.arg {arg}} must be one of {.val {choices}}.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
    return(value)
  }

  # an infinite clearance is wider than any panel and an infinite price
  # outbids every rule the router weighs, so neither is a value the router
  # can draw with
  numeric_ok <- is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    is.finite(value)
  in_range <- numeric_ok &&
    switch(
      kind,
      mm = value > 0,
      corner_radius = value >= edge_route_rc_min,
      price = value >= 0,
      fraction = value > 0 && value < 1,
      angle = value > 0 && value <= 90
    )

  if (!in_range) {
    rule <- switch(
      kind,
      mm = "{.arg {arg}} must be a single positive number of millimetres.",
      corner_radius = paste0(
        "{.arg {arg}} must be a single number of millimetres of at least ",
        "{.val {edge_route_rc_min}}, the radius the orthogonal ladder can ",
        "shrink a corner to."
      ),
      price = "{.arg {arg}} must be a single number of at least {.val {0}}.",
      fraction = "{.arg {arg}} must be a single number between {.val {0}} and {.val {1}}, a fraction of the chord it caps.",
      angle = "{.arg {arg}} must be a single number of degrees greater than {.val {0}} and at most {.val {90}}."
    )
    abort(
      c(rule, "x" = "You provided {.obj_type_friendly {value}}."),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  as.numeric(value)
}

#' @export
print.ggdag_edge_route_options <- function(x, ...) {
  cli::cli_text("{.cls ggdag_edge_route_options}")

  set <- names(x)[!vapply(x, is.null, logical(1))]
  if (length(set) > 0) {
    bullets <- vapply(
      set,
      function(name) {
        unit <- edge_route_option_fields[[name]]$unit
        value <- cli::format_inline("{.val {x[[name]]}}")
        if (nzchar(unit)) {
          paste0("{.field ", name, "}: ", value, " ", unit)
        } else {
          paste0("{.field ", name, "}: ", value)
        }
      },
      character(1)
    )
    cli::cli_bullets(stats::setNames(bullets, rep("*", length(bullets))))
  }

  unset <- length(x) - length(set)
  if (unset > 0) {
    cli::cli_bullets(c(
      "i" = "{unset} field{?s} left to the router; the size-dependent ones are derived when the plot is drawn."
    ))
  }

  invisible(x)
}

# The user field to router constant mapping, the one table in the package
# that translates the names. `route_opts_from()` is the one function that
# reads it, and both the drawn arrow grob and the label engine call that.
route_opts_field_map <- function() {
  vapply(
    edge_route_option_fields,
    function(field) field$router,
    character(1)
  )
}

# The router's constants for one reference radius under a user object. An
# object is a partial specification, so a field it does not set reaches the
# router exactly as no object at all does.
route_opts_from <- function(options, r_ref, layer_axis = "auto") {
  options <- options %||% edge_route_options()
  field_map <- route_opts_field_map()
  constructor_argument <- vapply(
    edge_route_option_fields,
    function(field) field$constructor_argument,
    logical(1)
  )

  # the fields a derivation reads have to be in place before the derivation
  # runs, so they travel as arguments; the rest are leaf constants, exact
  # under a substitution after the fact
  args <- list(r_ref = r_ref, layer_axis = layer_axis %||% "auto")
  for (name in names(field_map)[constructor_argument]) {
    if (!is.null(options[[name]])) {
      args[[field_map[[name]]]] <- options[[name]]
    }
  }
  opts <- do.call(route_constants, args)

  for (name in names(field_map)[!constructor_argument]) {
    if (!is.null(options[[name]])) {
      opts[[field_map[[name]]]] <- options[[name]]
    }
  }

  # `tangent_clamp` bounds an arrival tangent as well as a departure one, so
  # a value the user wrote down bounds the two windows the arrival
  # separation opens: a tight clamp suppresses the widening a squeeze would
  # otherwise earn, which is what asking for tight tangents means. Left
  # unset, the router keeps its own wider arrival window.
  if (!is.null(options$tangent_clamp)) {
    opts$arrival_clamp <- min(opts$arrival_clamp, opts$tangent_clamp)
    opts$head_clamp <- min(opts$head_clamp, opts$tangent_clamp)
  }
  opts
}

# The three millimetre arguments a routed layer takes at the call site,
# folded into the object they override. The layer always carries an object,
# so nothing downstream has to ask which of the two spellings was used.
merge_edge_route_options <- function(
  options,
  clearance = NULL,
  edge_sep = NULL,
  edge_sep_min = NULL,
  call = rlang::caller_env()
) {
  check_edge_route_options(options, call = call)
  options <- options %||% edge_route_options()

  # the three overrides are checked here rather than left to the constructor
  # this function calls, so that a value out of range names the layer the
  # user wrote it on and not the internal call that merges it
  clearance <- check_edge_route_field(clearance, "clearance", "mm", call = call)
  edge_sep <- check_edge_route_field(edge_sep, "edge_sep", "mm", call = call)
  edge_sep_min <- check_edge_route_field(
    edge_sep_min,
    "edge_sep_min",
    "mm",
    call = call
  )
  clearance <- clearance %||% options$clearance
  edge_sep <- edge_sep %||% options$edge_sep
  edge_sep_min <- edge_sep_min %||% options$edge_sep_min
  check_edge_sep_floor(edge_sep_min, edge_sep, call = call)

  edge_route_options(
    clearance = clearance,
    edge_sep = edge_sep,
    edge_sep_min = edge_sep_min,
    corners = options$corners,
    corner_radius = options$corner_radius,
    max_bow = options$max_bow,
    bend_penalty = options$bend_penalty,
    crossing_penalty = options$crossing_penalty,
    crossing_saturation = options$crossing_saturation,
    congestion_penalty = options$congestion_penalty,
    head_penalty = options$head_penalty,
    tight_penalty = options$tight_penalty,
    parallel_sep = options$parallel_sep,
    steep_angle = options$steep_angle,
    tangent_clamp = options$tangent_clamp
  )
}

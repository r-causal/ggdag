# Tests for `edge_route_options()`, the object that carries the constants the
# draw-time edge router draws with.
#
# The object is a partial specification: a field left unset is `NULL` and the
# router derives it when the plot is drawn, from the median node radius in
# millimetres, which is not known until the panel is measured. So the
# constructor validates and stores, and one function, `route_opts_from()`,
# turns the object plus a reference radius into the constant list the router
# reads. That function holds the only user-field-to-router-field mapping in
# the package, and both the drawn arrow grob and the label engine call it, so
# a partial specification cannot mean two different things in one picture.
#
# The reference radius here is 6 mm, what the default `node_size` of 16 draws.

r_ref <- 6

# The fifteen fields the object exposes, in the order the constructor takes
# them.
tier_1_fields <- c(
  "clearance",
  "edge_sep",
  "edge_sep_min",
  "corners",
  "corner_radius",
  "max_bow",
  "bend_penalty",
  "crossing_penalty",
  "crossing_saturation",
  "congestion_penalty",
  "head_penalty",
  "tight_penalty",
  "parallel_sep",
  "steep_angle",
  "tangent_clamp"
)

# One case per exposed field: the name the user writes, the name the router
# reads it under, and a value that is not the value the router would derive.
# Every mapping test is driven by this table, so a field cannot be added to
# the constructor without a mapping being written for it.
route_option_cases <- list(
  list(user = "clearance", router = "m", value = 4),
  list(user = "edge_sep", router = "sep_e", value = 5),
  list(user = "edge_sep_min", router = "sep_min", value = 2),
  list(user = "corners", router = "corners", value = "sharp"),
  list(user = "corner_radius", router = "rc", value = 1.5),
  list(user = "max_bow", router = "sagitta_max", value = 0.12),
  list(user = "bend_penalty", router = "bend_penalty", value = 5),
  list(user = "crossing_penalty", router = "crossing_penalty", value = 25),
  list(
    user = "crossing_saturation",
    router = "crossing_saturation",
    value = FALSE
  ),
  list(user = "congestion_penalty", router = "congestion_penalty", value = 3),
  list(user = "head_penalty", router = "head_penalty", value = 7),
  list(user = "tight_penalty", router = "tight_penalty", value = 1.5),
  list(user = "parallel_sep", router = "sep_m", value = 9),
  list(user = "steep_angle", router = "steep_deg", value = 70),
  list(user = "tangent_clamp", router = "tangent_clamp", value = 25)
)

# The object built with one field set to that case's value.
case_options <- function(case) {
  do.call(edge_route_options, stats::setNames(list(case$value), case$user))
}

# The names of the cases, for the table that has to agree with the
# constructor.
case_names <- function(part) {
  vapply(route_option_cases, function(case) case[[part]], character(1))
}

# Construction -------------------------------------------------------------

test_that("edge_route_options() builds an object whose fifteen fields start unset", {
  opts <- edge_route_options()

  expect_s3_class(opts, "ggdag_edge_route_options")
  expect_type(opts, "list")

  # the names stay in the constructor's order and every field is present, so
  # the print method and the mapping table can walk the object without asking
  # whether a field was given
  expect_identical(names(opts), tier_1_fields)
  expect_true(all(vapply(opts, is.null, logical(1))))
})

test_that("edge_route_options() keeps a partial specification partial", {
  opts <- edge_route_options(max_bow = 0.12)

  expect_identical(opts$max_bow, 0.12)
  expect_identical(names(opts), tier_1_fields)

  # the other fourteen are still the router's own, not a number the
  # constructor guessed from a radius it cannot know
  unset <- opts[setdiff(tier_1_fields, "max_bow")]
  expect_true(all(vapply(unset, is.null, logical(1))))
})

test_that("edge_route_options() takes several fields at once", {
  opts <- edge_route_options(
    clearance = 4,
    corners = "sharp",
    crossing_saturation = FALSE
  )

  expect_identical(opts$clearance, 4)
  expect_identical(opts$corners, "sharp")
  expect_identical(opts$crossing_saturation, FALSE)
  expect_null(opts$max_bow)
})

# Validation ---------------------------------------------------------------

test_that("edge_route_options() rejects a millimetre field that is not one positive number", {
  # the constructor's own messages are what these record, so it has to exist
  # before the first snapshot is taken
  expect_s3_class(edge_route_options(), "ggdag_edge_route_options")

  expect_ggdag_error(edge_route_options(clearance = "wide"))
  expect_ggdag_error(edge_route_options(edge_sep = c(2, 3)))
  expect_ggdag_error(edge_route_options(parallel_sep = NA_real_))
  expect_ggdag_error(edge_route_options(clearance = 0))
  expect_ggdag_error(edge_route_options(edge_sep_min = -1))
})

test_that("edge_route_options() rejects a corner radius below the radius the ladder can reach", {
  expect_s3_class(edge_route_options(), "ggdag_edge_route_options")

  # 0.8 mm is the floor the orthogonal ladder shrinks the corner radius to,
  # so anything under it is inert rather than shallow
  expect_ggdag_error(edge_route_options(corner_radius = 0.5))
})

test_that("edge_route_options() rejects a bow, a price, or an angle out of range", {
  expect_s3_class(edge_route_options(), "ggdag_edge_route_options")

  # the sagitta cap is a fraction of the chord it bounds
  expect_ggdag_error(edge_route_options(max_bow = 1.5))
  expect_ggdag_error(edge_route_options(max_bow = 0))

  # a price of zero opts out of the rule it prices; a negative one would pay
  # the router to break it
  expect_ggdag_error(edge_route_options(head_penalty = -1))

  # both angles are measured from an axis, so 90 degrees is the whole quarter
  # turn and nothing beyond it means anything
  expect_ggdag_error(edge_route_options(steep_angle = 120))
  expect_ggdag_error(edge_route_options(tangent_clamp = 0))
})

test_that("edge_route_options() rejects an infinite value", {
  # An infinite millimetre is greater than zero and an infinite price is at
  # least zero, so the range comparisons alone let both through and the
  # router is handed a clearance no panel can hold or a price nothing can
  # outbid. The bow cap and the two angles are bounded above, so their own
  # ranges already reject an infinity.
  expect_error(
    edge_route_options(clearance = Inf),
    class = "ggdag_type_error"
  )
  expect_error(
    edge_route_options(edge_sep = Inf),
    class = "ggdag_type_error"
  )
  expect_error(
    edge_route_options(corner_radius = Inf),
    class = "ggdag_type_error"
  )
  expect_error(
    edge_route_options(head_penalty = Inf),
    class = "ggdag_type_error"
  )
  expect_error(
    edge_route_options(max_bow = Inf),
    class = "ggdag_type_error"
  )
  expect_error(
    edge_route_options(steep_angle = -Inf),
    class = "ggdag_type_error"
  )

  # the rules the two kinds are named by say what they are, so the messages
  # are the ones a finite value out of range gets
  expect_ggdag_error(edge_route_options(clearance = Inf))
  expect_ggdag_error(edge_route_options(head_penalty = Inf))
})

test_that("edge_route_options() rejects a value that is not one of its choices", {
  expect_s3_class(edge_route_options(), "ggdag_edge_route_options")

  expect_ggdag_error(edge_route_options(corners = "beveled"))
  expect_ggdag_error(edge_route_options(crossing_saturation = "yes"))
})

test_that("edge_route_options() rejects a separation floor above the separation it floors", {
  expect_s3_class(edge_route_options(), "ggdag_edge_route_options")

  # the router clamps its own derived floor to the separation, which is right
  # for a number nobody typed and wrong for a pair the user wrote down: the
  # two together say something the router cannot do
  expect_ggdag_error(edge_route_options(edge_sep = 2, edge_sep_min = 5))
})

test_that("edge_route_options() accepts a separation floor equal to the separation", {
  # setting them equal is how a user fixes the orthogonal spacing, so it is
  # the boundary the cross-field rule must let through
  opts <- edge_route_options(edge_sep = 4, edge_sep_min = 4)
  expect_identical(opts$edge_sep_min, 4)
})

test_that("edge_route_options() accepts a price of zero", {
  # zero opts out of the rule the price enforces, which is a thing a user may
  # legitimately ask for
  opts <- edge_route_options(bend_penalty = 0, head_penalty = 0)
  expect_identical(opts$bend_penalty, 0)
  expect_identical(opts$head_penalty, 0)
})

# Printing -----------------------------------------------------------------

test_that("printing an edge_route_options object names what was set and what was not", {
  expect_s3_class(edge_route_options(), "ggdag_edge_route_options")

  expect_snapshot(edge_route_options(
    clearance = 4,
    max_bow = 0.12,
    corners = "sharp"
  ))
  expect_snapshot(edge_route_options())
})

# The mapping onto the router's constants -----------------------------------

test_that("route_opts_from() maps every exposed field onto the router constant it names", {
  for (case in route_option_cases) {
    opts <- route_opts_from(case_options(case), r_ref)
    expect_identical(opts[[case$router]], case$value, label = case$user)
  }
})

test_that("route_opts_from() leaves every field the object does not set to the router", {
  defaults <- route_opts_from(edge_route_options(), r_ref)

  # an object with nothing set says nothing, so it must reach the router as
  # no object at all does
  expect_identical(defaults, route_opts_from(NULL, r_ref))

  # and what the router derives at a 6 mm radius is what it derived before
  # the object existed
  expect_equal(defaults$m, 3)
  expect_equal(defaults$sep_e, 3.6)
  expect_equal(defaults$sep_min, 1.5)
  expect_equal(defaults$rc, 2.1)
  expect_equal(defaults$sagitta_max, 0.22)
  expect_equal(defaults$sep_m, 6)
  expect_equal(defaults$steep_deg, 60)
  expect_equal(defaults$tangent_clamp, 40)
  expect_identical(defaults$corners, "rounded")
  expect_identical(defaults$crossing_saturation, TRUE)
})

test_that("route_opts_from() bounds the arrival window by an explicit tangent_clamp", {
  # tangent_clamp is documented as the bound on a departure or arrival
  # tangent, and a crowded arrival is otherwise allowed a wider window and a
  # wider end tangent than it. A value the user wrote down wins over both, so
  # asking for tight tangents suppresses the widening a squeeze would earn;
  # left unset, the router keeps its own.
  defaults <- route_opts_from(edge_route_options(), r_ref)
  expect_equal(defaults$arrival_clamp, 60)
  expect_equal(defaults$head_clamp, 85)

  tight <- route_opts_from(edge_route_options(tangent_clamp = 10), r_ref)
  expect_equal(tight$tangent_clamp, 10)
  expect_equal(tight$arrival_clamp, 10)
  expect_equal(tight$head_clamp, 10)

  # a clamp above the router's own arrival window does not widen it: the
  # field bounds those windows rather than setting them
  loose <- route_opts_from(edge_route_options(tangent_clamp = 75), r_ref)
  expect_equal(loose$tangent_clamp, 75)
  expect_equal(loose$arrival_clamp, 60)
  expect_equal(loose$head_clamp, 75)
})

test_that("route_opts_from() derives the size-dependent fields from the radius it is given", {
  # the five millimetre fields and the tight-slot price are the reason an
  # unset field cannot be resolved in the constructor: they are the node size
  # the plot happens to draw
  small <- route_opts_from(edge_route_options(), 2)
  large <- route_opts_from(edge_route_options(), 12)

  expect_lt(small$m, large$m)
  expect_lt(small$sep_e, large$sep_e)
  expect_lt(small$sep_m, large$sep_m)
  expect_lte(small$rc, large$rc)

  # a field the user set is the same at either radius
  fixed_small <- route_opts_from(edge_route_options(clearance = 4), 2)
  fixed_large <- route_opts_from(edge_route_options(clearance = 4), 12)
  expect_identical(fixed_small$m, fixed_large$m)
})

test_that("the field table names the flag that says how a field travels", {
  # A field a derivation reads has to be in place before the derivation
  # runs, so it travels to route_constants() as an argument; the rest are
  # leaf constants, substituted afterwards. The flag that tells the two
  # apart is named for what it decides.
  flags <- vapply(
    edge_route_option_fields,
    function(field) field$constructor_argument,
    logical(1)
  )

  expect_setequal(names(flags), tier_1_fields)
  expect_true(flags[["clearance"]])
  expect_true(flags[["edge_sep"]])
  expect_false(flags[["max_bow"]])
  expect_false(flags[["parallel_sep"]])
})

test_that("route_opts_from() reduces a separation floor above the separation in force", {
  # The floor is a floor on the separation actually in force, so a value
  # above it is reduced to it rather than raising it: with the separation
  # left to the router, a 10 mm floor at a 6 mm radius resolves to the 3.6
  # mm the router derived. The roxygen of both spellings says so, since
  # nothing is raised at construction, where the derived separation is not
  # yet known.
  clamped <- route_opts_from(edge_route_options(edge_sep_min = 10), r_ref)
  expect_equal(clamped$sep_min, 3.6)

  # a floor the user typed beside a separation that holds it is kept
  typed <- route_opts_from(
    edge_route_options(edge_sep = 10, edge_sep_min = 10),
    r_ref
  )
  expect_equal(typed$sep_min, 10)
})

test_that("the mapping table and the constructor's fields agree", {
  field_map <- route_opts_field_map()

  # a field added to the constructor without a mapping would silently do
  # nothing, so the two lists are pinned to each other
  expect_setequal(names(formals(edge_route_options)), names(field_map))
  expect_setequal(names(field_map), tier_1_fields)

  # and the table here is the same table, so the mapping tests above cover
  # every field
  expect_setequal(case_names("user"), names(field_map))
  expect_setequal(case_names("router"), unname(field_map))

  # every router constant the table names is one the router actually reads
  expect_contains(
    names(route_opts_from(edge_route_options(), r_ref)),
    unname(field_map)
  )
})

# Changelog

## ggdag (development version)

- [`geom_dag()`](https://r-causal.github.io/ggdag/reference/geom_dag.md)
  gains a `label_geom` parameter that allows users to specify which geom
  function to use for labels when `use_labels = TRUE`
  ([\#133](https://github.com/r-causal/ggdag/issues/133)). The default
  remains `geom_dag_label_repel` for backward compatibility, but users
  can now choose any label/text geom function such as `geom_dag_label`,
  `geom_dag_text_repel`, `geom_dag_label_repel2`, or
  `geom_dag_text_repel2`.

- [`geom_dag_text_repel()`](https://r-causal.github.io/ggdag/reference/repel.md)
  and
  [`geom_dag_label_repel()`](https://r-causal.github.io/ggdag/reference/repel.md)
  now support all current ggrepel parameters
  ([\#172](https://github.com/r-causal/ggdag/issues/172)):

  - Added `stat`, `position`, `min.segment.length`, `force_pull`,
    `max.time`, `max.overlaps`, `xlim`, `ylim`, `direction`, `seed`, and
    `verbose` parameters
  - `segment.alpha` now defaults to 1 but can be overridden by users
  - All segment.\* parameters (e.g., `segment.linetype`,
    `segment.curvature`, `segment.angle`) now pass through correctly via
    `...`
  - Parameters like `point.size` and `point.colour` can also be passed
    through `...`
  - Fixed support for
    [`ggrepel::position_nudge_repel()`](https://ggrepel.slowkow.com/reference/position_nudge_repel.html) -
    users can now use either `nudge_x`/`nudge_y` parameters or
    `position = position_nudge_repel()`
  - Both approaches now work with vector inputs for differential nudging
    of labels

- Added comprehensive set of `is_*()` functions for testing DAG
  properties:

  - [`is_acyclic()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md):
    Test if a DAG is acyclic
  - [`is_adjustment_set()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md):
    Test if a set of variables is a valid adjustment set
  - [`is_d_separated()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md),
    [`is_d_connected()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md):
    Test d-separation relationships
  - [`is_exogenous()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md):
    Test if a variable has no parents
  - [`is_instrumental()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md):
    Test if a variable is instrumental
  - [`is_exposure()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md),
    [`is_outcome()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md),
    [`is_latent()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md):
    Test variable status
  - [`is_parent()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md),
    [`is_child()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md),
    [`is_ancestor()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md),
    [`is_descendant()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md):
    Test node relationships
  - [`is_adjacent()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md):
    Test if two nodes are adjacent

- Added
  [`edge_backdoor()`](https://r-causal.github.io/ggdag/reference/edge_backdoor.md)
  function to classify edges as being on backdoor paths, direct causal
  paths, or both between exposure and outcome
  ([\#137](https://github.com/r-causal/ggdag/issues/137))

- Enhanced
  [`dag_paths()`](https://r-causal.github.io/ggdag/reference/paths.md)
  and
  [`query_paths()`](https://r-causal.github.io/ggdag/reference/query_paths.md)
  to include `path_type` column that classifies paths as “backdoor” or
  “direct” ([\#137](https://github.com/r-causal/ggdag/issues/137))

- Changed default aesthetic in
  [`ggdag_paths()`](https://r-causal.github.io/ggdag/reference/paths.md)
  from coloring by path status to coloring by path type (direct vs
  backdoor), providing more informative visualization of causal pathways

- Introduced new `query_*()` API for direct analytical queries on DAGs
  ([\#185](https://github.com/r-causal/ggdag/issues/185)). These
  functions return tibbles with results rather than tidy DAG objects for
  plotting:

  - [`query_adjustment_sets()`](https://r-causal.github.io/ggdag/reference/query_adjustment_sets.md):
    Find adjustment sets to close backdoor paths
  - [`query_paths()`](https://r-causal.github.io/ggdag/reference/query_paths.md):
    Find and analyze paths between nodes
  - [`query_instrumental()`](https://r-causal.github.io/ggdag/reference/query_instrumental.md):
    Identify instrumental variables
  - [`query_dseparated()`](https://r-causal.github.io/ggdag/reference/query_dseparated.md)/[`query_dconnected()`](https://r-causal.github.io/ggdag/reference/query_dconnected.md):
    Test d-separation relationships
  - [`query_colliders()`](https://r-causal.github.io/ggdag/reference/query_colliders.md):
    Identify collider nodes
  - [`query_exogenous()`](https://r-causal.github.io/ggdag/reference/query_exogenous.md):
    Find exogenous variables
  - [`query_parents()`](https://r-causal.github.io/ggdag/reference/query_parents.md),
    [`query_children()`](https://r-causal.github.io/ggdag/reference/query_children.md),
    [`query_ancestors()`](https://r-causal.github.io/ggdag/reference/query_ancestors.md),
    [`query_descendants()`](https://r-causal.github.io/ggdag/reference/query_descendants.md):
    Query node relationships
  - [`query_markov_blanket()`](https://r-causal.github.io/ggdag/reference/query_markov_blanket.md):
    Find Markov blankets

- All error messages, warnings, and informational messages now use the
  cli package for better formatting and user experience. This change
  adds custom error classes (`ggdag_error`, `ggdag_warning`) that enable
  programmatic error handling
  ([\#191](https://github.com/r-causal/ggdag/issues/191)).

- Fixed
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)
  error when processing DAGs with no edges
  ([\#159](https://github.com/r-causal/ggdag/issues/159))

- Fixed
  [`dag_paths()`](https://r-causal.github.io/ggdag/reference/paths.md)
  error when no open paths exist between nodes
  ([\#180](https://github.com/r-causal/ggdag/issues/180))

- Introduced new functions
  [`query_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md),
  [`test_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md),
  and
  [`ggdag_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  for detecting, testing, and visualizing implied conditional
  independencies in a given DAG and dataset
  ([\#139](https://github.com/r-causal/ggdag/issues/139))

- Added `sort` parameter to
  [`ggdag_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  with default `TRUE` to sort conditional independence tests by their
  estimates for improved readability
  ([\#170](https://github.com/r-causal/ggdag/issues/170))

- Introduced new functions
  [`aes_dag()`](https://r-causal.github.io/ggdag/reference/aes_dag.md)
  and
  [`geom_dag()`](https://r-causal.github.io/ggdag/reference/geom_dag.md)
  to simplify specification of ggplot code for most DAGs. Also
  refactored most quick plots to use these functions
  ([\#121](https://github.com/r-causal/ggdag/issues/121))

- Added new function `geom_label_repel2()` for more opinionated repelled
  labels that often look better on DAGs
  ([\#132](https://github.com/r-causal/ggdag/issues/132))

- Improved DAG data structure by removing unnecessary `circular` column
  when not needed. The column is now only included when using circular
  layouts. This simplifies the tidy DAG structure for most use cases
  ([\#119](https://github.com/r-causal/ggdag/issues/119))

- Edge geoms now support mapping `color`/`colour` aesthetics to
  `edge_color`/`edge_colour` for more intuitive usage, matching ggplot2
  conventions ([\#166](https://github.com/r-causal/ggdag/issues/166))

- Fixed
  [`theme_dag()`](https://r-causal.github.io/ggdag/reference/theme_dag_blank.md)
  facet labels being clipped by adding margin to strip text
  ([\#173](https://github.com/r-causal/ggdag/issues/173))

- Added quick plot functions for the causal quartet:
  [`quartet_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md),
  [`quartet_confounder()`](https://r-causal.github.io/ggdag/reference/quick_plot.md),
  [`quartet_mediator()`](https://r-causal.github.io/ggdag/reference/quick_plot.md),
  [`quartet_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md),
  and
  [`quartet_time_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md),
  along with their `ggdag_*` counterparts. These functions create DAGs
  representing the causal quartet from D’Agostino McGowan, Gerke, and
  Barrett (2023), demonstrating that statistical properties alone cannot
  determine causal relationships
  ([\#171](https://github.com/r-causal/ggdag/issues/171))

## ggdag 0.2.11

CRAN release: 2024-01-24

- Internal update to address upcoming changes in ggplot2
  ([\#125](https://github.com/r-causal/ggdag/issues/125), thanks
  [@teunbrand](https://github.com/teunbrand))
- Implemented automatic time-ordered coordinates for
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  and `layout` ([\#115](https://github.com/r-causal/ggdag/issues/115))
- Added
  [`geom_dag_label()`](https://r-causal.github.io/ggdag/reference/geom_dag_label.md)
  to complement
  [`geom_dag_text()`](https://r-causal.github.io/ggdag/reference/geom_dag_text.md)
  and the repel functions
  ([\#116](https://github.com/r-causal/ggdag/issues/116))
- Added
  [`pull_dag()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  and
  [`pull_dag_data()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  to extract DAG components,
  [`update_dag()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  and `update_dag_data()` to update DAG components, and
  [`as_tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/as_tidy_dagitty.md)
  to construct DAGs from data frames
  ([\#117](https://github.com/r-causal/ggdag/issues/117)). Thanks to
  [@lorenzoFabbri](https://github.com/lorenzoFabbri) for inspiring some
  of the data-to-dag code in
  [\#90](https://github.com/r-causal/ggdag/issues/90).

## ggdag 0.2.10

CRAN release: 2023-05-28

- Fixed bug where ggdag was using wrong
  [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  interface with dplyr 1.1.0
  ([\#110](https://github.com/r-causal/ggdag/issues/110))

## ggdag 0.2.9

CRAN release: 2023-05-22

- Fixed bugs in instrumental functions for complex and non-existent IVs
  ([\#99](https://github.com/r-causal/ggdag/issues/99))
- Updated URLs to new r-causal GitHub repository
  ([\#94](https://github.com/r-causal/ggdag/issues/94))

## ggdag 0.2.8

CRAN release: 2023-03-17

- Added new function
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  for calculating time-ordered coordinates
  ([\#88](https://github.com/r-causal/ggdag/issues/88))
- Addressed changes requested by CRAN, other changes in tidyverse
  ([\#87](https://github.com/r-causal/ggdag/issues/87),
  [\#86](https://github.com/r-causal/ggdag/issues/86),
  [\#85](https://github.com/r-causal/ggdag/issues/85))

## ggdag 0.2.7

CRAN release: 2022-10-29

- Fixed various bugs in calculating and visualizing paths
  ([\#75](https://github.com/r-causal/ggdag/issues/75))
- Fixed breaking changes from the upcoming release of ggplot2 3.4.0
  ([\#79](https://github.com/r-causal/ggdag/issues/79))
- `size` is now deprecated for
  [`geom_dag_collider_edges()`](https://r-causal.github.io/ggdag/reference/geom_dag_collider_edges.md)
  in favor of `linewidth`, related to
  [\#79](https://github.com/r-causal/ggdag/issues/79).

## ggdag 0.2.6

CRAN release: 2022-08-26

- Fixed CRAN errors

## ggdag 0.2.5

CRAN release: 2022-07-16

- Fixed issue with
  [`dag_adjustment_sets()`](https://r-causal.github.io/ggdag/reference/adjustment_sets.md)
  when `width` option was low by extracting adjustment sets directly
  ([\#70](https://github.com/r-causal/ggdag/issues/70))
- Fixed issue with
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)
  that missed nodes with no edges attached. May cause minor breaking
  changes in randomized layouts.
  ([\#68](https://github.com/r-causal/ggdag/issues/68))
- Added the `limit` argument to `dag_paths`, `ggdag_paths`, and
  `ggdag_paths_fan` (see
  [`dagitty::paths`](https://rdrr.io/pkg/dagitty/man/paths.html))
  ([\#65](https://github.com/r-causal/ggdag/issues/65)).

## ggdag 0.2.4

CRAN release: 2021-10-10

- [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)
  no longer allows the dendogram layout type
  ([\#62](https://github.com/r-causal/ggdag/issues/62))
- [`scale_adjusted()`](https://r-causal.github.io/ggdag/reference/scale_adjusted.md)
  now correctly aligns legend types
  ([\#61](https://github.com/r-causal/ggdag/issues/61))
- ggdag no longer loads ggplot2 via `Depends`, instead importing it
  internally ([\#57](https://github.com/r-causal/ggdag/issues/57)).

## ggdag 0.2.3

CRAN release: 2021-01-12

- Update vdiffr to be explicitly conditional
  ([\#51](https://github.com/r-causal/ggdag/issues/51))
- Fixed bug in
  [`activate_collider_paths()`](https://r-causal.github.io/ggdag/reference/activate_collider_paths.md)
  that causes an error where a collider had a high number of ancestors
  ([\#49](https://github.com/r-causal/ggdag/issues/49))
- Fixed bug in
  [`node_equivalent_dags()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  where extra columns did not get joined to new tidy DAG
  ([\#40](https://github.com/r-causal/ggdag/issues/40))

## ggdag 0.2.2

CRAN release: 2020-02-13

- Added vdiffr tests, as well as a basic test file for every `.R` file.
  ([\#27](https://github.com/r-causal/ggdag/issues/27))
- Updated roxygen2 version
  ([\#29](https://github.com/r-causal/ggdag/issues/29))
- Fixed bugs in
  [`dag_paths()`](https://r-causal.github.io/ggdag/reference/paths.md)
  and
  [`geom_dag_edges_fan()`](https://r-causal.github.io/ggdag/reference/geom_dag_edge_functions.md)
  ([\#31](https://github.com/r-causal/ggdag/issues/31))
- Removed use of `use_defaults()` `ggproto` method in `GeomDAGEdge`
  ([\#33](https://github.com/r-causal/ggdag/issues/33))
- Use
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  instead of
  [`expand_scale()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  when ggplot2 version is at least `3.3.0` and removed local version of
  [`expand_scale()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  ([\#34](https://github.com/r-causal/ggdag/issues/34))

## ggdag 0.2.1

CRAN release: 2019-12-06

- Fixed bug in
  [`ggdag_collider()`](https://r-causal.github.io/ggdag/reference/colliders.md)
  where the aesthetics were flipped for the colors but not the labels
  (issue [\#15](https://github.com/r-causal/ggdag/issues/15), b72e34b)
- Used [`as.character.default()`](https://rdrr.io/r/base/character.html)
  directly for working with characters to avoid dispatch error when the
  formula.tools package is attached (issue
  [\#17](https://github.com/r-causal/ggdag/issues/17), 0f32bb4)

## ggdag 0.2.0

CRAN release: 2019-09-12

- Fixed compatibility issue with ggraph 2.0.0 by changing to `strength`
  parameter in curved geoms
- Fixed join bug in
  [`node_equivalent_class()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  that didn’t account for the way dagitty returns DAGs with no direction
- Fixed join bug in
  [`node_equivalent_class()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  that didn’t check `to` node
- Implemented `is_false()` to avoid dependency on R 3.5.0
- improved edge lengths
- add [`{}`](https://rdrr.io/r/base/Paren.html) to adjustment set names
  to reflect convention
- Set nodes to be unstyled by default
- Changed default themes and scales to be more like base ggplot2
- Added a `NEWS.md` file to track changes to the package.

## ggdag 0.1.0

CRAN release: 2018-03-27

- Initial release

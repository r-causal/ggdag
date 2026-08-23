# ggdag (development version)

* `is_confounder()` now requires `z` to be a common cause of `x` and `y`: `z` must reach each of them by a directed path that does not run through the other. It previously tested only whether `x` and `y` were descendants of `z`. Being a descendant is transitive through `x`, so every upstream cause of the exposure was reported as a confounder, including instruments, mediators, and variables that open no backdoor path at all.

* `is_instrumental()` no longer reports a conditioning variable as an instrument. dagitty returns a conditional instrument as the instrument together with the set that has to be conditioned on for it to work; both were matched against the variable being tested, so a variable that has to be adjusted for came back as an instrument.

* `node_instrumental()` now marks the variables of an unconditional instrument as unadjusted. A DAG with both an unconditional and a conditional instrument left that column missing for the unconditional one, and `ggdag_instrumental()` maps it to shape, so its facet was drawn with edges and labels but no nodes.

* `activate_collider_paths()`, and with it `control_for()` and `ggdag_adjust()`, now judges whether a path is open under the whole of `adjust_for` rather than under the adjusted colliders alone, and drops every adjusted variable from the pairs a biasing pathway can join. Adjusting for a collider and for a variable that blocks the path it opens no longer draws a bias line for an association that the adjustment blocks.

* `activate_collider_paths()` now warns when `dagitty::paths()` reaches its enumeration limit, which happens on dense DAGs and can hide a pathway the adjustment opens. One warning names every pair affected by a call. It also no longer fails with a type error when the two variables sit in disconnected parts of the DAG.

* `activate_collider_paths()` now rejects arguments passed through `...` when its input is already a `tidy_dagitty`. `...` is forwarded to `tidy_dagitty()`, which is not called in that case, so those arguments were silently discarded.

* `query_parents()`, `query_children()`, `query_ancestors()`, `query_descendants()`, and `query_markov_blanket()` now represent an empty set as `"{}"` with a zero-length list element, the convention `query_adjustment_sets()` and `query_instrumental()` already use. They previously substituted `NA`, so the list column reported a length of 1 for a node whose `n_*` column said 0.

* `query_parents()`, `query_children()`, `query_ancestors()`, `query_descendants()`, `query_markov_blanket()`, and `query_status()` now raise `ggdag_missing_nodes_error` when `.var` names a variable that is not in the DAG. The first five surfaced an internal dagitty error instead, and `query_status()` returned a plausible-looking row of `NA` status, indistinguishable from a real unlabeled node.

* `query_conditional_independence()` now returns a zero-row tibble with its documented columns when a DAG implies no conditional independencies, rather than a tibble with no columns at all. Its `set` column is now the integer position of each conditional independence for every query type. dagitty names the independencies of the default `"missing.edge"` query, so `set` came back as a character vector of those names, and a result could not be row-bound with an empty one.

* `query_conditional_independence()` now renders an empty conditioning set as `"{}"` with a zero-length list element, matching the rest of the query API. It previously reported `NA`.

* `query_parents()`, `query_children()`, `query_ancestors()`, `query_descendants()`, `query_markov_blanket()`, and `query_status()` now return a zero-row tibble with their documented columns when `.var` names no nodes, and ask about a repeated node once rather than giving it a row per mention.

* `query_instrumental()` now raises an error when `conditioned_on` is supplied. The argument never had an effect: `dagitty::instrumentalVariables()` works out the conditioning set an instrument requires itself and reports it in the `conditioning_set` and `conditioned_on` columns.

* `dag_adjustment_sets()`, `ggdag_adjustment_set()`, `is_adjustment_set()`, `is_instrumental()`, `node_instrumental()`, and `ggdag_instrumental()` now raise `ggdag_missing_error` when the exposure and outcome they need are not set, in place of the error dagitty raises, which carries no ggdag class. The instrumental variable functions also report when either endpoint names more than one variable, which the algorithm in dagitty does not allow. An endpoint of `NA` is treated as unset rather than as a variable missing from the DAG.

* `ggdag_adjust()` now raises the `ggdag_missing_error` it always intended to when no adjusting variable is set. `dagitty::adjustedNodes()` reports an unadjusted DAG as an empty list rather than as `NULL`, so the guard never fired and a plot of no adjustment was drawn instead. Passing `var = character(0)` errors for the same reason.

* `ggdag_conditional_independence()` now reports the missing `independence` column when given the raw output of `dagitty::localTests()`, which keeps the independence statements in its row names. It previously failed with a low-level replacement error.

* The `query_*()` functions now raise the package's own error classes when an exposure, outcome, `from`, or `to` is missing, or when `from` or `to` is not a character vector. These conditions carried no ggdag class, so `tryCatch()` handlers written for the rest of the package missed them.

* `is_collider()`, `is_downstream_collider()`, `node_collider()`, `ggdag_collider()`, and `query_colliders()` now count the arrowheads that bidirected edges contribute. A variable with one directed parent and one bidirected partner, or with two bidirected partners, has two arrowheads pointing into it and is a collider; dagitty counts only directed edges as parents, so such a variable was previously reported as a non-collider. `activate_collider_paths()`, and with it `control_for()` and `ggdag_adjust()`, now draws the biasing pathway that conditioning on such a collider opens.

* `activate_collider_paths()` now connects a pair of variables only when adjusting for the collider opens a path between them that is closed without the adjustment. It previously drew a biasing pathway between every pair of an adjusted collider's ancestors, including pairs that conditioning on the collider cannot connect, such as a cause and its own descendant.

* `activate_collider_paths()` now adds one row per activated pair. Its coordinate joins did not drop duplicate matches, so a variable with several edges multiplied the pair's row, which overdrew the dashed bias edge and inflated the path counts reported by printing a `tidy_dagitty` and by `n_edges()`.

* `node_collider()` is now idempotent: applying it to its own output, which happens when a precomputed result is piped into `ggdag_collider()`, returns the single-application result instead of failing with a size error from the suffixed `colliders.x` and `colliders.y` columns.

* Corrected documentation: the Colliders help page said `ggdag_collider()` plots exogenous variables and promised a `collider` column rather than `colliders`, `node_exogenous()` said that exogenous variables are defined given an exposure and outcome, and the Variable Status page named `node_collider()` and `ggdag_collider()` instead of `node_status()` and `ggdag_status()`.

* `node_dconnected()`, `node_dseparated()`, and `node_drelationship()` now label each node of `from` and `to` with its own d-relationship to the opposite set. Previously they computed the set-level answer from `dagitty::dconnected()` and wrote it onto every endpoint, so a node d-separated from the other set was labeled d-connected whenever any of its companions was. Multi-element endpoints arise without passing vectors, since `from` and `to` fall back to the exposures and outcomes set on the DAG.

* `node_dseparated()` no longer adds `collider_line` and `adjusted` columns when `controlling_for` is `NULL`. These columns now appear only when adjustment actually occurs, as they do in `node_dconnected()`, `node_drelationship()`, and `control_for()`. Code that maps `shape = adjusted` on unadjusted `node_dseparated()` output needs to set `controlling_for` or drop the mapping.

* The documented `list(c(...))` format for `controlling_for` now works in `node_dconnected()`, `node_dseparated()`, `node_drelationship()`, `is_d_separated()`, and `is_d_connected()`, and the same format is now accepted by `control_for()`'s `var`. The node functions previously reported the listed variables as missing from the DAG, and the `is_*()` functions failed with "the condition has length > 1".

* `node_dconnected()`, `node_dseparated()`, `node_drelationship()`, `is_d_separated()`, and `is_d_connected()` now raise `ggdag_missing_nodes_error` when `from`, `to`, or `controlling_for` names a variable that is not in the DAG, matching the rest of the package. Previously an internal error from dagitty, which carries no ggdag class, surfaced instead.

* `node_dseparated()` and `node_drelationship()` gain the documented `...`, and `node_dconnected()` now forwards it, so arguments such as `layout` and `coords` reach `tidy_dagitty()`. `ggdag_drelationship()`, `ggdag_dseparated()`, and `ggdag_dconnected()` pass their `...` on to the same place.

* `node_parents()`, `node_children()`, `node_ancestors()`, `node_descendants()`, `node_markov_blanket()`, and `node_adjacent()` now handle a `.var` with more than one variable, which the documentation has always allowed. They previously emitted a recycling warning and mislabeled nodes, and the ancestor and descendant sets dropped only the first queried variable.

* `is_collider()` and `is_downstream_collider()` no longer call each other recursively. The recursion was redundant, since ancestry is transitive, but it cost exponentially many dagitty calls, which made `node_collider()`, `ggdag_collider()`, and `control_for()` effectively hang on chain-like DAGs of realistic size.

* `query_paths()` now skips pairs of identical endpoints and collapses repeated ones, rather than asking dagitty for the paths from a node to itself.

* Printing a `dag_paths()` result no longer counts paths that dagitty cannot describe in the header, which previously produced an empty `{}` entry and a count larger than the list of paths shown.

* `dag_paths()`, `query_paths()`, and `edge_backdoor()` now classify a path that is neither causal nor backdoor, such as a path opened by conditioning on a collider, as `"other"`. Previously every non-causal path was labeled `"backdoor"`, even though a backdoor path is one whose first edge points into the exposure. `ggdag_paths()` gains an `"other"` key in its path legend.

* `dag_paths()`, `ggdag_paths()`, and `ggdag_paths_fan()` now pass `directed` to `dagitty::paths()`, so `directed = TRUE` returns and plots only directed causal paths. The argument was previously ignored.

* `dag_paths()` and `edge_backdoor()` now match path edges by edge type as well as by node pair, so a directed and a bidirected edge between the same two nodes are treated as the distinct edges they are, each classified by the path it actually lies on.

* `dag_paths()` no longer emits a duplicate row for the exposure or the outcome when the node is already the source of an edge on the open path. This removes duplicate nodes and labels drawn on top of each other by `ggdag_paths()`.

* `dag_paths()` is now idempotent: applying it to its own output, which happens when a precomputed result is piped into `ggdag_paths()` or `ggdag_paths_fan()`, reproduces the single-application result instead of corrupting the path columns.

* `dag_paths()` now raises its documented `ggdag_missing_error` when neither `from`/`to` nor an exposure and outcome are available, instead of passing empty endpoints to `dagitty::paths()`.

* `dag_paths()` and `edge_backdoor()` now raise a classed error when the exposure or the outcome has more than one element, which `dagitty::paths()` cannot enumerate. `query_paths()` instead enumerates paths for each ordered pair of endpoints, one row per path, rather than recycling endpoints across every path.

* Printing a `dag_paths()` result no longer errors when the DAG has no exposure and outcome set and the endpoints were given as `from` and `to`.

* Updated compatibility with ggrepel >= 0.9.7. `geom_dag_label_repel2()` now uses `linewidth = 0` (instead of `label.size = NA`) to hide label borders, matching ggrepel's new `linewidth` aesthetic. The `verbose` parameter in repel functions now defaults to `getOption("verbose", default = FALSE)`.

* `geom_dag()` gains a `label_geom` parameter that allows users to specify which geom function to use for labels when `use_labels = TRUE` (#133). The default remains `geom_dag_label_repel` for backward compatibility, but users can now choose any label/text geom function such as `geom_dag_label`, `geom_dag_text_repel`, `geom_dag_label_repel2`, or `geom_dag_text_repel2`.

* `geom_dag_text_repel()` and `geom_dag_label_repel()` now support all current ggrepel parameters (#172):
  - Added `stat`, `position`, `min.segment.length`, `force_pull`, `max.time`, `max.overlaps`, `xlim`, `ylim`, `direction`, `seed`, and `verbose` parameters
  - `segment.alpha` now defaults to 1 but can be overridden by users
  - All segment.* parameters (e.g., `segment.linetype`, `segment.curvature`, `segment.angle`) now pass through correctly via `...`
  - Parameters like `point.size` and `point.colour` can also be passed through `...`
  - Fixed support for `ggrepel::position_nudge_repel()` - users can now use either `nudge_x`/`nudge_y` parameters or `position = position_nudge_repel()`
  - Both approaches now work with vector inputs for differential nudging of labels

* Added comprehensive set of `is_*()` functions for testing DAG properties:
  - `is_acyclic()`: Test if a DAG is acyclic
  - `is_adjustment_set()`: Test if a set of variables is a valid adjustment set
  - `is_d_separated()`, `is_d_connected()`: Test d-separation relationships
  - `is_exogenous()`: Test if a variable has no parents
  - `is_instrumental()`: Test if a variable is instrumental
  - `is_exposure()`, `is_outcome()`, `is_latent()`: Test variable status
  - `is_parent()`, `is_child()`, `is_ancestor()`, `is_descendant()`: Test node relationships
  - `is_adjacent()`: Test if two nodes are adjacent
* Added `edge_backdoor()` function to classify edges as being on backdoor paths, direct causal paths, or both between exposure and outcome (#137)
* Enhanced `dag_paths()` and `query_paths()` to include `path_type` column that classifies paths as "backdoor" or "direct" (#137)
* Changed default aesthetic in `ggdag_paths()` from coloring by path status to coloring by path type (direct vs backdoor), providing more informative visualization of causal pathways
* Introduced new `query_*()` API for direct analytical queries on DAGs (#185). These functions return tibbles with results rather than tidy DAG objects for plotting:
  - `query_adjustment_sets()`: Find adjustment sets to close backdoor paths
  - `query_paths()`: Find and analyze paths between nodes
  - `query_instrumental()`: Identify instrumental variables
  - `query_dseparated()`/`query_dconnected()`: Test d-separation relationships
  - `query_colliders()`: Identify collider nodes
  - `query_exogenous()`: Find exogenous variables
  - `query_parents()`, `query_children()`, `query_ancestors()`, `query_descendants()`: Query node relationships
  - `query_markov_blanket()`: Find Markov blankets
* All error messages, warnings, and informational messages now use the cli package for better formatting and user experience. This change adds custom error classes (`ggdag_error`, `ggdag_warning`) that enable programmatic error handling (#191).
* Fixed `tidy_dagitty()` error when processing DAGs with no edges (#159)
* Fixed `dag_paths()` error when no open paths exist between nodes (#180)
* Introduced new functions `query_conditional_independence()`, `test_conditional_independence()`, and `ggdag_conditional_independence()` for detecting, testing, and visualizing implied conditional independencies in a given DAG and dataset (#139)
* Added `sort` parameter to `ggdag_conditional_independence()` with default `TRUE` to sort conditional independence tests by their estimates for improved readability (#170)
* Introduced new functions `aes_dag()` and `geom_dag()` to simplify specification of ggplot code for most DAGs. Also refactored most quick plots to use these functions (#121)
* Added new function `geom_label_repel2()` for more opinionated repelled labels that often look better on DAGs (#132)
* Improved DAG data structure by removing unnecessary `circular` column when not needed. The column is now only included when using circular layouts. This simplifies the tidy DAG structure for most use cases (#119)
* Edge geoms now support mapping `color`/`colour` aesthetics to `edge_color`/`edge_colour` for more intuitive usage, matching ggplot2 conventions (#166)
* Fixed `theme_dag()` facet labels being clipped by adding margin to strip text (#173)
* `scale_adjusted()` now sets an explicit legend `order` on each of its scales, so the adjustment legends appear in the same order in every session. The shape and color scales still merge into a single legend, but overriding one of them now requires passing `guide = guide_legend(order = 1)` as well; without it, the overridden scale gets a legend of its own and the pair splits.
* Nodes with no edges are now preserved throughout the tidying pipeline, including `tidy_dagitty()`, `as_tidy_dagitty()`, layout generation, `dag_saturate()`, and `as_tbl_graph()`.
* DAGs given coordinates for only some of their nodes now generate positions for the remaining nodes, reporting which nodes were missing, instead of producing a DAG with missing coordinates.
* Labels are now validated. They must be a named character vector, and no node may be named more than once. Labels also survive `dag_saturate()`, and the quick plot DAG constructors (such as `m_bias()` and `confounder_triangle()`) ignore zero-length labels instead of failing on them.
* The `direction` column of a data frame passed to `as_tidy_dagitty()` is now validated; only `"->"`, `"<->"`, and `"--"` are supported.
* Added a `rename()` method for `tidy_dagitty` objects.
* Grouping added with `group_by()` is now preserved when the tidy DAG data is rebuilt, rather than being dropped.
* `update_dag()` now errors when given arguments other than the DAG, which it previously ignored. Use `update_dag(x) <- value` to install a different `dagitty` object.
* Node names containing spaces or accented characters are now quoted when the underlying `dagitty` object is compiled, so they round-trip correctly.
* Fixed an error when building a node-only DAG from a data frame whose `to` column is logical, as in `data.frame(name = c("a", "b"), to = NA)`.
* `as_tidy_dagitty()` now errors informatively when a list of time points contains a time point with no nodes.
* `dag()` now accepts a character vector of `dagitty` statements, as its documentation says it does, instead of failing on anything longer than one element.
* A `dagify()` formula that mixes arrow types now keeps each term's arrow. `y ~ x + ~z` gives `x -> y` and `y <-> z`; previously every term on such a formula became bidirected, which silently changed d-separation and adjustment sets. R's parser lets a unary `~` take in the rest of the right-hand side, so every term after the tilde is bidirected.
* `dagify()` now accepts node names that `dagitty` cannot parse unquoted, such as names with accents or spaces, quoting them for `dagitty` rather than failing with a parser error.
* `dagify()` now errors informatively when given anything other than a two-sided formula, such as `~x` or `"y ~ x"`.
* `dagify()` now recognizes `ggdag::curved()` as `curved()`, and a namespace-qualified call anywhere in a formula no longer errors.
* `curve_edge()` and `set_curve_edges()` now error when asked to curve an edge the DAG does not have, rather than silently doing nothing. Either orientation names the same bidirected edge.
* `coords2df()` now reads the names of the coordinate list instead of assuming `x` comes before `y`, and errors when the list is not named `x` and `y`.
* The `"dendrogram"` layout is now rejected alongside the `"dendogram"` misspelling. It positions a node once per branch, which duplicated every node reachable by more than one path.
* Errors from `curved()` and from a non-numeric `edge_curvature` column now carry the `ggdag_error` classes, like the rest of the package's errors.
* dplyr (>= 1.1.0) is now required.
* Added quick plot functions for the causal quartet: `quartet_collider()`, `quartet_confounder()`, `quartet_mediator()`, `quartet_m_bias()`, and `quartet_time_collider()`, along with their `ggdag_*` counterparts. These functions create DAGs representing the causal quartet from D'Agostino McGowan, Gerke, and Barrett (2023), demonstrating that statistical properties alone cannot determine causal relationships (#171)

# ggdag 0.2.11

* Internal update to address upcoming changes in ggplot2 (#125, thanks @teunbrand)
* Implemented automatic time-ordered coordinates for `time_ordered_coords()` and `layout` (#115)
* Added `geom_dag_label()` to complement `geom_dag_text()` and the repel functions (#116)
* Added `pull_dag()` and `pull_dag_data()` to extract DAG components, `update_dag()` and `update_dag_data()` to update DAG components, and `as_tidy_dagitty()` to construct DAGs from data frames (#117). Thanks to @lorenzoFabbri for inspiring some of the data-to-dag code in #90.

# ggdag 0.2.10
* Fixed bug where ggdag was using wrong `left_join()` interface with dplyr 1.1.0 (#110)

# ggdag 0.2.9
* Fixed bugs in instrumental functions for complex and non-existent IVs (#99)
* Updated URLs to new r-causal GitHub repository (#94)

# ggdag 0.2.8
* Added new function `time_ordered_coords()` for calculating time-ordered coordinates (#88)
* Addressed changes requested by CRAN, other changes in tidyverse (#87, #86, #85)

# ggdag 0.2.7
* Fixed various bugs in calculating and visualizing paths (#75)
* Fixed breaking changes from the upcoming release of ggplot2 3.4.0 (#79)
* `size` is now deprecated for `geom_dag_collider_edges()` in favor of `linewidth`, related to #79.

# ggdag 0.2.6
* Fixed CRAN errors

# ggdag 0.2.5
* Fixed issue with `dag_adjustment_sets()` when `width` option was low by extracting adjustment sets directly (#70)
* Fixed issue with `tidy_dagitty()` that missed nodes with no edges attached. May cause minor breaking changes in randomized layouts. (#68)
* Added the `limit` argument to `dag_paths`, `ggdag_paths`, and `ggdag_paths_fan` (see `dagitty::paths`) (#65).

# ggdag 0.2.4
* `tidy_dagitty()` no longer allows the dendogram layout type (#62)
* `scale_adjusted()` now correctly aligns legend types (#61)
* ggdag no longer loads ggplot2 via `Depends`, instead importing it internally (#57).

# ggdag 0.2.3
* Update vdiffr to be explicitly conditional (#51)
* Fixed bug in `activate_collider_paths()` that causes an error where a collider had a high number of ancestors (#49)
* Fixed bug in `node_equivalent_dags()` where extra columns did not get joined to new tidy DAG (#40)

# ggdag 0.2.2
* Added vdiffr tests, as well as a basic test file for every `.R` file. (#27)
* Updated roxygen2 version (#29)
* Fixed bugs in `dag_paths()` and `geom_dag_edges_fan()` (#31)
* Removed use of `use_defaults()` `ggproto` method in `GeomDAGEdge` (#33)
* Use `ggplot2::expansion()` instead of `expand_scale()` when ggplot2 version is at least `3.3.0` and removed local version of `expand_scale()` (#34)

# ggdag 0.2.1
* Fixed bug in `ggdag_collider()` where the aesthetics were flipped for the colors but not the labels (issue #15, b72e34b)
* Used `as.character.default()` directly for working with characters to avoid dispatch error when the formula.tools package is attached (issue #17, 0f32bb4)

# ggdag 0.2.0
* Fixed compatibility issue with ggraph 2.0.0 by changing to `strength` parameter in curved geoms
* Fixed join bug in `node_equivalent_class()` that didn't account for the way dagitty returns DAGs with no direction
* Fixed join bug in `node_equivalent_class()` that didn't check `to` node
* Implemented `is_false()` to avoid dependency on R 3.5.0
* improved edge lengths
* add `{}` to adjustment set names to reflect convention
* Set nodes to be unstyled by default
* Changed default themes and scales to be more like base ggplot2
* Added a `NEWS.md` file to track changes to the package.


# ggdag 0.1.0
* Initial release

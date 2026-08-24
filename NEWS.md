# ggdag (development version)

* `geom_dag_arrow()`, `geom_dag_arrow_arc()`, and `geom_dag_arrows()` now treat `resect = 0` as a resection of zero rather than as a request for the automatic one. The value `0` was the marker for "nothing was set", so an explicit `resect = 0` was replaced by the 8mm `ggdag.edge_cap` fallback, while `resect = 0L` escaped the test and drew an arrow that was not shortened at all, giving two spellings of the same number two different plots. The unset marker is now `NULL`, which no user value collides with.

* Documented that auto-resection is decided one end at a time. The help page said that resection applies only when neither `resect` nor `resect_head`/`resect_fins` is set, while `geom_dag_arrow(resect_head = 4)` shortened the fins end by the automatic amount all the same. The implementation was already per end and is what the page now describes.

* `geom_dag(edge_engine = "ggarrow")` now draws its edges at the documented `edge_width`, scales them with `size`, and honors `arrow_length`. The ggarrow branch read only the edge cap out of the size vector, so edges were drawn at ggarrow's own defaults, and the `ggdag.edge_width` and `ggdag.arrow_length` options had no effect on them. The arrow length travels as a `grid::unit()` in points, since ggarrow reads a bare number as a multiple of the shaft width.

* `curved()` in a `dagify()` formula now reaches a bidirected edge. dagitty stores a bidirected edge in the order it was written, which is the reverse of the order `curved()` records, so the curvature was matched against nothing and filled in as zero. `curve_edge()` and `set_curve_edges()` accept either orientation of a bidirected edge for the same reason. Curvature is measured relative to the direction the edge is drawn in, so a match found the other way round has its sign flipped and the arc keeps the side of the page it was asked for.

* A bidirected edge under the ggarrow engine now keeps the arc its edge layer draws it with when some other edge is curved. Every edge row was filled with a curvature of zero as soon as any edge carried one, and a value in the data overrides the layer's own curvature, so curving one directed edge silently straightened every bidirected edge on the plot.

* Per-edge curvature is now documented as a feature of the ggarrow edge engine, and asking for it under the default ggraph engine raises a `ggdag_edge_curvature_warning` rather than passing without a word. The sign convention on the help pages for `curved()`, `curve_edge()`, and `set_curve_edges()` described how the ggraph engine renders positive curvature, which no ggraph stat or geom in the package has ever read.

* Edge caps now follow the node layer whichever order the layers were added in. The node size was read from the layers already on the plot when the edge layer was added, so the edges-before-nodes order that every layer-by-layer example uses left the caps at their 8mm default and ran arrowheads under the nodes. Caps that no node layer answered for at that point are settled when the plot is built, where the whole layer list is in view. Repelled labels find the plot's edge layers the same way, so a label geom added before its edges now repels from them.

* An edge layer inside the list `geom_dag()` returns now goes through the same cap injection as one added on its own. The list was walked as a plain list of layers, which stepped over the wrapper that carries the injection.

* A DAG edge or arrow layer stored in a variable and added to more than one plot no longer carries the first plot's caps and resections to the next. A layer is an environment, so what it learned from one plot was written into the object the caller was holding.

* Repelled labels now avoid drawn diagonal and fan edges as they already avoided arcs, and they follow the drawn curve through a transforming position scale such as `scale_x_log10()`. The obstacles that guide the repulsion were placed along the straight chord between the two nodes for those geometries, and the edges were matched to the plot data by untransformed coordinates, which no longer matched once a scale had moved them. Obstacles are now placed on the corners of the path each edge is drawn as.

* `ggdag_paths()`, `ggdag_adjustment_set()`, and `ggdag_equivalent_class()` now draw per-edge curvature under the ggarrow engine. These functions build their own edge layers so that they can color or fade edges by an analysis column, and those layers were given neither the `edge_curvature` aesthetic nor a geom able to bend an edge.

* `ggdag_adjust()` gains an `edge_engine` argument and draws ggarrow edges when asked for them, and `ggdag_equivalent_dags()` gains the same argument. Passing `edge_engine` to either was an error, and `ggdag_adjust()` drew ggraph edges whatever the `ggdag.edge_engine` option said.

* Legend key glyphs now follow the engine the plot was built with rather than the `ggdag.edge_engine` option in force when the legend is drawn. A plot given `edge_engine = "ggarrow"` as an argument drew ggarrow edges beside a key showing a grid arrow.

* `ggdag()` now honors the `ggdag.edge_type` option. It always passed an edge type on to `geom_dag()`, which reads the option only when the argument is missing, so `options(ggdag.edge_type = "arc")` changed nothing.

* The time-ordered layout no longer draws a directed edge backwards in time when its two nodes are also joined by a bidirected edge. Every group of nodes joined by bidirected edges is now condensed into a single node before the layers are assigned, so the shared layer a bidirected edge asks for comes out of the layering itself rather than from moving nodes onto one layer afterwards. Raising a group to the layer of its latest member used to leave a child of that group on its own parent's layer, and a pair joined by both a directed and a bidirected edge, such as `dagify(y ~ x + m, m ~ x, x ~~ y)`, placed the exposure after its own mediator. Where the directed edges order two members of a group no shared layer exists, and the directed order now decides.

* The time-ordered layout honors a `fixed_time` pin on a node that bidirected edges join to others. The group was moved to the layer of its latest member, which overrode the pin with nothing said, even when the whole group could have sat at the pinned layer.

* `sort_direction = "right"`, the default for the automatic time-ordered layout, now places every node one layer before its earliest child, as documented. The backward pass read the layer each node started at rather than the layer its children ended at, so a node whose child moved later was left behind, sometimes several time points before its only child.

* `time_ordered_coords(fixed_time = )` now raises `ggdag_dag_error` for a pin no ordering can satisfy, such as a time earlier than the node's own ancestors allow. Such a pin gave those ancestors negative internal layers, which dropped them from the layout with nothing said; `dagify()` then stored no coordinates for them and plotting failed inside dagitty with `ReferenceError: NA is not defined`. `tidy_dagitty()` now passes these errors on rather than falling back to another layout.

* `time_ordered_coords(fixed_time = )` now raises an error for a time that is not a whole number. A value such as `3.7` was truncated to 3, contradicting the documented promise that a pinned time comes back unchanged.

* The time-ordered layout no longer shifts an outcome that shares a layer with the exposure onto the layer of a pinned descendant, which drew a cause and its effect at the same time point. That shift is now skipped with the message already used when the outcome itself is pinned. The shift also carries along any node a bidirected edge ties to the outcome, so such a pair stays on one layer.

* `time_ordered_coords()` now raises `ggdag_missing_error` when the time column of a data frame holds a missing value, naming the variables it affects. Those rows were dropped, so the nodes left the coordinates with nothing said.

* `time_ordered_coords()` now raises `ggdag_type_error` for a data frame whose time column is not numeric. Text or factor time labels were used as coordinates directly, which gave the result a non-numeric `x` column and ordered the variables alphabetically rather than by time.

* `time_ordered_coords()` now raises an error when `time_points` is supplied alongside a data frame, whose second column already carries the time points. The argument was discarded with nothing said.

* Corrected documentation: the `time_points` argument of `time_ordered_coords()` defaults to a sequence from 1 to the number of time periods, not to the number of variables.

* `dag_prune()` now keeps a node whose every edge is pruned, as an isolated node. It protected such a node only when the node had exactly one edge to begin with and a row of its own to be converted, so pruning both edges of a node at once removed the node itself, batch pruning disagreed with pruning the same edges one at a time, and a node that only ever ends an edge was removed along with the edge.

* `dag_prune()` no longer sets every edge direction to `NA` when the `direction` column holds text rather than a factor, which is the shape `as_tidy_dagitty()` produces for a data frame that supplies its own coordinates. It rebuilt the column from factor codes, so the corrupted directions were then compiled into the DAG. Reordering the levels of a factor `direction` column had the same effect.

* `dag_prune()` now raises `ggdag_missing_edges_error` when `edges` names an edge that the DAG does not contain, including an edge written in the reverse direction, and reports which pairs are missing, each of them once. Such a call previously returned the DAG unchanged with nothing said. A bidirected or undirected edge carries no direction of its own, so either orientation of its endpoints names it for pruning. Unnamed and partially named `edges` vectors now raise the package's own `ggdag_type_error` rather than a bare `stopifnot()` message, and a partially named vector is no longer accepted with its unnamed elements quietly ignored. An `edges` vector that is not character, or that holds a missing value or a missing name, raises the same error; a missing value names no node.

* `dag_saturate()` now carries the bidirected edges of its input through to the saturated DAG. It assigned time order from every edge, and a bidirected edge holds its two nodes in the same layer, so the edge was dropped and the saturated model asserted an independence the input denies. A pair joined by both a directed and a bidirected edge previously saturated to a DAG with no edges at all.

* `dag_saturate()` now carries adjusted nodes over to the saturated DAG, as it already did for exposures, outcomes, latent variables, and labels.

* `dag_saturate(use_existing_coords = TRUE)` now treats stored coordinates that are entirely missing as no coordinates at all and computes the layout, as `use_existing_coords = FALSE` does. `dagitty::coordinates()` reports that shape for any DAG whose coordinates have never been set, and it reached the layout engine as an unresolved layout name, which failed with an error from ggraph.

* `update_dag()`, and with it every function that rebuilds a DAG from its own data, now carries the labels of the input over to the rebuilt `dagitty` object. `dagitty::coordinates<-` rebuilds the object and strips custom attributes, so `label()` came back empty afterwards even though the `label` column of the data survived. `dag_prune()` on a labeled DAG is the case most easily met.

* `node_equivalent_dags()` is now idempotent: applying it to its own output returns the single-application result. The `dag` column of the earlier application was joined back alongside the new one, which left the plotting functions with no `dag` column to facet by.

* `node_equivalent_class()` no longer marks a bidirected edge reversable. Both endpoints of a bidirected edge match the undirected edge of the equivalence class, so a pair holding both a directed and a bidirected edge had both of them drawn undirected; only the directed edge is the one the class leaves free to reverse.

* `ggdag_equivalent_class()` no longer empties a mapped colour or fill legend. It passed an undefined `breaks` argument to its colour and fill scales, which R resolved to the package's internal `breaks()` function, so the trained scale reported no breaks at all and any colour or fill aesthetic added to the plot lost its legend.

* `node_equivalent_dags()` no longer multiplies rows when its input carries columns beyond the standard ones, such as `label` or `status`. Those columns were joined back on node name from edge-level rows, so a node with several edges gained a row per edge in every equivalent DAG, which drew each of its edges more than once, doubled the reported edge counts, and mixed values across the edges of a node.

* `node_equivalent_class()` and `ggdag_equivalent_class()` now work on DAGs with no edges. dagitty describes such a graph with a data frame that has no columns, which the filter for undirected edges could not read.

* `node_equivalent_dags()` now identifies each equivalent DAG with an integer, so `ggdag_equivalent_dags()` orders its facets 1, 2, 3, and so on. They were sorted as text, so a DAG with ten or more equivalent DAGs drew its panels in the order 1, 10, 11, ..., 2.

* `node_equivalent_class()` now matches the edges of the equivalence class on their endpoints rather than on a key built by pasting the two node names together with an underscore. An underscore is legal in a node name, so two different edges could produce the same key, which tagged a compelled edge as reversible and drew it undirected. `node_equivalent_class()` is also idempotent now: applying it to its own output returns the single-application result.

* `node_equivalent_dags()` and `node_equivalent_class()` now default to `"nicely"`, the layout the rest of the package defaults to, rather than `"auto"`, so an equivalence plot arranges a DAG the way every other function does.

* `ggdag_canonical()` now defaults `label_col` to `"black"`, as every other plotting function in the package does. It defaulted to `text_col`, so labels were drawn in white.

* `ggdag_canonical()` gains the standard `size`, `edge_width`, `edge_cap`, `arrow_length`, `unified_legend`, and `key_glyph` arguments, and now forwards `text`, `label`, `node`, and `stylized` to `ggdag()`. Passing `size` previously reached the layout algorithm through `...` and raised an error from it, and the four forwarding arguments had no effect.

* `node_canonical()` now carries the labels of the input DAG over to the canonical graph, so `ggdag_canonical(dag, use_labels = TRUE)` labels the nodes that survive. The latent variables that replace bidirected edges have no label of their own.

* Corrected documentation: the Equivalent DAGs and Classes help page said that the functions return a set of complete partially directed acyclic graphs. `node_equivalent_dags()` returns the DAGs of the Markov equivalence class, and `node_equivalent_class()` works from the single such graph that represents that class. The `pull_dag()` example renamed only the `name` column, which rebuilds a DAG holding a mix of the old and the new names.

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

* `geom_dag_edges_fan()` now fans only the edges that join the same pair of nodes. The two node columns it hands to ggraph were numbered separately, so the same node carried a different number in each, and two edges with no node in common could be given the same pair identifier and drawn curved apart. Whether a DAG came out straight depended on the alphabetical spelling of its node names. Genuine parallel edges, such as the repeated path edges of `ggdag_paths_fan()`, now also fan symmetrically instead of being spread as though they belonged to a larger group.

* `geom_dag_edges_link()`, `geom_dag_edges_arc()`, `geom_dag_edges_diagonal()`, and `geom_dag_edges_fan()` now draw an empty layer for a DAG with no edges, such as a single-node DAG or one filtered down to isolated nodes. Each returned `NULL` once the edge filter left no rows, which surfaced either as a report that every required aesthetic was missing or as a replacement-length error from the helper that fills in the `circular` column.

* `geom_dag_text_repel()` and `geom_dag_label_repel()` now honor `segment.colour`, the British spelling ggrepel itself accepts. Because `segment.color` carries a default in these wrappers, the value passed under the other spelling could never be reached, and the segment kept its default color with nothing said. Passing `segment.color` still wins when both are given.

* `geom_dag_text()` and `geom_dag_label()` now use a `stat` supplied by the caller, as their documented `stat` argument has always implied. The layer was fixed to `StatNodes` and any supplied value was discarded. The default still resolves to `StatNodes`. `geom_dag_label()` no longer takes `check_overlap`, which `ggplot2::geom_label()` does not support and which was accepted and then dropped; supplying it now produces ggplot2's unknown-parameter warning.

* `geom_dag_text()` and `geom_dag_label()` now honor a `label` mapping made at the plot level, for example `ggplot(dag, aes_dag(label = label)) + geom_dag_text()`. They injected `label = name` as a layer mapping whenever their own mapping had none, and a layer mapping overrides the plot mapping, so node names were drawn over the labels the user asked for. Node names remain the default when nothing maps `label`, and a mapping given in the layer still wins. `aes(label = NULL)` in the layer now clears an inherited plot-level `label`, which is standard ggplot2 behavior and leaves the geom reporting `label` as a missing aesthetic; it previously fell back to node names. With nothing mapping `label` at the plot level, `aes(label = NULL)` still gives node names.

* `theme_dag()`, `theme_dag_blank()`, `theme_dag_grid()`, `theme_dag_grey()`, `theme_dag_grey_grid()`, and their `gray` aliases now let `...` override an element the theme itself sets, as their documentation promises. Passing one, such as `theme_dag(axis.text = element_text(size = 5))`, previously failed with R's duplicate-argument error because the theme named those elements alongside `...` in one call.

* `geom_dag(data = )` now reaches the edge layers for every `edge_type` and both edge engines. With the default `"link_arc"`, and with `edge_engine = "ggarrow"`, the edge layers ignored it and drew the whole DAG while the node and text layers used the supplied data, so filtered-out edges reappeared.

* Repelled labels are now pushed away from the whole of a node rather than from a circle much smaller than the one drawn. The `point.size` handed to ggrepel is now the size that its own conversion turns into the radius of the drawn node, so labels are no longer buried under large nodes. Labels sit slightly further from their nodes at every node size.

* Repelled labels now avoid nodes whose label is missing, the shape `dagify(labels = )` produces whenever only some variables are labeled. The repulsion geometry was built from the rows that survive label filtering, so an unlabeled node contributed neither a point size nor a skeleton disc and a label box could come to rest on top of it. With only one node labeled, the skeleton disappeared for that node as well.

* Repelled labels now avoid the curve a bidirected edge is drawn along, and the curve of `geom_dag_edges_arc()`, rather than the straight line between the two nodes. The invisible points that push labels off edges were interpolated along the chord, which left the drawn arc unprotected while pushing labels off empty space. Edges drawn by `geom_dag_edges_diagonal()`, `geom_dag_edges_fan()`, and the ggarrow engine are still traced along the chord.

* `geom_dag_text_repel()` and `geom_dag_label_repel()` now accept a `Stat` ggproto object for `stat`, as ggplot2's convention allows. Comparing the argument with `==` raised a low-level error about comparison of non-atomic types, which also left `stat = ggplot2::StatIdentity` as the only route to plain identity behavior unusable.

* `StatNodesRepel` now declares `xend` and `yend` as optional aesthetics. Mapping them in the layer, which is what `aes_dag()` does, warned that they were unknown and being ignored even though the stat uses both for edge-aware repulsion.

* `ggdag.debug_repel_points` is now part of the options API: `ggdag_options_set(debug_repel_points = TRUE)` sets it, `ggdag_options_reset()` clears it, and it is documented. The repel geoms read it to add a layer showing the invisible geometry that labels are repelled from, but the option was absent from `ggdag_defaults`, so the documented interface rejected it and only the raw `options()` name worked.

* Corrected documentation: the aesthetics section for `geom_dag_node()` and `geom_dag_point()` listed `filter`, which neither the geoms nor `StatNodes` support; the `n_node_points` argument of the repelling label geoms and of `geom_dag()` is a target count for a filled disc of a center point and four rings rather than a count of points around each node's perimeter, and every value from 1 to 16 produces the same 25 points; and the repel help page now records that the skeleton disc is measured in data units, so it matches the drawn node only on a panel about 180 mm wide.

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

# ggdag (development version)

* A DAG drawn on a single line no longer collapses under `coord_fixed()`. When every node shares one coordinate, that axis trains to a zero-width range, and a multiplicative expansion of a zero-width range adds nothing, so the panel was left only millimetres tall and the node discs were clipped flat. A degenerate axis now takes an additive expansion of an eighth of the other axis's span on each side. `ggplot()` on a tidy DAG, `ggdag()`, and the quick plotting functions expand this way. Plots whose axes both hold a range are unchanged, and so is a plot of a single node, where there is no span to borrow from.

* New `geom_dag_routed_arrows()` draws directed edges that detour around the nodes on their paths, and the new global option `edge_route` (default `"straight"`) swaps that layer into the packaged ggarrow edge rendering. Set `ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")` to have `geom_dag()`, `ggdag()`, and the quick plotting functions route their directed edges. Routing happens when the plot is drawn, in the millimetres of the device, so a detour clears the drawn node discs whatever the shape of the panel and the picture re-routes when the plot is resized. Unblocked edges stay straight, bidirected edges are drawn as arcs, and curvature you set through `curved()` or `curve_edge()` is never rerouted, though it still counts as an obstacle. Curving one edge no longer records a curvature of `0` on the others: an edge you have not curved keeps an unset (`NA`) curvature, which the arc geoms draw as a chord exactly as before and the router is free to detour, while an explicit `0` still pins an edge straight through whatever sits on it. Automatic labels keep clear of the routed paths: `geom_dag_label_auto()` and `geom_dag_text_auto()` call the same router on the same nodes, panel, and settings when the plot is drawn, so a label is never set down on a detour. They read the routed layer's arrowhead resection from the finished plot, so a label layer added before the routed layer, or before the node layer the resection is derived from, still traces the edges the reader sees. An edge handed to `geom_dag_routed_arrows()` through its own `data` argument, and not among the plot's own rows, is not one of the edges the label geoms trace, so a label may still be placed across it. The routing is deterministic: the same DAG at the same size always draws the same paths, and two edges with an equal claim on a route are ordered by the positions of their endpoints rather than by their names. A routed layer follows the axis the layout ordered time along: `time_ordered_coords()` records that axis on the coordinates it returns, `dagify()` and `tidy_dagitty()` carry the record on the DAG, and a routed edge layer takes its layer axis from it, so a layout built with `direction = "y"` is routed down the panel. Naming `layer_axis` in `geom_dag_routed_arrows()` still overrides it. `edge_route = "orthogonal"` (and `geom_dag_routed_arrows(route = "orthogonal")`) instead draws every directed edge as axis-aligned runs with rounded corners: an edge leaves its node through a port, crosses each gap between layers along an assigned vertical slot, and a long edge between the outermost nodes of their layers runs along a channel beyond the layers it passes. Every drawn segment belongs to one edge unless two edges share a port: channels are never shared, an arrival and a departure on one side of a node use distinct ports, the arrivals on a node's side take rows of their own beside its center line so that every arrowhead is drawn separately, a channel that would cut a node gives way to a run through the free space of the layers it crosses, and every candidate route is priced with its bends, so a two-bend channel beats a four-bend run of similar length. A long edge whose own line is clear now takes it, moving the channels placed before it aside by one separation when that is cheaper than a detour. A channel run keeps clear of the arrowhead at the end of a north-south stub, whether that run goes east-west or north-south: a run that would be drawn across such a head is moved past the head and past the same margin the router keeps behind every other head base, and is stacked again beyond any channel it crowds on the way. A chord tilted by no more than the corner radius is drawn straight rather than with a jog too short to show its corners, and such a nearly level edge now runs exactly horizontal, leaving its node at the height of its target rather than as a slightly tilted line between the two centers. A gap between layers too narrow for its slots at the nominal stub and separation gives those up rung by rung (a shorter stub, a tighter spacing down to the new `edge_sep_min` argument of `geom_dag_routed_arrows()`, a smaller corner radius) before it spreads the slots between the layers without clearance, so the picture holds at every plot size. In a gap too narrow for any stub, the slots are spread over the band from the source's clearance to a straight run before the target's layer that holds the cap, the head, and half an edge separation behind the head's base: centered in that band when they fit, and otherwise anchored at the target's end and overflowing toward the source, no further than the source's own layer line, so an arrowhead out of such a gap is drawn on a straight run rather than on a corner and no other edge's run is drawn across the base of it. A gap too narrow to hold its slots and that run together gives up the source's side first, then the margin behind the head, and the head's own run only after that, and a gap crossed in both directions keeps its centered slots. Once the run before the target is a whole cap and head, the arrivals out of such a gap take rows on their target's side like arrivals out of a wider gap, so the arrowheads at a shared target are drawn apart rather than on one point. The rows on a node's side are centered on the node when no arrival is level with it, so a pair of arrivals straddles the center line rather than sitting on it and one row above, and a stack keeps its rows only while they are at least half an edge separation apart, or the minimum slot separation apart where that is larger, merging onto one row otherwise. A node too small for rows of its own takes its arrivals on its center line, and below a node size of about 2.6 mm an arrival is no longer drawn on the far side of the center from its source. A node's center row has a single owner: a level chord drawn on its target's line keeps that line to itself, and a spanning edge into the same node takes a row beside it rather than the line where two arrowheads were drawn on one point. Parallel edges between one pair of nodes are spread in the order of their endpoints' positions rather than their names. Every arrowhead is drawn straight along the run it arrives on: a path into a row beside the center line ends on that row at the node's own coordinate instead of at the center, so the head is never angled toward the center, and each end is resected by the arc length its own port needs, so every tip sits the same distance past the node disc whatever its row. An arrival whose source lies within the corner radius of the row it is assigned is drawn as the horizontal run on that row, leaving its source at the height of the row, as a chord level with the center is drawn on the center line. `edge_route` applies to `edge_type = "link_arc"` and `"link"` under the ggarrow engine; the ggraph engine cannot route and says so.

* The spline router keeps detours clear of the other edges' arrowheads and reads better in dense scenes. The last 8 mm of every other edge's drawn ink is an obstacle for a detour, so a skip edge no longer passes through the arrowhead of an edge into a node it bows around, and an edge that shares its target with others arrives at least 26 degrees away from them, so the arrowheads at a shared target are drawn apart rather than on top of one another. The arrival is aimed once and then drawn: the feedback loop that corrects the sampled curve now drives it to the direction chosen for it rather than working out a fresh target from whichever neighbor is nearest on the pass, so the curve reaches the slot it was given instead of oscillating between two neighbors, and the pass that separates the arrival best is the one drawn. Where the picture leaves no room, an arrival crowded to within 2.5 mm of drawn tip everywhere inside the usual 40 degree tangent clamp may be aimed as far as 60 degrees off the chord, and turned by a tangent as steep as 85 degrees to get there, which is what separates the two arrowheads in a scene such as three edges converging on one node from one side. A hook drawn that way is bounded against the arrival it replaces rather than promised clear of every arrowhead: a bearing from the wider window is drawn only while it reaches no further into another edge's head zone than the bearing inside the ordinary tangent clamp would, and a pass that turns the arrival past that clamp is drawn only while it reaches no further in than the bearing that pass started from. Setting `tangent_clamp` through `edge_route_options()` bounds all three angles, so a tight clamp keeps every tangent tight and gives up the wider arrivals it would have earned. The price of crossing other edges now saturates, so a long edge through a tangle is drawn as a shallow curve through it rather than as a deep arch around it, and a gap between two nodes that is too narrow for a slot at the full clearance but wide enough at the soft margin is threaded rather than bowed around. A candidate that cannot be verified opens the pool to the remaining candidates, and a bow that would have to be pressed against the panel border never replaces a route drawn inside it. The prices of an arrowhead in the way, of a tight slot, and of a crossing past the first are all reachable through `edge_route_options()`.

* `max_bow` now caps the whole spline router rather than only its free bows. An edge that spans layers is routed through a free slot in each layer it crosses, and that tier used to ignore the option, so a DAG could still be drawn with a deep arch over a stack of nodes after the cap had been tightened. A route drawn deeper than the cap now gives way to any route drawn inside it. The gate that admits a route reads the cap off the curve the router draws rather than off the slots it passes through, since a curve interpolated through a chain of points bulges past them; the ranking that decides which route is tried first reads the chain of slots instead, which errs in the safe direction, since a candidate already over the cap at its chain is over it once drawn, so the ranking can only reorder the trials and never admit a route the gate would turn away. Where neither a slot nor a free bow meets the cap, the shallowest route the router could verify is drawn: the cap is a preference rather than a bound, and no edge is straightened through a node to honor it. Leaving `max_bow` unset draws what it drew before, since only a value you write reaches this tier, and writing `0.22`, the number a free bow is capped at by default, does reach it. One trade is worth knowing: a tightened cap can replace a route drawn at the full `clearance` with a shallower bow drawn at the soft margin, so the replacement passes a node with only the soft margin's daylight where the deeper route kept the whole `clearance`.

* A straight edge no longer runs through another edge's arrowhead. In spline mode an edge whose chord clears every node but passes within 2.8 mm of the center of another edge's drawn arrowhead (at the default node size) is nudged past it, on the side away from that arrowhead's target, exactly as an edge that grazes a node is: the chord is moved so that it passes 2.8 mm from the center of that arrowhead, so the edge still reads as straight while the arrowhead it crossed is drawn whole. Arrowheads into the edge's own source or target are left to the arrival rules, and an edge that detours around a node is unchanged.

* New `edge_route_options()` reaches the constants the edge router draws with. Pass it to `geom_dag()`, `ggdag()`, or `geom_dag_routed_arrows()`, or set it once with `ggdag_options_set(edge_route_options = ...)`, to change how deep a spline bows, how much daylight a detour keeps beyond a node, how far apart two edges sharing a slot are drawn, whether orthogonal corners are rounded and at what radius, and what the router charges for a crossing, a bend, a crowded arrival, or an arrowhead in the way. A field you leave unset is derived from the drawn node size when the plot is drawn, so one object holds at every plot size, and the automatic label geoms read the same object, so labels keep clear of the paths the options produce. A numeric field must be finite: an infinite value is rejected where it is written. An `edge_sep_min` above a separation the router derives for itself is reduced to it rather than widening the slots it is meant to floor, while an `edge_sep_min` above an `edge_sep` you set as well is an error, since the two together ask for something the router cannot do. An error about any of these values names the function you called, `geom_dag()`, `ggdag()`, or `geom_dag_routed_arrows()`, rather than an internal one.

* New `geom_dag_label_auto()` and `geom_dag_text_auto()` place node labels deterministically. Each label is measured at draw time, on the device the plot is drawn on, and set down so that label boxes avoid node discs, drawn edges, arrowheads, one another, and the panel edge, preferring positions close to the node in a fixed order of anchors. No simulation and no random numbers are involved, so the same plot always places its labels the same way, unlike the repel geoms. Edges are traced along the paths the plot draws, including per-edge curvature under the ggarrow engine, and a label pushed away from its node gets a leader line back to it. Use them anywhere a label geom goes, such as `ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto)`.

* The automatic label placement keeps each label beside its own node. The engine now scores every candidate position by its distance from the label's node and by how far it presses into a comfort zone around the other nodes' discs, and a position that would spill past the panel edge can slide the short way back inside instead of losing its spot. Labels that used to drift toward a far corner of the panel, or crowd a neighboring node, now settle on the nearest clear side of their own node. A label now sits within one and a half node radii of its node whenever a clear spot exists there, stays half a node radius inside the panel, is drawn with a leader line only when no clear spot within `min.segment.length` exists, and the labels the engine could not clear are named in the `unresolved` field of the drawn `dag_labels_auto` grob. A label is also kept from reading as another node's: a box that sits nearer a different node's disc than its own pays for the difference, and a box that would be drawn without a leader beside the wrong node gives way to a spot beside its own node even when that spot needs a leader. A label with no clear spot within reach of its node searches a fine grid around the node before it goes farther, and a leader is priced by its length and by every edge it crosses, so a longer leader over open space is preferred to a shorter one across the ink.

* The automatic label geoms say when they run out of room, and can be told what to do about it. A draw that leaves any label on the ink now warns once, naming the labels, instead of leaving the `unresolved` field of the drawn grob as the only sign. The warning speaks for every draw that leaves a label on the drawing, so a plot redrawn on a smaller device warns about the picture in front of the reader, while a draw that has already warned about what it drew stays quiet on a redraw of the same picture. `max.overlaps` on `geom_dag_label_auto()` and `geom_dag_text_auto()` takes ggrepel's meaning on the count of things a label's final box still hits: the default, `Inf`, draws every label, and a finite value leaves out the labels over the allowance, so `max.overlaps = 0` draws only the labels the engine placed cleanly. Dropping a label never moves the ones that stay, and when labels are taken out the warning names every label the engine could not place and then which of them were left out. `wrap` on the same geoms, and `label_wrap` on `geom_dag()`, `ggdag()`, every quick plotting function that takes `use_labels`, and the new `ggdag.label_wrap` option, wrap the label text to a width in characters before the boxes are measured, so a long label asks for a smaller box and more often finds a clear spot. A line break written into a label survives the wrap, each side of it wrapped on its own. `wrap` and `min.segment.length` are checked on the geom that takes them, so a bad value is refused where it is written rather than at draw time, and `min.segment.length` accepts a `grid::unit()`, resolved to millimetres on the device it is drawn on; a unit with a missing amount is refused. A label mapping naming a column the data does not hold is an error on either automatic label geom, naming the missing column; the silent no-op is reserved for the `label` mapping `geom_dag(use_labels = TRUE)` generates on a DAG that carries no labels.

* A label with no clear spot near its node now sits on the drawn edges beside it rather than flying across the panel. In a crowded scene the near field of a node can be nothing but discs and edges, and the engine used to send such a label to the nearest clear spot however far away that was, which on a 30-node DAG at 7 x 5 inches meant leaders of 46 to 82 mm crossing the whole picture. A box that covers only the mid-run of edges, never a node disc, an arrowhead, another label, or the panel border, is now admissible in a tier of its own: below every clear spot within seven and a half node radii of the label's disc, so a clear spot a reader would still read as beside the node keeps winning, and above every clear spot beyond that. Among such boxes the one covering the least ink and nearest its node wins, with a leader if it is past `min.segment.length`. A label placed this way is still named by the warning and by the `unresolved` field of the drawn grob, and `max.overlaps = 0` still leaves it out. Placements change only in scenes where, when some label's turn to be placed came, no clear spot within that distance was still free.

* The repel label geoms place labels beside their nodes and reproduce across sessions. `geom_dag_text_repel()` and `geom_dag_label_repel()` now default to `box.padding = 0.5`, `point.padding = 0.5`, `min.segment.length = 1`, `force_pull = 2`, `max.time = 1`, `max.iter = 10000`, and `seed = 1234`, so labels settle close to the nodes they name instead of drifting across the panel, and the same plot draws its labels the same way every time. The more-spaced `geom_dag_label_repel2()` and `geom_dag_text_repel2()` variants now default to `box.padding = 0.75`. `geom_dag(use_labels = TRUE)` threads the same box padding and no longer overrides the label padding of the boxed geoms. Any earlier value can be restored through the constructor arguments, for example `geom_dag_label_repel(box.padding = 1.25, seed = NA)`.

* Automatic and repelled labels follow the arc of an edge whose curvature is mapped on the plot rather than on the edge layer, so `aes_dag(edge_curvature = ...)` now places labels off the drawn curve instead of off its chord. The geometry of each drawn edge is also described once, so a plot whose panels repeat coordinates, such as `ggdag_equivalent_dags()`, no longer stacks repeated obstacle points along its edges.

* `time_ordered_coords()` optimizes manually supplied time periods. With `.vars` given, it now returns a layout function that keeps every variable at the time period you assigned while ordering and spacing the nodes with the same engine the automatic mode uses, so manual DAGs cross fewer edges and route fewer edges through nodes. Your `time_points` become the axis positions of the tiers, and your arrangement is respected: the evenly spread grid in your listed order is kept unless the optimized layout strictly reduces edge crossings, or matches them and routes fewer edges through nodes. An edge that contradicts the given time periods is still drawn, but a warning names it and it takes no part in the optimization. The optimized default is also stricter than the spread coordinates were: a variable in the DAG that the time periods do not cover is an error when the layout is computed, and a variable listed in more than one time period is an error when the coordinates are created. Set `optimize = FALSE` for the evenly spread coordinate tibble earlier versions returned, which now checks `time_points` and duplicated variables before it lays anything out, so a mismatch is reported in the same words the optimizing layout uses: a `time_points` longer than the list of periods failed with a recycling error from vctrs, and a shorter one silently put every variable at the first time point.

* The time-ordered layout crosses fewer edges. Within each time layer, small layers are now ordered by an exact search over their permutations rather than the barycenter heuristic alone, multi-layer edges count toward the ordering through every layer they pass, and vertical positions start from a median-based placement whenever that draws a better DAG than even spacing. A mirror-symmetric DAG keeps its symmetric figure, though: when the evenly spaced placement is symmetric and clear of every node, it is kept over a placement that unwinds only a crossing or two by breaking the symmetry. Layouts that were already at their best are unchanged in structure, and the members of a bidirected pair sharing a layer are nudged next to each other so their arc stays short.

* The time-ordered layout spaces nodes to the size they are drawn at. The spacing and clearance the layout solves for now scale with the `node_size` option, so larger nodes get room in proportion instead of overlapping the edges threaded between them.

* Time-ordered coordinates use one scale for both axes. The y axis was rescaled by the average within-layer gap, so the same DAG could come out vertically stretched or squashed depending on how many nodes shared a layer, and the clearance the layout had solved for did not survive into the drawn plot. Both axes are now divided by the layer gap, so solved spacing, edge clearance, and the bow of bidirected arcs mean the same thing on screen as in the layout. The arcs of bidirected edges are also traced during the final correction pass, so a node sitting on the drawn curve, invisible to the straight-line check, is moved clear. Under the `arc` and `diagonal` edge types, directed edges that span two or more time points are traced the same way, on the side the edge geoms draw them, so a node that cleared the straight chord no longer sits under the bow of the drawn arc. The layout models every arc at the value of the `curvature` option read when the layout is computed, so a DAG drawn with a shallower or deeper bow is cleared for the bow it is drawn with, not for the default. It also clears each arc on the side and at the depth its drawing engine gives it. ggraph draws a positive arc to the left of travel and ggarrow to the right, so the layout reads the `edge_engine` option when it traces bidirected arcs and the spanning arcs of the `arc` and `diagonal` edge types. A ggraph arc reaches three quarters of the sine of half its strength times pi, and a ggarrow curve its curvature exactly, both as a fraction of the half chord, against the 0.51 the model used to assume at the default: a node the drawn arc would have passed through is moved clear, and a node that was pushed aside for a bow no engine draws stays put.

* `dag_saturate()` no longer draws an edge out of a node that has no edges at all. Such a node says nothing about its time order, but it was placed at the first time point and completed forward, so `dag_saturate()` on a DAG holding `x -> y` and a lone `z` invented `z -> y`. An edge-free node now takes no part in the saturation and is kept as an isolated node, just as bidirected edges are set aside when the time order is read from the directed edges.

* A label naming a variable the DAG does not hold is an error, from `dagify()`, `dag_label()`, and `label<-` alike. Such a name is a typo or a leftover from an edit to the DAG, and it was silently dropped, so a misspelled name left its node unlabeled without a word. Labels carried over when a DAG is rebuilt from filtered data still drop the removed variables' labels quietly.

* Node layers now draw each variable once per panel. The tidy data holds a row per edge, so a node with several edges was drawn once per row, and where an analysis column such as `path` was mapped to a color, an unmarked copy drawn last hid the marked one underneath it. In `ggdag_paths()`, a variable on an open path could be drawn in the shadow color as a result. Two nodes given the same coordinates are still drawn as two. A node layer trains its scales on the rows it draws, so a level of an edge-level aesthetic that appeared only on a dropped duplicate row no longer reaches the legend.

* `dag_prune()` takes a data frame of edges, with `name` and `to` columns and an optional `direction` column, as well as the named character vector it has always taken. A pair of nodes can hold a directed edge and a bidirected edge at once, and endpoints alone name both; `dag_prune()` now errors on such a pair rather than pruning both, and the `direction` column names the one to remove. An `edges` argument that names no edge at all is an error too, in either form, rather than returning the DAG unchanged.

* `is_collider()`, `is_downstream_collider()`, `node_collider()`, `ggdag_collider()`, `activate_collider_paths()`, and `query_colliders()` count the arrowheads pointing into every variable from a single reading of the edges of the DAG. They asked dagitty for the parents and the spouses of one variable at a time, which cost two calls into its JavaScript engine per variable inspected and dominated the time a whole-DAG sweep took. The sweep behind `node_collider()` is around six times faster on a thirty-node DAG, and `query_colliders()` around three times.

* `set_curve_edges()` reads the edges of the DAG once rather than three times while validating the edges it was given, which about halves what it costs on a large set of edges.

* A dplyr verb on a `tidy_dagitty` that already has coordinates no longer computes a layout. Under the `time_ordered` default, every verb laid the DAG out again and threw the result away, which cost the work and could report on it.

* `dagify()` raises its own condition classes for a formula it rejects. Each formula was validated through `purrr::walk()`, so a self-loop arrived wrapped in `purrr_error_indexed` and `tryCatch(ggdag_dag_error = )` did not see it.

* `dagify()` keeps the curvature of an edge whose parent is a name written in backticks, such as `y ~ curved(\`my var\`, 0.5)`. The name was read in its deparsed form, backticks and all, so it matched no node and the curvature was dropped without a word.

* A node name ending in a backslash is rejected with an error from ggdag rather than a parse error from dagitty. dagitty reads the last backslash of a name as escaping the closing quote, so such a name cannot be written down at all.

* `ggdag_paths()` and `ggdag_adjust()` check `edge_type` under the ggarrow edge engine as well. Both build their own edge layers, and only the ggraph branch checked the type it was given.

* The `na.rm` documentation for the edge geoms describes what they do. Every one of them defaults to `na.rm = TRUE`, since a node with no outgoing edge has a missing edge end, while the documentation said that `FALSE` was the default.

* ggdag now requires ggplot2 3.5.0 or later, which is what it has used for some time: `linewidth` arrived in 3.4.0, the key sizes its legend glyphs report are read from 3.5.0 onward, and ggraph asks for 3.5.0 itself.

* `ggdag_markov_blanket()` now draws node text only through `geom_dag()`, like every other quick plotter. It added a text layer of its own before that call, so the text was drawn twice with the defaults, `use_text = FALSE` still showed it, and `text_col`, `text_size`, and the matching options had no say over the extra layer.

* `ggdag_status()`, `ggdag_instrumental()`, `ggdag_equivalent_dags()`, `ggdag_equivalent_class()`, and `ggdag_canonical()` now pass `...` on to `tidy_dagitty()` as documented. Each tidied its input before the dots could reach it, so `layout`, `seed`, and the rest were dropped without a word. `node_equivalent_class()` gains a `...` of its own for the same reason, and `node_canonical()` rejects `use_existing_coords`, which contradicts the layout it does from scratch.

* `ggdag_canonical()` takes the same `use_labels` and `edge_type` defaults as its sibling quick plotters. Its `use_labels` fell back to `NULL` rather than `FALSE`, and its `edge_type` was a bare string rather than the set of choices, so a misspelled type went unchecked.

* `geom_dag()` says what is missing when a plot does not map the DAG aesthetics. Such a plot failed with an empty-subscript error raised while the edge stat was computing; the missing aesthetics are now named, with a pointer to `aes_dag()`. The check runs when the plot is built rather than when the layers are added, so a mapping supplied afterwards, as in `ggplot(dag) + geom_dag() + aes_dag()`, is seen, and each layer asks only for what it draws with, so `geom_dag(use_edges = FALSE)` needs `x` and `y` alone.

* `geom_dag(size = )` now scales node text along with the nodes, edges, and labels. The scaled text size was computed and then passed over in favor of the unscaled one, so text stayed put while everything around it grew.

* `ggdag_options_set()` rejects an unnamed value rather than storing it under the bare `ggdag.` prefix, where nothing reads it again. `ggdag_options_set(20)` set that option and reported success.

* `ggdag_options_set()` accepts `NULL` for an option, which leaves it unset and returns it to the built-in default. Every value went through validation, which rejected `NULL`, so a single option could not be unset, `label_size`'s documented `NULL` default was unreachable, and restoring the previous values with `do.call(ggdag_options_set, old)` failed whenever one of them was unset.

* `ggdag_options_set()` rejects `NA` for every kind of option, with the package's own error. A missing numeric value reached a comparison and raised a bare "missing value where TRUE/FALSE needed" instead; missing logical, character, and layout values passed validation and were stored, where an `NA` for `use_edges` or `use_text` silently dropped the layer it named.

* The ten quick plotters in `quick_plots.R`, such as `ggdag_m_bias()` and `ggdag_quartet_collider()`, now forward the documented `text` and `label` arguments, including the deprecated logical `text = FALSE`. Both were accepted and discarded.

* `ggdag_adjustment_set()`, `ggdag_paths()`, `ggdag_paths_fan()`, `ggdag_adjust()`, and `ggdag_equivalent_class()` now size the edge layers they build for themselves. These functions draw their own edges so that they can color or fade them by an analysis column, and `edge_cap`, `edge_width`, `arrow_length`, and, for `ggdag_paths()` and `ggdag_adjust()`, `edge_type` reached only the `geom_dag()` call that draws no edges. `ggdag_adjust()` also scales its edge cap by `size`, as the ggarrow engine already did. An `edge_type` these functions cannot draw now raises the same error `geom_dag()` raises rather than being passed to a layer builder that does not exist.

* `ggdag()` gains `edge_engine`, `n_edge_points`, and `n_node_points`, which `geom_dag()` has always taken. Passing any of them went to `tidy_dagitty()` through `...` and on to the layout machinery, which either ignored the argument or failed with an unrelated error.

* The ten quick plotters in `quick_plots.R` gain `edge_engine`, `unified_legend`, and `key_glyph`; `ggdag_adjustment_set()` gains `unified_legend` and `key_glyph`, `ggdag_adjust()` gains `unified_legend`, and `ggdag_paths_fan()` gains `edge_engine` and `key_glyph`. These arguments were an error on the wrappers and, in the adjustment set functions, fell through `...` into dagitty.

* `set_curve_edges()` now raises `ggdag_dag_error` for a data frame that names one edge twice, in either orientation for a bidirected edge. An edge takes one curvature, and the second row silently replaced the first.

* The deprecation warning for `stylized` now names `use_stylized` as its replacement rather than `stylized` again.

* `quartet_time_collider()` no longer stores labels for `x0`, `x3`, `y1`, and `z1`, which name none of its six nodes, and no longer positions them. The arguments remain, so existing code still runs, and the help page now says that they are ignored.

* Corrected documentation: `x_y_associated` defaults to `TRUE` in `quartet_collider()`, `quartet_confounder()`, and `quartet_m_bias()`, whose datasets have x and y associated by construction, and to `FALSE` elsewhere. The shared help text claimed `FALSE` throughout. The help pages also now say that paths opened by conditioning on a collider are drawn as dashed ggraph curves whatever `edge_engine` is in use, since they mark an association rather than an edge of the DAG.

* `geom_dag_arrow()`, `geom_dag_arrow_arc()`, and `geom_dag_arrows()` now treat `resect = 0` as a resection of zero rather than as a request for the automatic one. The value `0` was the marker for "nothing was set", so an explicit `resect = 0` was replaced by the 8mm `ggdag.edge_cap` fallback, while `resect = 0L` escaped the test and drew an arrow that was not shortened at all, giving two spellings of the same number two different plots. The unset marker is now `NULL`, which no user value collides with.

* Documented that auto-resection is decided one end at a time. The help page said that resection applies only when neither `resect` nor `resect_head`/`resect_fins` is set, while `geom_dag_arrow(resect_head = 4)` shortened the fins end by the automatic amount all the same. The implementation was already per end and is what the page now describes.

* `geom_dag(edge_engine = "ggarrow")` now draws its edges at the documented `edge_width`, scales them with `size`, and honors `arrow_length`. The ggarrow branch read only the edge cap out of the size vector, so edges were drawn at ggarrow's own defaults, and the `ggdag.edge_width` and `ggdag.arrow_length` options had no effect on them. The arrow length travels as a `grid::unit()` in points, since ggarrow reads a bare number as a multiple of the shaft width.

* `curve_edge()` called twice on one bidirected edge now replaces the first curvature rather than recording a second one behind it. A bidirected edge has no direction of its own, so `curve_edge(dag, "y", "z", 0.5)` followed by `curve_edge(dag, "z", "y", 0.2)` names the same edge twice; the second call matched no record in its own orientation, appended one, and the stale first record was the one that reached the plot.

* `curved()` in a `dagify()` formula now reaches a bidirected edge. dagitty stores a bidirected edge in the order it was written, which is the reverse of the order `curved()` records, so the curvature was matched against nothing and filled in as zero. `curve_edge()` and `set_curve_edges()` accept either orientation of a bidirected edge for the same reason. Curvature is measured relative to the direction the edge is drawn in, so a match found the other way round has its sign flipped and the arc keeps the side of the page it was asked for.

* A bidirected edge under the ggarrow engine now keeps the arc its edge layer draws it with when some other edge is curved. Every edge row was filled with a curvature of zero as soon as any edge carried one, and a value in the data overrides the layer's own curvature, so curving one directed edge silently straightened every bidirected edge on the plot.

* The `curvature` option now reaches the ggraph edge layers. `geom_dag(edge_type = "arc")`, the quick plotting functions that build their own ggraph edge layers, and the bidirected arc of `geom_dag_edges()` all bend by the amount `curvature` resolves to, as the ggarrow engine already did. Arcs under ggraph were fixed at 0.5 and the bidirected arc at 0.3, so both engines now bow an edge by the amount the one option sets, each with its own depth and to its own side, and the default bend of `edge_type = "arc"` is 0.3 rather than 0.5, which draws shallower arcs. `geom_dag_edges_arc()` called directly keeps its own default of 0.5, and `ggdag_options_set(curvature = 0.5)` restores the old bow everywhere.

* Per-edge curvature is now documented as a feature of the ggarrow edge engine, and asking for it under the default ggraph engine raises a `ggdag_edge_curvature_warning` rather than passing without a word, from `ggdag_paths()`, `ggdag_adjustment_set()`, `ggdag_adjust()`, and `ggdag_equivalent_class()` as much as from `geom_dag()`. The sign convention on the help pages for `curved()`, `curve_edge()`, and `set_curve_edges()` described how the ggraph engine renders positive curvature, which no ggraph stat or geom in the package has ever read.

* Edge caps, and the automatic resection of ggarrow arrows, now follow the node layer whichever order the layers were added in. The node size was read from the layers already on the plot when the edge layer was added, so the edges-before-nodes order that every layer-by-layer example uses left the caps at their 8mm default and ran arrowheads under the nodes. Caps and resections that no node layer answered for at that point are settled when the plot is built, where the whole layer list is in view. Repelled labels find the plot's edge layers the same way, so a label geom added before its edges now repels from them.

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

* Repelled labels now avoid the curve a bidirected edge is drawn along, and the curve of `geom_dag_edges_arc()`, rather than the straight line between the two nodes. The invisible points that push labels off edges were interpolated along the chord, which left the drawn arc unprotected while pushing labels off empty space. Edges drawn by the ggarrow engine are still traced along the chord.

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

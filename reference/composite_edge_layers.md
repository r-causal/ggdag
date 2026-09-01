# Edge layers of the composite plotters

Edge layers of the composite plotters

## Edge layers of the composite plotters

The plotters that color or fade edges by an analysis column build their
edge layers themselves, and which layers they build is settled from the
DAG they are called with: a DAG with no bidirected edge is given no
bidirected edge layer. Replacing the data of the returned plot
afterwards, with ggplot2's `%+%`, does not bring a layer back, so a plot
built for one DAG is not a template for another.

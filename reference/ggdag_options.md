# Global DAG Options

Set, get, and reset global default options for DAG appearance. These
options are used as defaults by all
[`geom_dag()`](https://r-causal.github.io/ggdag/reference/geom_dag.md),
`ggdag_*()`, and related functions.

## Usage

``` r
ggdag_defaults

ggdag_options_set(...)

ggdag_options_get(name = NULL)

ggdag_options_reset()

ggdag_option(name, default)

ggdag_option_proportional(name, base_default, override_default)
```

## Format

An object of class `list` of length 21.

## Arguments

- ...:

  Named option values to set. See `ggdag_defaults` for valid names and
  types.

- name:

  Character string. The option name (without the `ggdag.` prefix). If
  `NULL`, returns all currently-set ggdag options.

- default:

  Default value to return if the option is not set.

- base_default:

  The base default for this option (e.g., 8 for edge_cap).

- override_default:

  The override default used by certain functions (e.g., 10 for edge_cap
  in adjustment set functions).

## Value

- `ggdag_options_set()`: Invisibly returns a named list of the previous
  option values.

- `ggdag_options_get()`: The option value, or a named list of all set
  options if `name` is `NULL`.

- `ggdag_options_reset()`: Called for its side effect; returns `NULL`
  invisibly.

- `ggdag_option()`: The option value if set, otherwise `default`.

- `ggdag_option_proportional()`: The scaled option value if set,
  otherwise `override_default`.

## Details

Options are stored in R's global
[`options()`](https://rdrr.io/r/base/options.html) as `ggdag.<name>`.
When an option is `NULL` (the default), each function uses its own
built-in default. Setting a global option overrides the built-in default
for all functions that use it.

Functions that normally use `edge_cap = 10` (e.g.,
[`ggdag_adjustment_set()`](https://r-causal.github.io/ggdag/reference/adjustment_sets.md),
[`ggdag_drelationship()`](https://r-causal.github.io/ggdag/reference/d_relationship.md))
maintain a proportional offset. If you set `ggdag.edge_cap` to a custom
value, these functions scale it by `10/8`.

## Examples

``` r
# Set global options
old <- ggdag_options_set(node_size = 20, text_size = 5)

# Check current value
ggdag_options_get("node_size")
#> [1] 20

# Reset to defaults
ggdag_options_reset()
```

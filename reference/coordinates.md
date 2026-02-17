# Manipulate DAG coordinates

Manipulate DAG coordinates

## Usage

``` r
coords2df(coord_list)

coords2list(coord_df)
```

## Arguments

- coord_list:

  a named list of coordinates

- coord_df:

  a data.frame with columns x, y, and name

## Value

either a list or a data.frame with DAG node coordinates

## Examples

``` r
library(dagitty)
#> 
#> Attaching package: ‘dagitty’
#> The following object is masked from ‘package:tidygraph’:
#> 
#>     convert
coords <- list(
  x = c(A = 1, B = 2, D = 3, C = 3, F = 3, E = 4, G = 5, H = 5, I = 5),
  y = c(A = 0, B = 0, D = 1, C = 0, F = -1, E = 0, G = 1, H = 0, I = -1)
)
coord_df <- coords2df(coords)
coords2list(coord_df)
#> $x
#> A B D C F E G H I 
#> 1 2 3 3 3 4 5 5 5 
#> 
#> $y
#>  A  B  D  C  F  E  G  H  I 
#>  0  0  1  0 -1  0  1  0 -1 
#> 

x <- dagitty("dag{
             G <-> H <-> I <-> G
             D <- B -> C -> I <- F <- B <- A
             H <- E <- C -> G <- D
             }")
coordinates(x) <- coords2list(coord_df)
```

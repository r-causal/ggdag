# tidy_dagitty print output snapshots

    Code
      tidy_dag1
    Output
      # A DAG with: 2 nodes and 1 edges
      #
      # A tibble: 2 x 7
        name       x     y direction to      xend   yend
        <chr>  <dbl> <dbl> <fct>     <chr>  <dbl>  <dbl>
      1 x     -0.611 0.166 ->        y      0.181  0.784
      2 y      0.181 0.784 <NA>      <NA>  NA     NA    

---

    Code
      tidy_dag2
    Output
      # A DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      #
      # A tibble: 4 x 7
        name       x       y direction to      xend    yend
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>   <dbl>
      1 x      0.335  0.635  ->        y     -0.469  0.0367
      2 y     -0.469  0.0367 <NA>      <NA>  NA     NA     
      3 z      0.451 -0.361  ->        x      0.335  0.635 
      4 z      0.451 -0.361  ->        y     -0.469  0.0367

---

    Code
      tidy_dag3
    Output
      # A DAG with: 3 nodes and 3 edges
      # Latent Variable: u
      #
      # A tibble: 4 x 7
        name       x       y direction to      xend    yend
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>   <dbl>
      1 u      0.335  0.635  ->        x     -0.469  0.0367
      2 u      0.335  0.635  ->        y      0.451 -0.361 
      3 x     -0.469  0.0367 ->        y      0.451 -0.361 
      4 y      0.451 -0.361  <NA>      <NA>  NA     NA     

---

    Code
      tidy_dag4
    Output
      # A DAG with: 3 nodes and 3 edges
      # Paths opened by conditioning on a collider: x <-> y, x <-> y
      #
      # A tibble: 6 x 8
        name       x       y direction to      xend   yend collider_line
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>  <dbl> <lgl>        
      1 m      0.335  0.635  <NA>      <NA>  NA     NA     FALSE        
      2 x     -0.469  0.0368 ->        m      0.335  0.635 FALSE        
      3 x     -0.469  0.0368 ->        y      0.451 -0.360 FALSE        
      4 y      0.451 -0.360  ->        m      0.335  0.635 FALSE        
      5 x     -0.469  0.0368 <->       y      0.451 -0.360 TRUE         
      6 x     -0.469  0.0368 <->       y      0.451 -0.360 TRUE         

---

    Code
      tidy_complex
    Output
      # A DAG with: 6 nodes and 8 edges
      # Exposure: x
      # Outcome: y
      # Latent Variable: u
      #
      # A tibble: 9 x 8
        name      x      y direction to     xend   yend label
        <chr> <dbl>  <dbl> <fct>     <chr> <dbl>  <dbl> <chr>
      1 m     -1.27 -0.235 <NA>      <NA>  NA    NA     M    
      2 u     -2.42 -2.24  ->        y     -2.35 -0.796 U    
      3 w     -3.11  1.36  ->        x     -2.26  0.426 W    
      4 w     -3.11  1.36  ->        z     -3.21  0.242 W    
      5 x     -2.26  0.426 ->        m     -1.27 -0.235 X    
      6 x     -2.26  0.426 ->        y     -2.35 -0.796 X    
      7 y     -2.35 -0.796 ->        m     -1.27 -0.235 Y    
      8 z     -3.21  0.242 ->        x     -2.26  0.426 Z    
      9 z     -3.21  0.242 ->        y     -2.35 -0.796 Z    


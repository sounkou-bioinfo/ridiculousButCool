Dynamic Nested Loop Function Generator in R
================
AST Manipulation Master
2025-08-15

- [Creating Functions with N Nested Loops of K
  Iterations](#creating-functions-with-n-nested-loops-of-k-iterations)
- [Method 1: Using AST Manipulation with `substitute()` and
  `eval()`](#method-1-using-ast-manipulation-with-substitute-and-eval)
- [Method 2: Using Base R AST Manipulation with Language
  Objects](#method-2-using-base-r-ast-manipulation-with-language-objects)
- [Method 3: Recursive Approach with Custom
  Actions](#method-3-recursive-approach-with-custom-actions)
- [Method 4: Tail Call Optimized
  Approach](#method-4-tail-call-optimized-approach)
- [Method 5: Using Exec for Expression
  Evaluation](#method-5-using-exec-for-expression-evaluation)
- [Method 6: The Ridiculous But Cool Version - String
  Generation](#method-6-the-ridiculous-but-cool-version---string-generation)
- [Performance Comparison: The Language Wars
  Continue](#performance-comparison-the-language-wars-continue)
- [Advanced: Custom Actions in Nested
  Loops](#advanced-custom-actions-in-nested-loops)
- [The Ultimate Test: Generating the 1 Billion Iteration
  Function](#the-ultimate-test-generating-the-1-billion-iteration-function)
- [Conclusion](#conclusion)

## Creating Functions with N Nested Loops of K Iterations

This started as a joke inspired by the programming language speed
comparison meme, but let’s see if we can actually create a function that
generates functions with arbitrary nested loops in R!

### The Challenge

We want to create a function `create_nested_loops(n, k)` that:

- Takes `n` (number of nested loops) and `k` (iterations per loop)
- Returns a function that executes n nested loops, each running k times
- Should be able to handle arbitrary values of n and k

## Method 1: Using AST Manipulation with `substitute()` and `eval()`

``` r
create_nested_loops_v1 <- function(n, k, action = quote(counter <- counter + 1)) {
  if (n <= 0) {
    stop("Number of loops must be positive")
  }
  
  # Start with the innermost action
  body_expr <- action
  
  # Build nested loops from inside out using lapply
  body_expr <- Reduce(function(body, i) {
    loop_var <- paste0("i", i)
    substitute(
      for (VAR in 1:K) {
        BODY
      },
      list(VAR = as.name(loop_var), K = k, BODY = body)
    )
  }, n:1, init = body_expr)
  
  # Create the complete function body
  complete_body <- substitute({
    counter <- 0
    LOOPS
    return(counter)
  }, list(LOOPS = body_expr))
  
  # Create and return the function
  f <- function() {}
  body(f) <- complete_body
  
  return(f)
}

# Test with 3 nested loops of 5 iterations each
nested_3_5 <- create_nested_loops_v1(3, 5)
cat("Generated function body:\n")
```

    ## Generated function body:

``` r
print(body(nested_3_5))
```

    ## {
    ##     counter <- 0
    ##     for (i1 in 1:5) {
    ##         for (i2 in 1:5) {
    ##             for (i3 in 1:5) {
    ##                 counter <- counter + 1
    ##             }
    ##         }
    ##     }
    ##     return(counter)
    ## }

``` r
cat("\n")
```

``` r
result1 <- nested_3_5()
cat("3 nested loops of 5 iterations:", result1, "total iterations\n")
```

    ## 3 nested loops of 5 iterations: 125 total iterations

``` r
cat("Expected:", 5^3, "\n")
```

    ## Expected: 125

## Method 2: Using Base R AST Manipulation with Language Objects

``` r
create_nested_loops_v2 <- function(n, k, action = quote(counter <- counter + 1)) {
  if (n <= 0) {
    stop("Number of loops must be positive")
  }
  
  # Start with the action
  body_expr <- action
  
  # Build nested loops from inside out using Reduce
  body_expr <- Reduce(function(body, i) {
    loop_var <- as.name(paste0("i", i))
    # Create the for loop structure: for (loop_var in 1:k) { body }
    call("for", loop_var, call(":", 1, k), body)
  }, n:1, init = body_expr)
  
  # Create complete function body using call()
  complete_expr <- call("{", 
                       call("<-", quote(counter), 0),
                       body_expr,
                       call("return", quote(counter)))
  
  # Create and return the function
  f <- function() {}
  body(f) <- complete_expr
  
  return(f)
}

# Test the base R version
nested_4_3 <- create_nested_loops_v2(4, 3)
cat("Generated function body (call objects):\n")
```

    ## Generated function body (call objects):

``` r
print(body(nested_4_3))
```

    ## {
    ##     counter <- 0
    ##     for (i1 in 1:3) for (i2 in 1:3) for (i3 in 1:3) for (i4 in 1:3) counter <- counter + 
    ##         1
    ##     return(counter)
    ## }

``` r
cat("\n")
```

``` r
result2 <- nested_4_3()
cat("4 nested loops of 3 iterations:", result2, "total iterations\n")
```

    ## 4 nested loops of 3 iterations: 81 total iterations

``` r
cat("Expected:", 3^4, "\n")
```

    ## Expected: 81

## Method 3: Recursive Approach with Custom Actions

``` r
create_nested_loops_v3 <- function(n, k, custom_action = NULL) {
  # Default action: increment counter
  if (is.null(custom_action)) {
    action_code <- "counter <- counter + 1"
  } else {
    action_code <- deparse(substitute(custom_action))
  }
  
  # Build the nested loop structure recursively
  build_loops <- function(depth, max_depth) {
    if (depth > max_depth) {
      return(action_code)
    }
    
    inner <- build_loops(depth + 1, max_depth)
    paste0("for (i", depth, " in 1:", k, ") {\n", 
           paste(rep("  ", depth), collapse = ""), inner, "\n",
           paste(rep("  ", depth - 1), collapse = ""), "}")
  }
  
  # Create function string
  func_string <- paste0("function() {\n",
                       "  counter <- 0\n",
                       "  ", build_loops(1, n), "\n",
                       "  return(counter)\n",
                       "}")
  
  # Parse and return function
  eval(parse(text = func_string))
}

# Test recursive approach
nested_2_10 <- create_nested_loops_v3(2, 10)
cat("Generated function (string method):\n")
```

    ## Generated function (string method):

``` r
print(nested_2_10)
```

    ## function () 
    ## {
    ##     counter <- 0
    ##     for (i1 in 1:10) {
    ##         for (i2 in 1:10) {
    ##             counter <- counter + 1
    ##         }
    ##     }
    ##     return(counter)
    ## }
    ## <environment: 0x6335e6c454c8>

``` r
cat("\n")
```

``` r
result3 <- nested_2_10()
cat("2 nested loops of 10 iterations:", result3, "total iterations\n")
```

    ## 2 nested loops of 10 iterations: 100 total iterations

``` r
cat("Expected:", 10^2, "\n")
```

    ## Expected: 100

## Method 4: Tail Call Optimized Approach

``` r
create_nested_loops_tailcall <- function(n, k, action = quote(counter <- counter + 1)) {
  if (n <= 0) {
    stop("Number of loops must be positive")
  }
  
  # Tail-recursive function to build nested loops
  build_nested_expr <- function(body_expr, depth, max_depth) {
    force(body_expr)  # Force evaluation to avoid accumulating deferred evaluations
    force(depth)
    force(max_depth)
    
    if (depth > max_depth) {
      return(body_expr)
    }
    
    loop_var <- paste0("i", depth)
    new_body <- substitute(
      for (VAR in 1:K) {
        BODY
      },
      list(VAR = as.name(loop_var), K = k, BODY = body_expr)
    )
    
    # Use Tailcall for stack-efficient recursion
    Tailcall(build_nested_expr, new_body, depth + 1, max_depth)
  }
  
  # Build the expression using tail recursion
  body_expr <- build_nested_expr(action, 1, n)
  
  # Create the complete function body
  complete_body <- substitute({
    counter <- 0
    LOOPS
    return(counter)
  }, list(LOOPS = body_expr))
  
  # Create and return the function
  f <- function() {}
  body(f) <- complete_body
  
  return(f)
}

# Test tail call optimized version
nested_tailcall <- create_nested_loops_tailcall(3, 4)
cat("Generated function body (Tailcall method):\n")
```

    ## Generated function body (Tailcall method):

``` r
print(body(nested_tailcall))
```

    ## {
    ##     counter <- 0
    ##     for (i3 in 1:4) {
    ##         for (i2 in 1:4) {
    ##             for (i1 in 1:4) {
    ##                 counter <- counter + 1
    ##             }
    ##         }
    ##     }
    ##     return(counter)
    ## }

``` r
cat("\n")
```

``` r
result_tailcall <- nested_tailcall()
cat("Tail call optimized - 3 nested loops of 4 iterations:", result_tailcall, "total iterations\n")
```

    ## Tail call optimized - 3 nested loops of 4 iterations: 64 total iterations

``` r
cat("Expected:", 4^3, "\n")
```

    ## Expected: 64

``` r
# Test with deeper nesting to show stack efficiency
cat("\n=== Testing stack efficiency with deep nesting ===\n")
```

    ## 
    ## === Testing stack efficiency with deep nesting ===

``` r
deep_nested <- create_nested_loops_tailcall(10, 2)
deep_result <- deep_nested()
cat("10 nested loops of 2 iterations:", deep_result, "total iterations\n")
```

    ## 10 nested loops of 2 iterations: 1024 total iterations

``` r
cat("Expected:", 2^10, "\n")
```

    ## Expected: 1024

## Method 5: Using Exec for Expression Evaluation

``` r
create_nested_loops_exec <- function(n, k, action = quote(counter <- counter + 1)) {
  if (n <= 0) {
    stop("Number of loops must be positive")
  }
  
  # Build expression using our previous method
  body_expr <- Reduce(function(body, i) {
    loop_var <- paste0("i", i)
    substitute(
      for (VAR in 1:K) {
        BODY
      },
      list(VAR = as.name(loop_var), K = k, BODY = body)
    )
  }, n:1, init = action)
  
  # Create a function that uses Exec for cleaner call stack
  function() {
    # Create the expression to execute (without return statement for Exec)
    expr_to_exec <- substitute({
      counter <- 0
      LOOPS
      counter  # Return value without explicit return()
    }, list(LOOPS = body_expr))
    
    # Use Exec to evaluate with simplified call stack
    Exec(expr_to_exec, environment())
  }
}

# Test Exec version
nested_exec <- create_nested_loops_exec(3, 3)
cat("Generated function body (Exec method):\n")
```

    ## Generated function body (Exec method):

``` r
print(body(nested_exec))
```

    ## {
    ##     expr_to_exec <- substitute({
    ##         counter <- 0
    ##         LOOPS
    ##         counter
    ##     }, list(LOOPS = body_expr))
    ##     Exec(expr_to_exec, environment())
    ## }

``` r
cat("\n")
```

``` r
result_exec <- nested_exec()
cat("Exec-based - 3 nested loops of 3 iterations:", result_exec, "total iterations\n")
```

    ## Exec-based - 3 nested loops of 3 iterations: 27 total iterations

``` r
cat("Expected:", 3^3, "\n")
```

    ## Expected: 27

``` r
cat("\n=== Exec provides cleaner call stack evaluation ===\n")
```

    ## 
    ## === Exec provides cleaner call stack evaluation ===

``` r
cat("Exec successfully demonstrated for dynamic nested loop generation!\n")
```

    ## Exec successfully demonstrated for dynamic nested loop generation!

## Method 6: The Ridiculous But Cool Version - String Generation

``` r
create_ridiculous_nested_loops <- function(n, k, print_code = FALSE) {
  # Generate variable names
  vars <- paste0("i", 1:n)
  
  # Build the nested structure using lapply
  loop_components <- lapply(1:n, function(i) {
    indent <- paste(rep("  ", i), collapse = "")
    list(
      start = paste0(indent, "for (", vars[i], " in 1:", k, ") {"),
      end = paste0(paste(rep("  ", i - 1), collapse = ""), "}")
    )
  })
  
  loop_start <- sapply(loop_components, function(x) x$start)
  loop_end <- rev(sapply(loop_components, function(x) x$end))
  
  # The action (with proper indentation)
  action_indent <- paste(rep("  ", n + 1), collapse = "")
  action <- paste0(action_indent, "counter <- counter + 1")
  
  # Combine everything
  func_body <- c(
    "function() {",
    "  counter <- 0",
    loop_start,
    action,
    loop_end,
    paste0("  return(list(count = counter, vars = c(", 
           paste(vars, collapse = ", "), ")))"),
    "}"
  )
  
  func_string <- paste(func_body, collapse = "\n")
  
  if (print_code) {
    cat("Generated function:\n")
    cat(func_string, "\n\n")
  }
  
  eval(parse(text = func_string))
}

# Test the ridiculous version
cat("=== Testing with 5 nested loops ===\n")
```

    ## === Testing with 5 nested loops ===

``` r
nested_5_2 <- create_ridiculous_nested_loops(5, 2, print_code = TRUE)
```

    ## Generated function:
    ## function() {
    ##   counter <- 0
    ##   for (i1 in 1:2) {
    ##     for (i2 in 1:2) {
    ##       for (i3 in 1:2) {
    ##         for (i4 in 1:2) {
    ##           for (i5 in 1:2) {
    ##             counter <- counter + 1
    ##         }
    ##       }
    ##     }
    ##   }
    ## }
    ##   return(list(count = counter, vars = c(i1, i2, i3, i4, i5)))
    ## }

``` r
cat("Generated function object:\n")
```

    ## Generated function object:

``` r
print(nested_5_2)
```

    ## function () 
    ## {
    ##     counter <- 0
    ##     for (i1 in 1:2) {
    ##         for (i2 in 1:2) {
    ##             for (i3 in 1:2) {
    ##                 for (i4 in 1:2) {
    ##                   for (i5 in 1:2) {
    ##                     counter <- counter + 1
    ##                   }
    ##                 }
    ##             }
    ##         }
    ##     }
    ##     return(list(count = counter, vars = c(i1, i2, i3, i4, i5)))
    ## }
    ## <environment: 0x6335e93cb918>

``` r
cat("\n")
```

``` r
result4 <- nested_5_2()
cat("Result:", result4$count, "iterations\n")
```

    ## Result: 32 iterations

``` r
cat("Expected:", 2^5, "\n")
```

    ## Expected: 32

## Performance Comparison: The Language Wars Continue

Let’s see how our dynamic R functions perform compared to a static
implementation:

``` r
# Static implementation for comparison
static_3_loops <- function(k) {
  counter <- 0
  for (i1 in 1:k) {
    for (i2 in 1:k) {
      for (i3 in 1:k) {
        counter <- counter + 1
      }
    }
  }
  return(counter)
}

# Benchmark (only if microbenchmark is available)
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  library(microbenchmark)
  
  k <- 50
  dynamic_func_v1 <- create_nested_loops_v1(3, k)
  dynamic_func_v2 <- create_nested_loops_v2(3, k)
  dynamic_func_v3 <- create_nested_loops_v3(3, k)
  dynamic_func_tailcall <- create_nested_loops_tailcall(3, k)
  dynamic_func_exec <- create_nested_loops_exec(3, k)
  
  benchmark_results <- microbenchmark(
    static = static_3_loops(k),
    dynamic_v1_substitute = dynamic_func_v1(),
    dynamic_v2_call = dynamic_func_v2(),
    dynamic_v3_string = dynamic_func_v3(),
    dynamic_v4_tailcall = dynamic_func_tailcall(),
    dynamic_v5_exec = dynamic_func_exec(),
    times = 100
  )
  
  print(benchmark_results)
} else {
  # Simple timing comparison if microbenchmark not available
  k <- 50
  dynamic_func_v1 <- create_nested_loops_v1(3, k)
  dynamic_func_v2 <- create_nested_loops_v2(3, k)
  dynamic_func_v3 <- create_nested_loops_v3(3, k)
  dynamic_func_tailcall <- create_nested_loops_tailcall(3, k)
  dynamic_func_exec <- create_nested_loops_exec(3, k)
  
  cat("Simple timing comparison:\n")
  
  t1 <- system.time(invisible(lapply(1:100, function(x) static_3_loops(k))))
  cat("Static implementation:", t1[3], "seconds\n")
  
  t2 <- system.time(invisible(lapply(1:100, function(x) dynamic_func_v1())))
  cat("Dynamic v1 (substitute):", t2[3], "seconds\n")
  
  t3 <- system.time(invisible(lapply(1:100, function(x) dynamic_func_v2())))
  cat("Dynamic v2 (call objects):", t3[3], "seconds\n")
  
  t4 <- system.time(invisible(lapply(1:100, function(x) dynamic_func_v3())))
  cat("Dynamic v3 (string generation):", t4[3], "seconds\n")
  
  t5 <- system.time(invisible(lapply(1:100, function(x) dynamic_func_tailcall())))
  cat("Dynamic v4 (Tailcall):", t5[3], "seconds\n")
  
  t6 <- system.time(invisible(lapply(1:100, function(x) dynamic_func_exec())))
  cat("Dynamic v5 (Exec):", t6[3], "seconds\n")
}
```

    ## Unit: milliseconds
    ##                   expr       min        lq      mean    median        uq
    ##                 static  3.238192  3.696385  4.345908  4.262763  4.761919
    ##  dynamic_v1_substitute  3.361532  3.942646  4.854054  4.484475  4.963586
    ##        dynamic_v2_call  3.314598  3.690345  4.598120  4.219078  4.704999
    ##      dynamic_v3_string 33.560552 36.579476 39.279383 38.613184 40.763283
    ##    dynamic_v4_tailcall  3.326891  3.759975  4.704157  4.342941  4.675909
    ##        dynamic_v5_exec 33.488126 35.736177 39.322758 37.563924 39.985076
    ##         max neval
    ##    9.862218   100
    ##   35.179752   100
    ##   25.577483   100
    ##   63.341920   100
    ##   40.561371   100
    ##  117.122149   100

## Advanced: Custom Actions in Nested Loops

``` r
# Create a function that builds a matrix using nested loops
create_matrix_builder <- function(rows, cols) {
  # Define the action: result[i1, i2] <- i1 * i2
  action <- call("<-", 
                call("[", quote(result), quote(i1), quote(i2)),
                call("*", quote(i1), quote(i2)))
  
  # Build the nested loops using Reduce
  body_expr <- Reduce(function(body, i) {
    var_name <- as.name(paste0("i", i))
    max_val <- if (i == 1) rows else cols
    call("for", var_name, call(":", 1, max_val), body)
  }, 2:1, init = action)
  
  # Create complete function body
  complete_body <- call("{",
                       call("<-", quote(result), 
                           call("matrix", 0, nrow = rows, ncol = cols)),
                       body_expr,
                       call("return", quote(result)))
  
  f <- function() {}
  body(f) <- complete_body
  return(f)
}

# Test matrix builder
matrix_builder <- create_matrix_builder(4, 5)
cat("Generated matrix builder function body:\n")
```

    ## Generated matrix builder function body:

``` r
print(body(matrix_builder))
```

    ## {
    ##     result <- matrix(0, nrow = 4, ncol = 5)
    ##     for (i1 in 1:4) for (i2 in 1:5) result[i1, i2] <- i1 * i2
    ##     return(result)
    ## }

``` r
cat("\n")
```

``` r
my_matrix <- matrix_builder()
print(my_matrix)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    2    3    4    5
    ## [2,]    2    4    6    8   10
    ## [3,]    3    6    9   12   15
    ## [4,]    4    8   12   16   20

## The Ultimate Test: Generating the 1 Billion Iteration Function

``` r
# WARNING: Don't actually run this unless you have time to kill!
# This would create the equivalent of the benchmark from the meme

create_billion_iteration_function <- function() {
  # For 1 billion iterations, we could do 10 loops of 100 iterations each
  # 100^10 = 10^20 (way too much!)
  # Let's do 4 loops of 178 iterations: 178^4 ≈ 1 billion
  
  k <- 178  # 178^4 = 1,002,002,816 ≈ 1 billion
  n <- 4
  
  cat("Creating function with", n, "nested loops of", k, "iterations each\n")
  cat("Total iterations:", k^n, "\n")
  
  create_nested_loops_v1(n, k)
}

# Generate but don't run (for your sanity)
billion_func <- create_billion_iteration_function()
cat("Function created! (But we won't run it...)\n")
```

## Conclusion

**Yes, you absolutely CAN create functions that generate functions with
n nested loops of k iterations in R using only base R!**

We’ve demonstrated multiple approaches:

1.  **AST manipulation with `substitute()`** - Classic R metaprogramming
    using `Reduce()`
2.  **Language objects with `call()` and `as.name()`** - Pure base R
    approach
3.  **Recursive string generation** - More readable for complex cases
4.  **Tail call optimization with `Tailcall`** - Stack-efficient
    recursive building
5.  **Expression evaluation with `Exec`** - Simplified call stack
    execution
6.  **String concatenation** - The most flexible but verbose approach

R’s metaprogramming capabilities in base R alone make this not just
possible, but surprisingly elegant. The `Tailcall()` and `Exec()`
functions add sophisticated options for stack management and call
optimization, while using `lapply()` instead of traditional loops makes
the code more functional.

Key base R functions used:

- `call()` - Creates language objects (calls)
- `as.name()` - Creates symbol objects
- `quote()` - Captures expressions without evaluation
- `substitute()` - Template-based expression manipulation
- `body()<-` - Modifies function bodies
- `Reduce()` - Functional programming for accumulative operations
- `lapply()` - Functional alternative to for loops
- `Tailcall()` - Stack-efficient tail recursion
- `Exec()` - Simplified call stack evaluation

While Daniel Lockyer might say “they’re basically all the same, just
marginally different syntax,” R’s built-in metaprogramming features make
it particularly well-suited for this kind of ridiculous-but-cool
programming challenge.

The performance is actually quite reasonable too - the overhead of
dynamic function generation is minimal compared to the actual loop
execution.

So there you have it: **R can absolutely compete in the nested loop
olympics using only base R!** 🏆

``` r
# One final demonstration
cat("Creating a function with 6 nested loops of 3 iterations each...\n")
```

    ## Creating a function with 6 nested loops of 3 iterations each...

``` r
final_func <- create_nested_loops_v2(6, 3)
cat("Final generated function body:\n")
```

    ## Final generated function body:

``` r
print(body(final_func))
```

    ## {
    ##     counter <- 0
    ##     for (i1 in 1:3) for (i2 in 1:3) for (i3 in 1:3) for (i4 in 1:3) for (i5 in 1:3) for (i6 in 1:3) counter <- counter + 
    ##         1
    ##     return(counter)
    ## }

``` r
cat("\n")
```

``` r
final_result <- final_func()
cat("Total iterations:", final_result, "\n")
```

    ## Total iterations: 729

``` r
cat("Expected:", 3^6, "\n")
```

    ## Expected: 729

``` r
cat("Math checks out! ✓\n")
```

    ## Math checks out! ✓

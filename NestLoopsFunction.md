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
- [More Ridiculous Test: 1000 NESTED LOOPS
  Function](#more-ridiculous-test-1000-nested-loops-function)

## Creating Functions with N Nested Loops of K Iterations

This started as a joke inspired by the programming language speed
comparison meme, but let’s see if we can actually create a function that
generates functions with arbitrary nested loops in R!

### Understanding R’s Homoiconic Nature and AST Manipulation

Before we dive into the implementation, it’s essential to understand
what makes this possible. R is a **homoiconic language**, meaning that
code and data share the same representation. As stated in the [R
Language
Definition](https://cran.r-project.org/doc/manuals/r-release/R-lang.html):

> “R is a functional programming language with lazy evaluation. R
> functions are objects and can be manipulated in much the same way as
> any other object.”

This homoiconicity allows us to:

- Treat code as data structures (Abstract Syntax Trees)
- Manipulate these ASTs programmatically
- Generate new code dynamically at runtime
- Build complex nested structures through metaprogramming

The R Language Definition further explains that expressions in R are
represented as language objects, which can be constructed using
functions like `call()`, `quote()`, and `substitute()`. This is exactly
what we’re exploiting to create arbitrary levels of nested loops.

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
print(nested_3_5)
```

    ## function () 
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
    ## <environment: 0x5d1ed5d918f8>

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
print(nested_4_3)
```

    ## function () 
    ## {
    ##     counter <- 0
    ##     for (i1 in 1:3) for (i2 in 1:3) for (i3 in 1:3) for (i4 in 1:3) counter <- counter + 
    ##         1
    ##     return(counter)
    ## }
    ## <environment: 0x5d1ed36bffb0>

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
    ## <environment: 0x5d1ed6679b48>

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
print(nested_tailcall)
```

    ## function () 
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
    ## <environment: 0x5d1ed75b5130>

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
print(nested_exec)
```

    ## function () 
    ## {
    ##     expr_to_exec <- substitute({
    ##         counter <- 0
    ##         LOOPS
    ##         counter
    ##     }, list(LOOPS = body_expr))
    ##     Exec(expr_to_exec, environment())
    ## }
    ## <bytecode: 0x5d1ed85fee58>
    ## <environment: 0x5d1ed8319828>

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
    ## <environment: 0x5d1ed8df43d0>

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
    ##                 static  3.355944  3.926687  4.717372  4.450811  4.940643
    ##  dynamic_v1_substitute  3.289106  4.166870  5.281867  4.760906  5.256954
    ##        dynamic_v2_call  3.270039  4.180839  4.935649  4.621643  5.035068
    ##      dynamic_v3_string  3.346656  3.896096  4.883679  4.372170  4.920528
    ##    dynamic_v4_tailcall  3.235398  4.068535  4.976384  4.467747  5.013907
    ##        dynamic_v5_exec 31.684894 36.609995 40.876614 38.921428 43.723261
    ##        max neval
    ##   9.474738   100
    ##  39.251917   100
    ##  25.907763   100
    ##  35.792329   100
    ##  31.798455   100
    ##  87.523624   100

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
print(matrix_builder)
```

    ## function () 
    ## {
    ##     result <- matrix(0, nrow = 4, ncol = 5)
    ##     for (i1 in 1:4) for (i2 in 1:5) result[i1, i2] <- i1 * i2
    ##     return(result)
    ## }
    ## <environment: 0x5d1ed6874438>

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
print(final_func)
```

    ## function () 
    ## {
    ##     counter <- 0
    ##     for (i1 in 1:3) for (i2 in 1:3) for (i3 in 1:3) for (i4 in 1:3) for (i5 in 1:3) for (i6 in 1:3) counter <- counter + 
    ##         1
    ##     return(counter)
    ## }
    ## <environment: 0x5d1ed77f0ae8>

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

## More Ridiculous Test: 1000 NESTED LOOPS Function

``` r
# The TRULY ridiculous test - let's create a function with 1000 NESTED LOOPS!
# Each loop will only do 1 iteration, but we'll have 1000 levels of nesting!

cat("=== Creating an absolutely ridiculous 1000 NESTED LOOPS function ===\n")
```

    ## === Creating an absolutely ridiculous 1000 NESTED LOOPS function ===

``` r
cat("WARNING: This will create a function with 1000 levels of nested for loops!\n")
```

    ## WARNING: This will create a function with 1000 levels of nested for loops!

``` r
cat("Each loop runs only 1 iteration, but the nesting depth is INSANE!\n\n")
```

    ## Each loop runs only 1 iteration, but the nesting depth is INSANE!

``` r
# Create the monster
cat("Building AST for 1000 nested loops... (this might take a moment)\n")
```

    ## Building AST for 1000 nested loops... (this might take a moment)

``` r
thousand_loops_func <- create_nested_loops_v2(1000, 1)

cat("Generated function AST structure analysis:\n")
```

    ## Generated function AST structure analysis:

``` r
# Print just the structure, not the full body (it would be enormous!)
func_str <- deparse(body(thousand_loops_func))
cat("Function has", length(func_str), "lines of generated AST code!\n")
```

    ## Function has 6 lines of generated AST code!

``` r
cat("First 5 lines of the parsed AST:\n")
```

    ## First 5 lines of the parsed AST:

``` r
cat(paste(func_str[1:5], collapse = "\n"), "\n")
```

    ## {
    ##     counter <- 0
    ##     for (i1 in 1:1) for (i2 in 1:1) for (i3 in 1:1) for (i4 in 1:1) for (i5 in 1:1) for (i6 in 1:1) for (i7 in 1:1) for (i8 in 1:1) for (i9 in 1:1) for (i10 in 1:1) for (i11 in 1:1) for (i12 in 1:1) for (i13 in 1:1) for (i14 in 1:1) for (i15 in 1:1) for (i16 in 1:1) for (i17 in 1:1) for (i18 in 1:1) for (i19 in 1:1) for (i20 in 1:1) for (i21 in 1:1) for (i22 in 1:1) for (i23 in 1:1) for (i24 in 1:1) for (i25 in 1:1) for (i26 in 1:1) for (i27 in 1:1) for (i28 in 1:1) for (i29 in 1:1) for (i30 in 1:1) for (i31 in 1:1) for (i32 in 1:1) for (i33 in 1:1) for (i34 in 1:1) for (i35 in 1:1) for (i36 in 1:1) for (i37 in 1:1) for (i38 in 1:1) for (i39 in 1:1) for (i40 in 1:1) for (i41 in 1:1) for (i42 in 1:1) for (i43 in 1:1) for (i44 in 1:1) for (i45 in 1:1) for (i46 in 1:1) for (i47 in 1:1) for (i48 in 1:1) for (i49 in 1:1) for (i50 in 1:1) for (i51 in 1:1) for (i52 in 1:1) for (i53 in 1:1) for (i54 in 1:1) for (i55 in 1:1) for (i56 in 1:1) for (i57 in 1:1) for (i58 in 1:1) for (i59 in 1:1) for (i60 in 1:1) for (i61 in 1:1) for (i62 in 1:1) for (i63 in 1:1) for (i64 in 1:1) for (i65 in 1:1) for (i66 in 1:1) for (i67 in 1:1) for (i68 in 1:1) for (i69 in 1:1) for (i70 in 1:1) for (i71 in 1:1) for (i72 in 1:1) for (i73 in 1:1) for (i74 in 1:1) for (i75 in 1:1) for (i76 in 1:1) for (i77 in 1:1) for (i78 in 1:1) for (i79 in 1:1) for (i80 in 1:1) for (i81 in 1:1) for (i82 in 1:1) for (i83 in 1:1) for (i84 in 1:1) for (i85 in 1:1) for (i86 in 1:1) for (i87 in 1:1) for (i88 in 1:1) for (i89 in 1:1) for (i90 in 1:1) for (i91 in 1:1) for (i92 in 1:1) for (i93 in 1:1) for (i94 in 1:1) for (i95 in 1:1) for (i96 in 1:1) for (i97 in 1:1) for (i98 in 1:1) for (i99 in 1:1) for (i100 in 1:1) for (i101 in 1:1) for (i102 in 1:1) for (i103 in 1:1) for (i104 in 1:1) for (i105 in 1:1) for (i106 in 1:1) for (i107 in 1:1) for (i108 in 1:1) for (i109 in 1:1) for (i110 in 1:1) for (i111 in 1:1) for (i112 in 1:1) for (i113 in 1:1) for (i114 in 1:1) for (i115 in 1:1) for (i116 in 1:1) for (i117 in 1:1) for (i118 in 1:1) for (i119 in 1:1) for (i120 in 1:1) for (i121 in 1:1) for (i122 in 1:1) for (i123 in 1:1) for (i124 in 1:1) for (i125 in 1:1) for (i126 in 1:1) for (i127 in 1:1) for (i128 in 1:1) for (i129 in 1:1) for (i130 in 1:1) for (i131 in 1:1) for (i132 in 1:1) for (i133 in 1:1) for (i134 in 1:1) for (i135 in 1:1) for (i136 in 1:1) for (i137 in 1:1) for (i138 in 1:1) for (i139 in 1:1) for (i140 in 1:1) for (i141 in 1:1) for (i142 in 1:1) for (i143 in 1:1) for (i144 in 1:1) for (i145 in 1:1) for (i146 in 1:1) for (i147 in 1:1) for (i148 in 1:1) for (i149 in 1:1) for (i150 in 1:1) for (i151 in 1:1) for (i152 in 1:1) for (i153 in 1:1) for (i154 in 1:1) for (i155 in 1:1) for (i156 in 1:1) for (i157 in 1:1) for (i158 in 1:1) for (i159 in 1:1) for (i160 in 1:1) for (i161 in 1:1) for (i162 in 1:1) for (i163 in 1:1) for (i164 in 1:1) for (i165 in 1:1) for (i166 in 1:1) for (i167 in 1:1) for (i168 in 1:1) for (i169 in 1:1) for (i170 in 1:1) for (i171 in 1:1) for (i172 in 1:1) for (i173 in 1:1) for (i174 in 1:1) for (i175 in 1:1) for (i176 in 1:1) for (i177 in 1:1) for (i178 in 1:1) for (i179 in 1:1) for (i180 in 1:1) for (i181 in 1:1) for (i182 in 1:1) for (i183 in 1:1) for (i184 in 1:1) for (i185 in 1:1) for (i186 in 1:1) for (i187 in 1:1) for (i188 in 1:1) for (i189 in 1:1) for (i190 in 1:1) for (i191 in 1:1) for (i192 in 1:1) for (i193 in 1:1) for (i194 in 1:1) for (i195 in 1:1) for (i196 in 1:1) for (i197 in 1:1) for (i198 in 1:1) for (i199 in 1:1) for (i200 in 1:1) for (i201 in 1:1) for (i202 in 1:1) for (i203 in 1:1) for (i204 in 1:1) for (i205 in 1:1) for (i206 in 1:1) for (i207 in 1:1) for (i208 in 1:1) for (i209 in 1:1) for (i210 in 1:1) for (i211 in 1:1) for (i212 in 1:1) for (i213 in 1:1) for (i214 in 1:1) for (i215 in 1:1) for (i216 in 1:1) for (i217 in 1:1) for (i218 in 1:1) for (i219 in 1:1) for (i220 in 1:1) for (i221 in 1:1) for (i222 in 1:1) for (i223 in 1:1) for (i224 in 1:1) for (i225 in 1:1) for (i226 in 1:1) for (i227 in 1:1) for (i228 in 1:1) for (i229 in 1:1) for (i230 in 1:1) for (i231 in 1:1) for (i232 in 1:1) for (i233 in 1:1) for (i234 in 1:1) for (i235 in 1:1) for (i236 in 1:1) for (i237 in 1:1) for (i238 in 1:1) for (i239 in 1:1) for (i240 in 1:1) for (i241 in 1:1) for (i242 in 1:1) for (i243 in 1:1) for (i244 in 1:1) for (i245 in 1:1) for (i246 in 1:1) for (i247 in 1:1) for (i248 in 1:1) for (i249 in 1:1) for (i250 in 1:1) for (i251 in 1:1) for (i252 in 1:1) for (i253 in 1:1) for (i254 in 1:1) for (i255 in 1:1) for (i256 in 1:1) for (i257 in 1:1) for (i258 in 1:1) for (i259 in 1:1) for (i260 in 1:1) for (i261 in 1:1) for (i262 in 1:1) for (i263 in 1:1) for (i264 in 1:1) for (i265 in 1:1) for (i266 in 1:1) for (i267 in 1:1) for (i268 in 1:1) for (i269 in 1:1) for (i270 in 1:1) for (i271 in 1:1) for (i272 in 1:1) for (i273 in 1:1) for (i274 in 1:1) for (i275 in 1:1) for (i276 in 1:1) for (i277 in 1:1) for (i278 in 1:1) for (i279 in 1:1) for (i280 in 1:1) for (i281 in 1:1) for (i282 in 1:1) for (i283 in 1:1) for (i284 in 1:1) for (i285 in 1:1) for (i286 in 1:1) for (i287 in 1:1) for (i288 in 1:1) for (i289 in 1:1) for (i290 in 1:1) for (i291 in 1:1) for (i292 in 1:1) for (i293 in 1:1) for (i294 in 1:1) for (i295 in 1:1) for (i296 in 1:1) for (i297 in 1:1) for (i298 in 1:1) for (i299 in 1:1) for (i300 in 1:1) for (i301 in 1:1) for (i302 in 1:1) for (i303 in 1:1) for (i304 in 1:1) for (i305 in 1:1) for (i306 in 1:1) for (i307 in 1:1) for (i308 in 1:1) for (i309 in 1:1) for (i310 in 1:1) for (i311 in 1:1) for (i312 in 1:1) for (i313 in 1:1) for (i314 in 1:1) for (i315 in 1:1) for (i316 in 1:1) for (i317 in 1:1) for (i318 in 1:1) for (i319 in 1:1) for (i320 in 1:1) for (i321 in 1:1) for (i322 in 1:1) for (i323 in 1:1) for (i324 in 1:1) for (i325 in 1:1) for (i326 in 1:1) for (i327 in 1:1) for (i328 in 1:1) for (i329 in 1:1) for (i330 in 1:1) for (i331 in 1:1) for (i332 in 1:1) for (i333 in 1:1) for (i334 in 1:1) for (i335 in 1:1) for (i336 in 1:1) for (i337 in 1:1) for (i338 in 1:1) for (i339 in 1:1) for (i340 in 1:1) for (i341 in 1:1) for (i342 in 1:1) for (i343 in 1:1) for (i344 in 1:1) for (i345 in 1:1) for (i346 in 1:1) for (i347 in 1:1) for (i348 in 1:1) for (i349 in 1:1) for (i350 in 1:1) for (i351 in 1:1) for (i352 in 1:1) for (i353 in 1:1) for (i354 in 1:1) for (i355 in 1:1) for (i356 in 1:1) for (i357 in 1:1) for (i358 in 1:1) for (i359 in 1:1) for (i360 in 1:1) for (i361 in 1:1) for (i362 in 1:1) for (i363 in 1:1) for (i364 in 1:1) for (i365 in 1:1) for (i366 in 1:1) for (i367 in 1:1) for (i368 in 1:1) for (i369 in 1:1) for (i370 in 1:1) for (i371 in 1:1) for (i372 in 1:1) for (i373 in 1:1) for (i374 in 1:1) for (i375 in 1:1) for (i376 in 1:1) for (i377 in 1:1) for (i378 in 1:1) for (i379 in 1:1) for (i380 in 1:1) for (i381 in 1:1) for (i382 in 1:1) for (i383 in 1:1) for (i384 in 1:1) for (i385 in 1:1) for (i386 in 1:1) for (i387 in 1:1) for (i388 in 1:1) for (i389 in 1:1) for (i390 in 1:1) for (i391 in 1:1) for (i392 in 1:1) for (i393 in 1:1) for (i394 in 1:1) for (i395 in 1:1) for (i396 in 1:1) for (i397 in 1:1) for (i398 in 1:1) for (i399 in 1:1) for (i400 in 1:1) for (i401 in 1:1) for (i402 in 1:1) for (i403 in 1:1) for (i404 in 1:1) for (i405 in 1:1) for (i406 in 1:1) for (i407 in 1:1) for (i408 in 1:1) for (i409 in 1:1) for (i410 in 1:1) for (i411 in 1:1) for (i412 in 1:1) for (i413 in 1:1) for (i414 in 1:1) for (i415 in 1:1) for (i416 in 1:1) for (i417 in 1:1) for (i418 in 1:1) for (i419 in 1:1) for (i420 in 1:1) for (i421 in 1:1) for (i422 in 1:1) for (i423 in 1:1) for (i424 in 1:1) for (i425 in 1:1) for (i426 in 1:1) for (i427 in 1:1) for (i428 in 1:1) for (i429 in 1:1) for (i430 in 1:1) for (i431 in 1:1) for (i432 in 1:1) for (i433 in 1:1) for (i434 in 1:1) for (i435 in 1:1) for (i436 in 1:1) for (i437 in 1:1) for (i438 in 1:1) for (i439 in 1:1) for (i440 in 1:1) for (i441 in 1:1) for (i442 in 1:1) for (i443 in 1:1) for (i444 in 1:1) for (i445 in 1:1) for (i446 in 1:1) for (i447 in 1:1) for (i448 in 1:1) for (i449 in 1:1) for (i450 in 1:1) for (i451 in 1:1) for (i452 in 1:1) for (i453 in 1:1) for (i454 in 1:1) for (i455 in 1:1) for (i456 in 1:1) for (i457 in 1:1) for (i458 in 1:1) for (i459 in 1:1) for (i460 in 1:1) for (i461 in 1:1) for (i462 in 1:1) for (i463 in 1:1) for (i464 in 1:1) for (i465 in 1:1) for (i466 in 1:1) for (i467 in 1:1) for (i468 in 1:1) for (i469 in 1:1) for (i470 in 1:1) for (i471 in 1:1) for (i472 in 1:1) for (i473 in 1:1) for (i474 in 1:1) for (i475 in 1:1) for (i476 in 1:1) for (i477 in 1:1) for (i478 in 1:1) for (i479 in 1:1) for (i480 in 1:1) for (i481 in 1:1) for (i482 in 1:1) for (i483 in 1:1) for (i484 in 1:1) for (i485 in 1:1) for (i486 in 1:1) for (i487 in 1:1) for (i488 in 1:1) for (i489 in 1:1) for (i490 in 1:1) for (i491 in 1:1) for (i492 in 1:1) for (i493 in 1:1) for (i494 in 1:1) for (i495 in 1:1) for (i496 in 1:1) for (i497 in 1:1) for (i498 in 1:1) for (i499 in 1:1) for (i500 in 1:1) for (i501 in 1:1) for (i502 in 1:1) for (i503 in 1:1) for (i504 in 1:1) for (i505 in 1:1) for (i506 in 1:1) for (i507 in 1:1) for (i508 in 1:1) for (i509 in 1:1) for (i510 in 1:1) for (i511 in 1:1) for (i512 in 1:1) for (i513 in 1:1) for (i514 in 1:1) for (i515 in 1:1) for (i516 in 1:1) for (i517 in 1:1) for (i518 in 1:1) for (i519 in 1:1) for (i520 in 1:1) for (i521 in 1:1) for (i522 in 1:1) for (i523 in 1:1) for (i524 in 1:1) for (i525 in 1:1) for (i526 in 1:1) for (i527 in 1:1) for (i528 in 1:1) for (i529 in 1:1) for (i530 in 1:1) for (i531 in 1:1) for (i532 in 1:1) for (i533 in 1:1) for (i534 in 1:1) for (i535 in 1:1) for (i536 in 1:1) for (i537 in 1:1) for (i538 in 1:1) for (i539 in 1:1) for (i540 in 1:1) for (i541 in 1:1) for (i542 in 1:1) for (i543 in 1:1) for (i544 in 1:1) for (i545 in 1:1) for (i546 in 1:1) for (i547 in 1:1) for (i548 in 1:1) for (i549 in 1:1) for (i550 in 1:1) for (i551 in 1:1) for (i552 in 1:1) for (i553 in 1:1) for (i554 in 1:1) for (i555 in 1:1) for (i556 in 1:1) for (i557 in 1:1) for (i558 in 1:1) for (i559 in 1:1) for (i560 in 1:1) for (i561 in 1:1) for (i562 in 1:1) for (i563 in 1:1) for (i564 in 1:1) for (i565 in 1:1) for (i566 in 1:1) for (i567 in 1:1) for (i568 in 1:1) for (i569 in 1:1) for (i570 in 1:1) for (i571 in 1:1) for (i572 in 1:1) for (i573 in 1:1) for (i574 in 1:1) for (i575 in 1:1) for (i576 in 1:1) for (i577 in 1:1) for (i578 in 1:1) for (i579 in 1:1) for (i580 in 1:1) for (i581 in 1:1) for (i582 in 1:1) for (i583 in 1:1) for (i584 in 1:1) for (i585 in 1:1) for (i586 in 1:1) for (i587 in 1:1) for (i588 in 1:1) for (i589 in 1:1) for (i590 in 1:1) for (i591 in 1:1) for (i592 in 1:1) for (i593 in 1:1) for (i594 in 1:1) for (i595 in 1:1) for (i596 in 1:1) for (i597 in 1:1) for (i598 in 1:1) for (i599 in 1:1) for (i600 in 1:1) for (i601 in 1:1) for (i602 in 1:1) for (i603 in 1:1) for (i604 in 1:1) for (i605 in 1:1) for (i606 in 1:1) for (i607 in 1:1) for (i608 in 1:1) for (i609 in 1:1) for (i610 in 1:1) for (i611 in 1:1) for (i612 in 1:1) for (i613 in 1:1) for (i614 in 1:1) for (i615 in 1:1) for (i616 in 1:1) for (i617 in 1:1) for (i618 in 1:1) for (i619 in 1:1) for (i620 in 1:1) for (i621 in 1:1) for (i622 in 1:1) for (i623 in 1:1) for (i624 in 1:1) for (i625 in 1:1) for (i626 in 1:1) for (i627 in 1:1) for (i628 in 1:1) for (i629 in 1:1) for (i630 in 1:1) for (i631 in 1:1) for (i632 in 1:1) for (i633 in 1:1) for (i634 in 1:1) for (i635 in 1:1) for (i636 in 1:1) for (i637 in 1:1) for (i638 in 1:1) for (i639 in 1:1) for (i640 in 1:1) for (i641 in 1:1) for (i642 in 1:1) for (i643 in 1:1) for (i644 in 1:1) for (i645 in 1:1) for (i646 in 1:1) for (i647 in 1:1) for (i648 in 1:1) for (i649 in 1:1) for (i650 in 1:1) for (i651 in 1:1) for (i652 in 1:1) for (i653 in 1:1) for (i654 in 1:1) for (i655 in 1:1) for (i656 in 1:1) for (i657 in 1:1) for (i658 in 1:1) for (i659 in 1:1) for (i660 in 1:1) for (i661 in 1:1) for (i662 in 1:1) for (i663 in 1:1) for (i664 in 1:1) for (i665 in 1:1) for (i666 in 1:1) for (i667 in 1:1) for (i668 in 1:1) for (i669 in 1:1) for (i670 in 1:1) for (i671 in 1:1) for (i672 in 1:1) for (i673 in 1:1) for (i674 in 1:1) for (i675 in 1:1) for (i676 in 1:1) for (i677 in 1:1) for (i678 in 1:1) for (i679 in 1:1) for (i680 in 1:1) for (i681 in 1:1) for (i682 in 1:1) for (i683 in 1:1) for (i684 in 1:1) for (i685 in 1:1) for (i686 in 1:1) for (i687 in 1:1) for (i688 in 1:1) for (i689 in 1:1) for (i690 in 1:1) for (i691 in 1:1) for (i692 in 1:1) for (i693 in 1:1) for (i694 in 1:1) for (i695 in 1:1) for (i696 in 1:1) for (i697 in 1:1) for (i698 in 1:1) for (i699 in 1:1) for (i700 in 1:1) for (i701 in 1:1) for (i702 in 1:1) for (i703 in 1:1) for (i704 in 1:1) for (i705 in 1:1) for (i706 in 1:1) for (i707 in 1:1) for (i708 in 1:1) for (i709 in 1:1) for (i710 in 1:1) for (i711 in 1:1) for (i712 in 1:1) for (i713 in 1:1) for (i714 in 1:1) for (i715 in 1:1) for (i716 in 1:1) for (i717 in 1:1) for (i718 in 1:1) for (i719 in 1:1) for (i720 in 1:1) for (i721 in 1:1) for (i722 in 1:1) for (i723 in 1:1) for (i724 in 1:1) for (i725 in 1:1) for (i726 in 1:1) for (i727 in 1:1) for (i728 in 1:1) for (i729 in 1:1) for (i730 in 1:1) for (i731 in 1:1) for (i732 in 1:1) for (i733 in 1:1) for (i734 in 1:1) for (i735 in 1:1) for (i736 in 1:1) for (i737 in 1:1) for (i738 in 1:1) for (i739 in 1:1) for (i740 in 1:1) for (i741 in 1:1) for (i742 in 1:1) for (i743 in 1:1) for (i744 in 1:1) for (i745 in 1:1) for (i746 in 1:1) for (i747 in 1:1) for (i748 in 1:1) for (i749 in 1:1) for (i750 in 1:1) for (i751 in 1:1) for (i752 in 1:1) for (i753 in 1:1) for (i754 in 1:1) for (i755 in 1:1) for (i756 in 1:1) for (i757 in 1:1) for (i758 in 1:1) for (i759 in 1:1) for (i760 in 1:1) for (i761 in 1:1) for (i762 in 1:1) for (i763 in 1:1) for (i764 in 1:1) for (i765 in 1:1) for (i766 in 1:1) for (i767 in 1:1) for (i768 in 1:1) for (i769 in 1:1) for (i770 in 1:1) for (i771 in 1:1) for (i772 in 1:1) for (i773 in 1:1) for (i774 in 1:1) for (i775 in 1:1) for (i776 in 1:1) for (i777 in 1:1) for (i778 in 1:1) for (i779 in 1:1) for (i780 in 1:1) for (i781 in 1:1) for (i782 in 1:1) for (i783 in 1:1) for (i784 in 1:1) for (i785 in 1:1) for (i786 in 1:1) for (i787 in 1:1) for (i788 in 1:1) for (i789 in 1:1) for (i790 in 1:1) for (i791 in 1:1) for (i792 in 1:1) for (i793 in 1:1) for (i794 in 1:1) for (i795 in 1:1) for (i796 in 1:1) for (i797 in 1:1) for (i798 in 1:1) for (i799 in 1:1) for (i800 in 1:1) for (i801 in 1:1) for (i802 in 1:1) for (i803 in 1:1) for (i804 in 1:1) for (i805 in 1:1) for (i806 in 1:1) for (i807 in 1:1) for (i808 in 1:1) for (i809 in 1:1) for (i810 in 1:1) for (i811 in 1:1) for (i812 in 1:1) for (i813 in 1:1) for (i814 in 1:1) for (i815 in 1:1) for (i816 in 1:1) for (i817 in 1:1) for (i818 in 1:1) for (i819 in 1:1) for (i820 in 1:1) for (i821 in 1:1) for (i822 in 1:1) for (i823 in 1:1) for (i824 in 1:1) for (i825 in 1:1) for (i826 in 1:1) for (i827 in 1:1) for (i828 in 1:1) for (i829 in 1:1) for (i830 in 1:1) for (i831 in 1:1) for (i832 in 1:1) for (i833 in 1:1) for (i834 in 1:1) for (i835 in 1:1) for (i836 in 1:1) for (i837 in 1:1) for (i838 in 1:1) for (i839 in 1:1) for (i840 in 1:1) for (i841 in 1:1) for (i842 in 1:1) for (i843 in 1:1) for (i844 in 1:1) for (i845 in 1:1) for (i846 in 1:1) for (i847 in 1:1) for (i848 in 1:1) for (i849 in 1:1) for (i850 in 1:1) for (i851 in 1:1) for (i852 in 1:1) for (i853 in 1:1) for (i854 in 1:1) for (i855 in 1:1) for (i856 in 1:1) for (i857 in 1:1) for (i858 in 1:1) for (i859 in 1:1) for (i860 in 1:1) for (i861 in 1:1) for (i862 in 1:1) for (i863 in 1:1) for (i864 in 1:1) for (i865 in 1:1) for (i866 in 1:1) for (i867 in 1:1) for (i868 in 1:1) for (i869 in 1:1) for (i870 in 1:1) for (i871 in 1:1) for (i872 in 1:1) for (i873 in 1:1) for (i874 in 1:1) for (i875 in 1:1) for (i876 in 1:1) for (i877 in 1:1) for (i878 in 1:1) for (i879 in 1:1) for (i880 in 1:1) for (i881 in 1:1) for (i882 in 1:1) for (i883 in 1:1) for (i884 in 1:1) for (i885 in 1:1) for (i886 in 1:1) for (i887 in 1:1) for (i888 in 1:1) for (i889 in 1:1) for (i890 in 1:1) for (i891 in 1:1) for (i892 in 1:1) for (i893 in 1:1) for (i894 in 1:1) for (i895 in 1:1) for (i896 in 1:1) for (i897 in 1:1) for (i898 in 1:1) for (i899 in 1:1) for (i900 in 1:1) for (i901 in 1:1) for (i902 in 1:1) for (i903 in 1:1) for (i904 in 1:1) for (i905 in 1:1) for (i906 in 1:1) for (i907 in 1:1) for (i908 in 1:1) for (i909 in 1:1) for (i910 in 1:1) for (i911 in 1:1) for (i912 in 1:1) for (i913 in 1:1) for (i914 in 1:1) for (i915 in 1:1) for (i916 in 1:1) for (i917 in 1:1) for (i918 in 1:1) for (i919 in 1:1) for (i920 in 1:1) for (i921 in 1:1) for (i922 in 1:1) for (i923 in 1:1) for (i924 in 1:1) for (i925 in 1:1) for (i926 in 1:1) for (i927 in 1:1) for (i928 in 1:1) for (i929 in 1:1) for (i930 in 1:1) for (i931 in 1:1) for (i932 in 1:1) for (i933 in 1:1) for (i934 in 1:1) for (i935 in 1:1) for (i936 in 1:1) for (i937 in 1:1) for (i938 in 1:1) for (i939 in 1:1) for (i940 in 1:1) for (i941 in 1:1) for (i942 in 1:1) for (i943 in 1:1) for (i944 in 1:1) for (i945 in 1:1) for (i946 in 1:1) for (i947 in 1:1) for (i948 in 1:1) for (i949 in 1:1) for (i950 in 1:1) for (i951 in 1:1) for (i952 in 1:1) for (i953 in 1:1) for (i954 in 1:1) for (i955 in 1:1) for (i956 in 1:1) for (i957 in 1:1) for (i958 in 1:1) for (i959 in 1:1) for (i960 in 1:1) for (i961 in 1:1) for (i962 in 1:1) for (i963 in 1:1) for (i964 in 1:1) for (i965 in 1:1) for (i966 in 1:1) for (i967 in 1:1) for (i968 in 1:1) for (i969 in 1:1) for (i970 in 1:1) for (i971 in 1:1) for (i972 in 1:1) for (i973 in 1:1) for (i974 in 1:1) for (i975 in 1:1) for (i976 in 1:1) for (i977 in 1:1) for (i978 in 1:1) for (i979 in 1:1) for (i980 in 1:1) for (i981 in 1:1) for (i982 in 1:1) for (i983 in 1:1) for (i984 in 1:1) for (i985 in 1:1) for (i986 in 1:1) for (i987 in 1:1) for (i988 in 1:1) for (i989 in 1:1) for (i990 in 1:1) for (i991 in 1:1) for (i992 in 1:1) for (i993 in 1:1) for (i994 in 1:1) for (i995 in 1:1) for (i996 in 1:1) for (i997 in 1:1) for (i998 in 1:1) for (i999 in 1:1) for (i1000 in 1:1) counter <- counter + 
    ##         1
    ##     return(counter)

``` r
cat("...\n")
```

    ## ...

``` r
cat("Last 5 lines of the parsed AST:\n")
```

    ## Last 5 lines of the parsed AST:

``` r
cat(paste(func_str[(length(func_str)-4):length(func_str)], collapse = "\n"), "\n\n")
```

    ##     counter <- 0
    ##     for (i1 in 1:1) for (i2 in 1:1) for (i3 in 1:1) for (i4 in 1:1) for (i5 in 1:1) for (i6 in 1:1) for (i7 in 1:1) for (i8 in 1:1) for (i9 in 1:1) for (i10 in 1:1) for (i11 in 1:1) for (i12 in 1:1) for (i13 in 1:1) for (i14 in 1:1) for (i15 in 1:1) for (i16 in 1:1) for (i17 in 1:1) for (i18 in 1:1) for (i19 in 1:1) for (i20 in 1:1) for (i21 in 1:1) for (i22 in 1:1) for (i23 in 1:1) for (i24 in 1:1) for (i25 in 1:1) for (i26 in 1:1) for (i27 in 1:1) for (i28 in 1:1) for (i29 in 1:1) for (i30 in 1:1) for (i31 in 1:1) for (i32 in 1:1) for (i33 in 1:1) for (i34 in 1:1) for (i35 in 1:1) for (i36 in 1:1) for (i37 in 1:1) for (i38 in 1:1) for (i39 in 1:1) for (i40 in 1:1) for (i41 in 1:1) for (i42 in 1:1) for (i43 in 1:1) for (i44 in 1:1) for (i45 in 1:1) for (i46 in 1:1) for (i47 in 1:1) for (i48 in 1:1) for (i49 in 1:1) for (i50 in 1:1) for (i51 in 1:1) for (i52 in 1:1) for (i53 in 1:1) for (i54 in 1:1) for (i55 in 1:1) for (i56 in 1:1) for (i57 in 1:1) for (i58 in 1:1) for (i59 in 1:1) for (i60 in 1:1) for (i61 in 1:1) for (i62 in 1:1) for (i63 in 1:1) for (i64 in 1:1) for (i65 in 1:1) for (i66 in 1:1) for (i67 in 1:1) for (i68 in 1:1) for (i69 in 1:1) for (i70 in 1:1) for (i71 in 1:1) for (i72 in 1:1) for (i73 in 1:1) for (i74 in 1:1) for (i75 in 1:1) for (i76 in 1:1) for (i77 in 1:1) for (i78 in 1:1) for (i79 in 1:1) for (i80 in 1:1) for (i81 in 1:1) for (i82 in 1:1) for (i83 in 1:1) for (i84 in 1:1) for (i85 in 1:1) for (i86 in 1:1) for (i87 in 1:1) for (i88 in 1:1) for (i89 in 1:1) for (i90 in 1:1) for (i91 in 1:1) for (i92 in 1:1) for (i93 in 1:1) for (i94 in 1:1) for (i95 in 1:1) for (i96 in 1:1) for (i97 in 1:1) for (i98 in 1:1) for (i99 in 1:1) for (i100 in 1:1) for (i101 in 1:1) for (i102 in 1:1) for (i103 in 1:1) for (i104 in 1:1) for (i105 in 1:1) for (i106 in 1:1) for (i107 in 1:1) for (i108 in 1:1) for (i109 in 1:1) for (i110 in 1:1) for (i111 in 1:1) for (i112 in 1:1) for (i113 in 1:1) for (i114 in 1:1) for (i115 in 1:1) for (i116 in 1:1) for (i117 in 1:1) for (i118 in 1:1) for (i119 in 1:1) for (i120 in 1:1) for (i121 in 1:1) for (i122 in 1:1) for (i123 in 1:1) for (i124 in 1:1) for (i125 in 1:1) for (i126 in 1:1) for (i127 in 1:1) for (i128 in 1:1) for (i129 in 1:1) for (i130 in 1:1) for (i131 in 1:1) for (i132 in 1:1) for (i133 in 1:1) for (i134 in 1:1) for (i135 in 1:1) for (i136 in 1:1) for (i137 in 1:1) for (i138 in 1:1) for (i139 in 1:1) for (i140 in 1:1) for (i141 in 1:1) for (i142 in 1:1) for (i143 in 1:1) for (i144 in 1:1) for (i145 in 1:1) for (i146 in 1:1) for (i147 in 1:1) for (i148 in 1:1) for (i149 in 1:1) for (i150 in 1:1) for (i151 in 1:1) for (i152 in 1:1) for (i153 in 1:1) for (i154 in 1:1) for (i155 in 1:1) for (i156 in 1:1) for (i157 in 1:1) for (i158 in 1:1) for (i159 in 1:1) for (i160 in 1:1) for (i161 in 1:1) for (i162 in 1:1) for (i163 in 1:1) for (i164 in 1:1) for (i165 in 1:1) for (i166 in 1:1) for (i167 in 1:1) for (i168 in 1:1) for (i169 in 1:1) for (i170 in 1:1) for (i171 in 1:1) for (i172 in 1:1) for (i173 in 1:1) for (i174 in 1:1) for (i175 in 1:1) for (i176 in 1:1) for (i177 in 1:1) for (i178 in 1:1) for (i179 in 1:1) for (i180 in 1:1) for (i181 in 1:1) for (i182 in 1:1) for (i183 in 1:1) for (i184 in 1:1) for (i185 in 1:1) for (i186 in 1:1) for (i187 in 1:1) for (i188 in 1:1) for (i189 in 1:1) for (i190 in 1:1) for (i191 in 1:1) for (i192 in 1:1) for (i193 in 1:1) for (i194 in 1:1) for (i195 in 1:1) for (i196 in 1:1) for (i197 in 1:1) for (i198 in 1:1) for (i199 in 1:1) for (i200 in 1:1) for (i201 in 1:1) for (i202 in 1:1) for (i203 in 1:1) for (i204 in 1:1) for (i205 in 1:1) for (i206 in 1:1) for (i207 in 1:1) for (i208 in 1:1) for (i209 in 1:1) for (i210 in 1:1) for (i211 in 1:1) for (i212 in 1:1) for (i213 in 1:1) for (i214 in 1:1) for (i215 in 1:1) for (i216 in 1:1) for (i217 in 1:1) for (i218 in 1:1) for (i219 in 1:1) for (i220 in 1:1) for (i221 in 1:1) for (i222 in 1:1) for (i223 in 1:1) for (i224 in 1:1) for (i225 in 1:1) for (i226 in 1:1) for (i227 in 1:1) for (i228 in 1:1) for (i229 in 1:1) for (i230 in 1:1) for (i231 in 1:1) for (i232 in 1:1) for (i233 in 1:1) for (i234 in 1:1) for (i235 in 1:1) for (i236 in 1:1) for (i237 in 1:1) for (i238 in 1:1) for (i239 in 1:1) for (i240 in 1:1) for (i241 in 1:1) for (i242 in 1:1) for (i243 in 1:1) for (i244 in 1:1) for (i245 in 1:1) for (i246 in 1:1) for (i247 in 1:1) for (i248 in 1:1) for (i249 in 1:1) for (i250 in 1:1) for (i251 in 1:1) for (i252 in 1:1) for (i253 in 1:1) for (i254 in 1:1) for (i255 in 1:1) for (i256 in 1:1) for (i257 in 1:1) for (i258 in 1:1) for (i259 in 1:1) for (i260 in 1:1) for (i261 in 1:1) for (i262 in 1:1) for (i263 in 1:1) for (i264 in 1:1) for (i265 in 1:1) for (i266 in 1:1) for (i267 in 1:1) for (i268 in 1:1) for (i269 in 1:1) for (i270 in 1:1) for (i271 in 1:1) for (i272 in 1:1) for (i273 in 1:1) for (i274 in 1:1) for (i275 in 1:1) for (i276 in 1:1) for (i277 in 1:1) for (i278 in 1:1) for (i279 in 1:1) for (i280 in 1:1) for (i281 in 1:1) for (i282 in 1:1) for (i283 in 1:1) for (i284 in 1:1) for (i285 in 1:1) for (i286 in 1:1) for (i287 in 1:1) for (i288 in 1:1) for (i289 in 1:1) for (i290 in 1:1) for (i291 in 1:1) for (i292 in 1:1) for (i293 in 1:1) for (i294 in 1:1) for (i295 in 1:1) for (i296 in 1:1) for (i297 in 1:1) for (i298 in 1:1) for (i299 in 1:1) for (i300 in 1:1) for (i301 in 1:1) for (i302 in 1:1) for (i303 in 1:1) for (i304 in 1:1) for (i305 in 1:1) for (i306 in 1:1) for (i307 in 1:1) for (i308 in 1:1) for (i309 in 1:1) for (i310 in 1:1) for (i311 in 1:1) for (i312 in 1:1) for (i313 in 1:1) for (i314 in 1:1) for (i315 in 1:1) for (i316 in 1:1) for (i317 in 1:1) for (i318 in 1:1) for (i319 in 1:1) for (i320 in 1:1) for (i321 in 1:1) for (i322 in 1:1) for (i323 in 1:1) for (i324 in 1:1) for (i325 in 1:1) for (i326 in 1:1) for (i327 in 1:1) for (i328 in 1:1) for (i329 in 1:1) for (i330 in 1:1) for (i331 in 1:1) for (i332 in 1:1) for (i333 in 1:1) for (i334 in 1:1) for (i335 in 1:1) for (i336 in 1:1) for (i337 in 1:1) for (i338 in 1:1) for (i339 in 1:1) for (i340 in 1:1) for (i341 in 1:1) for (i342 in 1:1) for (i343 in 1:1) for (i344 in 1:1) for (i345 in 1:1) for (i346 in 1:1) for (i347 in 1:1) for (i348 in 1:1) for (i349 in 1:1) for (i350 in 1:1) for (i351 in 1:1) for (i352 in 1:1) for (i353 in 1:1) for (i354 in 1:1) for (i355 in 1:1) for (i356 in 1:1) for (i357 in 1:1) for (i358 in 1:1) for (i359 in 1:1) for (i360 in 1:1) for (i361 in 1:1) for (i362 in 1:1) for (i363 in 1:1) for (i364 in 1:1) for (i365 in 1:1) for (i366 in 1:1) for (i367 in 1:1) for (i368 in 1:1) for (i369 in 1:1) for (i370 in 1:1) for (i371 in 1:1) for (i372 in 1:1) for (i373 in 1:1) for (i374 in 1:1) for (i375 in 1:1) for (i376 in 1:1) for (i377 in 1:1) for (i378 in 1:1) for (i379 in 1:1) for (i380 in 1:1) for (i381 in 1:1) for (i382 in 1:1) for (i383 in 1:1) for (i384 in 1:1) for (i385 in 1:1) for (i386 in 1:1) for (i387 in 1:1) for (i388 in 1:1) for (i389 in 1:1) for (i390 in 1:1) for (i391 in 1:1) for (i392 in 1:1) for (i393 in 1:1) for (i394 in 1:1) for (i395 in 1:1) for (i396 in 1:1) for (i397 in 1:1) for (i398 in 1:1) for (i399 in 1:1) for (i400 in 1:1) for (i401 in 1:1) for (i402 in 1:1) for (i403 in 1:1) for (i404 in 1:1) for (i405 in 1:1) for (i406 in 1:1) for (i407 in 1:1) for (i408 in 1:1) for (i409 in 1:1) for (i410 in 1:1) for (i411 in 1:1) for (i412 in 1:1) for (i413 in 1:1) for (i414 in 1:1) for (i415 in 1:1) for (i416 in 1:1) for (i417 in 1:1) for (i418 in 1:1) for (i419 in 1:1) for (i420 in 1:1) for (i421 in 1:1) for (i422 in 1:1) for (i423 in 1:1) for (i424 in 1:1) for (i425 in 1:1) for (i426 in 1:1) for (i427 in 1:1) for (i428 in 1:1) for (i429 in 1:1) for (i430 in 1:1) for (i431 in 1:1) for (i432 in 1:1) for (i433 in 1:1) for (i434 in 1:1) for (i435 in 1:1) for (i436 in 1:1) for (i437 in 1:1) for (i438 in 1:1) for (i439 in 1:1) for (i440 in 1:1) for (i441 in 1:1) for (i442 in 1:1) for (i443 in 1:1) for (i444 in 1:1) for (i445 in 1:1) for (i446 in 1:1) for (i447 in 1:1) for (i448 in 1:1) for (i449 in 1:1) for (i450 in 1:1) for (i451 in 1:1) for (i452 in 1:1) for (i453 in 1:1) for (i454 in 1:1) for (i455 in 1:1) for (i456 in 1:1) for (i457 in 1:1) for (i458 in 1:1) for (i459 in 1:1) for (i460 in 1:1) for (i461 in 1:1) for (i462 in 1:1) for (i463 in 1:1) for (i464 in 1:1) for (i465 in 1:1) for (i466 in 1:1) for (i467 in 1:1) for (i468 in 1:1) for (i469 in 1:1) for (i470 in 1:1) for (i471 in 1:1) for (i472 in 1:1) for (i473 in 1:1) for (i474 in 1:1) for (i475 in 1:1) for (i476 in 1:1) for (i477 in 1:1) for (i478 in 1:1) for (i479 in 1:1) for (i480 in 1:1) for (i481 in 1:1) for (i482 in 1:1) for (i483 in 1:1) for (i484 in 1:1) for (i485 in 1:1) for (i486 in 1:1) for (i487 in 1:1) for (i488 in 1:1) for (i489 in 1:1) for (i490 in 1:1) for (i491 in 1:1) for (i492 in 1:1) for (i493 in 1:1) for (i494 in 1:1) for (i495 in 1:1) for (i496 in 1:1) for (i497 in 1:1) for (i498 in 1:1) for (i499 in 1:1) for (i500 in 1:1) for (i501 in 1:1) for (i502 in 1:1) for (i503 in 1:1) for (i504 in 1:1) for (i505 in 1:1) for (i506 in 1:1) for (i507 in 1:1) for (i508 in 1:1) for (i509 in 1:1) for (i510 in 1:1) for (i511 in 1:1) for (i512 in 1:1) for (i513 in 1:1) for (i514 in 1:1) for (i515 in 1:1) for (i516 in 1:1) for (i517 in 1:1) for (i518 in 1:1) for (i519 in 1:1) for (i520 in 1:1) for (i521 in 1:1) for (i522 in 1:1) for (i523 in 1:1) for (i524 in 1:1) for (i525 in 1:1) for (i526 in 1:1) for (i527 in 1:1) for (i528 in 1:1) for (i529 in 1:1) for (i530 in 1:1) for (i531 in 1:1) for (i532 in 1:1) for (i533 in 1:1) for (i534 in 1:1) for (i535 in 1:1) for (i536 in 1:1) for (i537 in 1:1) for (i538 in 1:1) for (i539 in 1:1) for (i540 in 1:1) for (i541 in 1:1) for (i542 in 1:1) for (i543 in 1:1) for (i544 in 1:1) for (i545 in 1:1) for (i546 in 1:1) for (i547 in 1:1) for (i548 in 1:1) for (i549 in 1:1) for (i550 in 1:1) for (i551 in 1:1) for (i552 in 1:1) for (i553 in 1:1) for (i554 in 1:1) for (i555 in 1:1) for (i556 in 1:1) for (i557 in 1:1) for (i558 in 1:1) for (i559 in 1:1) for (i560 in 1:1) for (i561 in 1:1) for (i562 in 1:1) for (i563 in 1:1) for (i564 in 1:1) for (i565 in 1:1) for (i566 in 1:1) for (i567 in 1:1) for (i568 in 1:1) for (i569 in 1:1) for (i570 in 1:1) for (i571 in 1:1) for (i572 in 1:1) for (i573 in 1:1) for (i574 in 1:1) for (i575 in 1:1) for (i576 in 1:1) for (i577 in 1:1) for (i578 in 1:1) for (i579 in 1:1) for (i580 in 1:1) for (i581 in 1:1) for (i582 in 1:1) for (i583 in 1:1) for (i584 in 1:1) for (i585 in 1:1) for (i586 in 1:1) for (i587 in 1:1) for (i588 in 1:1) for (i589 in 1:1) for (i590 in 1:1) for (i591 in 1:1) for (i592 in 1:1) for (i593 in 1:1) for (i594 in 1:1) for (i595 in 1:1) for (i596 in 1:1) for (i597 in 1:1) for (i598 in 1:1) for (i599 in 1:1) for (i600 in 1:1) for (i601 in 1:1) for (i602 in 1:1) for (i603 in 1:1) for (i604 in 1:1) for (i605 in 1:1) for (i606 in 1:1) for (i607 in 1:1) for (i608 in 1:1) for (i609 in 1:1) for (i610 in 1:1) for (i611 in 1:1) for (i612 in 1:1) for (i613 in 1:1) for (i614 in 1:1) for (i615 in 1:1) for (i616 in 1:1) for (i617 in 1:1) for (i618 in 1:1) for (i619 in 1:1) for (i620 in 1:1) for (i621 in 1:1) for (i622 in 1:1) for (i623 in 1:1) for (i624 in 1:1) for (i625 in 1:1) for (i626 in 1:1) for (i627 in 1:1) for (i628 in 1:1) for (i629 in 1:1) for (i630 in 1:1) for (i631 in 1:1) for (i632 in 1:1) for (i633 in 1:1) for (i634 in 1:1) for (i635 in 1:1) for (i636 in 1:1) for (i637 in 1:1) for (i638 in 1:1) for (i639 in 1:1) for (i640 in 1:1) for (i641 in 1:1) for (i642 in 1:1) for (i643 in 1:1) for (i644 in 1:1) for (i645 in 1:1) for (i646 in 1:1) for (i647 in 1:1) for (i648 in 1:1) for (i649 in 1:1) for (i650 in 1:1) for (i651 in 1:1) for (i652 in 1:1) for (i653 in 1:1) for (i654 in 1:1) for (i655 in 1:1) for (i656 in 1:1) for (i657 in 1:1) for (i658 in 1:1) for (i659 in 1:1) for (i660 in 1:1) for (i661 in 1:1) for (i662 in 1:1) for (i663 in 1:1) for (i664 in 1:1) for (i665 in 1:1) for (i666 in 1:1) for (i667 in 1:1) for (i668 in 1:1) for (i669 in 1:1) for (i670 in 1:1) for (i671 in 1:1) for (i672 in 1:1) for (i673 in 1:1) for (i674 in 1:1) for (i675 in 1:1) for (i676 in 1:1) for (i677 in 1:1) for (i678 in 1:1) for (i679 in 1:1) for (i680 in 1:1) for (i681 in 1:1) for (i682 in 1:1) for (i683 in 1:1) for (i684 in 1:1) for (i685 in 1:1) for (i686 in 1:1) for (i687 in 1:1) for (i688 in 1:1) for (i689 in 1:1) for (i690 in 1:1) for (i691 in 1:1) for (i692 in 1:1) for (i693 in 1:1) for (i694 in 1:1) for (i695 in 1:1) for (i696 in 1:1) for (i697 in 1:1) for (i698 in 1:1) for (i699 in 1:1) for (i700 in 1:1) for (i701 in 1:1) for (i702 in 1:1) for (i703 in 1:1) for (i704 in 1:1) for (i705 in 1:1) for (i706 in 1:1) for (i707 in 1:1) for (i708 in 1:1) for (i709 in 1:1) for (i710 in 1:1) for (i711 in 1:1) for (i712 in 1:1) for (i713 in 1:1) for (i714 in 1:1) for (i715 in 1:1) for (i716 in 1:1) for (i717 in 1:1) for (i718 in 1:1) for (i719 in 1:1) for (i720 in 1:1) for (i721 in 1:1) for (i722 in 1:1) for (i723 in 1:1) for (i724 in 1:1) for (i725 in 1:1) for (i726 in 1:1) for (i727 in 1:1) for (i728 in 1:1) for (i729 in 1:1) for (i730 in 1:1) for (i731 in 1:1) for (i732 in 1:1) for (i733 in 1:1) for (i734 in 1:1) for (i735 in 1:1) for (i736 in 1:1) for (i737 in 1:1) for (i738 in 1:1) for (i739 in 1:1) for (i740 in 1:1) for (i741 in 1:1) for (i742 in 1:1) for (i743 in 1:1) for (i744 in 1:1) for (i745 in 1:1) for (i746 in 1:1) for (i747 in 1:1) for (i748 in 1:1) for (i749 in 1:1) for (i750 in 1:1) for (i751 in 1:1) for (i752 in 1:1) for (i753 in 1:1) for (i754 in 1:1) for (i755 in 1:1) for (i756 in 1:1) for (i757 in 1:1) for (i758 in 1:1) for (i759 in 1:1) for (i760 in 1:1) for (i761 in 1:1) for (i762 in 1:1) for (i763 in 1:1) for (i764 in 1:1) for (i765 in 1:1) for (i766 in 1:1) for (i767 in 1:1) for (i768 in 1:1) for (i769 in 1:1) for (i770 in 1:1) for (i771 in 1:1) for (i772 in 1:1) for (i773 in 1:1) for (i774 in 1:1) for (i775 in 1:1) for (i776 in 1:1) for (i777 in 1:1) for (i778 in 1:1) for (i779 in 1:1) for (i780 in 1:1) for (i781 in 1:1) for (i782 in 1:1) for (i783 in 1:1) for (i784 in 1:1) for (i785 in 1:1) for (i786 in 1:1) for (i787 in 1:1) for (i788 in 1:1) for (i789 in 1:1) for (i790 in 1:1) for (i791 in 1:1) for (i792 in 1:1) for (i793 in 1:1) for (i794 in 1:1) for (i795 in 1:1) for (i796 in 1:1) for (i797 in 1:1) for (i798 in 1:1) for (i799 in 1:1) for (i800 in 1:1) for (i801 in 1:1) for (i802 in 1:1) for (i803 in 1:1) for (i804 in 1:1) for (i805 in 1:1) for (i806 in 1:1) for (i807 in 1:1) for (i808 in 1:1) for (i809 in 1:1) for (i810 in 1:1) for (i811 in 1:1) for (i812 in 1:1) for (i813 in 1:1) for (i814 in 1:1) for (i815 in 1:1) for (i816 in 1:1) for (i817 in 1:1) for (i818 in 1:1) for (i819 in 1:1) for (i820 in 1:1) for (i821 in 1:1) for (i822 in 1:1) for (i823 in 1:1) for (i824 in 1:1) for (i825 in 1:1) for (i826 in 1:1) for (i827 in 1:1) for (i828 in 1:1) for (i829 in 1:1) for (i830 in 1:1) for (i831 in 1:1) for (i832 in 1:1) for (i833 in 1:1) for (i834 in 1:1) for (i835 in 1:1) for (i836 in 1:1) for (i837 in 1:1) for (i838 in 1:1) for (i839 in 1:1) for (i840 in 1:1) for (i841 in 1:1) for (i842 in 1:1) for (i843 in 1:1) for (i844 in 1:1) for (i845 in 1:1) for (i846 in 1:1) for (i847 in 1:1) for (i848 in 1:1) for (i849 in 1:1) for (i850 in 1:1) for (i851 in 1:1) for (i852 in 1:1) for (i853 in 1:1) for (i854 in 1:1) for (i855 in 1:1) for (i856 in 1:1) for (i857 in 1:1) for (i858 in 1:1) for (i859 in 1:1) for (i860 in 1:1) for (i861 in 1:1) for (i862 in 1:1) for (i863 in 1:1) for (i864 in 1:1) for (i865 in 1:1) for (i866 in 1:1) for (i867 in 1:1) for (i868 in 1:1) for (i869 in 1:1) for (i870 in 1:1) for (i871 in 1:1) for (i872 in 1:1) for (i873 in 1:1) for (i874 in 1:1) for (i875 in 1:1) for (i876 in 1:1) for (i877 in 1:1) for (i878 in 1:1) for (i879 in 1:1) for (i880 in 1:1) for (i881 in 1:1) for (i882 in 1:1) for (i883 in 1:1) for (i884 in 1:1) for (i885 in 1:1) for (i886 in 1:1) for (i887 in 1:1) for (i888 in 1:1) for (i889 in 1:1) for (i890 in 1:1) for (i891 in 1:1) for (i892 in 1:1) for (i893 in 1:1) for (i894 in 1:1) for (i895 in 1:1) for (i896 in 1:1) for (i897 in 1:1) for (i898 in 1:1) for (i899 in 1:1) for (i900 in 1:1) for (i901 in 1:1) for (i902 in 1:1) for (i903 in 1:1) for (i904 in 1:1) for (i905 in 1:1) for (i906 in 1:1) for (i907 in 1:1) for (i908 in 1:1) for (i909 in 1:1) for (i910 in 1:1) for (i911 in 1:1) for (i912 in 1:1) for (i913 in 1:1) for (i914 in 1:1) for (i915 in 1:1) for (i916 in 1:1) for (i917 in 1:1) for (i918 in 1:1) for (i919 in 1:1) for (i920 in 1:1) for (i921 in 1:1) for (i922 in 1:1) for (i923 in 1:1) for (i924 in 1:1) for (i925 in 1:1) for (i926 in 1:1) for (i927 in 1:1) for (i928 in 1:1) for (i929 in 1:1) for (i930 in 1:1) for (i931 in 1:1) for (i932 in 1:1) for (i933 in 1:1) for (i934 in 1:1) for (i935 in 1:1) for (i936 in 1:1) for (i937 in 1:1) for (i938 in 1:1) for (i939 in 1:1) for (i940 in 1:1) for (i941 in 1:1) for (i942 in 1:1) for (i943 in 1:1) for (i944 in 1:1) for (i945 in 1:1) for (i946 in 1:1) for (i947 in 1:1) for (i948 in 1:1) for (i949 in 1:1) for (i950 in 1:1) for (i951 in 1:1) for (i952 in 1:1) for (i953 in 1:1) for (i954 in 1:1) for (i955 in 1:1) for (i956 in 1:1) for (i957 in 1:1) for (i958 in 1:1) for (i959 in 1:1) for (i960 in 1:1) for (i961 in 1:1) for (i962 in 1:1) for (i963 in 1:1) for (i964 in 1:1) for (i965 in 1:1) for (i966 in 1:1) for (i967 in 1:1) for (i968 in 1:1) for (i969 in 1:1) for (i970 in 1:1) for (i971 in 1:1) for (i972 in 1:1) for (i973 in 1:1) for (i974 in 1:1) for (i975 in 1:1) for (i976 in 1:1) for (i977 in 1:1) for (i978 in 1:1) for (i979 in 1:1) for (i980 in 1:1) for (i981 in 1:1) for (i982 in 1:1) for (i983 in 1:1) for (i984 in 1:1) for (i985 in 1:1) for (i986 in 1:1) for (i987 in 1:1) for (i988 in 1:1) for (i989 in 1:1) for (i990 in 1:1) for (i991 in 1:1) for (i992 in 1:1) for (i993 in 1:1) for (i994 in 1:1) for (i995 in 1:1) for (i996 in 1:1) for (i997 in 1:1) for (i998 in 1:1) for (i999 in 1:1) for (i1000 in 1:1) counter <- counter + 
    ##         1
    ##     return(counter)
    ## }

``` r
# Show the AST object size
cat("AST object information:\n")
```

    ## AST object information:

``` r
cat("- Object class:", class(body(thousand_loops_func)), "\n")
```

    ## - Object class: {

``` r
cat("- Object size:", format(object.size(thousand_loops_func), units = "Kb"), "\n")
```

    ## - Object size: 657.7 Kb

``` r
cat("- Nesting depth: 1000 levels\n")
```

    ## - Nesting depth: 1000 levels

``` r
cat("- Variable names: i1, i2, i3, ..., i1000\n\n")
```

    ## - Variable names: i1, i2, i3, ..., i1000

``` r
# Run it and time it
cat("Now executing the 1000-nested-loops monster...\n")
```

    ## Now executing the 1000-nested-loops monster...

``` r
start_time <- Sys.time()
thousand_loops_result <- thousand_loops_func()
end_time <- Sys.time()

cat("Result: ", thousand_loops_result, " iterations\n")
```

    ## Result:  1  iterations

``` r
cat("Expected: ", 1^1000, " iterations (since each loop runs once)\n")
```

    ## Expected:  1  iterations (since each loop runs once)

``` r
cat("Execution time: ", round(as.numeric(end_time - start_time, units = "secs") * 1000, 2), " milliseconds\n")
```

    ## Execution time:  6.62  milliseconds

``` r
cat("Function call stack depth: 1000 levels deep!\n")
```

    ## Function call stack depth: 1000 levels deep!

``` r
cat("\nSuccess! We just executed a function with 1000 nested for loops! 🤯\n")
```

    ## 
    ## Success! We just executed a function with 1000 nested for loops! 🤯

``` r
cat("This demonstrates R's incredible AST manipulation capabilities! 🎉\n")
```

    ## This demonstrates R's incredible AST manipulation capabilities! 🎉

``` r
cat("R's homoiconic nature makes this kind of metaprogramming not just possible, but elegant!\n")
```

    ## R's homoiconic nature makes this kind of metaprogramming not just possible, but elegant!

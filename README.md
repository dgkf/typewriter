
# `typewriter`

An R package for runtime type-checking

## :scientist: Status: **Mad Science** :scientist:

This project is a thought exercise and syntactic experiment, trying to
cobble some typing syntax into R. It is not guaranteed to be robust, or
to be well vetted, but it does offer an introduction to type systems and
concepts for people familiar with R.

For anything beyond educational use, use with caution.

## :pencil2: Tutorial

Since type systems can seem rather foreign in R, let’s try to slowly
build up some motivating examples to show what value type systems add
when developing R code:

### Adding types to a function

Fundamentally, type systems are designed to impose some limits on what
types of data your function accepts. Practically all code assumes
*something* about your inputs, and type systems provide a way of
articulating those assumptions.

Specify types in the function header using `param = default :type`
syntax.

``` r
add <- type(function(x = 0 :numeric, y = 0 :numeric) {
  x + y
})

add(3, 14)
```

    ## [1] 17

And if we try to give it something that isn’t numeric, this is what we
see:

``` r
add(3, "not a numeric")
```

``` diff
- Error: Type of parameter 'y' does not match signature `numeric`
```

### Using `%@%` to decorate a function with type checking

Since it can be a bit annoying to wrap functions like this, you can also
use the `%@%` operator (styled after python’s `@` decorator syntax).

> Functionally, this is a “reverse pipe”

``` r
add <- type %@% function(x = 0 :numeric, y = 0 :numeric) {
  x + y
}
```

We’ll use this style for the rest of the examples.

### Adding “traits”

R doesn’t really have a concept of traits - behaviors that are defined
over groups of types. Testing for behavior, instead of defining
behaviors by type is typically referred to as an “interface”. To do this
in R, we can repurpose functions that check behaviors of R objects. This
includes many `is.*` or `has_*` functions like `is.numeric` or
`is.finite`.

``` r
add_finite <- type %@% function(
  x = 0 :numeric(is.finite),
  y = 0 :numeric(is.finite)
) {
  x + y
}

add_finite(1, 2)
```

    ## [1] 3

``` r
add_finite(1, NA)
```

``` diff
- Error: Type of parameter 'y' does not match signature `numeric(is.finite)`
```

Since we’re repurposing functions that probably weren’t intended to be
used as traits, we might need some extra flexibility to constrain their
output. For example, we might want to limit our adding function to only
operate on vectors with a specific length.

``` r
add_numeric_vec <- type %@% function(
  x = 0 :numeric(length=10), 
  y = 0 :numeric(length=10)
) {
  x + y
}

add_numeric_vec(1:10, 11:20)
```

    ##  [1] 12 14 16 18 20 22 24 26 28 30

### Parameterized Types

Now, implementing an adding function for each length of vector would be
pretty tedious. What we really want, is to make sure that both input
vectors have the *same* length, whatever that might be. For that we can
use type parameters.

``` r
add_numeric_vec <- type(N) %@% function(
  x = 0 :numeric(length=N), 
  y = 0 :numeric(length=N)
) {
  x + y
}

add_numeric_vec(1:5, 2:6)
```

    ## [1]  3  5  7  9 11

``` r
add_numeric_vec(1:5, 1:10)
```

``` diff
- Error: Type parameter 'N' not satisfied
```

### Generic Type

Finally, limiting our function so that it only applies to numeric types
might limit the use of other vectors that can be added, such as
`complex` or `difftime`. If all we need is for the type to implement the
`+` operator, then why should we need to re-write our function for each
type of vector?

For this, we want a ***Generic*** type.

``` r
add_vec <- type(T, N) %@% function(
  x = 0 :T(length=N), 
  y = 0 :T(length=N)
) {
  x + y
}

add_vec(1:5, 2:6)
```

    ## [1]  3  5  7  9 11

``` r
add_vec(complex(1:3, 2:4), complex(3:5, 4:6))
```

    ## [1]  6+0i  8+0i 10+0i

Cool! Our function is now *Generic*, only assuming that both inputs are
the same type and have the same length.

We can see how our function is different from `+` by looking at the
behavior when two different types are provided. `complex` and `numeric`
vectors *can* be added together.

``` r
complex(3, 1:3) + 3:5
```

    ## [1] 4+0i 6+0i 8+0i

But they fail when passed to our `add_vec` function because both inputs
must have the same type. Even though our function is *Generic* over
input types, it still asserts constraints over the relationships between
inputs.

``` r
add_vec(complex(3, 1:3), 3:5)
```

``` diff
- Error: Type parameter 'T' not satisfied
```

### Return Types (work in progress :construction:)

Just like function parameters, return types can be specified. The
biggest benefit to defining a return type is that a compiler or static
type checker can start to reason about the types of your variables.
Until more type inferencing is done based on return types, those
benefits are not going to be realized, but that isn’t going to stop us
from implementing it!

``` r
add_vec <- type(T, N, return :T) %@% function(
  x = 0 :T(length=N),
  y = 0 :T(length=N)
) {
  x + y
}

add_vec(1:5, 2:6)
```

### Putting it all together

Many of the tools implemented here can be mixed and matched. For a full
break-down of all the syntax that is supported, take a look at
`?type_match`, the function that handles all the behind-the-scenes type
checking. For now, let’s just take a look at some more involved
examples.

#### `replace_similar`

Let’s define a function for replacing elements in a list, but only when
the new values have the same type, so that we don’t modify the overall
structure of our list.

``` r
replace_similar <- type(T, N) %@% function(
  df  = .:list[[at :T(length=N)]],
  at  = .:character(length=1),
  vec = .:T(length=N)
) {
  df[[at]] <- vec
  df
}
```

Let’s give it a go

``` r
example_data <- list(a = 1:3, b = factor(c("a", "b")))
replace_similar(example_data, "b", factor(c("c", "d")))
```

    ## $a
    ## [1] 1 2 3
    ## 
    ## $b
    ## [1] c d
    ## Levels: c d

This seems to work, but our factor levels *did* change. This highlights
one of the challenges of type systems in a dynamic language. What
exactly *is* the type of *factor* anyways? Depending on how we want to
draw this type-shaped box around dynamically typed amorphous blob, we
*could* check that the attributes are the same, but that may be overly
specific for other data types. Since there’s no concensus, types in
dynamic languages will inevitably be a bit loose.

Enough with the caveats, though - let’s move on to more examples:

``` r
mtmini <- mtcars[1:3, c("wt", "cyl")]
replace_similar(mtmini, "cyl", c(4, 5, 2))
```

    ##                  wt cyl
    ## Mazda RX4     2.620   4
    ## Mazda RX4 Wag 2.875   5
    ## Datsun 710    2.320   2

Because a `data.frame` is just a `list` with style, our function works
just as well with a `data.frame` object. Even a `tibble` would work
here. What differentiates these objects isn’t necessarily their type,
but the interfaces that they implement and how they implement them.

You may have also noticed that the `at` argument is actually used as
part of the type definition for the `df` param. This is some black magic
wizardry that can really only happen because we’re doing run-time type
checking. A type checking algorithm would have to be pretty savvy to do
these sorts of checks in static analysis (before the code is executed).

## Other features

### Type Unions

Type definitions can also be unions using the `|` operator as a
separator.

``` r
f <- type %@% function(x = .:numeric|complex) {
  mean(x)
}
```

### Indexed or Infixed Types

You can also apply type constraints to destructured elements. To type
check every element in a list you can check against `list[type]`, or for
a specific element, you can type check `list[["at" :type]]`. Likewise,
you can specify that the result of an infix function result using
`rlang`-style lambda syntax such as `list(.$a :numeric)`, or an
alternative shorthand `list(a$numeric)`.

### Nested Structures

Type definitions may also define nested structures. Most likely, this
type of complex structure would be better checked by defining your own
“interface” function. Nevertheless, the checking is there if you want
it.

``` r
f <- type %@% function(
  t = .:list[["a" :list(length=3)[numeric], "b" :numeric]]
) {  
  sum(unlist(t$a), t$b)
}

f(list(a = list(1, 2, 3), b = c(4, 5, 6)))
```

## Further work

As it is now, this project is primarily just a playground for some
syntax exploration and language design musings. Depending on how it
grows, there are a few features that could use some attention (in order
of likelyhood of actually getting implemented):

-   [ ] **Better type bounds**:  
    Instead of repeatedly applying the same type bounds anywhere where a
    T appears, it would be more convenient to constrain T in the type
    definition.

    ``` r
    replace_similar <- type(T(length=N)) %@% function(
      df  = .:list[[at :T]],
      at  = .:character(length=1),
      vec = .:T
    ) {
    }
    ```

-   [ ] **Better error messages**:  
    The error messages are pretty terse and nondescript right now. It
    wouldn’t be too heavy of a lift to output something more meaningful,
    especially for type parameters that failed to match where there’s
    some relational qualities that are expected.

-   [ ] **“Compiled” type checking**:  
    Currently, type checking does some gnarly non-standard evaluation,
    doing type checking by walking the type definition syntax tree with
    the input data each time the function is called. This introduces
    pretty considerable execution overhead. Instead, the checks could be
    “compiled” into assertions when the function is declared. This would
    make the code faster, and the function bodies mor intelligable.

    If there’s any interst in using this package beyond educational
    purposes, this would be the lowest hanging fruit to get it into
    reasonable shape for real-world use.

    In some cursory benchmarks comparing this run-time type analysis
    against hand-written assertions, compiled assertions ran \~3x
    faster. Naturally the gains are very dependent on the complexity of
    the type definitions.

-   [ ] **Respecting Non-standard Evaluation**  
    R’s non-standard evaluation throws a big wrench into this whole
    thing. There’s currently no good way to do type checking on
    arguments that aren’t intended to be evaluated. For example, there’s
    no good way to define a type such as:

    ``` r
    code_echo <- type %@% function(expr = .:expression[1: "{"]) {
    }
    ```

    Maybe that’s a good thing? Just leave it untyped if you plan to do
    something wild with it. Otherwise, I can see this being quite the
    rabbit hole to fall into.

-   [ ] **Return types**:  
    Without something that actually uses these types, return types
    aren’t particulary valuable. Nevertheless, what type of type system
    doesn’t have return types!

    Unless a tools is interested in making use of return types for some
    static type checking, this is quite low priority.

# Diamant

Diamant is a simple programming language. It's main feature is minimalism and simplicity.

## Language definition

### Code example

``` lua
-- declare a module that exports main function
module Test (main)
(
    -- declare main function
    fn main()
    (
        f1 = factr(10), -- assign value of factr(10) to variable
        f2 = factl(10)  -- assign value of factr(10) to variable
    ), -- comma separate every statement
    -- declare function with one parameter
    fn factr(n)
    (
        if (n == 1) ( 1 ) -- if n equals to 1 return 1
        else ( n * factr(n - 1)) -- otherwise return n * fact(n-1)
    ),
    fn factl(n)
    (
        i = 1,
        result = 1,
        -- loops while condition is true
        while (i < n || i == n)
        (
            result = result * i,
            i = i + 1
        )
    )
)
```
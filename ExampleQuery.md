### A new example query

We will walk through an example of implementing a simple noisy count algorithm
in Fuzz, and compile it with Orchard. The algorithm can be motivated by the
following scenario.

Suppose a smartphone company wants to conduct a survey on smartphone screen time
among its users, and count how many users are heavy smartphone users. For the
sake of this example, let's say a heavy smartphone user is someone who has more
than 5 hours of screen time per day.

The best way to reproduce these steps in this document is

1. start a docker shell for the provided image
2. multiplex the docker shell with `tmux` or `screen`
3. run `stack ghci` under the `cps-fuzz` directory in one session
4. copy the example code into `cps-fuzz/src/Examples.hs` in the provided Docker
   image
5. Run `:r` in the `stack ghci` session after every edit to reload the changes

#### Writing the query

To express this query in Fuzz, let us first define a predicate for heavy
users. We will later use this predicate as part of the sum operation to count
the number of heavy users, so we map users whose screen time is more than 5
hours to the numeric value `1.0`, and the other users to the numeric value `0.0`.

```haskell
isHeavyUser ::
  Name "screen_time" (CPSFuzz f Double) -> CPSFuzz f Double
isHeavyUser (N screenTimeHours) =
  if_ (screenTimeHours %> 5) 1.0 0.0
```

Next, we can count, sum and add noise to the total number of heavy users across
the entire userbase with the query

```haskell
noisyCountHeavyUsers ::
  CPSFuzz f (Bag Double) -> CPSFuzz f (Distr Double)
noisyCountHeavyUsers db =
  bmap isHeavyUser db $ \($(named "db_isHeavyUser")) ->
  bsum 1.0 db_isHeavyUser $ \($(named "sum")) ->
  lap 1.0 sum
```

The Fuzz implementation in Orchard is an embedded language inside the host
language Haskell. This implementation technique allows us to save much
engineering time by reusing the Haskell syntax and parser. The cost of doing so
is the heavy type annotations required for expressing Fuzz queries in Haskell.

The function `isHeavyUser` has the type `Name "screen_time" (CPSFuzz f Double)
-> CPSFuzz f Double`. The type constructor `Name s a` is a simple wrapper around
the inner type `a`, while annotating `a` with a statically chosen type-level
string `s`. This allows us to name values in embedded Fuzz, and these names are
used to produce somewhat human readable compiled code.

The inner type `CPSFuzz f Double` means this is a numeric value in the embedded
`Fuzz` language. The `CPS` prefix to `Fuzz` means this implementation of `Fuzz`
is in the continuation-passing style. This somewhat unusual design is used to
help compilation.

Putting these pieces together, the type of `isHeavyUser` can be pronounced as "a
function that receives a numeric value named screen_time, and produces another
numeric value".

The type for `noisyCountHeavyUsers` is `CPSFuzz f (Bag Double) -> CPSFuzz f
(Distr Double)`. This type similarly can be pronounced as "a function that
receives a bag of numeric values, and produces a distribution over numeric
values".

The implementation for `noisyCountHeavyUsers` first maps the predicate
`isHeavyUser` over the input database `db`, we give the result of this bag map
operation a name `db_isHeavyUser` through the macro combinator `$(named "...")`.
This map result is then passed to bag sum (`bsum`). The `bsum` operator takes 2
arguments: a clip bound, and a bag of values to add up. Here, clip bound is set
to `1.0`, and the bag of values to add up is `db_isHeavyUser`. Each value within
`db_isHeavyUser` is clipped first so that each value's magnitude is smaller than
or equal to the clip bound. In this particular case, since we know each value in
`db_isHeavyUser` is either `1.0` or `0.0`, clipping with bound `1.0` is
essentially a no-op. However, in general, clipping is required to ensure
differential privacy. The result of bag sum is given the name `sum`, similarly
through `$(named "...")`. Finally, we add laplacian noise with width `1.0` to
the sum.

#### Compiling the query

Having written the query `noisyCountHeavyUsers`, let's try to compile it. In the
`stack ghci` session, type the following three commands:

```
> :r
Ok, 11 modules loaded.
> let Right output = compileAndExtract "db" noisyCountHeavyUsers
> pExtraction output
import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner


def anonymous_fun_0_(empty_closure_0_):
  """
  empty_closure_0_: ()
  """
  def anonymous_fun_1_(screen_time_0_):
    """
    screen_time_0_: Double
    """
    if screen_time_0_ > 5.0:
      cond_result_0_ = 1.0
    else:
      cond_result_0_ = 0.0
    return cond_result_0_
  def anonymous_fun_2_(dbrow_0_):
    """
    dbrow_0_: Double
    """
    return dbrow_0_
  return fun_comp(anonymous_fun_1_
                 ,anonymous_fun_2_)
def anonymous_fun_3_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_4_(orange_input_0_):
    """
    orange_input_0_: Double
    """
    return laplace_fx(cfix(1.0)
                     ,orange_input_0_)
  return anonymous_fun_4_
bmcs(1
    ,[1.0]
    ,()
    ,anonymous_fun_0_
    ,()
    ,anonymous_fun_3_)
```

The first command `:r` reloads changes to the `cps-fuzz/src/Examples.hs` file.

The second command uses the Orchard compilation procedure `compileAndExtract` to
compile the query. The string `"db"` is a name we give to the overall input
database. The compiled code representation is bound to the variable `output`.

Finally, we use the function `pExtraction` to prettyprint the compiled code in
`output`. The compiled code is in the Python3 MAMBA-MPC syntax.

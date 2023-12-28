A software package for fitting Bayesian hierarchical models to Sholl analaysis data. 

For supported models, see https://www.biorxiv.org/content/10.1101/2023.01.23.525256v1.

Running the ungrouped mouse example
================

``` r
library(ShollBayes)
library(dplyr)
```

We provide pre-specified models for each of the examples discussed in
REFERENCE. In this vignette, we go through the package workflow using
the ungrouped mouse example.

``` r
#load data
sholl_data = ShollBayes::ug3_data
```

To load a pre-specified model, use the `load_model()` function. For a
list of supported models, see `?load_model()`. Here we load the model
for the ungrouped mouse example.

``` r
m = load_model("ug3_model")
```

The output is an object of class `shollmcmcObj()`. While this is an S4
object, we only provide support for accessing the samples slot, which is
where the MCMC samples are stored as `an mcmc.list()` object. All slots
are automatically filled by moving through the workflow.

Data must be in a particular list format to be compatible with the
precompiled models. To streamline this process, use the
`restructure_data()` function. The relevant variables can natrually be
provided as columns from a dataframe to `restructure_data()` as separate
arguments, . The arguments provided to `restructure_data()` depend on
the model being fit, for a list of necessary parameters see
`?restructure_data()`. Note that the names of these arguments correspond
to the particular model fit in REFERENCE. For instance, the ungrouped
mouse example contains Sholl curve process crossings, the corresponding
radii, and ID variables for animal, image, and cell, since the data are
nested in that order.

``` r
m = restructure_data(m,
                     y = sholl_data$y,
                     R = sholl_data$R,
                     Cell_ID = sholl_data$Cell_ID,
                     Image_ID = sholl_data$Image_ID,
                     Animal_ID = sholl_data$Animal_ID)
```

We provide wrapper functions for initializing, updating, and sampling
from a JAGS model. Initialization compiles the model and performs some
initial optimization before sampleing, which is performed using
`intialize_model()`. Burn-in is performed by using `update_model()`.
Finally, the MCMC samples are obtained by running `sample_model()`.

``` r
set.seed(293402)
m = initialize_model(m, n.chains = 4, n.adapt = 200)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 6600
#>    Unobserved stochastic nodes: 804
#>    Total graph size: 93406
#> 
#> Initializing model

set.seed(123901)
update_model(m, n.iter = 200)

set.seed(349058)
m = sample_model(m, n.iter = 500, new_sample = F)
```

We can access the MCMC samples using the `sample()` function. This
returns an `mcmc.list()` from the `coda` package, so standard workflows
for subsequent Bayesian inference can be used.

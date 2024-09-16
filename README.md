
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RDSAssociation

<!-- badges -->

![Package Version](https://img.shields.io/badge/version-0.1.0-blue.svg)
![License](https://img.shields.io/badge/license-MIT-green.svg)

The goal of `RDSAssociation` is to implement the Semi-parametric
randomization test for continuous association (SPRTCA). SPRTCA was
developed to infer bivariate association between two variables collected
using respondent-driven sampling (RDS) when one or both variables are
continuous. RDS is a link-trace sampling design that uses
peer-recruitment to sample from hard to reach populations. The process
of peer-recruitment to obtain a sample introduces dependence between
recruited individuals as persons are often more likely to affiliate with
others like themselves. This concept is known as homophily. SPRTCA
offers a statistical approach to infer bivariate association that
accounts for homophily whereas classical statistical methods (ie.
t-test, Pearson correlation test) assume independence between
observations.

For a detailed discussion of the methods please see the manuscript
“Inferring bivariate associations with continuous data from studies
using respondent-driven sampling” by Malatesta et al. 

## Installation

You can install the development version of RDSAssociation from
[GitHub](https://github.com/). We recommend setting the option
`build_vignettes=T` when installing so the package vignette can be
accessed in your local R environment (note: this may take a while). The
`devtools` package must be installed prior to installing
`RDSAssociation`.

``` r
# install.packages("devtools")
devtools::install_github("samalatesta/RDSAssociation")
```

## Usage

To use `RDSAssociation` in your R scripts or projects, load the package
using:

``` r
library(RDSAssociation)
```

For detailed information on how to use each function, please refer to
the package documentation and vignette. The vignette can be viewed
locally after package installation or the knitted html is also included
in the `\vignettes` directory.

``` r
vignette(package="RDSAssociation")
```

## Documentation

Comprehensive documentation for `RDSAssociation` functions is available
within R. You can access documentation using the ? operator followed by
the function name. For example:

``` r
?sprtca
```

## License

`RDSAssociation` is distributed under the MIT License.

## Contact

For questions, please contact:

Samantha Malatesta (<samalate@bu.edu>)

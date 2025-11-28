# mcglm 0.9.0

The `mcglm` package fits multivariate covariance generalized linear
models (Bonat and Jorgensen, 2016).

## Introduction

`mcglm` is an R package designed to fit Multivariate Covariance
Generalized Linear Models. It allows you to specify a distinct linear
predictor for each response variable, offering exceptional flexibility
for analyses involving multiple outcomes.

With `mcglm`, you can model a wide range of response types — continuous,
discrete (such as counts and binary), limited, and even zero inflated
responses, whether continuous or mixed.

Its main strength lies in the ability to capture complex relationships
among variables through multiple covariance structures, enabling more
realistic and robust multivariate modeling.

This package was developed as part of the Wagner’s Ph.D. thesis,
combining academic rigor with practical value for the statistical
modeling community.

## Download and install

### Linux/Mac

Use the `devtools` package (available from
[CRAN](http://cran-r.c3sl.ufpr.br/web/packages/devtools/index.html)) to
install automatically from this GitHub repository:

    library(devtools)
    install_github("bonatwagner/mcglm")

Alternatively, download the package tarball:
[mcglm\_0.9.0.tar.gz](https://github.com/bonatwagner/mcglm/blob/main/mcglm_0.9.0.tar.gz)
and run from a UNIX terminal (make sure you are on the container file
directory):

    R CMD INSTALL -l /path/to/your/R/library mcglm_0.9.0.tar.gz

Or, inside an `R` session:

    install.packages("mcglm_0.9.0.tar.gz", repos = NULL,
                     lib.loc = "/path/to/your/R/library",
                     dependencies = TRUE)

Note that `-l /path/to/your/R/library` in the former and
`lib.loc = "/path/to/your/R/library"` in the latter are optional. Only
use it if you want to install in a personal library, other than the
standard R library.

### Windows

Download Windows binary version: \[mcglm\_0.9.0.zip\]\[\] (**do not
unzip it under Windows**), put the file in your working directory, and
from inside `R`:

    install.packages("mcglm_0.9.0.zip", repos = NULL,
                     dependencies = TRUE)

## Authors

-   [Wagner Hugo Bonat](https://www.linkedin.com/in/wagner-bonat)
    (author and main developer)

## Documentation

The reference manual in PDF can be found here:
[mcglm-manual.pdf](https://github.com/bonatwagner/mcglm/blob/main/mcglm.Rcheck/mcglm-manual.pdf)

## Contributing

This R package is develop using
[`roxygen2`](https://github.com/klutometis/roxygen) for documentation
and [`devtools`](https://github.com/hadley/devtools) to check and build.
Also, we adopt the [Gitflow
worflow](https://nvie.com/posts/a-successful-git-branching-model/) in
this repository. Please, see the [instructions for
contributing](./CONTRIBUTING.md) to collaborate.

## License

This package is released under the [GNU General Public License (GPL)
v3.0](https://www.gnu.org/licenses/gpl-3.0.html).

See [LICENSE](./LICENSE)

<!-- links -->

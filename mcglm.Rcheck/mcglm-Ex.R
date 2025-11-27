pkgname <- "mcglm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "mcglm-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('mcglm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Hunting")
### * Hunting

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Hunting
### Title: Hunting in Pico Basile, Bioko Island, Equatorial Guinea.
### Aliases: Hunting
### Keywords: datasets

### ** Examples

library(mcglm)
library(Matrix)
data(Hunting, package="mcglm")
formu <- OT ~ METHOD*ALT + SEX + ALT*poly(MONTH, 4)
Z0 <- mc_id(Hunting)
Z1 <- mc_mixed(~0 + HUNTER.MONTH, data = Hunting)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(c(Z0, Z1)),
            link = c("log"), variance = c("poisson_tweedie"),
            power_fixed = c(FALSE),
            control_algorithm = list(max_iter = 100),
            offset = list(log(Hunting$OFFSET)), data = Hunting)
summary(fit)
anova(fit)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Hunting", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NewBorn")
### * NewBorn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NewBorn
### Title: Respiratory Physiotherapy on Premature Newborns.
### Aliases: NewBorn
### Keywords: datasets

### ** Examples

library(mcglm)
library(Matrix)
data(NewBorn, package="mcglm")
formu <- SPO2 ~ Sex + APGAR1M + APGAR5M + PRE + HD + SUR
Z0 <- mc_id(NewBorn)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(Z0),
            link = c("logit"), variance = c("binomialP"),
            power_fixed = c(TRUE),
            data = NewBorn,
            control_algorithm = list(verbose = FALSE, tuning = 0.5))
summary(fit)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NewBorn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ahs")
### * ahs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ahs
### Title: Australian Health Survey
### Aliases: ahs
### Keywords: datasets

### ** Examples

require(mcglm)
data(ahs, package="mcglm")
form1 <- Ndoc ~ income + age
form2 <- Nndoc ~ income + age
Z0 <- mc_id(ahs)
fit.ahs <- mcglm(linear_pred = c(form1, form2),
                 matrix_pred = list(Z0, Z0), link = c("log","log"),
                 variance = c("poisson_tweedie","poisson_tweedie"),
                 data = ahs)
summary(fit.ahs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ahs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("anova.mcglm")
### * anova.mcglm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: anova.mcglm
### Title: Anova Tables
### Aliases: anova.mcglm

### ** Examples

x1 <- seq(-1, 1, l = 100)
x2 <- gl(5, 20)
beta <- c(5, 0, -2, -1, 1, 2)
X <- model.matrix(~ x1 + x2)
set.seed(123)
y <- rnorm(100, mean = X%*%beta, sd = 1)
data = data.frame("y" = y, "x1" = x1, "x2" = x2)
fit.anova <- mcglm(c(y ~ x1 + x2), list(mc_id(data)), data = data)
anova(fit.anova)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("anova.mcglm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_anova_disp")
### * mc_anova_disp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_anova_disp
### Title: Anova Tables for dispersion components
### Aliases: mc_anova_disp

### ** Examples

x1 <- seq(0, 1, l = 100)
x2 <- gl(5, 20)
beta <- c(5, 0, -2, -1, 1, 2)
X <- model.matrix(~ x1 + x2)
set.seed(123)
y <- rnorm(100, mean = 10, sd = X%*%beta)
data = data.frame("y" = y, "x1" = x1, "x2" = x2, "id" = 1)
fit.anova <- mcglm(c(y ~ 1), list(mc_dglm(~ x1 + x2, id = "id", data)),
                   control_algorithm = list(tuning = 0.9), data = data)
X <- model.matrix(~ x1 + x2, data = data)
idx <- attr(X, "assign")
idx_list <- list("idx" = idx, "idx" = idx)
names_list <- list(colnames(X), colnames(X))
mc_anova_disp(object = fit.anova, idx = idx_list, names_list = names_list)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_anova_disp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_dexp_gold")
### * mc_dexp_gold

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_dexp_gold
### Title: Exponential-matrix and its derivatives
### Aliases: mc_dexp_gold
### Keywords: internal

### ** Examples

M <- matrix(c(1,0.8,0.8,1), 2,2)
dM <- matrix(c(0,1,1,0),2,2)
mcglm::mc_dexp_gold(M = M, dM = dM)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_dexp_gold", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_dist")
### * mc_dist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_dist
### Title: Distance Models Structure
### Aliases: mc_dist

### ** Examples

id <- rep(1:2, each = 4)
time <- rep(1:4, 2)
data <- data.frame("id" = id, "time" = time)
mc_dist(id = "id", time = "time", data = data)
mc_dist(id = "id", time = "time", data = data, method = "canberra")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_dist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_link_function")
### * mc_link_function

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_link_function
### Title: Link Functions
### Aliases: mc_link_function mc_logit mc_probit mc_cauchit mc_cloglog
###   mc_loglog mc_identity mc_log mc_sqrt mc_invmu2 mc_inverse

### ** Examples

x1 <- seq(-1, 1, l = 5)
X <- model.matrix(~ x1)
mc_link_function(beta = c(1,0.5), X = X,
                 offset = NULL, link = 'log')
mc_link_function(beta = c(1,0.5), X = X,
                 offset = rep(10,5), link = 'identity')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_link_function", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_ma")
### * mc_ma

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_ma
### Title: Moving Average Models Structure
### Aliases: mc_ma

### ** Examples

id <- rep(1:2, each = 4)
time <- rep(1:4, 2)
data <- data.frame("id" = id, "time" = time)
mc_ma(id = "id", time = "time", data = data, order = 1)
mc_ma(id = "id", time = "time", data = data, order = 2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_ma", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_matrix_linear_predictor")
### * mc_matrix_linear_predictor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_matrix_linear_predictor
### Title: Matrix Linear Predictor
### Aliases: mc_matrix_linear_predictor

### ** Examples

require(Matrix)
Z0 <- Diagonal(5, 1)
Z1 <- Matrix(rep(1,5)%*%t(rep(1,5)))
Z <- list(Z0, Z1)
mc_matrix_linear_predictor(tau = c(1,0.8), Z = Z)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_matrix_linear_predictor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_mixed")
### * mc_mixed

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_mixed
### Title: Mixed Models Structure
### Aliases: mc_mixed

### ** Examples

SUBJECT <- gl(2, 6)
x1 <- rep(1:6, 2)
x2 <- rep(gl(2,3),2)
data <- data.frame(SUBJECT, x1 , x2)
# Compound symmetry structure
mc_mixed(~0 + SUBJECT, data = data)
# Compound symmetry + random slope for x1 and interaction or correlation
mc_mixed(~0 + SUBJECT/x1, data = data)
# Compound symmetry + random slope for x1 and x2 plus interactions
mc_mixed(~0 + SUBJECT/(x1 + x2), data = data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_mixed", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_rw")
### * mc_rw

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_rw
### Title: Random Walk Models Structure
### Aliases: mc_rw

### ** Examples

id <- rep(1:2, each = 4)
time <- rep(1:4, 2)
data <- data.frame("id" = id, "time" = time)
mc_rw(id = "id", time = "time", data = data, order = 1, proper = FALSE)
mc_rw(id = "id", time = "time", data = data, order = 1, proper = TRUE)
mc_rw(id = "id", time = "time", data = data, order = 2, proper = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_rw", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_sic")
### * mc_sic

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_sic
### Title: Score Information Criterion - Regression
### Aliases: mc_sic

### ** Examples

set.seed(123)
x1 <- runif(100, -1, 1)
x2 <- gl(2,50)
beta = c(5, 0, 3)
X <- model.matrix(~ x1 + x2)
y <- rnorm(100, mean = X%*%beta , sd = 1)
data <- data.frame(y, x1, x2)
# Reference model
Z0 <- mc_id(data)
fit0 <- mcglm(linear_pred = c(y ~ 1), matrix_pred = list(Z0), data = data)
# Computing SIC
mc_sic(fit0, scope = c("x1","x2"), data = data, response = 1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_sic", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_sic_covariance")
### * mc_sic_covariance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_sic_covariance
### Title: Score Information Criterion - Covariance
### Aliases: mc_sic_covariance

### ** Examples

set.seed(123)
SUBJECT <- gl(10, 10)
y <- rnorm(100)
data <- data.frame(y, SUBJECT)
Z0 <- mc_id(data)
Z1 <- mc_mixed(~0+SUBJECT, data = data)
# Reference model
fit0 <- mcglm(c(y ~ 1), list(Z0), data = data)
# Testing the effect of the matrix Z1
mc_sic_covariance(fit0, scope = Z1, idx = 1,
data = data, response = 1)
# As expected Tu < Chisq indicating non-significance of Z1 matrix



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_sic_covariance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_twin")
### * mc_twin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_twin
### Title: Twin Models Structure
### Aliases: mc_twin mc_twin_bio mc_twin_full

### ** Examples

id <- rep(1:5, each = 4)
id.twin <- rep(1:2, 10)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_twin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mc_variance_function")
### * mc_variance_function

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mc_variance_function
### Title: Variance Functions
### Aliases: mc_variance_function mc_power mc_binomialP mc_binomialPQ

### ** Examples

x1 <- seq(-1, 1, l = 5)
X <- model.matrix(~x1)
mu <- mc_link_function(beta = c(1, 0.5), X = X, offset = NULL,
                       link = "logit")
mc_variance_function(mu = mu$mu, power = c(2, 1), Ntrial = 1,
                     variance = "binomialPQ", inverse = FALSE,
                     derivative_power = TRUE, derivative_mu = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mc_variance_function", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("soil")
### * soil

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: soil
### Title: Soil Chemistry Properties Data
### Aliases: soil
### Keywords: datasets

### ** Examples

data(soil, package="mcglm")
## neigh <- spdep::tri2nb(soil[,1:2])
## Spatial model
## Z1 <- mc_car(neigh) take too long
Z1 <- mc_id(soil)
# Linear predictor
form.ca <- CA ~ COORD.X*COORD.Y + SAND + SILT + CLAY + PHWATER
fit.ca <- mcglm(linear_pred = c(form.ca), matrix_pred = list(Z1),
               link = "log", variance = "tweedie", covariance = "inverse",
               power_fixed = TRUE, data = soil,
               control_algorith = list(max_iter = 1000, tuning = 0.1,
               verbose = FALSE, tol = 1e-03))
## mc_compute_rho(fit.ca) only for spatial model




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("soil", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("soya")
### * soya

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: soya
### Title: Soybeans
### Aliases: soya
### Keywords: datasets

### ** Examples

library(mcglm)
library(Matrix)
data(soya, package="mcglm")
formu <- grain ~ block + factor(water) * factor(pot)
Z0 <- mc_id(soya)
fit <- mcglm(linear_pred = c(formu), matrix_pred = list(Z0),
          data = soya)
          anova(fit)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("soya", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

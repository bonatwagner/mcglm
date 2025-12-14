#' @title Wald Tests for Fixed Effects in mcglm Models
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Performs Wald chi-square tests for assessing the significance of
#' fixed-effect terms in the linear predictors of an \code{mcglm} model.
#' The tests are conducted separately for each response variable and
#' are particularly useful for joint hypothesis testing of regression
#' coefficients associated with categorical covariates with more than
#' two levels. This function is not intended for model comparison.
#'
#' @param object An object of class \code{mcglm}, typically the result of
#'   a call to \code{\link{mcglm}}.
#' @param ... Additional arguments. Currently ignored.
#' @param verbose
#' Logical indicating whether the Wald test results should be printed
#' to the console. If \code{FALSE}, the function silently returns the
#' results.
#' @return
#' A list of data frames, one for each response variable. Each data frame
#' contains the results of Wald chi-square tests for the fixed-effect
#' terms in the corresponding linear predictor, with the following
#' columns:
#' \describe{
#'   \item{Covariate}{Name of the covariate or model term tested.}
#'   \item{Chi.Square}{Value of the Wald chi-square statistic.}
#'   \item{Df}{Degrees of freedom associated with the test.}
#'   \item{p.value}{P-value of the Wald test.}
#' }
#' The returned object is invisible and is primarily intended for
#' programmatic use.
#'
#' @details
#' The Wald tests are computed using the observed covariance matrix of
#' the regression parameter estimates. For each response variable, joint
#' tests are performed for sets of parameters corresponding to the same
#' model term, as defined by the design matrix.
#'
#' @seealso \code{\link{summary.mcglm}}, \code{\link{coef.mcglm}},
#'   \code{\link{vcov.mcglm}}
#'
#' @examples
#' x1 <- seq(-1, 1, length.out = 100)
#' x2 <- gl(5, 20)
#' beta <- c(5, 0, -2, -1, 1, 2)
#' X <- model.matrix(~ x1 + x2)
#' set.seed(123)
#' y <- rnorm(100, mean = X %*% beta, sd = 1)
#' data <- data.frame(y = y, x1 = x1, x2 = x2)
#' fit <- mcglm(c(y ~ x1 + x2), list(mc_id(data)), data = data)
#' anova(fit)
#'
#' @method anova mcglm
#' @export

anova.mcglm <- function(object, ..., verbose = TRUE) {

  if (!inherits(object, "mcglm")) {
    stop("object must be of class 'mcglm'", call. = FALSE)
  }

  n_resp <- length(object$mu_list)
  n_beta <- vapply(object$list_X, ncol, integer(1))

  idx.list <- lapply(seq_len(n_resp), function(i) {
    rep(i, n_beta[i])
  })

  vv <- vcov(object)
  idx.vec <- c(unlist(idx.list),
               rep(0, ncol(vv) - length(unlist(idx.list))))

  temp.vcov <- vector("list", n_resp)
  temp.beta <- vector("list", n_resp)

  for (i in seq_len(n_resp)) {
    idx.id <- idx.vec == i
    temp.vcov[[i]] <- vv[idx.id, idx.id, drop = FALSE]
    temp.beta[[i]] <-
      coef(object, type = "beta", response = i)$Estimates
  }

  output <- vector("list", n_resp)

  for (i in seq_len(n_resp)) {

    assign_idx <- attr(object$list_X[[i]], "assign")
    term_names <- colnames(object$list_X[[i]])

    if (term_names[1] == "(Intercept)") {
      assign_idx <- assign_idx[-1]
      term_names <- term_names[-1]
      temp.beta[[i]] <- temp.beta[[i]][-1]
      temp.vcov[[i]] <- temp.vcov[[i]][-1, -1, drop = FALSE]
    }

    n_terms <- length(unique(assign_idx))
    res_i <- vector("list", n_terms)

    for (j in seq_len(n_terms)) {

      idx.term <- assign_idx == j
      beta_j <- temp.beta[[i]][idx.term]
      vcov_j <- temp.vcov[[i]][idx.term, idx.term, drop = FALSE]

      inv_vcov_j <- solve(vcov_j, tol = .Machine$double.eps)

      chi_sq <- as.numeric(t(beta_j) %*% inv_vcov_j %*% beta_j)
      df_j <- length(beta_j)

      res_i[[j]] <- data.frame(
        Covariate = term_names[idx.term][1],
        Chi.Square = round(chi_sq, 4),
        Df = df_j,
        p.value = round(pchisq(chi_sq, df_j, lower.tail = FALSE), 4)
      )
    }

    output[[i]] <- do.call(rbind, res_i)
  }

  if (verbose) {
    cat("Wald test for fixed effects\n")
    for (i in seq_len(n_resp)) {
      cat("Call: ")
      print(object$linear_pred[[i]])
      cat("\n")
      print(output[[i]])
      cat("\n")
    }
  }

  return(invisible(output))
}

# anova.mcglm <- function(object, ...) {
#     n_resp <- length(object$mu_list)
#     n_beta <- lapply(object$list_X, ncol)
#     idx.list <- list()
#     for (i in 1:n_resp) {
#         idx.list[[i]] <- rep(i, n_beta[i])
#     }
#     vv <- vcov(object)
#     n_par <- dim(vv)[1]
#     idx.vec <- do.call(c, idx.list)
#     n_cov <- n_par - length(idx.vec)
#     idx.vec <- c(idx.vec, rep(0, n_cov))
#     temp.vcov <- list()
#     temp.beta <- list()
#     for (i in 1:n_resp) {
#         idx.id <- idx.vec == i
#         temp.vcov[[i]] <- vv[idx.id, idx.id]
#         temp.beta[[i]] <-
#             coef(object, type = "beta", response = i)$Estimates
#     }
#     saida <- list()
#     for (i in 1:n_resp) {
#         idx <- attr(object$list_X[[i]], "assign")
#         names <- colnames(object$list_X[[i]])
#         if (names[1] == "(Intercept)") {
#             idx <- idx[-1]
#             names <- names[-1]
#             temp.beta[[i]] <- temp.beta[[i]][-1]
#             temp.vcov[[i]] <- temp.vcov[[i]][-1, -1]
#         }
#         n_terms <- length(unique(idx))
#         X2.resp <- list()
#         for (j in 1:n_terms) {
#             idx.TF <- idx == j
#             temp <- as.numeric(
#                 t(temp.beta[[i]][idx.TF]) %*%
#                     solve(as.matrix(temp.vcov[[i]])[idx.TF, idx.TF]) %*%
#                     temp.beta[[i]][idx.TF])
#             nbeta.test <- length(temp.beta[[i]][idx.TF])
#             X2.resp[[j]] <-
#                 data.frame(Covariate = names[idx.TF][1],
#                            Chi.Square = round(temp, 4), Df = nbeta.test,
#                            p.value = round(pchisq(temp, nbeta.test,
#                                                   lower.tail = FALSE),
#                                            4))
#         }
#         saida[[i]] <- do.call(rbind, X2.resp)
#     }
#     cat("Wald test for fixed effects\n")
#     for(i in 1:n_resp) {
#       cat("Call: ")
#       print(object$linear_pred[[i]])
#       cat("\n")
#       print(saida[[i]])
#       cat("\n")
#     }
#     return(invisible(saida))
# }

#' @title Model Coefficients
#' @name coef.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Extract regression, dispersion and correlation parameter estimates
#' from objects of class \code{mcglm}.
#'
#' @param object
#' An object of class \code{mcglm}.
#'
#' @param std.error
#' Logical indicating whether standard errors should be returned
#' alongside the parameter estimates. Default is \code{FALSE}.
#'
#' @param response
#' Integer vector indicating for which response variables the
#' coefficients should be returned. If \code{NA}, coefficients for
#' all response variables are returned.
#'
#' @param type
#' Character vector specifying which type of coefficients should be
#' returned. Possible values are \code{"beta"}, \code{"tau"},
#' \code{"power"} and \code{"correlation"}.
#'
#' @param ...
#' Additional arguments. Currently ignored and included for
#' compatibility with the generic \code{\link[stats]{coef}} function.
#'
#' @return
#' A \code{data.frame} with one row per parameter, containing:
#' \itemize{
#'   \item \code{Estimates}: parameter estimates;
#'   \item \code{Std.error}: standard errors (if requested);
#'   \item \code{Parameters}: parameter names;
#'   \item \code{Type}: parameter type;
#'   \item \code{Response}: response variable index.
#' }
#'
#' @method coef mcglm
#' @export

coef.mcglm <- function(object, std.error = FALSE,
                       response = NA,
                       type = c("beta", "tau", "power", "correlation"),
                       ...) {

  n_resp <- length(object$beta_names)

  if (any(!type %in% c("beta", "tau", "power", "correlation"))) {
    stop("Invalid 'type' argument.")
  }

  if (!all(is.na(response))) {
    if (any(response < 1 | response > n_resp)) {
      stop("Invalid 'response' argument.")
    }
  }

  cod <- type_cod <- response_cod <- character(0)

  ## Regression (beta), power and tau parameters
  for (i in seq_len(n_resp)) {

    ## beta
    nb <- object$Information$n_betas[[i]]
    cod_i <- paste0("beta", i, 0:(nb - 1))
    cod <- c(cod, cod_i)
    type_cod <- c(type_cod, rep("beta", nb))
    response_cod <- c(response_cod, rep(i, nb))

    ## power
    np <- object$Information$n_power[[i]]
    if (np > 0 || !object$power_fixed[[i]]) {
      cod_i <- paste0("power", i, seq_len(np))
      cod <- c(cod, cod_i)
      type_cod <- c(type_cod, rep("power", length(cod_i)))
      response_cod <- c(response_cod, rep(i, length(cod_i)))
    }

    ## tau
    nt <- object$Information$n_tau[[i]]
    cod_i <- paste0("tau", i, seq_len(nt))
    cod <- c(cod, cod_i)
    type_cod <- c(type_cod, rep("tau", nt))
    response_cod <- c(response_cod, rep(i, nt))
  }

  ## Correlation parameters
  if (n_resp > 1) {
    comb <- combn(n_resp, 2)
    rho_names <- apply(comb, 2, function(x) paste0("rho", x[1], x[2]))
    cod <- c(cod, rho_names)
    type_cod <- c(type_cod, rep("correlation", length(rho_names)))
    response_cod <- c(response_cod, rep(NA_integer_, length(rho_names)))
  }

  Estimates <- c(object$Regression, object$Covariance)

  coef_df <- data.frame(
    Estimates = Estimates,
    Parameters = cod,
    Type = type_cod,
    Response = response_cod,
    stringsAsFactors = FALSE
  )

  if (std.error) {
    coef_df$Std.error <- sqrt(diag(object$vcov))
  }

  ## Filtering
  if (!all(is.na(response))) {
    coef_df <- coef_df[coef_df$Response %in% response, ]
  }

  coef_df <- coef_df[coef_df$Type %in% type, ]

  rownames(coef_df) <- NULL
  coef_df
}

# coef.mcglm <- function(object, std.error = FALSE,
#                        response = c(NA, 1:length(object$beta_names)),
#                        type = c("beta", "tau", "power", "correlation"),
#                        ...) {
#     n_resp <- length(object$beta_names)
#     cod_beta <- list()
#     cod_power <- list()
#     cod_tau <- list()
#     type_beta <- list()
#     type_power <- list()
#     type_tau <- list()
#     resp_beta <- list()
#     resp_power <- list()
#     resp_tau <- list()
#     response_for <- 1:n_resp
#     for (i in response_for) {
#         cod_beta[[i]] <- paste0(
#             paste0("beta", i), 0:c(object$Information$n_betas[[i]] - 1))
#         type_beta[[i]] <- rep("beta", length(cod_beta[[i]]))
#         resp_beta[[i]] <- rep(response_for[i], length(cod_beta[[i]]))
#         if (object$Information$n_power[[i]] != 0 |
#             object$power_fixed[[i]] == FALSE) {
#             cod_power[[i]] <- paste0(
#                 paste0("power", i), 1:object$Information$n_power[[i]])
#             type_power[[i]] <- rep("power",
#                                    length(cod_power[[i]]))
#             resp_power[[i]] <- rep(response_for[i],
#                                    length(cod_power[[i]]))
#         }
#         if (object$Information$n_power[[i]] == 0) {
#             cod_power[[i]] <- rep(1, 0)
#             type_power[[i]] <- rep(1, 0)
#             resp_power[[i]] <- rep(1, 0)
#         }
#         cod_tau[[i]] <- paste0(
#             paste0("tau", i), 1:object$Information$n_tau[[i]])
#         type_tau[[i]] <- rep("tau", length(cod_tau[[i]]))
#         resp_tau[[i]] <- rep(response_for[i], length(cod_tau[[i]]))
#     }
#     rho_names <- c()
#     if (n_resp != 1) {
#         combination <- combn(n_resp, 2)
#         for (i in 1:dim(combination)[2]) {
#             rho_names[i] <- paste0(
#                 paste0("rho", combination[1, i]), combination[2, i])
#         }
#     }
#     type_rho <- rep("correlation", length(rho_names))
#     resp_rho <- rep(NA, length(rho_names))
#     cod <- c(do.call(c, cod_beta), rho_names,
#              do.call(c, Map(c, cod_tau)))
#     type_cod <- c(do.call(c, type_beta), type_rho,
#                   do.call(c, Map(c, type_tau)))
#     response_cod <- c(do.call(c, resp_beta), resp_rho,
#                       do.call(c, Map(c, resp_tau)))
#
#     if (length(cod_power) != 0) {
#         cod <- c(do.call(c, cod_beta), rho_names,
#                  do.call(c, Map(c, cod_power, cod_tau)))
#         type_cod <- c(do.call(c, type_beta), type_rho,
#                       do.call(c, Map(c, type_power, type_tau)))
#         response_cod <- c(do.call(c, resp_beta), resp_rho,
#                           do.call(c, Map(c, resp_power, resp_tau)))
#     }
#
#     Estimates <- c(object$Regression, object$Covariance)
#     coef_temp <- data.frame(
#         Estimates = Estimates,
#         Parameters = cod,
#         Type = type_cod,
#         Response = response_cod)
#     if (std.error == TRUE) {
#         coef_temp <- data.frame(
#             Estimates = Estimates,
#             Std.error = sqrt(diag(object$vcov)),
#             Parameters = cod, Type = type_cod,
#             Response = response_cod)
#     }
#     output <- coef_temp[
#         which(coef_temp$Response %in% response &
#                   coef_temp$Type %in% type), ]
#     return(output)
# }

#' @title Confidence Intervals for Model Parameters
#' @name confint.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Computes Wald-type confidence intervals for parameter estimates
#' from a fitted \code{mcglm} model, based on asymptotic normality.
#'
#' @param object
#' A fitted object of class \code{mcglm}.
#'
#' @param parm
#' Optional specification of parameters for which confidence intervals
#' are required. Can be a numeric vector of indices or a character
#' vector of parameter names. If omitted, confidence intervals for all
#' parameters are returned.
#'
#' @param level
#' Numeric value giving the confidence level. Must be between 0 and 1.
#' Default is \code{0.95}.
#'
#' @param ...
#' Additional arguments. Currently ignored and included for
#' compatibility with the generic
#' \code{\link[stats]{confint}} function.
#'
#' @return
#' A numeric matrix with two columns corresponding to the lower and
#' upper confidence limits. Rows correspond to model parameters.
#'
#' @method confint mcglm
#' @export

confint.mcglm <- function(object, parm, level = 0.95, ...) {

  if (!is.numeric(level) || length(level) != 1 ||
      level <= 0 || level >= 1) {
    stop("'level' must be a numeric value between 0 and 1.")
  }

  coef_tab <- coef(object, std.error = TRUE)

  if (is.null(coef_tab$Std.error)) {
    stop("Standard errors are required to compute confidence intervals.")
  }

  n_par <- nrow(coef_tab)

  if (missing(parm)) {
    parm_idx <- seq_len(n_par)
  } else if (is.numeric(parm)) {
    if (any(parm < 1 | parm > n_par)) {
      stop("Invalid parameter indices in 'parm'.")
    }
    parm_idx <- parm
  } else if (is.character(parm)) {
    if (any(!parm %in% coef_tab$Parameters)) {
      stop("Invalid parameter names in 'parm'.")
    }
    parm_idx <- match(parm, coef_tab$Parameters)
  } else {
    stop("'parm' must be numeric or character.")
  }

  alpha <- (1 - level) / 2
  z <- stats::qnorm(c(alpha, 1 - alpha))

  ci <- coef_tab$Estimates + coef_tab$Std.error %o% z
  colnames(ci) <- c("Lower", "Upper")
  rownames(ci) <- coef_tab$Parameters

  ci[parm_idx, , drop = FALSE]
}

# confint.mcglm <- function(object, parm, level = 0.95, ...) {
#     temp <- coef(object, std.error = TRUE)
#     if (missing(parm)) {
#         parm <- 1:length(temp$Estimates)
#     }
#     a <- (1 - level)/2
#     a <- c(a, 1 - a)
#     fac <- stats::qnorm(a)
#     ci <- temp$Estimates + temp$Std.error %o% fac
#     colnames(ci) <- paste0(format(a, 2), "%")
#     rownames(ci) <- temp$Parameters
#     return(ci[parm, ])
# }

#' @title Fitted Values
#' @name fitted.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Extract fitted (mean) values from a fitted \code{mcglm} model.
#' For multivariate responses, fitted values are returned in matrix
#' form, with one column per response variable.
#'
#' @param object
#' A fitted object of class \code{mcglm}.
#'
#' @param ...
#' Additional arguments. Currently ignored and included for
#' compatibility with the generic
#' \code{\link[stats]{fitted}} function.
#'
#' @return
#' A numeric matrix of fitted values. Rows correspond to observations
#' and columns correspond to response variables.
#'
#' @method fitted mcglm
#' @export

fitted.mcglm <- function(object, ...) {

  if (is.null(object$fitted)) {
    stop("No fitted values found in 'object'.")
  }

  n_resp <- length(object$beta_names)
  n_obs  <- object$n_obs

  if (length(object$fitted) != n_obs * n_resp) {
    stop("Length of fitted values is inconsistent with model dimensions.")
  }

  fitted_mat <- Matrix::Matrix(
    object$fitted,
    nrow = n_obs,
    ncol = n_resp
  )

  colnames(fitted_mat) <- paste0("Response_", seq_len(n_resp))
  rownames(fitted_mat) <- seq_len(n_obs)

  fitted_mat
}

# fitted.mcglm <- function(object, ...) {
#     n_resp <- length(object$beta_names)
#     output <- Matrix(object$fitted, ncol = n_resp, nrow = object$n_obs)
#     return(output)
# }


#' @title Diagnostic Plots for mcglm Objects
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Produces diagnostic plots for fitted \code{mcglm} objects.
#' Available diagnostics include residual analysis, inspection of
#' the fitting algorithm convergence and partial residual plots.
#'
#' @param x
#' A fitted object of class \code{mcglm}.
#'
#' @param type
#' Character string specifying the type of diagnostic plot to be produced.
#' Possible values are \code{"residuals"}, \code{"algorithm"} and
#' \code{"partial_residuals"}.
#'
#' @param ...
#' Additional arguments. Currently ignored and included for compatibility
#' with the generic \code{\link[graphics]{plot}} function.
#'
#' @return
#' No return value, called for its side effects (diagnostic plots).
#'
#' @seealso
#' \code{\link[stats]{residuals}},
#' \code{\link[stats]{fitted}},
#' \code{\link[graphics]{plot}}
#'
#' @method plot mcglm
#' @export

plot.mcglm <- function(x,
                       type = c("residuals", "algorithm", "partial_residuals"),
                       ...) {

  object <- x
  type <- match.arg(type)
  n_resp <- length(object$beta_names)

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  if (type == "residuals") {

    res <- residuals(object, type = "pearson")
    fit <- fitted(object)

    graphics::par(
      mar = c(2.6, 2.5, 0.1, 0.1),
      mgp = c(1.6, 0.6, 0),
      mfrow = c(2, n_resp)
    )

    for (i in seq_len(n_resp)) {
      graphics::plot(
        fit[, i], res[, i],
        xlab = "Fitted values",
        ylab = "Pearson residuals"
      )

      lo <- stats::loess.smooth(fit[, i], res[, i])
      graphics::lines(lo$x, lo$y)

      stats::qqnorm(res[, i])
      stats::qqline(res[, i])
    }
  }

  if (type == "algorithm") {

    n_iter <- sum(!is.na(object$IterationCovariance[, 1]))

    graphics::par(
      mar = c(2.6, 2.5, 0.1, 0.1),
      mgp = c(1.6, 0.6, 0),
      mfrow = c(2, 2)
    )

    idx <- seq_len(n_iter)

    graphics::matplot(
      object$IterationRegression[idx, , drop = FALSE],
      type = "l", lty = 2,
      xlab = "Iterations", ylab = "Regression"
    )

    graphics::matplot(
      object$IterationCovariance[idx, , drop = FALSE],
      type = "l", lty = 2,
      xlab = "Iterations", ylab = "Covariance"
    )

    graphics::matplot(
      object$ScoreRegression[idx, , drop = FALSE],
      type = "l", lty = 2,
      xlab = "Iterations", ylab = "Quasi-score Regression"
    )

    graphics::matplot(
      object$ScoreCovariance[idx, , drop = FALSE],
      type = "l", lty = 2,
      xlab = "Iterations", ylab = "Quasi-score Covariance"
    )
  }

  if (type == "partial_residuals") {

    if (!exists("mc_updateBeta", mode = "function")) {
      stop("Function 'mc_updateBeta' not found.")
    }

    list_beta <- mc_updateBeta(
      list_initial = object$list_initial,
      betas = object$Regression,
      n_resp = n_resp,
      information = object$Information
    )

    res <- residuals(object, type = "pearson")

    for (i in seq_len(n_resp)) {

      comp_X <- as.matrix(object$list_X[[i]]) *
        as.numeric(list_beta$regression[[i]])

      n_cov <- ncol(comp_X)

      grDevices::dev.new()
      graphics::par(
        mar = c(2.6, 2.5, 0.5, 0.5),
        mgp = c(1.6, 0.6, 0),
        mfrow = c(1, max(1, n_cov - 1))
      )

      for (j in 2:n_cov) {
        p_res <- comp_X[, j] + res[, i]
        graphics::plot(
          object$list_X[[i]][, j], p_res,
          xlab = object$beta_names[[i]][j],
          ylab = "Partial residuals"
        )
      }
    }
  }

  invisible(NULL)
}

# plot.mcglm <- function(x, type = "residuals", ...) {
#     object <- x
#     n_resp <- length(object$beta_names)
#     if (type == "residuals") {
#         graphics::par(mar = c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0),
#             mfrow = c(2, n_resp))
#         for (i in 1:n_resp) {
#             res <- residuals(object, type = "pearson")[, i]
#             fit_values <- fitted(object)[, i]
#             graphics::plot(res ~ fit_values, ylab = "Pearson residuals",
#                  xlab = "Fitted values")
#             temp <-
#                 stats::loess.smooth(fitted(object)[, i],
#                              residuals(object, type = "pearson")[, i])
#             graphics::lines(temp$x, temp$y)
#             stats::qqnorm(res)
#             stats::qqline(res)
#         }
#     }
#     if (type == "algorithm") {
#         n_iter <- length(na.exclude(object$IterationCovariance[,
#                                                                1]))
#         graphics::par(mar = c(2.6, 2.5, 0.1, 0.1), mgp = c(1.6, 0.6, 0),
#             mfrow = c(2, 2))
#         graphics::matplot(object$IterationRegression[1:c(n_iter + 5), ],
#                 type = "l", lty = 2, ylab = "Regression",
#                 xlab = "Iterations")
#         graphics::matplot(object$IterationCovariance[1:c(n_iter + 5), ],
#                 type = "l", lty = 2, ylab = "Covariance",
#                 xlab = "Iterations")
#         graphics::matplot(object$ScoreRegression[1:c(n_iter + 5), ], type = "l",
#                 lty = 2, ylab = "Quasi-score Regression",
#                 xlab = "Iterations")
#         graphics::matplot(object$ScoreCovariance[1:c(n_iter + 5), ], type = "l",
#                 lty = 2, ylab = "Quasi-score Covariance",
#                 xlab = "Iterations")
#     }
#     if (type == "partial_residuals") {
#         list_beta <- mc_updateBeta(list_initial = object$list_initial,
#                                    betas = object$Regression,
#                                    n_resp = n_resp,
#                                    information = object$Information)
#         comp_X <- list()
#         for (i in 1:n_resp) {
#             comp_X[[i]] <- as.matrix(object$list_X[[i]]) *
#                 as.numeric(list_beta$regression[[i]])
#         }
#         for (i in 1:n_resp) {
#             res <- residuals(object, type = "pearson")[, i]
#             dev.new()
#             n_cov <- dim(comp_X[[i]])[2]
#             graphics::par(mar = c(2.6, 2.5, 0.5, 0.5),
#                 mgp = c(1.6, 0.6, 0),
#                 mfrow = c(1, c(n_cov - 1)))
#             for (j in 2:n_cov) {
#                 p1 <- comp_X[[i]][, j] + res
#                 graphics::plot(p1 ~ object$list_X[[i]][, j],
#                      xlab = object$beta_names[[i]][j],
#                      ylab = "Partial residuals ")
#             }
#         }
#     }
# }

#' @title Print Method for mcglm Objects
#' @name print.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Prints a concise summary of a fitted \code{mcglm} object, including
#' the model call, link and variance functions, regression coefficients
#' and dispersion parameters for each response variable.
#'
#' @param x
#' A fitted object of class \code{mcglm}, typically returned by
#' \code{mcglm()}.
#'
#' @param ...
#' Further arguments passed to or from other methods.
#'
#' @return
#' No return value, called for its side effects.
#'
#' @seealso
#' \code{\link[base]{print}},
#' \code{\link{summary.mcglm}}
#'
#' @method print mcglm
#' @export

print.mcglm <- function(x, ...) {

  object <- x
  n_resp <- length(object$beta_names)

  if (!exists("mc_updateBeta", mode = "function")) {
    stop("Function 'mc_updateBeta' not found.")
  }

  regression <- mc_updateBeta(
    list_initial = list(),
    betas = object$Regression,
    information = object$Information,
    n_resp = n_resp
  )

  for (i in seq_len(n_resp)) {

    cat("Call:\n")
    print(object$linear_pred[[i]])
    cat("\n")

    cat("Link function: ", object$link[[i]], "\n", sep = "")
    cat("Variance function: ", object$variance[[i]], "\n", sep = "")
    cat("Covariance function: ", object$covariance[[i]], "\n\n", sep = "")

    coef_reg <- regression$regression[[i]]
    names(coef_reg) <- object$beta_names[[i]]

    cat("Regression coefficients:\n")
    print(coef_reg)
    cat("\n")

    tau_temp <- coef(object, response = i, type = "tau")$Estimates
    if (length(tau_temp) > 0) {
      cat("Dispersion parameters:\n")
      print(unname(tau_temp))
      cat("\n")
    }

    power_temp <- coef(object, response = i, type = "power")$Estimates
    if (length(power_temp) > 0) {
      cat("Power parameters:\n")
      print(unname(power_temp))
      cat("\n")
    }
  }

  invisible(x)
}

# print.mcglm <- function(x, ...) {
#     object <- x
#     n_resp <- length(object$beta_names)
#     regression <- mc_updateBeta(list_initial = list(),
#                                 betas = object$Regression,
#                                 information = object$Information,
#                                 n_resp = n_resp)
#     for (i in 1:n_resp) {
#         cat("Call: ")
#         print(object$linear_pred[[i]])
#         cat("\n")
#         cat("Link function:", object$link[[i]])
#         cat("\n")
#         cat("Variance function:", object$variance[[i]])
#         cat("\n")
#         cat("Covariance function:", object$covariance[[i]])
#         cat("\n")
#         names(regression[[1]][[i]]) <- object$beta_names[[i]]
#         cat("Regression:\n")
#         print(regression[[1]][[i]])
#         cat("\n")
#         cat("Dispersion:\n")
#         tau_temp <- coef(object, response = i,
#                          type = "tau")$Estimate
#         names(tau_temp) <- rep("", length(tau_temp))
#         print(tau_temp)
#         cat("\n")
#         power_temp <- coef(object, response = i,
#                            type = "power")$Estimate
#         if (length(power_temp) != 0) {
#             names(power_temp) <- ""
#             cat("Power:\n")
#             print(power_temp)
#             cat("\n")
#         }
#     }
# }

#' @title Residuals for mcglm Objects
#' @name residuals.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Computes residuals for a fitted \code{mcglm} object. Different types
#' of residuals can be extracted, depending on the specified argument
#' \code{type}.
#'
#' @param object
#' An object of class \code{mcglm}.
#'
#' @param type
#' A character string specifying the type of residuals to be returned.
#' Options are:
#' \describe{
#'   \item{\code{"raw"}}{Raw residuals, defined as observed minus fitted values.}
#'   \item{\code{"pearson"}}{Pearson residuals, scaled by the marginal standard deviation.}
#'   \item{\code{"standardized"}}{Standardized residuals, obtained using the inverse covariance matrix.}
#' }
#'
#' @param ...
#' Further arguments passed to or from other methods. Currently ignored.
#'
#' @return
#' A numeric matrix of class \code{Matrix} with dimensions
#' \eqn{n \times r}, where \eqn{n} is the number of observations and
#' \eqn{r} is the number of response variables.
#'
#' @seealso
#' \code{\link[stats]{residuals}},
#' \code{\link{fitted.mcglm}}
#'
#' @method residuals mcglm
#' @export

residuals.mcglm <- function(object,
                            type = c("raw", "pearson", "standardized"),
                            ...) {

  type <- match.arg(type)

  n_resp <- length(object$beta_names)
  n_obs  <- object$n_obs

  res_raw <- Matrix::Matrix(
    object$residuals,
    ncol = n_resp,
    nrow = n_obs
  )

  output <- switch(
    type,

    raw = res_raw,

    pearson = {
      sd_marginal <- sqrt(diag(object$C))
      if (any(sd_marginal <= 0)) {
        stop("Non-positive marginal variances detected.")
      }
      Matrix::Matrix(
        as.numeric(object$residuals / sd_marginal),
        ncol = n_resp,
        nrow = n_obs
      )
    },

    standardized = {
      chol_invC <- tryCatch(
        chol(object$inv_C),
        error = function(e)
          stop("Cholesky decomposition of inv_C failed.")
      )
      Matrix::Matrix(
        as.numeric(object$residuals %*% chol_invC),
        ncol = n_resp,
        nrow = n_obs
      )
    }
  )

  return(output)
}

# residuals.mcglm <- function(object, type = "raw", ...) {
#     n_resp <- length(object$beta_names)
#     output <- Matrix(object$residuals, ncol = n_resp, nrow = object$n_obs)
#     if (type == "standardized") {
#         output <- Matrix(
#             as.numeric(object$residuals %*% chol(object$inv_C)),
#             ncol = n_resp, nrow = object$n_obs)
#     }
#     if (type == "pearson") {
#         output <- Matrix(
#             as.numeric(object$residuals/sqrt(diag(object$C))),
#             ncol = n_resp, nrow = object$n_obs)
#     }
#     return(output)
# }

#' @title Summary for mcglm Objects
#' @name summary.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Produces a summary of a fitted \code{mcglm} object, including estimates,
#' standard errors, Wald statistics and p-values for regression,
#' dispersion, power and correlation parameters.
#'
#' @param object
#' An object of class \code{mcglm}.
#'
#' @param verbose
#' Logical indicating whether the summary should be printed to the console.
#' If \code{FALSE}, the summary is returned invisibly.
#'
#' @param print
#' A character vector specifying which components of the model summary
#' should be printed. Possible values are \code{"Regression"},
#' \code{"power"}, \code{"Dispersion"} and \code{"Correlation"}.
#'
#' @param ...
#' Further arguments passed to or from other methods. Currently ignored.
#'
#' @return
#' Invisibly returns a list containing summary tables for each response
#' variable, and optionally a correlation summary when applicable.
#'
#' @seealso
#' \code{\link{print.mcglm}},
#' \code{\link{coef.mcglm}}
#'
#' @method summary mcglm
#' @export

summary.mcglm <- function(object,
                          verbose = TRUE,
                          print = c("Regression", "power",
                                    "Dispersion", "Correlation"),
                          ...) {

  print <- match.arg(print, several.ok = TRUE)
  n_resp <- length(object$beta_names)
  output <- vector("list", n_resp)

  for (i in seq_len(n_resp)) {

    resp_out <- list()

    ## Regression
    tab_beta <- coef(object, std.error = TRUE,
                     response = i, type = "beta")[, c(1,5), drop = FALSE]
    tab_beta$`Z value` <- tab_beta[, 1] / tab_beta[, 2]
    tab_beta$`Pr(>|z|)` <- 2 * stats::pnorm(-abs(tab_beta$`Z value`))
    rownames(tab_beta) <- object$beta_names[[i]]
    resp_out$Regression <- tab_beta

    ## Power
    tab_power <- coef(object, std.error = TRUE,
                      response = i, type = "power")[, c(1,5), drop = FALSE]
    if (nrow(tab_power) > 0) {
      tab_power$`Z value` <- tab_power[, 1] / tab_power[, 2]
      tab_power$`Pr(>|z|)` <- 2 * stats::pnorm(-abs(tab_power$`Z value`))
      rownames(tab_power) <- NULL
      resp_out$Power <- tab_power
    }

    ## Dispersion (tau)
    tab_tau <- coef(object, std.error = TRUE,
                    response = i, type = "tau")[, c(1,5), drop = FALSE]
    tab_tau$`Z value` <- tab_tau[, 1] / tab_tau[, 2]
    tab_tau$`Pr(>|z|)` <- 2 * stats::pnorm(-abs(tab_tau$`Z value`))
    rownames(tab_tau) <- NULL
    resp_out$Dispersion <- tab_tau

    output[[i]] <- resp_out
  }

  ## Correlation
  tab_rho <- coef(object, std.error = TRUE,
                  response = NA, type = "correlation")
  if (nrow(tab_rho) > 0) {
    tab_rho <- tab_rho[, c("Parameters", "Estimates", "Std.error")]
    tab_rho$`Z value` <- tab_rho$Estimates / tab_rho$Std.error
    tab_rho$`Pr(>|z|)` <- 2 * stats::pnorm(-abs(tab_rho$`Z value`))
    rownames(tab_rho) <- NULL
    output$Correlation <- tab_rho
  }

  ## Printing
  if (isTRUE(verbose)) {

    for (i in seq_len(n_resp)) {

      if ("Regression" %in% print) {
        cat("Call: ")
        print(object$linear_pred[[i]])
        cat("\nLink function:", object$link[[i]], "\n")
        cat("Variance function:", object$variance[[i]], "\n")
        cat("Covariance function:", object$covariance[[i]], "\n\n")
        cat("Regression:\n")
        print(output[[i]]$Regression)
        cat("\n")
      }

      if ("power" %in% print && !is.null(output[[i]]$Power)) {
        cat("Power:\n")
        print(output[[i]]$Power)
        cat("\n")
      }

      if ("Dispersion" %in% print) {
        cat("Dispersion:\n")
        print(output[[i]]$Dispersion)
        cat("\n")
      }
    }

    if ("Correlation" %in% print && !is.null(output$Correlation)) {
      cat("Correlation:\n")
      print(output$Correlation)
      cat("\n")
    }

    cat("Algorithm:", object$con$method, "\n")
    cat("Correction:", object$con$correct, "\n")
    n_iter <- length(stats::na.exclude(object$IterationCovariance[, 1]))
    cat("Number iterations:", n_iter, "\n")
  }

  names(output) <- c(
    paste("Resp.Variable", seq_len(n_resp)),
    if (!is.null(output$Correlation)) "Correlation"
  )

  invisible(output)
}

# summary.mcglm <- function(object, verbose = TRUE,
#                           print = c("Regression","power","Dispersion","Correlation"),
#                           ...) {
#     n_resp <- length(object$beta_names)
#     output <- list()
#     for(i in 1:n_resp) {
#     tab_beta <- coef(object, std.error = TRUE,
#                      response = i, type = "beta")[, 1:2]
#     tab_beta$"Z value" <- tab_beta[, 1]/tab_beta[, 2]
#     tab_beta$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_beta[, 1]/tab_beta[, 2]))
#     rownames(tab_beta) <- object$beta_names[[i]]
#     output[i][[1]]$Regression <- tab_beta
#     tab_power <- coef(object, std.error = TRUE,
#                       response = i, type = "power")[, 1:2]
#     tab_power$"Z value" <- tab_power[, 1]/tab_power[, 2]
#     tab_power$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_power[, 1]/tab_power[, 2]))
#     rownames(tab_power) <- NULL
#     if (dim(tab_power)[1] != 0) {
#       output[i][[1]]$Power <- tab_power
#     }
#     tab_tau <- coef(object, std.error = TRUE,
#                     response = i, type = "tau")[, 1:2]
#     tab_tau$"Z value" <- tab_tau[, 1]/tab_tau[, 2]
#     tab_tau$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_tau[, 1]/tab_tau[, 2]))
#     rownames(tab_tau) <- NULL
#     output[i][[1]]$tau <- tab_tau
#     }
#     tab_rho <- coef(object, std.error = TRUE,
#                     response = NA, type = "correlation")[, c(3, 1, 2)]
#     tab_rho$"Z value" <- tab_rho[, 2]/tab_rho[, 3]
#     tab_rho$"Pr(>|z|)"  <- 2*pnorm(-abs(tab_rho[, 2]/tab_rho[, 3]))
#     if (dim(tab_rho)[1] != 0) {
#       rownames(tab_rho) <- NULL
#       output$Correlation <- tab_rho
#     }
#     if(verbose == TRUE) {
#     for (i in 1:n_resp) {
#       if("Regression" %in% print) {
#         cat("Call: ")
#         print(object$linear_pred[[i]])
#         cat("\n")
#         cat("Link function:", object$link[[i]])
#         cat("\n")
#         cat("Variance function:", object$variance[[i]])
#         cat("\n")
#         cat("Covariance function:", object$covariance[[i]])
#         cat("\n")
#         cat("Regression:\n")
#         print(output[i][[1]]$Regression)
#         cat("\n")
#       }
#         if (dim(tab_power)[1] != 0) {
#           if("power" %in% print) {
#             cat("Power:\n")
#             print(output[i][[1]]$Power)
#            cat("\n")
#           }
#         }
#       if("Dispersion" %in% print) {
#         cat("Dispersion:\n")
#         print(output[i][[1]]$tau)
#         cat("\n")
#       }
#     }
#     if (dim(tab_rho)[1] != 0) {
#       if("Correlation" %in% print) {
#         cat("Correlation matrix:\n")
#         print(tab_rho)
#         cat("\n")
#       }
#     }
#     names(object$con$correct) <- ""
#     iteration_cov <- length(na.exclude(object$IterationCovariance[, 1]))
#     names(iteration_cov) <- ""
#     names(object$con$method) <- ""
#     cat("Algorithm:", object$con$method)
#     cat("\n")
#     cat("Correction:", object$con$correct)
#     cat("\n")
#     cat("Number iterations:", iteration_cov)
#     cat("\n")
#     }
#     if (dim(tab_rho)[1] != 0) {
#     names(output) <- c(paste("Resp.Variable", 1:n_resp),"Correlation")
#     }
#     if (dim(tab_rho)[1] == 0) {
#       names(output) <- paste("Resp.Variable", 1:n_resp)
#     }
#     return(invisible(output))
# }

#' @title Variance-Covariance Matrix for mcglm Objects
#' @name vcov.mcglm
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description
#' Extracts the variance-covariance matrix of the estimated parameters
#' from a fitted \code{mcglm} object.
#'
#' @param object
#' An object of class \code{mcglm}.
#'
#' @param ...
#' Further arguments passed to or from other methods. Currently ignored.
#'
#' @return
#' A numeric matrix representing the variance-covariance matrix of all
#' estimated model parameters. Row and column names correspond to the
#' parameter identifiers.
#'
#' @seealso
#' \code{\link{coef.mcglm}},
#' \code{\link{summary.mcglm}}
#'
#' @method vcov mcglm
#' @export

vcov.mcglm <- function(object, ...) {

  vc <- object$vcov

  param_names <- coef(object)$Parameters

  if (!is.null(param_names) &&
      length(param_names) == nrow(vc)) {
    colnames(vc) <- param_names
    rownames(vc) <- param_names
  }

  return(vc)
}

# vcov.mcglm <- function(object, ...) {
#     cod <- coef(object)$Parameters
#     colnames(object$vcov) <- cod
#     rownames(object$vcov) <- cod
#     return(object$vcov)
# }



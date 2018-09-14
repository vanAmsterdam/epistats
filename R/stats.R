#' Modified t-test that works for aggregated data
#'
#' Modified t-test that works for aggregated data when only mean and sd of
#' 1 or 2 groups are reported
#'
#' @author Wouter van Amsterdam, authors of stats package
#' @export
#' @param n1 the number of observations in group 1
#' @param mu1 the mean in group 1
#' @param sd1 the sd in group 1
#' @param n2 the number of observations in group 2 (optional, leave empty for single sample t-test)
#' @param mu2 the mean in group 2 (optional, leave empty for single sample t-test)
#' @param sd2 the sd in group 2 (optional, leave empty for single sample t-test)
#' @param var_equal assume equal variance?
#' @return an object of class htest
#' @seealso \code{\link{t.test}}

t.test_agg <- function(n1, mu1, sd1, n2 = NULL, mu2 = NULL, sd2 = NULL,
  x = NULL, y = NULL, alternative = c("two.sided", "less",
                                       "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
          ...)
{
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 ||
                               !is.finite(conf.level) || conf.level < 0 || conf.level >
                               1))
    stop("'conf.level' must be a single number between 0 and 1")
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired)
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    dname <- deparse(substitute(x))
    if (paired)
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  if (paired) {
    x <- x - y
    y <- NULL
  }
  # nx <- length(x)
  # mx <- mean(x)
  # vx <- var(x)
  nx <- n1
  mx <- mu1
  vx <- sd1^2
  if (is.null(y) & is.null(n2)) {
    if (nx < 2)
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")
    tstat <- (mx - mu)/stderr
    method <- if (paired)
      "Paired t-test"
    else "One Sample t-test"
    estimate <- setNames(mx, if (paired)
      "mean of the differences"
      else "mean of x")
  }
  else {
    # ny <- length(y)
    ny <- n2
    if (nx < 1 || (!var.equal && nx < 2))
      stop("not enough 'x' observations")
    if (ny < 1 || (!var.equal && ny < 2))
      stop("not enough 'y' observations")
    if (var.equal && nx + ny < 3)
      stop("not enough observations")
    # my <- mean(y)
    # vy <- var(y)
    my <- mu2
    vy <- sd2^2
    method <- paste(if (!var.equal)
      "Welch", "Two Sample t-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    if (var.equal) {
      df <- nx + ny - 2
      v <- 0
      if (nx > 1)
        v <- v + (nx - 1) * vx
      if (ny > 1)
        v <- v + (ny - 1) * vy
      v <- v/df
      stderr <- sqrt(v * (1/nx + 1/ny))
    }
    else {
      stderrx <- sqrt(vx/nx)
      stderry <- sqrt(vy/ny)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny -
                                                        1))
    }
    if (stderr < 10 * .Machine$double.eps * max(abs(mx),
                                                abs(my)))
      stop("data are essentially constant")
    tstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired || !is.null(y))
    "difference in means"
  else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu,
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)
}


#' Partial residuals of a linear regression fit
#'
#' Partial residual of a linear regression fit
#'
#' @author Wouter van Amsterdam
#' @export
#' @param fit the result of a call to \code{\link{lm}}
#' @param term the term for which to got the partial residuals, see details
#' @param resid_type type of residuals, gets passed to \code{\link{resid}}
#' @return a \code{data.frame} with the results from the partial model of
#' the response (including all terms except for \code{term}) and the
#' residuals of the partial model for \code{term}, where \code{term}
#' is the response variable and the other original terms of the models
#' are used as predictors.
#' @details Fits 2 models: 1 for the response of the fit, based on all terms
#' except for the provided \code{term}, and a model where \code{term}
#' is the response variable and the other original terms of the models
#' are used as predictors. The residuals for from these models can be used
#' to assess an assumption from a multivariate model (i.e. the response and
#' term should be linked linearily, condiational on all the other terms)

partial_residuals.lm <- function(fit, term, type = "response") {
  formula0  = formula(fit)
  all_vars  = all.vars(formula0)
  response  = all_vars[1]
  all_terms = all_vars[-1]
  new_terms = setdiff(all_terms, term)

  fit_resp <- lm(reformulate(new_terms, response), data = fit$model)
  fit_term <- lm(reformulate(new_terms, term), data = fit$model)

  return(
    data.frame(resid_response = resid(fit_resp, type = type),
               resid_term     = resid(fit_term, type = type))
  )
}



#' Calculate dispersion of a model fit
#'
#' Calculate dispersion of glm model fit
#'
#' @author Wouter van Amsterdam
#' @export
#' @param fit the result of a glm fit
#' @return a lenght 1 numeric with the dispersion factor

dispersion <- function(fit) {
  p_resids = residuals(fit, type = "pearson")
  df_resid = fit$df.resid
  return(sum(p_resids^2)/df_resid)
}


#' Get proportion and confidence interval from a logical variable
#'
#' Get proportion and confidence interval from a logical variable in one line
#'
#' @author Wouter van Amsterdam
#' @export
#' @param x a logical variable (or 0-1 numeric / integer)
#' @param prop_true logical flag, get proportion of 'trues' or 'falses'
#' @param method passed to \code{\link{binom::binom.confint}} for method
#' of interval calculation. Default is 'wilson'
#' @param conf.level confidence level for interval
#' @param ... other options for \code{\link{binom::binom.confint}}
#' @return a \code{data.frame} with 1 row containing
#' number of trues, length of vector, proportion (mean), lower and upper bound for intervals.

binom.confint_logical <- function(x, prop_true = T,
                                  method = "wilson", conf.level = 0.95, ...) {
  if (!(is.logical(x) | setequal(unique(x), c(0, 1)))) {
    stop("provide logical vector or vector with 0-1 values")
  }

  if (!is.logical(x)) x = as.logical(x)

  binom::binom.confint(
    x = ifelse(prop_true, sum(x), sum(!x)),
    n = length(x),
    conf.level = conf.level,
    method = method,
    ...
  )
}


#' Sorted data.frame of correlations
#'
#' Get a sorted `data.frame` of correlation coefficients, insted of a correlation matrix
#'
#' @param x a matrix (-like structure) that goes into \code{\link{cor}}
#' @param absolute a logical, use absolute values of correlation for ordering?
#' @param decreasing a logical, order with highest correlation first or last
#' @param ... other arguments for \code{\link{cor}}, like `method = "spearman"`
#'
#' @author Wouter van Amsterdam
#' @export
#'

cor.sorted <- function(x, absolute = TRUE, decreasing = T, ...) {
  # get correlation matrix
  cor_matrix <- cor(x, ...)

  # reduce redundancy by removing diagonal and lower triangle
  # (which is the same as the upper diagonal)
  cor_matrix[lower.tri(cor_matrix, diag = T)] <- NA

  # melt to data.frame with columns Var1 and Var2, and correlation,
  # so the pairwise correlations
  suppressWarnings(
    cor_melted <- data.table::melt(cor_matrix, value.name = "correlation")
  )

  # remove redundant rows
  cor_melted <- cor_melted[!is.na(cor_melted$correlation),]

  # add absolute correlation
  cor_melted$abs_correlation <- abs(cor_melted$correlation)

  # order results
  if (absolute) {
    cor_order <- order(cor_melted$abs_correlation, decreasing = decreasing)
  } else {
    cor_order <- order(cor_melted$correlation, decreasing = decreasing)
  }

  cor_sorted <- cor_melted[cor_order,]
  rownames(cor_sorted) <- 1:nrow(cor_sorted)

  return(cor_sorted)
}

##### Development area
# lm.steps <- function(fit, scope, direction = c("both", "backward","forward")) {
#
# }
#
#
# stand.coef <- function(fit) {
#
# }

#' Partial plots of a linear regression fit
#'
#' Partial plots of a linear regression fit
#'
#' @author Wouter van Amsterdam
#' @export
#' @param fit the result of a call to \code{\link{lm}}
#' @param terms for which terms to make the plot. Default (when omitted) is
#' all terms in \code{fit}
#' @param resid_type type of residuals, gets passed to \code{\link{resid}}
#' @param what a character of what to return, either \code{"resid"} for the
#' residual data or \code{"plot"} for the plot
#' @return a \code{data.frame} with the results of all the partials model of
#' the response (including all terms except for \code{term}) and the
#' residuals of the partial model for \code{term}, where \code{term}
#' is the response variable and the other original terms of the models
#' are used as predictors.
#' @seealso \code{\link{partial_residuals.lm}}

partial_plots.lm <- function(fit, terms = NULL, what = "resid") {
  formula0  = formula(fit)
  all_vars  = all.vars(formula0)
  response  = all_vars[1]
  all_terms = all_vars[-1]

  terms = if (!is.null(terms)) {terms} else {all_terms}

  resid_data = pmap_df(list(terms), function(term) {
    data.frame(term = term,
               partial_residuals.lm(fit, term), stringsAsFactors = F)
  })

  p = ggplot(resid_data, aes(x = resid_term, y = resid_response)) +
    geom_point() + geom_smooth(method = "lm", alpha = 0.15) +
    facet_wrap(~term, scales = "free_x") +
    theme_minimal() +
    labs(x = "Residual of term ~ .",
         y = paste0("Residual of ", response, " ~ ."))

  if (what == "resid") {
    print(p)
    return(resid_data)
  } else if (what == "plot") {
    return(p)
  }
}


#' Multiple plot function for ggplot
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' @author www.cookbook-r.com
#' @details If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @export
#' @import grid
#'
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Build Bayesian ordered categorical linear model for estimation an ordered multinomial distribution
#'
#' @param feedback A data-frame column of ordered integer scores
#' @param outcomes A vector of the unique categories (e.g. c(1, 2, 3, 4))
#' @param cores The number of cores to use in the Stan sampler
#' @param iter The number of iterations to use in the Stan sampler
#' @param warmup The number of warmup iterations to use in the Stan sampler
#' @return The fitted `stanfit` model object
#' @export

buildModel <- function(feedback, outcomes, chains = 1, cores = 1, iter = 1000, warmup = 1000) {
    data <- list(obs = feedback, N = length(feedback))
    initial_values <- list(cutpoints = sort(outcomes)[-length(outcomes)])
    fit <- sampling(
        object = stanmodels$model,
        data = data,
        init = rep( list(initial_values), chains),
        chains = chains,
        cores = cores,
        iter = iter,
        warmup = warmup
    )
    return( fit )
}

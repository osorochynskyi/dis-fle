# Created by Oleksandr Sorochynskyi and fixed by Sebastien Meyer
# On 22/04/2023

#' Conditional survival time
#'
#' Residual survival time conditional on surviving past a certain point.
#'
#' @param (time, surv) coordinates defining the survival function
#' @param x time on which to condition
#' @return expected survival time, beyond `x`, conditional on surviving to `x`
#' @examples
#' library(survival)
#' library(purrr)
#'
#' y <- sample(c(0, 1, 2), size = 100e3, replace = TRUE, prob = rep(1 / 3, 3))
#' m <- survfit(Surv(y) ~ 1)
#' with(m, surv_time(time, surv))
#' # Check what is returned outsied observed times
#' x <- seq(-3, 3, length.out = 100)
#' ex <- with(m, surv_time(time, surv, x = x))
#' plot(x, ex)
#' lines(x, ex)
#'
#' y2 <- sample(
#'     c(1, 100),
#'     size = 100e3,
#'     replace = TRUE,
#'     prob = c(0.9, 0.1)
#' )
#' m2 <- survfit(Surv(y2) ~ 1)
#' with(m2, surv_time(time, surv, x = c(-1, 0, 1)))
#' # Explicit expected value calculations
#' map_dbl(c(-1, 0, 1), \(z) mean(y2[y2 > z]))
#'
#'
#' y3 <- sample(
#'     c(1, 2, 100),
#'     size = 100e3,
#'     replace = TRUE,
#'     prob = c(0.9, 0.05, 0.05)
#' )
#' m3 <- survfit(Surv(y3) ~ 1)
#' with(m3, surv_time(time, surv, x = c(-1, 0, 1, 2)))
#' # Explicit expected value calculations
#' map_dbl(c(-1, 0, 1), \(z) mean(y3[y3 > z]))
#'
#'
#' y4 <- sample(
#'     -100:100,
#'     size = 100e3,
#'     replace = TRUE
#' )
#' m4 <- survfit(Surv(y4) ~ 1)
#' plot(-150:150, with(m4, surv_time(time, surv, x = -150:150)))
#' # Explicit expected value calculations
#' map_dbl(-150:150, \(z) mean(y4[y4 > z]))
surv_time <- function(time, surv, x = time, maxtime = max(time)) {
    # Force evaluation of x and maxtime before time's length changes
    force(x)
    force(maxtime)

    if (anyDuplicated(time)) {
        warning(
            "Duplicated times in survival function.\n",
            "Are you sure you are not mixing two survival functions?\n\n"
        )
    }

    if (surv[which.max(time)] != 0 && maxtime == max(time)) {
        warning(
            "Survival curve improper, likely due to longest observation being ",
            "censored.\n",
            "Consider correcting the curve, or setting the `maxtime` ",
            "option.\n\n"
        )
    }

    if (!missing(maxtime)) {
        idx <- time < maxtime

        surv <- c(surv[idx], 0)
        time <- c(time[idx], maxtime)
    }

    prob_eq_k_plus_1 <- c(-diff(surv), 0)
    prob_gt_k <- surv
    t_plus_1 <- c(time[-1], 0)
    expect <- rev(cumsum(rev(t_plus_1 * prob_eq_k_plus_1))) / prob_gt_k
    full_expectancy <- sum(time * -diff(c(1, surv)))

    if (length(na.omit(expect)) < 2) {
        return(
            ifelse(x < min(time), full_expectancy,
                ifelse(x >= maxtime, NaN,
                    expect
                )
            )
        )
    }

    result <- approx(
        time,
        expect,
        method = "linear",
        rule = 1,
        xout = x,
        yleft = full_expectancy,
        yright = NaN
    )$y

    result
}

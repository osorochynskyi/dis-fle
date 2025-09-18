# Created by Oleksandr Sorochynskyi
# On 15/02/2023

#' Summarize a `survfit.object` with a data.frame
#'
#' Does the same thing as `broom::tidy.surfit.object` but has different
#' column names.
tidy_surv <- function(surv) {
    if (!is.null(surv$states)) {
        num_states <- length(surv$states)
    } else {
        num_states <- 1
    }
    num_times <- length(surv$time)

    # Some components of `surv` may be NULL bat that's OK, the corresponding
    # column in `surv_df` just won't be defined
    surv_df <- tibble::tibble(
        time = rep(surv$time, num_states),
        n.risk = as.vector(surv$n.risk),
        n.event = as.vector(surv$n.event),
        n.censor = rep(surv$n.censor, num_states),
        std.err = as.vector(surv$std.err),
        lower = as.vector(surv$lower),
        upper = as.vector(surv$upper),
        state = rep(surv$states, rep(num_times, num_states)),
        strata = rep(rep(names(surv$strata), surv$strata), num_states),
        pstate = as.vector(surv$pstate),
        cumhaz = as.vector(surv$cumhaz),
        surv = as.vector(surv$surv),
    )

    surv_df
}

#' Extract `cumhaz` component from a `survfit.object`
#'
#' Extract the `cumhaz` component as a data.frame.
tidy_surv_cumhaz <- function(surv) {
    tibble::tibble(
        time = surv$time,
        strata = rep(names(surv$strata), surv$strata),
        as_tibble(surv$cumhaz)
    ) %>%
        pivot_longer(
            cols = -c(time, strata),
            names_to = "transition",
            values_to = "cumhaz"
        ) %>%
        mutate(
            from = str_split_fixed(transition, "\\.", 2)[, 1],
            to = str_split_fixed(transition, "\\.", 2)[, 2],
            transition = NULL,
            from = as.integer(from),
            to = as.integer(to),
            from = factor(surv$states[from], levels = surv$states),
            to = surv$states[to],
        )
}

tidy_surv_exper <- function(surv) {
    if (FALSE) {
        cox_test <- coxph(
            Surv(debut_obs_age, fin_obs_age, factor(sample(1:5, size = 10e3, replace = TRUE))) ~ sex,
            data = tar_read(health) %>% slice_sample(n = 10e3),
            id = id
        )
        sct <- survfit(
            cox_test,
            newdata = data.frame(sex = c("F", "M"))
        )
    }
    if (!is.null(surv$states)) {
        num_states <- length(surv$states)
    } else {
        num_states <- 1
    }
    num_times <- length(surv$time)

    surv_df <- tibble::tibble(
        time = rep(surv$time, num_states),
        n.risk = as.vector(surv$n.risk),
        n.event = as.vector(surv$n.event),
        n.censor = rep(surv$n.censor, num_states),
        std.err = as.vector(surv$std.err),
        lower = as.vector(surv$lower),
        upper = as.vector(surv$upper),
        state = rep(surv$states, rep(num_times, num_states)),
        strata = rep(rep(names(surv$strata), surv$strata), num_states)
    )

    # num_pred is the number of lines in `newdata` data.frame
    if (!is.null(surv$surv)) {
        num_pred <- dim(surv$surv)[2]
    } else if (length(dim(surv$pstate)) == 3) {
        num_pred <- dim(surv$pstate)[2]
    } else {
        num_pred <- 1
    }
    num_times <- length(surv$time)

    if (is.null(surv$strata)) {
        strata <- rep(1, num_pred)
    }

    # Add initial state to cumhas
    dim_cum_haz <- dim(surv$cumhaz)
    dim_cum_haz[3] <- dim_cum_haz[3] + 1
    cumhaz <- array(NA, dim = dim_cum_haz)
    cumhaz[, , seq_len(num_states - 1) + 1] <- surv$cumhaz
    cumhaz[, , 1] <- NA

    # Order of arguments is important, later arguments run faster
    surv_df <- expand_grid(
        state_idx = seq_len(num_states),
        pred_idx = seq_len(num_pred),
        time_idx = seq_len(num_times),
    ) %>%
        transmute(
            time = surv$time[time_idx],
            pred = pred_idx,
            state = surv$states[state_idx],
            n.risk = surv$n.risk[time_idx, pred_idx],
            n.event = surv$n.event[time_idx, pred_idx],
            n.censor = surv$n.censor[time_idx],
            std.err = surv$n.censor[time_idx],
            strata = NA,
            cumhaz = as.vector(cumhaz),
            pstate = as.vector(surv$pstate),
            surv = as.vector(surv$surv),
            std.err = as.vector(surv$std.err),
            lower = as.vector(surv$lower),
            upper = as.vector(surv$upper),
        )

    if (FALSE) {
        surv_df <- tibble::tibble(
            time = rep(surv$time, num_pred * num_states),
            n.risk = rep(as.vector(surv$n.risk), num_pred),
            n.event = rep(as.vector(surv$n.event), num_pred),
            n.censor = rep(surv$n.censor, num_pred * num_states),
            state = rep(surv$states, rep(num_times * num_pred, num_states)),
            strata = rep(rep(names(surv$strata), surv$strata), num_states),
            cumhaz = as.vector(surv$cumhaz),
            pstate = as.vector(surv$pstate),
            surv = as.vector(surv$surv),
            std.err = as.vector(surv$std.err),
            lower = as.vector(surv$lower),
            upper = as.vector(surv$upper),
        )
    }

    surv_df
}

tidy_aalen <- function(x, maxtime) {
    se <- TRUE
    type <- "s"

    # Adapted from plot.aalen
    if (!inherits(x, "aareg")) stop("Must be an aareg object")

    if (missing(maxtime)) {
        keep <- 1:length(x$time)
    } else {
        keep <- 1:sum(x$time <= maxtime)
    }

    yylab <- names(x$test.statistic)
    if (is.matrix(x$coefficient) && ncol(x$coefficient) > 1) {
        yy <- apply(x$coefficient[keep, ], 2, cumsum)
        yy <- rbind(0, yy) # make the plot start at 0,0
        if (se) {
            if (!is.null(x$dfbeta)) {
                # There was a cluster term, so use the robust variance
                #  dfbeta will be of dimension (n, nvar, n-unique-times)
                # The first variance increment is apply(dfbeta[,,1]^2,2,sum)
                #              second is          apply(dfbeta[,,2]^2,2,sum)
                #              ... , apply(dfbeta[,,ndeath].....
                # By being sneaky, it can be done quickly
                dd <- dim(x$dfbeta)
                keep2 <- 1:length(unique(x$time[keep]))
                temp <- matrix(x$dfbeta[, , keep2], nrow = dd[1])
                se.increment <- matrix(apply(temp^2, 2, sum), nrow = dd[2])
                se.yy <- sqrt(apply(t(se.increment), 2, cumsum))
            } else {
                se.yy <- sqrt(apply(x$coefficient[keep, ]^2, 2, cumsum))
            }
            se.yy <- rbind(0, se.yy)
        }
        ncurve <- ncol(yy)
    } else {
        # this is the branch most often called, when someone has done
        #   plot(fit[3]), so that only 1 coefficient remains
        yy <- cumsum(c(0, x$coefficient[keep]))
        if (se) {
            if (!is.null(x$dfbeta)) {
                dd <- dim(x$dfbeta)
                keep2 <- 1:length(unique(x$time[keep]))
                temp <- matrix(x$dfbeta[, , keep2], nrow = dd[1])
                se.yy <- sqrt(cumsum(c(0, apply(temp^2, 2, sum))))
            } else {
                se.yy <- sqrt(cumsum(c(0, x$coefficient[keep]^2)))
            }
        }
        ncurve <- 1
    }

    xx <- c(0, x$time[keep])

    # There may be multiplicities in x$times.  Only plot the last of
    #  each of them
    indx <- 1 + length(xx) - rev(match(unique(rev(xx)), rev(xx)))
    xx <- xx[indx]
    yy <- as.matrix(yy)[indx, ]

    tibble(
        term = rep(yylab, rep(length(xx), ncol(yy))),
        time = rep(xx, ncol(yy)),
        coef = as.vector(yy),
        coef_up = as.vector(yy + 1.96 * se.yy[indx, ]),
        coef_dn = as.vector(yy - 1.96 * se.yy[indx, ]),
    )
}

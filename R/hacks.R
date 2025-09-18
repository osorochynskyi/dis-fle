# Created by Oleksandr Sorochynskyi
# On 19/06/2023

#' Veribatim copy of survival::predict.coxph
#'
#' Exact copy of survival::predict.coxph, except it doesnt throw error "Data is
#' not the same size as it was in the original fit". In my case this error
#' is generated for no apperent reason, and functions works fine without it.
#'
#' Better approach would of course be to get to the bottom of why there is a
#' difference and report the bug ! (or fix my error)
my_predict_cox <- function(object, newdata,
                           type = c("lp", "risk", "expected", "terms", "survival"),
                           se.fit = FALSE, na.action = na.pass,
                           terms = names(object$assign), collapse,
                           reference = c("strata", "sample", "zero"), ...) {
    if (!inherits(object, "coxph")) {
        stop("Primary argument much be a coxph object")
    }

    Call <- match.call()
    type <- match.arg(type)
    if (type == "survival") {
        survival <- TRUE
        type <- "expected" # this is to stop lots of "or" statements
    } else {
        survival <- FALSE
    }

    n <- object$n
    Terms <- object$terms

    if (!missing(terms)) {
        if (is.numeric(terms)) {
            if (any(terms != floor(terms) |
                terms > length(object$assign) |
                terms < 1)) {
                stop("Invalid terms argument")
            }
        } else if (any(is.na(match(terms, names(object$assign))))) {
            stop("a name given in the terms argument not found in the model")
        }
    }

    # I will never need the cluster argument, if present delete it.
    #  Terms2 are terms I need for the newdata (if present), y is only
    #  needed there if type == 'expected'
    if (length(attr(Terms, "specials")$cluster)) {
        temp <- untangle.specials(Terms, "cluster", 1)
        Terms <- drop.special(Terms, attr(Terms, "specials")$cluster)
    }

    if (type != "expected") {
        Terms2 <- delete.response(Terms)
    } else {
        Terms2 <- Terms
    }

    has.strata <- !is.null(attr(Terms, "specials")$strata)
    has.offset <- !is.null(attr(Terms, "offset"))
    has.weights <- any(names(object$call) == "weights")
    na.action.used <- object$na.action
    n <- length(object$residuals)

    if (missing(reference) && type == "terms") {
        reference <- "sample"
    } else {
        reference <- match.arg(reference)
    }
    have.mf <- FALSE
    if (type == "expected") {
        y <- object[["y"]]
        if (is.null(y)) { # very rare case
            mf <- stats::model.frame(object)
            y <- model.extract(mf, "response")
            have.mf <- TRUE # for the logic a few lines below, avoid double work
        }
    }

    # This will be needed if there are strata, and is cheap to compute
    strat.term <- untangle.specials(Terms, "strata")
    if (se.fit || type == "terms" || (!missing(newdata) && type == "expected") ||
        (has.strata && (reference == "strata") || type == "expected") ||
        (reference == "zero" && any(object[["means"]] != 0))) {
        use.x <- TRUE
        if (is.null(object[["x"]]) || has.weights || has.offset ||
            (has.strata && is.null(object$strata))) {
            # I need the original model frame
            if (!have.mf) mf <- stats::model.frame(object)
            if (nrow(mf) != n) {
                # There is a difference for no good reason !
                # stop("Data is not the same size as it was in the original fit")
                x <- model.matrix(object, data = mf)
            }
            if (has.strata) {
                if (!is.null(object$strata)) {
                    oldstrat <- object$strata
                } else {
                    if (length(strat.term$vars) == 1) {
                        oldstrat <- mf[[strat.term$vars]]
                    } else {
                        oldstrat <- strata(mf[, strat.term$vars], shortlabel = TRUE)
                    }
                }
            } else {
                oldstrat <- rep(0L, n)
            }

            weights <- model.weights(mf)
            if (is.null(weights)) weights <- rep(1.0, n)
            offset <- model.offset(mf)
            if (is.null(offset)) offset <- 0
        } else {
            x <- object[["x"]]
            if (has.strata) {
                oldstrat <- object$strata
            } else {
                oldstrat <- rep(0L, n)
            }
            weights <- rep(1., n)
            offset <- 0
        }
    } else {
        # I won't need strata in this case either
        if (has.strata) {
            Terms2 <- drop.special(Terms2, attr(Terms2, "specials")$strata)
            has.strata <- FALSE # remaining routine never needs to look
        }
        oldstrat <- rep(0L, n)
        offset <- 0
        use.x <- FALSE
    }
    if (!missing(newdata)) {
        use.x <- TRUE # we do use an X matrix later
        tcall <- Call[c(1, match(c("newdata", "collapse"), names(Call), nomatch = 0))]
        names(tcall)[2] <- "data" # rename newdata to data
        tcall$formula <- Terms2 # version with no response
        tcall$na.action <- na.action # always present, since there is a default
        tcall[[1L]] <- quote(stats::model.frame) # change the function called

        if (!is.null(attr(Terms, "specials")$strata) && !has.strata) {
            temp.lev <- object$xlevels
            temp.lev[strat.term$vars] <- NULL
            tcall$xlev <- temp.lev
        } else {
            tcall$xlev <- object$xlevels
        }
        mf2 <- eval(tcall, parent.frame())

        collapse <- model.extract(mf2, "collapse")
        n2 <- nrow(mf2)

        if (has.strata) {
            if (length(strat.term$vars) == 1) {
                newstrat <- mf2[[strat.term$vars]]
            } else {
                newstrat <- strata(mf2[, strat.term$vars], shortlabel = TRUE)
            }
            if (any(is.na(match(levels(newstrat), levels(oldstrat))))) {
                stop("New data has a strata not found in the original model")
            } else {
                newstrat <- factor(newstrat, levels = levels(oldstrat))
            } # give it all
            if (length(strat.term$terms)) {
                newx <- model.matrix(Terms2[-strat.term$terms], mf2,
                    contr = object$contrasts
                )[, -1, drop = FALSE]
            } else {
                newx <- model.matrix(Terms2, mf2,
                    contr = object$contrasts
                )[, -1, drop = FALSE]
            }
        } else {
            newx <- model.matrix(Terms2, mf2,
                contr = object$contrasts
            )[, -1, drop = FALSE]
            newstrat <- rep(0L, nrow(mf2))
        }

        newoffset <- model.offset(mf2)
        if (is.null(newoffset)) newoffset <- 0
        if (type == "expected") {
            newy <- model.response(mf2)
            if (attr(newy, "type") != attr(y, "type")) {
                stop("New data has a different survival type than the model")
            }
        }
        na.action.used <- attr(mf2, "na.action")
    } else {
        n2 <- n
    }
    if (type == "expected") {
        if (missing(newdata)) {
            pred <- y[, ncol(y)] - object$residuals
        }
        if (!missing(newdata) || se.fit) {
            ustrata <- unique(oldstrat)
            risk <- exp(object$linear.predictors)
            x <- x - rep(object$means, each = nrow(x)) # subtract from each column
            if (missing(newdata)) { # se.fit must be true
                se <- double(n)
            } else {
                pred <- se <- double(nrow(mf2))
                newx <- newx - rep(object$means, each = nrow(newx))
                newrisk <- c(exp(newx %*% object$coef) + newoffset)
            }

            survtype <- ifelse(object$method == "efron", 3, 2)
            for (i in ustrata) {
                indx <- which(oldstrat == i)
                afit <- agsurv(
                    y[indx, , drop = F], x[indx, , drop = F],
                    weights[indx], risk[indx],
                    survtype, survtype
                )
                afit.n <- length(afit$time)
                if (missing(newdata)) {
                    # In this case we need se.fit, nothing else
                    j1 <- approx(afit$time, 1:afit.n, y[indx, 1],
                        method = "constant",
                        f = 0, yleft = 0, yright = afit.n
                    )$y
                    chaz <- c(0, afit$cumhaz)[j1 + 1]
                    varh <- c(0, cumsum(afit$varhaz))[j1 + 1]
                    xbar <- rbind(0, afit$xbar)[j1 + 1, , drop = F]
                    if (ncol(y) == 2) {
                        dt <- (chaz * x[indx, ]) - xbar
                        se[indx] <- sqrt(varh + rowSums((dt %*% object$var) * dt)) *
                            risk[indx]
                    } else {
                        j2 <- approx(afit$time, 1:afit.n, y[indx, 2],
                            method = "constant",
                            f = 0, yleft = 0, yright = afit.n
                        )$y
                        chaz2 <- c(0, afit$cumhaz)[j2 + 1]
                        varh2 <- c(0, cumsum(afit$varhaz))[j2 + 1]
                        xbar2 <- rbind(0, afit$xbar)[j2 + 1, , drop = F]
                        dt <- (chaz * x[indx, ]) - xbar
                        v1 <- varh + rowSums((dt %*% object$var) * dt)
                        dt2 <- (chaz2 * x[indx, ]) - xbar2
                        v2 <- varh2 + rowSums((dt2 %*% object$var) * dt2)
                        se[indx] <- sqrt(v2 - v1) * risk[indx]
                    }
                } else {
                    # there is new data
                    use.x <- TRUE
                    indx2 <- which(newstrat == i)
                    j1 <- approx(afit$time, 1:afit.n, newy[indx2, 1],
                        method = "constant", f = 0, yleft = 0, yright = afit.n
                    )$y
                    chaz <- c(0, afit$cumhaz)[j1 + 1]
                    pred[indx2] <- chaz * newrisk[indx2]
                    if (se.fit) {
                        varh <- c(0, cumsum(afit$varhaz))[j1 + 1]
                        xbar <- rbind(0, afit$xbar)[j1 + 1, , drop = F]
                    }
                    if (ncol(y) == 2) {
                        if (se.fit) {
                            dt <- (chaz * newx[indx2, ]) - xbar
                            se[indx2] <- sqrt(varh + rowSums((dt %*% object$var) * dt)) *
                                newrisk[indx2]
                        }
                    } else {
                        j2 <- approx(afit$time, 1:afit.n, newy[indx2, 2],
                            method = "constant", f = 0, yleft = 0, yright = afit.n
                        )$y
                        chaz2 <- approx(-afit$time, afit$cumhaz, -newy[indx2, 2],
                            method = "constant", rule = 2, f = 0
                        )$y
                        chaz2 <- c(0, afit$cumhaz)[j2 + 1]
                        pred[indx2] <- (chaz2 - chaz) * newrisk[indx2]

                        if (se.fit) {
                            varh2 <- c(0, cumsum(afit$varhaz))[j1 + 1]
                            xbar2 <- rbind(0, afit$xbar)[j1 + 1, , drop = F]
                            dt <- (chaz * newx[indx2, ]) - xbar
                            dt2 <- (chaz2 * newx[indx2, ]) - xbar2

                            v2 <- varh2 + rowSums((dt2 %*% object$var) * dt2)
                            v1 <- varh + rowSums((dt %*% object$var) * dt)
                            se[indx2] <- sqrt(v2 - v1) * risk[indx2]
                        }
                    }
                }
            }
        }
        if (survival) { # it actually was type= survival, do one more step
            if (se.fit) se <- se * exp(-pred)
            pred <- exp(-pred) # probablility of being in state 0
        }
    } else {
        if (is.null(object$coefficients)) {
            coef <- numeric(0)
        } else {
            # Replace any NA coefs with 0, to stop NA in the linear predictor
            coef <- ifelse(is.na(object$coefficients), 0, object$coefficients)
        }

        if (missing(newdata)) {
            offset <- offset - mean(offset)
            if (has.strata && any(is.na(oldstrat))) is.na(newx) <- is.na(oldstrat)
            if (has.strata && reference == "strata") {
                # We can't use as.integer(oldstrat) as an index, if oldstrat is
                #   a factor variable with unrepresented levels as.integer could
                #   give 1,2,5 for instance.
                xmeans <- rowsum(x * weights, oldstrat) / c(rowsum(weights, oldstrat))
                newx <- x - xmeans[match(oldstrat, row.names(xmeans)), ]
            } else if (use.x) {
                if (reference == "zero") {
                    newx <- x
                } else {
                    newx <- x - rep(object$means, each = nrow(x))
                }
            }
        } else {
            offset <- newoffset - mean(offset)
            if (has.strata && any(is.na(newstrat))) is.na(newx) <- is.na(newstrat)
            if (has.strata && reference == "strata") {
                xmeans <- rowsum(x * weights, oldstrat) / c(rowsum(weights, oldstrat))
                newx <- newx - xmeans[match(newstrat, row.names(xmeans)), ]
            } else if (reference != "zero") {
                newx <- newx - rep(object$means, each = nrow(newx))
            }
        }

        if (type == "lp" || type == "risk") {
            if (use.x) {
                pred <- drop(newx %*% coef) + offset
            } else {
                pred <- object$linear.predictors
            }
            if (se.fit) se <- sqrt(rowSums((newx %*% object$var) * newx))

            if (type == "risk") {
                pred <- exp(pred)
                if (se.fit) se <- se * sqrt(pred) # standard Taylor series approx
            }
        } else if (type == "terms") {
            asgn <- object$assign
            nterms <- length(asgn)
            pred <- matrix(ncol = nterms, nrow = NROW(newx))
            dimnames(pred) <- list(rownames(newx), names(asgn))
            if (se.fit) se <- pred

            for (i in 1:nterms) {
                tt <- asgn[[i]]
                tt <- tt[!is.na(object$coefficients[tt])]
                xtt <- newx[, tt, drop = F]
                pred[, i] <- xtt %*% object$coefficient[tt]
                if (se.fit) {
                    se[, i] <- sqrt(rowSums((xtt %*% object$var[tt, tt]) * xtt))
                }
            }
            pred <- pred[, terms, drop = F]
            if (se.fit) se <- se[, terms, drop = F]

            attr(pred, "constant") <- sum(object$coefficients * object$means, na.rm = T)
        }
    }
    if (type != "terms") {
        pred <- drop(pred)
        if (se.fit) se <- drop(se)
    }

    if (!is.null(na.action.used)) {
        pred <- napredict(na.action.used, pred)
        if (is.matrix(pred)) {
            n <- nrow(pred)
        } else {
            n <- length(pred)
        }
        if (se.fit) se <- napredict(na.action.used, se)
    }

    if (!missing(collapse) && !is.null(collapse)) {
        if (length(collapse) != n2) stop("Collapse vector is the wrong length")
        pred <- rowsum(pred, collapse) # in R, rowsum is a matrix, always
        if (se.fit) se <- sqrt(rowsum(se^2, collapse))
        if (type != "terms") {
            pred <- drop(pred)
            if (se.fit) se <- drop(se)
        }
    }

    if (se.fit) {
        list(fit = pred, se.fit = se)
    } else {
        pred
    }
}

#' Same as `gt::to_latex` but takes into account captions !
#'
#' (and ignores headders)
#' Also allows to wrap in floating enviroment
my_as_latex <- function(data, float = FALSE, env = "longtable") {
    gt:::stop_if_not_gt_tbl(data = data)
    data <- gt:::build_data(data = data, context = "latex")
    table_start <- gt:::create_table_start_l(data = data)
    capt <- data[["_options"]]$value[
        data[["_options"]]$parameter == "table_caption"
    ]
    if (!is.na(capt)) {
        heading_component <- sprintf(
            "\\caption{\n%s\n} \\\\ \n",
            paste(
                data[["_options"]]$value[
                    data[["_options"]]$parameter == "table_caption"
                ],
                collapse = ""
            )
        )
    } else {
        heading_component <- ""
    }
    columns_component <- gt:::create_columns_component_l(data = data)
    body_component <- gt:::create_body_component_l(data = data)
    footer_component <- gt:::create_footer_component_l(data = data)
    table_end <- gt:::create_table_end_l()

    # Change enviroment
    table_start <- str_replace(table_start, "longtable", env)
    table_end <- str_replace(table_end, "longtable", env)
    if (float) {
        table_start <- paste0("\\begin{table}", table_start, collapse = "\n")
        table_end <- paste0(table_end, "\\end{table}", collapse = "\n")
    }

    if (requireNamespace("rmarkdown", quietly = TRUE)) {
        latex_packages <- lapply(gt:::latex_packages(), rmarkdown::latex_dependency)
    } else {
        latex_packages <- NULL
    }
    knitr::asis_output(paste0(table_start, heading_component,
        columns_component, body_component, table_end, footer_component,
        collapse = ""
    ), meta = latex_packages)
}
# model <- tar_read(cox_health_formula3_tt1)
#
# cox_f3_tt1_times <- seq(50, 105, by = 2) %>%
#     head(-1) %>%
#     tail(-1)
# newdat <- do.call(
#     expand_grid,
#     # Use xlevels from model without tt
#     model$xlevels
# ) %>%
#     filter(
#         fdr_obesity_cat3 == "0",
#         dep == "75",
#         immigration == "0",
#         diplome == "0",
#         # Only exaclty one of these three is TRUE
#         rowSums(
#             cbind(
#                 fdr_aud_cat3 != "0",
#                 sex == "M",
#                 fdr_smoker_cat3 != "0"
#             )
#         ) == 1,
#     ) %>%
#     rowwise() %>%
#     reframe(
#         across(everything(), \(x) rep(x, length(cox_f3_tt1_times))),
#         tgroup = seq_along(cox_f3_tt1_times),
#     )
#
# my_predict_cox(
#     model,
#     newdata = newdat,
#     se.fit = TRUE,
#     reference = "zero"
# )
#

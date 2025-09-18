
#' Tidy time dependent effects
#
#' Summarize time dependent effects in a data.frame. Only works for
#' `cox_health_*` models. Although might be useful in general.
tidy_health_tt <- function(m, times) {
    do.call(
        expand_grid,
        m$xlevels
    ) %>%
        filter(
            dep == "01",
            immigration == "0",
            diplome == "0",
            # Only exaclty one of these three is TRUE
            rowSums(
                cbind(
                    fdr_aud_cat3 != "0",
                    fdr_obesity_cat3 != "0",
                    fdr_smoker_cat3 != "0",
                    sex == "M"
                )
            ) == 1,
        ) %>%
        rowwise() %>%
        reframe(
            across(everything(), \(x) rep(x, length(times))),
            tgroup = seq_along(times),
            times = times
        ) %>%
        mutate(
            as_tibble(
                predict(
                    m,
                    newdata = .,
                    se.fit = TRUE,
                    reference = "zero",
                    type = "risk"
                )
            ),
            fit_up = fit + 1.96 * se.fit,
            fit_dn = fit - 1.96 * se.fit,
        )
}

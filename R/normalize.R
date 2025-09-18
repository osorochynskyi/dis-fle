# Created by Oleksandr Sorochynskyi
# On 09/01/2023

#' Normalize individual data
#'
#' Normalize the full data to a one-line-per-individual format.
#'
#' @param d a data.frame
#' @param imput logical, should imputation method be applied ?
#'
#' Certain unobserved death were imputed, this goes together with the some
#' "facteur de risque" (fdr) which were changed to reflect probable state (i.e.
#' "non-smoker" dying from lung cancer).
normalize_individu <- function(d, imput) {
    d <- d %>%
        dplyr::select(
            id, age_20080101, male,
            fdr_obesity_cat3, fdr_aud_cat3, fdr_smoker_cat3,
            cp_dep, cp_immi, cp_dipl0,
            fdr_smoker_cat3_imput, fdr_aud_cat3_imput
        )
    if (imput) {
        d$fdr_smoker_cat3 <- as.factor(d$fdr_smoker_cat3_imput)
        d$fdr_aud_cat3 <- as.factor(d$fdr_aud_cat3_imput)
    }
    d$fdr_smoker_cat3_imput <- NULL
    d$fdr_aud_cat3_imput <- NULL
    d %>%
        filter(!duplicated(id)) %>%
        dplyr::mutate(
            date_naissance = lubridate::ymd("2008-01-01") -
                lubridate::days(round(365.25 * age_20080101)),
            age_20080101 = NULL,
            sex = ifelse(male == 1L, "M", "F"),
            male = NULL,
        ) %>%
        dplyr::rename(
            dep = cp_dep,
            immigration = cp_immi,
            diplome = cp_dipl0
        )
}

#' Censor evenements table
#'
#' Replace all events after `date` by `"STATE2_PDV"` at `date`, and if there
#' are no other events before `date` add  `"STATE2_NO_PATHOL1"`.
#'
#' This function is used to censor data at 2012-12-31, but the method is
#' different from censoring applied by Michaël. For some reason, when there is
#' only a `"STATE2_NO_PATHOL1"` after `date` it is left as is. Whereas here
#' I chose to treat `"STATE2_NO_PATHOL1"` as any other event and censor it at
#' `date`, adding PDV and NO_PATHOL events. I justify this by thinking that this
#' individual was still exposed, even though we did not observe an event.
#' Perhaps this was done to avoid adding a ficticious "NO_PATHOL1".
#'
#' @param events data.frame with `id`, `date_event`, `event` columns
#' @param date date of censorship
#' @return censored `events` data.frame
censor_evenements <- function(events, date) {
    before_date <- events %>% filter(.data$date_event <= .env$date)
    censored_id <- unique(events$id[events$date_event > date])
    all_censored_id <- events$id[!events$id %in% before_date$id]
    pdv_df <- events %>%
        filter(id %in% censored_id) %>%
        # Take first row of every id
        filter(!duplicated(id)) %>%
        mutate(event = "STATE2_PDV", date_event = date, event_censor = 2)
    no_pathol1_df <- events %>%
        # filter(id %in% censored_id) %>%
        # mutate(.event_censored = date_event > date) %>%
        # group_by(id) %>%
        # mutate(.all_censored = all(.event_censored)) %>%
        # ungroup() %>%
        # filter(.all_censored) %>%
        # # Take first row of every id
        filter(id %in% all_censored_id) %>%
        filter(!duplicated(id)) %>%
        mutate(event = "STATE1_NO_PATHOL1", date_event = date, event_censor = 0)
    bind_rows(before_date, pdv_df, no_pathol1_df) %>%
        # select(-any_of(c(".all_censored", ".event_censored"))) %>%
        arrange(id, date_event)
}

#' Shift birthdays such that all first events are beyod the age of 50
#'
#' Shift birthays to guaratee minimal age of 50. Due to noise applied to the
#' dates somme events take place before the 50th birthday, and are promplty
#' excluded. This function shifts birthdays so that all events start at least
#' at the 50h birthday, plus some random quantity. This random quantity is a
#' scaled Beta(1,2) distribution, used due to its similarity to the overall
#' look of the noise applied.
#'
#' @param data data before normalization
#' @return A `data.frame` with columns `id`, and `date_naissance` with adjusted
#' birth date.
birthdates_beyond_50 <- function(d) {
    d %>%
        arrange(date_event, id) %>%
        filter(!duplicated(id)) %>%
        mutate(
            date_naissance = lubridate::ymd("2008-01-01") -
                lubridate::days(round(365.25 * age_20080101)),
            age_event = interval(date_naissance, date_event) / years(1),
            shift_bday = pmax(0, 50 - age_event),
            shift_bday = shift_bday +
                if_else(
                    shift_bday > 0,
                    (6 - shift_bday) * rbeta(n(), 1, 2),
                    0
                ),
            date_naissance = date_naissance - days(ceiling(365.25 * shift_bday))
        ) %>%
        select(id, date_naissance)
}

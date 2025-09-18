#' Calculate age of pathologies
#'
#' Given a list of events, calculte the time (age) of incidence of a pathology.
#' Certain events are considered 'ending', i.e. they end the observation period.
#' All individuals are considered to be observed from their 50th birthday,
#' until until the an 'ending' event event, but within the observation period
#' \code{interval(debut_obs, fin_obs)}. Overall the observation period is
#' \code{interval(pmin(debut_obs, 50th_birthday), pmax(date_fin, fin_obs))}.
#'
#' Lorsque il y a un événement observé avant le début du période d'observaiton
#' il nest pas pris en compte. Meme si on a un événement un mois avant le début
#' d'observation il n'est pas clair si tous les événements comme-ça étaient
#' prises en compte. Les événements après `date_fin` sont censurés à `date_fin`.
#'
#' The observation period is assumed to be half open, i.e. including
#' `debut_obs`, but excluding `fin_obs`.
#'
#' A half-day is added for all non-censored exit ages. This is to avoid having
#' equal ages for start and end for those who are exit on their entry date. It
#' also reflects the fact that such an individual could reasonably be assumed
#' to have been exposed for half a day.
#'
#' @param indi table with individuals
#' @param even table with events
#' @param patho the pathology that is being studied, in case of ties priority
#' is given to `patho` ealier in the list
#' @param pathos table with pathologies, notably which pathologies are to be
#' considered 'ending'
#' @param age_debut_obs age of the start of observation
#' @param debut_obs start of observation period
#' @param fin_obs end of observation period, note that a left-open iterval is
#' assumed
#' @param correct_same_day_exit Should half a day be added to same-day exits ?
#' @return a data.frame with one line per individual, with the following
#' new columns :
#' \begin{itemize}
#'      \item \code{patho_obs}
#'      \item \code{debut_obs_date}  and \code{debut_obs_age}
#'      \item \code{fin_obs_date}    and \code{fin_obs_age}
#' \end{itemize}
ages_patho <- function(indi,
                       even,
                       patho,
                       pathos,
                       age_debut_obs,
                       debut_obs,
                       fin_obs,
                       ...) {
    ages_patho_cr(
        indi,
        even,
        patho,
        pathos,
        age_debut_obs,
        debut_obs,
        fin_obs,
        ...
    ) %>%
        mutate(
            patho_obs = patho_obs != "STATE2_PDV"
        )
}

# For compettive risks
# patho peut etre un vecteur
# le premier risque dans patho gagne
ages_patho_cr <- function(indi,
                          even,
                          patho,
                          pathos,
                          age_debut_obs,
                          debut_obs,
                          fin_obs,
                          correct_same_day_exit = FALSE) {
    # rev to keep DECES before pdv
    ending_events <- rev(as.character(pathos$event[pathos$fin]))
    # rev because levels are given in increasing order
    levs <- rev(unique(c(patho, ending_events)))

    even %>%
        mutate(
            event = as.character(event),
            event = fct_other(
                event,
                keep = levs,
                other_level = "other"
            ),
            event = factor(event, levels = .env$levs, ordered = TRUE)
        ) %>%
        filter(
            event %in% levs,
            !id %in% id[date_event < debut_obs & event %in% ending_events],
        ) %>%
        arrange(date_event, event, id) %>%
        filter(!duplicated(id)) %>%
        inner_join(indi, ., by = "id") %>%
        dplyr::mutate(
            birthday_deb_obs = date_naissance %m+% years(age_debut_obs),
            debut_obs_date = pmax(debut_obs, birthday_deb_obs),
            birthday_deb_obs = NULL,
            patho_obs = if_else(date_event < fin_obs, event, "STATE2_PDV"),
            fin_obs_date = pmin(date_event, fin_obs),
            event = as.character(event)
        ) %>%
        mutate(
            dplyr::across(
                c(tidyselect::ends_with("_date")),
                list(
                    age = ~ lubridate::interval(date_naissance, .) /
                        lubridate::years(1)
                )
            )
        ) %>%
        dplyr::rename_with(
            .cols = tidyselect::ends_with("_date_age"),
            .fn = stringr::str_replace, "_date_age$", "_age"
        ) %>%
        mutate(
            # Add a half-day to avoid same-day exits
            fin_obs_age = fin_obs_age +
                if_else(correct_same_day_exit, 1 / (2 * 365.25), 0)
        )
}



#
ages_patho_long <- function(indi, even, pathos, debut_obs, fin_obs) {
    warning("Work in progress.")
    ending_events <- as.character(pathos$event[pathos$fin])
    censoring_event <- as.character(pathos$event[pathos$censure])
    even %>%
        mutate(
            event = factor(event),
            event = fct_relevel(event, censoring_event, after = 0L),
        ) %>%
        group_by(id) %>%
        arrange(date_event) %>%
        mutate(
            t1 = dplyr::lag(date_event, 1),
            t2 = if_else(
                date_event == max(date_event) & !event %in% ending_events,
                fin_obs,
                date_event
            ),
        ) %>%
        ungroup() %>%
        dplyr::inner_join(indi, by = "id") %>%
        mutate(
            event = relevel(event, ref = censoring_event),
            t1 = replace_na(t1, debut_obs),
            dplyr::across(
                c(t1, t2),
                list(age = ~ lubridate::interval(date_naissance, .) /
                    lubridate::years(1))
            )
        ) %>%
        arrange(id, date_event)
}

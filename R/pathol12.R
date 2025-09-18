#' Determine the occurence of pathol 1 or 2
#'
#' We define good health as the absence of pathol 1 or 2, which this function
#' helpfully calcultes.
#' @param even `evenement` data.frame
#' @return an `evenement` dataframe but only with "pathol12" or "STATE2_PDV"
#' events, whichever one comes first.
pathol12 <- function(even, pathos) {
    even %>%
        filter(!event %in% "STATE1_NO_PATHOL1") %>%
        mutate(
            event = fct_other(
                event,
                drop = c(
                    .env$pathos$event[.env$pathos$pathol == "1"],
                    "STATE2_DECES"
                ),
                other_level = "pathol12"
            ),
            event = fct_drop(event)
        ) %>%
        arrange(date_event, id) %>%
        filter(!duplicated(id))
}

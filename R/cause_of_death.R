
#' Determine the cause of death
#'
#' Determine the cause of death, defined as the last pathology before death.
#' Vast majority of cases have some pathology before "STATE2_DECES".
#'
#' @param events character vector of events.
#' @param dates date vector of event dates.
#' @return The event having caused death, i.e., the event one before last.
death_cause <- function(events, dates) {
    stopifnot(length(events) == length(dates))
    if (is.unsorted(dates)) {
        events <- events[order(dates, decreasing = FALSE)]
    }
    if (length(events) < 2 || tail(events, 1) != "STATE2_DECES") {
        return(NA)
    }
    events[length(events) - 1]
}

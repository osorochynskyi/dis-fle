# Created by Oleksandr Sorochynskyi
# On 19/10/2022

load_benchmark <- function(path) {
    new <- load(path)
    stopifnot(length(new) == 1)
    tibble::as_tibble(dplyr::mutate(get(new), file = basename(path)))
}

load_csv <- function(path) {
    read_csv(
        path,
        col_types = cols(
            DATE_EVENT = col_date(format = "%m/%d/%Y"),
            EVENT = col_character(),
            EVENT_CENSOR = col_integer(),
            Age_20080101 = col_double(),
            MALE = col_character(),
            ID = col_integer(),
            FDR_AUD_ALL = col_character(),
            FDR_OBESITY_CAT3 = col_character(),
            FDR_OBESITY_ALL = col_character(),
            CP_DEP = col_character(),
            CP_IMMI = col_character(),
            CP_DIPL0 = col_character(),
            PROB_HOSP0813 = col_double(),
            CP_REG2 = col_character(),
            FDR_SMOKER = col_character(),
            Date0_PATHOL1 = col_date(format = "%m/%d/%Y"),
            FDR_AUD_CAT3 = col_character(),
            FDR_SMOKER_CAT3 = col_character()
        )
    ) %>%
        rename_with(str_to_lower)
}

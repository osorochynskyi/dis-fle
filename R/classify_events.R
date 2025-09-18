# Created by Oleksandr Sorochynskyi
# On 30/01/2023

classify_events <- function(e) {
    forcats::fct_count(e, sort = TRUE) %>%
        dplyr::rename(event = f) %>%
        dplyr::mutate(
            fin = event %in% c("STATE2_PDV", "STATE2_DECES"),
            type = stringr::str_split_fixed(event, "_", 2)[, 1],
            all_cancers = event %in% c(
                "CANCER1_AUD_SMOKER", "CANCER1_COLORECTAL",
                "CANCER1_HEMATO", "CANCER1_PC_GOOD",
                "CANCER1_PC_POOR", "CANCER1_SMOKER",
                "CANCER1_BREAST", "CANCER1_PROSTATE"
            ),
            cancer_poor = event %in% c(
                "CANCER1_AUD_SMOKER", "CANCER1_PC_POOR", "CANCER1_SMOKER"
            ),
            cancer_good = event %in% c(
                "CANCER1_BREAST", "CANCER1_COLORECTAL", "CANCER1_HEMATO",
                "CANCER1_PC_GOOD", "CANCER1_PROSTATE"
            ),
            vasc = event %in% c(
                "CARDIO1_IHD_1MI", "CV1_PVD", "STROKE1_1HEMO",
                "STROKE1_1ISCHEMIC"
            ),
            vasc_other = event %in% c(
                "CV1_PVD", "STROKE1_1HEMO", "STROKE1_1ISCHEMIC"
            ),
            organ_failure = event %in% c(
                "CARDIO1_INSUF_CHRO", "DIG1_LIVER_1CirrD",
                "KIDNEY1_1INSUF_CHRO", "RESP1_1INSUF_CHRO"
            ),
            organ_other = event %in% c(
                "DIG1_LIVER_1CirrD", "KIDNEY1_1INSUF_CHRO", "RESP1_1INSUF_CHRO"
            )
        )
}

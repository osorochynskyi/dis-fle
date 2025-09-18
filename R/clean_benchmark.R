#' clean_benchmark
#'
#' Fonction qui permet de mettre en forme l'objet benchmark pour appliquer le
#' modele de Cox il s'agit d'une serie de retraitement a appliquer
#'
#' Cette version de la fonction voit quelques certains étapes, jugés ne plus
#' être d'actualité, supprimés. Nottament les retraitements relatif à la
#' probabilité d'hospitalisation et l'imputation relatifs au "type_1" et
#' "type_3". Uniqueemnt le "type_0" a été retenu dans cette fonction. Or,
#' des retraitements sonts faits par ailleurs qui permettent de retrouver
#' l'imputation "type_2".
#'
#' En fin de compte le seul retraitement appliqué est de considérer que
#' fdr_smoker_cat3 == 1 correspond à fdr_smoker == 0.
#'
#' @param benchmark donnees 'benchmark'
#' @param correc_fumeur indicatrice de la correction fumeur
#' @param ipw_name nom de la variable qui comporte le poids pour correctino poids
#' @param PROB_IPW lien vers la constante PROB_IPW
#' @param TAB_DEP lien vers la constante TAB_DEP
#' @param TAB_OUTCOMES_ORIGINAL lien vers la constante TAB_OUTCOMES_ORIGINAL
#' @param ID_CORR_DEM_SEV lien vers ID_CORR_DEM_SEV
#' @author Quentin Guibert, Oleksandr Sorochynskyi
#' @return benchmark mis a jour
clean_benchmark <- function(benchmark,
                            correc_fumeur = FALSE,
                            ipw_name = NULL, # Correction poids
                            TAB_OUTCOMES_ORIGINAL = NULL, # Toujours correction poids
                            TAB_DEP,
                            ID_CORR_DEM_SEV = NULL) {
    # Retraitement de la variable fumeur fdr_smoker_cat3 == "0" qui est mal definies
    benchmark$fdr_smoker_cat3 <- as.character(benchmark$fdr_smoker_cat3)
    benchmark$fdr_smoker <- as.character(benchmark$fdr_smoker)

    benchmark$fdr_smoker <- replace(
        benchmark$fdr_smoker,
        benchmark$fdr_smoker_cat3 == "1",
        "0"
    )

    # Correction eventuelle des fumeurs avec BPCO
    if (correc_fumeur) {
        # Individus avec BPCO
        ids_bpco <- unique(benchmark$id[which(benchmark$RESP0_2COPD > 0)])
        row_fdr_smoker <- which(
            benchmark$id %in% ids_bpco & benchmark$fdr_smoker == "0"
        )
        row_fdr_smoker_cat3 <- which(
            benchmark$id %in% ids_bpco & benchmark$fdr_smoker_cat3 == "0"
        )

        # Modification des covariables fumeurs
        benchmark$fdr_smoker[row_fdr_smoker] <- "1"
        benchmark$fdr_smoker[row_fdr_smoker_cat3] <- "2"
    }

    # Correction eventuelle des poids
    if (!is.null(ipw_name)) {
        # Conversion temporaire en character (pas fait avant car les donnees ont deja ete generees)
        benchmark[, ipw_name] <- as.character(benchmark[, ipw_name])

        # Individus avec au moins un PATHOL1
        ids_pathol <- unique(benchmark$id[which(
            benchmark$event %in% TAB_OUTCOMES_ORIGINAL[which(TAB_OUTCOMES_ORIGINAL != "STATE2_DECES")]
        )])
        row_pathol <- which(benchmark$id %in% ids_pathol)


        # Modification des poids
        if (length(row_pathol) > 0) {
            benchmark[row_pathol, ipw_name] <- "1"
        }

        # Reconversion temporaire en facteur
        benchmark[, ipw_name] <- as.factor(benchmark[, ipw_name])
    }

    # Mise a jour de la region
    temp_dep <- data.frame(cp_dep = benchmark$cp_dep)
    temp_dep <- inner_join(temp_dep, filter(TAB_DEP, sexe == "M"), by = "cp_dep")
    benchmark$cp_reg <- temp_dep$cp_reg_3
    rm(temp_dep)

    if (!is.null(ID_CORR_DEM_SEV)) {
        # 19/09/2019 Correction des dates de DEM_SEV si DEM_SEV anterieur a DEM_ANY
        benchmark <- left_join(benchmark, ID_CORR_DEM_SEV, by = "id", suffix = c("", ".y"))
        temp <- interval(benchmark$date_event.y, dmy("01/01/2008")) / years(1) + benchmark$age_20080101

        # Correction de date_event (a priori n est pas utile)
        row <- which(!(is.na(temp)) & (benchmark$event == "STATE1_DEM_SEVERE"))
        benchmark$date_event[row] <- benchmark$date_event.y[row]

        # Correction des dates STATE1_DEM_SEVERE, age_pathol1
        row_0 <- which(!(is.na(temp)) & (benchmark$STATE1_DEM_SEVERE > 0) & (benchmark$NEURO1_DEM_ANY == 0))
        benchmark$STATE1_DEM_SEVERE[row_0] <- 0
        row <- which(!(is.na(temp)) & (benchmark$STATE1_DEM_SEVERE > 0) & (benchmark$NEURO1_DEM_ANY > 0))
        benchmark$STATE1_DEM_SEVERE[row] <- temp[row]


        row_age_pathol1 <- which(!(is.na(temp)) & (benchmark$STATE1_DEM_SEVERE > 0) &
            (benchmark$STATE1_DEM_SEVERE == benchmark$age_pathol1))
        benchmark$age_pathol1[row_age_pathol1] <- temp[row_age_pathol1]

        # Correction de time1 et time2
        row_time2 <- row[which(benchmark$STATE1_DEM_SEVERE[row - 1] == 0 & (benchmark$id[row] == benchmark$id[row - 1]))]
        benchmark$time2[row_time2] <- temp[row_time2]

        row_time1 <- row[which(benchmark$t_last[row] == 0 & benchmark$STATE1_DEM_SEVERE[row - 1] == 0 &
            (benchmark$id[row] == benchmark$id[row - 1]))]
        benchmark$time1[row_time1 + 1] <- benchmark$time2[row_time1]

        # Colonne inutile
        benchmark$event.y <- NULL
        benchmark$date_event.y <- NULL
    }

    benchmark
}

#' Ajouter le premier evenement
#'
#' `date0_hopital` donne la date du premier pathol0 observé. Or lorsqu'il y a
#' d'autres pathol1 observés, cette date est supprimé. Cette fonction rajoute
#' un événement "STATE1_NO_PATHOL" lorsque celui-ci n'aparait pas.
add_first_pathol <- function(events, dates_hopital_df) {
    date0 <- tapply(events$date_event, events$id, min) %>%
        enframe(name = "id", value = "date0_obs") %>%
        mutate(id = as.integer(id)) %>%
        left_join(dates_hopital_df, by = c("id")) %>%
        filter(date0_hopital < date0_obs)

    new_pathol0 <- events %>%
        filter(id %in% date0$id) %>%
        arrange(date_event) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(
            event = "STATE1_NO_PATHOL1",
            date_event = dates_hopital_df$date0_hopital[
                match(id, dates_hopital_df$id)
            ],
            event_censor = 0
        )

    events %>%
        bind_rows(new_pathol0) %>%
        arrange(id, date_event, event_censor)
}

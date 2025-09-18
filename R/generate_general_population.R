# Created by Oleksandr Sorochynskyi
# On 11/05/2023

#' Generate a sample of individuals not included in the databse
#'
#' A large percentage of the population is not included in the databse. This
#' makes it difficult to generalize results to the entire population. As a
#' way to adjust exposure to the entire France I generate data for those who
#' are missing. I assume that if no information is available then that
#' individual is healthy.
#'
#' First I determine the number of people missing, by calculating the observed
#' poupluation on 1st of Janruary by sex and age.
#' I assume everyone is exposed form their
#' 50th birthday to first of either their death or 2013-12-31.
#' The number of missing individuals is determined
#' by difference with INSEE age pyramid. Note that I ignore the censored
#' state "STATE2_PDV", i.e. even when individual is censored I assume he/she
#' was A-OK.
#'
#' I add individuals by cohort. I generate only as many individuals as there
#' are missing in 2010 for a cohort. This is because under my assumptions the
#' number of exposed people cannot increase. This is almost, but not completely
#' true, at least because there is migration.
#' For example if for the 1940 cohort there are 100,
#' 200, 50, 300 individuals missing for 70, 71, 72, and 73 years olds then
#' I will only add 50 individuals that will be censored only at the end.
#' If the number of missing individuals decreases, I censor the chort
#' accordingly. This guaratees that I will not add too many people.
#'
#' @param pyarmaid data.frame with populations by age, and year
#' @param indi individus data.frame
#' @param even evenements data.frame
#' @param adjust_exclusions Should exclusions applied by Michaël be neutralized?
#' @return data.frame with the union of columns of individus, and events with
#' indivduals representing missing population from PMSI.
#'
#' If `adjust_exclusions = TRUE` then all observed volumes are increased by
#' 18440022/13170355 ~ 1.4. This is intended to reflect the fact that
#' increasing exposure to replace those who were included in PMSI (but not
#' this data) does not make sense and leads to over-estimation of health (
#' because too many people were added back in). This proportional increase
#' assumes that excluded population is "same" as the observed population.
#' Which is most likely not true, but better than assuming that they are
#' healthy.
compare_pmsi_to_general_population <- function(pyrmaid, even, indi, adjust_exclusions = FALSE) {
    # 18M is the number of individuals in original PMSI dataset
    exclusion_adj_coef <- 18440022 / nrow(indi)

    pop_pmsi <- even %>%
        filter(event == "STATE2_DECES") %>%
        right_join(indi, by = "id") %>%
        mutate(
            date_event = replace(
                date_event,
                is.na(date_event),
                ymd("2013-12-31")
            ),
            event = replace(
                event,
                is.na(event),
                "STATE2_PDV"
            )
        ) %>%
        group_by(sex) %>%
        reframe(population_1jan(id, date_naissance, date_event))

    if (adjust_exclusions) {
        pop_pmsi$pop <- round(exclusion_adj_coef * pop_pmsi$pop)
    }

    pyrmaid %>%
        filter(age >= 50, annee %in% c(2010:2013)) %>%
        inner_join(
            pop_pmsi,
            by = c(annee = "year", age = "age", sexe = "sex"),
            suffix = c("_insee", "_pmsi")
        ) %>%
        rename(year = annee, age = age, sex = sexe) %>%
        relocate(cohort, .after = age) %>%
        relocate(sex, .after = age)
}

generate_general_population <- function(missing_pop, id_start) {
    missing_pop %>%
        group_by(cohort, sex) %>%
        mutate(
            missing_pop = pmax(pop_insee - pop_pmsi, 0),
            # Missing pop décroissante : on ne peux pas ajouter des personnes
            # qui appareissent au premier RDV
            missing_pop_decr = cummin(missing_pop),
            exits = c(rev(diff(rev(missing_pop_decr))), NA),
            exits = replace_na(exits, missing_pop[which.min(age)] - sum(exits, na.rm = TRUE)),
        ) %>%
        # summarize(
        #     tot = missing_pop[which.min(age)],
        #     tot_exits = sum(exits)
        # ) %>%
        #     filter(tot != tot_exits)
        reframe(
            date_naissance = ymd(paste(rep(cohort, exits) - 1, "-01-01")) +
                days(round(365.25 * runif(sum(exits)))),
            date_naissance = date_naissance %>%
                pmin(ymd(paste(rep(cohort, exits) - 1, "-12-31"))) %>%
                # 01-02, because otherwise those born on the 1st wil be too young
                pmax(ymd(paste(rep(cohort, exits) - 1, "-01-02"))),
            date_event = ymd(paste(rep(year, exits), "-12-31")),
            event = "STATE2_PDV",
            sex = rep(sex, exits),
        ) %>%
        mutate(id = id_start + seq_len(n())) %>%
        relocate(id, .before = everything())
}

#' Count the populatin on the first of January
#'
#' By convention population pyramids present the population on the 1st of
#' Januruary of each year. In order to compare population counts we have to
#' do the same. This crutially counts an individual mutliple times if he is
#' exposed over multiple years.
#'
#' Assumes all individuals are exposed from their 50th birthday to 31st of
#' December 2013.
#'
#' @param id vector identifiers, necessairy for joins later
#' @param brith birth dates
#' @param exit date of end of exposure, presumably death or end of observation
#' @return data.frame with columns `year`, `age`, `cohort`, `pop`. `cohort` is
#' just the difference between year and age.
population_1jan <- function(id, birth, exit) {
    age_prefix <- ".age_"
    presence_prefix <- ".presence_"
    tibble(
        id = id,
        aniv50 = birth %m+% years(50),
        map_dfc(
            set_names(2010:2013),
            ~ ymd(paste0(., "-01-01")) %within% interval(aniv50, exit)
        ) %>%
            rename_with(~ paste0(presence_prefix, .)),
        map_dfc(
            set_names(2010:2013),
            ~ floor(interval(birth, ymd(paste0(., "-01-01"))) / years(1))
        ) %>%
            rename_with(~ paste0(age_prefix, .)),
    ) %>%
        pivot_longer(
            cols = starts_with(c(presence_prefix, age_prefix)),
            names_to = c(".value", "year"),
            names_pattern = ".(presence|age)_([0-9]{4})"
        ) %>%
        mutate(age = replace(age, age > 99, 99)) %>%
        group_by(year = as.numeric(year), age) %>%
        summarize(pop = sum(presence), .groups = "drop") %>%
        mutate(cohort = year - age) %>%
        relocate(cohort, .after = age)
}

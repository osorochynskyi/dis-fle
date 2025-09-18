# Created by Oleksandr Sorochynskyi
# On 19/10/2022

library(targets)
library(purrr)

required <- c(
    "conflicted",
    "tidyverse",
    "lubridate",
    "tarchetypes",
    "magrittr",
    "knitr",
    "kableExtra",
    "bookdown",
    "scales",
    "glmnet",
    "ranger",
    "wesanderson",
    "openxlsx",
    "survival",
    "data.table",
    "dtplyr",
    "broom",
    "readxl",
    "rlang",
    "gt",
    "sf"
)

walk(
    required,
    ~ suppressPackageStartupMessages(
        suppressWarnings(
            library(., character.only = TRUE, quietly = TRUE)
        )
    )
)
walk(list.files("R", pattern = "*.R", full.names = TRUE), source)

conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("lag", "dplyr", quiet = TRUE)
conflict_prefer("discard", "purrr", quiet = TRUE)
conflict_prefer("year", "lubridate", quiet = TRUE)
conflict_prefer("month", "lubridate", quiet = TRUE)
conflict_prefer("day", "lubridate", quiet = TRUE)
conflict_prefer("today", "lubridate", quiet = TRUE)
conflict_prefer("first", "dplyr", quiet = TRUE)
set_names <- rlang::set_names

POP_SAMPLE_PROP <- 1

aux_data_import <- list(
    tar_target(
        benchmark_files,
        list.files(
            "data/benchmarks/02_Data_For_R",
            pattern = "Benchmark_DATE50_.*.RData",
            full.names = TRUE
        ),
        format = "file"
    ),
    tar_target(
        data_files,
        c(
            "data/benchmarks/02_Data_For_R/DATE50_F.csv",
            "data/benchmarks/02_Data_For_R/DATE50_M.csv"
        ),
        format = "file"
    ),
    tar_target(pathologies_file, "data/Patho.csv", format = "file"),
    tar_target(
        pathologies_desc,
        read_csv2(
            pathologies_file,
            col_types = cols(
                PATHOL = col_integer(),
                EVENT = col_character(),
                EVENT_EN = col_character(),
                Description = col_character(),
                Description_en = col_character(),
                `PATHO0 => PATHO1` = col_character(),
                Remarque = col_character()
            )
        )
    ),
    tar_target(
        imputation_fdr_file, "data/ID_IMPUT0812_FDR_20190918.csv",
        format = "file"
    ),
    tar_target(
        imputation_fdr,
        read_csv(
            imputation_fdr_file,
            col_types = cols(
                ID = col_integer(),
                FDR_AUD_CAT3 = col_integer(),
                FDR_SMOKER_CAT3 = col_integer()
            )
        ) %>%
            rename_with(str_to_lower)
    ),
    tar_target(
        correction_dem_sev_type_2_file,
        "data/DATE50_IMP0812_DEM_SEV_20190918.csv",
        format = "file"
    ),
    tar_target(
        correction_dem_sev_type_2,
        read_csv(
            correction_dem_sev_type_2_file,
            col_types = cols(
                ID = col_integer(),
                EVENT = col_character(),
                DATE_EVENT = col_date("%m/%d/%Y")
            )
        ) %>%
            rename_with(tolower)
    ),
    tar_target(
        codes_departement_file, "data/tab_dep_sexe.csv",
        format = "file"
    ),
    tar_target(
        codes_departement,
        read_csv2(
            codes_departement_file,
            col_types = cols(
                nom_fichier = col_character(),
                region2 = col_character(),
                region3 = col_character(),
                cp_reg = col_character(),
                cp_reg_2 = col_character(),
                cp_reg_3 = col_character(),
                cp_dep = col_character(),
                sexe = col_character()
            )
        ) %>%
            rename_with(str_to_lower)
    ),
    tar_target(
        date_hopital_file, "data/ID_Date0_HOPITAL.csv",
        format = "file"
    ),
    tar_target(
        date_hopital,
        read_csv(
            date_hopital_file,
            col_types = cols(
                ID = col_integer(),
                Date0_HOPITAL = col_date("%m/%d/%Y")
            )
        ) %>%
            rename_with(str_to_lower)
    ),
    tar_target(
        insee_age_pyramid_file,
        # Source : https://www.insee.fr/fr/statistiques/6327226?sommaire=6327254
        # Section : "Pyramides des âges > France Metropolitaine > Icône téléchargement"
        # Date : 15 may 2023
        "data/donnees_pyramide_act.csv",
        format = "file"
    ),
    tar_target(
        insee_age_pyramid,
        read_csv2(
            insee_age_pyramid_file,
            col_types = cols(
                ANNEE = col_double(),
                SEXE = col_character(),
                AGE = col_integer(),
                POP = col_integer()
            )
        ) %>%
            rename_with(str_to_lower) %>%
            mutate(pop = ceiling(pop * POP_SAMPLE_PROP))
    ),
    tar_target(
        # Source : https://ec.europa.eu/eurostat/databrowser/view/HLTH_HLYE/default/table?lang=en
        # Metadata available at https://ec.europa.eu/eurostat/cache/metadata/en/hlth_hlye_esms.htm
        # Dowloaded on 16 May 2023
        eurostat_hly_file,
        "data/hlth_hlye.tsv.gz"
    ),
    tar_target(
        eurostat_hly,
        read_eurostat_hly_data(eurostat_hly_file)
    )
)

benchmark_treatment <- list(
    tar_target(
        raw_data, map_dfr(data_files, load_csv),
        format = "fst_tbl"
    ),
    tar_target(
        raw_benchmarks, map_dfr(benchmark_files, load_benchmark),
        format = "fst_tbl"
    ),
    tar_target(
        data_before_norm_before_sample,
        raw_data %>%
            # Does almost nothing
            clean_benchmark(
                correc_fumeur = FALSE, # RESP0_2COPD toujours 0
                ipw_name = NULL, # Correction poids (proba hospitalisation)
                TAB_OUTCOMES_ORIGINAL = NULL, # Toujours correction poids
                TAB_DEP = codes_departement,
                ID_CORR_DEM_SEV = NULL # fichier avec corr. est avec mêmes dates
            ) %>%
            select(
                id, date_event, event, event_censor, age_20080101, male,
                fdr_aud_all, fdr_obesity_cat3, fdr_obesity_all, cp_dep, cp_immi,
                cp_dipl0, cp_reg, fdr_smoker, fdr_aud_cat3,
                fdr_smoker_cat3
            ) %>%
            left_join(imputation_fdr, by = "id", suffix = c("", "_imput")) %>%
            left_join(
                correction_dem_sev_type_2,
                by = c("id", "event"), suffix = c("", "_dem_imput")
            ) %>%
            add_first_pathol(date_hopital),
        format = "fst_tbl"
    ),
    tar_target(
        data_before_norm,
        data_before_norm_before_sample %>%
            filter(
                id %in% sample(
                    unique(id),
                    ceiling(n_distinct(id) * POP_SAMPLE_PROP)
                )
            ),
        format = "fst_tbl"
    ),
    tar_target(
        birthdays_shifted,
        birthdates_beyond_50(data_before_norm)
    ),
    tar_target(
        individu,
        data_before_norm %>%
            normalize_individu(imput = FALSE) %>%
            left_join(
                birthdays_shifted,
                by = "id",
                suffix = c("", "_adju")
            ),
        format = "fst_tbl"
    ),
    tar_target(
        individu_imput,
        data_before_norm %>%
            normalize_individu(imput = TRUE) %>%
            left_join(
                birthdays_shifted,
                by = "id",
                suffix = c("", "_adju")
            ),
        format = "fst_tbl"
    ),
    # Pour neutraliser l'imputation des décès
    # il faut convertir les STATE2_DECES & event_censure != 0 en PDV
    tar_target(
        evenement,
        data_before_norm %>%
            mutate(
                event = replace(
                    event,
                    event == "STATE2_DECES" & event_censor != 0,
                    "STATE2_PDV"
                )
            ) %>%
            select(id, date_event, event, event_censor),
        format = "fst_tbl"
    ),
    tar_target(
        evenement_imput,
        data_before_norm %>%
            mutate(
                date_event = if_else(
                    !is.na(date_event_dem_imput) & event == "STATE1_DEM_SEVERE",
                    date_event_dem_imput,
                    date_event
                ),
            ) %>%
            # J'applique la règle de Michaël et j'exclue les obsevrations
            # sans evenements avant 2013
            group_by(id) %>%
            filter(year(min(date_event)) < 2013) %>%
            ungroup() %>%
            censor_evenements(ymd("2012-12-31")) %>%
            select(id, date_event, event, event_censor),
        format = "fst_tbl"
    ),
    tar_target(
        pathology,
        classify_events(evenement$event) %>%
            full_join(pathologies_desc, by = c("event" = "EVENT")) %>%
            rename_with(str_to_lower) %>%
            mutate(
                pathol = if_else(
                    is.na(pathol),
                    str_extract(event, "[012]"),
                    as.character(pathol)
                ),
                pathol = factor(
                    pathol,
                    levels = c("0", "1", "2"),
                    ordered = TRUE
                ),
                n = replace_na(n, 0),
                type = stringr::str_split_fixed(event, "_", 2)[, 1],
            ) %>%
            mutate(across(where(is.logical), ~ replace_na(., FALSE)))
    ),
    tar_target(
        comparaison_pop_pmsi_gen,
        compare_pmsi_to_general_population(
            insee_age_pyramid,
            evenement,
            individu,
            adjust_exclusions = TRUE
        )
    ),
    tar_target(
        pop_outside_pmsi,
        generate_general_population(
            comparaison_pop_pmsi_gen,
            id_start = max(individu$id) + 1
        ),
        format = "fst_tbl"
    ),
    tar_target(
        individu_with_adj,
        bind_rows(
            mutate(individu, pmsi = TRUE),
            mutate(pop_outside_pmsi, pmsi = FALSE)
        ) %>%
            select(id, date_naissance, sex, pmsi),
        format = "fst_tbl"
    ),
    tar_target(
        evenement_with_adj,
        bind_rows(
            mutate(evenement, pmsi = TRUE),
            mutate(pop_outside_pmsi, pmsi = FALSE)
        ) %>%
            select(id, date_event, event),
        format = "fst_tbl"
    ),
    tar_target(
        evenement_with_adj_imput,
        bind_rows(
            evenement_imput %>%
                mutate(pmsi = TRUE),
            pop_outside_pmsi %>%
                mutate(pmsi = FALSE) %>%
                censor_evenements(ymd("2012-12-31"))
        ) %>%
            select(id, date_event, event, pmsi),
        format = "fst_tbl"
    )
)

# Datasets ---------------------------------------------------------------------


# static target generator for datasets
life_datasets <- tar_eval(
    tar_target(
        target_name,
        evedf %>%
            filter(event %in% c("STATE2_PDV", "STATE2_DECES")) %>%
            ages_patho(
                inddf,
                .,
                "STATE2_DECES",
                pathology,
                50,
                ymd("2010-01-01"),
                ymd("2014-01-01"),
                correct_same_day_exit = TRUE
            ) %>%
            filter(fin_obs_age >= 50),
        format = "fst_tbl"
    ),
    values = expand_grid(
        indi_imput = c(TRUE, FALSE),
        even_imput = c(TRUE, FALSE),
        adj = c(TRUE, FALSE)
    ) %>%
        # indi_imput only impacts fdr, which are excluded for adj
        filter(!(adj & indi_imput)) %>%
        mutate(
            target_name = paste0(
                "life",
                if_else(even_imput, "_eveimp", ""),
                if_else(indi_imput, "_indiimp", ""),
                if_else(adj, "_with_adj", "")
            ),
            evedf = rlang::syms(paste0(
                "evenement",
                if_else(adj, "_with_adj", ""),
                if_else(even_imput, "_imput", "")
            )),
            inddf = rlang::syms(paste0(
                "individu",
                if_else(adj, "_with_adj", ""),
                if_else(indi_imput, "_imput", "")
            ))
        )
)

health_datasets <- tar_eval(
    tar_target(
        target_name,
        evedf %>%
            pathol12(pathology) %>%
            ages_patho(
                inddf,
                .,
                "pathol12",
                pathology,
                50,
                ymd("2010-01-01"),
                ymd("2014-01-01"),
                correct_same_day_exit = TRUE
            ) %>%
            filter(fin_obs_age >= 50),
        format = "fst_tbl"
    ),
    values = expand_grid(
        indi_imput = c(TRUE, FALSE),
        even_imput = c(TRUE, FALSE),
        adj = c(TRUE, FALSE)
    ) %>%
        # indi_imput only impacts fdr, which are excluded for adj
        filter(!(adj & indi_imput)) %>%
        mutate(
            target_name = paste0(
                "health",
                if_else(even_imput, "_eveimp", ""),
                if_else(indi_imput, "_indiimp", ""),
                if_else(adj, "_with_adj", "")
            ),
            evedf = rlang::syms(paste0(
                "evenement",
                if_else(adj, "_with_adj", ""),
                if_else(even_imput, "_imput", "")
            )),
            inddf = rlang::syms(paste0(
                "individu",
                if_else(adj, "_with_adj", ""),
                if_else(indi_imput, "_imput", "")
            ))
        )
)

test_train <- list(
    tar_target(
        train_id, sample(individu$id, size = floor(0.6 * nrow(individu)))
    ),
    tar_target(
        test_id, individu$id[!individu$id %in% train_id]
    )
)

# Analysis ---------------------------------------------------------------------

surv_curves <- list(
    tar_target(
        surv_life,
        survfit(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~ sex,
            data = life_eveimp
        )
    ),
    tar_target(
        surv_life_adj,
        survfit(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~ sex,
            data = life_eveimp_with_adj
        )
    ),
    tar_target(
        surv_health,
        survfit(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~ sex,
            data = health
        )
    ),
    tar_target(
        surv_health_adj,
        survfit(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~ sex,
            data = health_with_adj
        )
    ),
    tar_target(
        haz_health,
        survfit(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~ sex,
            data = health,
            ctype = 1
        )
    )
)

cox_models <- list(
    tar_target(
        cox_health_formula1,
        coxph(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~
                (fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex)^2 +
                dep + immigration + diplome,
            data = health %>% filter(id %in% train_id)
        )
    ),
    tar_target(tt1_times, seq(50, 105, by = 2) %>% head(-1) %>% tail(-1)),
    tar_target(
        health_tt1,
        survSplit(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~
                fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex +
                dep + immigration + diplome + id,
            health,
            cut = tt1_times,
            episode = "tgroup"
        )
    ),
    tar_target(
        cox_health_formula1_tt1,
        coxph(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~
                (fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex)^2 +
                (fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex) *
                    nsk(tgroup, df = 8, b = 0) +
                dep + immigration + diplome,
            data = health_tt1 %>% filter(id %in% train_id),
            id = id
        )
    )
)

article1_targets <- list(
    tar_target(
        cox_tidy_health_formula1,
        tidy(cox_health_formula1, conf.int = TRUE, exponentiate = TRUE)
    ),
    tar_target(
        cox_tidy_health_formula1_tt1,
        tidy(cox_health_formula1_tt1, conf.int = TRUE, exponentiate = TRUE)
    ),
    tar_target(
        cox_tidy_tt_health_formula1_tt1,
        tidy_health_tt(cox_health_formula1_tt1, tt1_times)
    ),
    tar_target(
        health_demographic_statistics_table,
        health %>%
            mutate(
                exposure = fin_obs_age - debut_obs_age,
                across(starts_with("fdr_"), \(x) paste("Category ", x)),
                across(c(immigration, diplome), \(x) paste("Quartile ", x)),
            ) %>%
            bind_rows(mutate(., sex = "B")) %>%
            group_by(sex) %>%
            summarize(
                n = list(tibble(statistic = "n", value = n())),
                across(
                    c(
                        debut_obs_age, exposure, fdr_obesity_cat3, fdr_aud_cat3,
                        fdr_smoker_cat3, immigration, diplome
                    ),
                    \(x) {
                        if (is.numeric(x)) {
                            list(
                                tibble(
                                    statistic = "Median (IQR)",
                                    value = median(x),
                                    value2 = quantile(x, 0.25),
                                    value3 = quantile(x, 0.75),
                                )
                            )
                        } else {
                            list(
                                fct_count(x, p = TRUE) %>%
                                    transmute(
                                        statistic = f,
                                        value = n,
                                        value2 = p,
                                    )
                            )
                        }
                    }
                ),
            ) %>%
            pivot_longer(
                cols = -sex,
                values_to = "summaries",
                names_to = "var"
            ) %>%
            unnest(summaries) %>%
            mutate(
                var = case_when(
                    var == "n" ~ "Number of indiduals",
                    var == "debut_obs_age" ~ "Age at start of exposure",
                    TRUE ~ var
                )
            )
    ),
    tar_target(
        health_risk_factors_cor,
        {
            colnames_dict <- c(
                `Obesity` = "fdr_obesity_cat3",
                `Alcohol` = "fdr_aud_cat3",
                `Smoking` = "fdr_smoker_cat3",
                `Immigration` = "immigration",
                `Education` = "diplome"
            )
            health2 <- health %>%
                mutate(
                    across(starts_with("fdr"), \(x) if_else(x != "0", 1L, 0L)),
                    across(c(immigration, diplome), as.integer),
                ) %>%
                select(all_of(colnames_dict))
            expand_grid(
                x = names(colnames_dict),
                y = x
            ) %>%
                filter(x < y) %>%
                mutate(
                    ct = map2(
                        x,
                        y,
                        \(x, y) {
                            suppressWarnings(
                                cor.test(
                                    health2[[x]],
                                    health2[[y]]
                                )
                            )
                        }
                    ),
                    cor = map_dbl(ct, \(x) x$estimate),
                    pvalue = map_dbl(ct, \(x) x$p.value),
                )
        }
    ),
    tar_target(
        cox_health_profiles_surv,
        {
            profils_examples <- do.call(
                expand_grid,
                cox_health_formula1_tt1$xlevels
            ) %>%
                filter(
                    immigration == "0",
                    fdr_aud_cat3 %in% c("0", "2"),
                    fdr_obesity_cat3 %in% c("0", "2"),
                    fdr_smoker_cat3 %in% c("0", "2"),
                ) %>%
                mutate(
                    profile = case_when(
                        dep == "06" &
                            fdr_aud_cat3 == "0" &
                            fdr_obesity_cat3 == "0" &
                            fdr_smoker_cat3 == "0" &
                            diplome == "0" ~ "Lowest",
                        dep == "75" &
                            rowSums(cbind(
                                fdr_aud_cat3 == "2",
                                fdr_obesity_cat3 == "2",
                                fdr_smoker_cat3 == "2"
                            )) == 1 &
                            diplome == "1" ~ "Intermediate",
                        dep == "57" &
                            rowSums(cbind(
                                fdr_aud_cat3 == "2",
                                fdr_obesity_cat3 == "2",
                                fdr_smoker_cat3 == "2"
                            )) == 2 &
                            diplome == "3" ~ "Highest"
                    )
                ) %>%
                filter(!is.na(profile)) %>%
                mutate(id = seq_len(n()))
            res <- survfit(
                cox_health_formula1_tt1,
                newdata = profils_examples %>%
                    rowwise() %>%
                    reframe(
                        across(everything(), \(x) rep(x, length(tt1_times))),
                        tgroup = seq_along(tt1_times),
                        debut_obs_age = 50 + 2 * (tgroup - 1),
                        fin_obs_age = 50 + 2 * tgroup,
                        patho_obs = 0
                    ),
                id = id
            )
            res %>%
                tidy_surv() %>%
                mutate(id = as.integer(strata)) %>%
                left_join(
                    profils_examples,
                    by = "id"
                )
        }
    ),
    tar_target(
        cox_health_f1_tt1_inter_tt,
        expand_grid(
            var1 = c("fdr_obesity_cat3", "fdr_aud_cat3", "fdr_smoker_cat3"),
            var2 = var1,
            val1 = as.character(0:2),
            val2 = val1
        ) %>%
            filter(
                # can't be smoker1 and 2 at the same time
                # !(var1 == var2 & val1 != val2),
                var1 != var2,
            ) %>%
            mutate(
                fdr_aud_cat3 =
                    if_else(var1 == "fdr_aud_cat3" & val1 != "0", val1,
                        if_else(var2 == "fdr_aud_cat3" & val2 != "0", val2, "0")
                    ),
                fdr_smoker_cat3 =
                    if_else(var1 == "fdr_smoker_cat3" & val1 != "0", val1,
                        if_else(var2 == "fdr_smoker_cat3" & val2 != "0", val2, "0")
                    ),
                fdr_obesity_cat3 =
                    if_else(var1 == "fdr_obesity_cat3" & val1 != "0", val1,
                        if_else(var2 == "fdr_obesity_cat3" & val2 != "0", val2, "0")
                    ),
                dep = "01",
                immigration = "0",
                diplome = "0",
                sex = "F"
            ) %>%
            rowwise() %>%
            reframe(
                across(everything(), \(x) rep(x, length(tt1_times))),
                tgroup = seq_along(tt1_times),
                times = tt1_times
            ) %>%
            mutate(
                as_tibble(
                    predict(
                        cox_health_formula1_tt1,
                        newdata = .,
                        se.fit = TRUE,
                        reference = "zero",
                        type = "risk"
                    )
                ),
                fit_up = fit + 1.96 * se.fit,
                fit_dn = fit - 1.96 * se.fit,
            )
    ),
    tar_target(
        cox_health_f1_tt1_concordance_test,
        concordance(
            cox_health_formula1_tt1,
            newdata = filter(health_tt1, id %in% test_id)
        )
    ),
    tar_target(
        # This one is a little complicated
        # 1. We predict the risk score (lp) for each "individual" in test set
        # (key = id, tgroup)
        # 2. We calcuate "average" lp for each true individual (key = id)
        # 3. We calculate KM curves by using groupped lp as predictor
        # 4. We calculate "estimated" KM by combining base hazard and the
        # average lp within each group of lp
        # 5. compare curves from 3 and 4
        tidy_cox_health_f1_tt1_risk_group_surv_test,
        {
            test_profiles <- health_tt1 %>%
                filter(id %in% test_id) %>%
                mutate(
                    # 1
                    lp = predict(
                        cox_health_formula1_tt1,
                        type = "lp", newdata = .
                    ),
                ) %>%
                # data.table is much faster for many small groups
                # 2
                lazy_dt() %>%
                group_by(id) %>%
                summarize(
                    debut_obs_age = min(debut_obs_age),
                    fin_obs_age = max(fin_obs_age),
                    patho_obs = max(patho_obs),
                    lp_max = max(lp),
                    lp_min = min(lp),
                    lp = mean(lp),
                    .groups = "drop"
                ) %>%
                as_tibble() %>%
                mutate(
                    risk_group = cut(
                        lp, c(-Inf, 0.2, 0.7, 1.1, 1.5, Inf)
                    )
                )
            rg_surv <- survfit(
                Surv(debut_obs_age, fin_obs_age, patho_obs) ~ risk_group,
                data = test_profiles
            )
            base_surv <- survfit(
                cox_health_formula1_tt1,
                newdata = tibble(
                    id = 1,
                    sex = "F",
                    immigration = "0",
                    fdr_aud_cat3 = "0",
                    fdr_obesity_cat3 = "0",
                    fdr_smoker_cat3 = "0",
                    dep = "01",
                    diplome = "0"
                ) %>%
                    reframe(
                        across(everything(), \(x) rep(x, length(tt1_times))),
                        times = tt1_times,
                        tgroup = seq_along(tt1_times),
                        debut_obs_age = 50 + 2 * (tgroup - 1),
                        fin_obs_age = 50 + 2 * tgroup,
                        patho_obs = 0
                    ),
                id = id
            )
            # Both curves in one data.frame
            bind_rows(
                `predicted` = tidy_surv(base_surv) %>%
                    mutate(time = time + 50) %>%
                    cross_join(
                        test_profiles %>%
                            group_by(risk_group) %>%
                            summarize(lp = mean(lp))
                    ) %>%
                    mutate(
                        cumhaz = cumhaz * exp(lp),
                        surv = exp(-cumhaz)
                    ),
                `observed` = tidy_surv(rg_surv) %>%
                    mutate(
                        risk_group = str_extract(
                            strata, "risk_group=(.*)",
                            group = 1
                        )
                    ),
                .id = "origin"
            )
        }
    ),
    tar_render(article1_risks, "reports/article1_risks.Rmd"),
    tar_render(article1_arxiv, "reports/article1_arxiv.Rmd"),
    tar_render(risks_cover, "reports/risks_cover.Rmd")
)

article1_sup_targets <- list(
    tar_target(
        tt2_times,
        surv_health %>%
            tidy_surv() %>%
            filter(strata == "sex=F", n.event > 0) %>%
            arrange(time) %>%
            with(
                approx(
                    surv,
                    time,
                    xout = seq(0, 1, length.out = 30),
                    method = "constant"
                )$y
            ) %>%
            head(-1) %>%
            tail(-1)
    ),
    tar_target(
        cox_health_formula1_tt2,
        coxph(
            Surv(debut_obs_age, fin_obs_age, patho_obs) ~
                (fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex)^2 +
                (fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex) *
                    nsk(tgroup, df = 8, b = 0) +
                dep + immigration + diplome,
            data = survSplit(
                Surv(debut_obs_age, fin_obs_age, patho_obs) ~
                    fdr_obesity_cat3 + fdr_aud_cat3 + fdr_smoker_cat3 + sex +
                    dep + immigration + diplome + id,
                health %>% filter(id %in% train_id),
                cut = tt2_times,
                episode = "tgroup"
            ),
            id = id
        )
    ),
    # PH tests (and residuals calculations)
    tar_target(
        cox_zph_health_formula1,
        cox.zph(cox_health_formula1)
    ),
    tar_target(
        cox_zph_health_formula1_tt1,
        cox.zph(cox_health_formula1_tt1)
    ),
    # Calculate ERLYGH
    tar_target(
        cox_erlygh_health_formula1,
        {
            all_covar_f1 <- do.call(
                expand_grid,
                cox_health_formula1$xlevels
            ) %>%
                filter(dep == "01", immigration == "0", diplome == "0")
            surv_cox_f1 <- survfit(
                cox_health_formula1,
                newdata = all_covar_f1
            )
            surv_cox_f1_tidy <- tibble(
                time = rep(surv_cox_f1$time, nrow(all_covar_f1)),
                surv = as.vector(surv_cox_f1$surv),
                id = as.vector(col(surv_cox_f1$surv))
            ) %>%
                left_join(
                    mutate(all_covar_f1, id = seq_len(n())),
                    by = "id"
                )
            surv_cox_f1_tidy %>%
                group_by(across(c(starts_with("fdr"), sex))) %>%
                summarize(ERLYGH = surv_time(time, surv, 50), .groups = "drop")
        }
    ),
    tar_target(
        compar_adj_to_insee,
        evenement_with_adj %>%
            filter(event == "STATE2_DECES") %>%
            right_join(individu_with_adj, by = "id") %>%
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
            mutate(
                date_naissance = date_naissance + days(
                    if_else(format(date_naissance, "%d-%m") == "29-02", 1, 0)
                )
            ) %>%
            group_by(sex) %>%
            reframe(population_1jan(id, date_naissance, date_event)) %>%
            rename(pop_pmsi_adj = pop) %>%
            full_join(
                comparaison_pop_pmsi_gen,
                by = c("age", "year", "sex", "cohort")
            ) %>%
            pivot_longer(
                cols = starts_with("pop"),
                values_to = "pop",
                names_to = "method",
                names_prefix = "pop_"
            ) %>%
            mutate(
                method = case_when(
                    method == "insee" ~ "INSEE",
                    method == "pmsi" ~ "Without adj.",
                    method == "pmsi_adj" ~ "With adj.",
                )
            )
    ),
    tar_render(article1_sup, "reports/article1_sup.Rmd")
)

reports <- list(
    tar_render(data_exploration, "reports/data_exploration.Rmd"),
    tar_render(replication, "reports/replication.Rmd"),
    tar_render(thesis, "reports/thesis.Rmd"),
    tar_render(test_rmd, "reports/test.Rmd")
)

tests <- list(
    tar_target(
        data_sanity_check,
        bind_rows(
            # death or PDV is last event
            # (this test does not work because it detects cases with multiple events on same day)
            # evenement %>%
            #     arrange(date_event) %>%
            #     mutate(last = !duplicated(id, fromLast = TRUE)) %>%
            #     filter(event %in% c("STATE2_DECES", "STATE2_PDV")) %>%
            #     filter(id %in% id[!last]) %>%
            #     arrange(id, date_event) %>%
            #     reframe(stopifnot(all(last)))
            # Check order of pathols (cf pathology notes)
            # Check only one death/PDV per person
            evenement %>%
                filter(event %in% c("STATE2_DECES", "STATE2_PDV")) %>%
                summarize(cond = all(!duplicated(id))),
            # Check exactly one death/PDV per person
            evenement %>%
                filter(event %in% c("STATE2_DECES", "STATE2_PDV")) %>%
                summarize(
                    cond = n_distinct(
                        id[event %in% c("STATE2_DECES", "STATE2_PDV")]
                    ) == n_distinct(id)
                )
        ) %>%
            {
                stopifnot(all(.$cond))
            }
    ),
    tar_target(
        check_health_volume,
        {
            # `health` thas less individuals than initial data
            # Check that all those individuals are accounted for
            # 2 sources of difference
            # (1) those censored before 2010
            # (2) those who had their pathol1 before their 50th birthday

            # (1) censored before 2010
            id_censored <- evenement %>%
                filter(event == "STATE2_PDV", year(date_event) < 2010) %>%
                pull(id)

            # (2) those who had their pathol1 before their 50th bday
            id_young <- evenement %>%
                filter(event != "STATE1_NO_PATHOL1") %>%
                arrange(date_event, id) %>%
                filter(!duplicated(id)) %>%
                mutate(
                    bday = individu$date_naissance[match(id, individu$id)],
                    age_pathol1 = interval(bday, date_event) / years(1),
                ) %>%
                filter(age_pathol1 < 50) %>%
                pull(id)

            stopifnot(
                nrow(health) == n_distinct(data_before_norm$id) -
                    n_distinct(c(id_censored, id_young))
            )
        }
    )
)

targets <- list() %>%
    append(aux_data_import) %>%
    append(benchmark_treatment) %>%
    append(life_datasets) %>%
    append(health_datasets) %>%
    append(test_train) %>%
    append(surv_curves) %>%
    append(cox_models) %>%
    append(reports) %>%
    append(article1_targets) %>%
    # append(article1_sup_targets) %>%
    append(tests)

targets

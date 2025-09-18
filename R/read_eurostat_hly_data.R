read_eurostat_hly_data <- function(file) {
    read_tsv(
        gzfile(file),
        col_types = cols(
            `unit,sex,indic_he,geo\\time` = col_character(),
            `2020` = col_character(),
            `2019` = col_character(),
            `2018` = col_character(),
            `2017` = col_character(),
            `2016` = col_character(),
            `2015` = col_character(),
            `2014` = col_character(),
            `2013` = col_character(),
            `2012` = col_character(),
            `2011` = col_character(),
            `2010` = col_character(),
            `2009` = col_character(),
            `2008` = col_character(),
            `2007` = col_character(),
            `2006` = col_character(),
            `2005` = col_character(),
            `2004` = col_character()
        )
    ) %>%
        rename(info = `unit,sex,indic_he,geo\\time`) %>%
        pivot_longer(-info, values_to = "hly", names_to = "year") %>%
        mutate(
            unit = str_split_i(info, ",", 1),
            sex = str_split_i(info, ",", 2),
            indic_he = str_split_i(info, ",", 3),
            geo = str_split_i(info, ",", 4),
            info = NULL,
            hly = replace(hly, hly == ":", NA),
            note = str_split_i(hly, " ", 2),
            hly = as.double(str_split_i(hly, " ", 1)),
            year = as.integer(year),
        ) %>%
        relocate(hly, .after = geo)
}

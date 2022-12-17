
library(command)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(purrr)

cmd_assign(zipfile = "data/hmd_statistics_20221129.zip",
           version = "20221129")

dirs <- c("lt_both/bltper_1x1",
          "lt_both/bltper_5x1",
          "lt_female/fltper_1x1",
          "lt_female/fltper_5x1",
          "lt_male/mltper_1x1",
          "lt_male/mltper_5x1")

tmp_dir <- tempdir()
unzip(zipfile, exdir = tmp_dir)
data_dir <- file.path(tmp_dir, sub("\\.zip$", "", basename(zipfile)))

get_data <- function(dir) {
    country_code <- sub("([A-Z]+)\\..*", "\\1", list.files(dir))
    sex <- sub(".*lt_([a-z]+).*", "\\1", dir)
    plan <- sub(".*ltper_([1x5]+).*", "\\1", dir)
    files <- list.files(dir, full.names = TRUE)
    ans <- map(files,
               read_table,
               col_types = "ic----ii--",
               na = ".",
               skip = 1)
    names(ans) <- country_code
    ans <- bind_rows(ans, .id = "country")
    ans$sex <- sex
    ans$plan <- plan
    ans
}

lt <- dirs %>%
    file.path(data_dir, .) %>%
    map(get_data) %>%
    bind_rows() %>%
    rename(age = Age, time = Year)

unlink(tmp_dir, recursive = TRUE)


has_missing <- lt %>%
    group_by(country, time, plan) %>%
    summarise(has_missing = anyNA(dx) || anyNA(Lx)) %>%
    ungroup()

lt <- inner_join(lt, has_missing, by = c("country", "time", "plan")) %>%
    filter(!has_missing) %>%
    select(-has_missing)


saveRDS(lt, file = sprintf("out/lt_%s.rds", version))



library(command)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(purrr)

cmd_assign(zipfile = "data/hmd_statistics_20221129.zip",
           .out = "out/lt_20221129.rds")

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
    type_age <- sub(".*ltper_([1x5]+).*", "\\1", dir)
    files <- list.files(dir, full.names = TRUE)
    ans <- map(files,
               read_table,
               col_types = "ic----ii--",
               na = ".",
               skip = 1)
    names(ans) <- country_code
    ans <- bind_rows(ans, .id = "country")
    ans$sex <- sex
    ans$type_age <- type_age
    ans
}

lt <- dirs %>%
    file.path(data_dir, .) %>%
    map(get_data) %>%
    bind_rows() %>%
    rename(age = Age, time = Year)

unlink(tmp_dir, recursive = TRUE)


has_missing <- lt %>%
    group_by(country, time, type_age) %>%
    summarise(has_missing = anyNA(dx) || anyNA(Lx), .groups = "drop")

lt <- inner_join(lt, has_missing, by = c("country", "time", "type_age")) %>%
    filter(!has_missing) %>%
    select(-has_missing) %>%
    mutate(sex = case_when(sex == "female" ~ "Female",
                           sex == "male" ~ "Male",
                           sex == "both" ~ "Both"))

saveRDS(lt, file = .out)


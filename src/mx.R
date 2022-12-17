
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(command)

file_args(p_dir_female = "data/hmd_statistics_20221129.zip/fltper_5x1",
          p_dir_male = "data/hmd_statistics_20221129.zip/mltper_5x1")

## Female

codes <- p_dir_female %>%
    list.files() %>%
    sub("([A-Z]+)\\..*", "\\1", .)

mx_f <- p_dir_female %>%
    list.files(full.name = TRUE) %>%
    map(read_table,
        col_types = "ic----ii--",
        na = ".",
        skip = 1) %>%
    set_names(codes) %>%
    bind_rows(.id = "country") %>%
    mutate(sex = "Female")


## Male

codes <- p_dir_male %>%
    list.files() %>%
    sub("([A-Z]+)\\..*", "\\1", .)

mx_m <- p_dir_male %>%
    list.files(full.name = TRUE) %>%
    map(read_table,
        col_types = "ic----ii--",
        na = ".",
        skip = 1) %>%
    set_names(codes) %>%
    bind_rows(.id = "country") %>%
    mutate(sex = "Male")


## Combined

mx <- bind_rows(mx_f, mx_m) %>%
    filter(!(country == "BEL" & Year %in% 1914:1918))


## Save

saveRDS(mx, file = "out/mx.rds")


library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(forcats)
library(purrr)
library(poputils)
library(bage)
library(command)

cmd_assign(lt_raw = "out/lt_20221129.rds",
           n_comp = 10L,
           .out = "out/HMD_20221129.rds")

## ages_open <- seq(from = 60, to = 110, by = 5)
ages_open <- seq(from = 60, to = 65, by = 5)
## lt_raw <- filter(lt_raw, time %in% 1990:2021)

aggregate_age <- function(x, age_open) {
    x %>%
        mutate(age = set_age_open(age, lower = age_open)) %>%
        group_by(age, country, time) %>%
        summarise(dx = sum(dx), Lx = sum(Lx), .groups = "drop") %>%
        mutate(age = reformat_age(age)) %>%
        arrange(age, country, time)
}

aggregate_sexage <- function(x, age_open) {
    x %>%
        mutate(age = set_age_open(age, lower = age_open)) %>%
        group_by(sex, age, country, time) %>%
        summarise(dx = sum(dx), Lx = sum(Lx), .groups = "drop") %>%
        mutate(age = reformat_age(age)) %>%
        arrange(sex, age, country, time)
}


calculate_mx <- function(x) {
    x %>%
        mutate(mx = dx / Lx) %>%
        select(-dx, -Lx)
}

recode_age <- function(x) {
    x %>%
        mutate(age = paste(sex, age, sep = "."),
               age = factor(age, levels = unique(age)))
}


lt_five <- lt_raw %>%
    filter(type_age == "5x1") %>%
    mutate(type_age = "five") %>%
    mutate(age = reformat_age(age),
           age = collapse_age(age, type_to = "five"))
           
lt <- lt_raw %>%
    bind_rows(lt_five) %>%
    mutate(type_age = case_when(type_age == "5x1" ~ "lt",
                                type_age == "1x1" ~ "single",
                                TRUE ~ type_age)) 

lt_joint <- lt %>%
    filter(sex %in% c("Female", "Male"))

mx <- lt %>%
    group_by(type_age, sex) %>%
    nest() %>%
    expand_grid(age_open = ages_open) %>%
    mutate(data = map2(data, age_open, aggregate_age),
           data = map(data, calculate_mx),
           data = map(data,
                      to_matrix,
                      rows = age,
                      cols = c(country, time),
                      measure = mx))

mx_joint <- lt_joint %>%
    group_by(type_age) %>%
    nest() %>%
    expand_grid(age_open = ages_open) %>%
    mutate(data = map2(data, age_open, aggregate_sexage),
           data = map(data, calculate_mx),
           data = map(data,
                      to_matrix,
                      rows = c(sex, age),
                      cols = c(country, time),
                      measure = mx)) %>%
    mutate(sex = ".Joint")

HMD <- bind_rows(mx, mx_joint) %>%
    mutate(scaled_svd_comp = map(data, scaled_svd_comp, n = n_comp, transform = "log")) %>%
    select(sex, scaled_svd_comp) %>%
    unnest_wider(scaled_svd_comp)

saveRDS(HMD, file = .out)
           

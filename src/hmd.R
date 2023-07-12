
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

ages_open <- seq(from = 60, to = 110, by = 5)

weighted_mx <- function(mx, Lx) {
    if (length(mx) == 1L)
        mx
    else if (sum(Lx) > 0)
        sum(mx * Lx) / sum(Lx)
    else
        mean(mx)
}

aggregate_ageopen_by_age <- function(x, age_open) {
    x %>%
        mutate(age = set_age_open(age, lower = age_open)) %>%
        group_by(age, country, time) %>%
        summarise(mx = weighted_mx(mx = mx, Lx = Lx), Lx = sum(Lx), .groups = "drop") %>%
        mutate(age = reformat_age(age)) %>%
        arrange(age, country, time)
}

aggregate_ageopen_by_agesex <- function(x, age_open) {
    x %>%
        mutate(age = set_age_open(age, lower = age_open)) %>%
        group_by(sex, age, country, time) %>%
        summarise(mx = weighted_mx(mx = mx, Lx = Lx),
                  Lx = sum(Lx),
                  .groups = "drop") %>%
        mutate(age = reformat_age(age)) %>%
        arrange(sex, age, country, time)
}

lt_five <- lt_raw %>%
    filter(type_age == "5x1") %>%
    mutate(type_age = "five") %>%
    mutate(age = combine_age(age, type_to = "five")) %>%
    group_by(sex, age, country, time) %>%
    summarise(mx = weighted_mx(mx = mx, Lx = Lx),
              Lx = sum(Lx),
              .groups = "drop") %>%
    mutate(type_age = "five")

           
lt <- lt_raw %>%
    mutate(type_age = case_when(type_age == "5x1" ~ "lt",
                                type_age == "1x1" ~ "single",
                                TRUE ~ "invalid")) %>%    
    bind_rows(lt_five)

lt_joint <- lt %>%
    filter(sex %in% c("Female", "Male"))

mx <- lt %>%
    group_by(type_age, sex) %>%
    nest() %>%
    expand_grid(age_open = ages_open) %>%
    mutate(data = map2(data, age_open, aggregate_ageopen_by_age),
           data = map(data,
                      to_matrix,
                      rows = age,
                      cols = c(country, time),
                      measure = mx))


mx_joint <- lt_joint %>%
    group_by(type_age) %>%
    nest() %>%
    expand_grid(age_open = ages_open) %>%
    mutate(data = map2(data, age_open, aggregate_ageopen_by_agesex),
           data = map(data,
                      to_matrix,
                      rows = c(sex, age),
                      cols = c(country, time),
                      measure = mx)) %>%
    mutate(sex = "joint")

HMD <- bind_rows(mx, mx_joint) %>%
    mutate(scaled_svd_comp = map(data,
                                 scaled_svd_comp,
                                 n = n_comp,
                                 transform = "log")) %>%
    unnest_wider(scaled_svd_comp) %>%
    select(-data) %>%
    mutate(type = ifelse(sex %in% c("total", "joint"), sex, "indep")) %>%
    group_by(type, type_age, age_open) %>%
    nest(data = c(sex, matrix, offset)) %>%
    ungroup()

HMD_total <- HMD %>%
    filter(type == "total") %>%
    mutate(labels_sexgender = rep(list(NULL), times = n()),
           labels_age = map(data, function(x) rownames(x$matrix[[1L]]))) %>%
    unnest(data) %>%
    select(type, labels_age, labels_sexgender, matrix, offset)

HMD_joint <- HMD %>%
    filter(type == "joint") %>%
    mutate(labels = map(data, function(x) rownames(x$matrix[[1L]])),
           labels_sexgender = map(labels, function(x) sub("^(.*)\\.(.*)$", "\\1", x)),
           labels_age = map(labels, function(x) sub("^(.*)\\.(.*)$", "\\2", x))) %>%
    unnest(data) %>%
    select(type, labels_age, labels_sexgender, matrix, offset)

make_df_indep <- function(x) {
    labels_sexgender <- .mapply(function(x, y) rep(x, times = length(y)),
                                dots = list(x = x$sex, y = x$offset),
                                MoreArgs = list())
    labels_age <- lapply(x$offset, names)
    labels_sexgender <- do.call(c, labels_sexgender)
    labels_age <- do.call(c, labels_age)
    matrix <- do.call(rbind, x$matrix)
    offset <- do.call(c, x$offset)
    tibble::tibble(labels_age = list(labels_age),
                   labels_sexgender = list(labels_sexgender),
                   matrix = list(matrix),
                   offset = list(offset))
}

HMD_indep <- HMD %>%
    filter(type == "indep") %>%
    mutate(data = map(data, make_df_indep)) %>%
    unnest(data) %>%
    select(type, labels_age, labels_sexgender, matrix, offset)

HMD <- bind_rows(HMD_total, HMD_joint, HMD_indep)

saveRDS(HMD, file = .out)
           



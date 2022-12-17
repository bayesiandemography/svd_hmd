
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(forcats)
library(purrr)
library(command)

cmd_assign(lt = "out/lt_20221129.rds",
           epsilon = 0.0001,
           n_pc = 10L,
           version = "20221129")

lt_single <- lt %>%
    filter(plan == "1x1") %>%
    select(-plan)

lt_five <- lt %>%
    filter(plan == "5x1") %>%
    select(-plan) %>%
    mutate(age = fct_inorder(age),
           age = fct_recode(age, "0-4" = "0", "0-4" = "1-4")) %>%
    group_by(country, age, sex, time) %>%
    summarise(dx = sum(dx), Lx = sum(Lx)) %>%
    ungroup() %>%
    mutate(age = as.character(age))

lt_lt <- lt %>%
    filter(plan == "5x1") %>%
    select(-plan)


make_mx <- function(x, age_open) {
    x %>%
        mutate(agenum = sub("^([0-9]+).*", "\\1", age),
               agenum = as.integer(agenum),
               age = if_else(agenum >= age_open, paste0(age_open, "+"), age),
               age = fct_inorder(age)) %>%
        group_by(country, age, sex, time) %>%
        summarise(dx = sum(dx), Lx = sum(Lx)) %>%
        ungroup() %>%
        mutate(mx = dx / Lx) %>%
        select(-dx, -Lx)
}

make_lmx <- function(x, sex, epsilon) {
    if (sex == "comb")
        x <- filter(x, sex != "both")
    else if (sex == "total")
        x <- filter(x, sex == "both")
    else
        x <- filter(x, sex == !!sex)
    x %>%
        arrange(sex, age) %>%
        mutate(country_time = paste(country, time),
               sex_age = fct_inorder(paste(sex, age))) %>%
        mutate(mx = pmax(mx, epsilon), 
               mx = log(mx)) %>%
        xtabs(mx ~ sex_age + country_time, data = .) %>%
        array(dim = dim(.), dimnames = dimnames(.))
}

make_bX <- function(x, n_pc) {
    svd <- svd(x = x,
               nu = n_pc,
               nv = n_pc)
    U <- svd$u
    s <- seq_len(n_pc)
    D <- diag(svd$d[s])
    V <- svd$v
    mean_V <- colMeans(V)
    sd_V <- apply(V, 2L, sd)
    b <- as.numeric(U %*% D %*% mean_V)
    X <- U %*% D %*% diag(sd_V)
    list(b = b, X = X)
}

make_bX_one <- function(type, age_open, sex, epsilon, n_pc) {
    get(paste("lt", type, sep = "_")) %>%
        make_mx(age_open = age_open) %>%
        make_lmx(sex = sex, epsilon = epsilon) %>%
        make_bX(n_pc = n_pc)
}


svd <- expand_grid(type = c("single", "five", "lt"),
                   age_open = seq(70, 110, 5),
                   sex = c("comb", "total", "female", "male")) %>%
    mutate(bX = pmap(list(type, age_open, sex),
                     make_bX_one,
                     epsilon = epsilon,
                     n_pc = n_pc))


saveRDS(svd,
        file = sprintf("out/svd_mortality_num_%s.rds", version))
           

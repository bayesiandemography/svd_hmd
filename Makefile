
EPSILON = 0.0001
N_PC = 10

.PHONY: all
all: out/svd_mortality_20221129.csv

## 2022-11-29 -----------------------------------------------------------------

out/lt_20221129.rds: src/lt.R \
  data/hmd_statistics_20221129.zip
	Rscript $^ --version=20221129

out/svd_mortality_num_20221129.rds: src/svd_mortality_num.R \
  out/lt_20221129.rds
	Rscript $^ --epsilon=$(EPSILON) --n_pc=$(N_PC) --version=20221129


## Clean

.PHONY: clean
clean:
	rm -rf out
	mkdir out

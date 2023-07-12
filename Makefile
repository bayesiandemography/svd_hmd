
N_COMP = 10

.PHONY: all
all: out/HMD_20221129.rds

## 2022-11-29 -----------------------------------------------------------------

out/lt_20221129.rds: src/lt.R \
  data/hmd_statistics_20221129.zip
	Rscript $^ $@

out/HMD_20221129.rds: src/hmd.R \
  out/lt_20221129.rds
	Rscript $^ $@ --n_comp=$(N_COMP)
	rm out/lt_20221129.rds

## Clean

.PHONY: clean
clean:
	rm -rf out
	mkdir out


N_PC = 10

.PHONY: all
all: out/hmd_transform_20221129.rds

## 2022-11-29 -----------------------------------------------------------------

out/lt_20221129.rds: src/lt.R \
  data/hmd_statistics_20221129.zip
	Rscript $^ $@

out/hmd_transform_20221129.rds: src/hmd_transform.R \
  out/lt_20221129.rds
	Rscript $^ $@ --n_pc=$(N_PC)


## Clean

.PHONY: clean
clean:
	rm -rf out
	mkdir out

RSCRIPT=Rscript --vanilla
all: fig-v.pdf fig-vi.pdf
fig-v.pdf: fig-v.R
fig-vi.pdf: fig-vi.R
	$(RSCRIPT) fig-v.R
	$(RSCRIPT) fig-vi.R

# run by gmake -f makefile.mk
# output indicates if the rule was run

#$^ is for "all the prerequisites of this file"
#$......
# $@ is short for "the target of this rule"

# Introduce a phony target to make everything run
all : manuscript/latitudepaper.pdf

# Rule for specific things
manuscript/latitudepaper.pdf : manuscript/Figures/by_TL/marginal/*.eps manuscript/Figures/by_TL/scaling_with_S/proportions/fitlines_*.eps manuscript/noISN.bib manuscript/prsb.bst
	cd manuscript && \
	pwd && \
	latex latitudepaper.tex && \
	bibtex latitudepaper.aux && \
	latex latitudepaper.tex && \
	latex latitudepaper.tex && \
	dvips latitudepaper.dvi && \
	ps2pdf latitudepaper.ps && \
	cd ../

# Jackknife figure
manuscript/Figures/Jackknife/&.eps : code/jackknife_display_figure.py Jackknifed/main/coefficients/*.tsv 
	cd code && \
	python jackknife_display_figure.py && \
	cd ../

# Figures depend on the figure making code, datafiles
manuscript/Figures/by_TL/marginal/%.eps : code/TL_marginal_latitude.py non_TS/proportions/marginals/*.tsv non_TS/summary-properties.tsv
	cd code && \
	python TL_marginal_latitude.py && \
	python TL_scaling_justlines.py && \
	cd ../

# Figures depend on the figure making code, datafiles
manuscript/Figures/scaling_with_S/fitlines%.eps : code/TL_observedfits.py non_TS/proportions/coefficients/*.tsv non_TS/summary-properties.tsv
	cd code && \
	python TL_observedfits.py && \
	cd ../

# Output files depend on the master R script and the datafile
non_TS/proportions/marginals/%.tsv : code/byTL_Models.R non_TS/summary-properties.tsv
	cd code && \
	Rscript byTL_Models.R && \
	cd ../

# Output files depend on the master R script and the datafile
non_TS/%/%.tsv : code/byTL_Models.R non_TS/summary-properties.tsv
	cd code && \
	Rscript byTL_Models.R && \
	cd ../

# If any of the utility R files have changed, re-run the master R script
code/byTL_Models.R : code/marginal_CIs.R code/property_models_proportions.R code/recreate_with_subset.R code/power_analyses.R
	cd code && \
	Rscript byTL_Models.R && \
	cd ../

# The summary data file depends on the raw datafiles, food web property script
non_TS/summary-properties.tsv : code/food_web_properties.py mod_data/lists/pred-prey-lists-to-use/*.web mod_data/sup_data/food_web_notes_updated.csv
	cd code && \
	python food_web_properties.py && \
	cd ../

# The pred-prey lists depend on the raw webs
mod_data/lists/pred-prey-lists-to-use/%.web : code/listcreator.py mod_data/webs/%
	cd code && \
	python listcreator.py && \
	cd ../


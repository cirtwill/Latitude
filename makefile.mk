# run by gmake -f makefile.mk
# output indicates if the rule was run

$^ is for "all the prerequisites of this file"
$......

# Introduce a phony target to make everything run
all: manuscript/latitudepaper.tex

# Rule for specific things
manuscript/latitudepaper.tex : manuscript/Figures/by_TL/marginal/*.eps manuscript/Figures/by_TL/scaling_with_S/proportions/fitlines_*.eps manuscript/noISN.bib manuscript/jae.bst
  cd manuscript
  latex $@    # $@ is short for "the target of this rule"
  bibtex $@
  latex $@
  latex $@
  dvips $@
  ps2pdf $@
  cd ../

manuscript/Figures/by_TL/marginal/%.eps : code/TL_marginal_latitude.py non_TS/proportions/marginals/*.tsv
  cd code
  python TL_marginal_latitude.py
  cd ../

manuscript/Figures/scaling_with_S/fitlines%.eps : code/TL_observedfits.py #Datafiles
  cd code
  python TL_observedfits.py
  cd ../

#python listcreator.py
#python food_web_properties.py

#R junk
# try both Rscript byTL_Models.R and R CMD BATCH byTL_Models.R

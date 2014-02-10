


data=read.table('summary-properties.tsv',sep='\t',header=TRUE)
attach(data)

##################################
##################################
##		Part 1: testing S, C, LS over lat
##################################
##################################
species_latbytype = glm(log10(Species)~Latitude*Ecotype+Year_pub)
links_latbytype = glm(log10(LS)~Latitude*Ecotype+Year_pub)
conn_latbytype = glm(log10(Connectance)~Latitude*Ecotype+Year_pub)

#stepped models:
species_stepped = glm(log10(Species)~Latitude+Ecotype+Year_pub)
conn_stepped = glm(log10(Connectance)~Ecotype+Year_pub)
links_stepped = glm(log10(LS)~Latitude+Ecotype+Year_pub)

species_justlat = glm(log10(Species)~Latitude+Year_pub)
links_justlat = glm(log10(LS)~Latitude+Year_pub)
conn_justlat = glm(log10(Connectance)~Latitude+Year_pub)


*Year published main significant thing, ask Daniel how to standardize for that. Inc. as random effect?
* In additive ones, freshwater webs a little bit weird
* Just latitude, species and links significant. Opposite to expected trend, but hey.

##################################
##################################
## 		Part 2: testing scaling relationships
##################################
##################################

# Latitude

linkSD_scale=glm(log10(LinkSD)~Latitude*(log10(Connectance)+log10(Species)))
gen_scale=glm(log10(Gen)~Latitude*(log10(Connectance)+log10(Species)))
#genSD_scale=glm(log10(GenSD)~Latitude*(log10(Connectance)+log10(Species)))
#log scale not working
genSD_scale=glm(GenSD~Latitude*(log10(Connectance)+log10(Species)))
vul_scale=glm(log10(Vul)~Latitude*(log10(Connectance)+log10(Species)))
vulSD_scale=glm(log10(VulSD)~Latitude*(log10(Connectance)+log10(Species)))
mean_SWTL_scale=glm(log10(mean_SWTL)~Latitude*(log10(Connectance)+log10(Species)))
max_SWTL_scale=glm(log10(max_SWTL)~Latitude*(log10(Connectance)+log10(Species)))
path_scale=glm(log10(Path)~Latitude*(log10(Connectance)+log10(Species)))
#clus_scale=glm(log10(Clus)~Latitude*(log10(Connectance)+log10(Species)))
#log-scaling isn't working here. Raw works.
clus_scale=glm(Clus~Latitude*(log10(Connectance)+log10(Species)))
basal_scale=glm(pBas~Latitude*(log10(Connectance)+log10(Species)),family='binomial')
int_scale=glm(pInt~Latitude*(log10(Connectance)+log10(Species)),family='binomial')
top_scale=glm(pTop~Latitude*(log10(Connectance)+log10(Species)),family='binomial')
herb_scale=glm(pHerb~Latitude*(log10(Connectance)+log10(Species)),family='binomial')
omni_scale=glm(pOmni~Latitude*(log10(Connectance)+log10(Species)),family='binomial')

# both

linkSD_both=glm(log10(LinkSD)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
gen_both=glm(log10(Gen)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
#genSD_both=glm(log10(GenSD)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
#log scale not working
genSD_both=glm(GenSD~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
vul_both=glm(log10(Vul)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
vulSD_both=glm(log10(VulSD)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
mean_SWTL_both=glm(log10(mean_SWTL)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
max_SWTL_both=glm(log10(max_SWTL)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
path_both=glm(log10(Path)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
#clus_both=glm(log10(Clus)~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
#log-scaling isn't working here. Raw works.
clus_both=glm(Clus~Latitude*Ecotype*(log10(Connectance)+log10(Species)))
basal_both=glm(pBas~Latitude*Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
int_both=glm(pInt~Latitude*Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
top_both=glm(pTop~Latitude*Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
herb_both=glm(pHerb~Latitude*Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
omni_both=glm(pOmni~Latitude*Ecotype*(log10(Connectance)+log10(Species)),family='binomial')

# Ecotype

linkSD_eco=glm(log10(LinkSD)~Ecotype*(log10(Connectance)+log10(Species)))
gen_eco=glm(log10(Gen)~Ecotype*(log10(Connectance)+log10(Species)))
#genSD_eco=glm(log10(GenSD)~Ecotype*(log10(Connectance)+log10(Species)))
#log scale not working
genSD_eco=glm(GenSD~Ecotype*(log10(Connectance)+log10(Species)))
vul_eco=glm(log10(Vul)~Ecotype*(log10(Connectance)+log10(Species)))
vulSD_eco=glm(log10(VulSD)~Ecotype*(log10(Connectance)+log10(Species)))
mean_SWTL_eco=glm(log10(mean_SWTL)~Ecotype*(log10(Connectance)+log10(Species)))
max_SWTL_eco=glm(log10(max_SWTL)~Ecotype*(log10(Connectance)+log10(Species)))
path_eco=glm(log10(Path)~Ecotype*(log10(Connectance)+log10(Species)))
#clus_eco=glm(log10(Clus)~Ecotype*(log10(Connectance)+log10(Species)))
#log-scaling isn't working here. Raw works.
clus_eco=glm(Clus~Ecotype*(log10(Connectance)+log10(Species)))
basal_eco=glm(pBas~Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
int_eco=glm(pInt~Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
top_eco=glm(pTop~Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
herb_eco=glm(pHerb~Ecotype*(log10(Connectance)+log10(Species)),family='binomial')
omni_eco=glm(pOmni~Ecotype*(log10(Connectance)+log10(Species)),family='binomial')


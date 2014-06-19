data=read.table('../mod_data/summary-properties.tsv',sep='\t',header=TRUE)
attach(data)

##################################
##################################
##    Part 1: testing S, C, LS over lat
##################################
##################################

#Including year published as a random effect

species_latbytype = glm(log10(Species)~Latitude*(Ecotype2-1)+(1|Year_pub))
links_latbytype = glm(log10(LS)~Latitude*(Ecotype2-1)+(1|Year_pub))
conn_latbytype = glm(log10(Connectance)~Latitude*(Ecotype2-1)+(1|Year_pub))

#dredged models:
species_min = glm(log10(Species)~Ecotype2-1+(1|Year_pub))
links_min = glm(log10(LS)~Ecotype2-1+(1|Year_pub))
conn_min = glm(log10(Connectance)~Ecotype2-1+(1|Year_pub))

##################################
##################################
##    Part 2: testing scaling relationships
##################################
##################################

linkSD_scale=glm(log10(LinkSD)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
gen_scale=glm(log10(Gen)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# 5 (very small) webs with 0 GenSD is messing with this. Eliminating  1  4 30 84 85 for this model.
Gendata=data[data$GenSD!=0,]
genSD_scale=with(Gendata,glm(log10(GenSD)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub)))
vul_scale=glm(log10(Vul)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
vulSD_scale=glm(log10(VulSD)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
mean_SWTL_scale=glm(log10(mean_SWTL)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
path_scale=glm(log10(Path)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
basal_scale=glm(pBas~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
int_scale=glm(pInt~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
top_scale=glm(pTop~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
herb_scale=glm(pHerb~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
omni_scale=glm(pOmni~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')


#Dredging in now.

mean_SWTL_dredged=glm(log10(mean_SWTL)~(Latitude+Ecotype2)*log10(Species)+(1|Year_pub))
gen_dredged=glm(log10(Gen)~(Latitude*log10(Species)+Ecotype2+Ecotype2:log10(Species)+log10(Connectance))+(1|Year_pub))

int_dredged=glm(pInt~Latitude*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial') #No sig. scaling.

vul_dredged=glm(log10(Vul)~Latitude*log10(Species)+log10(Connectance)+(1|Year_pub))
vulSD_dredged=glm(log10(VulSD)~Latitude+log10(Species)+log10(Connectance)+Ecotype2+(1|Year_pub))
path_dredged=glm(log10(Path)~Latitude+Ecotype2*log10(Connectance)+(1|Year_pub))

linkSD_dredged=glm(log10(LinkSD)~Ecotype2*log10(Connectance)+log10(Species)+(1|Year_pub))

genSD_dredged=with(Gendata,glm(GenSD~log10(Connectance)+log10(Species)+(1|Year_pub)))
vul_dredged=glm(log10(Vul)~log10(Connectance)+log10(Species)+(1|Year_pub))
top_dredged=glm(pTop~log10(Connectance)+(1|Year_pub),family='binomial')

basal_dredged=glm(pBas~(1|Year_pub),family='binomial')
herb_dredged=glm(pHerb~(1|Year_pub),family='binomial')
omni_dredged=glm(pOmni~(1|Year_pub),family='binomial')


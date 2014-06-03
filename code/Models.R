data=read.table('../mod_data/summary-properties.tsv',sep='\t',header=TRUE)
attach(data)

##################################
##################################
##		Part 1: testing S, C, LS over lat
##################################
##################################

#Including year published as a randtom effect

species_latbytype = glm(log10(Species)~Latitude*(Ecotype-1)+(1|Year_pub))
links_latbytype = glm(log10(LS)~Latitude*(Ecotype-1)+(1|Year_pub))
conn_latbytype = glm(log10(Connectance)~Latitude*(Ecotype-1)+(1|Year_pub))

#dredged models:
species_min = glm(log10(Species)~Ecotype-1+(1|Year_pub))
links_min = glm(log10(LS)~Ecotype-1+(1|Year_pub))
conn_min = glm(log10(Connectance)~Ecotype-1+(1|Year_pub))

##################################
##################################
## 		Part 2: testing scaling relationships
##################################
##################################

linkSD_scale=glm(log10(LinkSD)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
gen_scale=glm(log10(Gen)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# 5 (very small) webs with 0 GenSD is messing with this. Eliminating  1  4 30 84 85 for this model.
Gendata=data[data$GenSD!=0,]
genSD_scale=with(Gendata,glm(log10(GenSD)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub)))
vul_scale=glm(log10(Vul)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
vulSD_scale=glm(log10(VulSD)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
mean_SWTL_scale=glm(log10(mean_SWTL)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
max_SWTL_scale=glm(log10(max_SWTL)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
path_scale=glm(log10(Path)~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
basal_scale=glm(pBas~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
int_scale=glm(pInt~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
top_scale=glm(pTop~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
herb_scale=glm(pHerb~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
omni_scale=glm(pOmni~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')

#Motifs are not to be logged.
x102_scale=glm(X102~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x108_scale=glm(X108~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x110_scale=glm(X110~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x12_scale=glm(X12~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x14_scale=glm(X14~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x238_scale=glm(X238~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x36_scale=glm(X36~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x38_scale=glm(X38~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x46_scale=glm(X46~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x6_scale=glm(X6~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x74_scale=glm(X74~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x78_scale=glm(X78~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))
x98_scale=glm(X98~(Latitude*Ecotype)*(log10(Connectance)+log10(Species))+(1|Year_pub))

#Dredging in now.

mean_SWTL_dredged=glm(log10(mean_SWTL)~(Latitude+Ecotype)*log10(Species)+(1|Year_pub))
max_SWTL_dredged=glm(log10(max_SWTL)~Latitude*log10(Species)+Ecotype*log10(Connectance)+(1|Year_pub))
int_dredged=glm(pInt~Latitude*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
gen_dredged=glm(log10(Gen)~(Latitude*log10(Species)+Ecotype+Ecotype:log10(Species)+log10(Connectance))+(1|Year_pub))

vulSD_dredged=glm(log10(VulSD)~Latitude+Ecotype+log10(Connectance)+log10(Species)+(1|Year_pub))
path_dredged=glm(log10(Path)~Latitude+Ecotype*log10(Connectance)+(1|Year_pub))

linkSD_dredged=glm(log10(LinkSD)~Ecotype*log10(Connectance)+log10(Species)+(1|Year_pub))

genSD_dredged=with(Gendata,glm(GenSD~log10(Connectance)+log10(Species)+(1|Year_pub)))
vul_dredged=glm(log10(Vul)~log10(Connectance)+log10(Species)+(1|Year_pub))
top_dredged=glm(pTop~log10(Connectance)+(1|Year_pub),family='binomial')

basal_dredged=glm(pBas~(1|Year_pub),family='binomial')
herb_dredged=glm(pHerb~(1|Year_pub),family='binomial')
omni_dredged=glm(pOmni~(1|Year_pub),family='binomial')

# Logged motifs don't work.



# Non-logged motifs
#Ecotype
x102_dredged=glm(X102~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x108_dredged=glm(X108~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x110_dredged=glm(X110~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x14_dredged=glm(X14~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x238_dredged=glm(X238~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x46_dredged=glm(X46~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x74_dredged=glm(X74~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))
x78_dredged=glm(X78~Ecotype*(log10(Connectance)+log10(Species))+(1|Year_pub))

#No predictor
x12_dredged=glm(X12~log10(Connectance)+log10(Species)+(1|Year_pub))
x36_dredged=glm(X36~log10(Connectance)+log10(Species)+(1|Year_pub))
x6_dredged=glm(X6~log10(Connectance)+log10(Species)+(1|Year_pub))

#Latitude
x108_dredged=glm(X108~Latitude*log10(Connectance)+(1|Year_pub))

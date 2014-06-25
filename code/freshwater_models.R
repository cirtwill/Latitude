data=read.table('../mod_data/summary-properties.tsv',sep='\t',header=TRUE)


## Testing which is better for LS, log or nls:

x=data$Species
y=data$LS

power_analysis(x,y) based on http://www.esapubs.org/archive/ecol/E092/160/Sup_2_Guidelines.r
log-normal error better supported :)

For generality: same :)

For TL: log-normal on both S and C. 


LS_full=with(data,glm(log10(LS)~log10(Species)+log10(Species):Latitude+log10(Species):(Fresh+Marine+Terr)+log10(Species):Latitude:(Fresh+Marine+Terr)+Year_pub))
LS_min=with(data,glm(log10(LS)~log10(Species)+log10(Species):Fresh+log10(Species):Marine+log10(Species):Latitude+log10(Species):Latitude:Fresh))

Gen_full=with(data,glm(log10(Gen)~log10(Species)+log10(Species):Latitude+log10(Species):(Fresh+Marine+Terr)+log10(Species):Latitude:(Fresh+Marine+Terr)+Year_pub))
Gen_min=with(data,glm(log10(Gen)~log10(Species)+log10(Species):Fresh+log10(Species):Latitude+log10(Species):Latitude:Fresh+Year_pub))

Vul_full=with(data,glm(log10(Vul)~log10(Species)+log10(Species):Latitude+log10(Species):(Fresh+Marine+Terr)+log10(Species):Latitude:(Fresh+Marine+Terr)+Year_pub))
Vul_min=with(data,glm(log10(Vul)~log10(Species)+log10(Species):Fresh+log10(Species):Marine+log10(Species):Latitude+log10(Species):Latitude:Fresh))



##################################
##################################
##    Part 1: testing S, C, LS over lat
##################################
##################################

n_LS_full <- nls(LS ~ A*Species**(B+C*Latitude+D*Fresh+E*Marine+F*Terr+G*Latitude*Fresh+H*Latitude*Marine+I*Latitude*Terr),
  data = data,start = list(A=0.35, B=0.48, C=0.002, D=.1, E=.1, F=.02,G=.01,H=.01,I=.01))
# n_LS_1 <- nls(LS ~ A*Species**(B+C*Latitude+D*Fresh+E*Marine+F*Terr+G*Latitude*Fresh+I*Latitude*Terr),
#   data = data,start = list(A=0.35, B=0.48, C=0.002, D=.1, E=.1, F=.02,G=.01,I=.01))
# n_LS_2 <- nls(LS ~ A*Species**(B+C*Latitude+D*Fresh+E*Marine+F*Terr+G*Latitude*Fresh),
#   data = data,start = list(A=0.35, B=0.48, C=0.002, D=.1, E=.1, F=.02,G=.01))
# n_LS_3 <- nls(LS ~ A*Species**(B+C*Latitude+D*Fresh+E*Marine+F*Terr+G*Latitude*Fresh+H*Latitude*Estuary),
#   data = data,start = list(A=0.35, B=0.48, C=0.002, D=.1, E=.1, F=.02,G=.01,H=.01))
# n_LS_4 <- nls(LS ~ A*Species**(B+C*Latitude+D*Fresh+E*Marine+G*Latitude*Fresh),
#   data = data,start = list(A=0.35, B=0.48, C=0.002, D=.1, E=.1,G=.01))
# n_LS_5 <- nls(LS ~ A*Species**(B+C*Latitude+E*Marine+G*Latitude*Fresh),
#   data = data,start = list(A=0.35, B=0.48, C=0.002, E=.1,G=.01))

n_LS_6 <- nls(LS ~ A*Species**(B+E*Marine+G*Latitude*Fresh),data = data,start = list(A=0.35, B=0.48, E=.1,G=.01)) #LS gradient in Freshwater

n_gen_full <- nls(Gen ~ A*Species**(B+C*Latitude+D*Fresh+E*Marine+F*Terr)*Connectance**(J+K*Latitude+L*Fresh+M*Marine+N*Terr),
  data = data,start = list(A=0.02, B=-0.006, C=0.018, D=.14, E=.75, F=.75,J=-0.43 ,K=0.023,L=1.3,M=2.4,N=2.3))




# Gen_full=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh)
#    +Z*Year_pub,
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
#     R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1,Z=0),control=list(nlsTol=100000000))

# Gen_red1=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
#     R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1),control=list(nlsTol=100000000))

# Gen_red2a=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
#     R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

# Gen_red2b=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,
#     R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1),control=list(nlsTol=100000000))

Gen_best=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh), data=data,
  start=c(Q=0.02,A=-0.006,L=0.018,L2=0.023,F=-.43,B=0.1,C=0.75,D=0.14,R=0.1,S=0.96,T=0.14))

Vul_full=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
  Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh)
   +Z*Year_pub,
  data=data,
  start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
    R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1,Z=0),control=list(nlsTol=100000000))

Vul_red1=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
  Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh),
  data=data,
  start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
    R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1),control=list(nlsTol=100000000))

Vul_red2a=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
  Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),data=data,
  start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
    R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

Vul_red2b=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
  Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh),
  data=data,start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,
    R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1),control=list(nlsTol=100000000))

Vul_red3=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
  Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),
  data=data,start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,
    R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

Vul_red4a=gnls(mean_SWTL~Q*Species**(A+L*Latitude)*Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),
  data=data,start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

Vul_red_lats=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*Connectance**(F+L2*Latitude),
  data=data,start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))

Vul_red5a=gnls(mean_SWTL~Q*Species**(A+B*Estuary+C*Marine+D*Fresh)*Connectance**(F+L2*Latitude),
  data=data,start=c(Q=1.5,A=0.01,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))

Vul_red5b=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*Connectance**(F),
  data=data,start=c(Q=1.5,A=0.01,L=0.0005,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))

Vul_red_best=gnls(mean_SWTL~Q*Species**(A+B*Estuary+C*Marine+D*Fresh)*Connectance**(F),
  data=data,start=c(Q=1.5,A=0.01,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=.001))


# TL_full=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh)
#    +Z*Year_pub,
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
#     R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1,Z=0),control=list(nlsTol=100000000))

# TL_red1=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
#     R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1),control=list(nlsTol=100000000))

# TL_red2a=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh+LB*Latitude*Estuary+LC*Latitude*Marine+LD*Latitude*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,LB=0.1,LC=0.1,LD=0.1,
#     R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

# TL_red2b=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh+L2R*Latitude*Estuary+L2S*Latitude*Marine+L2T*Latitude*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,
#     R=0.1,S=0.1,T=0.1,L2R=.1,L2S=.1,L2T=.1),control=list(nlsTol=100000000))

# TL_red3=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
#   Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),
#   data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1,
#     R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

# TL_red4b=gnls(mean_SWTL~Q*Species**(A+L*Latitude)*Connectance**(F+L2*Latitude+R*Estuary+S*Marine+T*Fresh),
#   data=data,start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,R=0.1,S=0.1,T=0.1),control=list(nlsTol=100000000))

# TL_lats_best=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
#   Connectance**(F+L2*Latitude),  data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))
# TL_red_best1=gnls(mean_SWTL~Q*Species**(A+B*Estuary+C*Marine+D*Fresh)*
#   Connectance**(F+L2*Latitude),  data=data,
#   start=c(Q=1.5,A=0.01,L2=0.00005,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))
# TL_red_best2=gnls(mean_SWTL~Q*Species**(A+L*Latitude+B*Estuary+C*Marine+D*Fresh)*
#   Connectance**(F),  data=data,
#   start=c(Q=1.5,A=0.01,L=0.0005,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))

TL_red_best=gnls(mean_SWTL~Q*Species**(A+B*Estuary+C*Marine+D*Fresh)*Connectance**(F),  data=data,
  start=c(Q=1.5,A=0.01,F=.07,B=0.1,C=0.1,D=0.1),control=list(nlsTol=100000000))














# #Including year published as a random effect

# species_latbytype = glm(log10(Species)~Latitude*(Ecotype2-1)+(1|Year_pub))
# links_latbytype = glm(log10(LS)~Latitude*(Ecotype2-1)+(1|Year_pub))
# conn_latbytype = glm(log10(Connectance)~Latitude*(Ecotype2-1)+(1|Year_pub))

# #dredged models:
# species_min = glm(log10(Species)~Ecotype2-1+(1|Year_pub))
# links_min = glm(log10(LS)~Ecotype2-1+(1|Year_pub))
# conn_min = glm(log10(Connectance)~Ecotype2-1+(1|Year_pub))

# ##################################
# ##################################
# ##    Part 2: testing scaling relationships
# ##################################
# ##################################

# linkSD_scale=glm(log10(LinkSD)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# gen_scale=glm(log10(Gen)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# # 5 (very small) webs with 0 GenSD is messing with this. Eliminating  1  4 30 84 85 for this model.
# Gendata=data[data$GenSD!=0,]
# genSD_scale=with(Gendata,glm(log10(GenSD)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub)))
# vul_scale=glm(log10(Vul)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# vulSD_scale=glm(log10(VulSD)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# mean_SWTL_scale=glm(log10(mean_SWTL)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# path_scale=glm(log10(Path)~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub))
# basal_scale=glm(pBas~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
# int_scale=glm(pInt~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
# top_scale=glm(pTop~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
# herb_scale=glm(pHerb~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')
# omni_scale=glm(pOmni~(Latitude*Ecotype2)*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial')

# #Dredging in now.

# mean_SWTL_dredged=glm(log10(mean_SWTL)~(Latitude+Ecotype2)*log10(Species))
# gen_dredged=glm(log10(Gen)~(Latitude*log10(Species)+Ecotype2+Ecotype2:log10(Species)+log10(Connectance))+(1|Year_pub))

# #NS
# vul_dredged=glm(log10(Vul)~Latitude*log10(Species)+log10(Connectance)+(1|Year_pub))
# int_dredged=glm(pInt~Latitude*(log10(Connectance)+log10(Species))+(1|Year_pub),family='binomial') #No sig. scaling.
# vulSD_dredged=glm(log10(VulSD)~Latitude+log10(Species)+log10(Connectance)+Ecotype2+(1|Year_pub))

# #Lat sig but doesn't affect scaling.
# path_dredged=glm(log10(Path)~Latitude+Ecotype2*log10(Connectance)+(1|Year_pub))

# #Ecoype only
# linkSD_dredged=glm(log10(LinkSD)~Ecotype2*log10(Connectance)+log10(Species)+(1|Year_pub))

# genSD_dredged=with(Gendata,glm(GenSD~log10(Connectance)+log10(Species)+(1|Year_pub)))
# vul_dredged=glm(log10(Vul)~log10(Connectance)+log10(Species)+(1|Year_pub))
# top_dredged=glm(pTop~log10(Connectance)+(1|Year_pub),family='binomial')

# basal_dredged=glm(pBas~(1|Year_pub),family='binomial')
# herb_dredged=glm(pHerb~(1|Year_pub),family='binomial')
# omni_dredged=glm(pOmni~(1|Year_pub),family='binomial')
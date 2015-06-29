## Power analysis function from http://www.esapubs.org/archive/ecol/E092/160/Sup_2_Guidelines.r
## Loading required packages. May need to be installed from CRAN.
library(nlrwr)
library(lmerTest)
library(MuMIn)

infile='../non_TS/summary-properties.tsv'
# infile='../mod_data/summary-properties.tsv'

format='proportions'
# format='numbers' # Don't care about numbers actually

power_analysis=FALSE
# For proportions and numbers, everything is log-normal.

  data=read.delim(infile,sep='\t',header=TRUE,stringsAsFactors=FALSE)
  data$Connectance=as.numeric(as.character(data$Connectance))
  data$LS=as.numeric(as.character(data$LS))
  data$Gen=as.numeric(as.character(data$Gen))
  data$Vul=as.numeric(as.character(data$Vul))
  data$Ecotype=as.factor(data$Ecotype)
  data$Site=as.factor(data$Site)
  data$Basal=as.numeric(as.character(data$Basal))
  data$Herbivores=as.numeric(as.character(data$Herbivores))
  data$Intermediate=as.numeric(as.character(data$Intermediate))
  data$Intermediate=data$Intermediate+data$Herbivores   # Role herbivores into intermediates
  data$Toppreds=as.numeric(as.character(data$Toppreds))

  data$Estuary=0
  data$Lake=0
  data$Marine=0
  data$Stream=0
  data$Terr=0

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='estuary'){
      data$Estuary[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='lake'){
      data$Lake[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='marine'){
      data$Marine[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='stream'){
      data$Stream[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='terrestrial'){
      data$Terr[i]=1  }}

  data=as.data.frame(data)
  data=subset(data,data$Basal>0)
  data=subset(data,data$Intermediate>0)
  data=subset(data,data$Toppreds>0)
  data$Latitude = as.numeric(as.character(data$Latitude))
  # Let's just make sure this is a numeric...
  # Eliminate zeros. Don't care so much about herbies

  source('recreate_with_subset.R')

  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/'  } else {
      outdir='../non_TS/'     }

  if(format=='proportions'){
    source('property_models_proportions.R')
    save.image(file=paste(outdir,'proportion_Models.RData',sep=''))

  } else {
    source('property_models_numbers.R')
    save.image(file=paste(outdir,'number_Models.RData',sep=''))
  }

  if(power_analysis==TRUE){
    source('power_analyses.R')
  }

  # TS and non-TS webs, logarithmic models are always better.
  # Not even sure if I care about TS webs for this.

  # Make some predictions for plotting
  # Observed species range is 3!-169.

  # Want species on the x-axis, properties on y. 
  # Maybe 3 levels of latitude.
  # Range is 0 - 78 degrees.
  # 0, 45, 75 == equatorial, temperate, arctic
  newdata=matrix(nrow=500*10*5,ncol=7)
  k=1
  for(j in c("Lake","Marine","Stream","Terr","Other")){
    for(latitude in c(0,10,20,30,40,50,60,70,80,90)){
        for(i in 1:500){
        # newdata[k,1]=i
        newdata[k,1]=latitude
        newdata[k,2]=0
        newdata[k,3]=0
        newdata[k,4]=0
        newdata[k,5]=0
        if(j=="Lake"){
          newdata[k,2]=1 }
        if(j=="Marine"){
          newdata[k,3]=1 }
        if(j=="Stream"){
          newdata[k,4]=1 }
        if(j=="Terr"){
          newdata[k,5]=1 }
        newdata[k,6]=i/500
        newdata[k,7]=(i/500)*200

        k=k+1
  }}}
  colnames(newdata)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species")  

  newdata=as.data.frame(newdata) 
  newdata$Intermediate=newdata$Basal  # Because there's no interaction
  newdata$Toppreds=newdata$Basal  # I can just use the same values for each pred

  # No link function, so no type="response"
  LS_preds=predict(LS_min,newdata=newdata)
  Gen_preds=predict(Gen_min,newdata=newdata)
  Vul_preds=predict(Vul_min,newdata=newdata)

  LS_fake=cbind(newdata,LS_preds)
  Gen_fake=cbind(newdata,Gen_preds)
  Vul_fake=cbind(newdata,Vul_preds)


  LB_preds=predict(LS_B_min,newdata=newdata)
  LI_preds=predict(LS_I_min,newdata=newdata)
  LT_preds=predict(LS_T_min,newdata=newdata)

  GB_preds=predict(Gen_B_min,newdata=newdata)
  GI_preds=predict(Gen_I_min,newdata=newdata)
  GT_preds=predict(Gen_T_min,newdata=newdata)

  VB_preds=predict(Vul_B_min,newdata=newdata)
  VI_preds=predict(Vul_I_min,newdata=newdata)
  VT_preds=predict(Vul_T_min,newdata=newdata)

  LB_fake=cbind(newdata,LB_preds)
  LI_fake=cbind(newdata,LI_preds)
  LT_fake=cbind(newdata,LT_preds)

  GB_fake=cbind(newdata,GB_preds)
  GI_fake=cbind(newdata,GI_preds)
  GT_fake=cbind(newdata,GT_preds)

  VB_fake=cbind(newdata,VB_preds)
  VI_fake=cbind(newdata,VI_preds)
  VT_fake=cbind(newdata,VT_preds)

#################################################################

  colnames(LS_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(Gen_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(Vul_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")

  colnames(LB_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(LI_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(LT_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")

  colnames(GB_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(GI_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(GT_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")

  colnames(VB_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(VI_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(VT_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
 
  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/'  } else {
      outdir='../non_TS/'     }
  # Data
  write.table(LS_fake,file=paste(outdir,'subset/predictions/LS.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Gen_fake,file=paste(outdir,'subset/predictions/Gen.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Vul_fake,file=paste(outdir,'subset/predictions/Vul.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  # Predictions
  write.table(summary(LS_min)$coefficients,file=paste(outdir,'subset/coefficients/LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Gen_min)$coefficients,file=paste(outdir,'subset/coefficients/Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Vul_min)$coefficients,file=paste(outdir,'subset/coefficients/Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  # Coefficients from non-corrected models
  write.table(summary(obs_LS)$coefficients,file=paste(outdir,'subset/coefficients/LS_obs.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(obs_Gen)$coefficients,file=paste(outdir,'subset/coefficients/Gen_obs.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(obs_Vul)$coefficients,file=paste(outdir,'subset/coefficients/Vul_obs.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')

  write.table(LB_fake,file=paste(outdir,'predictions/LB.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(LI_fake,file=paste(outdir,'predictions/LI.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(LT_fake,file=paste(outdir,'predictions/LT.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   

  write.table(GB_fake,file=paste(outdir,'predictions/GB.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(GI_fake,file=paste(outdir,'predictions/GI.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(GT_fake,file=paste(outdir,'predictions/GT.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   

  write.table(VB_fake,file=paste(outdir,'predictions/VB.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(VI_fake,file=paste(outdir,'predictions/VI.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(VT_fake,file=paste(outdir,'predictions/VT.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   

  write.table(summary(LS_B_min)$coefficients,file=paste(outdir,'coefficients/LB_co.tsv',sep=''),sep='\t')    
  write.table(summary(LS_I_min)$coefficients,file=paste(outdir,'coefficients/LI_co.tsv',sep=''),sep='\t')    
  write.table(summary(LS_T_min)$coefficients,file=paste(outdir,'coefficients/LT_co.tsv',sep=''),sep='\t')

  write.table(summary(Gen_B_min)$coefficients,file=paste(outdir,'coefficients/GB_co.tsv',sep=''),sep='\t')    
  write.table(summary(Gen_I_min)$coefficients,file=paste(outdir,'coefficients/GI_co.tsv',sep=''),sep='\t')    
  write.table(summary(Gen_T_min)$coefficients,file=paste(outdir,'coefficients/GT_co.tsv',sep=''),sep='\t')    

  write.table(summary(Vul_B_min)$coefficients,file=paste(outdir,'coefficients/VB_co.tsv',sep=''),sep='\t')    
  write.table(summary(Vul_I_min)$coefficients,file=paste(outdir,'coefficients/VI_co.tsv',sep=''),sep='\t')    
  write.table(summary(Vul_T_min)$coefficients,file=paste(outdir,'coefficients/VT_co.tsv',sep=''),sep='\t')    

  write.table(summary(obs_LS_B)$coefficients,file=paste(outdir,'coefficients/LB_obs.tsv',sep=''),sep='\t')    
  write.table(summary(obs_LS_I)$coefficients,file=paste(outdir,'coefficients/LI_obs.tsv',sep=''),sep='\t')    
  write.table(summary(obs_LS_T)$coefficients,file=paste(outdir,'coefficients/LT_obs.tsv',sep=''),sep='\t')

  write.table(summary(obs_Gen_B)$coefficients,file=paste(outdir,'coefficients/GB_obs.tsv',sep=''),sep='\t')    
  write.table(summary(obs_Gen_I)$coefficients,file=paste(outdir,'coefficients/GI_obs.tsv',sep=''),sep='\t')    
  write.table(summary(obs_Gen_T)$coefficients,file=paste(outdir,'coefficients/GT_obs.tsv',sep=''),sep='\t')    

  write.table(summary(obs_Vul_B)$coefficients,file=paste(outdir,'coefficients/VB_obs.tsv',sep=''),sep='\t')    
  write.table(summary(obs_Vul_I)$coefficients,file=paste(outdir,'coefficients/VI_obs.tsv',sep=''),sep='\t')    
  write.table(summary(obs_Vul_T)$coefficients,file=paste(outdir,'coefficients/VT_obs.tsv',sep=''),sep='\t')    


  ########################################################################################################
  ########################################################################################################

  if(format=='numbers'){
    write.table(summary(B_latdirect_num)$coefficients,file=paste(outdir,'coefficients/B_lobs.tsv',sep=''))
    write.table(summary(I_latdirect_num)$coefficients,file=paste(outdir,'coefficients/I_lat.tsv',sep=''))
    write.table(summary(T_latdirect_num)$coefficients,file=paste(outdir,'coefficients/T_lat.tsv',sep=''))
  }
  if(format=='proportions'){
    write.table(summary(B_latdirect)$coefficients,file=paste(outdir,'coefficients/B_lat.tsv',sep=''))
    write.table(summary(I_latdirect)$coefficients,file=paste(outdir,'coefficients/I_lat.tsv',sep=''))
    write.table(summary(T_latdirect)$coefficients,file=paste(outdir,'coefficients/T_lat.tsv',sep=''))
  }

  ########################################################################################################
  ########################################################################################################
  ####################   Calculate marginal effect of latitude in each ecotype

  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/'  } else {
      outdir=paste('../non_TS/',format,'/',sep='')     }

  if(format=='proportions'){
    source('marginal_CIs.R')
  } else {
    source('number_marginal_CIs.R')
  }

  LS_marg=S_CIs("LS_min")
  write.table(LS_marg,file=paste(outdir,'marginals/LS_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Gen_marg=S_CIs("Gen_min")
  write.table(Gen_marg,file=paste(outdir,'marginals/Gen_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Vul_marg=S_CIs("Vul_min")
  write.table(Vul_marg,file=paste(outdir,'marginals/Vul_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)

  LS_B_marg=B_CIs("LS_B_min")
  write.table(LS_B_marg,file=paste(outdir,'marginals/LS_B_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Gen_B_marg=B_CIs("Gen_B_min")
  write.table(Gen_B_marg,file=paste(outdir,'marginals/Gen_B_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Vul_B_marg=B_CIs("Vul_B_min")
  write.table(Vul_B_marg,file=paste(outdir,'marginals/Vul_B_marginal.tsv',sep=''),sep='\t',col.names=TRUE)

  LS_I_marg=I_CIs("LS_I_min")
  write.table(LS_I_marg,file=paste(outdir,'marginals/LS_I_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Gen_I_marg=I_CIs("Gen_I_min")
  write.table(Gen_I_marg,file=paste(outdir,'marginals/Gen_I_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Vul_I_marg=I_CIs("Vul_I_min")
  write.table(Vul_I_marg,file=paste(outdir,'marginals/Vul_I_marginal.tsv',sep=''),sep='\t',col.names=TRUE)

  LS_T_marg=T_CIs("LS_T_min")
  write.table(LS_T_marg,file=paste(outdir,'marginals/LS_T_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Gen_T_marg=T_CIs("Gen_T_min")
  write.table(Gen_T_marg,file=paste(outdir,'marginals/Gen_T_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Vul_T_marg=T_CIs("Vul_T_min")
  write.table(Vul_T_marg,file=paste(outdir,'marginals/Vul_T_marginal.tsv',sep=''),sep='\t',col.names=TRUE)


######### And we're done!!!!!! ###########
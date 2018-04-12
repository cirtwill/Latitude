
## Power analysis function from http://www.esapubs.org/archive/ecol/E092/160/Sup_2_Guidelines.r
## Loading required packages. May need to be installed from CRAN.
# library(nlrwr) ## Not available for 3.3.1
library(lmerTest)
library(boot)
library(MuMIn)

# Need to re-run with the corrected matrices...
# for(infile in c('../non_TS/summary-properties_extended_connected.tsv')){
# for(infile in c('../mod_data/summary-properties_extended_connected.tsv')){
format='proportions'
power_analysis=FALSE
by_TL=FALSE
for(infile in c('../mod_data/summary-properties_extended_connected.tsv','../non_TS/summary-properties_extended_connected.tsv')){
# for(infile in c('../non_TS/summary-properties_extended_connected.tsv')){

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
  data$Lakeweb=0
  data$Marine=0
  data$Stream=0
  data$Terr=0

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='estuary'){
      data$Estuary[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='lake'){
      data$Lakeweb[i]=1  }} # Right, the author "Lake"

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='marine'){
      data$Marine[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='stream'){
      data$Stream[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='terrestrial'){
      data$Terr[i]=1  }}


  # Eliminate webs that don`t have at least one basal, intermediate, and top species.
  data=as.data.frame(data)
  data=subset(data,data$Basal>0)
  data=subset(data,data$Intermediate>0)
  data=subset(data,data$Toppreds>0)
  data$Latitude = as.numeric(as.character(data$Latitude))

  # Save the original data to prevent accidents with jackknifing subsets.
  olddata=data
  # Not doing the jackknifing at this time, but keeping them just in case.
  # Calculate the models with the full dataset.
  data=olddata

  if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
    outdir='../updated/mod_data/'  } else {
      outdir='../updated/non_TS/'     }


  #Using lognormal with confidence for modified models, following previous version.
  LS_full=with(data,lm(log10(LS)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lakeweb+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
  ls_dredge=dredge(LS_full,rank=AIC)
  if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
    LS_min =with(data,lm(log10(LS)~log10(Species)
    +log10(Species):(Lakeweb+Marine+Terr)
    +log10(Species):Latitude
    +log10(Species):Latitude:Lakeweb
    ,na.action=na.fail))    } else {
    LS_min=with(data,lm(log10(LS)~log10(Species)
    +log10(Species):(Stream+Terr),
    na.action=na.fail))    }

  Gen_full=with(data,lm(log10(Gen)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lakeweb+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
  g_dredge=dredge(Gen_full,rank=AIC)
  if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
    Gen_min=with(data,lm(log10(Gen)~log10(Species)
      +log10(Species):(Lakeweb+Marine+Stream+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lakeweb
      +log10(Species):Latitude:Stream
      ,na.action=na.fail)) } else {
    Gen_min=with(data,lm(log10(Gen)~log10(Species)
      +log10(Species):(Lakeweb+Marine+Terr+Stream)
      +log10(Species):Latitude
      +log10(Species):Latitude:(Lakeweb+Stream)
      ,na.action=na.fail))
    }

  Vul_full=with(data,lm(log10(Vul)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lakeweb+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
  v_dredge=dredge(Vul_full,rank=AIC)
  if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
    Vul_min=with(data,lm(log10(Vul)~log10(Species)
      +log10(Species):(Lakeweb+Marine+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lakeweb
      ,na.action=na.fail)) } else {
    Vul_min=with(data,lm(log10(Vul)~log10(Species)
      +log10(Species):(Stream+Terr)
      ,na.action=na.fail))  }

  # What's the basic correlation with latitude?
  Sp_latdirect_full=(with(data,lm(Species~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  LS_latdirect_full=(with(data,lm(LS~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  Gen_latdirect_full=(with(data,lm(Gen~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  Vul_latdirect_full=(with(data,lm(Vul~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))

  sld=dredge(Sp_latdirect_full,rank=AIC)
  lld=dredge(LS_latdirect_full,rank=AIC)
  gld=dredge(Gen_latdirect_full,rank=AIC)
  vld=dredge(Vul_latdirect_full,rank=AIC)

  Sp_latdirect=(with(data,lm(Species~Lakeweb+Marine+Terr),na.action=na.fail))
  LS_latdirect=(with(data,lm(LS~Lakeweb+Terr),na.action=na.fail))
  if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
      Gen_latdirect=(with(data,lm(Gen~Terr+Lakeweb),na.action=na.fail))  
    } else {
      Gen_latdirect=(with(data,lm(Gen~Terr+Stream),na.action=na.fail))  
    }
  Vul_latdirect=(with(data,lm(Vul~Lakeweb+Terr),na.action=na.fail))

  obs_LS=(with(data,lm(log(LS)~log(Species),na.action=na.fail)))
  obs_Gen=(with(data,lm(log(Gen)~log(Species),na.action=na.fail)))
  obs_Vul=(with(data,lm(log(Vul)~log(Species),na.action=na.fail)))

  newdata=matrix(nrow=500*10*5,ncol=7)
  k=1
  for(j in c("Lakeweb","Marine","Stream","Terr","Other")){
    for(latitude in c(0,10,20,30,40,50,60,70,80,90)){
        for(i in 1:500){
        # newdata[k,1]=i
        newdata[k,1]=latitude
        newdata[k,2]=0 # Lake
        newdata[k,3]=0 # Marine
        newdata[k,4]=0 # Stream
        newdata[k,5]=0 # Terrestrial
        if(j=="Lake"){
          newdata[k,2]=1 }
        if(j=="Marine"){
          newdata[k,3]=1 }
        if(j=="Stream"){
          newdata[k,4]=1 }
        if(j=="Terr"){
          newdata[k,5]=1 }
        newdata[k,6]=i/500 # Basal, leftover from when I was doing models by TL.
        newdata[k,7]=(i/500)*200 # Species

        k=k+1
  }}}
  colnames(newdata)=c("Latitude","Lakeweb","Marine","Stream","Terr","Basal","Species")  

  newdata=as.data.frame(newdata) 
  # No link function, so no type="response"
  LS_preds=predict(LS_min,newdata=newdata)
  Gen_preds=predict(Gen_min,newdata=newdata)
  Vul_preds=predict(Vul_min,newdata=newdata)

  LS_fake=cbind(newdata,LS_preds)
  Gen_fake=cbind(newdata,Gen_preds)
  Vul_fake=cbind(newdata,Vul_preds)

  colnames(LS_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","pred")
  colnames(Gen_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","pred")
  colnames(Vul_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","pred")

  if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
    outdir='../updated/mod_data/'  } else {
      outdir='../updated/non_TS/'     }

  save.image(file=paste(outdir,'updated_Models.RData',sep=''))
  # Data
  write.table(LS_fake,file=paste(outdir,'predictions/LS.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Gen_fake,file=paste(outdir,'predictions/Gen.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Vul_fake,file=paste(outdir,'predictions/Vul.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  # Predictions
  write.table(summary(LS_min)$coefficients,file=paste(outdir,'coefficients/LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Gen_min)$coefficients,file=paste(outdir,'coefficients/Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Vul_min)$coefficients,file=paste(outdir,'coefficients/Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  # Coefficients from non-corrected models
  write.table(summary(obs_LS)$coefficients,file=paste(outdir,'coefficients/LS_obs.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(obs_Gen)$coefficients,file=paste(outdir,'coefficients/Gen_obs.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(obs_Vul)$coefficients,file=paste(outdir,'coefficients/Vul_obs.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')

# Calculating marginal effect of latitude for each ecotype

if(infile=='../mod_data/summary-properties_extended_connected.tsv'){
  outdir='../updated/mod_data/'
  source('updated_marginal_CIs_TS.R')  } else {
    outdir='../updated/non_TS/'   
    source('updated_marginal_CIs_nonTS.R') }


  LS_marg=S_CIs("LS_min")
  write.table(LS_marg,file=paste(outdir,'marginals/LS_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Gen_marg=S_CIs("Gen_min")
  write.table(Gen_marg,file=paste(outdir,'marginals/Gen_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  Vul_marg=S_CIs("Vul_min")
  write.table(Vul_marg,file=paste(outdir,'marginals/Vul_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)


}




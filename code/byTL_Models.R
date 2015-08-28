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
by_TL=FALSE
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
  data$Lakeweb=0
  data$Marine=0
  data$Stream=0
  data$Terr=0

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='estuary'){
      data$Estuary[i]=1  }}

  for(i in 1:length(data$Ecotype)){
    if(data$Ecotype[i]=='lake'){ 
      data$Lakeweb[i]=1  }} # Annoyingly, there's also an author called "Lake"

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


  # # Check for redundant authors
  # auths=as.data.frame(data[,20:188])
  # for(col1 in colnames(auths)){
  #   duplicates=c(col1)
  #   for(col2 in colnames(auths)){
  #     v1=as.vector(auths[,col1])
  #     v2=as.vector(auths[,col2])
  #     if(cor(v1,v2)==1){
  #       duplicates=union(duplicates,c(col2))
  #     }
  #   }
  #   if(length(duplicates)>1){
  #   print(duplicates)
  #   }
  # }

  # Eliminate the redundant authors for jacknifing.
  source('redundant_authors.R')

  # Calculate the base models.

  olddata=data

  # Jackknifing by webs (easy)
  for(i in 1:nrow(olddata)){
    newdata=olddata[-i,]
    name=olddata$Web[i]
    print(name)

    data=newdata
    source('recreate_with_subset.R')

    write.table(summary(LS_min)$coefficients,file=paste('../Jackknifed/main/coefficients/',name,'LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
    write.table(summary(Gen_min)$coefficients,file=paste('../Jackknifed/main/coefficients/',name,'Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
    write.table(summary(Vul_min)$coefficients,file=paste('../Jackknifed/main/coefficients/',name,'Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  }

  # Jackknifing by authors (slightly harder)

  # If an author is only on one web, then I've already checked that in the webwise version :)
  for(i in 20:189){
    if(sum(olddata[,i])>1){
      newdata=olddata[which(olddata[,i]==0),]
      name=colnames(olddata)[i]

      data=newdata
      source('recreate_with_subset.R')

      write.table(summary(LS_min)$coefficients,file=paste('../Jackknifed/main/coefficients/',name,'LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
      write.table(summary(Gen_min)$coefficients,file=paste('../Jackknifed/main/coefficients/',name,'Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
      write.table(summary(Vul_min)$coefficients,file=paste('../Jackknifed/main/coefficients/',name,'Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
    }
  }

  data=olddata

  source('recreate_with_subset.R')

  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/'  } else {
      outdir='../non_TS/'     }

  # Calculate the by-TL models
  if(by_TL==TRUE){
  if(format=='proportions'){
    source('property_models_proportions.R')
    save.image(file=paste(outdir,'proportion_Models.RData',sep=''))

  } else {
    source('property_models_numbers.R')
    save.image(file=paste(outdir,'number_Models.RData',sep=''))
  }
  }

  # Do the power analyses
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
        newdata[k,6]=i/500 # Basal
        newdata[k,7]=(i/500)*200 # Species

        k=k+1
  }}}
  colnames(newdata)=c("Latitude","Lakeweb","Marine","Stream","Terr","Basal","Species")  

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


  colnames(LS_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(Gen_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")
  colnames(Vul_fake)=c("Latitude","Lake","Marine","Stream","Terr","Basal","Species","Intermediate","Toppreds","pred")

  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/'  } else {
      outdir='../non_TS/'     }
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

  # Not going to bother working out the marginals for TS models.

  if(infile=='../non_TS/summary-properties.tsv'){

    LS_marg=S_CIs("LS_min")
    write.table(LS_marg,file=paste(outdir,'marginals/LS_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
    Gen_marg=S_CIs("Gen_min")
    write.table(Gen_marg,file=paste(outdir,'marginals/Gen_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
    Vul_marg=S_CIs("Vul_min")
    write.table(Vul_marg,file=paste(outdir,'marginals/Vul_S_marginal.tsv',sep=''),sep='\t',col.names=TRUE)
  }

######### And we're done!!!!!! ###########

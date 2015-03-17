
## Power analysis function from http://www.esapubs.org/archive/ecol/E092/160/Sup_2_Guidelines.r
## Loading required packages. May need to be installed from CRAN.
library(nlrwr)
library(lmerTest)
library(boot)
library(MuMIn)

for(infile in c('../mod_data/summary-properties.tsv','../non_TS/summary-properties.tsv')){

  data=read.delim(infile,sep='\t',header=TRUE,stringsAsFactors=FALSE)
  data$Connectance=as.numeric(as.character(data$Connectance))
  data$LS=as.numeric(as.character(data$LS))
  data$Gen=as.numeric(as.character(data$Gen))
  data$Vul=as.numeric(as.character(data$Vul))
  data$Ecotype=as.factor(data$Ecotype)
  data$Ecotype2=as.factor(data$Ecotype2)
  data$Humans=as.factor(data$Humans)
  data$Site=as.factor(data$Site)
  data$Basal=as.numeric(as.character(data$Basal))
  data$Herbivores=as.numeric(as.character(data$Herbivores))
  data$Intermediate=as.numeric(as.character(data$Intermediate))
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

  q=data$Species
  x=data$LS
  y=data$Gen
  z=data$Vul         


  #Therefore using lognormal with confidence for modified models.
  LS_full=with(data,lm(log10(LS)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lake+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  ls_dredge=dredge(LS_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    LS_min =with(data,lm(log10(LS)~log10(Species)
    +log10(Species):(Lake+Marine+Stream)
    +log10(Species):Latitude
    +log10(Species):Latitude:Lake
    +log10(Species):Latitude:Stream
    ,na.action=na.fail))    } else {
    LS_min=with(data,lm(log10(LS)~log10(Species)
    +log10(Species):(Lake+Marine+Terr)
    +log10(Species):Latitude
    +log10(Species):Latitude:Marine
    +log10(Species):Latitude:Terr,
    na.action=na.fail))    }

  Gen_full=with(data,lm(log10(Gen)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lake+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  g_dredge=dredge(Gen_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    Gen_min=with(data,lm(log10(Gen)~log10(Species)
      +log10(Species):(Lake+Marine+Stream)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      +log10(Species):Latitude:Stream
      ,na.action=na.fail)) } else {
    Gen_min=with(data,lm(log10(Gen)~log10(Species)
      +log10(Species):(Lake+Marine+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      ,na.action=na.fail))
    }

  Vul_full=with(data,lm(log10(Vul)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lake+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  v_dredge=dredge(Vul_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    Vul_min=with(data,lm(log10(Vul)~log10(Species)
      +log10(Species):(Lake+Marine+Stream)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      +log10(Species):Latitude:Stream
      ,na.action=na.fail)) } else {
    Vul_min=with(data,lm(log10(Vul)~log10(Species)
      +log10(Species):(Lake+Marine+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Marine
      +log10(Species):Latitude:Terr
      ,na.action=na.fail))  }

  # What's the basic correlation with latitude?
  Sp_latdirect_full=(with(data,lm(Species~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  LS_latdirect_full=(with(data,lm(LS~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  Gen_latdirect_full=(with(data,lm(Gen~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  Vul_latdirect_full=(with(data,lm(Vul~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))

  sld=dredge(Sp_latdirect_full,rank=AIC)
  lld=dredge(LS_latdirect_full,rank=AIC)
  gld=dredge(Gen_latdirect_full,rank=AIC)
  vld=dredge(Vul_latdirect_full,rank=AIC)

  Sp_latdirect=(with(data,lm(Species~Stream),na.action=na.fail))
  LS_latdirect=(with(data,lm(LS~1),na.action=na.fail))
  Gen_latdirect=(with(data,lm(Gen~Terr+Lake),na.action=na.fail))
  Vul_latdirect=(with(data,lm(Vul~1),na.action=na.fail))

  # Make some predictions for plotting
  # Observed species range is 3!-169.

  # Want species on the x-axis, properties on y. 
  # Maybe 3 levels of latitude.
  # Range is 0 - 78 degrees.
  # 0, 45, 75 == equatorial, temperate, arctic
  if(infile=='../mod_data/summary-properties.tsv'){
    newdata=matrix(nrow=200*3*4,ncol=5)
    k=1
    for(j in c("Lake","Marine","Stream","Other")){
      for(latitude in c(0,30,60)){
        for(i in 1:200){
        newdata[k,1]=i
        newdata[k,2]=latitude
        newdata[k,3]=0
        newdata[k,4]=0
        newdata[k,5]=0
        if(j=="Lake"){
          newdata[k,3]=1
          newdata[k,4]=0
          newdata[k,5]=0} 
        if(j=="Marine"){
          newdata[k,3]=0
          newdata[k,4]=1
          newdata[k,5]=0}
        if(j=="Stream"){
          newdata[k,3]=0
          newdata[k,4]=0
          newdata[k,5]=1}
        k=k+1
    }}}
    colnames(newdata)=c("Species","Latitude","Lake","Marine","Stream") } else {
    newdata=matrix(nrow=200*3*4,ncol=5)
    k=1
    for(j in c("Lake","Marine","Terr","Other")){
      for(latitude in c(0,30,60)){
        for(i in 1:200){
        newdata[k,1]=i
        newdata[k,2]=latitude
        newdata[k,3]=0
        newdata[k,4]=0
        newdata[k,5]=0
        if(j=="Lake"){
          newdata[k,3]=1
          newdata[k,4]=0
          newdata[k,5]=0} 
        if(j=="Marine"){
          newdata[k,3]=0
          newdata[k,4]=1
          newdata[k,5]=0}
        if(j=="Terr"){
          newdata[k,3]=0
          newdata[k,4]=0
          newdata[k,5]=1}
        k=k+1
    }}}
    colnames(newdata)=c("Species","Latitude","Lake","Marine","Terr")    }

  newdata=as.data.frame(newdata) 
  # No link function, so no type="response"
  LS_preds=predict(LS_min,newdata=newdata)
  Gen_preds=predict(Gen_min,newdata=newdata)
  Vul_preds=predict(Vul_min,newdata=newdata)

  LS_fake=cbind(newdata,LS_preds)
  Gen_fake=cbind(newdata,Gen_preds)
  Vul_fake=cbind(newdata,Vul_preds)

  colnames(LS_fake)=c("Species","Latitude","Lake","Marine","Stream","pred")
  colnames(Gen_fake)=c("Species","Latitude","Lake","Marine","Stream","pred")
  colnames(Vul_fake)=c("Species","Latitude","Lake","Marine","Stream","pred")

  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/'  } else {
      outdir='../non_TS/'     }

  save.image(file=paste(outdir,'Models.RData',sep=''))
  write.table(LS_fake,file=paste(outdir,'predictions/LS.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Gen_fake,file=paste(outdir,'predictions/Gen.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Vul_fake,file=paste(outdir,'predictions/Vul.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')

  write.table(summary(LS_min)$coefficients,file=paste(outdir,'coefficients/LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Gen_min)$coefficients,file=paste(outdir,'coefficients/Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Vul_min)$coefficients,file=paste(outdir,'coefficients/Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')

}

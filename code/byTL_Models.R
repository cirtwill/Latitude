## Power analysis function from http://www.esapubs.org/archive/ecol/E092/160/Sup_2_Guidelines.r
## Loading required packages. May need to be installed from CRAN.
library(nlrwr)
library(lmerTest)
library(boot)
library(MuMIn)

source('Models.R')  # Load the other models, power_analysis


# Set up for proportions and numbers, proportions first.



##############################################################################################
##############################################################################################
for(infile in c('../non_TS/summary-properties.tsv')){

  # c('../mod_data/summary-properties.tsv','../non_TS/summary-properties.tsv')){

  data=subset(data,data$Basal>0)
  data=subset(data,data$Intermediate>0)
  data=subset(data,data$Toppreds>0)
  # Eliminate zeros. Don't care so much about herbies

  q=data$Species
  x=data$LS
  y=data$Gen
  z=data$Vul         

  # Not working because of zeros...
  power_analysis(data$Species,data$Basal)   #To determine whether scaling of LS~S is more lognormal or nonlinear.
                        #It is lognormal, AIC=-141 vs -112 with nonlinear.
  # power_analysis(data$Species,data$Herbivores)   #To determine whether scaling of Gen~S is more lognormal or nonlinear.
  # Too many zeros.
  power_analysis(data$Species,data$Intermediate)   #To determine whether scaling of Vul~S is more lognormal or nonlinear.
                        #It is lognormal, AIC=-121.4 vs -72.6
  power_analysis(data$Species,data$Toppreds)   #To determine whether scaling of Vul~S is more lognormal or nonlinear.
                        #It is lognormal, AIC=-163.5 vs -138.7

  source('recreate_with_subset.R')

  if(format=='proportion'){
    source('property_models_proportions.R')
  } else {
    source('property_models_numbers.R')
  }


  #Both number and proportion look roughly equally interesting.... at least in terms of non-TS webs.
  # Not even sure if I care about TS webs for this.

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
    outdir='../mod_data/subset/'  } else {
      outdir='../non_TS/subset/'     }

  save.image(file=paste(outdir,'Models.RData',sep=''))
  write.table(LS_fake,file=paste(outdir,'predictions/LS.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Gen_fake,file=paste(outdir,'predictions/Gen.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Vul_fake,file=paste(outdir,'predictions/Vul.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')

  write.table(summary(LS_min)$coefficients,file=paste(outdir,'coefficients/LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Gen_min)$coefficients,file=paste(outdir,'coefficients/Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Vul_min)$coefficients,file=paste(outdir,'coefficients/Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')

}

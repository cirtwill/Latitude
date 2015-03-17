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
  newdata=matrix(nrow=200*3*5,ncol=6)
  k=1
  for(j in c("Lake","Marine","Stream","Terr","Other")){
    for(latitude in c(0,30,60)){
      for(i in 1:200){
      newdata[k,1]=i
      newdata[k,2]=latitude
      newdata[k,3]=0
      newdata[k,4]=0
      newdata[k,5]=0
      newdata[k,6]=0
      if(j=="Lake"){
        newdata[k,3]=1 }
      if(j=="Marine"){
        newdata[k,4]=1 }
      if(j=="Stream"){
        newdata[k,5]=1 }
      if(j=="Terr"){
        newdata[k,6]=1 }
      k=k+1
  }}}
  colnames(newdata)=c("Species","Latitude","Lake","Marine","Stream","Terr")  

  newdata=as.data.frame(newdata) 
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

  colnames(LS_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(Gen_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(Vul_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")

  colnames(LB_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(LI_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(LT_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")

  colnames(GB_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(GI_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(GT_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")

  colnames(VB_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(VI_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
  colnames(VT_fake)=c("Species","Latitude","Lake","Marine","Stream","Terr","pred")
 

  if(infile=='../mod_data/summary-properties.tsv'){
    outdir='../mod_data/subset/'  } else {
      outdir='../non_TS/subset/'     }

  save.image(file=paste(outdir,'Models.RData',sep=''))
  write.table(LS_fake,file=paste(outdir,'subset/predictions/LS.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Gen_fake,file=paste(outdir,'subset/predictions/Gen.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')
  write.table(Vul_fake,file=paste(outdir,'subset/predictions/Vul.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')

  write.table(summary(LS_min)$coefficients,file=paste(outdir,'subset/coefficients/LS_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Gen_min)$coefficients,file=paste(outdir,'subset/coefficients/Gen_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')
  write.table(summary(Vul_min)$coefficients,file=paste(outdir,'subset/coefficients/Vul_co.tsv',sep=''),col.names=TRUE,row.names=TRUE,sep='\t')

  write.table(LB_fake,file=paste(outdir,'predictions/LB.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(LI_fake,file=paste(outdir,'predictions/LI.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(LT_fake,file=paste(outdir,'predictions/LT.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   

  write.table(GB_fake,file=paste(outdir,'predictions/GB.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(GI_fake,file=paste(outdir,'predictions/GI.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(GT_fake,file=paste(outdir,'predictions/GT.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   

  write.table(VB_fake,file=paste(outdir,'predictions/VB.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(VI_fake,file=paste(outdir,'predictions/VI.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   
  write.table(VT_fake,file=paste(outdir,'predictions/VT.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')   

  write.table(summary(LB_fake)$coefficients,file=paste(outdir,'coefficients/LB_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    
  write.table(summary(LI_fake)$coefficients,file=paste(outdir,'coefficients/LI_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    
  write.table(summary(LT_fake)$coefficients,file=paste(outdir,'coefficients/LT_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    

  write.table(summary(GB_fake)$coefficients,file=paste(outdir,'coefficients/GB_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    
  write.table(summary(GI_fake)$coefficients,file=paste(outdir,'coefficients/GI_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    
  write.table(summary(GT_fake)$coefficients,file=paste(outdir,'coefficients/GT_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    

  write.table(summary(VB_fake)$coefficients,file=paste(outdir,'coefficients/VB_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    
  write.table(summary(VI_fake)$coefficients,file=paste(outdir,'coefficients/VI_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    
  write.table(summary(VT_fake)$coefficients,file=paste(outdir,'coefficients/VT_co.tsv',sep=''),col.names=TRUE,row.names=FALSE,sep='\t')    


}

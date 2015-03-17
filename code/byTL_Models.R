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
for(infile in c('../mod_data/summary-properties.tsv','../non_TS/summary-properties.tsv')){

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


    LS_B_full=with(data,lm(log10(LS)~log10(Basal)
    +log10(Basal):Latitude
    +log10(Basal):(Stream+Lake+Marine+Terr)
    +log10(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lbdredge=dredge(LS_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_B_min=with(data,lm(log10(LS)~log10(Basal)
        +log10(Basal):Latitude
        +log10(Basal):(Marine+Stream+Terr)
        +log10(Basal):Latitude:(Marine+Terr) , na.action=na.fail   ))
    }

    # And if I use number
    LS_B_full=with(data,lm(log10(LS)~log10(Basal*Species)
    +log10(Basal*Species):Latitude
    +log10(Basal*Species):(Stream+Lake+Marine+Terr)
    +log10(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lbdredge=dredge(LS_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_B_min=with(data,lm(log10(LS)~log10(Basal*Species)+log10(Basal*Species):Marine)) }
    # Boooooooring

    LS_I_full=with(data,lm(log10(LS)~log10(Intermediate)
    +log10(Intermediate):Latitude
    +log10(Intermediate):(Stream+Lake+Marine+Terr)
    +log10(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lidredge=dredge(LS_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_B_min=with(data,lm(log10(LS)~log10(Intermediate)
        +log10(Intermediate):(Lake+Stream), na.action=na.fail   ))
    }

    # And with number
    LS_I_full=with(data,lm(log10(LS)~log10(Intermediate*Species)
    +log10(Intermediate*Species):Latitude
    +log10(Intermediate*Species):(Stream+Lake+Marine+Terr)
    +log10(Intermediate*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lidredge=dredge(LS_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_I_min=with(data,lm(log10(LS)~log10(Intermediate*Species)
        +log10(Intermediate*Species):(Lake+Marine+Stream+Terr)
        +log10(Intermediate*Species):Latitude
        +log10(Intermediate*Species):Latitude:(Lake+Stream) )) 
    } # Dang, I was hoping this'd be simple....

    LS_T_full=with(data,lm(log10(LS)~log10(Toppreds)
    +log10(Toppreds):Latitude
    +log10(Toppreds):(Stream+Lake+Marine+Terr)
    +log10(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_T_min=with(data,lm(log10(LS)~log10(Toppreds)
      +log10(Toppreds):(Lake+Marine+Stream+Terr)
      +log10(Toppreds):Latitude
      +log10(Toppreds):Latitude:(Stream+Terr) ))
    }

    # And with number
    LS_T_full=with(data,lm(log10(LS)~log10(Toppreds*Species)
    +log10(Toppreds*Species):Latitude
    +log10(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log10(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)

    LS_T_min=with(data,lm(log10(LS)~log10(Toppreds*Species)
    +log10(Toppreds*Species):Latitude
    +log10(Toppreds*Species):(Lake+Marine+Terr)
    +log10(Toppreds*Species):Latitude:(Lake)
    ,na.action=na.fail))
    # And both of these are interesting. 


  # What's the basic correlation with latitude?
  B_latdirect_full=(with(data,lm(Basal~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  I_latdirect_full=(with(data,lm(Intermediate~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  T_latdirect_full=(with(data,lm(Toppreds~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  H_latdirect_full=(with(data,lm(Herbivores~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))

  bld=dredge(B_latdirect_full,rank=AIC)
  ild=dredge(I_latdirect_full,rank=AIC)
  tld=dredge(T_latdirect_full,rank=AIC)
  hld=dredge(H_latdirect_full,rank=AIC)

  B_latdirect=(with(data,lm(Basal~Latitude*Stream+Lake),na.action=na.fail))
  I_latdirect=(with(data,lm(Intermediate~1),na.action=na.fail))
  T_latdirect=(with(data,lm(Toppreds~Latitude*Lake+Stream+Marine+Latitude:Stream ,na.action=na.fail))
  H_latdirect=(with(data,lm(Herbivores~Latitude+Lake),na.action=na.fail))


  B_latdirect_full_num=(with(data,lm(Basal*Species~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  I_latdirect_full_num=(with(data,lm(Intermediate*Species~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  T_latdirect_full_num=(with(data,lm(Toppreds*Species~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  H_latdirect_full_num=(with(data,lm(Herbivores*Species~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))

  bldn=dredge(B_latdirect_full_num,rank=AIC)
  ildn=dredge(I_latdirect_full_num,rank=AIC)
  tldn=dredge(T_latdirect_full_num,rank=AIC)
  hldn=dredge(H_latdirect_full_num,rank=AIC)

  B_latdirect_num=(with(data,lm(Basal~Latitude*Stream),na.action=na.fail))
  I_latdirect_num=(with(data,lm(Intermediate~Latitude*Stream+Lake+Marine+Terr),na.action=na.fail))
  T_latdirect_num=(with(data,lm(Toppreds~Lake+Marine ,na.action=na.fail))
  H_latdirect_num=(with(data,lm(Herbivores~Latitude*Stream+Lake+Marine),na.action=na.fail))

  #Both number and proportion look roughly equally interesting....

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

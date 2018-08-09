##########################################################################################
##########################################################################################
#
#           LS models
#
##########################################################################################
##########################################################################################

    LS_B_full=with(data,lm(log(LS)~log(Basal)
    +log(Basal):Latitude
    +log(Basal):(Stream+Lakeweb+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    lbdredge=dredge(LS_B_full,rank=AIC)
    # Same in both infiles :)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # Updated 22/07/2018
      LS_B_min=with(data,lm(log(LS)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Stream+Terr)
        +log(Basal):Latitude:Terr
        , na.action=na.fail   ))
      } else {       ### Probably wrong
      # TS - updated 29/06/2015
      LS_B_min=with(data,lm(log(LS)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Marine+Stream+Terr)
        +log(Basal):Latitude:(Marine+Terr)
        , na.action=na.fail   ))
      }

    obs_LS_B=with(data,lm(log(LS)~log(Basal), na.action=na.fail   ))

    LS_I_full=with(data,lm(log(LS)~log(Intermediate)
    +log(Intermediate):Latitude
    +log(Intermediate):(Stream+Lakeweb+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    lidredge=dredge(LS_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # Updated 22/07/2018
      # The only major change so far.
      LS_I_min=with(data,lm(log(LS)~log(Intermediate)
        +log(Intermediate):(Stream*Latitude), na.action=na.fail   ))
      } else {  #updated 29/06/2015
      LS_I_min=with(data,lm(log(LS)~log(Intermediate)
        +log(Intermediate):Latitude
        +log(Intermediate):Stream
        +log(Intermediate):Latitude:Stream
        , na.action=na.fail   ))
      }

    obs_LS_I=with(data,lm(log(LS)~log(Intermediate), na.action=na.fail   ))

    LS_T_full=with(data,lm(log(LS)~log(Toppreds)
    +log(Toppreds):Latitude
    +log(Toppreds):(Stream+Lakeweb+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # Updated 22/07/2018
      LS_T_min=with(data,lm(log(LS)~log(Toppreds)
        +log(Toppreds):(Lakeweb+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:(Stream+Terr)
      , na.action=na.fail   ))
      } else {  #updated 29/06/2015
      LS_T_min=with(data,lm(log(LS)~log(Toppreds)
        +log(Toppreds):(Lakeweb+Marine+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:(Stream+Terr)
        , na.action=na.fail   ))
      }

    obs_LS_T=with(data,lm(log(LS)~log(Toppreds), na.action=na.fail   ))


##########################################################################################
##########################################################################################
#
#           Gen models
#
##########################################################################################
#########################################################################################
    Gen_B_full=with(data,lm(log(Gen)~log(Basal)
    +log(Basal):Latitude
    +log(Basal):(Stream+Lakeweb+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    gbdredge=dredge(Gen_B_full,rank=AIC)
    # Same for both infiles :)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # updated 22/07/2018
      Gen_B_min=with(data,lm(log(Gen)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Stream+Terr)
        +log(Basal):Latitude:Terr
        , na.action=na.fail   ))
    } else {  #updated 29/06/2015
      Gen_B_min=with(data,lm(log(Gen)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Marine+Stream+Terr)
        +log(Basal):Latitude:(Marine+Terr)
        , na.action=na.fail   ))
    }

    obs_Gen_B=with(data,lm(log(Gen)~log(Basal), na.action=na.fail   ))

    Gen_I_full=with(data,lm(log(Gen)~log(Intermediate)
    +log(Intermediate):Latitude
    +log(Intermediate):(Stream+Lakeweb+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    gidredge=dredge(Gen_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # updated 22/07/2018
      Gen_I_min=with(data,lm(log(Gen)~log(Intermediate)
        +log(Intermediate):Latitude
        +log(Intermediate):(Lakeweb+Stream)
        +log(Intermediate):Latitude:(Lakeweb+Stream) , na.action=na.fail   ))
    } else {  # TS - updated 29/06/2015
      Gen_I_min=with(data,lm(log(Gen)~log(Intermediate)
        +log(Intermediate):Stream
        +log(Intermediate):Latitude
        +log(Intermediate):Stream:Latitude
        , na.action=na.fail   ))
    }

    obs_Gen_I=with(data,lm(log(Gen)~log(Intermediate), na.action=na.fail   ))

    Gen_T_full=with(data,lm(log(Gen)~log(Toppreds)
    +log(Toppreds):Latitude
    +log(Toppreds):(Stream+Lakeweb+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    gtdredge=dredge(Gen_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # updated 22/07/2018
      Gen_T_min=with(data,lm(log(Gen)~log(Toppreds)
        +log(Toppreds):(Lakeweb+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:Terr
        , na.action=na.fail   ))
    } else {  # TS - updated 29/06/2015
      Gen_T_min=with(data,lm(log(Gen)~log(Toppreds)
        +log(Toppreds):(Lakeweb+Marine+Stream+Terr)
        , na.action=na.fail   ))
    }

    obs_Gen_T=with(data,lm(log(Gen)~log(Toppreds), na.action=na.fail   ))

##########################################################################################
##########################################################################################
#
#           Vul models
#
##########################################################################################
#########################################################################################
    Vul_B_full=with(data,lm(log(Vul)~log(Basal)
    +log(Basal):Latitude
    +log(Basal):(Stream+Lakeweb+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    vbdredge=dredge(Vul_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # updated 22/07/2018
      Vul_B_min=with(data,lm(log(Vul)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Stream+Terr)
        +log(Basal):Latitude:Terr
        , na.action=na.fail   ))
    } else {  # TS - updated 29/06/2015
      Vul_B_min=with(data,lm(log(Vul)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Marine+Stream+Terr)
        +log(Basal):Latitude:(Marine+Terr)
        , na.action=na.fail   ))
    }

    obs_Vul_B=with(data,lm(log(Vul)~log(Basal), na.action=na.fail   ))

    Vul_I_full=with(data,lm(log(Vul)~log(Intermediate)
    +log(Intermediate):Latitude
    +log(Intermediate):(Stream+Lakeweb+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    vidredge=dredge(Vul_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # updated 29/06/2015
      Vul_I_min=with(data,lm(log(Vul)~log(Intermediate)
        +log(Intermediate):Stream
        , na.action=na.fail   ))
    } else {   # TS - updated 29/06/2015
      Vul_I_min=with(data,lm(log(Vul)~log(Intermediate)
        +log(Intermediate):Stream
        +log(Intermediate):Latitude
        +log(Intermediate):Stream:Latitude
        , na.action=na.fail   ))
    }

    obs_Vul_I=with(data,lm(log(Vul)~log(Intermediate), na.action=na.fail   ))

    Vul_T_full=with(data,lm(log(Vul)~log(Toppreds)
    +log(Toppreds):Latitude
    +log(Toppreds):(Stream+Lakeweb+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
    vtdredge=dredge(Vul_T_full,rank=AIC)
    # Same for both infiles :)
    if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
      # updated 29/06/2015
      Vul_T_min=with(data,lm(log(Vul)~log(Toppreds)
        +log(Toppreds):(Lakeweb+Marine+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:(Stream+Terr)
        ,na.action=na.fail))
      } else {   # TS - updated 29/06/2015
      Vul_T_min=with(data,lm(log(Vul)~log(Toppreds)
        +log(Toppreds):(Lakeweb+Marine+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:(Stream+Terr)
        ,na.action=na.fail))
      }

    obs_Vul_T=with(data,lm(log(Vul)~log(Toppreds), na.action=na.fail   ))

##########################################################################################
##########################################################################################
#
#           Correlations with Latitude
#
##########################################################################################
#########################################################################################

  B_latdirect_full=(with(data,lm(Basal~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  I_latdirect_full=(with(data,lm(Intermediate~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  T_latdirect_full=(with(data,lm(Toppreds~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))

  bld=dredge(B_latdirect_full,rank=AIC)
  ild=dredge(I_latdirect_full,rank=AIC)
  tld=dredge(T_latdirect_full,rank=AIC)

  if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
    # non-TS, updated 22/07/2018
    B_latdirect=(with(data,lm(Basal~Latitude*Stream+Lakeweb),na.action=na.fail))
    } else { # TS, updated 29/06/2015
    B_latdirect=(with(data,lm(Basal~Latitude*Stream+Lakeweb+Marine),na.action=na.fail))
    }

  if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
    # non-TS, updated 22/07/2018
    I_latdirect=(with(data,lm(Intermediate~Latitude*Stream+Lakeweb),na.action=na.fail))
    } else { # TS, updated 29/06/2015
    I_latdirect=(with(data,lm(Intermediate~Latitude*Lakeweb+Stream),na.action=na.fail))
    }

  if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
    # non-TS, updated 22/07/2018
    T_latdirect=(with(data,lm(Toppreds~Lakeweb+Marine),na.action=na.fail))
    } else { # TS, updated 29/06/2015
    T_latdirect=(with(data,lm(Toppreds~Latitude*Lakeweb+Stream+Latitude:Stream),na.action=na.fail))
    }


###############################################################################################
###############################################################################################
###############################################################################################
## ##        
## ##         Writing out a pile of files
## ##
###############################################################################################
###############################################################################################
###############################################################################################

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

  if(infile=='../non_TS/summary-properties_corrected_webs.tsv'){
    source('marginal_CIs_nonTS_errorwebs.R')
# Haven't updated these yet.
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

  }



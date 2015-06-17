##########################################################################################
##########################################################################################
#
#           LS models
#
##########################################################################################
##########################################################################################
    LS_B_full=with(data,lm(log(LS)~log(Basal*Species)
    +log(Basal*Species):Latitude
    +log(Basal*Species):(Stream+Lake+Marine+Terr)
    +log(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lbdredge=dredge(LS_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_B_min=with(data,lm(log(LS)~log(Basal*Species))) 
    }

    obs_LS_B=with(data,lm(log(LS)~log(Basal*Species), na.action=na.fail   ))

    LS_I_full=with(data,lm(log(LS)~log(Intermediate*Species)
    +log(Intermediate*Species):Latitude
    +log(Intermediate*Species):(Stream+Lake+Marine+Terr)
    +log(Intermediate*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lidredge=dredge(LS_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_I_min=with(data,lm(log(LS)~log(Intermediate*Species)
        +log(Intermediate*Species):(Lake+Stream)
        +log(Intermediate*Species):Latitude
        +log(Intermediate*Species):Latitude:(Lake+Stream) )) 
    } 

    obs_LS_I=with(data,lm(log(LS)~log(Intermediate*Species), na.action=na.fail   ))

    LS_T_full=with(data,lm(log(LS)~log(Toppreds*Species)
    +log(Toppreds*Species):Latitude
    +log(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
        LS_T_min=with(data,lm(log(LS)~log(Toppreds*Species)
        +log(Toppreds*Species):(Marine+Terr)
        ,na.action=na.fail))
    } 

    obs_LS_T=with(data,lm(log(LS)~log(Toppreds*Species), na.action=na.fail   ))

##########################################################################################
##########################################################################################
#
#           Gen models
#
##########################################################################################
#########################################################################################
    Gen_B_full=with(data,lm(log(Gen)~log(Basal*Species)
    +log(Basal*Species):Latitude
    +log(Basal*Species):(Stream+Lake+Marine+Terr)
    +log(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gbdredge=dredge(Gen_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Gen_B_min=with(data,lm(log(Gen)~log(Basal*Species) ))
    }

    obs_Gen_B=with(data,lm(log(Gen)~log(Basal*Species), na.action=na.fail   ))

    Gen_I_full=with(data,lm(log(Gen)~log(Intermediate*Species)
    +log(Intermediate*Species):Latitude
    +log(Intermediate*Species):(Stream+Lake+Marine+Terr)
    +log(Intermediate*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gidredge=dredge(Gen_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Gen_I_min=with(data,lm(log(Gen)~log(Intermediate*Species)
        +log(Intermediate*Species):Latitude
        +log(Intermediate*Species):(Lake+Marine+Stream+Terr)
        +log(Intermediate*Species):Latitude:(Lake+Stream), na.action=na.fail   ))
    }

    obs_Gen_I=with(data,lm(log(Gen)~log(Intermediate*Species), na.action=na.fail   ))

    Gen_T_full=with(data,lm(log(Gen)~log(Toppreds*Species)
    +log(Toppreds*Species):Latitude
    +log(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gtdredge=dredge(Gen_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Gen_T_min=with(data,lm(log(Gen)~log(Toppreds*Species)
      +log(Toppreds*Species):(Marine+Stream+Terr)
      +log(Toppreds*Species):Latitude
      +log(Toppreds*Species):Latitude:Terr ))
    }

    obs_Gen_T=with(data,lm(log(Gen)~log(Toppreds*Species), na.action=na.fail   ))

##########################################################################################
##########################################################################################
#
#           Vul models
#
##########################################################################################
#########################################################################################
    Vul_B_full=with(data,lm(log(Vul)~log(Basal*Species)
    +log(Basal*Species):Latitude
    +log(Basal*Species):(Stream+Lake+Marine+Terr)
    +log(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vbdredge=dredge(Vul_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Vul_B_min=with(data,lm(log(Vul)~log(Basal*Species), na.action=na.fail   ))
    }

    obs_Vul_B=with(data,lm(log(Vul)~log(Basal*Species), na.action=na.fail   ))

    Vul_I_full=with(data,lm(log(Vul)~log(Intermediate*Species)
    +log(Intermediate*Species):Latitude
    +log(Intermediate*Species):(Stream+Lake+Marine+Terr)
    +log(Intermediate*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vidredge=dredge(Vul_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Vul_I_min=with(data,lm(log(Vul)~log(Intermediate*Species)
        +log(Intermediate*Species):Latitude
        +log(Intermediate*Species):(Lake+Stream)
        +log(Intermediate*Species):Latitude:(Lake+Stream)
        , na.action=na.fail   ))
    }

    obs_Vul_I=with(data,lm(log(Vul)~log(Intermediate*Species), na.action=na.fail   ))

    Vul_T_full=with(data,lm(log(Vul)~log(Toppreds*Species)
    +log(Toppreds*Species):Latitude
    +log(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vtdredge=dredge(Vul_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Vul_T_min=with(data,lm(log(Vul)~log(Toppreds*Species)
      +log(Toppreds*Species):(Marine+Terr)))
    }

    obs_Vul_T=with(data,lm(log(Vul)~log(Toppreds*Species), na.action=na.fail   ))

##########################################################################################
##########################################################################################
#
#           Correlations with Latitude
#
##########################################################################################
#########################################################################################

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
  T_latdirect_num=(with(data,lm(Toppreds~Lake+Marine),na.action=na.fail))
  H_latdirect_num=(with(data,lm(Herbivores~Latitude*Stream+Lake+Marine),na.action=na.fail))


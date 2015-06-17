##########################################################################################
##########################################################################################
#
#           LS models
#
##########################################################################################
##########################################################################################

    LS_B_full=with(data,lm(log(LS)~log(Basal)
    +log(Basal):Latitude
    +log(Basal):(Stream+Lake+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lbdredge=dredge(LS_B_full,rank=AIC)
    # Same in both infiles :)
    LS_B_min=with(data,lm(log(LS)~log(Basal)
      +log(Basal):Latitude
      +log(Basal):(Marine+Stream+Terr)
      +log(Basal):Latitude:(Marine+Terr) , na.action=na.fail   ))

    obs_LS_B=with(data,lm(log(LS)~log(Basal), na.action=na.fail   ))

    LS_I_full=with(data,lm(log(LS)~log(Intermediate)
    +log(Intermediate):Latitude
    +log(Intermediate):(Stream+Lake+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lidredge=dredge(LS_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_I_min=with(data,lm(log(LS)~log(Intermediate)
        +log(Intermediate):(Lake+Stream+Terr)
        +log(Intermediate):Latitude
        +log(Intermediate):Latitude:Terr, na.action=na.fail   ))
    } else {          ############# THIS IS PROBABLY WRONG!
      LS_I_min=with(data,lm(log(LS)~log(Intermediate)
        +log(Intermediate):Stream, na.action=na.fail   ))
    }

    obs_LS_I=with(data,lm(log(LS)~log(Intermediate), na.action=na.fail   ))

    LS_T_full=with(data,lm(log(LS)~log(Toppreds)
    +log(Toppreds):Latitude
    +log(Toppreds):(Stream+Lake+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)
    # Same on both input files :)
    LS_T_min=with(data,lm(log(LS)~log(Toppreds)
    +log(Toppreds):(Lake+Marine+Stream+Terr)
    +log(Toppreds):Latitude
    +log(Toppreds):Latitude:(Stream+Terr) ))

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
    +log(Basal):(Stream+Lake+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gbdredge=dredge(Gen_B_full,rank=AIC)
    # Same for both infiles :)
    Gen_B_min=with(data,lm(log(Gen)~log(Basal)
      +log(Basal):Latitude
      +log(Basal):(Marine+Stream+Terr)
      +log(Basal):Latitude:(Marine+Terr) , na.action=na.fail   ))

    obs_Gen_B=with(data,lm(log(Gen)~log(Basal), na.action=na.fail   ))

    Gen_I_full=with(data,lm(log(Gen)~log(Intermediate)
    +log(Intermediate):Latitude
    +log(Intermediate):(Stream+Lake+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gidredge=dredge(Gen_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Gen_I_min=with(data,lm(log(Gen)~log(Intermediate)
        +log(Intermediate):Latitude
        +log(Intermediate):(Lake+Stream+Terr)
        +log(Intermediate):Latitude:Terr , na.action=na.fail   ))
    } else {    ########## MAY BE INCORRECT
      Gen_I_min=with(data,lm(log(Gen)~log(Intermediate)
        +log(Intermediate):(Marine+Stream), na.action=na.fail   ))
    }

    obs_Gen_I=with(data,lm(log(Gen)~log(Intermediate), na.action=na.fail   ))

    Gen_T_full=with(data,lm(log(Gen)~log(Toppreds)
    +log(Toppreds):Latitude
    +log(Toppreds):(Stream+Lake+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gtdredge=dredge(Gen_T_full,rank=AIC)
    # Same in both input files :)
    Gen_T_min=with(data,lm(log(Gen)~log(Toppreds)
      +log(Toppreds):(Marine+Stream) ))

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
    +log(Basal):(Stream+Lake+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vbdredge=dredge(Vul_B_full,rank=AIC)
    # Same for both infiles :)
    Vul_B_min=with(data,lm(log(Vul)~log(Basal)
      +log(Basal):Latitude
      +log(Basal):(Marine+Stream+Terr)
      +log(Basal):Latitude:(Marine+Terr) , na.action=na.fail   ))

    obs_Vul_B=with(data,lm(log(Vul)~log(Basal), na.action=na.fail   ))

    Vul_I_full=with(data,lm(log(Vul)~log(Intermediate)
    +log(Intermediate):Latitude
    +log(Intermediate):(Stream+Lake+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vidredge=dredge(Vul_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      Vul_I_min=with(data,lm(log(Vul)~log(Intermediate)
        +log(Intermediate):Latitude
        +log(Intermediate):(Lake+Stream+Terr)
        +log(Intermediate):Latitude:Terr, na.action=na.fail   ))
    } else {
      Vul_I_min=with(data,lm(log(Vul)~log(Intermediate)
        +log(Intermediate):Stream, na.action=na.fail   ))
    }

    obs_Vul_I=with(data,lm(log(Vul)~log(Intermediate), na.action=na.fail   ))

    Vul_T_full=with(data,lm(log(Vul)~log(Toppreds)
    +log(Toppreds):Latitude
    +log(Toppreds):(Stream+Lake+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vtdredge=dredge(Vul_T_full,rank=AIC)
    # Same for both infiles :)
    Vul_T_min=with(data,lm(log(Vul)~log(Toppreds)
      +log(Toppreds):(Lake+Marine+Stream+Terr)
      +log(Toppreds):Latitude
      +log(Toppreds):Latitude:(Stream+Terr) ))

    obs_Vul_T=with(data,lm(log(Vul)~log(Toppreds), na.action=na.fail   ))

##########################################################################################
##########################################################################################
#
#           Correlations with Latitude
#
##########################################################################################
#########################################################################################

  B_latdirect_full=(with(data,lm(Basal~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  I_latdirect_full=(with(data,lm(Intermediate~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  T_latdirect_full=(with(data,lm(Toppreds~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  H_latdirect_full=(with(data,lm(Herbivores~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))

  bld=dredge(B_latdirect_full,rank=AIC)
  ild=dredge(I_latdirect_full,rank=AIC)
  tld=dredge(T_latdirect_full,rank=AIC)
  hld=dredge(H_latdirect_full,rank=AIC)

  B_latdirect=(with(data,lm(Basal~Latitude*Stream+Lake),na.action=na.fail))

  if(infile=='../non_TS/summary-properties.tsv'){
    I_latdirect=(with(data,lm(Intermediate~1),na.action=na.fail))
    } else {
    I_latdirect=(with(data,lm(Intermediate~Latitude*Lake+Stream),na.action=na.fail))
    }

  if(infile=='../non_TS/summary-properties.tsv'){
    T_latdirect=(with(data,lm(Toppreds~Latitude*Lake+Stream+Marine+Latitude:Stream),na.action=na.fail))
    } else {
    T_latdirect=(with(data,lm(Toppreds~Latitude*Lake+Stream+Latitude:Stream),na.action=na.fail))
    }

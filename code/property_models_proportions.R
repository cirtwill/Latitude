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
    if(infile=='../non_TS/summary-properties.tsv'){
      # non-TS - updated 29/06/2015
      LS_B_min=with(data,lm(log(LS)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Marine+Stream+Terr)
        +log(Basal):Latitude:(Marine+Terr)
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
    +log(Intermediate):(Stream+Lake+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lidredge=dredge(LS_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      # TS, updated 29/06/2015
      # The only major change so far.
      LS_I_min=with(data,lm(log(LS)~log(Intermediate)
        +log(Intermediate):Stream, na.action=na.fail   ))
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
    +log(Toppreds):(Stream+Lake+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      # Updated 29/06/2015
      LS_T_min=with(data,lm(log(LS)~log(Toppreds)
        +log(Toppreds):(Lake+Marine+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:(Stream+Terr)
      , na.action=na.fail   ))
      } else {  #updated 29/06/2015
      LS_T_min=with(data,lm(log(LS)~log(Toppreds)
        +log(Toppreds):(Lake+Marine+Stream+Terr)
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
    +log(Basal):(Stream+Lake+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gbdredge=dredge(Gen_B_full,rank=AIC)
    # Same for both infiles :)
    if(infile=='../non_TS/summary-properties.tsv'){
      # updated 29/06/2015
      Gen_B_min=with(data,lm(log(Gen)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Marine+Stream+Terr)
        +log(Basal):Latitude:(Marine+Terr)
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
    +log(Intermediate):(Stream+Lake+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gidredge=dredge(Gen_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      # updated 29/06/2015
      Gen_I_min=with(data,lm(log(Gen)~log(Intermediate)
        +log(Intermediate):Latitude
        +log(Intermediate):(Lake+Stream)
        +log(Intermediate):Latitude:(Lake+Stream) , na.action=na.fail   ))
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
    +log(Toppreds):(Stream+Lake+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gtdredge=dredge(Gen_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      # updated 29/06/2015
      Gen_T_min=with(data,lm(log(Gen)~log(Toppreds)
        +log(Toppreds):(Lake+Marine+Stream)
        , na.action=na.fail   ))
    } else {  # TS - updated 29/06/2015
      Gen_T_min=with(data,lm(log(Gen)~log(Toppreds)
        +log(Toppreds):(Lake+Marine+Stream+Terr)
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
    +log(Basal):(Stream+Lake+Marine+Terr)
    +log(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vbdredge=dredge(Vul_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      # updated 29/06/2015, same in both infiles
      Vul_B_min=with(data,lm(log(Vul)~log(Basal)
        +log(Basal):Latitude
        +log(Basal):(Marine+Stream+Terr)
        +log(Basal):Latitude:(Marine+Terr)
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
    +log(Intermediate):(Stream+Lake+Marine+Terr)
    +log(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vidredge=dredge(Vul_I_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
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
    +log(Toppreds):(Stream+Lake+Marine+Terr)
    +log(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vtdredge=dredge(Vul_T_full,rank=AIC)
    # Same for both infiles :)
    if(infile=='../non_TS/summary-properties.tsv'){
      # updated 29/06/2015
      Vul_T_min=with(data,lm(log(Vul)~log(Toppreds)
        +log(Toppreds):(Lake+Marine+Stream+Terr)
        +log(Toppreds):Latitude
        +log(Toppreds):Latitude:(Stream+Terr)
        ,na.action=na.fail))
      } else {   # TS - updated 29/06/2015
      Vul_T_min=with(data,lm(log(Vul)~log(Toppreds)
        +log(Toppreds):(Lake+Marine+Stream+Terr)
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

  B_latdirect_full=(with(data,lm(Basal~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  I_latdirect_full=(with(data,lm(Intermediate~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  T_latdirect_full=(with(data,lm(Toppreds~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))

  bld=dredge(B_latdirect_full,rank=AIC)
  ild=dredge(I_latdirect_full,rank=AIC)
  tld=dredge(T_latdirect_full,rank=AIC)

  if(infile=='../non_TS/summary-properties.tsv'){
    # non-TS, updated 29/06/2015
    B_latdirect=(with(data,lm(Basal~Latitude*Stream+Lake+Marine),na.action=na.fail))
    } else { # TS, updated 29/06/2015
    B_latdirect=(with(data,lm(Basal~Latitude*Stream+Lake+Marine),na.action=na.fail))
    }

  if(infile=='../non_TS/summary-properties.tsv'){
    # non-TS, updated 29/06/2015
    I_latdirect=(with(data,lm(Intermediate~Latitude*Lake+Stream),na.action=na.fail))
    } else { # TS, updated 29/06/2015
    I_latdirect=(with(data,lm(Intermediate~Latitude*Lake+Stream),na.action=na.fail))
    }

  if(infile=='../non_TS/summary-properties.tsv'){
    # non-TS, updated 29/06/2015
    T_latdirect=(with(data,lm(Toppreds~Latitude*Lake+Stream+Latitude:Stream),na.action=na.fail))
    } else { # TS, updated 29/06/2015
    T_latdirect=(with(data,lm(Toppreds~Latitude*Lake+Stream+Latitude:Stream),na.action=na.fail))
    }

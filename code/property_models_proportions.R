##########################################################################################
##########################################################################################
#
#           LS models
#
##########################################################################################
##########################################################################################

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

##########################################################################################
##########################################################################################
#
#           Gen models
#
##########################################################################################
#########################################################################################
    Gen_B_full=with(data,lm(log10(Gen)~log10(Basal)
    +log10(Basal):Latitude
    +log10(Basal):(Stream+Lake+Marine+Terr)
    +log10(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gbdredge=dredge(Gen_B_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Gen)~log10(Basal)
    #     +log10(Basal):Latitude
    #     +log10(Basal):(Marine+Stream+Terr)
    #     +log10(Basal):Latitude:(Marine+Terr) , na.action=na.fail   ))
    # }

    Gen_I_full=with(data,lm(log10(Gen)~log10(Intermediate)
    +log10(Intermediate):Latitude
    +log10(Intermediate):(Stream+Lake+Marine+Terr)
    +log10(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gidredge=dredge(Gen_I_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Gen)~log10(Intermediate)
    #     +log10(Intermediate):(Lake+Stream), na.action=na.fail   ))
    # }

    Gen_T_full=with(data,lm(log10(Gen)~log10(Toppreds)
    +log10(Toppreds):Latitude
    +log10(Toppreds):(Stream+Lake+Marine+Terr)
    +log10(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gtdredge=dredge(Gen_T_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_T_min=with(data,lm(log10(Gen)~log10(Toppreds)
    #   +log10(Toppreds):(Lake+Marine+Stream+Terr)
    #   +log10(Toppreds):Latitude
    #   +log10(Toppreds):Latitude:(Stream+Terr) ))
    # }


##########################################################################################
##########################################################################################
#
#           Vul models
#
##########################################################################################
#########################################################################################
    Vul_B_full=with(data,lm(log10(Vul)~log10(Basal)
    +log10(Basal):Latitude
    +log10(Basal):(Stream+Lake+Marine+Terr)
    +log10(Basal):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vbdredge=dredge(Vul_B_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Vul)~log10(Basal)
    #     +log10(Basal):Latitude
    #     +log10(Basal):(Marine+Stream+Terr)
    #     +log10(Basal):Latitude:(Marine+Terr) , na.action=na.fail   ))
    # }

    Vul_I_full=with(data,lm(log10(Vul)~log10(Intermediate)
    +log10(Intermediate):Latitude
    +log10(Intermediate):(Stream+Lake+Marine+Terr)
    +log10(Intermediate):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vidredge=dredge(Vul_I_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Vul)~log10(Intermediate)
    #     +log10(Intermediate):(Lake+Stream), na.action=na.fail   ))
    # }

    Vul_T_full=with(data,lm(log10(Vul)~log10(Toppreds)
    +log10(Toppreds):Latitude
    +log10(Toppreds):(Stream+Lake+Marine+Terr)
    +log10(Toppreds):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vtdredge=dredge(Vul_T_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_T_min=with(data,lm(log10(Vul)~log10(Toppreds)
    #   +log10(Toppreds):(Lake+Marine+Stream+Terr)
    #   +log10(Toppreds):Latitude
    #   +log10(Toppreds):Latitude:(Stream+Terr) ))
    # }



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




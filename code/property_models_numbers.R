##########################################################################################
##########################################################################################
#
#           LS models
#
##########################################################################################
##########################################################################################
    LS_B_full=with(data,lm(log10(LS)~log10(Basal*Species)
    +log10(Basal*Species):Latitude
    +log10(Basal*Species):(Stream+Lake+Marine+Terr)
    +log10(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    lbdredge=dredge(LS_B_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
      LS_B_min=with(data,lm(log10(LS)~log10(Basal*Species)+log10(Basal*Species):Marine)) 
    }

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
    } 

    LS_T_full=with(data,lm(log10(LS)~log10(Toppreds*Species)
    +log10(Toppreds*Species):Latitude
    +log10(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log10(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    ltdredge=dredge(LS_T_full,rank=AIC)
    if(infile=='../non_TS/summary-properties.tsv'){
        LS_T_min=with(data,lm(log10(LS)~log10(Toppreds*Species)
        +log10(Toppreds*Species):Latitude
        +log10(Toppreds*Species):(Lake+Marine+Terr)
        +log10(Toppreds*Species):Latitude:(Lake)
        ,na.action=na.fail))
    } 

##########################################################################################
##########################################################################################
#
#           Gen models
#
##########################################################################################
#########################################################################################
    Gen_B_full=with(data,lm(log10(Gen)~log10(Basal*Species)
    +log10(Basal*Species):Latitude
    +log10(Basal*Species):(Stream+Lake+Marine+Terr)
    +log10(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gbdredge=dredge(Gen_B_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Gen)~log10(Basal*Species)
    #     +log10(Basal*Species):Latitude
    #     +log10(Basal*Species):(Marine+Stream+Terr)
    #     +log10(Basal*Species):Latitude:(Marine+Terr) , na.action=na.fail   ))
    # }

    Gen_I_full=with(data,lm(log10(Gen)~log10(Intermediate*Species)
    +log10(Intermediate*Species):Latitude
    +log10(Intermediate*Species):(Stream+Lake+Marine+Terr)
    +log10(Intermediate*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gidredge=dredge(Gen_I_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Gen)~log10(Intermediate*Species)
    #     +log10(Intermediate*Species):(Lake+Stream), na.action=na.fail   ))
    # }

    Gen_T_full=with(data,lm(log10(Gen)~log10(Toppreds*Species)
    +log10(Toppreds*Species):Latitude
    +log10(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log10(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    gtdredge=dredge(Gen_T_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_T_min=with(data,lm(log10(Gen)~log10(Toppreds*Species)
    #   +log10(Toppreds*Species):(Lake+Marine+Stream+Terr)
    #   +log10(Toppreds*Species):Latitude
    #   +log10(Toppreds*Species):Latitude:(Stream+Terr) ))
    # }


##########################################################################################
##########################################################################################
#
#           Vul models
#
##########################################################################################
#########################################################################################
    Vul_B_full=with(data,lm(log10(Vul)~log10(Basal*Species)
    +log10(Basal*Species):Latitude
    +log10(Basal*Species):(Stream+Lake+Marine+Terr)
    +log10(Basal*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vbdredge=dredge(Vul_B_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Vul)~log10(Basal*Species)
    #     +log10(Basal*Species):Latitude
    #     +log10(Basal*Species):(Marine+Stream+Terr)
    #     +log10(Basal*Species):Latitude:(Marine+Terr) , na.action=na.fail   ))
    # }

    Vul_I_full=with(data,lm(log10(Vul)~log10(Intermediate*Species)
    +log10(Intermediate*Species):Latitude
    +log10(Intermediate*Species):(Stream+Lake+Marine+Terr)
    +log10(Intermediate*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vidredge=dredge(Vul_I_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_B_min=with(data,lm(log10(Vul)~log10(Intermediate*Species)
    #     +log10(Intermediate*Species):(Lake+Stream), na.action=na.fail   ))
    # }

    Vul_T_full=with(data,lm(log10(Vul)~log10(Toppreds*Species)
    +log10(Toppreds*Species):Latitude
    +log10(Toppreds*Species):(Stream+Lake+Marine+Terr)
    +log10(Toppreds*Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
    vtdredge=dredge(Vul_T_full,rank=AIC)
    # if(infile=='../non_TS/summary-properties.tsv'){
    #   LS_T_min=with(data,lm(log10(Vul)~log10(Toppreds*Species)
    #   +log10(Toppreds*Species):(Lake+Marine+Stream+Terr)
    #   +log10(Toppreds*Species):Latitude
    #   +log10(Toppreds*Species):Latitude:(Stream+Terr) ))
    # }




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


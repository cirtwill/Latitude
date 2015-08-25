  #Let's look at the same old regressions with the subset of data...
  # For proportions, regressions are similar but vary slightly over ecotype.
  LS_full=with(data,lm(log(LS)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lake+Marine+Terr)
    +log(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  ls_dredge=dredge(LS_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    # TS - updated 29/06/2015
    LS_min =with(data,lm(log(LS)~log(Species)
    +log(Species):Lake
    +log(Species):Latitude
    +log(Species):Latitude:Lake
    ,na.action=na.fail))    } else {
      # non-TS - updated 29/06/2015
    LS_min=with(data,lm(log(LS)~log(Species)
    +log(Species):(Lake+Terr)
    +log(Species):Latitude
    +log(Species):Latitude:Lake,
    na.action=na.fail))    }
    # Decided on DF

    # cutoff<- 4/((nrow(data)-length(LS_min$coefficients)-2)) 
    # plot(LS_min, which=4, cook.levels=cutoff)
    if(infile!='../mod_data/summary-properties.tsv'){
      # The only change is that the different intercept for terrestrial is not sig.
      subset=data[-c(60,136,163),]
      subLS_min=(with(subset,lm(log(LS)~log(Species)
      +log(Species):(Lake+Terr)
      +log(Species):Latitude
      +log(Species):Latitude:Lake,na.action=na.fail)))
    }


  obs_LS=(with(data,lm(log(LS)~log(Species),na.action=na.fail)))

  Gen_full=with(data,lm(log(Gen)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lake+Marine+Terr)
    +log(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  g_dredge=dredge(Gen_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    # TS - updated 29/06/2015
    Gen_min=with(data,lm(log(Gen)~log(Species)
      +log(Species):(Lake+Stream)
      +log(Species):Latitude
      +log(Species):Latitude:(Lake+Stream)
      ,na.action=na.fail)) } else {
      # non-TS - updated 29/06/2015
    Gen_min=with(data,lm(log(Gen)~log(Species)
      +log(Species):(Lake+Stream)
      +log(Species):Latitude
      +log(Species):Latitude:(Lake+Stream)
      ,na.action=na.fail))
    }
    # Decided on DF

    # cutoff<- 4/((nrow(data)-length(Gen_min$coefficients)-2)) 
    # plot(Gen_min, which=4, cook.levels=cutoff)
    if(infile!='../mod_data/summary-properties.tsv'){
      # The only change is that the different intercept for stream is not sig. Int still is.
      subset=data[-c(60,79,136),]
      subGen_min=with(subset,lm(log(Gen)~log(Species)
      +log(Species):(Lake+Stream)
      +log(Species):Latitude
      +log(Species):Latitude:(Lake+Stream)
      ,na.action=na.fail))
    }

  obs_Gen=(with(data,lm(log(Gen)~log(Species),na.action=na.fail)))

  Vul_full=with(data,lm(log(Vul)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lake+Marine+Terr)
    +log(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  v_dredge=dredge(Vul_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    # TS - updated 29/06/2015
    Vul_min=with(data,lm(log(Vul)~log(Species)
      +log(Species):Lake
      +log(Species):Latitude
      +log(Species):Latitude:Lake
      ,na.action=na.fail)) } else {
      # non-TS - updated 29/06/2015
    Vul_min=with(data,lm(log(Vul)~log(Species)
      +log(Species):(Lake+Terr)
      +log(Species):Latitude
      +log(Species):Latitude:Lake
      ,na.action=na.fail))  }
    # Decided based on Df

    # cutoff<- 4/((nrow(data)-length(Vul_min$coefficients)-2)) 
    # plot(Vul_min, which=4, cook.levels=cutoff)
    if(infile!='../mod_data/summary-properties.tsv'){
      # Just like LS
      subset=data[-c(60,136,163),]
      subVul_min=with(subset,lm(log(Vul)~log(Species)
      +log(Species):(Lake+Terr)
      +log(Species):Latitude
      +log(Species):Latitude:Lake
      ,na.action=na.fail))
    }

  obs_Vul=(with(data,lm(log(Vul)~log(Species),na.action=na.fail)))

      # What's the basic correlation with latitude?
  Sp_latdirect_full=(with(data,lm(Species~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  LS_latdirect_full=(with(data,lm(LS~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  Gen_latdirect_full=(with(data,lm(Gen~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))
  Vul_latdirect_full=(with(data,lm(Vul~Latitude*(Stream+Lake+Marine+Terr),na.action=na.fail)))

  sld=dredge(Sp_latdirect_full,rank=AIC)
  lld=dredge(LS_latdirect_full,rank=AIC)
  gld=dredge(Gen_latdirect_full,rank=AIC)
  vld=dredge(Vul_latdirect_full,rank=AIC)

  if(infile=='../mod_data/summary-properties.tsv'){
    # TS - updated 29/06/2015
    Sp_latdirect=(with(data,lm(Species~Lake+Marine+Terr),na.action=na.fail)) 
    LS_latdirect=(with(data,lm(LS~Stream+Marine),na.action=na.fail))
    Gen_latdirect=(with(data,lm(Gen~Stream),na.action=na.fail))
    Vul_latdirect=(with(data,lm(Vul~Stream+Marine),na.action=na.fail))
    } else {
    # non-TS - updated 29/06/2015
    Sp_latdirect=(with(data,lm(Species~Lake+Marine),na.action=na.fail)) }

    # cutoff<- 4/((nrow(data)-length(Sp_latdirect$coefficients)-2)) 
    # plot(Sp_latdirect, which=4, cook.levels=cutoff)
    # subset=data[-c(48,71,114),]
    # subSp_latdirect=(with(subset,lm(Species~Lake+Marine),na.action=na.fail)) 

    LS_latdirect=(with(data,lm(LS~Stream+Marine),na.action=na.fail))
    Gen_latdirect=(with(data,lm(Gen~Stream),na.action=na.fail))
    Vul_latdirect=(with(data,lm(Vul~Stream+Marine),na.action=na.fail))

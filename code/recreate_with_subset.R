  #Let's look at the same old regressions with the subset of data...
  # For proportions, regressions are similar but vary slightly over ecotype.
  LS_full=with(data,lm(log(LS)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lake+Marine+Terr)
    +log(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  ls_dredge=dredge(LS_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    LS_min =with(data,lm(log(LS)~log(Species)
    +log(Species):(Lake+Terr)
    +log(Species):Latitude
    +log(Species):Latitude:Lake
    ,na.action=na.fail))    } else {
    LS_min=with(data,lm(log(LS)~log(Species)
    +log(Species):(Lake+Terr)
    +log(Species):Latitude
    +log(Species):Latitude:Lake,
    na.action=na.fail))    }

    # cutoff<- 4/((nrow(data)-length(LS_min$coefficients)-2)) 
    # plot(LS_min, which=4, cook.levels=cutoff)
    # #  57, 66, 138 are outliers.
    # if(infile=='../mod_data/summary-properties.tsv'){
    #   subset=data[-c(57,127,138),]
    # } else {
    #   subset=data[-c(57,66,138),]
    # }

    # subLS_min=(with(subset,lm(log(LS)~log(Species)
    # +log(Species):(Lake+Terr)
    # +log(Species):Latitude
    # +log(Species):Latitude:Lake,na.action=na.fail)))
  obs_LS=(with(subset,lm(log(LS)~log(Species),na.action=na.fail)))

  Gen_full=with(data,lm(log(Gen)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lake+Marine+Terr)
    +log(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  g_dredge=dredge(Gen_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    Gen_min=with(data,lm(log(Gen)~log(Species)
      +log(Species):(Lake+Stream)
      +log(Species):Latitude
      +log(Species):Latitude:Lake
      +log(Species):Latitude:Stream
      ,na.action=na.fail)) } else {
    Gen_min=with(data,lm(log(Gen)~log(Species)
      +log(Species):(Lake+Stream)
      +log(Species):Latitude
      +log(Species):Latitude:(Lake+Stream)
      ,na.action=na.fail))
    }

  obs_Gen=(with(subset,lm(log(Gen)~log(Species),na.action=na.fail)))

  Vul_full=with(data,lm(log(Vul)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lake+Marine+Terr)
    +log(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  v_dredge=dredge(Vul_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    Vul_min=with(data,lm(log(Vul)~log(Species)
      +log(Species):(Lake+Terr)
      +log(Species):Latitude
      +log(Species):Latitude:Lake
      ,na.action=na.fail)) } else {
    Vul_min=with(data,lm(log(Vul)~log(Species)
      +log(Species):(Lake+Terr)
      +log(Species):Latitude
      +log(Species):Latitude:Lake
      ,na.action=na.fail))  }

  obs_Vul=(with(subset,lm(log(Vul)~log(Species),na.action=na.fail)))

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
    Sp_latdirect=(with(data,lm(Species~Stream*Latitude+Lake+Marine+Terr),na.action=na.fail)) 
    } else {
    Sp_latdirect=(with(data,lm(Species~Stream*Latitude+Lake+Marine),na.action=na.fail)) }
    cutoff<- 4/((nrow(data)-length(Sp_latdirect$coefficients)-2)) 
    plot(Sp_latdirect, which=4, cook.levels=cutoff)
    subset=data[-c(122,126,128),]

  LS_latdirect=(with(data,lm(LS~Stream*Latitude+Marine),na.action=na.fail))
    cutoff<- 4/((nrow(data)-length(LS_latdirect$coefficients)-2)) 
    plot(LS_latdirect, which=4, cook.levels=cutoff)
    subset=data[-c(110,122,127),]

  Gen_latdirect=(with(data,lm(Gen~Marine+Stream),na.action=na.fail))
  Vul_latdirect=(with(data,lm(Vul~Latitude*Stream+Marine),na.action=na.fail))
    cutoff<- 4/((nrow(data)-length(Vul_latdirect$coefficients)-2)) 
    plot(Vul_latdirect, which=4, cook.levels=cutoff)
    subset=data[-c(110,122,127),]

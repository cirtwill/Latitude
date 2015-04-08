  #Let's look at the same old regressions with the subset of data...
  # For proportions, regressions are similar but vary slightly over ecotype.
  LS_full=with(data,lm(log10(LS)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lake+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  ls_dredge=dredge(LS_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    LS_min =with(data,lm(log10(LS)~log10(Species)
    +log10(Species):(Lake+Terr)
    +log10(Species):Latitude
    +log10(Species):Latitude:Lake
    ,na.action=na.fail))    } else {
    LS_min=with(data,lm(log10(LS)~log10(Species)
    +log10(Species):(Lake+Terr)
    +log10(Species):Latitude
    +log10(Species):Latitude:Lake,
    na.action=na.fail))    }

    cutoff<- 4/((nrow(data)-length(LS_min$coefficients)-2)) 
    plot(LS_min, which=4, cook.levels=cutoff)
    #  57, 66, 138 are outliers.
    if(infile=='../mod_data/summary-properties.tsv'){
      subset=data[-c(57,127,138),]
    } else {
      subset=data[-c(57,66,138),]
    }

    subLS_min=(with(subset,lm(log10(LS)~log10(Species)
    +log10(Species):(Lake+Terr)
    +log10(Species):Latitude
    +log10(Species):Latitude:Lake,na.action=na.fail)))


  Gen_full=with(data,lm(log10(Gen)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lake+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  g_dredge=dredge(Gen_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    Gen_min=with(data,lm(log10(Gen)~log10(Species)
      +log10(Species):(Lake+Stream)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      +log10(Species):Latitude:Stream
      ,na.action=na.fail)) } else {
    Gen_min=with(data,lm(log10(Gen)~log10(Species)
      +log10(Species):(Lake+Stream+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:(Lake+Stream)
      ,na.action=na.fail))
    }

    cutoff<- 4/((nrow(data)-length(Gen_min$coefficients)-2)) 
    plot(Gen_min, which=4, cook.levels=cutoff)
    #  57, 66, 109 are outliers.
    if(infile=='../mod_data/summary-properties.tsv'){
      subset=data[-c(43,57,109),]
      subGen_min=with(subset,lm(log10(Gen)~log10(Species)
        +log10(Species):(Lake+Stream+Terr)
        +log10(Species):Latitude
        +log10(Species):Latitude:(Lake+Stream)
        ,na.action=na.fail)) } else {
      subset=data[-c(57,66,109),]
      subGen_min=with(subset,lm(log10(Gen)~log10(Species)
        +log10(Species):(Lake+Stream+Terr)
        +log10(Species):Latitude
        +log10(Species):Latitude:(Lake+Stream)
        ,na.action=na.fail))
      }

  Vul_full=with(data,lm(log10(Vul)~log10(Species)
    +log10(Species):Latitude
    +log10(Species):(Stream+Lake+Marine+Terr)
    +log10(Species):Latitude:(Stream+Lake+Marine+Terr)
    ,na.action=na.fail))
  v_dredge=dredge(Vul_full,rank=AIC)
  if(infile=='../mod_data/summary-properties.tsv'){
    Vul_min=with(data,lm(log10(Vul)~log10(Species)
      +log10(Species):(Lake+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      ,na.action=na.fail)) } else {
    Vul_min=with(data,lm(log10(Vul)~log10(Species)
      +log10(Species):(Lake+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      ,na.action=na.fail))  }

    cutoff<- 4/((nrow(data)-length(Vul_min$coefficients)-2)) 
    plot(Vul_min, which=4, cook.levels=cutoff)
    #  57, 66, 138 are outliers.
    if(infile=='../mod_data/summary-properties.tsv'){
      subset=data[-c(57,127,138),] } else {
      subset=data[-c(57,66,138),]  }

    subVul_min=with(subset,lm(log10(Vul)~log10(Species)
      +log10(Species):(Lake+Terr)
      +log10(Species):Latitude
      +log10(Species):Latitude:Lake
      ,na.action=na.fail)) 

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
    Sp_latdirect=(with(data,lm(Species~Stream*Latitude+Lake+Marine+Terr),na.action=na.fail)) }
    } else {
    Sp_latdirect=(with(data,lm(Species~Stream*Latitude+Lake+Marine),na.action=na.fail)) }

  LS_latdirect=(with(data,lm(LS~Stream*Latitude+Marine),na.action=na.fail))
  Gen_latdirect=(with(data,lm(Gen~Marine+Stream),na.action=na.fail))
  Vul_latdirect=(with(data,lm(Vul~Latitude*Stream+Marine),na.action=na.fail))

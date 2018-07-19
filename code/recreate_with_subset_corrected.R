  #Let's look at the same old regressions with the subset of data...
  # For proportions, regressions are similar but vary slightly over ecotype.
  LS_full=with(data,lm(log(LS)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lakeweb+Marine+Terr)
    +log(Species):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
  ls_dredge=dredge(LS_full,rank=AIC)
  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    LS_min =with(data,lm(log(LS)~log(Species)
      +log(Species):Latitude
      +log(Species):(Lakeweb+Marine)
      +log(Species):Latitude:Lakeweb
      ,na.action=na.fail))    
    } else {
    LS_min =with(data,lm(log(LS)~log(Species)
      +log(Species):Latitude
      +log(Species):(Lakeweb+Marine+Terr)
      +log(Species):Latitude:Lakeweb
      ,na.action=na.fail))          
    }
  cutoff<- 4/((nrow(data)-length(LS_min$coefficients)-2)) 
  plot(LS_min, which=4, cook.levels=cutoff)
  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    # Very small changes to parameters, none to sig.
    subset=data[-c(78,187,222),]
    subLS_min=(with(subset,lm(log(LS)~log(Species)
    +log(Species):Latitude
    +log(Species):(Lakeweb+Marine)
    +log(Species):Latitude:Lakeweb
    ,na.action=na.fail)))
     } else {
    # Very small changes to parameters, only terrestrial intercept becomes non-sig.
    subset=data[-c(78,91,92),]
    subLS_min=(with(subset,lm(log(LS)~log(Species)
    +log(Species):Latitude
    +log(Species):(Lakeweb+Marine+Terr)
    +log(Species):Latitude:Lakeweb
    ,na.action=na.fail)))
    # Decided on DF
    }


  obs_LS=(with(data,lm(log(LS)~log(Species),na.action=na.fail)))

  Gen_full=with(data,lm(log(Gen)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lakeweb+Marine+Terr)
    +log(Species):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
  g_dredge=dredge(Gen_full,rank=AIC)
  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    Gen_min=with(data,lm(log(Gen)~log(Species)
        +log(Species):(Lakeweb+Stream+Marine)
        +log(Species):Latitude
        +log(Species):Latitude:(Lakeweb+Stream)
        ,na.action=na.fail)) 
    } else {
    Gen_min=with(data,lm(log(Gen)~log(Species)
        +log(Species):(Lakeweb+Stream+Marine+Terr)
        +log(Species):Latitude
        +log(Species):Latitude:(Lakeweb+Stream)
        ,na.action=na.fail)) 
    }
  cutoff<- 4/((nrow(data)-length(Gen_min$coefficients)-2)) 
  plot(Gen_min, which=4, cook.levels=cutoff)
  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    subset=data[-c(78,141,190),]
    subGen_min=(with(subset,lm(log(LS)~log(Species)
    +log(Species):(Lakeweb+Stream+Marine)
    +log(Species):Latitude
    +log(Species):Latitude:(Lakeweb+Stream),na.action=na.fail)))
    # Removing 3 outliers, stream and stream:latitude effects no longer significant.
    # No change to marine and lake effects
    } else {
    # Removing 3 outliers, stream and stream:latitude effects no longer significant.
    subset=data[-c(20,91,185),]
    subGen_min=(with(subset,lm(log(LS)~log(Species)
    +log(Species):(Lakeweb+Stream+Terr)
    +log(Species):Latitude
    +log(Species):Latitude:(Lakeweb+Stream),na.action=na.fail)))
    }

  obs_Gen=(with(data,lm(log(Gen)~log(Species),na.action=na.fail)))

  Vul_full=with(data,lm(log(Vul)~log(Species)
    +log(Species):Latitude
    +log(Species):(Stream+Lakeweb+Marine+Terr)
    +log(Species):Latitude:(Stream+Lakeweb+Marine+Terr)
    ,na.action=na.fail))
  v_dredge=dredge(Vul_full,rank=AIC)
  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    Vul_min=with(data,lm(log(Vul)~log(Species)
        +log(Species):(Lakeweb+Marine)
        +log(Species):Latitude
        +log(Species):Lakeweb:Latitude
        ,na.action=na.fail)) 
    } else {
    Vul_min=with(data,lm(log(Vul)~log(Species)
        +log(Species):(Lakeweb+Marine+Terr)
        +log(Species):Latitude
        +log(Species):Lakeweb:Latitude
        ,na.action=na.fail))       
    }
  cutoff<- 4/((nrow(data)-length(Vul_min$coefficients)-2)) 
  plot(Vul_min, which=4, cook.levels=cutoff)
  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    # Removing 3 outliers, no change to significance and tiny changes to coefficients
    subset=data[-c(78,187,222),]
    subVul_min=(with(subset,lm(log(LS)~log(Species)
    +log(Species):(Lakeweb+Marine)
    +log(Species):Latitude
    +log(Species):Lakeweb:Latitude,na.action=na.fail)))
    } else {
    # Removing 3 outliers, Terrestrial effect no longer significant.
    subset=data[-c(78,91,92),]
    subVul_min=(with(subset,lm(log(LS)~log(Species)
    +log(Species):Latitude
    +log(Species):(Lakeweb+Marine+Terr)
    +log(Species):Lakeweb:Latitude,na.action=na.fail)))
    }
    # Decided based on Df


  obs_Vul=(with(data,lm(log(Vul)~log(Species),na.action=na.fail)))

      # What's the basic correlation with latitude?
  Sp_latdirect_full=(with(data,lm(Species~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  LS_latdirect_full=(with(data,lm(LS~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  Gen_latdirect_full=(with(data,lm(Gen~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))
  Vul_latdirect_full=(with(data,lm(Vul~Latitude*(Stream+Lakeweb+Marine+Terr),na.action=na.fail)))

  sld=dredge(Sp_latdirect_full,rank=AIC)
  lld=dredge(LS_latdirect_full,rank=AIC)
  gld=dredge(Gen_latdirect_full,rank=AIC)
  vld=dredge(Vul_latdirect_full,rank=AIC)

  # Most cases, same model for both TS and original
  Sp_latdirect=(with(data,lm(Species~Lakeweb+Marine+Terr),na.action=na.fail)) 
  LS_latdirect=(with(data,lm(LS~Lakeweb+Terr),na.action=na.fail))
  Vul_latdirect=(with(data,lm(Vul~Lakeweb+Terr),na.action=na.fail))

  if(infile=='../mod_data/summary-properties_corrected_webs.tsv'){
    Gen_latdirect=(with(data,lm(Gen~Marine+Stream),na.action=na.fail))
    } else {
    Gen_latdirect=(with(data,lm(Gen~Stream),na.action=na.fail))
    }




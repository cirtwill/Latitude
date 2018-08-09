S_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      # All have Species:Lakeweb, Species:Marine, Species:Terr
      # All have Species:Latitude:Lakeweb
      # Gen_min also has Species:Stream, Species:Latitude:Stream

      marginal <- betas["log(Species)"]+lat*betas["log(Species):Latitude"]

      se <- sqrt(covar["log(Species)","log(Species)"]+2*lat*covar["log(Species):Latitude","log(Species)"]
        +lat^2*covar["log(Species):Latitude","log(Species):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se


      lake_m <- marginal + lat*betas["log(Species):Lakeweb:Latitude"] + betas["log(Species):Lakeweb"]
      lake_se <- sqrt(se^2 + 
        lat^2*covar["log(Species):Lakeweb:Latitude","log(Species):Lakeweb:Latitude"] 
        +2*lat*covar["log(Species):Lakeweb:Latitude","log(Species)"]
        +2*lat^2*covar["log(Species):Lakeweb:Latitude","log(Species):Latitude"]
        +covar["log(Species):Lakeweb","log(Species):Lakeweb"] 
        +2*covar["log(Species):Lakeweb","log(Species)"]
        +2*lat*covar["log(Species):Lakeweb","log(Species):Latitude"])

      lake_upper <- lake_m + 1.64*lake_se
      lake_lower <- lake_m - 1.64*lake_se

      marine_m <- marginal + betas["log(Species):Marine"]
      marine_se <- sqrt(se^2 + 
          covar["log(Species):Marine","log(Species):Marine"] 
          +2*covar["log(Species):Marine","log(Species)"]
          +2*lat*covar["log(Species):Marine","log(Species):Latitude"])
      marine_upper <- marine_m + 1.64*marine_se
      marine_lower <- marine_m - 1.64*marine_se

      terr_m <- marginal + betas["log(Species):Terr"]
      terr_se <- sqrt(se^2 + 
          covar["log(Species):Terr","log(Species):Terr"] 
          +2*covar["log(Species):Terr","log(Species)"]
          +2*lat*covar["log(Species):Terr","log(Species):Latitude"])
      terr_upper <- terr_m + 1.64*terr_se
      terr_lower <- terr_m - 1.64*terr_se

      if(model=="LS_min" | model=="Vul_min"){ # No stream terms
        results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,
          terr_m,terr_upper,terr_lower,lake_m,lake_upper,lake_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower",
          "Terr","Terr_upper","Terr_lower","Lakeweb","Lakeweb_upper","Lakeweb_lower")
      }

      if(model=="Gen_min"){

        stream_m <- marginal + lat*betas["log(Species):Stream:Latitude"] + betas["log(Species):Stream"]
        stream_se <- sqrt(se^2 +
          lat^2*covar["log(Species):Stream:Latitude","log(Species):Stream:Latitude"] 
          +2*lat*covar["log(Species):Stream:Latitude","log(Species)"]
          +2*lat^2*covar["log(Species):Stream:Latitude","log(Species):Latitude"]
          +covar["log(Species):Stream","log(Species):Stream"] 
          +2*covar["log(Species):Stream","log(Species)"]
          +2*lat*covar["log(Species):Stream","log(Species):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,
        terr_m,terr_upper,terr_lower,lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower",
        "Terr","Terr_upper","Terr_lower","Lakeweb","Lakeweb_upper","Lakeweb_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

S_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)
      marginal <- betas["log(Species)"]+lat*betas["log(Species):Latitude"]

      se <- sqrt(covar["log(Species)","log(Species)"]+2*lat*covar["log(Species):Latitude","log(Species)"]
        +lat^2*covar["log(Species):Latitude","log(Species):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se


      lake_m <- marginal + lat*betas["log(Species):Lake:Latitude"] + betas["log(Species):Lake"]
      lake_se <- sqrt(se^2 + 
        lat^2*covar["log(Species):Lake:Latitude","log(Species):Lake:Latitude"] 
        +2*lat*covar["log(Species):Lake:Latitude","log(Species)"]
        +2*lat^2*covar["log(Species):Lake:Latitude","log(Species):Latitude"]
        +covar["log(Species):Lake","log(Species):Lake"] 
        +2*covar["log(Species):Lake","log(Species)"]
        +2*lat*covar["log(Species):Lake","log(Species):Latitude"])

      lake_upper <- lake_m + 1.64*lake_se
      lake_lower <- lake_m - 1.64*lake_se

      if(model=="LS_min" | model=="Vul_min"){
        terr_m <- marginal + betas["log(Species):Terr"]
        terr_se <- sqrt(se^2 +
          covar["log(Species):Terr","log(Species):Terr"] 
          +2*covar["log(Species):Terr","log(Species)"]
          +2*lat*covar["log(Species):Terr","log(Species):Latitude"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        results <- cbind(lat,marginal,upper,lower,terr_m,terr_upper,terr_lower,lake_m,lake_upper,lake_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Terrestrial","Terr_upper","Terr_lower","Lake","Lake_upper","Lake_lower")
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

      results <- cbind(lat,marginal,upper,lower,lake_m,lake_upper,lake_lower,
        stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower",
        "Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

B_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)
      marginal <- betas["log(Basal)"]+lat*betas["log(Basal):Latitude"]

      se <- sqrt(covar["log(Basal)","log(Basal)"]+2*lat*covar["log(Basal):Latitude","log(Basal)"]
        +lat^2*covar["log(Basal):Latitude","log(Basal):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      stream_m <- marginal + betas["log(Basal):Stream"]
      stream_se <- sqrt(se^2 +
        covar["log(Basal):Stream","log(Basal):Stream"] 
        +2*covar["log(Basal):Stream","log(Basal)"]
        +2*lat*covar["log(Basal):Stream","log(Basal):Latitude"])
      stream_upper <- stream_m + 1.64*stream_se
      stream_lower <- stream_m - 1.64*stream_se

      marine_m <- marginal + lat*betas["log(Basal):Latitude:Marine"] + betas["log(Basal):Marine"]
      marine_se <- sqrt(se^2 + 
        lat^2*covar["log(Basal):Latitude:Marine","log(Basal):Latitude:Marine"] 
        +2*lat*covar["log(Basal):Latitude:Marine","log(Basal)"]
        +2*lat^2*covar["log(Basal):Latitude:Marine","log(Basal):Latitude"]
        +covar["log(Basal):Marine","log(Basal):Marine"] 
        +2*covar["log(Basal):Marine","log(Basal)"]
        +2*lat*covar["log(Basal):Marine","log(Basal):Latitude"])

      marine_upper <- marine_m + 1.64*marine_se
      marine_lower <- marine_m - 1.64*marine_se

      terr_m <- marginal + lat*betas["log(Basal):Latitude:Terr"] + betas["log(Basal):Terr"]
      terr_se <- sqrt(se^2 + 
        lat^2*covar["log(Basal):Latitude:Terr","log(Basal):Latitude:Terr"] 
        +2*lat*covar["log(Basal):Latitude:Terr","log(Basal)"]
        +2*lat^2*covar["log(Basal):Latitude:Terr","log(Basal):Latitude"]
        +covar["log(Basal):Terr","log(Basal):Terr"] 
        +2*covar["log(Basal):Terr","log(Basal)"]
        +2*lat*covar["log(Basal):Terr","log(Basal):Latitude"])

      terr_upper <- terr_m + 1.64*terr_se
      terr_lower <- terr_m - 1.64*terr_se

      # Marine, Terr (Stream)
      results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,marine_m,marine_upper,marine_lower,terr_m,terr_upper,terr_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
                          "Marine","Marine_upper","Marine_lower","Terrestrial","Terr_upper","Terr_lower")
      return(results)
    }

I_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      marginal <- betas["log(Intermediate)"]

      se <- sqrt(covar["log(Intermediate)","log(Intermediate)"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      stream_m <- marginal + betas["log(Intermediate):Stream"]
      stream_se <- sqrt(se^2 +
        covar["log(Intermediate):Stream","log(Intermediate):Stream"] 
        +2*covar["log(Intermediate):Stream","log(Intermediate)"]        )

      stream_upper <- stream_m + 1.64*stream_se
      stream_lower <- stream_m - 1.64*stream_se

      if(model=="LS_I_min"){
        results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower")
        } 
        
      if(model=="Vul_I_min"){
        lake_m <- marginal + betas["log(Intermediate):Lake"]
        lake_se <- sqrt(se^2 + 
          +covar["log(Intermediate):Lake","log(Intermediate):Lake"] 
          +2*covar["log(Intermediate):Lake","log(Intermediate)"])

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,
          lake_m,lake_upper,lake_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
          "Lake","Lake_upper","Lake_lower")        }

      if(model=="Gen_I_min"){

        marginal <- betas["log(Intermediate)"]+lat*betas["log(Intermediate):Latitude"]

        se <- sqrt(covar["log(Intermediate)","log(Intermediate)"]
          +2*lat*covar["log(Intermediate)","log(Intermediate):Latitude"]
          +lat^2*covar["log(Intermediate):Latitude","log(Intermediate):Latitude"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        stream_m <- marginal + betas["log(Intermediate):Stream"] +  lat*betas["log(Intermediate):Latitude:Stream"]
        stream_se <- sqrt(se^2 + 
          +covar["log(Intermediate):Stream","log(Intermediate):Stream"] 
          +2*covar["log(Intermediate):Stream","log(Intermediate)"]
          +2*lat*covar["log(Intermediate):Stream","log(Intermediate):Latitude"]
          +2*lat*covar["log(Intermediate):Stream","log(Intermediate):Latitude:Stream"]
          +2*lat^2*covar["log(Intermediate):Latitude:Stream","log(Intermediate):Latitude"]
          +2*lat*covar["log(Intermediate):Latitude:Stream","log(Intermediate)"]
          +lat^2*covar["log(Intermediate):Latitude:Stream","log(Intermediate):Latitude:Stream"]
          )

        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se
 
        lake_m <- marginal + betas["log(Intermediate):Lake"] +  lat*betas["log(Intermediate):Latitude:Lake"]
        lake_se <- sqrt(se^2 + 
          +covar["log(Intermediate):Lake","log(Intermediate):Lake"] 
          +2*covar["log(Intermediate):Lake","log(Intermediate)"]
          +2*lat*covar["log(Intermediate):Lake","log(Intermediate):Latitude"]
          +2*lat*covar["log(Intermediate):Lake","log(Intermediate):Latitude:Lake"]
          +2*lat^2*covar["log(Intermediate):Latitude:Lake","log(Intermediate):Latitude"]
          +2*lat*covar["log(Intermediate):Latitude:Lake","log(Intermediate)"]
          +lat^2*covar["log(Intermediate):Latitude:Lake","log(Intermediate):Latitude:Lake"]
          )

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,
          lake_m,lake_upper,lake_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
          "Lake","Lake_upper","Lake_lower")      }

      return(results)
    }

T_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      if(model=="LS_T_min" | model=="Vul_T_min"){
        marginal <- betas["log(Toppreds)"]+lat*betas["log(Toppreds):Latitude"]

        se <- sqrt(covar["log(Toppreds)","log(Toppreds)"]+2*lat*covar["log(Toppreds):Latitude","log(Toppreds)"]
          +lat^2*covar["log(Toppreds):Latitude","log(Toppreds):Latitude"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        marine_m <- marginal + betas["log(Toppreds):Marine"]
        marine_se <- sqrt(se^2 +
          covar["log(Toppreds):Marine","log(Toppreds):Marine"] 
          +2*covar["log(Toppreds):Marine","log(Toppreds)"]
          +2*lat*covar["log(Toppreds):Marine","log(Toppreds):Latitude"])
        marine_upper <- marine_m + 1.64*marine_se
        marine_lower <- marine_m - 1.64*marine_se

        lake_m <- marginal + betas["log(Toppreds):Lake"]
        lake_se <- sqrt(se^2 +covar["log(Toppreds):Lake","log(Toppreds):Lake"] 
          +2*covar["log(Toppreds):Lake","log(Toppreds)"]
          +2*lat*covar["log(Toppreds):Lake","log(Toppreds):Latitude"])

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        stream_m <- marginal + lat*betas["log(Toppreds):Stream:Latitude"] + betas["log(Toppreds):Stream"]
        stream_se <- sqrt(se^2 +
          lat^2*covar["log(Toppreds):Stream:Latitude","log(Toppreds):Stream:Latitude"] 
          +2*lat*covar["log(Toppreds):Stream:Latitude","log(Toppreds)"]
          +2*lat^2*covar["log(Toppreds):Stream:Latitude","log(Toppreds):Latitude"]
          +covar["log(Toppreds):Stream","log(Toppreds):Stream"] 
          +2*covar["log(Toppreds):Stream","log(Toppreds)"]
          +2*lat*covar["log(Toppreds):Stream","log(Toppreds):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

        terr_m <- marginal + betas["log(Toppreds):Terr"]
        terr_se <- sqrt(se^2 +covar["log(Toppreds):Terr","log(Toppreds):Terr"] 
          +2*covar["log(Toppreds):Terr","log(Toppreds)"]
          +2*lat*covar["log(Toppreds):Terr","log(Toppreds):Latitude"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,lake_m,lake_upper,lake_lower,
          stream_m,stream_upper,stream_lower,terr_m,terr_upper,terr_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower","Lake","Lake_upper","Lake_lower",
          "Stream","Stream_upper","Stream_lower","Terrestrial","Terr_upper","Terr_lower")
      }

      if(model=="Gen_T_min"){        
        marginal <- betas["log(Toppreds)"]

        se <- sqrt(covar["log(Toppreds)","log(Toppreds)"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        marine_m <- marginal + betas["log(Toppreds):Marine"]
        marine_se <- sqrt(se^2 +
          covar["log(Toppreds):Marine","log(Toppreds):Marine"] 
          +2*covar["log(Toppreds):Marine","log(Toppreds)"])
        marine_upper <- marine_m + 1.64*marine_se
        marine_lower <- marine_m - 1.64*marine_se

        stream_m <- marginal + betas["log(Toppreds):Stream"]
        stream_se <- sqrt(se^2 +
          +covar["log(Toppreds):Stream","log(Toppreds):Stream"] 
          +2*covar["log(Toppreds):Stream","log(Toppreds)"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }
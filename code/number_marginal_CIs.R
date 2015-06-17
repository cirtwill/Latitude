##### Need to update all these for number best-fit models

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

      terr_m <- marginal + betas["log(Species):Terr"]
      terr_se <- sqrt(se^2 +
        covar["log(Species):Terr","log(Species):Terr"] 
        +2*covar["log(Species):Terr","log(Species)"]
        +2*lat*covar["log(Species):Terr","log(Species):Latitude"])
      terr_upper <- terr_m + 1.64*terr_se
      terr_lower <- terr_m - 1.64*terr_se

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

      results <- cbind(lat,marginal,upper,lower,terr_m,terr_upper,terr_lower,lake_m,lake_upper,lake_lower,
        stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Terrestrial","Terr_upper","Terr_lower","Lake","Lake_upper","Lake_lower",
        "Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

B_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      marginal <- betas["log(Basal * Species)"]
      # No effects of latitude or ecosystem type in any case
      se <- sqrt(covar["log(Basal * Species)","log(Basal * Species)"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se
  
      results=cbind(marginal,upper,lower)
      colnames(results)=c("Marginal","Upper","Lower")

      return(results)
    }

I_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      marginal <- betas["log(Intermediate * Species)"] + lat*betas["log(Intermediate * Species):Latitude"]

      se <- sqrt(covar["log(Intermediate * Species)","log(Intermediate * Species)"]
        +2*lat*covar["log(Intermediate * Species)","log(Intermediate * Species):Latitude"]
        +lat^2*covar["log(Intermediate * Species):Latitude","log(Intermediate * Species):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      if(model=="LS_I_min"){
        stream_m <- marginal + betas["log(Intermediate * Species):Stream"] 
          + lat*betas["log(Intermediate * Species):Stream:Latitude"]
        stream_se <- sqrt(se^2 +
          covar["log(Intermediate * Species):Stream","log(Intermediate * Species):Stream"] 
          +2*covar["log(Intermediate * Species):Stream","log(Intermediate * Species)"]  
          +2*lat*covar["log(Intermediate * Species):Stream","log(Intermediate * Species):Latitude"]
          +lat^2*covar["log(Intermediate * Species):Stream:Latitude","log(Intermediate * Species):Stream:Latitude"]
          +2*lat*covar["log(Intermediate * Species):Stream:Latitude","log(Intermediate * Species)"]
          +2*lat*covar["log(Intermediate * Species):Stream:Latitude","log(Intermediate * Species):Stream"]
          +2*lat^2*covar["log(Intermediate * Species):Stream:Latitude","log(Intermediate * Species):Latitude"])

        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

        lake_m <- marginal + betas["log(Intermediate * Species):Lake"] 
          + lat*betas["log(Intermediate * Species):Lake:Latitude"]
        lake_se <- sqrt(se^2 + 
          +covar["log(Intermediate * Species):Lake","log(Intermediate * Species):Lake"] 
          +2*covar["log(Intermediate * Species):Lake","log(Intermediate * Species)"]
          +2*lat*covar["log(Intermediate * Species):Lake","log(Intermediate * Species):Latitude"]
          +lat^2*covar["log(Intermediate * Species):Lake:Latitude","log(Intermediate * Species):Lake:Latitude"]
          +2*lat*covar["log(Intermediate * Species):Lake:Latitude","log(Intermediate * Species)"]
          +2*lat*covar["log(Intermediate * Species):Lake:Latitude","log(Intermediate * Species):Lake"]
          +2*lat^2*covar["log(Intermediate * Species):Lake:Latitude","log(Intermediate * Species):Latitude"])

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se
      } else {
        stream_m <- marginal + betas["log(Intermediate * Species):Stream"] 
          + lat*betas["log(Intermediate * Species):Latitude:Stream"]
        stream_se <- sqrt(se^2 +
          covar["log(Intermediate * Species):Stream","log(Intermediate * Species):Stream"] 
          +2*covar["log(Intermediate * Species):Stream","log(Intermediate * Species)"]  
          +2*lat*covar["log(Intermediate * Species):Stream","log(Intermediate * Species):Latitude"]
          +lat^2*covar["log(Intermediate * Species):Latitude:Stream","log(Intermediate * Species):Latitude:Stream"]
          +2*lat*covar["log(Intermediate * Species):Latitude:Stream","log(Intermediate * Species)"]
          +2*lat*covar["log(Intermediate * Species):Latitude:Stream","log(Intermediate * Species):Stream"]
          +2*lat^2*covar["log(Intermediate * Species):Latitude:Stream","log(Intermediate * Species):Latitude"])

        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

        lake_m <- marginal + betas["log(Intermediate * Species):Lake"] 
          + lat*betas["log(Intermediate * Species):Latitude:Lake"]
        lake_se <- sqrt(se^2 + 
          +covar["log(Intermediate * Species):Lake","log(Intermediate * Species):Lake"] 
          +2*covar["log(Intermediate * Species):Lake","log(Intermediate * Species)"]
          +2*lat*covar["log(Intermediate * Species):Lake","log(Intermediate * Species):Latitude"]
          +lat^2*covar["log(Intermediate * Species):Latitude:Lake","log(Intermediate * Species):Latitude:Lake"]
          +2*lat*covar["log(Intermediate * Species):Latitude:Lake","log(Intermediate * Species)"]
          +2*lat*covar["log(Intermediate * Species):Latitude:Lake","log(Intermediate * Species):Lake"]
          +2*lat^2*covar["log(Intermediate * Species):Latitude:Lake","log(Intermediate * Species):Latitude"])

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se
        }

      if(model=="LS_I_min" | model=="Vul_I_min"){

        results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,
          lake_m,lake_upper,lake_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
          "Lake","Lake_upper","Lake_lower")
      }

      if(model=="Gen_I_min"){
        marginal <- betas["log(Intermediate * Species)"]+lat*betas["log(Intermediate * Species):Latitude"]

        se <- sqrt(covar["log(Intermediate * Species)","log(Intermediate * Species)"]+2*lat*covar["log(Intermediate * Species):Latitude","log(Intermediate * Species)"]
          +lat^2*covar["log(Intermediate * Species):Latitude","log(Intermediate * Species):Latitude"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        terr_m <- marginal + betas["log(Intermediate * Species):Terr"]
        terr_se <- sqrt(se^2 +
          covar["log(Intermediate * Species):Terr","log(Intermediate * Species):Terr"] 
          +2*covar["log(Intermediate * Species):Terr","log(Intermediate * Species)"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        marine_m <- marginal + betas["log(Intermediate * Species):Marine"]
        marine_se <- sqrt(se^2 + 
          +covar["log(Intermediate * Species):Marine","log(Intermediate * Species):Marine"] 
          +2*covar["log(Intermediate * Species):Marine","log(Intermediate * Species)"])
        marine_upper <- marine_m + 1.64*marine_se
        marine_lower <- marine_m - 1.64*marine_se

        results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,
          lake_m,lake_upper,lake_lower,terr_m,terr_upper,terr_lower,marine_m,marine_upper,marine_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
          "Lake","Lake_upper","Lake_lower","Terrestrial","Terr_upper","Terr_lower","Marine","Marine_upper","Marine_lower")
      }
      return(results)
    }

T_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      if(model=="LS_T_min" | model=="Vul_T_min"){
        marginal <- betas["log(Toppreds * Species)"]

        se <- sqrt(covar["log(Toppreds * Species)","log(Toppreds * Species)"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        marine_m <- marginal + betas["log(Toppreds * Species):Marine"]
        marine_se <- sqrt(se^2 +
          covar["log(Toppreds * Species):Marine","log(Toppreds * Species):Marine"] 
          +2*covar["log(Toppreds * Species):Marine","log(Toppreds * Species)"])
        marine_upper <- marine_m + 1.64*marine_se
        marine_lower <- marine_m - 1.64*marine_se

        terr_m <- marginal + betas["log(Toppreds * Species):Terr"]
        terr_se <- sqrt(se^2 +
          +covar["log(Toppreds * Species):Terr","log(Toppreds * Species):Terr"] 
          +2*covar["log(Toppreds * Species):Terr","log(Toppreds * Species)"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,
          terr_m,terr_upper,terr_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower",
          "Terrestrial","Terr_upper","Terr_lower")
      }

      if(model=="Gen_T_min"){        
        marginal <- betas["log(Toppreds * Species)"] + lat*betas["log(Toppreds * Species):Latitude"]

        se <- sqrt(covar["log(Toppreds * Species)","log(Toppreds * Species)"]
          +2*lat*covar["log(Toppreds * Species)","log(Toppreds * Species):Latitude"]
          +lat^2*covar["log(Toppreds * Species):Latitude","log(Toppreds * Species):Latitude"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        marine_m <- marginal + betas["log(Toppreds * Species):Marine"]
        marine_se <- sqrt(se^2 +
          covar["log(Toppreds * Species):Marine","log(Toppreds * Species):Marine"] 
          +2*covar["log(Toppreds * Species):Marine","log(Toppreds * Species)"]
          +2*lat*covar["log(Toppreds * Species):Marine","log(Toppreds * Species):Latitude"])
        marine_upper <- marine_m + 1.64*marine_se
        marine_lower <- marine_m - 1.64*marine_se

        stream_m <- marginal + betas["log(Toppreds * Species):Stream"]
        stream_se <- sqrt(se^2 +
          +covar["log(Toppreds * Species):Stream","log(Toppreds * Species):Stream"] 
          +2*covar["log(Toppreds * Species):Stream","log(Toppreds * Species)"]
          +2*lat*covar["log(Toppreds * Species):Stream","log(Toppreds * Species):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

        terr_m <- marginal + betas["log(Toppreds * Species):Terr"] + lat*betas["log(Toppreds * Species):Terr:Latitude"]
        terr_se <- sqrt(se^2
          +covar["log(Toppreds * Species):Terr","log(Toppreds * Species):Terr"] 
          +2*covar["log(Toppreds * Species):Terr","log(Toppreds * Species)"]
          +2*lat*covar["log(Toppreds * Species):Terr","log(Toppreds * Species):Latitude"]
          +2*lat*covar["log(Toppreds * Species):Terr:Latitude","log(Toppreds * Species)"]
          +2*lat^2*covar["log(Toppreds * Species):Terr:Latitude","log(Toppreds * Species):Latitude"]
          +2*lat*covar["log(Toppreds * Species):Terr:Latitude","log(Toppreds * Species):Terr"]
          +lat^2*covar["log(Toppreds * Species):Terr:Latitude","log(Toppreds * Species):Terr:Latitude"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,
          terr_m,terr_upper,terr_lower,stream_m,stream_upper,stream_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower",
          "Terrestrial","Terr_upper","Terr_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }
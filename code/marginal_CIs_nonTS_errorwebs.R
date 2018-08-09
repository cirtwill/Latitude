S_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      # All have Species:Lakeweb, Species:Marine
      # All have Species:Latitude:Lakeweb
      # Gen_min also has Species:Stream, Species:Latitude:Stream

      marginal <- betas["log(Species)"]+lat*betas["log(Species):Latitude"]

      se <- sqrt(covar["log(Species)","log(Species)"]+2*lat*covar["log(Species):Latitude","log(Species)"]
        +lat^2*covar["log(Species):Latitude","log(Species):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

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

        if(model=="LS_min"){
          lake_m <- marginal + lat*betas["log(Species):Latitude:Lakeweb"] + betas["log(Species):Lakeweb"]
          lake_se <- sqrt(se^2 + 
            lat^2*covar["log(Species):Latitude:Lakeweb","log(Species):Latitude:Lakeweb"] 
            +2*lat*covar["log(Species):Latitude:Lakeweb","log(Species)"]
            +2*lat^2*covar["log(Species):Latitude:Lakeweb","log(Species):Latitude"]
            +covar["log(Species):Lakeweb","log(Species):Lakeweb"] 
            +2*covar["log(Species):Lakeweb","log(Species)"]
            +2*lat*covar["log(Species):Lakeweb","log(Species):Latitude"])

          lake_upper <- lake_m + 1.64*lake_se
          lake_lower <- lake_m - 1.64*lake_se
        } else {
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
        }

        results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,lake_m,lake_upper,lake_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower","Lakeweb","Lakeweb_upper","Lakeweb_lower")
      }

      if(model=="Gen_min"){

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
        lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower",
        "Lakeweb","Lakeweb_upper","Lakeweb_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

B_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      # All have Species:Lakeweb, Species:Marine
      # All have Species:Latitude:Lakeweb
      # Gen_min also has Species:Stream, Species:Latitude:Stream

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

      results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,terr_m,terr_upper,terr_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower","Terrestrial","Terr_upper","Terr_lower")

      return(results)
    }

I_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      # All have Species:Lakeweb, Species:Marine
      # All have Species:Latitude:Lakeweb
      # Gen_min also has Species:Stream, Species:Latitude:Stream

      if(model %in% c("LS_I_min","Gen_I_min")){
        marginal <- betas["log(Intermediate)"]+lat*betas["log(Intermediate):Latitude"]

        se <- sqrt(covar["log(Intermediate)","log(Intermediate)"]+2*lat*covar["log(Intermediate):Latitude","log(Intermediate)"]
          +lat^2*covar["log(Intermediate):Latitude","log(Intermediate):Latitude"])

      } else {
        marginal <- betas["log(Intermediate)"]

        se <- sqrt(covar["log(Intermediate)","log(Intermediate)"])
      }

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      if(model=="LS_I_min" | model=="Vul_I_min"){ # No stream terms

        if(model=="LS_I_min"){
          stream_m <- marginal + lat*betas["log(Intermediate):Stream:Latitude"] + betas["log(Intermediate):Stream"]
          stream_se <- sqrt(se^2 + 
            lat^2*covar["log(Intermediate):Stream:Latitude","log(Intermediate):Stream:Latitude"] 
            +2*lat*covar["log(Intermediate):Stream:Latitude","log(Intermediate)"]
            +2*lat^2*covar["log(Intermediate):Stream:Latitude","log(Intermediate):Latitude"]
            +covar["log(Intermediate):Stream","log(Intermediate):Stream"] 
            +2*covar["log(Intermediate):Stream","log(Intermediate)"]
            +2*lat*covar["log(Intermediate):Stream","log(Intermediate):Latitude"])

          stream_upper <- stream_m + 1.64*stream_se
          stream_lower <- stream_m - 1.64*stream_se
        } else {
          stream_m <- marginal + betas["log(Intermediate):Stream"]
          stream_se <- sqrt(se^2+covar["log(Intermediate):Stream","log(Intermediate):Stream"] 
            +2*covar["log(Intermediate):Stream","log(Intermediate)"])

          stream_upper <- stream_m + 1.64*stream_se
          stream_lower <- stream_m - 1.64*stream_se          
        }

        results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower")
      }

      if(model=="Gen_I_min"){

        lake_m <- marginal + lat*betas["log(Intermediate):Latitude:Lakeweb"] + betas["log(Intermediate):Lakeweb"]
        lake_se <- sqrt(se^2 + 
          lat^2*covar["log(Intermediate):Latitude:Lakeweb","log(Intermediate):Latitude:Lakeweb"] 
          +2*lat*covar["log(Intermediate):Latitude:Lakeweb","log(Intermediate)"]
          +2*lat^2*covar["log(Intermediate):Latitude:Lakeweb","log(Intermediate):Latitude"]
          +covar["log(Intermediate):Lakeweb","log(Intermediate):Lakeweb"] 
          +2*covar["log(Intermediate):Lakeweb","log(Intermediate)"]
          +2*lat*covar["log(Intermediate):Lakeweb","log(Intermediate):Latitude"])

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        stream_m <- marginal + lat*betas["log(Intermediate):Latitude:Stream"] + betas["log(Intermediate):Stream"]
        stream_se <- sqrt(se^2 +
          lat^2*covar["log(Intermediate):Latitude:Stream","log(Intermediate):Latitude:Stream"] 
          +2*lat*covar["log(Intermediate):Latitude:Stream","log(Intermediate)"]
          +2*lat^2*covar["log(Intermediate):Latitude:Stream","log(Intermediate):Latitude"]
          +covar["log(Intermediate):Stream","log(Intermediate):Stream"] 
          +2*covar["log(Intermediate):Stream","log(Intermediate)"]
          +2*lat*covar["log(Intermediate):Stream","log(Intermediate):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(lat,marginal,upper,lower,
        lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower",
        "Lakeweb","Lakeweb_upper","Lakeweb_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
  }

T_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      # All have Species:Lakeweb, Species:Marine
      # All have Species:Latitude:Lakeweb
      # Gen_min also has Species:Stream, Species:Latitude:Stream

      marginal <- betas["log(Toppreds)"]+lat*betas["log(Toppreds):Latitude"]

      se <- sqrt(covar["log(Toppreds)","log(Toppreds)"]+2*lat*covar["log(Toppreds):Latitude","log(Toppreds)"]
        +lat^2*covar["log(Toppreds):Latitude","log(Toppreds):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      if(model=="LS_T_min" | model=="Gen_T_min"){ # No stream terms

          lake_m <- marginal + betas["log(Toppreds):Lakeweb"]
          lake_se <- sqrt(se^2 + 
            covar["log(Toppreds):Lakeweb","log(Toppreds):Lakeweb"] 
            +2*covar["log(Toppreds):Lakeweb","log(Toppreds)"]
            +2*lat*covar["log(Toppreds):Lakeweb","log(Toppreds):Latitude"])

          lake_upper <- lake_m + 1.64*lake_se
          lake_lower <- lake_m - 1.64*lake_se

          terr_m <- marginal + lat*betas["log(Toppreds):Terr:Latitude"] + betas["log(Toppreds):Terr"]
          terr_se <- sqrt(se^2 + 
            lat^2*covar["log(Toppreds):Terr:Latitude","log(Toppreds):Terr:Latitude"] 
            +2*lat*covar["log(Toppreds):Terr:Latitude","log(Toppreds)"]
            +2*lat^2*covar["log(Toppreds):Terr:Latitude","log(Toppreds):Latitude"]
            +covar["log(Toppreds):Terr","log(Toppreds):Terr"] 
            +2*covar["log(Toppreds):Terr","log(Toppreds)"]
            +2*lat*covar["log(Toppreds):Terr","log(Toppreds):Latitude"])

          terr_upper <- terr_m + 1.64*terr_se
          terr_lower <- terr_m - 1.64*terr_se

        if(model=="LS_T_min"){

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
        } else {
          stream_m <- marginal + betas["log(Toppreds):Stream"]
          stream_se <- sqrt(se^2 + 
            +covar["log(Toppreds):Stream","log(Toppreds):Stream"] 
            +2*covar["log(Toppreds):Stream","log(Toppreds)"]
            +2*lat*covar["log(Toppreds):Stream","log(Toppreds):Latitude"])

          stream_upper <- stream_m + 1.64*stream_se
          stream_lower <- stream_m - 1.64*stream_se          
        }

        results <- cbind(lat,marginal,upper,lower,lake_m,lake_upper,lake_lower,
          terr_m,terr_upper,terr_lower,stream_m,stream_upper,stream_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Lakeweb","Lakeweb_upper","Lakeweb_lower",
          "Terrestrial","Terr_upper","Terr_lower","Stream","Stream_upper","Stream_lower")
      }

      if(model=="Vul_T_min"){

          lake_m <- marginal + betas["log(Toppreds):Lakeweb"]
          lake_se <- sqrt(se^2 + 
            covar["log(Toppreds):Lakeweb","log(Toppreds):Lakeweb"] 
            +2*covar["log(Toppreds):Lakeweb","log(Toppreds)"]
            +2*lat*covar["log(Toppreds):Lakeweb","log(Toppreds):Latitude"])

          lake_upper <- lake_m + 1.64*lake_se
          lake_lower <- lake_m - 1.64*lake_se

          marine_m <- marginal + betas["log(Toppreds):Marine"]
          marine_se <- sqrt(se^2 + 
            covar["log(Toppreds):Marine","log(Toppreds):Marine"] 
            +2*covar["log(Toppreds):Marine","log(Toppreds)"]
            +2*lat*covar["log(Toppreds):Marine","log(Toppreds):Latitude"])

          marine_upper <- marine_m + 1.64*marine_se
          marine_lower <- marine_m - 1.64*marine_se

          terr_m <- marginal + lat*betas["log(Toppreds):Terr:Latitude"] + betas["log(Toppreds):Terr"]
          terr_se <- sqrt(se^2 + 
            lat^2*covar["log(Toppreds):Terr:Latitude","log(Toppreds):Terr:Latitude"] 
            +2*lat*covar["log(Toppreds):Terr:Latitude","log(Toppreds)"]
            +2*lat^2*covar["log(Toppreds):Terr:Latitude","log(Toppreds):Latitude"]
            +covar["log(Toppreds):Terr","log(Toppreds):Terr"] 
            +2*covar["log(Toppreds):Terr","log(Toppreds)"]
            +2*lat*covar["log(Toppreds):Terr","log(Toppreds):Latitude"])

          terr_upper <- terr_m + 1.64*terr_se
          terr_lower <- terr_m - 1.64*terr_se

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

        results <- cbind(lat,marginal,upper,lower,lake_m,lake_upper,lake_lower,marine_m,marine_upper,marine_lower,
          terr_m,terr_upper,terr_lower,stream_m,stream_upper,stream_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Lakeweb","Lakeweb_upper","Lakeweb_lower","Marine","Marine_upper","Marine_lower",
          "Terrestrial","Terr_upper","Terr_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
  }

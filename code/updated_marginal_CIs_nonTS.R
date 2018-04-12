S_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      lat=seq(0,90,length.out=200)

      if(model=="LS_min" | model=="Vul_min"){ # No latitude terms

        marginal <- betas["log10(Species)"]

        se <- sqrt(covar["log10(Species)","log10(Species)"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        terr_m <- marginal + betas["log10(Species):Terr"]
        terr_se <- sqrt(se^2 +
          covar["log10(Species):Terr","log10(Species):Terr"] 
          +2*covar["log10(Species):Terr","log10(Species)"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        stream_m <- marginal + betas["log10(Species):Stream"]
        stream_se <- sqrt(se^2 +
          covar["log10(Species):Stream","log10(Species):Stream"] 
          +2*covar["log10(Species):Stream","log10(Species)"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se


        results <- cbind(lat,marginal,upper,lower,terr_m,terr_upper,terr_lower,stream_m,stream_upper,stream_lower)
        colnames(results)=c("Latitude","Marginal","Upper","Lower","Terrestrial","Terr_upper","Terr_lower","Stream","Stream_upper","Stream_lower")
      }

      if(model=="Gen_min"){

        marginal <- betas["log10(Species)"]+lat*betas["log10(Species):Latitude"]

        se <- sqrt(covar["log10(Species)","log10(Species)"]+2*lat*covar["log10(Species):Latitude","log10(Species)"]
          +lat^2*covar["log10(Species):Latitude","log10(Species):Latitude"])

        upper <- marginal + 1.64*se
        lower <- marginal - 1.64*se

        terr_m <- marginal + betas["log10(Species):Terr"]
        terr_se <- sqrt(se^2 +
          covar["log10(Species):Terr","log10(Species):Terr"] 
          +2*covar["log10(Species):Terr","log10(Species)"]
          +2*lat*covar["log10(Species):Terr","log10(Species):Latitude"])
        terr_upper <- terr_m + 1.64*terr_se
        terr_lower <- terr_m - 1.64*terr_se

        marine_m <- marginal + betas["log10(Species):Marine"]
        marine_se <- sqrt(se^2 +
          covar["log10(Species):Marine","log10(Species):Marine"] 
          +2*covar["log10(Species):Marine","log10(Species)"]
          +2*lat*covar["log10(Species):Marine","log10(Species):Latitude"])
        marine_upper <- marine_m + 1.64*marine_se
        marine_lower <- marine_m - 1.64*marine_se

        lake_m <- marginal + lat*betas["log10(Species):Lakeweb:Latitude"] + betas["log10(Species):Lakeweb"]
        lake_se <- sqrt(se^2 + 
          lat^2*covar["log10(Species):Lakeweb:Latitude","log10(Species):Lakeweb:Latitude"] 
          +2*lat*covar["log10(Species):Lakeweb:Latitude","log10(Species)"]
          +2*lat^2*covar["log10(Species):Lakeweb:Latitude","log10(Species):Latitude"]
          +covar["log10(Species):Lakeweb","log10(Species):Lakeweb"] 
          +2*covar["log10(Species):Lakeweb","log10(Species)"]
          +2*lat*covar["log10(Species):Lakeweb","log10(Species):Latitude"])

        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        stream_m <- marginal + lat*betas["log10(Species):Stream:Latitude"] + betas["log10(Species):Stream"]
        stream_se <- sqrt(se^2 +
          lat^2*covar["log10(Species):Stream:Latitude","log10(Species):Stream:Latitude"] 
          +2*lat*covar["log10(Species):Stream:Latitude","log10(Species)"]
          +2*lat^2*covar["log10(Species):Stream:Latitude","log10(Species):Latitude"]
          +covar["log10(Species):Stream","log10(Species):Stream"] 
          +2*covar["log10(Species):Stream","log10(Species)"]
          +2*lat*covar["log10(Species):Stream","log10(Species):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(lat,marginal,upper,lower,terr_m,terr_upper,terr_lower,marine_m,marine_upper,marine_lower,
        lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Latitude","Marginal","Upper","Lower","Terrestrial","Terr_upper","Terr_lower","Marine","Marine_upper","Marine_lower",
        "Lakeweb","Lakeweb_upper","Lakeweb_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

# # These are for within-TL stuff. Not updated.
# B_CIs <- function(model){
#       modname=eval(as.name(model))
#       betas=summary(modname)$coefficients[,1]
#       covar=vcov(modname)
    
#       lat=seq(0,90,length.out=200)
#       marginal <- betas["log10(Basal)"]+lat*betas["log10(Basal):Latitude"]

#       se <- sqrt(covar["log10(Basal)","log10(Basal)"]+2*lat*covar["log10(Basal):Latitude","log10(Basal)"]
#         +lat^2*covar["log10(Basal):Latitude","log10(Basal):Latitude"])

#       upper <- marginal + 1.64*se
#       lower <- marginal - 1.64*se

#       stream_m <- marginal + betas["log10(Basal):Stream"]
#       stream_se <- sqrt(se^2 +
#         covar["log10(Basal):Stream","log10(Basal):Stream"] 
#         +2*covar["log10(Basal):Stream","log10(Basal)"]
#         +2*lat*covar["log10(Basal):Stream","log10(Basal):Latitude"])
#       stream_upper <- stream_m + 1.64*stream_se
#       stream_lower <- stream_m - 1.64*stream_se

#       marine_m <- marginal + lat*betas["log10(Basal):Latitude:Marine"] + betas["log10(Basal):Marine"]
#       marine_se <- sqrt(se^2 + 
#         lat^2*covar["log10(Basal):Latitude:Marine","log10(Basal):Latitude:Marine"] 
#         +2*lat*covar["log10(Basal):Latitude:Marine","log10(Basal)"]
#         +2*lat^2*covar["log10(Basal):Latitude:Marine","log10(Basal):Latitude"]
#         +covar["log10(Basal):Marine","log10(Basal):Marine"] 
#         +2*covar["log10(Basal):Marine","log10(Basal)"]
#         +2*lat*covar["log10(Basal):Marine","log10(Basal):Latitude"])

#       marine_upper <- marine_m + 1.64*marine_se
#       marine_lower <- marine_m - 1.64*marine_se

#       terr_m <- marginal + lat*betas["log10(Basal):Latitude:Terr"] + betas["log10(Basal):Terr"]
#       terr_se <- sqrt(se^2 + 
#         lat^2*covar["log10(Basal):Latitude:Terr","log10(Basal):Latitude:Terr"] 
#         +2*lat*covar["log10(Basal):Latitude:Terr","log10(Basal)"]
#         +2*lat^2*covar["log10(Basal):Latitude:Terr","log10(Basal):Latitude"]
#         +covar["log10(Basal):Terr","log10(Basal):Terr"] 
#         +2*covar["log10(Basal):Terr","log10(Basal)"]
#         +2*lat*covar["log10(Basal):Terr","log10(Basal):Latitude"])

#       terr_upper <- terr_m + 1.64*terr_se
#       terr_lower <- terr_m - 1.64*terr_se

#       # Marine, Terr (Stream)
#       results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,marine_m,marine_upper,marine_lower,terr_m,terr_upper,terr_lower)
#       colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
#                           "Marine","Marine_upper","Marine_lower","Terrestrial","Terr_upper","Terr_lower")
#       return(results)
#     }

# I_CIs <- function(model){
#   modname=eval(as.name(model))
#   betas=summary(modname)$coefficients[,1]
#   covar=vcov(modname)

#   lat=seq(0,90,length.out=200)

#   if(model=="LS_I_min" | model=="Vul_I_min"){

#     marginal <- betas["log10(Intermediate)"]

#     se <- sqrt(covar["log10(Intermediate)","log10(Intermediate)"])

#     upper <- marginal + 1.64*se
#     lower <- marginal - 1.64*se

#     stream_m <- marginal + betas["log10(Intermediate):Stream"]
#     stream_se <- sqrt(se^2 +
#       covar["log10(Intermediate):Stream","log10(Intermediate):Stream"] 
#       +2*covar["log10(Intermediate):Stream","log10(Intermediate)"]        )

#     stream_upper <- stream_m + 1.64*stream_se
#     stream_lower <- stream_m - 1.64*stream_se
#       results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower)
#       colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower")        
#   } else {
#     marginal <- betas["log10(Intermediate)"]+lat*betas["log10(Intermediate):Latitude"]

#     se <- sqrt(covar["log10(Intermediate)","log10(Intermediate)"]
#       +2*lat*covar["log10(Intermediate)","log10(Intermediate):Latitude"]
#       +lat^2*covar["log10(Intermediate):Latitude","log10(Intermediate):Latitude"])

#     upper <- marginal + 1.64*se
#     lower <- marginal - 1.64*se

#     stream_m <- marginal + betas["log10(Intermediate):Stream"] +  lat*betas["log10(Intermediate):Latitude:Stream"]
#     stream_se <- sqrt(se^2 + 
#       +covar["log10(Intermediate):Stream","log10(Intermediate):Stream"] 
#       +2*covar["log10(Intermediate):Stream","log10(Intermediate)"]
#       +2*lat*covar["log10(Intermediate):Stream","log10(Intermediate):Latitude"]
#       +2*lat*covar["log10(Intermediate):Stream","log10(Intermediate):Latitude:Stream"]
#       +2*lat^2*covar["log10(Intermediate):Latitude:Stream","log10(Intermediate):Latitude"]
#       +2*lat*covar["log10(Intermediate):Latitude:Stream","log10(Intermediate)"]
#       +lat^2*covar["log10(Intermediate):Latitude:Stream","log10(Intermediate):Latitude:Stream"]
#       )

#     stream_upper <- stream_m + 1.64*stream_se
#     stream_lower <- stream_m - 1.64*stream_se

#     lake_m <- marginal + betas["log10(Intermediate):Lakeweb"] +  lat*betas["log10(Intermediate):Latitude:Lakeweb"]
#     lake_se <- sqrt(se^2 + 
#       +covar["log10(Intermediate):Lakeweb","log10(Intermediate):Lakeweb"] 
#       +2*covar["log10(Intermediate):Lakeweb","log10(Intermediate)"]
#       +2*lat*covar["log10(Intermediate):Lakeweb","log10(Intermediate):Latitude"]
#       +2*lat*covar["log10(Intermediate):Lakeweb","log10(Intermediate):Latitude:Lakeweb"]
#       +2*lat^2*covar["log10(Intermediate):Latitude:Lakeweb","log10(Intermediate):Latitude"]
#       +2*lat*covar["log10(Intermediate):Latitude:Lakeweb","log10(Intermediate)"]
#       +lat^2*covar["log10(Intermediate):Latitude:Lakeweb","log10(Intermediate):Latitude:Lakeweb"]
#       )

#     lake_upper <- lake_m + 1.64*lake_se
#     lake_lower <- lake_m - 1.64*lake_se

#     results <- cbind(lat,marginal,upper,lower,stream_m,stream_upper,stream_lower,
#       lake_m,lake_upper,lake_lower)
#     colnames(results)=c("Latitude","Marginal","Upper","Lower","Stream","Stream_upper","Stream_lower",
#       "Lakeweb","Lakeweb_upper","Lakeweb_lower")      }

#   return(results)
#   }

# T_CIs <- function(model){
#       modname=eval(as.name(model))
#       betas=summary(modname)$coefficients[,1]
#       covar=vcov(modname)
    
#       lat=seq(0,90,length.out=200)

#       if(model=="LS_T_min" | model=="Vul_T_min"){
#         marginal <- betas["log10(Toppreds)"]+lat*betas["log10(Toppreds):Latitude"]

#         se <- sqrt(covar["log10(Toppreds)","log10(Toppreds)"]+2*lat*covar["log10(Toppreds):Latitude","log10(Toppreds)"]
#           +lat^2*covar["log10(Toppreds):Latitude","log10(Toppreds):Latitude"])

#         upper <- marginal + 1.64*se
#         lower <- marginal - 1.64*se

#         marine_m <- marginal + betas["log10(Toppreds):Marine"]
#         marine_se <- sqrt(se^2 +
#           covar["log10(Toppreds):Marine","log10(Toppreds):Marine"] 
#           +2*covar["log10(Toppreds):Marine","log10(Toppreds)"]
#           +2*lat*covar["log10(Toppreds):Marine","log10(Toppreds):Latitude"])
#         marine_upper <- marine_m + 1.64*marine_se
#         marine_lower <- marine_m - 1.64*marine_se

#         lake_m <- marginal + betas["log10(Toppreds):Lakeweb"]
#         lake_se <- sqrt(se^2 +covar["log10(Toppreds):Lakeweb","log10(Toppreds):Lakeweb"] 
#           +2*covar["log10(Toppreds):Lakeweb","log10(Toppreds)"]
#           +2*lat*covar["log10(Toppreds):Lakeweb","log10(Toppreds):Latitude"])

#         lake_upper <- lake_m + 1.64*lake_se
#         lake_lower <- lake_m - 1.64*lake_se

#         stream_m <- marginal + lat*betas["log10(Toppreds):Stream:Latitude"] + betas["log10(Toppreds):Stream"]
#         stream_se <- sqrt(se^2 +
#           lat^2*covar["log10(Toppreds):Stream:Latitude","log10(Toppreds):Stream:Latitude"] 
#           +2*lat*covar["log10(Toppreds):Stream:Latitude","log10(Toppreds)"]
#           +2*lat^2*covar["log10(Toppreds):Stream:Latitude","log10(Toppreds):Latitude"]
#           +covar["log10(Toppreds):Stream","log10(Toppreds):Stream"] 
#           +2*covar["log10(Toppreds):Stream","log10(Toppreds)"]
#           +2*lat*covar["log10(Toppreds):Stream","log10(Toppreds):Latitude"])
#         stream_upper <- stream_m + 1.64*stream_se
#         stream_lower <- stream_m - 1.64*stream_se

#         terr_m <- marginal + betas["log10(Toppreds):Terr"] + lat*betas["log10(Toppreds):Terr:Latitude"]
#         terr_se <- sqrt(se^2 +
#           lat^2*covar["log10(Toppreds):Terr:Latitude","log10(Toppreds):Terr:Latitude"] 
#           +2*lat*covar["log10(Toppreds):Terr:Latitude","log10(Toppreds)"]
#           +2*lat^2*covar["log10(Toppreds):Terr:Latitude","log10(Toppreds):Latitude"]
#           +covar["log10(Toppreds):Terr","log10(Toppreds):Terr"] 
#           +2*covar["log10(Toppreds):Terr","log10(Toppreds)"]
#           +2*lat*covar["log10(Toppreds):Terr","log10(Toppreds):Latitude"])

#         terr_upper <- terr_m + 1.64*terr_se
#         terr_lower <- terr_m - 1.64*terr_se

#         results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,lake_m,lake_upper,lake_lower,
#           stream_m,stream_upper,stream_lower,terr_m,terr_upper,terr_lower)
#         colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower","Lakeweb","Lakeweb_upper","Lakeweb_lower",
#           "Stream","Stream_upper","Stream_lower","Terrestrial","Terr_upper","Terr_lower")
#       }

#       if(model=="Gen_T_min"){        
#         marginal <- betas["log10(Toppreds)"]

#         se <- sqrt(covar["log10(Toppreds)","log10(Toppreds)"])

#         upper <- marginal + 1.64*se
#         lower <- marginal - 1.64*se

#         marine_m <- marginal + betas["log10(Toppreds):Marine"]
#         marine_se <- sqrt(se^2 +
#           covar["log10(Toppreds):Marine","log10(Toppreds):Marine"] 
#           +2*covar["log10(Toppreds):Marine","log10(Toppreds)"])
#         marine_upper <- marine_m + 1.64*marine_se
#         marine_lower <- marine_m - 1.64*marine_se

#         stream_m <- marginal + betas["log10(Toppreds):Stream"]
#         stream_se <- sqrt(se^2 +
#           +covar["log10(Toppreds):Stream","log10(Toppreds):Stream"] 
#           +2*covar["log10(Toppreds):Stream","log10(Toppreds)"])
#         stream_upper <- stream_m + 1.64*stream_se
#         stream_lower <- stream_m - 1.64*stream_se

#         lake_m <- marginal + betas["log10(Toppreds):Lakeweb"]
#         lake_se <- sqrt(se^2 +covar["log10(Toppreds):Lakeweb","log10(Toppreds):Lakeweb"] 
#           +2*covar["log10(Toppreds):Lakeweb","log10(Toppreds)"])

#         lake_upper <- lake_m + 1.64*lake_se
#         lake_lower <- lake_m - 1.64*lake_se

#       results <- cbind(lat,marginal,upper,lower,marine_m,marine_upper,marine_lower,
#         lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
#       colnames(results)=c("Latitude","Marginal","Upper","Lower","Marine","Marine_upper","Marine_lower",
#         "Lakeweb","Lakeweb_upper","Lakeweb_lower","Stream","Stream_upper","Stream_lower")
#       }
#       return(results)
#     }
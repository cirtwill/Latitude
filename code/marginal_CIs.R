S_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
    
      s0=seq(1,200,length.out=500)
      # logged data in the modnames, logged data in the plots. No scaling shenanigans
      logs0 <- log(s0)
      marginal <- logs0*betas["log(Species):Latitude"]
      estimate <- exp(marginal)

      lat_effect <- function(data,ind){
        d <- data[ind,] # Is this the sampling step?

        if(model=='LS_min'){
          LS_m <- with(d,lm(log(LS)~log(Species)+log(Species):(Lake+Terr)
              +log(Species):Latitude+log(Species):Latitude:Lake,na.action=na.fail))
          latbeta=summary(LS_m)$coefficients["log(Species):Latitude",1]
          lakebeta=summary(LS_m)$coefficients["log(Species):Lake:Latitude",1]

          results=c(latbeta,lakebeta)
          results    }
        if(model=='Gen_min'){
          Gen_m <- with(d,lm(log(Gen)~log(Species)
          +log(Species):(Lake+Stream+Terr)+log(Species):Latitude
          +log(Species):Latitude:(Lake+Stream),na.action=na.fail))
          latbeta=summary(Gen_m)$coefficients[6,1]
          lakebeta=summary(Gen_m)$coefficients[7,1]
          streambeta=summary(Gen_m)$coefficients[8,1]

          results=c(latbeta,lakebeta,streambeta)
          results    }
        if(model=='Vul_min'){
          Vul_m <- with(d,lm(log(Vul)~log(Species)
          +log(Species):(Lake+Terr)+log(Species):Latitude
          +log(Species):Latitude:Lake,na.action=na.fail))
          latbeta=summary(Vul_m)$coefficients[5,1]
          lakebeta=summary(Vul_m)$coefficients[6,1]

          results=c(latbeta,lakebeta)
          results    }
        }

      bootstraps=boot(data,lat_effect,R=10)
#      samples=bootstraps$t
#      latlist=samples[,1]
 #     lakelist=samples[,2]
  #    lat95=sort(latlist)[26:975]
   #   lake95=sort(lakelist)[26:975]

    #  CIs=cbind(lat95,lake95)

     # if(model=="Gen_min"){
      #  streamlist=bootstraps$t[,3]
       # stream95=sort(streamlist)[26:975]
        #CIs=cbind(lat95,lake95,stream95)
      #}

      return(bootstraps)
    }

B_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
      modname
    
      s0=seq(1,250,length.out=500)
      # logged data in the modnames, logged data in the plots. No scaling shenanigans
      logs0 <- log(s0)
      marginal <- logs0*betas["log(Basal):Latitude"]
      se <- sqrt(logs0^2*covar["log(Basal):Latitude","log(Basal):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      results <- cbind(s0,marginal,upper,lower)
      colnames(results)=c("Weight","Marginal","Upper","Lower")

      if(model=="LS_min" | model=="Vul_min"){
        lake_m <- marginal + betas["log(Basal):Lake:Latitude"]
        lake_se <- sqrt(se^2 + logs0^2*covar["log(Basal):Lake:Latitude","log(Basal):Lake:Latitude"] 
          + 2*logs0^2*covar["log(Basal):Lake:Latitude","log(Basal):Latitude"])
        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

      results <- cbind(s0,marginal,upper,lower,lake_m,lake_upper,lake_lower)
      colnames(results)=c("Basal","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower")
      }

      if(model=="Gen_min"){
        lake_m <- marginal + betas["log(Basal):Lake:Latitude"]
        lake_se <- sqrt(se^2 + logs0^2*covar["log(Basal):Lake:Latitude","log(Basal):Lake:Latitude"] 
          + 2*logs0^2*covar["log(Basal):Lake:Latitude","log(Basal):Latitude"])
        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        stream_m <- marginal + betas["log(Basal):Stream:Latitude"]
        stream_se <- sqrt(se^2 + logs0^2*covar["log(Basal):Stream:Latitude","log(Basal):Stream:Latitude"] 
          + 2*logs0^2*covar["log(Basal):Stream:Latitude","log(Basal):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(s0,marginal,upper,lower,lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Basal","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

I_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
      modname
    
      s0=seq(1,250,length.out=500)
      # logged data in the modnames, logged data in the plots. No scaling shenanigans
      logs0 <- log(s0)
      marginal <- logs0*betas["log(Intermediate):Latitude"]
      se <- sqrt(logs0^2*covar["log(Intermediate):Latitude","log(Intermediate):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      results <- cbind(s0,marginal,upper,lower)
      colnames(results)=c("Weight","Marginal","Upper","Lower")

      if(model=="LS_min" | model=="Vul_min"){
        lake_m <- marginal + betas["log(Intermediate):Lake:Latitude"]
        lake_se <- sqrt(se^2 + logs0^2*covar["log(Intermediate):Lake:Latitude","log(Intermediate):Lake:Latitude"] 
          + 2*logs0^2*covar["log(Intermediate):Lake:Latitude","log(Intermediate):Latitude"])
        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

      results <- cbind(s0,marginal,upper,lower,lake_m,lake_upper,lake_lower)
      colnames(results)=c("Intermediate","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower")
      }

      if(model=="Gen_min"){
        lake_m <- marginal + betas["log(Intermediate):Lake:Latitude"]
        lake_se <- sqrt(se^2 + logs0^2*covar["log(Intermediate):Lake:Latitude","log(Intermediate):Lake:Latitude"] 
          + 2*logs0^2*covar["log(Intermediate):Lake:Latitude","log(Intermediate):Latitude"])
        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        stream_m <- marginal + betas["log(Intermediate):Stream:Latitude"]
        stream_se <- sqrt(se^2 + logs0^2*covar["log(Intermediate):Stream:Latitude","log(Intermediate):Stream:Latitude"] 
          + 2*logs0^2*covar["log(Intermediate):Stream:Latitude","log(Intermediate):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(s0,marginal,upper,lower,lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Intermediate","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }

T_CIs <- function(model){
      modname=eval(as.name(model))
      betas=summary(modname)$coefficients[,1]
      covar=vcov(modname)
      modname
    
      s0=seq(1,250,length.out=500)
      # logged data in the modnames, logged data in the plots. No scaling shenanigans
      logs0 <- log(s0)
      marginal <- logs0*betas["log(Toppreds):Latitude"]
      se <- sqrt(logs0^2*covar["log(Toppreds):Latitude","log(Toppreds):Latitude"])

      upper <- marginal + 1.64*se
      lower <- marginal - 1.64*se

      results <- cbind(s0,marginal,upper,lower)
      colnames(results)=c("Weight","Marginal","Upper","Lower")

      if(model=="LS_min" | model=="Vul_min"){
        lake_m <- marginal + betas["log(Toppreds):Lake:Latitude"]
        lake_se <- sqrt(se^2 + logs0^2*covar["log(Toppreds):Lake:Latitude","log(Toppreds):Lake:Latitude"] 
          + 2*logs0^2*covar["log(Toppreds):Lake:Latitude","log(Toppreds):Latitude"])
        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

      results <- cbind(s0,marginal,upper,lower,lake_m,lake_upper,lake_lower)
      colnames(results)=c("Toppreds","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower")
      }

      if(model=="Gen_min"){
        lake_m <- marginal + betas["log(Toppreds):Lake:Latitude"]
        lake_se <- sqrt(se^2 + logs0^2*covar["log(Toppreds):Lake:Latitude","log(Toppreds):Lake:Latitude"] 
          + 2*logs0^2*covar["log(Toppreds):Lake:Latitude","log(Toppreds):Latitude"])
        lake_upper <- lake_m + 1.64*lake_se
        lake_lower <- lake_m - 1.64*lake_se

        stream_m <- marginal + betas["log(Toppreds):Stream:Latitude"]
        stream_se <- sqrt(se^2 + logs0^2*covar["log(Toppreds):Stream:Latitude","log(Toppreds):Stream:Latitude"] 
          + 2*logs0^2*covar["log(Toppreds):Stream:Latitude","log(Toppreds):Latitude"])
        stream_upper <- stream_m + 1.64*stream_se
        stream_lower <- stream_m - 1.64*stream_se

      results <- cbind(s0,marginal,upper,lower,lake_m,lake_upper,lake_lower,stream_m,stream_upper,stream_lower)
      colnames(results)=c("Top","Marginal","Upper","Lower","Lake","Lake_upper","Lake_lower","Stream","Stream_upper","Stream_lower")
      }
      return(results)
    }
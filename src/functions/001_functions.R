# Visually check data ----------------------------------------------------------
visCheck = function(datapath, polygonfile, band = 109){
  ds = list.files(datapath, full.names = TRUE)
  pb = shapefile(polygonfile)
  
  reproj = TRUE
  for(d in ds){
    r = readRDS(d)
    if(reproj){
      pb = spTransform(pb, projection(r))
      reproj = FALSE
    }
    plot(r[[band]], main = substr(basename(d), 1, 4))
    plot(pb[grep(substr(basename(d), 1, 4), pb$PlotID),], add = TRUE)
  }
}



# Compute predictions ----------------------------------------------------------
compPredictions = function(model, input){
  if(inherits(model, "try-error")){
    predictions = NA
  } else {
    non_na_pos = which(
      complete.cases(
        input[, model$selectedvars]))
    
    predictions = NA
    predictions[non_na_pos] = predict(model, input[non_na_pos,])
  }
  return(predictions)
}



# Compile residual datasets ----------------------------------------------------
compResData = function(comb_sr, pt, mt, model_path = path_model_gpm_sr, suf = "_res"){
  comb_sr_elev_res = comb_sr
  model_files = list.files(model_path, full.names = TRUE,
                           pattern = glob2rx(paste0(pt, mt)))
  
  for(m in model_files){
    act_model = readRDS(m)@model[[1]][[1]][[1]]
    
    if(inherits(act_model$model, "try-error")){
      act_predictions = NA
    } else {
      non_na_pos = which(
        complete.cases(
          comb_sr_elev_res@data$input[, act_model$model$selectedvars]))
      
      act_predictions = NA
      act_predictions[non_na_pos] = predict(act_model$model, comb_sr@data$input[non_na_pos,])
    }  
    comb_sr_elev_res@data$input[, act_model$response] = 
      comb_sr_elev_res@data$input[, act_model$response] - 
      act_predictions
    
    colname_pos = grep(act_model$response, colnames(comb_sr_elev_res@data$input))
    colnames(comb_sr_elev_res@data$input)[colname_pos] = 
      paste0(colnames(comb_sr_elev_res@data$input)[colname_pos], 
             gsub("[*]", "", paste0("_", mt, "_", pt, suf)))
  }
  
  comb_sr_elev_res@meta$input$RESPONSE = 
    paste0(comb_sr_elev_res@meta$input$RESPONSE,
           gsub("[*]", "", paste0("_", mt, "_", pt, suf)))
  
  comb_sr_elev_res@meta$input$RESPONSE_FINAL = comb_sr_elev_res@meta$input$RESPONSE
  return(comb_sr_elev_res)
}



# Train and tune models --------------------------------------------------------
compModels = function(model, pt, mt, rs = NULL, outpath, nested_cv = FALSE){
  foreach (i = seq(length(model@meta$input$RESPONSE)), .packages = c("gpm", "caret", "randomForest", "CAST")) %dopar% {
    
    model@meta$input$RESPONSE_FINAL = model@meta$input$RESPONSE[i]
    model@data$input = model@data$input[complete.cases(model@data$input[, c(model@meta$input$RESPONSE_FINAL, model@meta$input$PREDICTOR_FINAL)]), ]
    if(length(model@meta$input$PREDICTOR_FINAL) < 3 | !is.null(rs)){
      var_mode = "none"
    } else {
      var_mode = "ffs"
    }
    if(nrow(model@data$input) > 0){
      model = createIndexFolds(x = model, nested_cv = nested_cv)
      model = trainModel(x = model,
                         metric = "RMSE",
                         n_var = NULL,
                         mthd = mt,
                         mode = var_mode,
                         seed_nbr = 11,
                         cv_nbr = NULL,
                         var_selection = "indv",
                         filepath_tmp = NULL)
    }
    
    if(is.null(rs)){
      outfile_name = gsub("[*]", "", paste0(outpath, 
                                            "ki_sr_", pt, "_non_scaled_", mt, "_", 
                                            model@meta$input$RESPONSE_FINAL,
                                            ".rds"))
    } else {
      outfile_name = gsub("[*]", "", paste0(outpath, 
                                            "ki_sr_", pt, "_non_scaled_", mt, "_", 
                                            model@meta$input$RESPONSE_FINAL,
                                            "_iv.rds"))
    }
    
    print(outfile_name)
    saveRDS(model, file = outfile_name)
  }
}



# Collect model performance ----------------------------------------------------
modelPerformance = function(model){
  smr_all = lapply(names(model), function(pt){
    print(pt)
    smr_pt = lapply(model[[pt]]@model[[1]], function(mi){
      if(is.na(mi[[1]])){
        df = NULL
      } else if(inherits(mi[[1]]$model, "try-error")){
        df = NULL
      } else {
        if(ncol(mi[[1]]$model$resample) == 6){
          temp = rbind(mi[[1]]$model$resample[
            mi[[1]]$model$resample$method == mi[[1]]$model$bestTune$method & 
              mi[[1]]$model$resample$select == mi[[1]]$model$bestTune$select, c(1:3, 6)],
            data.frame(
              t(colMeans(mi[[1]]$model$resample[
                mi[[1]]$model$resample$method == mi[[1]]$model$bestTune$method & 
                  mi[[1]]$model$resample$select == mi[[1]]$model$bestTune$select, 1:3], na.rm = TRUE)),
              Resample = "Mean"))
        } else if(ncol(mi[[1]]$model$resample) == 4) {
          temp = rbind(mi[[1]]$model$resample,
                       data.frame(t(colMeans(mi[[1]]$model$resample[, 1:3], na.rm = TRUE)),
                                  Resample = "Mean"))
          temp$mtry = NA
          
        } else {
          temp = rbind(mi[[1]]$model$resample,
                       data.frame(t(colMeans(mi[[1]]$model$resample[, 1:3], na.rm = TRUE)),
                                  mtry = NA, Resample = "Mean"))
          
        }
        temp$RMSE_normSD =  temp$RMSE/sd(mi[[1]]$model$trainingData$.outcome)
        df = data.frame(mtype = mi[[1]]$model$method,
                        ptype = pt,
                        resp = mi[[1]]$response,
                        mi[[1]]$model$bestTune,
                        nvars = length(mi[[1]]$model$selectedvars),
                        temp)
        # for(i in seq(df$nvars)){
        #   df[paste0("V",i)] = mi[[1]]$model$selectedvars[i]
        # }
      }
    })
    smr_pt = do.call("rbind", smr_pt)
    return(smr_pt)
  })
  smr_all = do.call("rbind", smr_all)
  return(smr_all)
}



# Compile two step prediction datasets -----------------------------------------
comp2StepPred = function(comb_sr_two_step, model, model_res){
  
  smr = lapply(seq(length(model)), function(i){
    mi = model[[i]][[1]]
    mi_res = model_res[[i]][[1]]
    
    mi_pred = compPredictions(mi$model, comb_sr_two_step@data$input)
    mi_res_pred = compPredictions(mi_res$model, comb_sr_two_step@data$input)
    
    rmse_1step = sqrt(mean((comb_sr_two_step@data$input[, mi$response]-(mi_pred))**2, na.rm = TRUE))
    rmse_2step = sqrt(mean((comb_sr_two_step@data$input[, mi$response]-(mi_pred + mi_res_pred))**2, na.rm = TRUE))
    pmean = mean(comb_sr_two_step@data$input[, mi$response], na.rm = TRUE)
    psd = sd(comb_sr_two_step@data$input[, mi$response], na.rm = TRUE)
    rmse_1step_nd = rmse_1step/psd
    rmse_2step_nd = rmse_2step/psd
    
    data.frame(mtype1 = mi$model$method,
               mtype2 = mi_res$model$method,
               resp =  mi$response,
               RMSE1 = rmse_1step,
               RMSE2 = rmse_2step,
               RMSE_normSD1 = rmse_1step_nd,
               RMSE_normSD2 = rmse_2step_nd)
    
  })
  smr = do.call("rbind", smr)
  return(smr)
}


# Spectral rao -----------------------------------------------------------------
######### SPECTRALRAO #############################
## Developed by Matteo Marcantonio
## Latest update: 04th October 2018
## -------------------------------------------------
## Code to calculate Rao's quadratic entropy on a
## numeric matrix, RasterLayer object (or lists)
## using a moving window algorithm. 
## The function also calculates Shannon-Wiener index.
## -------------------------------------------------
## Rao's Q Min = 0, if all pixel classes have
## distance 0. If the chosen distance ranges between
## 0 and 1, Rao's Max = 1-1/S (Simpson Diversity,
## where S is the number of pixel classes).
## -------------------------------------------------
## Find more info and application here: 
## 1) https://doi.org/10.1016/j.ecolind.2016.07.039 Titel anhand dieser DOI in Citavi-Projekt ?bernehmen 
## 2) https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12941 %CITAVIPICKER?10.1111/2041-210X.12941?Titel anhand dieser DOI in Citavi-Projekt ?bernehmen?%
#####################################################
# Function
spectralrao <- function(input, distance_m="euclidean", p=NULL, window=9, mode="classic", lambda=0, shannon=FALSE, rescale=FALSE, na.tolerance=0.0, simplify=3, nc.cores=1, cluster.type="MPI", debugging=FALSE, ...)
{
  #
  ## Load required packages
  #
  require(raster)
  require(svMisc)
  require(proxy)
  #
  ## Define function to check if a number is an integer
  #
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  #
  ## Initial checks
  #
  if( !(is(input,"matrix") | is(input,"SpatialGridDataFrame") | is(input,"RasterLayer") | is(input,"list")) ) {
    stop("\nNot a valid input object.")
  }
  if( is(input,"SpatialGridDataFrame") ) {
    input <- raster(input) # Change input matrix/ces names
  }
  if( is(input,"matrix") | is(input,"RasterLayer")) {
    rasterm<-input
  } else if( is(input,"list") ) {
    rasterm<-input[[1]]
  }
  if(na.tolerance>1){
    stop("na.tolerance must be in the [0-1] interval. Exiting...")
  }
  # Deal with matrices and RasterLayer in a different way
  # If data are raster layers
  if( is(input[[1]],"RasterLayer") ) {
    if( mode=="classic" ){
      isfloat<-FALSE # If data are float numbers, transform them in integer
      if( !is.wholenumber(rasterm@data@min) | !is.wholenumber(rasterm@data@max) | is.infinite(rasterm@data@min) ){
        message("Converting input data in an integer matrix...")
        isfloat<-TRUE
        mfactor<-100^simplify
        rasterm<-getValues(rasterm)*mfactor
        gc()
        rasterm<-as.integer(rasterm)
        gc()
        rasterm<-matrix(rasterm,nrow(input),ncol(input),byrow=TRUE)
        gc()
      }else{
        rasterm<-matrix(getValues(rasterm),ncol=ncol(input),nrow=nrow(input),byrow=T)
      }
    }
    #Print user messages
    if( mode=="classic" & shannon ){
      message("Matrix check OK: \nRao and Shannon output matrices will be returned")
    }else if( mode=="classic" & !shannon ){
      message("Matrix check OK: \nRao output matrix will be returned")
    }else if( mode=="multidimension" & !shannon ){
      message(("Matrix check OK: \nA matrix with multimension RaoQ will be returned"))
    }else if( mode=="multidimension" & shannon ){
      stop("Matrix check failed: \nMultidimension and Shannon not compatible, set shannon=FALSE")
    }else{
      stop("Matrix check failed: \nNot a valid input | method | distance, please check all these options...")
    }
    # If data are a matrix or a list
  }else if( is(input,"matrix") | is(input,"list") ) {
    if( mode=="classic" ){ 
      isfloat<-FALSE # If data are float numbers, transform them in integer
      if( !is.integer(rasterm) ){
        message("Converting input data in an integer matrix...")
        isfloat<-TRUE
        mfactor<-100^simplify
        rasterm<-as.integer(rasterm*mfactor)
        rasterm<-matrix(rasterm,nrow(input),ncol(input),byrow=TRUE)
        gc()
      }else{
        rasterm<-as.matrix(rasterm)
      }
    }
    if( mode=="classic" & shannon ){
      message("Matrix check OK: \nRao and Shannon output matrices will be returned")
    }else if( mode=="classic" & !shannon ){
      message("Matrix check OK: \nRao output matrix will be returned")
    }else if( mode=="multidimension" & shannon ){
      stop("Matrix check failed: \nMultidimension and Shannon not compatible, set shannon=FALSE")
    }else if( mode=="multidimension" & !shannon ){
      message(("Matrix check OK: \nA matrix with multimension RaoQ will be returned"))
    }else{
      stop("Matrix check failed: \nNot a valid input | method | distance, please check all these options")
    }
  }
  
  if(nc.cores>1) {
    if(mode=="multidimension"){
      message(
        "Multi-core is not supported for multidimensional Rao, proceding with 1 core...")
      nc.cores=1
    }else{
      message("
              ##################### Starting parallel calculation #######################")
    }
  }
  #
  ## Derive operational moving window
  #
  if( window%%2==1 ){
    w <- (window-1)/2
  } else {
    stop("Moving window size must be an odd number.")
  }
  #
  ## Preparation of output matrices
  #
  if(nc.cores==1) {
    raoqe<-matrix(rep(NA,dim(rasterm)[1]*dim(rasterm)[2]),nrow=dim(rasterm)[1],ncol=dim(rasterm)[2])
  }
  if(shannon){
    shannond<-matrix(rep(NA,dim(rasterm)[1]*dim(rasterm)[2]),nrow=dim(rasterm)[1],ncol=dim(rasterm)[2])
  }
  #
  ## If mode is classic Rao
  #
  if(mode=="classic") {
    #
    # If classic RaoQ is parallelized
    #
    if(nc.cores>1) {
      #
      ## Required packages for parallel calculation
      #
      require(foreach)
      require(doSNOW)
      require(parallel)
      if( cluster.type=="MPI" ){
        require(Rmpi)
      }
      #
      ## Reshape values
      #
      values<-as.numeric(as.factor(rasterm))
      rasterm_1<-matrix(data=values,nrow=dim(rasterm)[1],ncol=dim(rasterm)[2])
      #
      ## Add fake columns and rows for moving window
      #
      hor<-matrix(NA,ncol=dim(rasterm)[2],nrow=w)
      ver<-matrix(NA,ncol=w,nrow=dim(rasterm)[1]+w*2)
      trasterm<-cbind(ver,rbind(hor,rasterm_1,hor),ver)
      rm(hor,ver,rasterm_1,values); gc()
      if(debugging){cat("#check: RaoQ parallel function.")}
      #       
      ## Derive distance matrix
      #
      d1<-proxy::dist(as.numeric(levels(as.factor(rasterm))),method=distance_m)
      gc()
      #
      ## Export variables in the global environment
      #
      if(isfloat) {
        sapply(c("mfactor"), function(x) {assign(x,get(x),envir= .GlobalEnv)})
      }
      #       
      ## Create cluster object with given number of slaves
      #
      plr<<-TRUE
      if( cluster.type=="SOCK" || cluster.type=="FORK" ) {
        cls <- parallel::makeCluster(nc.cores,type=cluster.type, outfile="",useXDR=FALSE,methods=FALSE,output="")
      } else if( cluster.type=="MPI" ) {
        cls <- makeMPIcluster(nc.cores,outfile="",useXDR=FALSE,methods=FALSE,output="")
      }
      registerDoSNOW(cls)
      clusterCall(cl=cls, function() library("parallel"))
      if(isfloat) {
        parallel::clusterExport(cl=cls, varlist=c("mfactor"))
      }
      on.exit(stopCluster(cls)) # Close the clusters on exit
      gc()
      #
      ## Start the parallelized loop over iter
      #
      pb <- txtProgressBar(min = (1+w), max = dim(rasterm)[2], style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      raop <- foreach(cl=(1+w):(dim(rasterm)[2]+w),.options.snow = opts,.verbose = F) %dopar% {
        if(debugging) {
          cat(paste(cl))
        }
        raout <- sapply((1+w):(dim(rasterm)[1]+w), function(rw) {
          if( length(!which(!trasterm[c(rw-w):c(rw+w),c(cl-w):c(cl+w)]%in%NA)) < window^2-((window^2)*na.tolerance) ) {
            vv<-NA
            return(vv)
          } 
          else {
            tw<-summary(as.factor(trasterm[c(rw-w):c(rw+w),c(cl-w):c(cl+w)]),maxsum=10000)
            if( "NA's"%in%names(tw) ) {
              tw<-tw[-length(tw)]
            }
            if( debugging ) {
              message("Working on coords ",rw,",",cl,". classes length: ",length(tw),". window size=",window)
            }
            tw_labels<-names(tw)
            tw_values<-as.vector(tw)
            if( length(tw_values) <=2 ) {
              vv<-NA
              return(vv)
            }
            else {
              p <- tw_values/sum(tw_values)
              p1 <- diag(0,length(tw_values))
              p1[upper.tri(p1)] <- c(combn(p,m=2,FUN=prod))
              p1[lower.tri(p1)] <- c(combn(p,m=2,FUN=prod))
              d2 <- unname(as.matrix(d1)[as.numeric(tw_labels),as.numeric(tw_labels)])
              vv <- sum(p1*d2)
              return(vv)
            }
          }
        })
        return(raout)
      } # End classic RaoQ - parallelized
      message(("\n\nCalculation of Rao's index complete.\n"))
      #
      ## If classic RaoQ is sequential
      #
    } else if(nc.cores==1) {
      # Reshape values
      values<-as.numeric(as.factor(rasterm))
      rasterm_1<-matrix(data=values,nrow=dim(rasterm)[1],ncol=dim(rasterm)[2])
      # Add fake columns and rows for moving window
      hor<-matrix(NA,ncol=dim(rasterm)[2],nrow=w)
      ver<-matrix(NA,ncol=w,nrow=dim(rasterm)[1]+w*2)
      trasterm<-cbind(ver,rbind(hor,rasterm_1,hor),ver)
      # Derive distance matrix
      classes<-levels(as.factor(rasterm))
      d1<-proxy::dist(x=as.numeric(classes),method=distance_m)
      # Loop over each pixel
      for (cl in (1+w):(dim(rasterm)[2]+w)) {
        for(rw in (1+w):(dim(rasterm)[1]+w)) {
          if( length(!which(!trasterm[c(rw-w):c(rw+w),c(cl-w):c(cl+w)]%in%NA)) < window^2-((window^2)*na.tolerance) ) {
            raoqe[rw-w,cl-w]<-NA
          } else {
            tw<-summary(as.factor(trasterm[c(rw-w):c(rw+w),c(cl-w):c(cl+w)]),maxsum=10000)
            if( "NA's"%in%names(tw) ) {
              tw<-tw[-length(tw)]
            }
            if(debugging) {
              message("Working on coords ",rw ,",",cl,". classes length: ",length(tw),". window size=",window)
            }
            tw_labels<-names(tw)
            tw_values<-as.vector(tw)
            if(length(tw_values) <= 2) {
              raoqe[rw-w,cl-w]<-NA
            } else {
              p <- tw_values/sum(tw_values)
              p1 <- diag(0,length(tw_values))
              p1[upper.tri(p1)] <- c(combn(p,m=2,FUN=prod))
              p1[lower.tri(p1)] <- c(combn(p,m=2,FUN=prod))
              d2 <- unname(as.matrix(d1)[as.numeric(tw_labels),as.numeric(tw_labels)])
              if(isfloat) {
                raoqe[rw-w,cl-w]<-sum(p1*d2)/mfactor
              } else {
                raoqe[rw-w,cl-w]<-sum(p1*d2)
              }
            }
          } 
          progress(value=cl, max.value=c((dim(rasterm)[2]+w)+(dim(rasterm)[1]+w))/2, progress.bar = FALSE)
        } 
      } # End of for loop 
      message(("\nCalculation of Rao's index complete.\n"))
    }
  }  # End classic RaoQ - sequential
  else if( mode=="multidimension" ){
    if(debugging) {
      message("#check: Into multidimensional clause.")
    }
    #----------------------------------------------------#
    #
    ## If multimensional RaoQ
    #
    # Check if there are NAs in the matrices
    if ( is(rasterm,"RasterLayer") ){
      if(any(sapply(lapply(unlist(input),length),is.na)==TRUE))
        message("\n Warning: One or more RasterLayers contain NA which will be threated as 0")
    } else if ( is(rasterm,"matrix") ){
      if(any(sapply(input, is.na)==TRUE) ) {
        message("\n Warning: One or more matrices contain NA which will be threated as 0")
      }
    }
    #
    ## Check whether the chosen distance metric is valid or not
    #
    if( distance_m=="euclidean" | distance_m=="manhattan" | distance_m=="canberra" | distance_m=="minkowski" | distance_m=="mahalanobis" ) {
      #
      ## Define distance functions
      #
      #euclidean
      multieuclidean <- function(x) {
        tmp <- lapply(x, function(y) {
          (y[[1]]-y[[2]])^2
        })
        return(sqrt(Reduce(`+`,tmp)))
      }
      #manhattan
      multimanhattan <- function(x) {
        tmp <- lapply(x, function(y) {
          abs(y[[1]]-y[[2]])
        })
        return(Reduce(`+`,tmp))
      }
      #canberra
      multicanberra <- function(x) {
        tmp <- lapply(x, function(y) {
          abs(y[[1]] - y[[2]]) / (abs(y[[1]]) + abs(y[[2]]))
        })
        return(Reduce(`+`,tmp))
      }
      #minkowski
      multiminkowski <- function(x) {
        tmp <- lapply(x, function(y) {
          abs((y[[1]]-y[[2]])^lambda)
        })
        return(Reduce(`+`,tmp)^(1/lambda))
      }
      #mahalanobis
      multimahalanobis <- function(x){
        tmp <- matrix(unlist(lapply(x,function(y) as.vector(y))),ncol=2)
        tmp <- tmp[!is.na(tmp[,1]),] 
        if( length(tmp)==0 | is.null(dim(tmp)) ) {
          return(NA)
        } else if(rcond(cov(tmp)) <= 0.001) {
          return(NA)
        } else {
          #return the inverse of the covariance matrix of tmp; aka the precision matrix
          inverse<-solve(cov(tmp)) 
          if(debugging){
            print(inverse)
          }
          tmp<-scale(tmp,center=T,scale=F)
          tmp<-as.numeric(t(tmp[1,])%*%inverse%*%tmp[1,])
          return(sqrt(tmp))
        }
      }
      #
      ## Decide what function to use
      #
      if( distance_m=="euclidean") {
        distancef <- get("multieuclidean")
      } else if( distance_m=="manhattan" ) {
        distancef <- get("multimanhattan")
      } else if( distance_m=="canberra" ) {
        distancef <- get("multicanberra")
      } else if( distance_m=="minkowski" ) {
        if( lambda==0 ) {
          stop("The Minkowski Distance for lambda = 0 is Infinity; please choose another value for lambda.")
        } else {
          distancef <- get("multiminkowski") 
        }
      } else if( distance_m=="mahalanobis" ) {
        distancef <- get("multimahalanobis")
        warning("Multimahalanobis distance is not fully supported...")
      }
    } else {
      stop("Distance function not defined for multidimensional Rao's Q; please choose among euclidean, manhattan, canberra, minkowski, mahalanobis!")
    }
    #
    ## Reshape values
    #
    vls<-lapply(input, function(x) {raster::as.matrix(x)})
    #
    ## Rescale and add fake columns and rows for moving w
    #
    hor<-matrix(NA,ncol=dim(vls[[1]])[2],nrow=w)
    ver<-matrix(NA,ncol=w,nrow=dim(vls[[1]])[1]+w*2)
    if(rescale) {
      trastersm<-lapply(vls, function(x) {
        t1 <- raster::scale(raster(cbind(ver,rbind(hor,x,hor),ver)))
        t2 <- as.matrix(t1)
        return(t2)
      })
    } else {
      trastersm<-lapply(vls, function(x) {
        cbind(ver,rbind(hor,x,hor),ver)
      })
    }
    #
    ## Loop over all the pixels in the matrices
    #
    if( (ncol(vls[[1]])*nrow(vls[[1]]))> 10000) {
      message("\n Warning: ",ncol(vls[[1]])*nrow(vls[[1]])*length(vls), " cells to be processed, may take some time... \n")
    }
    
    
    cores = 4
    clp = parallel::makeCluster(cores)
    doParallel::registerDoParallel(clp)
    on.exit(stopCluster(clp))
    
    
    t = foreach (cl = (1+w):(dim(vls[[1]])[2]+w), .combine="rbind", .packages="foreach") %dopar% {
      foreach (rw = (1+w):(dim(vls[[1]])[1]+w), .combine="rbind") %dopar% {
        if( length(!which(!trastersm[[1]][c(rw-w):c(rw+w),c(cl-w):c(cl+w)]%in%NA)) < window^2-((window^2)*na.tolerance) ) {
          # raoqe[rw-w,cl-w] <- NA
          return(data.frame(row=rw-w, col=cl-w, value=NA))
        } else {
          tw<-lapply(trastersm, function(x) { x[(rw-w):(rw+w),(cl-w):(cl+w)]
          })
          #
          ## Vectorize the matrices in the list and calculate
          #Among matrix pairwase distances
          lv <- lapply(tw, function(x) {as.vector(t(x))})
          vcomb <- combn(length(lv[[1]]),2)
          vout <- c()
          for(p in 1:ncol(vcomb) ) {
            lpair <- lapply(lv, function(chi) {
              c(chi[vcomb[1,p]],chi[vcomb[2,p]])
            })
            vout[p] <- distancef(lpair)
          }
          # raoqe[rw-w,cl-w] <- sum(rep(vout,2) * (1/(window)^4),na.rm=TRUE)
          return(data.frame(row=rw-w, col=cl-w, value=sum(rep(vout,2) * (1/(window)^4),na.rm=TRUE)))
        }
        
      }
      
      
      # do.call("rbind", lapply((1+w):(dim(vls[[1]])[1]+w), function(rw){
      #   if( length(!which(!trastersm[[1]][c(rw-w):c(rw+w),c(cl-w):c(cl+w)]%in%NA)) < window^2-((window^2)*na.tolerance) ) {
      #     # raoqe[rw-w,cl-w] <- NA
      #     return(data.frame(row=rw-w, col=cl-w, value=NA))
      #   } else {
      #     tw<-lapply(trastersm, function(x) { x[(rw-w):(rw+w),(cl-w):(cl+w)]
      #     })
      #     #
      #     ## Vectorize the matrices in the list and calculate
      #     #Among matrix pairwase distances
      #     lv <- lapply(tw, function(x) {as.vector(t(x))})
      #     vcomb <- combn(length(lv[[1]]),2)
      #     vout <- c()
      #     for(p in 1:ncol(vcomb) ) {
      #       lpair <- lapply(lv, function(chi) {
      #         c(chi[vcomb[1,p]],chi[vcomb[2,p]])
      #       })
      #       vout[p] <- distancef(lpair)
      #     }
      #     # raoqe[rw-w,cl-w] <- sum(rep(vout,2) * (1/(window)^4),na.rm=TRUE)
      #     return(data.frame(row=rw-w, col=cl-w, value=sum(rep(vout,2) * (1/(window)^4),na.rm=TRUE)))
      #   }
      # }))
      
      
      
      # progress(value=cl, max.value=dim(rasterm)[2]+w, progress.bar = FALSE)
    }
    raoqe<-matrix(rep(NA,dim(rasterm)[1]*dim(rasterm)[2]),nrow=dim(rasterm)[1],ncol=dim(rasterm)[2])
    for(i in seq(nrow(t))){
      raoqe[t[i,"row"], t[i,"col"]] = t[i,"value"]
    }
    
    if(exists("pb")) {
      close(pb) 
    }
  } else{
    message("Something went wrong when trying to calculate Rao's indiex.")
  }  # end of multimensional RaoQ
  message("\nCalculation of Multidimensional Rao's index complete.\n")
  
  #----------------------------------------------------#
  
  #
  ## Shannon
  #
  if( shannon==T ) {
    message("\nStarting Shannon-Wiener index calculation:\n")
    # Reshape values
    values<-as.numeric(as.factor(rasterm))
    rasterm_1<-matrix(data=values,nrow=dim(rasterm)[1],ncol=dim(rasterm)[2])
    #
    ## Add "fake" columns and rows for moving window
    #
    hor<-matrix(NA,ncol=dim(rasterm)[2],nrow=w)
    ver<-matrix(NA,ncol=w,nrow=dim(rasterm)[1]+w*2)
    trasterm<-cbind(ver,rbind(hor,rasterm_1,hor),ver)
    #
    ## Loop over all the pixels
    #
    for (cl in (1+w):(dim(rasterm)[2]+w)) {
      for(rw in (1+w):(dim(rasterm)[1]+w)) {
        if( length(!which(!trasterm[c(rw-w):c(rw+w),c(cl-w):c(cl+w)]%in%NA)) < window^2-((window^2)*na.tolerance) ) {
          shannond[rw-w,cl-w]<-NA
        } else {
          tw<-summary(as.factor(trasterm[c(rw-w):c(rw+w),c(cl-w):c(cl+w)]))
          if( "NA's"%in%names(tw) ) {
            tw<-tw[-length(tw)]
          }
          tw[tw>1]<-1
          tw_values<-as.vector(tw)
          p<-tw_values/length(tw_values)
          p_log<-log(p)
          shannond[rw-w,cl-w]<-(-(sum(p*p_log)))
        }
      }   
      svMisc::progress(value=cl, max.value=(c((dim(rasterm)[2]+w)+(dim(rasterm)[1]+w))/2), progress.bar = FALSE)
    } 
    message(("\nCalculation of Shannon's index is also complete!\n"))
  } # End ShannonD
  
  #----------------------------------------------------#
  
  #
  ## Return multiple outputs
  #
  if(debugging){
    message( "#check: return function." )
  }
  
  if( shannon ) {
    if( nc.cores>1 ) {
      outl<-list(do.call(cbind,raop),shannond)
      names(outl)<-c("Rao","Shannon")
      return(outl)
    } else if( nc.cores==1 ){ 
      outl<-list(raoqe,shannond)
      names(outl)<-c("Rao","Shannon")
      return(outl)
    }
  } else if( !shannon & mode=="classic" ) {
    if( isfloat & nc.cores>1 ) {
      #return(raop)
      return(do.call(cbind,raop)/mfactor)
      if(debugging){
        message("#check: return function - classic.")
      }
    } else if( !isfloat & nc.cores>1 ) {
      outl<-list(do.call(cbind,raop))
      names(outl)<-c("Rao")
      return(outl)
    } else if( sfloat & nc.cores==1 ) {
      outl<-list(raoqe/mfactor)
      names(outl)<-c("Rao")
      return(outl)    
    } else if( !isfloat & nc.cores==1 ) {
      outl<-list(raoqe)
      names(outl)<-c("Rao")
      return(outl)    
    } else if( !isfloat & nc.cores>1 ) {
      outl<-list(do.call(cbind,raoqe))
      names(outl)<-c("Rao")
      return(outl)
    }
  } else if( !shannon & mode=="multidimension" ) {
    outl<-list(raoqe)
    names(outl)<-c("Multidimension_Rao")
    return(outl)
  }
}










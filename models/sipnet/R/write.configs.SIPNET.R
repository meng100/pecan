#' Write SIPNET model configuration files to run directory
#' 
#' @name write.config.SIPNET
#' 
#' @param defaults pft
#' @param trait.values vector of samples for a given trait
#' @param settings PEcAn settings object
#' @param run.id run ID
#' @param inputs list of model inputs
#' @param IC initial condition 
#' @param restart In case this is a continuation of an old simulation. restart needs to be a list with name tags of runid, inputs, new.params (parameters), new.state (initial condition), ensemble.id (ensemble id), start.time and stop.time.See Details.
#' @param spinup currently unused, included for compatibility with other models
#' @export
#' @author Michael Dietze
write.config.SIPNET <- function(defaults, trait.values, settings, run.id, 
                                inputs=NULL, IC=NULL, restart=NULL, spinup=NULL) {
  
  ### WRITE sipnet.in
  template.in <- system.file("sipnet.in", package = "PEcAn.SIPNET")
  config.text <- readLines(con = template.in, n = -1)
  writeLines(config.text, con = file.path(settings$rundir, run.id, "sipnet.in"))
  
  ### WRITE *.clim
  template.clim <- settings$run$inputs$met$path  ## read from settings
  if (!is.null(inputs)) {
    ## override if specified in inputs
    if ("met" %in% names(inputs)) {
      template.clim <- inputs$met$path
    }
  }
  PEcAn.logger::logger.info(paste0("Writing SIPNET configs with input ", template.clim))
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, as.character(run.id))
  outdir <- file.path(settings$host$outdir, as.character(run.id))
  if (is.null(settings$host$qsub) && (settings$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  
  jobsh <- get_sipnet_bash_runner(settings, rundir, outdir, template.clim)
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  ### WRITE *.param-spatial
  template.paramSpatial <- system.file("template.param-spatial", package = "PEcAn.SIPNET")
  file.copy(template.paramSpatial, file.path(settings$rundir, run.id, "sipnet.param-spatial"))
  
  
  param <- get_sipnet_default_params(settings)
  param <- override_sipnet_default_params(param, settings, defaults, trait.values, IC)
  
  if (file.exists(file.path(settings$rundir, run.id, "sipnet.param"))) {
    file.rename(
      file.path(settings$rundir, run.id, "sipnet.param"),
      file.path(
        settings$rundir,
        run.id,
        paste0("sipnet_", lubridate::year(settings$run$start.date), "_", lubridate::year(settings$run$end.date), ".param")
      )
    )
  }

  utils::write.table(
    param,
    file.path(settings$rundir, run.id, "sipnet.param"),
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
}


#' Build Bash Script to Run SIPNET Model
#'
#' @param settings A PEcAn \code{Settings} object.
#'
#' @export
get_sipnet_bash_runner <- function(settings, rundir, outdir, template.clim) {
  
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.SIPNET"), n = -1)
  }
  
  # create host specific settings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  # create cdo specific settings
  cdosetup <- ""
  if (!is.null(settings$host$cdosetup)) {
    cdosetup <- paste(cdosetup, sep = "\n", paste(settings$host$cdosetup, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create rabbitmq specific setup.
  cpruncmd <- cpoutcmd <- rmoutdircmd <- rmrundircmd <- ""
  if (!is.null(settings$host$rabbitmq)) {
    #rsync cmd from remote to local host.
    settings$host$rabbitmq$cpfcmd <- ifelse(is.null(settings$host$rabbitmq$cpfcmd), "", settings$host$rabbitmq$cpfcmd)
    cpruncmd <- gsub("@OUTDIR@", settings$host$rundir, settings$host$rabbitmq$cpfcmd)
    cpruncmd <- gsub("@OUTFOLDER@", rundir, cpruncmd)
    
    cpoutcmd <- gsub("@OUTDIR@", settings$host$outdir, settings$host$rabbitmq$cpfcmd)
    cpoutcmd <- gsub("@OUTFOLDER@", outdir, cpoutcmd)
    
    #delete files within rundir and outdir.
    rmoutdircmd <- paste("rm", file.path(outdir, "*"))
    rmrundircmd <- paste("rm", file.path(rundir, "*"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@CDO_SETUP@", cdosetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", template.clim, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@",settings$run$end.date , jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  jobsh <- gsub("@REVISION@", settings$model$revision, jobsh)
  
  jobsh <- gsub("@CPRUNCMD@", cpruncmd, jobsh)
  jobsh <- gsub("@CPOUTCMD@", cpoutcmd, jobsh)
  jobsh <- gsub("@RMOUTDIRCMD@", rmoutdircmd, jobsh)
  jobsh <- gsub("@RMRUNDIRCMD@", rmrundircmd, jobsh)
  
  if(is.null(settings$state.data.assimilation$NC.Prefix)){
    settings$state.data.assimilation$NC.Prefix <- "sipnet.out"
  }
  jobsh <- gsub("@PREFIX@", settings$state.data.assimilation$NC.Prefix, jobsh)
  
  #overwrite argument
  if(is.null(settings$state.data.assimilation$NC.Overwrite)){
    settings$state.data.assimilation$NC.Overwrite <- FALSE
  }
  jobsh <- gsub("@OVERWRITE@", settings$state.data.assimilation$NC.Overwrite, jobsh)
  
  #allow conflict? meaning allow full year nc export.
  if(is.null(settings$state.data.assimilation$FullYearNC)){
    settings$state.data.assimilation$FullYearNC <- FALSE
  }
  jobsh <- gsub("@CONFLICT@", settings$state.data.assimilation$FullYearNC, jobsh)
  
  if (is.null(settings$model$delete.raw)) {
    settings$model$delete.raw <- FALSE
  }
  jobsh <- gsub("@DELETE.RAW@", settings$model$delete.raw, jobsh)
  
  return(jobsh)
}


#' Returns Table of Default SIPNET Parameter Settings
#'
#' @details
#' The term "parameter" here is used generally to include SIPNET model
#' parameters (e.g., trait values) and initial conditions for state variables.
#' If \code{settings$model$default.param} is specified, then this value will
#' be taken as the default parameters. Otherwise, falls back on the defaults
#' specified in https://github.com/PecanProject/pecan/blob/develop/models/sipnet/inst/template.param
#' 
#' @param settings A PEcAn \code{Settings} object.
#' 
#' @returns \code{data.frame}, the table of default parameter values, with 
#'  one row per parameter. 
#' @export
get_sipnet_default_params <- function(settings) {
  template.param <- system.file("template.param", package="PEcAn.SIPNET")
  if ("default.param" %in% names(settings$model)) {
    template.param <- settings$model$default.param
  }
  
  param <- utils::read.table(template.param)
  return(param)
}


#' Overrides Values in Default SIPNET Parameter Settings
#'
#' Given a table \code{param} of default SIPNET parameters
#' (see \code{\link{get_sipnet_default_params}}), overrides default parameter
#' values specified in \code{trait.values} and initial conditions specified
#' in \code{IC}.
#' 
#' @details
#' Default initial condition values can also be overwritten indirectly via
#' specifying file paths in \code{setttings}. See 
#' \code{\link{override_sipnet_default_ic}}.
#'
#' @param param \code{data.frame}, the table of default parameters.
#' @param settings A PEcAn \code{Settings} object.
#' @param defaults TODO: add requirements
#' @param trait.values TODO: add requirements
#' @param IC TODO: add requirements
#' 
#' @returns \code{data.frame} of the same structure as \code{param}, with 
#'  default values overwritten.
override_sipnet_default_params <- function(param, settings, defaults, trait.values, IC=NULL) {
  
  # Flatten to single list.
  trait_list <- simplify_sipnet_trait_values(trait.values)

  # Set constants and drop missing values.
  trait_list <- set_sipnet_constant_traits(settings, defaults, trait_list)
  trait_list <- drop_missing_sipnet_traits(trait_list)
  trait_names <- names(trait_list)
  
  # Leaf carbon concentration
  if ("leafC" %in% trait_names) {
    leafC <- trait_names[["leafC"]]
    cFracLeaf <- leafC * 0.01  # convert from percentage to fraction
    param <- .set_sipnet_param_value(param, "cFracLeaf", cFracLeaf)
  } else {
    leafC <- 0.48
  }
  
  # TODO: Left off here - need to:
  # 1.) Continue updating variable names (e.g., use trait_list variable)
  # 2.) Use helper function .set_sipnet_param_value()
  #
  # This has been updated for a few parameters to illustrate the pattern. For
  # parameters only requiring a name change (no unit conversion) we can simply
  # use .set_sipnet_param_value() to directly set the value. No need to check
  # if the pecan param is provided, since this helper will not change anything
  # if the value is NULL.
  
  # Specific leaf area converted to SLW
  # leafCSpWt [gC/m2 leaf], SLA [m2 leaf/kg C], leafC [percentage C]
  SLA <- NA
  id <- which(param[, 1] == "leafCSpWt")
  if ("SLA" %in% pft.names) {
    SLA <- pft.traits[which(pft.names == "SLA")]
    param[id, 2] <- PEcAn.utils::ud_convert(leafC / SLA, "kg/m2", "g/m2")
  } else {
    SLA <- PEcAn.utils::ud_convert(leafC / param[id, 2], "kg", "g")
  }
  
  # SIPNET: aMax [nmol CO2 / g   leaf / sec]
  # PEcAn:  Amax [umol CO2 / m^2 leaf / sec]
  SLA_g <- PEcAn.utils::ud_convert(SLA, "1/kg", "1/g") 
  if ("Amax" %in% pft.names) {
    Amax_area <- pft.traits[which(pft.names == "Amax")] # [µmol/m2/s]
    param[id, 2] <- PEcAn.utils::ud_convert(Amax_area * SLA_g, "umol/m2/s", "nmol/g/s")
  } else {
    amax_mass <- param[id, 2] # [nmol/g/s]
    Amax <- PEcAn.utils::ud_convert(amax_mass / SLA_g, "nmol/g/s", "umol/m2/s")
  }
  
  # Daily fraction of maximum photosynthesis
  param <- .set_sipnet_param_value(param, "aMaxFrac", trait_list[["AmaxFrac"]])

  # Canopy extinction coefficient (k)
  param <- .set_sipnet_param_value(param, "attenuation", trait_list[["extinction_coefficient"]])

  # Leaf respiration rate converted to baseFolRespFrac
  if ("leaf_respiration_rate_m2" %in% pft.names) {
    Rd <- pft.traits[which(pft.names == "leaf_respiration_rate_m2")]
    id <- which(param[, 1] == "baseFolRespFrac")
    param[id, 2] <- max(min(Rd/Amax, 1), 0)
  }
  
  # Low temp threshold for photosynethsis
  param <- .set_sipnet_param_value(param, "psnTMin", trait_list[["Vm_low_temp"]])

  # Opt. temp for photosynthesis
  param <- .set_sipnet_param_value(param, "psnTOpt", trait_list[["psnTOpt"]])

  # Growth respiration factor (fraction of GPP)
  param <- .set_sipnet_param_value(param, "growthRespFrac", trait_list[["growth_resp_factor"]])
  
  ### !!! NOT YET USED
  #Jmax = NA
  #if("Jmax" %in% pft.names){
  #  Jmax = pft.traits[which(pft.names == 'Jmax')]
  ### Using Jmax scaled to 25 degC. Maybe not be the best approach
  #}
  
  #alpha = NA
  #if("quantum_efficiency" %in% pft.names){
  #  alpha = pft.traits[which(pft.names == 'quantum_efficiency')]
  #}
  
  # Half saturation of PAR.  PAR at which photosynthesis occurs at 1/2 theoretical maximum (Einsteins * m^-2 ground area * day^-1).
  #if(!is.na(Jmax) & !is.na(alpha)){
  # param[which(param[,1] == "halfSatPar"),2] = Jmax/(2*alpha)
  ### WARNING: this is a very coarse linear approximation and needs improvement *****
  ### Yes, we also need to work on doing a paired query where we have both data together.
  ### Once halfSatPar is calculated, need to remove Jmax and quantum_efficiency from param list so they are not included in SA
  #}
  ### !!!
  
  # Half saturation of PAR.  PAR at which photosynthesis occurs at 1/2 
  # theoretical maximum (Einsteins * m^-2 ground area * day^-1).
  # Temporary implementation until above is working.
  param <- .set_sipnet_param_value(param, "halfSatPar", trait_list[["half_saturation_PAR"]])

  # Ball-berry slomatal slope parameter m
  if ("stomatal_slope.BB" %in% pft.names) {
    id <- which(param[, 1] == "m_ballBerry")
    param[id, 2] <- pft.traits[which(pft.names == "stomatal_slope.BB")]
  }
  
  # Slope of VPD–photosynthesis relationship. dVpd = 1 - dVpdSlope * vpd^dVpdExp
  param <- .set_sipnet_param_value(param, "dVpdSlope", trait_list[["dVPDSlope"]])

  # VPD–water use efficiency relationship.  dVpd = 1 - dVpdSlope * vpd^dVpdExp
  param <- .set_sipnet_param_value(param, "dVpdExp", trait_list[["dVpdExp"]])

  # Leaf turnover rate average turnover rate of leaves, in fraction per day 
  # NOTE: read in as per-year rate!
  param <- .set_sipnet_param_value(param, "leafTurnoverRate", trait_list[["leaf_turnover_rate"]])

  param <- .set_sipnet_param_value(param, "wueConst", trait_list[["wueConst"]])
  
  # vegetation respiration Q10.
  param <- .set_sipnet_param_value(param, "vegRespQ10", trait_list[["veg_respiration_Q10"]])

  # Base vegetation respiration. vegetation maintenance respiration at 0 degrees C (g C respired * g^-1 plant C * day^-1)
  # NOTE: only counts plant wood C - leaves handled elsewhere (both above and below-ground: assumed for now to have same resp. rate)
  # NOTE: read in as per-year rate!
  if ("stem_respiration_rate" %in% pft.names) {
    vegRespQ10 <- param[which(param[, 1] == "vegRespQ10"), 2]
    id <- which(param[, 1] == "baseVegResp")
    ## Convert from umols CO2 kg s-1 to gC g day-1
    stem_resp_g <- (((pft.traits[which(pft.names == "stem_respiration_rate")]) *
                       (44.0096 / 1e+06) * (12.01 / 44.0096)) / 1000) * 86400
    ## use Q10 to convert stem resp from reference of 25C to 0C param[id,2] =
    ## pft.traits[which(pft.names=='stem_respiration_rate')]*vegRespQ10^(-25/10)
    param[id, 2] <- stem_resp_g * vegRespQ10^(-25/10)
  }
  
  # turnover of fine roots (per year rate)
  param <- .set_sipnet_param_value(param, "fineRootTurnoverRate", trait_list[["root_turnover_rate"]])

  # fine root respiration Q10
  param <- .set_sipnet_param_value(param, "fineRootQ10", trait_list[["fine_root_respiration_Q10"]])
  
  # base respiration rate of fine roots (per year rate)
  if ("root_respiration_rate" %in% pft.names) {
    fineRootQ10 <- param[which(param[, 1] == "fineRootQ10"), 2]
    id <- which(param[, 1] == "baseFineRootResp")
    ## Convert from umols CO2 kg s-1 to gC g day-1
    root_resp_rate_g <- (((pft.traits[which(pft.names == "root_respiration_rate")]) *
                            (44.0096/1e+06) * (12.01 / 44.0096)) / 1000) * 86400
    ## use Q10 to convert stem resp from reference of 25C to 0C param[id,2] =
    ## pft.traits[which(pft.names=='root_respiration_rate')]*fineRootQ10^(-25/10)
    param[id, 2] <- root_resp_rate_g * fineRootQ10 ^ (-25 / 10)
  }
  
  # coarse root respiration Q10
  param <- .set_sipnet_param_value(param, "coarseRootQ10", trait_list[["coarse_root_respiration_Q10"]])

  
  # WARNING: fineRootAllocation + woodAllocation + leafAllocation isn't supposed to exceed 1
  # see sipnet.c code L2005 :
  # fluxes.coarseRootCreation=(1-params.leafAllocation-params.fineRootAllocation-params.woodAllocation)*npp;
  # priors can be chosen accordingly, and SIPNET doesn't really crash when sum>1 but better keep an eye
  alloc_params <- c("root_allocation_fraction", "wood_allocation_fraction", "leaf_allocation_fraction")
  if (all(alloc_params %in% pft.names)) {
    sum_alloc <- pft.traits[which(pft.names == "root_allocation_fraction")] +
      pft.traits[which(pft.names == "wood_allocation_fraction")] +
      pft.traits[which(pft.names == "leaf_allocation_fraction")]
    if(sum_alloc > 1){
      # I want this to be a severe for now, lateer can be changed back to warning
      PEcAn.logger::logger.warn("Sum of allocation parameters exceeds 1 for runid = ", run.id,
                                "- This won't break anything since SIPNET has internal check, but notice that such combinations might not take effect in the outputs.")
    }
  }
  
  
  # fineRootAllocation
  param <- .set_sipnet_param_value(param, "fineRootAllocation", trait_list[["root_allocation_fraction"]])

  # woodAllocation
  param <- .set_sipnet_param_value(param, "woodAllocation", trait_list[["wood_allocation_fraction"]])

  # leafAllocation
  param <- .set_sipnet_param_value(param, "leafAllocation", trait_list[["leaf_allocation_fraction"]])

  # wood_turnover_rate
  param <- .set_sipnet_param_value(param, "woodTurnoverRate", trait_list[["wood_turnover_rate"]])

  ### ----- Soil parameters soil respiration Q10.
  param <- .set_sipnet_param_value(param, "soilRespQ10", trait_list[["soil_respiration_Q10"]])

  # soil respiration rate -- units = 1/year, reference = 0C
  param <- .set_sipnet_param_value(param, "baseSoilResp", trait_list[["som_respiration_rate"]])

  # litterBreakdownRate
  param <- .set_sipnet_param_value(param, "litterBreakdownRate", trait_list[["turn_over_time"]])

  # frozenSoilEff
  param <- .set_sipnet_param_value(param, "frozenSoilEff", trait_list[["frozenSoilEff"]])

  # frozenSoilFolREff
  param <- .set_sipnet_param_value(param, "frozenSoilFolREff", trait_list[["frozenSoilFolREff"]])

  # soilWHC
  param <- .set_sipnet_param_value(param, "soilWHC", trait_list[["soilWHC"]])
  
  # 10/31/2017 IF: these were the two assumptions used in the emulator paper in order to reduce dimensionality
  # These results in improved winter soil respiration values
  # they don't affect anything when the seasonal soil respiration functionality in SIPNET is turned-off
  if(TRUE){
    # assume soil resp Q10 cold == soil resp Q10
    param[which(param[, 1] == "soilRespQ10Cold"), 2] <- param[which(param[, 1] == "soilRespQ10"), 2]
    # default SIPNET prior of baseSoilRespCold was 1/4th of baseSoilResp
    # assuming they will scale accordingly
    param[which(param[, 1] == "baseSoilRespCold"), 2] <- param[which(param[, 1] == "baseSoilResp"), 2] * 0.25
  }
  
  param <- .set_sipnet_param_value(param, "immedEvapFrac", trait_list[["immedEvapFrac"]])
  param <- .set_sipnet_param_value(param, "leafPoolDepth", trait_list[["leafWHC"]])
  param <- .set_sipnet_param_value(param, "waterRemoveFrac", trait_list[["waterRemoveFrac"]])
  param <- .set_sipnet_param_value(param, "fastFlowFrac", trait_list[["fastFlowFrac"]])
  param <- .set_sipnet_param_value(param, "rdConst", trait_list[["rdConst"]])
  
  ### ----- Phenology parameters GDD leaf on
  param <- .set_sipnet_param_value(param, "gddLeafOn", trait_list[["GDD"]])
  
  # Fraction of leaf fall per year (should be 1 for decid)
  param <- .set_sipnet_param_value(param, "fracLeafFall", trait_list[["fracLeafFall"]])
  
  # Leaf growth.  Amount of C added to the leaf during the greenup period
  param <- .set_sipnet_param_value(param, "leafGrowth", trait_list[["leafGrowth"]])

  #update LeafOnday and LeafOffDay
  if (!is.null(settings$run$inputs$leaf_phenology)) {
    obs_year_start <- lubridate::year(settings$run$start.date)
    obs_year_end <- lubridate::year(settings$run$end.date)
    if (obs_year_start != obs_year_end) {
      PEcAn.logger::logger.info(
        "Start.date and end.date are not in the same year.",
        "Using phenological data from start year only."
      )
    }
    leaf_pheno_path <- settings$run$inputs$leaf_phenology$path
    if (!is.null(leaf_pheno_path)) {
      ##read data
      leafphdata <- utils::read.csv(leaf_pheno_path)
      leafOnDay <- leafphdata$leafonday[leafphdata$year == obs_year_start
                                        & leafphdata$site_id == settings$run$site$id]
      leafOffDay <- leafphdata$leafoffday[leafphdata$year == obs_year_start
                                          & leafphdata$site_id == settings$run$site$id]
      if (!is.na(leafOnDay)) {
        param[which(param[, 1] == "leafOnDay"), 2] <- leafOnDay
      }
      if (!is.na(leafOffDay)) {
        param[which(param[, 1] == "leafOffDay"), 2] <- leafOffDay
      }
    } else {
      PEcAn.logger::logger.info("No phenology data were found.",
                                "Please consider running `PEcAn.data.remote::extract_phenology_MODIS`",
                                "to get the parameter file."
      )
    }
  }

  ####### end parameter update
  #working on reading soil file
  if (length(settings$run$inputs$soil_physics$path) > 0) {
    template.soil_physics <- settings$run$inputs$soil_physics$path  ## read from settings
    
    if (!is.null(inputs)) {
      ## override if specified in inputs
      if ("soil_physics" %in% names(inputs)) {
        template.soil_physics <- inputs$soil_physics$path
      }
    }
    
    if (length(template.soil_physics)!=1) {
      PEcAn.logger::logger.warn(
        paste0("No single soil physical parameter file was found for ",
               run.id))
    } else {
      soil_IC_list <- PEcAn.data.land::pool_ic_netcdf2list(template.soil_physics)
      #SoilWHC
      if ("volume_fraction_of_water_in_soil_at_saturation" %in% names(soil_IC_list$vals)) {
        #if depth is provided in the file
        if ("depth" %in% names(soil_IC_list$dims)) {
          # Calculate the thickness of soil layers based on the assumption that the depth values are at bottoms and the first layer top is at 0
          thickness<-c(soil_IC_list$dims$depth[1],diff(soil_IC_list$dims$depth))
          thickness<-PEcAn.utils::ud_convert(thickness, "m", "cm")
          # Calculate the soilWHC for the whole soil profile in cm
          soilWHC_total <- sum(unlist(soil_IC_list$vals["volume_fraction_of_water_in_soil_at_saturation"])*thickness)
          if (thickness[1]<=10) {
            #LitterWHC in cm, assuming the litter depth is within the top 10 cm
            param[which(param[, 1] == "litterWHC"), 2] <- unlist(soil_IC_list$vals["volume_fraction_of_water_in_soil_at_saturation"])[1]*thickness[1]
          }
        } else {
          #if no depth/thickness is provided
          PEcAn.logger::logger.warn("No depth info was found in the soil file. Will use the default or user-specified soil depth")
          thickness <- 100 #assume the default soil depth is the plant rooting depth of 100 cm, or use the user-specified value
          soilWHC_total <- soil_IC_list$vals["volume_fraction_of_water_in_soil_at_saturation"]*thickness
        }
        param[which(param[, 1] == "soilWHC"), 2] <- soilWHC_total
      }
      if ("soil_hydraulic_conductivity_at_saturation" %in% names(soil_IC_list$vals)) {
        #litwaterDrainrate in cm/day
        param[which(param[, 1] == "litWaterDrainRate"), 2] <- PEcAn.utils::ud_convert(unlist(soil_IC_list$vals["soil_hydraulic_conductivity_at_saturation"])[1], "m s-1", "cm day-1")
      }
    }
  }
  # Edits with IC starts here: switched order for the if/ifelse statement and make it an if-if 
  # logistics: first read in the nc file, overwrite specific values with what we pass
  #            in through the IC argument
  if (length(settings$run$inputs$poolinitcond$path) > 0) {
    IC.path <- settings$run$inputs$poolinitcond$path
    if (length(IC.path) > 1) {
      PEcAn.logger::logger.error(
        "write.config.SIPNET needs one poolinitcond path",
        "got", length(IC.path)
      )
    }
    
    IC.pools <- PEcAn.data.land::prepare_pools(IC.path, constants = list(sla = SLA))
    
    if (!is.null(IC.pools)) {
      IC.nc <- ncdf4::nc_open(IC.path) #for additional variables specific to SIPNET
      
      # Optional variables: Use these if present, but don't complain if missing
      # TODO: Each variable here is used in a corresponding `if` block below,
      # which are mixed in among the variables from prepare_pools.
      # Should reorder to separate these, and consider making this an input
      # to let user control at runtime what's optional and what's mandatory
      ic_ncvars_to_try <- c(
        "nee",
        "SoilMoistFrac",
        "SWE",
        "date_of_budburst",
        "date_of_senescence",
        "Microbial Biomass C"
      )
      ic_has_ncvars <- ic_ncvars_to_try %in% names(IC.nc$var)
      names(ic_has_ncvars) <- ic_ncvars_to_try
      
      ## plantWoodInit gC/m2
      if ("wood" %in% names(IC.pools)) {
        param[param[, 1] == "plantWoodInit", 2] <- PEcAn.utils::ud_convert(IC.pools$wood, "kg m-2", "g m-2")
      }
      ## laiInit m2/m2
      lai <- IC.pools$LAI
      if (!is.na(lai) && is.numeric(lai)) {
        param[param[, 1] == "laiInit", 2] <- lai
      }
      
      # Sipnet always starts from initial LAI whether day 0 is in or out of the
      # growing season -> set LAI=0 when a deciduous PFT starts with leaves off
      #
      # Note: At this writing in Jan 2025, leafOnDay and LeafOffDay are taken
      # from the model defaults (template.param) unless:
      # - settings$run$inputs$leaf_phenology is provided, or
      # - the PFT sets leafOnDay/leafOffday as traits.
      # So unless you set something different, it's probably using DOY 144/285
      # ==> leaves are on from late May through mid-October.
      is_deciduous_pft <- isTRUE(param[param[, 1] == "fracLeafFall", 2] > 0.5)
      start_day <- lubridate::yday(settings$run$start.date)
      starts_with_leaves <- (
        start_day >= param[param[, 1] == "leafOnDay", 2]
        && start_day <= param[param[, 1] == "leafOffDay", 2]
      )
      if (is_deciduous_pft && !starts_with_leaves) {
        # Note that this doesn't adjust for winter LAI of evergreens!
        # Could consider using LAI*fracLeafFall,
        # But that strongly assumes that IC LAI is both (1) reported at
        # season peak and not (2) adjusted by any earlier step (i.e. SDA).
        param[param[, 1] == "laiInit", 2] <- 0
      }
      
      ## neeInit gC/m2
      if (ic_has_ncvars[["nee"]]) {
        nee <- ncdf4::ncvar_get(IC.nc, "nee")
        if (!is.na(nee) && is.numeric(nee)) {
          param[param[, 1] == "neeInit", 2] <- nee
        }
      }
      ## litterInit gC/m2
      if ("litter" %in% names(IC.pools)) {
        param[param[, 1] == "litterInit", 2] <- PEcAn.utils::ud_convert(IC.pools$litter, "g m-2", "g m-2") # BETY: kgC m-2
      }
      ## soilInit gC/m2
      if ("soil" %in% names(IC.pools)) {
        param[param[, 1] == "soilInit", 2] <- PEcAn.utils::ud_convert(sum(IC.pools$soil), "kg m-2", "g m-2") # BETY: kgC m-2
      }
      ## soilWFracInit fraction
      if (ic_has_ncvars[["SoilMoistFrac"]]) {
        soilWFrac <- ncdf4::ncvar_get(IC.nc, "SoilMoistFrac")
        if (!is.na(soilWFrac) && is.numeric(soilWFrac)) {
          param[param[, 1] == "soilWFracInit", 2] <- sum(soilWFrac) / 100
        }
      }
      ## litterWFracInit fraction
      litterWFrac <- soilWFrac
      
      ## snowInit cm water equivalent (cm = g / cm2 because 1 g water = 1 cm3 water)
      if (ic_has_ncvars[["SWE"]]) {
        snow <- ncdf4::ncvar_get(IC.nc, "SWE")
        if (!is.na(snow) && is.numeric(snow)) {
          param[param[, 1] == "snowInit", 2] <- PEcAn.utils::ud_convert(snow, "kg m-2", "g cm-2")  # BETY: kg m-2
        }
      }
      ## leafOnDay
      if (ic_has_ncvars[["date_of_budburst"]]) {
        leafOnDay <- ncdf4::ncvar_get(IC.nc, "date_of_budburst")
        if (!is.na(leafOnDay) && is.numeric(leafOnDay)) {
          param[param[, 1] == "leafOnDay", 2] <- leafOnDay
        }
      }
      ## leafOffDay
      if (ic_has_ncvars[["date_of_senescence"]]) {
        leafOffDay <- ncdf4::ncvar_get(IC.nc, "date_of_senescence")
        if (!is.na(leafOffDay) && is.numeric(leafOffDay)) {
          param[param[, 1] == "leafOffDay", 2] <- leafOffDay
        }
      }
      if (ic_has_ncvars[["Microbial Biomass C"]]) {
        microbe <- ncdf4::ncvar_get(IC.nc, "Microbial Biomass C")
        if (!is.na(microbe) && is.numeric(microbe)) {
          param[param[, 1] == "microbeInit", 2] <- PEcAn.utils::ud_convert(microbe, "mg kg-1", "mg g-1") #BETY: mg microbial C kg-1 soil
        }
      }
      
      ncdf4::nc_close(IC.nc)
    } else {
      PEcAn.logger::logger.error("Bad initial conditions filepath; keeping defaults")
    }
  } else {
    #some stuff about IC file that we can give in lieu of actual ICs
  }
  # Edits:
  # Conditional statements added to address pecan variable names
  # Additional arguments added to address ic vars missing from original IC options
  if (!is.null(IC)) {
    ic.names <- names(IC)
    ## plantWoodInit gC/m2
    plant_wood_vars <- c("AbvGrndWood", "abvGrndWoodFrac", "coarseRootFrac", "fineRootFrac")
    if (all(plant_wood_vars %in% ic.names)) {
      # reconstruct total wood C
      if(IC$abvGrndWoodFrac < 0.05){
        wood_total_C <- IC$AbvGrndWood
      }else{
        wood_total_C <- IC$AbvGrndWood / IC$abvGrndWoodFrac
      }
      
      #Sanity check
      if (is.infinite(wood_total_C) | is.nan(wood_total_C) | wood_total_C < 0) {
        wood_total_C <- 0
        if (round(IC$AbvGrndWood) > 0 & round(IC$abvGrndWoodFrac, 3) == 0)
          PEcAn.logger::logger.warn(
            paste0(
              "There is a major problem with ",
              run.id,
              " in either the model's parameters or IC.",
              "Because the ABG is estimated=",
              IC$AbvGrndWood,
              " while AGB Frac is estimated=",
              IC$abvGrndWoodFrac
            )
          )
      }
      param[which(param[, 1] == "plantWoodInit"),  2] <- wood_total_C
      param[which(param[, 1] == "coarseRootFrac"), 2] <- IC$coarseRootFrac
      param[which(param[, 1] == "fineRootFrac"),   2] <- IC$fineRootFrac
    }
    ## neeInit gC/m2
    if ("NEE" %in% ic.names) {
      PEcAn.utils::ud_convert(IC$NEE, 'kg m-2', 'g m-2')
    }
    ## laiInit m2/m2
    if ("LAI" %in% ic.names) {
      param[which(param[, 1] == "laiInit"), 2] <- IC$LAI
    }
    ## litterInit gC/m2
    if ("litter_carbon_content" %in% ic.names) {
      param[which(param[, 1] == "litterInit"), 2] <- IC$litter_carbon_content
    }
    ## soilInit gC/m2
    if ("TotSoilCarb" %in% ic.names) {    # TotSoilCarb kgC/m2 to gC/m2
      param[which(param[, 1] == "soilInit"), 2] <- PEcAn.utils::ud_convert(IC$TotSoilCarb, 'kg m-2', 'g m-2')
    }
    ## litterWFracInit fraction
    if ("litter_mass_content_of_water" %in% ic.names) {
      #here we use litterWaterContent/litterWHC to calculate the litterWFracInit
      param[which(param[, 1] == "litterWFracInit"), 2] <- IC$litter_mass_content_of_water/(param[which(param[, 1] == "litterWHC"), 2]*10)
    }
    ## soilWater IC$soilMoist is in kg/m2, and soilWHC is in cm
    if ("SoilMoist" %in% ic.names) {
      param[which(param[, 1] == "soilWFracInit"), 2] <- IC$SoilMoist/(param[which(param[, 1] == "soilWHC"), 2]*10)
    }
    ## soilWFracInit fraction
    if ("SoilMoistFrac" %in% ic.names) {
      soilWFrac <- IC$SoilMoistFrac / 100
      if (soilWFrac < 0 || soilWFrac > 1) soilWFrac <- 0.5
      param[which(param[, 1] == "soilWFracInit"), 2] <- soilWFrac
    }
    ## snowInit cm water equivalent
    if ("SWE" %in% ic.names) {
      param[which(param[, 1] == "snowInit"), 2] <- IC$SWE
    }
    ## microbeInit mgC/g soil
    if ("microbe" %in% ic.names) {
      param[which(param[, 1] == "microbeInit"), 2] <- IC$microbe
    }
  } 
  
  if (!is.null(settings$run$inputs$soilmoisture)) {
    #read soil moisture netcdf file, grab closet date to start_date, set equal to soilWFrac
    if (!is.null(settings$run$inputs$soilmoisture$path)) {
      soil.path <- settings$run$inputs$soilmoisture$path
      soilWFrac <- ncdf4::ncvar_get(ncdf4::nc_open(soil.path), varid = "mass_fraction_of_unfrozen_water_in_soil_moisture")
      
      param[which(param[, 1] == "soilWFracInit"), 2] <- soilWFrac
    }
  }
  
  return(param)
}


#' Validate SIPNET Parameter Input Formatting
#'
#' Ensures the \code{trait.values} argument, which specifies parameter values
#' that will overwrite defaults, is of the valid form as required by the 
#' SIPNET model.
#' 
#' @details
#' For consistency with other PEcAn models, \code{trait.values} is a list over 
#' plant functional types (PFTs). SIPNET does not model any sort of interaction
#' between competing PFTs and thus nominally only runs with a single PFT.
#' However, it is common to conceptually partition the SIPNET parameters into
#' plant traits and soil traits. Therefore, one may want to assign a plant 
#' PFT and a soil "PFT" to these two parameter groups, in which case 
#' \code{trait.values} may be a two element list. The parameter sets specified 
#' in each element of the list must be disjoint subsets, as only one value
#' for each parameter is allowed.
#' 
#' Each element of the list must be a named numeric vector, named list, 
#' or single-row data.frame. In all cases, the names (which must be unique)
#' correspond to the parameter names, while the values are the corresponding
#' parameter values. The function \code{\link{simplify_sipnet_trait_values}}
#' is responsible for reducing \code{trait.values} to a single list. 
#' 
#' @param trait.values list of named vectors/lists/data.frames. See details
#'  for specific requirements.
#'  
#' @returns Invisibly returns \code{TRUE} if validation passes. Throws error
#'  otherwise.
#' 
#' @author Andrew Roberts
#' @export
validate_sipnet_trait_values <- function(trait.values) {
  
  is_valid_trait_format <- function(x) {
    PEcAn.utils::has_unique_names(x) && 
      (is.list(x) || is.atomic(x))   &&
      (!is.data.frame(x) || nrow(x) == 1L)
  }
  
  assertthat::assert_that(is.list(trait.values))
  assertthat::assert_that(all(vapply(trait.values, is_valid_trait_format, logical(1))))
  
  # Ensure no duplication of parameter names across PFTs.
  all_trait_names <- unlist(lapply(l, names))
  dup_traits <- unique(all_trait_names[duplicated(all_trait_names)])
  
  if(length(dup_traits) > 0L) {
    stop("Duplicate trait names found in `trait.values`: ",
         paste(dup_traits, sep=", "))
  }
  
  invisible(TRUE)
}


#' Flatten List over PFTs to Single List
#'
#' Combines the elements of \code{trait.values} into a single list.
#' See \code{\link{validate_sipnet_trait_values}} for details on the structure
#' of \code{trait.values}.
#' 
#' @details
#' The elements of \code{trait.values} specify disjoint subsets of parameters.
#' This function combines these subsets into a single list `l` so that 
#' `l[[trait.name]]` accesses the value for a particular parameter. At present,
#' the elements of \code{trait.values} are assumed to be vectors, lists, or
#' one-row data.frames specifyinig values for scalar parameters. Therefore,
#' the resulting flattened list will contain only scalar elements. However, 
#' this function is robust to future generalizations in which the notion of 
#' "parameters" may include multivariate parameters or more structured objects.
#' A list is returned so that these generalizations may be easily accomodated
#' in the future. See examples for more details.
#' 
#' @param trait.values list of named vectors/lists/data.frames. See 
#' \code{\link{validate_sipnet_trait_values}} for details.
#' 
#' @returns list, of length \code{sum(sapply(trait.values, length))}. If
#' \code{trait.values[[j]][["par_name"]]} exists, then it can be accessed
#' via \code{l[["par_name"]]} in the returned list.
#' 
#' @examples
#' # Current case: scalar parameters
#' trait.values <- list(plant = c(par1=1, par2=2), soil = list(par3=3), 
#'                      other = data.frame(par4=4, par5=5))
#' simplify_sipnet_trait_values(trait.values)
#' 
#' # Supports future generalization: "parameter" of list type
#' trait.values <- list(plant = c(par1=1, par2=2), soil = list(par3=3), 
#'                      other = list(list_par = list(4,5,6)))
#' simplify_sipnet_trait_values(trait.values)
#'
#' @author Andrew Roberts
simplify_sipnet_trait_values <- function(trait.values) {
  
  validate_sipnet_trait_values(trait.values)
  
  # Flatten to single list.
  flatten_one_level <- function(x) {
    if (is.data.frame(x)) as.list(x[1,, drop=FALSE])
    else as.list(x)
  }
  
  flat_list <- do.call(c, lapply(trait.values, flatten_one_level))
  names(flat_list) <- unlist(lapply(trait.values, names))
  
  return(flat_list)
}


# TODO: test and document
set_sipnet_constant_traits <- function(settings, defaults, trait_list) {
  
  # Append/replace params specified as constants
  constant.traits <- unlist(defaults[[1]]$constants)
  constant.names <- names(constant.traits)
  
  trait_names <- names(trait_list)
  
  # Replace matches
  for (i in seq_along(constant.traits)) {
    ind <- match(constant.names[i], trait_names)
    if (is.na(ind)) { 
      # Add to list
      trait_list[[constant.names[i]]] <- constant.traits[i]
    } else { 
      # Replace existing value
      trait_list[[ind]] <- constant.traits[i]
    }
  }
  
  return(trait_names)
}


#' Drop Missing Values from Trait List
#' 
#' Drops NULL, NA, or missing (e.g., empty vector/list) values from the trait
#' parameter list. Also drops string \code{"NA"} values, which appear when NA
#' is specified in the XML settings file. This XML convention is used in the
#' \code{constants} tag to signal that the constant should fall back on the
#' SIPNET template default.
#' 
#' @param list, flattened trait list as returned by 
#'  \code{\link{simplify_sipnet_trait_values}}.
#'  
#' @returns list, updated trait list potentially with some values dropped.
#' @author Andrew Roberts
drop_missing_sipnet_traits <- function(trait_list) {
  
  trait_is_missing <- function(x) {
    is.null(x) ||
      !assertthat::not_empty(x) ||
      any(is.na(x)) ||
      any(x == "NA")
  }
  
  traits_not_missing <- vapply(trait_list, !trait_is_missing, logical(1))
  trait_list[traits_not_missing]
}


#' Update One Value in SIPNET Parameter Table
#'
#' @param param_table data.frame, with first column containing SIPNET parameter
#'  names and second column containing the associated parameter values.
#' @param sipnet_param_name character(1), the SIPNET parameter name. Used to 
#'  select a row of \code{param_table}.
#' @param new_value numeric(1), the value to insert in the second column of the
#'  selected for of \code{param_table}.
#' 
#' @returns data.frame, \code{param_table}, potentially containing an updated
#'  value. If \code{new_value} is \code{NULL} or \code{sipnet_param_name} does
#'  not match a value in the first column of \code{param_table}, then the table
#'  will be unmodified.
#' 
#' @author Andrew Roberts
.set_sipnet_param_value <- function(param_table, sipnet_param_name, new_value) {
  
  if(is.null(new_value)) return(param_table)
  param_table <- param_table[param_table[,1] == sipnet_param_name, 2] <- new_value
  
  return(param_table)
}


#--------------------------------------------------------------------------------------------------#
##'
##' Clear out previous SIPNET config and parameter files.
##'
##' @name remove.config.SIPNET
##' @title Clear out previous SIPNET config and parameter files.
##' @param main.outdir Primary PEcAn output directory (will be depreciated)
##' @param settings PEcAn settings file
##' @return nothing, removes config files as side effect
##' @export
##'
##' @author Shawn Serbin, David LeBauer
remove.config.SIPNET <- function(main.outdir, settings) {
  
  ### Remove files on localhost
  if (settings$host$name == "localhost") {
    files <- paste0(settings$outdir, list.files(path = settings$outdir, recursive = FALSE))  # Need to change this to the run folder when implemented
    files <- files[-grep("*.xml", files)]  # Keep pecan.xml file
    pft.dir <- strsplit(settings$pfts$pft$outdir, "/")[[1]]
    ln <- length(pft.dir)
    pft.dir <- pft.dir[ln]
    files <- files[-grep(pft.dir, files)]  # Keep pft folder
    # file.remove(files,recursive=TRUE)
    system(paste("rm -r ", files, sep = "", collapse = " "), ignore.stderr = TRUE)  # remove files/dirs
    
    ### On remote host
  } else {
    print("*** WARNING: Removal of files on remote host not yet implemented ***")
  }
} # remove.config.SIPNET 

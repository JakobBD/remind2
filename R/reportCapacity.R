#' Read in GDX and calculate capacities, used in convGDX2MIF.R for the reporting
#'
#' Read in capacity information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param gdx_ref a GDX object as created by readGDX, or the path to a gdx of the reference run.
#' It is used to guarantee consistency before 'cm_startyear' for capacity variables
#' using time averaging.
#'
#' @return MAgPIE object - contains the capacity variables
#' @author Lavinia Baumstark, Christoph Bertram, Fabrice Lecuyer
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportCapacity(gdx)
#' }
#' @importFrom quitte calcCumulatedDiscount
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind setNames getSets getSets<- as.magpie
#' @importFrom dplyr %>% filter mutate

reportCapacity <- function(gdx, regionSubsetList = NULL,
                           t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                           gdx_ref = gdx_ref) {
  # read sets
  teall2rlf <- readGDX(gdx, name = c("te2rlf", "teall2rlf"), format = "first_found")
  ttot <- readGDX(gdx, name = "ttot")

  # read parameters
  pm_eta_conv <- readGDX(gdx, "pm_eta_conv", field = "l", restore_zeros = FALSE)
  pm_prodCouple <- readGDX(gdx, c("pm_prodCouple", "p_prodCouple", "p_dataoc"), restore_zeros = FALSE, format = "first_found")

  # read variables
  vm_cap      <- readGDX(gdx, name = c("vm_cap"), field = "l", format = "first_found") * 1000
  vm_deltaCap <- readGDX(gdx, name = c("vm_deltaCap"), field = "l", format = "first_found") * 1000
  v_earlyreti <- readGDX(gdx, name = c("vm_capEarlyReti", "v_capEarlyReti", "v_earlyreti"), field = "l", format = "first_found")

  # read scalars
  sm_c_2_co2 <- as.vector(readGDX(gdx, "sm_c_2_co2"))

  # data preparation
  ttot <- as.numeric(as.vector(ttot))
  vm_cap <- vm_cap[teall2rlf]
  vm_cap <- vm_cap[, ttot, ]

  vm_deltaCap <- vm_deltaCap[teall2rlf]
  vm_deltaCap <- vm_deltaCap[, ttot, ]

  # apply 'modifyInvestmentVariables' to shift from the model-internal time coverage (deltacap and investment
  # variables for step t represent the average of the years from t-4years to t) to the general convention for
  # the reporting template (all variables represent the average of the years from t-2.5years to t+2.5years)
  if (!is.null(gdx_ref)) {
    cm_startyear <- as.integer(readGDX(gdx, name = "cm_startyear", format = "simplest"))
    vm_deltaCapRef <- readGDX(gdx_ref, name = c("vm_deltaCap"), field = "l", format = "first_found") * 1000
    vm_deltaCapRef <- vm_deltaCapRef[teall2rlf]
    vm_deltaCapRef <- vm_deltaCapRef[, ttot, ]
    vm_deltaCap <- modifyInvestmentVariables(vm_deltaCap, vm_deltaCapRef, cm_startyear)
  } else {
    vm_deltaCap <- modifyInvestmentVariables(vm_deltaCap)
  }

  v_earlyreti <-   v_earlyreti[, ttot, ]
  t2005 <- ttot[ttot > 2004]

  ####### fix negative values of dataoc to 0 - using the lines from reportSE.R ##################
  #### adjust regional dimension of dataoc
  dataoc <- new.magpie(getRegions(vm_cap), getYears(pm_prodCouple), magclass::getNames(pm_prodCouple), fill = 0)
  dataoc[getRegions(pm_prodCouple), , ] <- pm_prodCouple
  getSets(dataoc) <- getSets(pm_prodCouple)

  dataoc[dataoc < 0] <- 0

  # determine whether onshore wind is called wind or windon
  if ("windon" %in% magclass::getNames(vm_cap, dim = 1)) {
    windonStr <- "windon"
    storwindonStr <- "storwindon"
  } else {
    windonStr <- "wind"
    storwindonStr <- "storwind"
  }

  ####### internal function for reporting ###########
  # this function is just a shortcut for setNames(dimSums(vm_cap ...))
  

  capacity_reporting <- function(prefix = "Cap") {
    if(prefix == "Cap") {
      unit <- " (GW)"
      gms_data <- vm_cap
    } else if(prefix == "New Cap") {
      unit <- " (GW/yr)"
      gms_data <- vm_deltaCap
    } else {
      stop("prefix must be either 'Cap' or 'New Cap'")
    }

    full_name <- function(variable) {
      return(paste0(prefix, variable, unit))
    }
    get_cap <- function(technologies, variable, factor = 1) {
      return(setNames(dimSums(gms_data[, , technologies], dim = 3) * factor, full_name(variable)))
    }

    # electricity
    cap_renew_nonBio <- mbind(
      get_cap("geohdr",                "|Electricity|+|Geothermal"),
      get_cap("hydro",                 "|Electricity|+|Hydro"),
      get_cap(c("spv", "csp"),         "|Electricity|+|Solar"),
      get_cap(c(windonStr, "windoff"), "|Electricity|+|Wind")
    )

    cap_electricity <- mbind(
      cap_renew_nonBio,
      get_cap(c("igcc", "pc", "coalchp", "igccc"), "|Electricity|+|Coal"),
      get_cap("dot",                               "|Electricity|+|Oil"),
      get_cap(c("ngcc", "ngt", "gaschp", "ngccc"), "|Electricity|+|Gas"),
      get_cap(c("bioigccc", "biochp", "bioigcc"),  "|Electricity|+|Biomass"),
      get_cap(c("tnrs", "fnrs"),                   "|Electricity|+|Nuclear")
    )
  
    if (all(c("h2turbVRE", "h2turb") %in% magclass::getNames(gms_data, dim = 1))) {
      cap_electricity <- mbind(cap_electricity, get_cap(c("h2turb", "h2turbVRE"), "|Electricity|+|Hydrogen"))
    }

    cap_electricity <- mbind(
      cap_electricity,
      setNames(dimSums(cap_electricity, dim = 3), full_name("|Electricity")), # sum of the above
      setNames(dimSums(cap_renew_nonBio, dim = 3), full_name("|Electricity|Non-Biomass Renewables")) # sum of cap_renew_nonBio
    )

    # electricity details
    cap_electricity <- mbind(
      cap_electricity,
      get_cap("igccc",                          "|Electricity|Coal|+|w/ CC"),
      get_cap(c("igcc", "pc", "coalchp"),       "|Electricity|Coal|+|w/o CC"),
      get_cap("igccc",                          "|Electricity|Coal|IGCC|+|w/ CC"),
      get_cap("igcc",                           "|Electricity|Coal|IGCC|+|w/o CC"),
      get_cap("coalchp",                        "|Electricity|Coal|CHP"),
      get_cap("dot",                            "|Electricity|Oil|w/o CC"),
      get_cap("ngccc",                          "|Electricity|Gas|+|w/ CC"),
      get_cap(c("ngcc", "ngt", "gaschp"),       "|Electricity|Gas|+|w/o CC"),
      get_cap("ngccc",                          "|Electricity|Gas|CC|+|w/ CC"),
      get_cap("ngcc",                           "|Electricity|Gas|CC|+|w/o CC"),
      get_cap("gaschp",                         "|Electricity|Gas|CHP"),
      get_cap("ngt",                            "|Electricity|Gas|GT"),
      get_cap(c("bioigccc"),                    "|Electricity|Biomass|w/ CC"),
      get_cap(c("biochp", "bioigcc"),           "|Electricity|Biomass|w/o CC"),
      get_cap("biochp",                         "|Electricity|Biomass|CHP"),
      get_cap(c("biochp", "gaschp", "coalchp"), "|Electricity|CHP"),
      get_cap("spv",                            "|Electricity|Solar|+|PV"),
      get_cap("csp",                            "|Electricity|Solar|+|CSP"),
      get_cap(windonStr,                        "|Electricity|Wind|+|Onshore"),
      get_cap("windoff",                        "|Electricity|Wind|+|Offshore")
    )

    # battery storage
    cap_storage <- mbind(
      get_cap("storspv",                        "|Electricity|Storage|Battery|For PV", factor = 4),
      get_cap(c(storwindonStr, "storwindoff"),  "|Electricity|Storage|Battery|For Wind", factor = 1.2)
    )
    cap_storage <- mbind(cap_storage, setNames(dimSums(cap_storage, dim = 3), full_name("|Electricity|Storage|Battery"))) # sum of the above

    # hydrogen
    cap_hydrogen <- mbind(
      get_cap(c("gash2", "gash2c", "coalh2", "coalh2c"), "|Hydrogen|Fossil"),
      get_cap(c("gash2c", "coalh2c"),                    "|Hydrogen|Fossil|+|w/ CC"),
      get_cap(c("gash2", "coalh2"),                      "|Hydrogen|Fossil|+|w/o CC"),

      get_cap(c("coalh2", "coalh2c"), "|Hydrogen|+|Coal"),
      get_cap(c("coalh2c"),           "|Hydrogen|Coal|+|w/ CC"),
      get_cap(c("coalh2"),            "|Hydrogen|Coal|+|w/o CC"),

      get_cap(c("gash2", "gash2c"),   "|Hydrogen|+|Gas"),
      get_cap(c("gash2c"),            "|Hydrogen|Gas|+|w/ CC"),
      get_cap(c("gash2"),             "|Hydrogen|Gas|+|w/o CC"),

      get_cap(c("bioh2", "bioh2c"),   "|Hydrogen|+|Biomass"),
      get_cap(c("bioh2c"),            "|Hydrogen|Biomass|+|w/ CC"),
      get_cap(c("bioh2"),             "|Hydrogen|Biomass|+|w/o CC"),

      get_cap(c("elh2", "elh2VRE"),   "|Hydrogen|+|Electricity"),
      get_cap(c("elh2", "elh2VRE"),   " (GWel)|Hydrogen|Electricity", factor = 1 / pm_eta_conv[, , "elh2"]), # convert to electric power GWel

      get_cap(c("coalh2", "coalh2c", "gash2", "gash2c", "bioh2", "bioh2c", "elh2", "elh2VRE"), "|Hydrogen") # sum of the above, avoiding double counting
    )


    # heat
    cap_heat <- mbind(
      get_cap("solhe", "|Heat|+|Solar"),
      get_cap("geohe", "|Heat|+|Electricity|Heat Pump"),
      setNames(dimSums(gms_data[, , c("coalhp")], dim = 3)
            + dimSums(gms_data[, , c("coalchp")] * dataoc[, , "pecoal.seel.coalchp.sehe"], dim = 3, na.rm = TRUE), full_name("|Heat|+|Coal")),
      setNames(dimSums(gms_data[, , c("biohp")], dim = 3)
            + dimSums(gms_data[, , c("biochp")] * dataoc[, , "pebiolc.seel.biochp.sehe"], dim = 3, na.rm = TRUE),  full_name("|Heat|+|Biomass")),
      setNames(dimSums(gms_data[, , c("gashp")], dim = 3)
            + dimSums(gms_data[, , c("gaschp")] * dataoc[, , "pegas.seel.gaschp.sehe"], dim = 3, na.rm = TRUE),    full_name("|Heat|+|Gas"))
    )
    cap_heat <- mbind(cap_heat, setNames(dimSums(cap_heat, dim = 3), full_name("|Heat"))) # sum of the above

    # gases
    cap_gas <- mbind(
      get_cap(c("coalgas"),           "|Gases|+|Coal"),
      get_cap(c("gastr"),             "|Gases|+|Natural Gas"),
      get_cap(c("biogas", "biogasc"), "|Gases|+|Biomass"),
      get_cap(c("h22ch4"),            "|Gases|+|Hydrogen")
    )
    cap_gas <- mbind(cap_gas, setNames(dimSums(cap_gas, dim = 3), full_name("|Gases"))) # sum of the above

    # liquids
    cap_liquids <- mbind(
      get_cap(c("coalftrec", "coalftcrec"),                                  "|Liquids|+|Coal"),
      get_cap(c("refliq"),                                                   "|Liquids|+|Oil"),
      get_cap(c("gasftrec", "gasftcrec"),                                    "|Liquids|+|Gas"),
      get_cap(c("bioftrec", "bioftcrec", "biodiesel", "bioeths", "bioethl"), "|Liquids|+|Biomass"),
      get_cap(c("MeOH"),                                                     "|Liquids|+|Hydrogen")
    )
    cap_liquids <- mbind(cap_liquids, setNames(dimSums(cap_liquids, dim = 3), full_name("|Liquids"))) # sum of the above

    cap_liquids <- mbind(
      cap_liquids,
      get_cap(c("refliq", "coalftrec", "coalftcrec"),                        "|Liquids|Fossil")
    )

    # solids
    cap_solids <- mbind(
      get_cap(c("coaltr"),            "|Solids|+|Coal"),
      get_cap(c("biotr", "biotrmod"), "|Solids|+|Biomass")
    )
    cap_solids <- mbind(cap_solids, setNames(dimSums(cap_solids, dim = 3), full_name("|Solids"))) # sum of the above


    reported_cap <- mbind(cap_electricity, cap_storage, cap_hydrogen, cap_heat, cap_gas, cap_liquids, cap_solids)

    # carbon management
    if ("dac" %in% magclass::getNames(gms_data, dim = 1)) {
      unit_dac <- ifelse(prefix == "Cap", " (Mt CO2/yr)", " (Mt CO2/yr/yr)") # finding the relevant unit
      reported_cap <- mbind(reported_cap, setNames(dimSums(gms_data[, , "dac"], dim = 3) * sm_c_2_co2, paste0(prefix,"|Carbon Management|DAC", unit_dac)))
    }

    return(reported_cap)
  }


  ####### calculate parameters for capacity and newly built capacity ############
  reported_cap <- capacity_reporting("Cap")
  reported_new <- capacity_reporting("New Cap")


  # add terms calculated from previously calculated capacity values
  names_capacities <- c("Cap|Electricity|+|Gas (GW)",
                        "Cap|Electricity|+|Nuclear (GW)",
                        "Cap|Electricity|+|Coal (GW)",
                        "Cap|Electricity|+|Biomass (GW)",
                        "Cap|Electricity|+|Hydrogen (GW)",
                        "Cap|Electricity|+|Geothermal (GW)",
                        "Cap|Electricity|+|Oil (GW)")
  names_capacities <- intersect(names_capacities, getNames(reported_cap))

  reported_cap <- mbind(reported_cap,
    setNames(dimSums(reported_cap[, , names_capacities], dim = 3) +
                     0.6 * reported_cap[, , "Cap|Electricity|+|Hydro (GW)"] +
                     reported_cap[, , "Cap|Electricity|Storage|Battery (GW)"],
                     "Cap|Electricity|Estimated firm capacity counting hydro at 0p6 (GW)"))


  # Idle capacities and Total (sum of operating and idle)
  reported_cap <- mbind(reported_cap, setNames(dimSums(vm_cap[, , "igcc"], dim = 3)    * v_earlyreti[, , "igcc"]    / (1 - v_earlyreti[, , "igcc"]) +
                                       dimSums(vm_cap[, , "coalchp"], dim = 3) * v_earlyreti[, , "coalchp"] / (1 - v_earlyreti[, , "coalchp"]) +
                                       dimSums(vm_cap[, , "pc"], dim = 3)      * v_earlyreti[, , "pc"]      / (1 - v_earlyreti[, , "pc"]),
                                       "Idle Cap|Electricity|Coal|w/o CC (GW)"))
  reported_cap <- mbind(reported_cap, setNames(dimSums(vm_cap[, , "dot"], dim = 3) * v_earlyreti[, , "dot"] / (1 - v_earlyreti[, , "dot"]),
                                       "Idle Cap|Electricity|Oil|w/o CC (GW)"))
  reported_cap <- mbind(reported_cap, setNames(dimSums(vm_cap[, , "ngcc"], dim = 3)   * v_earlyreti[, , "ngcc"]   / (1 - v_earlyreti[, , "ngcc"]) +
                                       dimSums(vm_cap[, , "gaschp"], dim = 3) * v_earlyreti[, , "gaschp"] / (1 - v_earlyreti[, , "gaschp"]) +
                                       dimSums(vm_cap[, , "ngt"], dim = 3)    * v_earlyreti[, , "ngt"]    / (1 - v_earlyreti[, , "ngt"]),
                                       "Idle Cap|Electricity|Gas|w/o CC (GW)"))
  reported_cap <- mbind(reported_cap,
    setNames(reported_cap[, , "Idle Cap|Electricity|Coal|w/o CC (GW)"] + reported_cap[, , "Cap|Electricity|Coal|+|w/o CC (GW)"], "Total Cap|Electricity|Coal|w/o CC (GW)"),
    setNames(reported_cap[, , "Idle Cap|Electricity|Gas|w/o CC (GW)"]  + reported_cap[, , "Cap|Electricity|Gas|+|w/o CC (GW)"],  "Total Cap|Electricity|Gas|w/o CC (GW)"))


  # Cumulative capacities = cumulating new capacities, starting with 0 in 2005
  reported_cumul <- reported_new[, t2005, ]
  getSets(reported_cumul)[3] <- "variable"
  reported_cumul <- quitte::as.quitte(reported_cumul)
  mylist <- lapply(levels(reported_cumul$variable), function(x) {
    calcCumulatedDiscount(data = reported_cumul %>%
                            filter(.data$variable == x),
                          nameVar = x,
                          discount = 0.0) %>%
      mutate(variable = gsub("New", replacement = "Cumulative", x))
  })
  reported_cumul <- do.call("rbind", mylist)
  reported_cumul <- as.magpie(quitte::as.quitte(reported_cumul))
  magclass::getNames(reported_cumul) <- paste0( # append unit
    magclass::getNames(reported_cumul),
    ifelse(
      grepl("Cumulative Cap\\|Carbon Management", magclass::getNames(reported_cumul)),
      " (Mt CO2/yr)", 
      " (GW)"
    )
  )


  reported <- mbind(reported_cap[, t2005, ], reported_new[, t2005, ], reported_cumul)
  # add global values
  reported <- mbind(reported, dimSums(reported, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    reported <- mbind(reported, calc_regionSubset_sums(reported, regionSubsetList))

  getSets(reported)[3] <- "variable"

  return(reported)
}

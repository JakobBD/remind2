#' Read in GDX and calculate LCOE reporting used in convGDX2MIF_LCOE.
#'
#' This function provides a post-processing calculation of LCOE (Levelized Cost
#' of Energy) for energy conversion technologies in REMIND.
#' It includes most technologies that generate secondary energy and the distribution
#' technologies which convert secondary energy to final energy.
#' This script calculates two different types of LCOE: average LCOE (standing system)
#' and marginal LCOE (new plant).
#' The average LCOE reflect the total cost incurred by the technology deployment
#' in a specific time step divided by its energy output.
#' The marginal LCOE estimate the per-unit lifetime cost of the output if the model
#' added another capacity of that technology in the respective time step.
#' The marginal LCOE are calculate in two versions: 1) with fuel prices and co2
#' taxes of the time step for which the LCOE are calculated (time step prices),
#' or 2) with an intertemporal weighted-average of fuel price and co2 taxes over
#' the lifetime of the plant (intertemporal prices).
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param output.type string to determine which output shall be produced.
#' Can be either "average" (returns only average LCOE),
#' "marginal" (returns only marginal LCOE), "both" (returns marginal and average LCOE) and
#' and "marginal detail" (returns table to trace back how marginal LCOE are calculated).
#' @return MAgPIE object - LCOE calculated by model post-processing.
#' Two types a) standing system LCOE b) new plant LCOE.
#' @author Felix Schreyer, Robert Pietzcker, Lavinia Baumstark
#' @seealso \code{\link{convGDX2MIF_LCOE}}
#' @examples
#' \dontrun{
#' reportLCOE(gdx)
#' }
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie getRegions getYears getNames setNames clean_magpie dimReduce as.magpie magpie_expand
#' @importFrom dplyr %>% mutate select rename group_by ungroup right_join filter full_join arrange summarise
#' @importFrom quitte as.quitte overwrite getRegs getPeriods
#' @importFrom tidyr spread gather expand fill

reportLCOE <- function(gdx, output.type = "both") {
  # test whether output.type defined
  if (!output.type %in% c("marginal", "average", "both", "marginal detail")) {
    print("Unknown output type. Please choose either marginal, average, both or marginal detail.")
    return(new.magpie(cells_and_regions = "GLO",
                      years = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)))
  }

  # check whether key variables are there
  # LCOE reporting does not make sense for old gdx
  # where variables are missing and model structure is different

  vm_capFac <- readGDX(gdx, "vm_capFac", field = "l", restore_zeros = FALSE)
  qm_balcapture  <- readGDX(gdx, "q_balcapture", field = "m", restore_zeros = FALSE)
  vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = FALSE)
  vm_co2capture <- readGDX(gdx, c("vm_co2capture","v_co2capture"), field = "l", restore_zeros = FALSE)
  pm_emifac <- readGDX(gdx, "pm_emiFac", field = "l", restore_zeros = FALSE)
  v32_storloss <- readGDX(gdx, "v32_storloss", field = "l")

  if (is.null(vm_capFac) || is.null(qm_balcapture) || is.null(vm_co2CCS) ||
      is.null(pm_emifac) || is.null(v32_storloss)) {
    print("The gdx file is too old for generating a LCOE reporting...returning NULL")
    return(new.magpie(cells_and_regions = "GLO",
                      years = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)))
  }


  # get module realizations
  module2realisation <- readGDX(gdx, "module2realisation")
  rownames(module2realisation) <- module2realisation$modules


  # initialize output array
  LCOE.out <- NULL


  # read in general data (needed for average and marginal LCOE calculation)
  s_twa2mwh <- readGDX(gdx, c("sm_TWa_2_MWh", "s_TWa_2_MWh", "s_twa2mwh"), format = "first_found")
  s_GtC2tCO2 <-  10^9 * readGDX(gdx, c("sm_c_2_co2", "s_c_2_co2"), format = "first_found")
  s_usd2017t2015 <- GDPuc::convertSingle(1, iso3c = "USA", unit_in = "constant 2017 US$MER",
                                         unit_out = "constant 2015 US$MER")

  ttot     <- as.numeric(readGDX(gdx, "ttot"))
  ttot_before2005 <- paste0("y", ttot[which(ttot <= 2000)])
  ttot_from2005 <- paste0("y", ttot[which(ttot >= 2005)])
  te        <- readGDX(gdx, "te")
  te <- te[!te %in% c("lng_liq", "gas_pipe", "lng_gas", "lng_ves", "coal_ves", "pipe_gas", "termX_lng", "termM_lng", "vess_lng")]
  p_priceCO2 <- readGDX(gdx, name = c("p_priceCO2", "pm_priceCO2"), format = "first_found") # co2 price


  ## equations
  qm_pebal  <- readGDX(gdx, name = c("q_balPe"), field = "m", format = "first_found")
  qm_budget <- readGDX(gdx, name = c("qm_budget"), field = "m", format = "first_found")

  ## variables
  vm_prodSe  <- readGDX(gdx, name = c("vm_prodSe"), field = "l", restore_zeros = FALSE, format = "first_found")



  #### A) Calculation of average (standing system) LCOE ----

  if (output.type %in% c("both", "average")) {
    # read in needed data ----

    ## sets
    opTimeYr <- readGDX(gdx, "opTimeYr")
    opTimeYr2te   <- readGDX(gdx, "opTimeYr2te")
    temapse  <- readGDX(gdx, "en2se")
    teall2rlf <- readGDX(gdx, c("te2rlf", "teall2rlf"), format = "first_found")
    te2stor   <- readGDX(gdx, "VRE2teStor")
    te2grid   <- readGDX(gdx, "VRE2teGrid")
    teVRE   <- readGDX(gdx, "teVRE")
    # exclude "windoff" from teVRE as "windoff" does not have separate grid, storage technologies
    if ("windoff" %in% as.vector(teVRE)) {
      teVRE <- as.vector(teVRE)
      teVRE <- teVRE[teVRE != "windoff"]
    }

    se2fe     <- readGDX(gdx, "se2fe")
    pe2se     <- readGDX(gdx, "pe2se")
    teCCS     <- readGDX(gdx, "teCCS")
    teReNoBio <- readGDX(gdx, "teReNoBio")
    teCDR     <- readGDX(gdx, "te_used33")
    EW_name   <- "weathering" # necessary for backward compatibility

    pc2te <- readGDX(gdx, "pc2te") # mapping of couple production & consumption

    ## parameter
    p_omeg  <- readGDX(gdx, c("pm_omeg", "p_omeg"), format = "first_found")
    p_omeg  <- p_omeg[opTimeYr2te]
    pm_ts   <- readGDX(gdx, "pm_ts")
    pm_data <- readGDX(gdx, "pm_data")
    pm_emifac <- readGDX(gdx, "pm_emifac", restore_zeros = FALSE) # emission factor per technology
    pm_eta_conv <- readGDX(gdx, "pm_eta_conv", restore_zeros = FALSE) # efficiency oftechnologies with time-dependent eta
    pm_dataeta <- readGDX(gdx, "pm_dataeta", restore_zeros = FALSE) # efficiency of technologies with time-independent eta
    p47_taxCO2eq_AggFE <- readGDX(gdx, "p47_taxCO2eq_AggFE", restore_zeros = FALSE, react = "silent")

    pm_prodCouple <- readGDX(gdx, "pm_prodCouple", restore_zeros = FALSE) # Second fuel production or demand per unit output of technology. Negative values mean own consumption, positive values mean coupled product.
    pm_PEPrice <- readGDX(gdx, "pm_PEPrice", restore_zeros = FALSE)
    pm_SEPrice <- readGDX(gdx, "pm_SEPrice", restore_zeros = FALSE)

    ## variables

    ## Total direct Investment Cost in Timestep
    vm_costInvTeDir <- readGDX(gdx, name = c("vm_costInvTeDir", "v_costInvTeDir", "v_directteinv"), field = "l", format = "first_found")[, ttot, ]

    ## total adjustment cost in period
    vm_costInvTeAdj <- readGDX(gdx, name = c("vm_costInvTeAdj", "v_costInvTeAdj"), field = "l", format = "first_found")[, ttot, ]

    # capacity additions per year
    vm_deltaCap <- readGDX(gdx, name = c("vm_deltaCap"), field = "l", format = "first_found")[, ttot, ]

    vm_demPe      <- readGDX(gdx, name = c("vm_demPe", "v_pedem"), field = "l", restore_zeros = FALSE, format = "first_found")
    v_investcost  <- readGDX(gdx, name = c("vm_costTeCapital", "v_costTeCapital", "v_investcost"), field = "l", format = "first_found")[, ttot, ]
    vm_cap        <- readGDX(gdx, name = c("vm_cap"), field = "l", format = "first_found")
    vm_prodFe     <- readGDX(gdx, name = c("vm_prodFe"), field = "l", restore_zeros = FALSE, format = "first_found")
    v_emiTeDetail <- readGDX(gdx, name = c("vm_emiTeDetail", "v_emiTeDetail"), field = "l", restore_zeros = FALSE, format = "first_found")
    vm_emiIndCCS <- readGDX(gdx, name = c("vm_emiIndCCS", "v_emiIndCCS"), field = "l", restore_zeros = FALSE, format = "first_found")
    vm_emiCdrTeDetail <- readGDX(gdx, c("vm_emiCdrTeDetail", "v33_emi"), field = "l", restore_zeros = FALSE, react = "silent")[, ttot_from2005, teCDR]
    if (is.null(vm_emiCdrTeDetail)) { # compatibility with the CDR module before the portfolio was added
      # captured CO2 by DAC
      v33_emiDAC <- readGDX(gdx, "v33_emiDAC", field = "l", restore_zeros = FALSE, react = "silent")[, ttot_from2005, ]
      if (!is.null(v33_emiDAC)) {
        teCDR <- c(teCDR, "dac")
      }
      # captured CO2 by Enhanced Weathering
      v33_emiEW <- readGDX(gdx, "v33_emiEW", field = "l", restore_zeros = FALSE, react = "silent")
      if (!is.null(v33_emiEW)) {
        v33_emiEW <- add_columns(v33_emiEW, addnm = c("y2005", "y2010", "y2015", "y2020"), dim = 2, fill = 0)[, ttot_from2005, ]
        EW_name <- intersect(c("rockgrind", "weathering"), te)
        teCDR <- c(teCDR, EW_name)
      }
      # variable used in the rest of the reporting
      vm_emiCdrTeDetail <- mbind(v33_emiDAC, v33_emiEW)
      vm_emiCdrTeDetail <- setNames(vm_emiCdrTeDetail, c("dac", EW_name))
    }

    # module-specific
    # amount of curtailed electricity
    if (module2realisation["power", 2] == "RLDC") {
      v32_curt <- readGDX(gdx, name = c("v32_curt"), field = "l", restore_zeros = FALSE, format = "first_found")
    } else if (module2realisation["power", 2] %in% c("IntC", "DTcoup")) {
      v32_curt <- v32_storloss[, ttot_from2005, getNames(vm_prodSe, dim = 3)]
    } else {
      v32_curt <- 0
    }

    # dac FE demand
    v33_FEdemand <- readGDX(gdx, name = "v33_FEdemand", field = "l", restore_zeros = FALSE, format = "first_found")[, ttot_from2005, teCDR]
    DAC_ccsdemand <- readGDX(gdx, name = c("vm_co2emi_cdrFE_beforeCapture","v33_co2emi_non_atm_gas"), field = "l", restore_zeros = FALSE, format = "first_found")[, ttot_from2005, "dac"]
    pm_FEPrice <- readGDX(gdx, "pm_FEPrice")[, ttot_from2005, "indst.ETS"]
    fe2cdr <- readGDX(gdx, name = "fe2cdr")
    if (!is.null(fe2cdr)) {
      fe2cdr <- fe2cdr %>% filter(.data$all_te %in% teCDR)
    }

    discount_rate <- 0.05

    # calculates annuity cost:
    # annuity cost = 1/ sum_t (p_omeg(t) / (1+discount_rate)^t)  * direct investment cost
    # t is in T which is the lifetime of the technology
    # direct investment cost = directteinv or for past values (before 2005) (v_investcost * deltaCap)
    # annuity represents (total investment cost + interest over lifetime) distributed equally over all years of lifetime

    # # quick fix for h22ch4 problem
    # if (! "h22ch4" %in% magclass::getNames(p_omeg,dim=2)) {
    # te <- te[te!="h22ch4"]
    # }

    # get a representative region
    reg1 <- getRegions(vm_prodSe)[1]

    te_annuity <- new.magpie("GLO", names = magclass::getNames(p_omeg, dim = 2))
    for (a in magclass::getNames(p_omeg[reg1, , ], dim = 2)) {
      te_annuity[, , a] <- 1 / dimSums(p_omeg[reg1, , a] / (1 + discount_rate)**as.numeric(magclass::getNames(p_omeg[reg1, , a], dim = 1)), dim = 3.1)
    }

    te_inv_annuity <- 1e+12 * te_annuity[, , te] *
      mbind(
        v_investcost[, ttot_before2005, te] * dimSums(vm_deltaCap[teall2rlf][, ttot_before2005, te], dim = 3.2),
        vm_costInvTeDir[, ttot_from2005, te]
      )

    te_inv_annuity_wadj <- 1e+12 * te_annuity[, , te] *
      mbind(
        v_investcost[, ttot_before2005, te] * dimSums(vm_deltaCap[teall2rlf][, ttot_before2005, te], dim = 3.2),
        vm_costInvTeAdj[, ttot_from2005, te] + vm_costInvTeDir[, ttot_from2005, te]
      )

    # average LCOE components ----

    #  calculate sub-parts of "COSTS"
    # LCOE calculation only for pe2se technologies so far!

    # 1. sub-part: Investment Cost ----

    # AnnualInvCost(t) = sum_(td) [annuity(td) * p_pmeg(t-td+1) * deltaT],
    # where td in [t-lifetime,t]

    # p_pmeg(t-td+1) = 1 for td = t (measure of how many capacities still standing from past investments)
    # p_pmeg(t-td+1) = 0 for td = t-lifetime

    # where t is the year for which the investment cost are calculated and
    # td are all past time steps before t back to t-lifetime of the technology
    # so: annuity cost incurred over past lifetime years are are divided by current production
    # (annuity cost = discounted investment cost spread over lifetime)



    # take 74 tech from p_omeg, although in v_direct_in 114 in total
    te_annual_inv_cost <- new.magpie(getRegions(te_inv_annuity[, ttot, ]), getYears(te_inv_annuity[, ttot, ]), magclass::getNames(te_inv_annuity[, ttot, ]))
    # loop over ttot
    for (t0 in ttot) {
      for (a in magclass::getNames(te_inv_annuity)) {
        # all ttot before t0
        t_id <- ttot[ttot <= t0]
        # only the time (t_id) within the opTimeYr of a specific technology a
        t_id <- t_id[t_id >= (t0 - max(as.numeric(opTimeYr2te$opTimeYr[opTimeYr2te$all_te == a])) + 1)]
        p_omeg_h <- new.magpie(getRegions(p_omeg), years = t_id, names = a)
        for (t_id0 in t_id) {
          p_omeg_h[, t_id0, a] <- p_omeg[, , a][, , t0 - t_id0 + 1]
        }
        te_annual_inv_cost[, t0, a] <- dimSums(pm_ts[, t_id, ] * te_inv_annuity[, t_id, a] * p_omeg_h[, t_id, a], dim = 2)
      } # a
    }  # t0

    te_annual_inv_cost_wadj <- new.magpie(getRegions(te_inv_annuity_wadj[, ttot, ]), getYears(te_inv_annuity_wadj[, ttot, ]), magclass::getNames(te_inv_annuity_wadj[, ttot, ]))
    # loop over ttot
    for (t0 in ttot) {
      for (a in magclass::getNames(te_inv_annuity_wadj)) {
        # all ttot before t0
        t_id <- ttot[ttot <= t0]
        # only the time (t_id) within the opTimeYr of a specific technology a
        t_id <- t_id[t_id >= (t0 - max(as.numeric(opTimeYr2te$opTimeYr[opTimeYr2te$all_te == a])) + 1)]
        p_omeg_h <- new.magpie(getRegions(p_omeg), years = t_id, names = a)
        for (t_id0 in t_id) {
          p_omeg_h[, t_id0, a] <- p_omeg[, , a][, , t0 - t_id0 + 1]
        }
        te_annual_inv_cost_wadj[, t0, a] <- dimSums(pm_ts[, t_id, ] * te_inv_annuity_wadj[, t_id, a] * p_omeg_h[, t_id, a], dim = 2)
      } # a
    }  # t0

    # 2. sub-part: fuel cost ----

    # 2.1 primary fuel cost = PE price * PE demand of technology

    te_annual_fuel_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)
    te_annual_fuel_cost[, , pe2se$all_te] <- setNames(1e+12 * qm_pebal[, ttot_from2005, pe2se$all_enty] / qm_budget[, ttot_from2005, ] *
                                                        setNames(vm_demPe[, , pe2se$all_te], pe2se$all_enty), pe2se$all_te)

    # 2.2 secondary fuel cost
    Fuel.Price <- mbind(pm_PEPrice, pm_SEPrice)[, , ] * 1e12 # convert from trUSD/TWa to USD/TWa [note: this already includes the CO2 price]
    Fuel.Price <- magclass::matchDim(Fuel.Price, vm_prodSe, dim = c(1), fill = 0)

    pm_SecFuel <- pm_prodCouple[, , getNames(pm_prodCouple)[pm_prodCouple[reg1, , ] < 0]] # keep only second fuel consumption, not co-production
    SecFuelTechs <- intersect(getNames(pm_SecFuel, dim = 3), pc2te$all_te) # determine all te that have couple production
    SecFuelTechs_pe2se <- intersect(SecFuelTechs, pe2se$all_te)

    te_annual_secFuel_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, getNames(te_inv_annuity), fill = 0)
    # calculate secondary fuel cost for pe2se
    te_annual_secFuel_cost[, , SecFuelTechs_pe2se] <- setNames(dimSums(-pm_SecFuel[, , SecFuelTechs_pe2se] * Fuel.Price[, ttot_from2005, getNames(pm_SecFuel, dim = 4)] *
                                                                         vm_prodSe[, ttot_from2005, SecFuelTechs_pe2se], dim = 3.4), SecFuelTechs_pe2se)
    # calculate secondary fuel cost for ccsinje
    te_annual_secFuel_cost[, , "ccsinje"] <- setNames(-pm_SecFuel[, , "ccsinje"] * Fuel.Price[, , "seel"] * vm_co2CCS[, , "ccsinje.1"], "ccsinje")
    # calculation explanation:
    # units: -1 (so pm_SecFuel turns positive because consuming energy)
    # * electricity or heat demand (pm_SecFuel, TWa_input/TWa_mainOutput OR TWa/GtC)
    # * electricity price (Fuel.Price, USD2017/TWa_inpu)
    # * main Output (for pe2se: vm_prodSe (TWa_mainOutput); for ccsinje: amount of CO2 captured (vm_co2CCS, GtC))
    # = te_annual_secFuel_cost = [USD2017]

    # 2.3 additional fuel demand of CDR module technologies
    te_annual_otherFuel_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, getNames(te_inv_annuity), fill = 0)
    if (length(teCDR) > 0 && !is.null(v33_FEdemand)) {  # i.e. this is only computed for remind post refactoring of cdr module (Nov 2024)
      for (te in teCDR) {
        te_annual_otherFuel_cost[, ttot_from2005, te] <- setNames(dimSums(
          1e+12 * setNames(pm_FEPrice[, , unique(fe2cdr$all_enty)], unique(fe2cdr$all_enty)) *
            setNames(dimSums(v33_FEdemand[, , te], dim = 3.2), unique(getNames(v33_FEdemand[, , te], dim = 1)))),
          te)
      }
    }
    # calculation explanation: 10^12 USD/TrnUSD * pm_FEPrice(TrnUSD/TWa) * FEdemand(TWa) = USD

    # 3. sub-part: OMV cost ----

    # omv cost (from pm_data) * SE production


    temapse.names <- apply(temapse, 1, function(x) paste0(x, collapse = "."))
    te_annual_OMV_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)
    te_annual_OMV_cost[, , temapse$all_te] <- 1e+12 * collapseNames(pm_data[, , "omv"])[, , temapse$all_te] * setNames(vm_prodSe[, , temapse.names], temapse$all_te)

    # 4. sub-part: OMF cost ----

    # omf cost = omf (from pm_data) * investcost (trUSD/TW) * capacity
    # omf in pm_data given as share of investment cost


    te_annual_OMF_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)

    te_annual_OMF_cost[, , getNames(te_inv_annuity)] <- 1e+12 * collapseNames(pm_data[, , "omf"])[, , getNames(te_inv_annuity)] * v_investcost[, ttot_from2005, getNames(te_inv_annuity)] *
      dimSums(vm_cap[, ttot_from2005, ], dim = 3.2)[, , getNames(te_inv_annuity)]

    # 5. sub-part: storage cost (for wind, spv, csp) ----

    # storage cost = investment cost + omf cost
    # of corresponding storage technology ("storwind", "storspv", "storcsp")
    # clarify: before, they used omv cost here, but storage, grid etc. does not have omv...instead, we use omf now!

    te_annual_stor_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)
    te_annual_stor_cost[, , te2stor$all_te] <- setNames(te_annual_inv_cost[, ttot_from2005, te2stor$teStor] +
                                                          te_annual_OMF_cost[, , te2stor$teStor], te2stor$all_te)

    te_annual_stor_cost_wadj <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)
    te_annual_stor_cost_wadj[, , te2stor$all_te] <- setNames(te_annual_inv_cost_wadj[, ttot_from2005, te2stor$teStor] +
                                                               te_annual_OMF_cost[, , te2stor$teStor], te2stor$all_te)


    # 6. sub-part: grid cost ----

    # same as for storage cost only with grid technologies: "gridwind", "gridspv", "gridcsp"
    # only "gridwind" technology active, wind requires 1.5 * the gridwind capacities as spv and csp

    grid_factor_tech <- new.magpie(names = te2grid$all_te, fill = 1)
    getSets(grid_factor_tech)[3] <- "all_te"
    if ("wind" %in% getNames(grid_factor_tech)) {
      grid_factor_tech[, , "wind"] <- 1.5
    }
    grid_factor_tech[, , "windon"] <- 1.5
    grid_factor_tech[, , "windoff"] <- 3.0

    vm_VRE_prodSe_grid <- dimSums(grid_factor_tech * vm_prodSe[, , te2grid$all_te])
    te_annual_grid_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)
    te_annual_grid_cost_wadj <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, magclass::getNames(te_inv_annuity), fill = 0)


    gridwindonStr <- ifelse("windon" %in% te2grid$all_te, "gridwindon", "gridwind")

    te_annual_grid_cost[, , te2grid$all_te] <-
      collapseNames(te_annual_inv_cost[, ttot_from2005, gridwindonStr] + te_annual_OMF_cost[, , gridwindonStr]) *
      1 / vm_VRE_prodSe_grid *
      # this multiplcative factor is added to reflect higher grid demand of wind, see q32_limitCapTeGrid
      grid_factor_tech * vm_prodSe[, , te2grid$all_te]

    te_annual_grid_cost_wadj[, , te2grid$all_te] <-
      collapseNames(te_annual_inv_cost_wadj[, ttot_from2005, gridwindonStr] + te_annual_OMF_cost[, , gridwindonStr]) *
      1 / vm_VRE_prodSe_grid *
      # this multiplcative factor is added to reflect higher grid demand of wind, see q32_limitCapTeGrid
      grid_factor_tech * vm_prodSe[, , te2grid$all_te]

    # 7. sub-part: ccs injection cost (for technologies capturing CO2) ----

    # same as for storage/grid but with ccs inejection technology
    # distributed to technolgies according to share of total captured co2 of ccs technology

    # (annual ccsinje investment + annual ccsinje omf) * captured co2 (tech) / total captured co2 of all tech

    te_annual_ccsInj_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, getNames(te_inv_annuity), fill = 0)
    te_annual_ccsInj_inclAdjCost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005, getNames(te_inv_annuity), fill = 0)


    # calculate total ccsinjection cost for all techs
    total_ccsInj_cost <- dimReduce(te_annual_inv_cost[getRegions(te_annual_OMF_cost), getYears(te_annual_OMF_cost), "ccsinje"] +
                                     te_annual_OMF_cost[, , "ccsinje"] + te_annual_secFuel_cost[, , "ccsinje"])

    total_ccsInj_inclAdjCost <- dimReduce(te_annual_inv_cost_wadj[getRegions(te_annual_OMF_cost), getYears(te_annual_OMF_cost), "ccsinje"] +
                                            te_annual_OMF_cost[, , "ccsinje"] +
                                            te_annual_secFuel_cost[, , "ccsinje"])
    # all captured co2 by tech: pe2se and cdr and industry
    cco2_byTech <-  mbind(dimSums(v_emiTeDetail[, , "cco2"][, ttot_from2005, teCCS], dim = c(3.1, 3.2, 3.4), na.rm = TRUE),
                          setNames(DAC_ccsdemand, "dac"), vm_emiIndCCS[, ttot_from2005, ])
    cco2Techs <- intersect(getNames(cco2_byTech), getNames(te_inv_annuity))


    # distribute ccs injection cost over techs
    te_annual_ccsInj_cost[, , cco2Techs] <- setNames(total_ccsInj_cost * cco2_byTech[, , cco2Techs] / vm_co2capture,
                                                     cco2Techs)
    te_annual_ccsInj_inclAdjCost[, , cco2Techs] <- setNames(total_ccsInj_inclAdjCost * cco2_byTech[, , cco2Techs] / vm_co2capture,
                                                            cco2Techs)



    # 8. sub-part: co2 cost ----

    te_annual_co2_cost <- new.magpie(getRegions(te_inv_annuity), ttot_from2005,
                                     getNames(te_inv_annuity), fill = 0)

    # global part of the CO2 price
    # limit technology names to those existing in both te_inv_annuity and
    # v_emiTeDetail
    tmp <- intersect(getNames(te_inv_annuity),
                     getNames(v_emiTeDetail[, , "co2"], dim = 3))
    te_annual_co2_cost[, , tmp] <- setNames(
      (p_priceCO2[, getYears(v_emiTeDetail), ]
       * dimSums(v_emiTeDetail[, , tmp], dim = c(3.1, 3.2, 3.4), na.rm = TRUE)
       * 1e9   # $/tC * GtC/yr * 1e9 t/Gt = $/yr
      ),
      tmp)
    rm(tmp)

    # regional part of the CO2 price (p47_taxCO2eq_AggFE only exits when regipol is run)
    if (!is.null(p47_taxCO2eq_AggFE)) {
      tmp <- intersect(getNames(te_inv_annuity),
                       getNames(v_emiTeDetail[, , "co2"], dim = 3))

      te_annual_co2_cost[, getYears(p47_taxCO2eq_AggFE), tmp] <- setNames(
        (dimSums(p47_taxCO2eq_AggFE, dim = 3, na.rm = TRUE)
         * dimSums(v_emiTeDetail[, getYears(p47_taxCO2eq_AggFE), tmp], dim = c(3.1, 3.2, 3.4), na.rm = TRUE)
         * 1e9 * 1e3  # T$/tC * GtC/yr * 1e9 t/Gt 1e3$/T$ = $/yr
        ),
        tmp)

      rm(tmp)
    }

    # 9. sub-part: curtailment cost ----
    # (only relevant for RLDC version!)

    # note: this step can only be done after all the other parts have already been calcuated!

    # curtailment cost = total  annual cost / (production * (1-curt_share)) - total annual cost / production
    # total annual cost = sum of all previous annual cost
    # curt_share = share of curtailed electricity relative to total generated electricity

    te_curt_cost <- new.magpie(getRegions(te_annual_fuel_cost), getYears(te_annual_fuel_cost), getNames(te_annual_fuel_cost), fill = 0)

    # calculate curtailment share of total generation
    curt_share <- v32_curt[, , teVRE] / vm_prodSe[, , teVRE]

    # calculate total annual cost (without curtailment cost) as sum of previous parts (excl. grid and storage cost)
    te_annual_total_cost_noCurt <- new.magpie(getRegions(te_annual_fuel_cost), getYears(te_annual_fuel_cost), getNames(te_annual_fuel_cost))

    te_annual_total_cost_noCurt <- te_annual_inv_cost[, getYears(te_annual_fuel_cost), ] +
      te_annual_fuel_cost +
      te_annual_OMV_cost  +
      te_annual_OMF_cost  +
      te_annual_ccsInj_cost +
      te_annual_co2_cost

    ###### 10. sub-part: Additional Enhanced Weathering data & calculations
    if (EW_name %in% teCDR) {
      # read amount of rock spread
      v33_EW_onfield <- readGDX(gdx, c("v33_EW_onfield", "v33_grindrock_onfield"), restore_zeros = FALSE, field = "l", format = "first_found")[, ttot_from2005, ]
      v33_EW_onfield_sum <- dimSums(v33_EW_onfield, dim = 3)

      # EW-specific fixed OM cost are given by vm_omcosts_cdr. This cumulates a) completely fixed cost for mining, grinding and spreading; and b) fixed transportation cost that depend on the distance grade.
      # To allow interpretation of the LC, separate these cost components.
      s33_costs_fix <- readGDX(gdx, "s33_costs_fix")
      p33_EW_transport_costs <- readGDX(gdx, c("p33_EW_transport_costs", "p33_transport_costs"), format = "first_found")

      EW_fixed_other_cost <- 10^12 * s33_costs_fix * v33_EW_onfield_sum
      EW_fixed_transport_cost <-  10^12 *  dimSums(p33_EW_transport_costs[, , getNames(v33_EW_onfield)] * v33_EW_onfield)
    }

    ####### 11. sub-part: calculate total energy production and carbon storage  #################################
    # a) production volumes to divide by:
    # for LCO-ccsinje: change unit of stored CO2 from GtC to tCO2
    vm_co2CCS_tCO2 <- vm_co2CCS * s_GtC2tCO2

    # for LCO-cc: calculate captured CO2 to calculate cost per tCO2
    cco2_byTech_tCO2 <-  cco2_byTech * s_GtC2tCO2
    cco2_byTech_tCO2[cco2_byTech_tCO2 < 1000] <- NA   # set to NA if very small (<1000tCO2/yr) to avoid very large cost parts & weird plots

    te_cco2 <- intersect(getNames(cco2_byTech_tCO2), getNames(te_inv_annuity)) # Technologies that capture co2 to enter ccus system, no matter which source

    # for LCO-sc: calculate stored CO2 to calculate cost per tCO2
    cdrco2_byTech_tCO2 <- vm_emiCdrTeDetail[, , setdiff(teCDR, te_cco2)] * s_GtC2tCO2 * -1 # will become relevant when biochar included

    # enhanced weathering:
    if (EW_name %in% teCDR) {
      # The calculation of cost is associated with the spreading of rocks in a time step. However, the removal induced thereby is spread across time steps.
      # Thus, we need to calculate the total removal induced through spreading a given amount of rock as reference value for the cost incurred in that time step.
      s33_co2_rem_pot <- readGDX(gdx, "s33_co2_rem_pot")
      EW_induced_in_tCO2 <- dimSums(v33_EW_onfield * s33_co2_rem_pot * s_GtC2tCO2, dim = 3) # here grades do not matter because the overall removal depends on the type of stone and not grade
      # [Gt stone] * [GtC/GtStone] * [tCO2/GtC]
      EW_induced_in_tCO2[EW_induced_in_tCO2 == 0] <- NA # set NA to avoid infinite investment cost for the standing system when regions do not spread EW in time steps after initial investment was taken
      cdrco2_byTech_tCO2[, , EW_name] <- EW_induced_in_tCO2[, ttot_from2005, ]
      # Note that there is an alternative way to allocate the cost which may be added in the future.
    }

    te_sco2 <- getNames(cdrco2_byTech_tCO2)

    # for pe2se: SE and FE production in MWh
    total_te_energy <- new.magpie(getRegions(vm_prodSe), getYears(vm_prodSe),
                                  c(magclass::getNames(collapseNames(vm_prodSe[temapse], collapsedim = c(1, 2))),
                                    magclass::getNames(collapseNames(vm_prodFe[se2fe], collapsedim = c(1, 2)))))
    total_te_energy[, , temapse$all_te] <- s_twa2mwh * setNames(vm_prodSe[, , temapse.names], temapse$all_te)
    total_te_energy[, , se2fe$all_te]   <- s_twa2mwh * vm_prodFe[, , se2fe$all_te]

    # set total energy to NA if it is very small (< 100 MWh/yr),
    # this avoids very large or infinite cost parts and weird plots
    total_te_energy[total_te_energy < 100] <- NA

    # b) calculate curtailment cost & total energy production after curtailment
    te_curt_cost[, , teVRE] <- te_annual_total_cost_noCurt[, , teVRE] / (total_te_energy[, , teVRE] * (1 - curt_share[, , teVRE])) -
      te_annual_total_cost_noCurt[, , teVRE] / total_te_energy[, , teVRE]

    # calculate total energy production after curtailment
    total_te_energy_usable <- total_te_energy
    total_te_energy_usable[, , teVRE] <- total_te_energy[, , teVRE] - v32_storloss[, ttot_from2005, teVRE] * s_twa2mwh



    ####################################################
    ######### calculate average LCOE  ##################
    ####################################################
    LCOE.avg <- NULL

    # calculate standing system LCOE
    # - for pe2se: divide total cost of standing system in that time step by total generation (before curtailment) in that time step
    #              exception: grid and storage cost are calculated by dividing by generation after curtailment
    # - carbon transport & storage: cost are calculated by dividing by tons of CO2 that are stored
    # - technologies capturing carbon: total cost are  divided by tons of CO2 captured.
    #               Adding cost for stored carbon, this yields full CCS or CDR cost (depending on source of C)
    # convert from USD2017/MWh (or tCO2) to USD2015/MWh (or tCO2)


    LCOE.avg <- mbind(
      #### Energy technologies (pe2se$all_te)
      ### production cost components
      setNames(te_annual_inv_cost[, ttot_from2005, pe2se$all_te] /
                 total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Investment Cost")),
      setNames(te_annual_inv_cost_wadj[, ttot_from2005, pe2se$all_te] /
                 total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Investment Cost w/ Adj Cost")),
      setNames(te_annual_fuel_cost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Fuel Cost")),
      setNames(te_annual_secFuel_cost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Second Fuel Cost")),
      setNames(te_annual_OMF_cost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|OMF Cost")),
      setNames(te_annual_OMV_cost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|OMV Cost")),
      ### calculate VRE grid and storage cost by dividing by usable generation (after generation)
      setNames(te_annual_stor_cost[, , pe2se$all_te] / total_te_energy_usable[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Storage Cost")),
      setNames(te_annual_grid_cost[, , pe2se$all_te] / total_te_energy_usable[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Grid Cost")),
      ### calculate VRE grid and storage cost (with adjustment costs) by dividing by usable generation (after generation)
      setNames(te_annual_stor_cost_wadj[, , pe2se$all_te] / total_te_energy_usable[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Storage Cost w/ Adj Cost")),
      setNames(te_annual_grid_cost_wadj[, , pe2se$all_te] / total_te_energy_usable[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Grid Cost w/ Adj Cost")),
      ### add cost for carbon storage and for carbon emissions
      setNames(te_annual_ccsInj_cost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|CCS Cost")),
      setNames(te_annual_ccsInj_inclAdjCost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|CCS Cost w/ Adj Cost")),
      setNames(te_annual_co2_cost[, , pe2se$all_te] / total_te_energy[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|CO2 Cost")),
      ### add curtailment cost
      setNames(te_curt_cost[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Curtailment Cost")),
      ### Total Cost
      setNames((te_annual_inv_cost[, ttot_from2005, pe2se$all_te] + te_annual_fuel_cost[, , pe2se$all_te] + te_annual_secFuel_cost[, , pe2se$all_te] + te_annual_OMF_cost[, , pe2se$all_te] +
                  te_annual_OMV_cost[, , pe2se$all_te] + te_annual_ccsInj_cost[, , pe2se$all_te] + te_annual_co2_cost[, , pe2se$all_te]) / total_te_energy[, , pe2se$all_te] +
                 (te_annual_stor_cost[, , pe2se$all_te] + te_annual_grid_cost[, , pe2se$all_te]) / total_te_energy_usable[, , pe2se$all_te] + te_curt_cost[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Total Cost")),
      setNames((te_annual_inv_cost_wadj[, ttot_from2005, pe2se$all_te] + te_annual_fuel_cost[, , pe2se$all_te] + te_annual_secFuel_cost[, , pe2se$all_te] + te_annual_OMF_cost[, , pe2se$all_te] +
                  te_annual_OMV_cost[, , pe2se$all_te] + te_annual_ccsInj_inclAdjCost[, , pe2se$all_te] + te_annual_co2_cost[, , pe2se$all_te]) / total_te_energy[, , pe2se$all_te] +
                 (te_annual_stor_cost_wadj[, , pe2se$all_te] + te_annual_grid_cost_wadj[, , pe2se$all_te]) / total_te_energy_usable[, , pe2se$all_te] + te_curt_cost[, , pe2se$all_te],
               paste0("LCOE|average|", pe2se$all_enty1, "|", pe2se$all_te, "|supply-side", "|Total Cost w/ Adj Cost")),
      #### Carbon Transport and storage ("ccsinje")
      setNames(te_annual_inv_cost[, ttot_from2005, "ccsinje"] /
                 vm_co2CCS_tCO2[, , "ccsinje.1"],
               paste0("LCOCS|average|", "ico2|", "ccsinje", "|carbon management", "|Investment Cost")),
      setNames(te_annual_inv_cost_wadj[, ttot_from2005, "ccsinje"] /
                 vm_co2CCS_tCO2[, , "ccsinje.1"],
               paste0("LCOCS|average|", "ico2|", "ccsinje", "|carbon management", "|Investment Cost w/ Adj Cost")),
      setNames(te_annual_OMF_cost[, , "ccsinje"] / vm_co2CCS_tCO2[, , "ccsinje.1"],
               paste0("LCOCS|average|", "ico2|", "ccsinje", "|carbon management", "|OMF Cost")),
      setNames(te_annual_secFuel_cost[, , "ccsinje"] / vm_co2CCS_tCO2[, , "ccsinje.1"],
               paste0("LCOCS|average|", "ico2|", "ccsinje", "|carbon management", "|Second Fuel Cost")),
      setNames((te_annual_inv_cost[, ttot_from2005, "ccsinje"] + te_annual_OMF_cost[, , "ccsinje"] + te_annual_secFuel_cost[, , "ccsinje"]) / vm_co2CCS_tCO2[, , "ccsinje.1"],
               paste0("LCOCS|average|", "ico2|", "ccsinje", "|carbon management", "|Total Cost")),
      setNames((te_annual_inv_cost_wadj[, ttot_from2005, "ccsinje"] + te_annual_OMF_cost[, , "ccsinje"] + te_annual_secFuel_cost[, , "ccsinje"]) / vm_co2CCS_tCO2[, , "ccsinje.1"],
               paste0("LCOCS|average|", "ico2|", "ccsinje", "|carbon management", "|Total Cost w/ Adj Cost")),
      #### Carbon Management Technologies
      ### main cost
      setNames(te_annual_inv_cost[, ttot_from2005, te_cco2] /
                 cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Investment Cost")),
      setNames(te_annual_inv_cost_wadj[, ttot_from2005, te_cco2] /
                 cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Investment Cost w/ Adj Cost")),
      setNames(te_annual_fuel_cost[, , te_cco2] / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Fuel Cost")),
      setNames(te_annual_secFuel_cost[, , te_cco2] / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Second Fuel Cost")),
      setNames(te_annual_otherFuel_cost[, , te_cco2] / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Other Fuel Cost")),
      setNames(te_annual_OMF_cost[, , te_cco2] / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|OMF Cost")),
      setNames(te_annual_OMV_cost[, , te_cco2] / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|OMV Cost")),
      ### cost of storage / amount of the total captured that is stored
      setNames(te_annual_ccsInj_cost[, ttot_from2005, cco2Techs] /
                 (cco2_byTech_tCO2[, ttot_from2005, cco2Techs] * dimSums(vm_co2CCS / vm_co2capture)),
               paste0("LCOCC|average|", "ico2|", cco2Techs, "|carbon management", "|CCS Cost")),
      setNames(te_annual_ccsInj_inclAdjCost[, ttot_from2005, cco2Techs] /
                 (cco2_byTech_tCO2[, ttot_from2005, cco2Techs] * dimSums(vm_co2CCS / vm_co2capture)),
               paste0("LCOCC|average|", "ico2|", cco2Techs, "|carbon management", "|CCS Cost w/ Adj Cost")),
      ### total cost for capturing co2
      setNames((te_annual_inv_cost[, ttot_from2005, te_cco2] + te_annual_OMF_cost[, , te_cco2] + te_annual_OMV_cost[, , te_cco2] +
                  te_annual_fuel_cost[, , te_cco2] + te_annual_secFuel_cost[, , te_cco2] + te_annual_otherFuel_cost[, , te_cco2]) / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Total Cost")),
      setNames((te_annual_inv_cost_wadj[, ttot_from2005, te_cco2] + te_annual_OMF_cost[, , te_cco2] + te_annual_OMV_cost[, , te_cco2] +
                  te_annual_fuel_cost[, , te_cco2] + te_annual_secFuel_cost[, , te_cco2] + te_annual_otherFuel_cost[, , te_cco2]) / cco2_byTech_tCO2[, ttot_from2005, te_cco2],
               paste0("LCOCC|average|", "cco2|", te_cco2, "|carbon management", "|Total Cost w/ Adj Cost")),
      ### total cost for injecting co2.
      setNames((te_annual_inv_cost[, ttot_from2005, te_cco2] + te_annual_OMF_cost[, , te_cco2] + te_annual_OMV_cost[, , te_cco2] +
                  te_annual_fuel_cost[, , te_cco2] + te_annual_secFuel_cost[, , te_cco2] + te_annual_otherFuel_cost[, , te_cco2]) / cco2_byTech_tCO2[, ttot_from2005, te_cco2] +
                 te_annual_ccsInj_cost[, ttot_from2005, cco2Techs] / (cco2_byTech_tCO2[, ttot_from2005, cco2Techs] * dimSums(vm_co2CCS / vm_co2capture)),
               paste0("LCOCCS|average|", "ico2|", te_cco2, "|carbon management", "|Total Cost")),
      setNames((te_annual_inv_cost_wadj[, ttot_from2005, te_cco2] + te_annual_OMF_cost[, , te_cco2] + te_annual_OMV_cost[, , te_cco2] +
                  te_annual_fuel_cost[, , te_cco2] + te_annual_secFuel_cost[, , te_cco2] + te_annual_otherFuel_cost[, , te_cco2]) / cco2_byTech_tCO2[, ttot_from2005, te_cco2] +
                 te_annual_ccsInj_inclAdjCost[, ttot_from2005, cco2Techs] / (cco2_byTech_tCO2[, ttot_from2005, cco2Techs] * dimSums(vm_co2CCS / vm_co2capture)),
               paste0("LCOCCS|average|", "ico2|", te_cco2, "|carbon management", "|Total Cost w/ Adj Cost")),
      #### Carbon storing Technologies (other than CCUS chain; for now only if EW included, will include biochar in future)
      if (EW_name %in% teCDR) {
        mbind(
          ## calculation based on removal initiated in t
          setNames(te_annual_inv_cost[, ttot_from2005, te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCS|average|", "sco2|", te_sco2, "|carbon management", "|Investment Cost")),
          setNames(te_annual_inv_cost_wadj[, ttot_from2005, te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCS|average|", "sco2|", te_sco2, "|carbon management", "|Investment Cost w/ Adj Cost")),
          setNames(te_annual_fuel_cost[, , te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCS|average|", "sco2|", te_sco2, "|carbon management", "|Fuel Cost")),
          setNames(te_annual_secFuel_cost[, , te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCS|average|", "sco2|", te_sco2, "|carbon management", "|Second Fuel Cost")),
          setNames(te_annual_otherFuel_cost[, , te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCC|average|", "sco2|", te_sco2, "|carbon management", "|Other Fuel Cost")),
          setNames(te_annual_OMF_cost[, , te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCS|average|", "sco2|", te_sco2, "|carbon management", "|OMF Cost")),
          setNames(te_annual_OMV_cost[, , te_sco2] / cdrco2_byTech_tCO2[, ttot_from2005, te_sco2],
                   paste0("LCOCS|average|", "sco2|", te_sco2, "|carbon management", "|OMV Cost")),
          # specific to enhanced weathering
          setNames(EW_fixed_other_cost[, , ] / cdrco2_byTech_tCO2[, ttot_from2005, EW_name],
                   paste0("LCOCS|average|", "sco2|", EW_name, "|carbon management", "|OMF other Cost")),
          setNames(EW_fixed_transport_cost / cdrco2_byTech_tCO2[, ttot_from2005, EW_name],
                   paste0("LCOCS|average|", "sco2|", EW_name, "|carbon management", "|OMF transport Cost")),
          # sum for enhanced weathering (incl. special om cost)
          setNames((te_annual_inv_cost[, ttot_from2005, EW_name] +  te_annual_OMF_cost[, , EW_name] + te_annual_otherFuel_cost[, , EW_name] + EW_fixed_other_cost +
                      EW_fixed_transport_cost) / cdrco2_byTech_tCO2[, ttot_from2005, EW_name],
                   paste0("LCOCS|average|", "sco2|", EW_name, "|carbon management", "|Total Cost")),
          setNames((te_annual_inv_cost_wadj[, ttot_from2005, EW_name] + te_annual_otherFuel_cost[, , EW_name] + EW_fixed_other_cost +
                      EW_fixed_transport_cost) / cdrco2_byTech_tCO2[, ttot_from2005, EW_name],
                   paste0("LCOCS|average|", "sco2|", EW_name, "|carbon management", "|Total Cost w/ Adj Cost"))
        )
      } else {
        NULL
      }
    ) * s_usd2017t2015

    # convert to better dimensional format
    df.lcoe.avg <- as.quitte(LCOE.avg) %>%
      select("region", "period", "data", "value") %>%
      rename(variable = data) %>%
      replace(is.na(.), 0)

    # extract further dimensions from variable name
    df.lcoe.avg$type <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
    df.lcoe.avg$type <- sapply(df.lcoe.avg$type, "[[", 2)

    df.lcoe.avg$output <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
    df.lcoe.avg$output <- sapply(df.lcoe.avg$output, "[[", 3)

    df.lcoe.avg$tech <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
    df.lcoe.avg$tech <- sapply(df.lcoe.avg$tech, "[[", 4)

    df.lcoe.avg$sector <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
    df.lcoe.avg$sector <- sapply(df.lcoe.avg$sector, "[[", 5)

    df.lcoe.avg$cost <- strsplit(as.character(df.lcoe.avg$variable), "\\|")
    df.lcoe.avg$cost <- sapply(df.lcoe.avg$cost, "[[", 6)

    df.lcoe.avg <- df.lcoe.avg %>%
      mutate(unit = "US$2015/MWh") %>%
      mutate(unit = case_when(output == "ico2" | output == "cco2" | output == "sco2" ~ "US$2015/tCO2", TRUE ~ unit)) %>%
      select("region", "period", "type", "output", "tech", "sector", "unit", "cost", "value")

    # reconvert to magpie object
    LCOE.avg.out <- suppressWarnings(as.magpie(df.lcoe.avg, spatial = 1, temporal = 2, datacol = 9))
    # bind to output file
    LCOE.out <- mbind(LCOE.out, LCOE.avg.out)
  }



  #### B) Calculation of marginal (new plant) LCOE ----


  if (output.type %in% c("marginal", "both", "marginal detail")) {
    # variable definitions for dplyr operations in the following section
    # avoids error of "no visible binding for global variable for X" in buildLibrary()
    region <- NULL
    period <- NULL
    all_te <- NULL
    value <- NULL
    char <- NULL
    CapDistr <- NULL
    nur <- NULL
    rlf <- NULL
    CAPEX <- NULL
    tech <- NULL
    all_enty <- NULL
    pomeg <- NULL
    fuel.price <- NULL
    eff <- NULL
    OMF <- NULL
    OMV <- NULL
    lifetime <- NULL
    annuity.fac <- NULL
    CapFac <- NULL
    `Investment Cost` <- NULL
    `OMF Cost` <- NULL
    `OMV Cost` <- NULL
    `cost` <- NULL
    `variable` <- NULL
    fuel <- NULL
    output <- NULL
    opTimeYr <- NULL
    co2.price <- NULL
    emiFac <- NULL
    all_enty1 <- NULL
    input <- NULL
    emiFac.se2fe <- NULL
    co2_dem <- NULL
    CO2StoreShare <- NULL
    all_enty2 <- NULL
    secfuel <- NULL
    secfuel.prod <- NULL
    all_te1 <- NULL
    CCStax.cost <- NULL
    secfuel.price <- NULL
    `CO2 Provision Cost` <- NULL
    `Second Fuel Cost` <- NULL
    `CCS Tax Cost` <- NULL
    type <- NULL
    data <- NULL
    sector <- NULL
    FlexPriceShare <- NULL
    FEtax <- NULL
    `Flex Tax` <- NULL
    `FE Tax` <- NULL
    curtShare <- NULL
    `Curtailment Cost` <- NULL
    AddH2TdCost <- NULL
    `Additional H2 t&d Cost` <- NULL
    emi_sectors <- NULL
    model <- scenario <- variable <- unit <- NULL
    AdjCost <- NULL
    operationPeriod <- NULL
    discount <- NULL
    weight <- NULL
    p_teAnnuity <- NULL
    subrate <- NULL
    taxrate <- NULL
    fuel.price.weighted <- NULL
    co2.price.weighted <- NULL
    `Fuel Cost (time step prices)` <- NULL
    `Fuel Cost (intertemporal prices)` <- NULL
    `CO2 Tax Cost (time step prices)` <- NULL
    `CO2 Tax Cost (intertemporal prices)` <- NULL
    `Total LCOE (time step prices)` <- NULL
    `Total LCOE (intertemporal prices)` <- NULL
    `Adjustment Cost` <- NULL
    `SE Tax` <- NULL
    `tau_SE_tax` <- NULL

    # Prepare data for LCOE calculation ----


    ### Read sets and mappings from GDX ----


    ### technologies
    pe2se <- readGDX(gdx, "pe2se") # pe2se technology mappings
    se2se <- readGDX(gdx, "se2se") # hydrogen <--> electricity technologies
    se2fe <- readGDX(gdx, "se2fe") # se2fe technology mappings
    te <- readGDX(gdx, "te") # all technologies
    teStor <- readGDX(gdx, "teStor") # storage technologies for VREs
    teGrid <- readGDX(gdx, "teGrid") # grid technologies for VREs
    ccs2te <-  readGDX(gdx, "ccs2te") # ccsinje technology
    teReNoBio <- readGDX(gdx, "teReNoBio") # renewable technologies without biomass
    teCCS <- readGDX(gdx, "teCCS") # ccs technologies
    teReNoBio <- c(teReNoBio) # renewables without biomass
    teVRE   <- readGDX(gdx, "teVRE") # VRE technologies
    # exclude "windoff" from teVRE as "windoff" does not have separate grid, storage technologies
    if ("windoff" %in% as.vector(teVRE)) {
      teVRE <- as.vector(teVRE)
      teVRE <- teVRE[teVRE != "windoff"]
    }

    # final energy demand
    vm_demFeSector <- readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = FALSE)

    # conversion factors
    s_twa2mwh <- readGDX(gdx, c("sm_TWa_2_MWh", "s_TWa_2_MWh", "s_twa2mwh"), format = "first_found")


    # all technologies to calculate LCOE for
    te_LCOE <- c(pe2se$all_te, se2se$all_te, se2fe$all_te, "ccsinje")

    # all technologies to calculate investment, adjustment and O&M LCOE for (needed for storage, grid cost)
    te_LCOE_Inv <- c(te_LCOE, as.vector(teStor), as.vector(teGrid), te[te %in% c("dac")])

    # mappings
    se_gen_mapping <- rbind(pe2se, se2se)
    colnames(se_gen_mapping) <- c("fuel", "output", "tech")

    # all energy system technologies mapping
    en2en <- readGDX(gdx, "en2en") %>%
      filter(all_te %in% te_LCOE)
    colnames(en2en) <- c("fuel", "output", "tech")

    ### time steps
    ttot     <- as.numeric(readGDX(gdx, "ttot"))
    ttot_from2005 <- paste0("y", ttot[which(ttot >= 2005)])

    ### conversion factors
    s_twa2mwh <- as.vector(readGDX(gdx, c("sm_TWa_2_MWh", "s_TWa_2_MWh", "s_twa2mwh"), format = "first_found"))



    ### Read investment and O&M cost ----

    # investment cost
    vm_costTeCapital <- readGDX(gdx, "vm_costTeCapital", field = "l", restore_zeros = FALSE)[, ttot_from2005, te_LCOE_Inv]


    df.CAPEX <- as.quitte(vm_costTeCapital) %>%
      select(region, period, all_te, value) %>%
      rename(tech = all_te, CAPEX = value) %>%
      left_join(en2en, by = c("tech")) %>%
      select(region, period, tech, fuel, output, CAPEX)

    # omf cost
    pm_data_omf <- readGDX(gdx, "pm_data", restore_zeros = FALSE)[, , "omf"]

    df.OMF <- as.quitte(pm_data_omf) %>%
      select(region, all_te, value) %>%
      rename(tech = all_te, OMF = value)

    # omv cost
    pm_data_omv <- readGDX(gdx, "pm_data", restore_zeros = FALSE)[, , "omv"]

    df.OMV <- as.quitte(pm_data_omv) %>%
      select(region, all_te, value) %>%
      rename(tech = all_te, OMV = value)


    # Read/calculate capacity factors ----

    # capacity factor of non-renewables
    vm_capFac <- readGDX(gdx, "vm_capFac", field = "l", restore_zeros = FALSE)[, ttot_from2005, ]

    # calculate renewable capacity factors of new plants
    v_capDistr <- readGDX(gdx, c("vm_capDistr", "v_capDistr"), field = "l", restore_zeros = FALSE)
    pm_dataren <- readGDX(gdx, "pm_dataren", restore_zeros = FALSE)


    ### determine capacity factor of highest free grade for renewables
    # RE capacity distribution over grades
    df.CapDistr <- as.quitte(v_capDistr) %>%
      select(region, all_te, period,  rlf, value) %>%
      rename(tech = all_te, CapDistr = value)

    # max. potential of renewable production per grade
    df.RE.maxprod <- as.quitte(pm_dataren[, , c("maxprod", "nur")]) %>%
      rename(tech = all_te) %>%
      select(region, tech, rlf, char, value) %>%
      spread(char, value)

    # calculate marginal renewable capacity factor
    df.CapFac.ren <- df.CapDistr %>%
      filter(tech %in% c(teReNoBio, teVRE)) %>%
      left_join(df.RE.maxprod, by = c("region", "tech", "rlf")) %>%
      # filter for the lowest grade that is filled (remark by Robert: this will then slightly overestimate CF for the cases that the model expands this technology, but in such a case it will anyway go into the next bad grade in the next time step, so it should in general look better)
      # but not smaller than ninth grade (last grade of spv; still check all REN technologies for number of last grade)
      filter(CapDistr >= 1e-7 | as.numeric(rlf) >= 9)  %>%
      # choose highest grade
      group_by(region, period, tech) %>%
      summarise(CapFac = min(nur)) %>%
      ungroup()

    # CapFac, merge renewble and non renewable Cap Facs
    df.CapFac <- as.quitte(vm_capFac) %>%
      select(region, period, all_te, value) %>%
      rename(tech = all_te, CapFac = value) %>%
      filter(!tech %in% teReNoBio) %>%
      rbind(df.CapFac.ren) %>%
      mutate(period = as.numeric(period))

    ### Read plant lifetime and annuity factor ----

    # discount rate
    r <- 0.051
    # r <- readGDX(gdx, name="p_r", restore_zeros = F)
    #
    # r <- r %>%
    #   as.data.frame() %>%
    #   # filter(Year < 2110) %>%
    #   dplyr::group_by() %>%
    #   dplyr::summarise( Value = mean(Value) , .groups = 'keep' ) %>%
    #   dplyr::ungroup() %>%
    #   as.magpie()

    # read lifetime of technology
    # calculate annuity factor to annuitize CAPEX and OMF (annuity factor labeled "annuity.fac")
    lt <- readGDX(gdx, name = "fm_dataglob", restore_zeros = FALSE)[, , "lifetime"][, , te_LCOE_Inv][, , "lifetime"]

    df.lifetime <- as.quitte(lt) %>%
      select(all_te, value) %>%
      rename(tech = all_te, lifetime = value) %>%
      mutate(annuity.fac = r * (1 + r)^lifetime / (-1 + (1 + r)^lifetime))


    ### note: just take LCOE investment cost formula with fix lifetime, ignoring that in REMIND capacity is drepeciating over time
    ### actually you would need to divide CAPEX by CapFac * 8760 * p_omeg and then add a discounting.
    ### Would need to discuss again how to add the discount in such formula.

    # # annuity factor from REMIND,
    # TODO: check whether this is the same as calculated above
    # so far only used in levelized cost of DAC calculation below
    p_teAnnuity <- readGDX(gdx, c("p_teAnnuity", "pm_teAnnuity"), restore_zeros = FALSE)

    ### Read marginal adjustment costs ----

    # Read marginal adjustment cost calculated in core/postsolve.gms
    # It is calculated as d(vm_costInvTeAdj) / d(vm_deltaCap).
    # Unit: trUSD2017/ (TW(out)/yr).
    o_margAdjCostInv <- readGDX(gdx, "o_margAdjCostInv", restore_zeros = FALSE)

    df.margAdjCostInv <- as.quitte(o_margAdjCostInv) %>%
      rename(tech = all_te,
             AdjCost = value) %>%
      select(region, period, tech, AdjCost)

    ### Read fuel price ----

    # fuels to calculate price for
    fuels <- c("peoil", "pegas", "pecoal", "peur", "pebiolc", "pebios", "pebioil",
               "seel", "seliqbio", "seliqfos", "seliqsyn", "sesobio", "sesofos", "seh2", "segabio",
               "segafos", "segasyn", "sehe")

    pm_PEPrice <- readGDX(gdx, "pm_PEPrice", restore_zeros = FALSE)
    pm_SEPrice <- readGDX(gdx, "pm_SEPrice", restore_zeros = FALSE)
    # bind PE and SE prices and convert from tr USD 2017/TWa to USD2015/MWh
    Fuel.Price <- mbind(pm_PEPrice, pm_SEPrice)[, , fuels] * 1e12 / s_twa2mwh * s_usd2017t2015

    # Fuel price for the time period for which LCOE are calculated
    df.Fuel.Price <- as.quitte(Fuel.Price) %>%
      select(region, period, all_enty, value) %>%
      rename(fuel = all_enty, fuel.price = value) %>%
      # replace zeros by last or (if not available) next value of time series (marginals are sometimes zero if there are other constriants)
      mutate(fuel.price = ifelse(fuel.price == 0, NA, fuel.price)) %>%
      group_by(region, fuel) %>%
      tidyr::fill(fuel.price, .direction = "downup") %>%
      ungroup()

    ### Read carbon price ----
    p_priceCO2 <- readGDX(gdx, name = c("p_priceCO2", "pm_priceCO2"), format = "first_found") # co2 price

    # carbon price for the time period for which LCOE are calculated
    df.co2price <- as.quitte(p_priceCO2) %>%
      select(region, period, value) %>%
      # where carbon price is NA, it is zero
      # convert from USD2017/tC CO2 to USD2015/tCO2
      mutate(value = ifelse(is.na(value), 0, value * s_usd2017t2015 / 3.66)) %>%
      rename(co2.price = value)

    # Calculate intertemporal fuel and carbon prices ----

    # This section calculates weighted-average of the prices seen by a plant over
    # its time of operation. It takes into account that fuel cost are discounted over time
    # and that capacities depreciate over time such that near-term prices
    # should be weighted higher in the LCOE than long-term prices.
    # Weighted average fuel prices FP_avg are calculated
    # from fuel prices over period t FP_t as:
    # FP_avg = sum_t (  (discount_t * pomeg_t * FP_t) / (sum_t (discount_t * pomeg_t))
    # discount_t: discounting factor
    # pomeg_t: depreciation factor, gives fraction of operating capacity over lifetime of plant


    # retrieve capacity distribution over lifetime
    # (fraction of capacity still standing in that year of plant lifetime)
    p_omeg  <- readGDX(gdx, c("pm_omeg", "p_omeg"), format = "first_found")

    # operation time steps of plant, only take every 5 years and limit calculation to 50 years as
    # by then most of the capacity of all technologies is already retired
    set_operation_period <- c(1, seq(5, 50, 5))


    # capacity distribution over lifetime
    df.pomeg <- as.quitte(p_omeg) %>%
      rename(tech = all_te, pomeg = value) %>%
      # to save run time, only take p_omeg in ten year time steps only up to lifetimes of 50 years,
      # erase region dimension, pomeg same across all regions
      filter(opTimeYr %in% set_operation_period,
             region %in% getRegions(vm_costTeCapital)[1],
             tech %in% te_LCOE) %>%
      select(opTimeYr, tech, pomeg)

    # create dataframe with all combinations of time steps "period" in which plant was built (commissioning period)
    # time steps "operationPeriod" in which production takes place, year of operation "opTimeYr" in lifetime of plant
    df.period_operationPeriod <- expand.grid(opTimeYr = unique(df.pomeg$opTimeYr),
                                             period = getPeriods(df.Fuel.Price)) %>%
      # operationPeriod is commissioning period + years of operation (opTimeYr)
      # for comissioning period take value of first year of operation
      mutate(operationPeriod = ifelse(as.numeric(opTimeYr) > 1,
                                      as.numeric(opTimeYr) + period,
                                      period)) %>%
      # remove time steps from operationPeriod that are not remind_timesteps
      filter(operationPeriod %in% unique(quitte::remind_timesteps$period))

    # Join capacity distribution with above dataframe to get
    # capacity distribution over all commissioning years (period) and
    # operation years (operationPeriod)
    df.pomeg.expand <- df.pomeg %>%
      # only take capacity distribution of 5-year time steps and up to 50 years of operation
      right_join(df.period_operationPeriod,
                 relationship = "many-to-many") %>%
      # add energy input and output carrier dimension
      left_join(en2en)

    # calculate average capacity-weighted and discounted fuel price over lifetime of plant
    df.fuel.price.weighted <- df.pomeg.expand %>%
      left_join(df.Fuel.Price,
                by = c("operationPeriod" = "period",
                       "fuel" = "fuel"),
                relationship = "many-to-many") %>%
      # calculate discount factor
      mutate(discount = 1 / (1 + r)^(operationPeriod - period)) %>%
      # calculate weights over lifetime as operational capacity * discount factor,
      # normalize by sum over all time steps for weights to add up to one
      group_by(region, period, tech) %>%
      mutate(weight = pomeg * discount / sum(pomeg * discount)) %>%
      # calculate average weighted fuel price over lifetime as
      # the sum of the capacity-discount weights
      summarise(fuel.price.weighted = sum(fuel.price * weight)) %>%
      ungroup()

    # calculate average capacity-weighted and discounted fuel price over lifetime of plant
    df.co2price.weighted <- df.pomeg.expand %>%
      left_join(df.co2price,
                by = c("operationPeriod" = "period"),
                relationship = "many-to-many") %>%
      # calculate discount factor
      mutate(discount = 1 / (1 + r)^(operationPeriod - period)) %>%
      # calculate weights over lifetime as operational capacity * discount factor,
      # normalize by sum over all time steps for weights to add up to one
      group_by(region, period, tech) %>%
      mutate(weight = pomeg * discount / sum(pomeg * discount)) %>%
      # calculate average weighted fuel price over lifetime as
      # the sum of the capacity-discount weights
      summarise(co2.price.weighted = sum(co2.price * weight)) %>%
      ungroup()

    ### Read conversion efficiencies ----
    pm_eta_conv <- readGDX(gdx, "pm_eta_conv", restore_zeros = FALSE)[, ttot_from2005, ] # efficiency oftechnologies with time-independent eta
    pm_dataeta <- readGDX(gdx, "pm_dataeta", restore_zeros = FALSE)[, ttot_from2005, ] # efficiency of technologies with time-dependent eta

    df.eff <- as.quitte(mbind(pm_eta_conv, pm_dataeta[, , setdiff(getNames(pm_dataeta), getNames(pm_eta_conv))])) %>%
      rename(tech = all_te, eff = value) %>%
      select(region, period, tech, eff)

    ### 11. get emission factors of technologies
    pm_emifac <- readGDX(gdx, "pm_emifac", restore_zeros = FALSE)[, ttot_from2005, "co2"] # co2 emission factor per technology
    pm_emifac_cco2 <- readGDX(gdx, "pm_emifac", restore_zeros = FALSE)[, ttot_from2005, "cco2"] # captured co2 emission factor per technology

    df.emiFac <- as.quitte(pm_emifac) %>%
      # do not need period dimension
      filter(period == 2005) %>%
      select(region, all_te, value) %>%
      # convert from GtC CO2/TWa to tCo2/MWh
      mutate(value = value / s_twa2mwh * 3.66 * 1e9) %>%
      rename(tech = all_te, emiFac = value) %>%
      # join other technologies without emission factor -> set emission factor to zero
      # (so far, only emissions of energy input captured, not of co2 input for CCU)
      full_join(data.frame(tech = te_LCOE_Inv) %>%
                  expand(tech, region = getRegs(df.CAPEX)), by = c("region", "tech")) %>%
      mutate(emiFac = ifelse(is.na(emiFac), 0, emiFac))


    # get se2fe emision factor to add to calculate CO2 cost for pe2se technologies whose outputs are seliqfos/segasfos etc.
    df.emifac.se2fe <- df.emiFac %>%
      left_join(se2fe, by = c("tech" = "all_te")) %>%
      # filter for SE fossil, take diesel emifac for all liquids
      filter(all_enty %in% c("seliqfos", "segafos", "sesofos")) %>%
      filter(all_enty != "seliqfos" | all_enty1 == "fedie") %>%
      rename(input = all_enty, emiFac.se2fe = emiFac) %>%
      select(region, input, emiFac.se2fe) %>%
      left_join(pe2se, by = c("input" = "all_enty1")) %>%
      rename(tech = all_te) %>%
      filter(emiFac.se2fe > 0) %>%
      select(region, tech, emiFac.se2fe)




    ### Calculate CO2 capture cost ----


    # temporary solution, take LCOD of DAC as the CO2 Capture Cost

    # TODO: take co2 capture cost from a fitting marginal of REMIND equations

    ### DAC: calculate Levelized Cost of CO2 from direct air capture
    # DAC energy demand per unit captured CO2 (EJ/GtC)

    LCOD <- new.magpie(getRegions(vm_costTeCapital), getYears(vm_costTeCapital),
                       c("Investment Cost", "OMF Cost", "Electricity Cost", "Heat Cost", "Total LCOE"), fill = 0)

    if ("dac" %in% te) {
      p33_fedem <- readGDX(gdx, "p33_fedem", restore_zeros = FALSE, react = "silent")
      if (is.null(p33_fedem)) { # compatibility with the REMIND version prior to adding a CDR portfolio
        p33_dac_fedem_el <- readGDX(gdx, "p33_dac_fedem_el", restore_zeros = FALSE)
        p33_dac_fedem_heat <- readGDX(gdx, "p33_dac_fedem_heat", restore_zeros = FALSE)
        p33_fedem <- new.magpie(getRegions(p33_dac_fedem_el), getYears(p33_dac_fedem_el), c("dac.feels", "dac.fehes"))
        p33_fedem[, , "dac.feels"] <- p33_dac_fedem_el
        p33_fedem[, , "dac.fehes"] <- p33_dac_fedem_heat[, , "fehes"]
      }

      Fuel.Price <- magclass::matchDim(Fuel.Price, p33_fedem, dim = c(1), fill = 0)
      # capital cost in trUSD2017/GtC -> convert to USD2015/tCO2
      LCOD[, , "Investment Cost"] <- vm_costTeCapital[, , "dac"] * s_usd2017t2015 / 3.66 / vm_capFac[, , "dac"] * p_teAnnuity[, , "dac"] * 1e3
      LCOD[, , "OMF Cost"] <-  pm_data_omf[, , "dac"] * vm_costTeCapital[, , "dac"] * s_usd2017t2015 / 3.66 / vm_capFac[, , "dac"] * 1e3
      # electricity cost (convert DAC FE demand to GJ/tCO2 and fuel price to USD/GJ)
      LCOD[, , "Electricity Cost"] <-  p33_fedem[, , "dac.feels"] / 3.66 * Fuel.Price[, , "seel"] / 3.66
      # TODO: adapt to FE prices and new CDR FE structure, temporary: conversion as above, assume for now that heat is always supplied by district heat
      LCOD[, , "Heat Cost"] <- p33_fedem[, , "dac.fehes"] / 3.66 * Fuel.Price[, , "sehe"]  / 3.66
      LCOD[, , "Total LCOE"] <- LCOD[, , "Investment Cost"] + LCOD[, , "OMF Cost"] + LCOD[, , "Electricity Cost"] + LCOD[, , "Heat Cost"]
    }

    getSets(LCOD)[3] <- "cost"

    # add dimensions to fit to other tech LCOE
    LCOD <- LCOD %>%
      add_dimension(add = "unit", nm = "US$2015/tCO2") %>%
      add_dimension(add = "tech", nm = "dac") %>%
      add_dimension(add = "output", nm = "cco2") %>%
      add_dimension(add = "type", nm = "marginal") %>%
      add_dimension(add = "sector", dim = 3.4, nm = "carbon management")


    # Co2 Capture price, marginal of q_balcapture,  convert from tr USD 2017/GtC to USD2015/tCO2
    qm_balcapture  <- readGDX(gdx, "q_balcapture", field = "m", restore_zeros = FALSE)
    Co2.Capt.Price <- qm_balcapture /
      (qm_budget[, getYears(qm_balcapture), ] + 1e-10) * 1e3 * s_usd2017t2015 / 3.66 ## looks weird


    ### for now, just assume CO2 Capture Cost = DAC Cost
    Co2.Capt.Price[, , ] <- LCOD[, , "dac"][, , "Total LCOE"]

    df.Co2.Capt.Price <- as.quitte(Co2.Capt.Price) %>%
      rename(Co2.Capt.Price = value) %>%
      select(region, period, Co2.Capt.Price)




    # CO2 required per unit output (for CCU technologies)
    if (module2realisation["CCU", 2] == "on") {
      p39_co2_dem <- readGDX(gdx, c("p39_co2_dem", "p39_ratioCtoH"), restore_zeros = FALSE)[, , ]
    } else {
      # some dummy data, only needed to create the following data frame if CCU is off
      p39_co2_dem <- new.magpie(getRegions(vm_costTeCapital), getYears(vm_costTeCapital), fill = 0)
    }

    df.co2_dem <- as.quitte(p39_co2_dem) %>%
      rename(co2_dem = value, tech = all_te) %>%
      select(region, period, tech, co2_dem) %>%
      # from GtC CO2/TWa(H2) to tCO2/MWh(H2)
      mutate(co2_dem = co2_dem * 3.66 / s_twa2mwh * 1e9)

    ### 13. calculate share stored carbon from capture carbon
    vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = FALSE)
    vm_co2capture <- readGDX(gdx, c("vm_co2capture","v_co2capture"), field = "l", restore_zeros = FALSE)

    if (getSets(vm_co2capture)[[3]] == "emiAll") {
      sel_vm_co2capture_cco2 <- mselect(vm_co2capture, emiAll = "cco2")
    } else {
      sel_vm_co2capture_cco2 <- mselect(vm_co2capture, all_enty = "cco2")
    }

    p_share_carbonCapture_stor <- (
      vm_co2CCS[, , "cco2.ico2.ccsinje.1"]
      / dimSums(sel_vm_co2capture_cco2, dim = 3)
    )
    p_share_carbonCapture_stor[is.na(p_share_carbonCapture_stor)] <- 1

    df.CO2StoreShare <- as.quitte(p_share_carbonCapture_stor) %>%
      rename(CO2StoreShare = value) %>%
      select(region, period, CO2StoreShare)

    # Note: This is assuming that the CO2 capture to storage share stays constant over time.
    # Still to do: calculate average over lifetime of plant

    ### Calculate cost of second fuel ----

    # for technologies with coupled production
    # (pm_prodCouple: negative values mean own consumption, positive values mean coupled product)

    # mapping of SE technologies that require second fuel input (own consumption)
    pc2te <- readGDX(gdx, "pc2te")
    # second fuel production per unit output of technology
    pm_prodCouple <- readGDX(gdx, "pm_prodCouple", restore_zeros = FALSE)

    # secfuel.prod (share of coupled production per unit output),
    # secfuel.price (price of coupled product)
    df.secfuel <- as.quitte(pm_prodCouple) %>%
      rename(tech = all_te, fuel = all_enty, secfuel = all_enty2, secfuel.prod = value) %>%
      select(region, tech, fuel, secfuel, secfuel.prod) %>%
      right_join(df.Fuel.Price, by = c("region", "fuel")) %>%
      rename(secfuel.price = fuel.price)




    ### Read curtailment share -----

    # curtailment share is needed to calculate curtailment cost of VREs:
    # curtailment cost = LCOE(VRE) * curtshare/((1-curtShare)),
    # where LCOE(VRE) is the generation LCOE of VREs, so Investment Cost + O&M Cost

    if (module2realisation["power", 2] == "RLDC") {
      v32_curt <- readGDX(gdx, name = c("v32_curt"), field = "l", restore_zeros = FALSE, format = "first_found")
    } else if (module2realisation["power", 2] %in% c("IntC", "DTcoup")) {
      v32_curt <- v32_storloss[, ttot_from2005, getNames(vm_prodSe, dim = 3)]
    } else {
      v32_curt <- 0
    }


    curt_share <- v32_curt[, , teVRE] / vm_prodSe[, , teVRE]

    df.curtShare <- as.quitte(curt_share) %>%
      rename(tech = all_te, curtShare = value) %>%
      select(region, period, tech, curtShare)



    ### Calculate CCS tax ----

    # following q21_taxrevCCS
    vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = FALSE)
    sm_ccsinjecrate <- readGDX(gdx, c("sm_ccsinjecrate", "s_ccsinjecrate"), format = "first_found")
    pm_ccsinjecrate <- readGDX(gdx, "pm_ccsinjecrate", react = "silent")
    if (is.null(pm_ccsinjecrate)) pm_ccsinjecrate <- sm_ccsinjecrate
    pm_dataccs <- readGDX(gdx, "pm_dataccs", restore_zeros = FALSE)


    # calculate storage share of captured CO2,
    # for now take the storage share of the construction year of plant, it will not change much over time
    # (if CCS, then no CCU and v_capturevalve is mostly small)
    vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = FALSE)
    vm_co2capture <- readGDX(gdx, c("vm_co2capture","v_co2capture"), field = "l", restore_zeros = FALSE)


    # calculate stored CO2 per output of capture technology (GtC/TWa)
    pm_eff <- mbind(pm_eta_conv, pm_dataeta[, , setdiff(getNames(pm_dataeta), getNames(pm_eta_conv))])
    vm_co2CCS_m <- pm_emifac_cco2 / pm_eff[, , getNames(pm_emifac_cco2, dim = 3)] * collapseNames(p_share_carbonCapture_stor)


    # calculate CCS tax markup following q21_taxrevCCS, convert to USD2015/MWh
    CCStax <- dimReduce(pm_data_omf[, , "ccsinje"] * vm_costTeCapital[, , "ccsinje"] * vm_co2CCS_m^2 / pm_dataccs[, , "quan.1"] / pm_ccsinjecrate / s_twa2mwh * 1e12 * s_usd2017t2015)


    df.CCStax <- as.quitte(CCStax) %>%
      rename(tech = all_te1, CCStax.cost = value) %>%
      select(region, period, tech, CCStax.cost)

    # Note: CCS tax still to fix, set temporarily to 0
    df.CCStax <- df.CCStax %>%
      mutate(CCStax.cost = 0)



    ### Read Flexibility Tax ----

    cm_FlexTax <- readGDX(gdx, "cm_flex_tax")
    v32_flexPriceShare <- readGDX(gdx, "v32_flexPriceShare", field = "l", restore_zeros = FALSE)
    if (is.null(v32_flexPriceShare) || is.null(cm_FlexTax)) {
      v32_flexPriceShare <- vm_costTeCapital
      v32_flexPriceShare[, , ] <- 1
    } else {
      if (cm_FlexTax == 0) {
        v32_flexPriceShare <- vm_costTeCapital
        v32_flexPriceShare[, , ] <- 1
      }
    }

    df.flexPriceShare <- as.quitte(v32_flexPriceShare) %>%
      rename(tech = all_te, FlexPriceShare = value) %>%
      select(region, period, tech, FlexPriceShare)


    ### Read SE Tax for Electrolysis ----

    # read SE tax for electrolysis from GDX
    v21_tau_SE_tax <- readGDX(gdx, "v21_tau_SE_tax", field = "l", restore_zeros = FALSE, react = "silent")
    # if not SE tax in run, set to 0
    if (is.null(v21_tau_SE_tax)) {
      v21_tau_SE_tax <- vm_costTeCapital
      v21_tau_SE_tax[, , ] <- 0
    }

    df.tau_SE_tax <- as.quitte(v21_tau_SE_tax) %>%
      rename(tech = all_te, tau_SE_tax = value) %>%
      select(region, period, tech, tau_SE_tax) %>%
      # convert to USD2015/MWh
      mutate(tau_SE_tax = s_usd2017t2015 / as.vector(s_twa2mwh) * 1e12 * tau_SE_tax)

    ### Read Final Energy Taxes ----

    sector.mapping <- c("build" = "buildings", "indst" = "industry", "trans" = "transport")

    pm_tau_fe_tax <- readGDX(gdx, c("p21_tau_fe_tax", "pm_tau_fe_tax"), restore_zeros = FALSE)
    pm_tau_fe_sub <- readGDX(gdx, c("p21_tau_fe_sub", "pm_tau_fe_sub"), restore_zeros = FALSE)

    df.taxrate <- as.quitte(pm_tau_fe_tax * s_usd2017t2015 / s_twa2mwh * 1e12) %>%
      rename(taxrate = value)

    df.subrate <- as.quitte(pm_tau_fe_sub * s_usd2017t2015 / s_twa2mwh * 1e12) %>%
      rename(subrate = value)

    df.FEtax <- df.taxrate %>%
      full_join(df.subrate) %>%
      # set NA to 0
      mutate(subrate = ifelse(is.na(subrate), 0, subrate),
             taxrate = ifelse(is.na(taxrate), 0, taxrate)) %>%
      # taxrate + subsidy rate = net FE tax
      mutate(FEtax = taxrate + subrate) %>%
      filter(emi_sectors %in% names(sector.mapping)) %>%
      revalue.levels(emi_sectors = sector.mapping) %>%
      rename(sector = emi_sectors, output = all_enty) %>%
      select(region, period, sector, output, FEtax)


    ### Read H2 Phase-in cost for buildings and industry ----

    ### for buildings and industry (calculate average cost for simplicity, not marginal)

    v36_costAddTeInvH2 <- readGDX(gdx, "v36_costAddTeInvH2", field = "l", restore_zeros = FALSE, react = "silent")

    df.AddTeInvH2 <- NULL
    if (!is.null(v36_costAddTeInvH2)) {

      df.AddTeInvH2Build <- as.quitte(v36_costAddTeInvH2[, ttot_from2005, "tdh2s"] / collapseNames(dimSums(vm_demFeSector[, ttot_from2005, "seh2.feh2s.build"], dim = 3.4, na.rm = TRUE)) / s_twa2mwh * 1e12 * s_usd2017t2015) %>%
        mutate(value = ifelse(is.infinite(value), 0, value)) %>%
        mutate(emi_sectors = "buildings") %>%
        select(region, period, all_te, emi_sectors, value) %>%
        rename(AddH2TdCost = value, sector = emi_sectors, tech = all_te)

      df.AddTeInvH2 <- rbind(df.AddTeInvH2, df.AddTeInvH2Build)
    }

    # Join all data for marginal LCOE calculation ----

    ### create table with all parameters needed for LCOE calculation
    df.LCOE <- df.CAPEX %>%
      left_join(df.OMF, by = c("region", "tech")) %>%
      left_join(df.OMV, by = c("region", "tech")) %>%
      left_join(df.CapFac, by = c("region", "period", "tech")) %>%
      left_join(df.lifetime, by = "tech") %>%
      left_join(df.margAdjCostInv, by = c("region", "period", "tech")) %>%
      left_join(df.Fuel.Price, by = c("region", "period", "fuel")) %>%
      left_join(df.co2price, by = c("region", "period")) %>%
      left_join(df.fuel.price.weighted, by = c("region", "period", "tech")) %>%
      left_join(df.co2price.weighted, by = c("region", "period", "tech")) %>%
      left_join(df.eff, by = c("region", "period", "tech")) %>%
      left_join(df.emiFac, by = c("region", "tech")) %>%
      left_join(df.emifac.se2fe, by = c("region", "tech")) %>%
      left_join(df.Co2.Capt.Price, by = c("region", "period")) %>%
      left_join(df.co2_dem, by = c("region", "period", if (module2realisation["CCU", 2] == "on") "tech")) %>%
      left_join(df.CO2StoreShare, by = c("region", "period")) %>%
      left_join(df.secfuel, by = c("region", "period", "tech", "fuel")) %>%
      left_join(df.curtShare, by = c("region", "period", "tech")) %>%
      left_join(df.CCStax, by = c("region", "period", "tech")) %>%
      left_join(df.flexPriceShare, by = c("region", "period", "tech")) %>%
      left_join(df.tau_SE_tax, by = c("region", "period", "tech")) %>%
      left_join(df.FEtax, relationship = "many-to-many", by = c("region", "period", "output")) %>%
      left_join(df.AddTeInvH2, by = c("region", "period", "tech", "sector")) %>%
      # filter to only have LCOE technologies
      filter(tech %in% c(te_LCOE))


    # only retain unique region, period, tech combinations
    df.LCOE <- df.LCOE %>%
      unique(by = c("region", "period", "tech", "sector"))


    # replace NA by 0 in certain columns
    # columns where NA should be replaced by 0
    col.NA.zero <- c("OMF", "OMV", "AdjCost", "co2.price", "co2.price.weighted", "fuel.price", "fuel.price.weighted", "co2_dem", "emiFac.se2fe", "Co2.Capt.Price",
                     "secfuel.prod", "secfuel.price", "curtShare", "CCStax.cost", "FEtax", "AddH2TdCost", "tau_SE_tax")
    df.LCOE[, col.NA.zero][is.na(df.LCOE[, col.NA.zero])] <- 0

    # replace NA by 1 in certain columns
    # columns where NA should be replaced by 1
    col.NA.one <- c("FlexPriceShare")
    df.LCOE[, col.NA.one][is.na(df.LCOE[, col.NA.one])] <- 1

    # replace NA for sectors by "supply-side" (for all SE generating technologies)
    # Carbon management for all technologies handling CO2
    df.LCOE <- df.LCOE %>%
      mutate(sector = ifelse(tech %in% c(ccs2te$all_te),
                             "carbon management",
                             sector)) %>%
      mutate(sector = ifelse(tech %in% c(pe2se$all_te,
                                         se2se$all_te),
                             "supply-side",
                             sector)) %>%
      filter(!is.na(sector))




    ### unit conversion and data preparation before LCOE calculation
    df.LCOE <- df.LCOE %>%
      # remove some unnecessary columns
      select(-model, -scenario, -variable, -unit) %>%
      # unit conversions for CAPEX and OMV cost, adjustment cost
      # conversion from tr USD 2017/TW to USD2015/kW
      # in case of CCS technologies convert from tr USD2017/GtC to USD2015/tCO2
      mutate(CAPEX = ifelse(tech %in% ccs2te$all_te,
                            # CCS technology unit conversion
                            CAPEX * s_usd2017t2015 * 1e3 / 3.66,
                            # energy technology unit conversion
                            CAPEX * s_usd2017t2015 * 1e3),
             AdjCost = ifelse(tech %in% ccs2te$all_te,
                              # CCS technology unit conversion
                              AdjCost * s_usd2017t2015 * 1e3 / 3.66,
                              # energy technology unit conversion
                              AdjCost * s_usd2017t2015 * 1e3)) %>%
      # conversion from tr USD 2017/TWa to USD2015/MWh
      # in case of CCS technologies convert from tr USD2017/GtC to USD2015/tCO2
      mutate(OMV = ifelse(tech %in% ccs2te$all_te,
                          # CCS technology unit conversion
                          OMV * s_usd2017t2015 * 1e3 / 3.66,
                          # energy technology unit conversion
                          OMV * s_usd2017t2015 / as.numeric(s_twa2mwh) * 1e12)) %>%
      # share of stored carbon from captured carbon is only relevant for CCS technologies, others -> 1
      mutate(CO2StoreShare = ifelse(tech %in% teCCS, CO2StoreShare, 1)) %>%
      # calculate annuity factor for annualizing investment cost over lifetime
      mutate(annuity.fac = r * (1 + r)^lifetime / (-1 + (1 + r)^lifetime))


    # LCOE Calculation (marginal) ----


    df.LCOE <- df.LCOE %>%
      # investment cost LCOE in USD/MWh or USD/tCO2
      mutate(`Investment Cost` = ifelse(tech %in% ccs2te$all_te,
                                        # CCS technology, CAPEX in USD/tCO2
                                        CAPEX * annuity.fac / CapFac,
                                        # energy technology, CAPEX in USD/kW(out)
                                        CAPEX * annuity.fac / (CapFac * 8760) * 1e3)) %>%
      # OMF cost LCOE in USD/MWh, OMF are defined as share of CAPEX
      mutate(`OMF Cost` = ifelse(tech %in% ccs2te$all_te,
                                 # CCS technology, CAPEX in USD/tCO2
                                 CAPEX * OMF / CapFac,
                                 # energy technology, CAPEX in USD/kW(out)
                                 CAPEX * OMF / (CapFac * 8760) * 1e3)) %>%
      mutate(`OMV Cost` = OMV) %>%
      # adjustment cost LCOE in USD/MWh or USD/tCO2
      # (parallel to investment cost calculation)
      mutate(`Adjustment Cost` = ifelse(tech %in% ccs2te$all_te,
                                        # CCS technology, CAPEX in USD/tCO2
                                        AdjCost * annuity.fac / CapFac,
                                        # energy technology, CAPEX in USD/kW(out)
                                        AdjCost * annuity.fac / (CapFac * 8760) * 1e3)) %>%
      # Fuel Cost
      # # Fuel cost with fuel price of time step for which LCOE are calculated
      mutate(`Fuel Cost (time step prices)` = fuel.price / eff) %>%
      # Fuel cost with weighted average fuel price over plant lifetime
      mutate(`Fuel Cost (intertemporal prices)` = fuel.price.weighted / eff) %>%
      # CO2 Tax cost
      # CO2 Tax cost on SE level refer to (supply-side) emissions of pe2se technologies
      # CO2 Tax cost on FE level refer to (demand-side) emissions of FE carriers
      # # CO2 Tax cost with co2 price of time step for which LCOE are calculated
      mutate(`CO2 Tax Cost (time step prices)` = co2.price * (emiFac / eff) * CO2StoreShare) %>%
      # CO2 Tax cost with weighted average co2 price over plant lifetime
      # These are the  CO2 tax cost that feature the total LCOE calculate below
      mutate(`CO2 Tax Cost (intertemporal prices)` = co2.price.weighted * (emiFac / eff) * CO2StoreShare) %>%
      mutate(`CO2 Provision Cost` = Co2.Capt.Price * co2_dem) %>%
      # fuel cost of second fuel if technology has two inputs or outputs
      # is positive for cost of second input
      # is negative for benefit of a second output
      mutate(`Second Fuel Cost` = -(secfuel.prod * secfuel.price)) %>%
      # curtailment cost are generation LCOE of VRE technologies of curtailed generation
      mutate(`Curtailment Cost` = curtShare / (1 - curtShare) * (`Investment Cost` + `OMF Cost` + `OMV Cost`)) %>%
      # CCS Tax cost as defined in 21_tax module
      mutate(`CCS Tax Cost` = CCStax.cost) %>%
      # Flex Tax benefit for electrolysis
      # FlexPriceShare denotes share of the electricity price that electrolysis sees
      mutate(`Flex Tax` = -(1 - FlexPriceShare) * `Fuel Cost (time step prices)`) %>%
      # SE tax for electrolysis, calculates fuel cost increase due to taxes and grid fees on electricity going into electrolysis
      mutate(`SE Tax` = tau_SE_tax / eff) %>%
      # se2fe technologies come with FE tax
      mutate(`FE Tax` = FEtax,
             # FE H2 has some additional t&d cost at low H2 shares (phase-in cost) in REMIND
             `Additional H2 t&d Cost` = AddH2TdCost) %>%
      # calculate total LCOE by adding all components
      mutate(
        # Total LCOE with fuel cost and co2 tax cost based on fuel prices and carbon prices of time step for which LCOE are calculated
        `Total LCOE (time step prices)` = `Investment Cost` + `OMF Cost` + `OMV Cost` + `Adjustment Cost` + `Fuel Cost (time step prices)` + `CO2 Tax Cost (time step prices)` +
          `CO2 Provision Cost` + `Second Fuel Cost` + `CCS Tax Cost` + `Curtailment Cost` + `Flex Tax` + `SE Tax` + `FE Tax` + `Additional H2 t&d Cost`,
        # Total LCOE with fuel cost and co2 tax cost based on fuel prices and carbon prices that are intertemporally weighted and averaged over the plant lifetime
        `Total LCOE (intertemporal prices)` = `Investment Cost` + `OMF Cost` + `OMV Cost` + `Adjustment Cost` + `Fuel Cost (intertemporal prices)` + `CO2 Tax Cost (intertemporal prices)` +
          `CO2 Provision Cost` + `Second Fuel Cost` + `CCS Tax Cost` + `Curtailment Cost` + `Flex Tax` + `SE Tax` + `FE Tax` + `Additional H2 t&d Cost`)


    # Levelized Cost of UE in Buildings Putty Realization ----


    # Transform LCOE to output format ----

    # transform marginal LCOE to mif output format
    df.LCOE.out <- df.LCOE %>%
      select(region, period, tech, output, sector,
             `Investment Cost`, `Adjustment Cost`, `OMF Cost`, `OMV Cost`,
             `Fuel Cost (time step prices)`, `CO2 Tax Cost (time step prices)`,
             `Fuel Cost (intertemporal prices)`, `CO2 Tax Cost (intertemporal prices)`,
             `CO2 Provision Cost`, `Second Fuel Cost`, `Curtailment Cost`,
             `CCS Tax Cost`, `Flex Tax`, `SE Tax`, `FE Tax`, `Additional H2 t&d Cost`,
             `Total LCOE (time step prices)`,
             `Total LCOE (intertemporal prices)`
      ) %>%
      gather(cost, value, -region, -period, -tech, -output, -sector) %>%
      mutate(unit = ifelse(tech %in% ccs2te$all_te,
                           "US$2015/tCO2",
                           "US$2015/MWh"),
             type = "marginal") %>%
      filter(value != 0) %>%
      select(region, period, type, output, tech, sector, unit, cost, value)

    LCOE.mar.out <- as.magpie(df.LCOE.out, spatial = 1, temporal = 2, datacol = 9)



    # add DAC levelized cost, buildings UE LCOE to marginal LCOE
    LCOE.mar.out <- mbind(LCOE.mar.out, LCOD)
    # bind to previous calculations (if there are)
    LCOE.out <- mbind(LCOE.out, LCOE.mar.out)

  }

  ### calculate global average LCOE for region "World"
  LCOE.out.inclGlobal <- new.magpie(c(getRegions(LCOE.out), "GLO"), getYears(LCOE.out), getNames(LCOE.out))
  getSets(LCOE.out.inclGlobal) <- getSets(LCOE.out)
  LCOE.out.inclGlobal[getRegions(LCOE.out), , ] <- LCOE.out
  LCOE.out.inclGlobal["GLO", , ] <- dimSums(LCOE.out, dim = 1) / length(getRegions(LCOE.out))

  if (output.type  == "marginal detail") {
    out <- df.LCOE
  } else {
    out <- LCOE.out.inclGlobal
  }

  return(out)

}

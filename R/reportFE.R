#' Read in GDX and calculate final energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in final energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Renato Rodrigues, Felix Schreyer
#' @examples
#'
#'   \dontrun{reportFE(gdx)}
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie mselect getRegions getYears mbind setNames
#'                      getNames<- as.data.frame as.magpie getSets
#' @importFrom dplyr %>% filter full_join group_by left_join mutate rename
#'     select semi_join summarize ungroup
#' @importFrom quitte inline.data.frame revalue.levels
#' @importFrom rlang syms
#' @importFrom tibble as_tibble tibble tribble
#' @importFrom tidyr complete crossing expand_grid replace_na
#' @importFrom utils tail

reportFE <- function(gdx, regionSubsetList = NULL,
                     t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130,
                           2150)) {

  # conversion factors
  TWa_2_EJ     <- 31.536

  out <- NULL

  ####### Core Variables ##########

  # ---- read in needed data

  # ---- sets
  se2fe <- readGDX(gdx, "se2fe")
  entyFe2Sector <- readGDX(gdx, "entyFe2Sector")
  sector2emiMkt <- readGDX(gdx, "sector2emiMkt")

  entyFe2sector2emiMkt_NonEn <- readGDX(gdx, "entyFe2sector2emiMkt_NonEn", react = "silent")
  if (is.null(entySEfos <- readGDX(gdx, 'entySEfos', react = 'silent')))
    entySEfos <- c('sesofos', 'seliqfos', 'segafos')

  if (is.null(entySEbio <- readGDX(gdx, 'entySEbio', react = 'silent')))
    entySEbio <- c('sesobio', 'seliqbio', 'segabio')

  if (   is.null(entySEsyn <- readGDX(gdx, 'entySEsyn', react = 'silent'))
         || (length(entySEbio) == length(entySEsyn) && all(entySEbio == entySEsyn)))
    entySEsyn <- c('seliqsyn', 'segasyn')

  demFemapping <- entyFe2Sector %>%
    full_join(sector2emiMkt, by = 'emi_sectors', relationship = "many-to-many") %>%
    # rename such that all_enty1 always signifies the FE carrier like in
    # vm_demFeSector
    rename(all_enty1 = 'all_enty') %>%
    left_join(se2fe, by = 'all_enty1', relationship = "many-to-many") %>%
    select(-'all_te')

  # ---- parameter
  p_eta_conv <- readGDX(gdx, c("pm_eta_conv"), restore_zeros = FALSE, format="first_found")[, t, ]

  # ---- variables
  vm_prodSe <-      readGDX(gdx, name = c("vm_prodSe","v_seprod"), field = "l",
                            restore_zeros = FALSE, format = "first_found")[, t, ] * TWa_2_EJ
  vm_prodFe  <-     readGDX(gdx, name = c("vm_prodFe"), field = "l",
                            restore_zeros = FALSE, format = "first_found")[, t, ] * TWa_2_EJ
  vm_demFeSector <- readGDX(gdx, name = c("vm_demFeSector"), field = "l",
                            restore_zeros = FALSE, format = "first_found")[, t, ] * TWa_2_EJ
  vm_demFeSector[is.na(vm_demFeSector)] <- 0

  # FE non-energy use
  vm_demFENonEnergySector <- readGDX(gdx, "vm_demFENonEnergySector", field = "l",
                                     spatial = 2, restore_zeros = FALSE,
                                     react = "silent")[, t, ] * TWa_2_EJ


  # only retain combinations of SE, FE, sector, and emiMkt which actually exist in the model (see qm_balFe)
  vm_demFeSector <- vm_demFeSector[demFemapping]

  # only retain combinations of SE, FE, te which actually exist in the model (qm_balFe)
  vm_prodFe <- vm_prodFe[se2fe]

  # FE demand per industry subsector
  o37_demFeIndSub <- readGDX(gdx, "o37_demFeIndSub", restore_zeros = FALSE,
                             format = "first_found", react = 'silent')
  o37_demFeIndSub <- o37_demFeIndSub[,t,]
  o37_demFeIndSub[is.na(o37_demFeIndSub)] <- 0
  # convert to EJ
  o37_demFeIndSub <- o37_demFeIndSub * TWa_2_EJ



  ####### Realisation specific Variables ##########

  # Define current realisation for the different modules
  module2realisation <- readGDX(gdx, "module2realisation")
  rownames(module2realisation) <- module2realisation$modules

  find_real_module <- function(module_set, module_name){
    return(module_set[module_set$modules == module_name,2])
  }

  tran_mod = find_real_module(module2realisation,"transport")
  indu_mod = find_real_module(module2realisation,"industry")
  buil_mod = find_real_module(module2realisation,"buildings")
  cdr_mod  = find_real_module(module2realisation,"CDR")

  any_process_based <- length(readGDX(gdx, "secInd37Prc", react='silent')) > 0.
  steel_process_based <- "steel" %in% readGDX(gdx, "secInd37Prc", react='silent')
  chemicals_process_based <- "chemicals" %in% readGDX(gdx, "secInd37Prc", react='silent') #TOCHECK:QIANZHI



  # Preliminary Calculations ----


  # calculate FE non-energy use and FE without non-energy use
  vm_demFENonEnergySector <-  mselect(vm_demFENonEnergySector[demFemapping],
                                      all_enty1 = entyFe2sector2emiMkt_NonEn$all_enty,
                                      emi_sectors = entyFe2sector2emiMkt_NonEn$emi_sectors,
                                      all_emiMkt = entyFe2sector2emiMkt_NonEn$all_emiMkt)

  # calculate FE without non-energy use
  vm_demFeSector_woNonEn <- vm_demFeSector
  vm_demFeSector_woNonEn[,,getNames(vm_demFENonEnergySector )] <- vm_demFeSector[,,getNames(vm_demFENonEnergySector )]-vm_demFENonEnergySector


  # ---- FE total production (incl. non-energy use) ------
  out <- mbind(out,

               #Total
               setNames((dimSums(vm_prodFe,dim=3,na.rm=T)), "FE (EJ/yr)"),

               #Liquids
               setNames(dimSums(vm_prodFe[,,c("seliqfos","seliqbio","seliqsyn")],dim=3,na.rm=T),                                     "FE|+|Liquids (EJ/yr)"),
               setNames(dimSums(vm_prodFe[,,"seliqbio"],dim=3,na.rm=T),                                                              "FE|Liquids|+|Biomass (EJ/yr)"),
               setNames(dimSums(vm_prodFe[,,"seliqfos"],dim=3,na.rm=T),                                                              "FE|Liquids|+|Fossil (EJ/yr)"),
               setNames(dimSums(mselect(vm_prodFe, all_enty="seliqsyn") ,dim=3,na.rm=T),                                             "FE|Liquids|+|Hydrogen (EJ/yr)"),

               #Solids
               setNames(dimSums(vm_prodFe[,,c("sesobio","sesofos")],dim=3,na.rm=T),                                                  "FE|+|Solids (EJ/yr)"),
               setNames(dimSums(vm_prodFe[,,"sesobio"],dim=3,na.rm=T),                                                               "FE|Solids|+|Biomass (EJ/yr)"),
               setNames(p_eta_conv[,,"tdbiosos"] * dimSums(mselect(vm_prodSe,all_enty1="sesobio",all_te="biotrmod"),dim=3,na.rm=T),  "FE|Solids|Biomass|+|Modern (EJ/yr)"),
               setNames(p_eta_conv[,,"tdbiosos"] * dimSums(mselect(vm_prodSe,all_enty1="sesobio",all_te="biotr"),dim=3,na.rm=T),     "FE|Solids|Biomass|+|Traditional (EJ/yr)"),
               setNames(dimSums(vm_prodFe[,,"sesofos"],dim=3,na.rm=T),                                                               "FE|Solids|+|Fossil (EJ/yr)"),
               setNames(p_eta_conv[,,"tdfossos"] * dimSums(mselect(vm_prodSe,all_enty1="sesofos",all_enty="pecoal"),dim=3,na.rm=T),  "FE|Solids|Fossil|+|Coal (EJ/yr)"),

               #Gases
               setNames(dimSums(vm_prodFe[,,c("segafos","segabio","segasyn")],dim=3,na.rm=T),                                        "FE|+|Gases (EJ/yr)"),
               setNames(dimSums(vm_prodFe[,,"segabio"],dim=3,na.rm=T),                                                               "FE|Gases|+|Biomass (EJ/yr)"),
               setNames(dimSums(vm_prodFe[,,"segafos"],dim=3,na.rm=T),                                                               "FE|Gases|+|Fossil (EJ/yr)"),
               setNames(dimSums(mselect(vm_prodFe, all_enty="segasyn"),dim=3,na.rm=T),                                               "FE|Gases|+|Hydrogen (EJ/yr)"),

               #Electricity
               setNames(dimSums(vm_prodFe[,,c("feels","feelt")],dim=3,na.rm=T),                                                      "FE|+|Electricity (EJ/yr)"),

               #Heat
               setNames(dimSums(vm_prodFe[,,"sehe.fehes.tdhes"],dim=3,na.rm=T),                                                      "FE|+|Heat (EJ/yr)"),

               #Hydrogen
               setNames(dimSums(vm_prodFe[,,c("feh2s","feh2t")],dim=3,na.rm=T),                                                      "FE|+|Hydrogen (EJ/yr)"),

               #Emission markets
               setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="ES")  ,dim=3,na.rm=T)),                                         "FE|ESR (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="ETS")  ,dim=3,na.rm=T)),                                        "FE|ETS (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="other")  ,dim=3,na.rm=T)),                                      "FE|Outside ETS and ESR (EJ/yr)")
  )

  getSets(out, fulldim=FALSE)[3] <- "data"

  # ---- FE sector demand ------



  #FE per sector and per emission market (ETS and ESR)
  out <- mbind(out,

               #industry
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst")  ,dim=3,na.rm=T)),                                      "FE|++|Industry (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Industry|++|ESR (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Industry|++|ETS (EJ/yr)"),

               # industry liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Industry|ESR|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Industry|ETS|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Hydrogen (EJ/yr)"),

               # industry solids
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Solids (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|Industry|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|Industry|Solids|+|Fossil (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Industry|ESR|+|Solids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Solids|+|Fossil (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|Industry|ETS|+|Solids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Solids|+|Fossil (EJ/yr)"),

               # industry gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst"),dim=3,na.rm=T)),  "FE|Industry|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|Industry|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|Industry|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Industry|ESR|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                  "FE|Industry|ETS|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Hydrogen (EJ/yr)"),

               # industry electricity
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Electricity (EJ/yr)"),

               # industry heat
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Heat (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Heat (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Heat (EJ/yr)"),

               # industry hydrogen
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Hydrogen (EJ/yr)"),


               # transport
               ## all transport variables include bunkers, except when expressly written otherwise in the variable name
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|++|Transport (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|++|ESR (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|++|Outside ETS and ESR (EJ/yr)"),

               # transport liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|ESR|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Hydrogen (EJ/yr)"),


               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)),                    "FE|Transport|Outside ETS and ESR|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Hydrogen (EJ/yr)"),

               # transport gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans")  ,dim=3,na.rm=T)),                    "FE|Transport|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans"),dim=3,na.rm=T)),  "FE|Transport|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans") ,dim=3,na.rm=T)),  "FE|Transport|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans") ,dim=3,na.rm=T)),  "FE|Transport|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|ESR|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)),                   "FE|Transport|Outside ETS and ESR|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Hydrogen (EJ/yr)"),

               # transport electricity
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|ESR|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|+|Electricity (EJ/yr)"),

               # transport hydrogen
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|ESR|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),


               # Buildings
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|++|Buildings (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|++|ESR (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|++|ETS (EJ/yr)"),

               # buildings liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build")  ,dim=3,na.rm=T)),                     "FE|Buildings|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Buildings|ESR|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Hydrogen (EJ/yr)"),


               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Buildings|ETS|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Hydrogen (EJ/yr)"),

               # buildings solids
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build")  ,dim=3,na.rm=T)),                    "FE|Buildings|+|Solids (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Solids|+|Fossil (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Buildings|ESR|+|Solids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Solids|+|Fossil (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|Buildings|ETS|+|Solids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Solids|+|Fossil (EJ/yr)"),

               # buildings gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build")  ,dim=3,na.rm=T)),                    "FE|Buildings|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build"),dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build") ,dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build") ,dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Buildings|ESR|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Hydrogen (EJ/yr)"),

               # buildings electricity
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Electricity (EJ/yr)"),

               # buildings heat
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Heat (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Heat (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Heat (EJ/yr)"),

               # buildings hydrogen
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Hydrogen (EJ/yr)"),

               # CDR
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="CDR")  ,dim=3,na.rm=T)),                  "FE|++|CDR (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|+++|ETS (EJ/yr)"),

               # CDR liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",emi_sectors="CDR")  ,dim=3,na.rm=T)),                     "FE|CDR|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqbio",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqfos",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqsyn",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|CDR|ETS|+|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqbio",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqfos",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqsyn",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Hydrogen (EJ/yr)"),
               # CDR gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="CDR"),dim=3,na.rm=T)), "FE|CDR|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="CDR") ,dim=3,na.rm=T)), "FE|CDR|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="CDR") ,dim=3,na.rm=T)), "FE|CDR|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|CDR|ETS|+|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Hydrogen (EJ/yr)"),

               # CDR electricity
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Electricity (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Electricity (EJ/yr)"),

               # CDR hydrogen
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Hydrogen (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Hydrogen (EJ/yr)"),

               # CDR heat
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Heat (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Heat (EJ/yr)")

  )

  # TODO: can be removed as "complex" is not used anymore?
  # quick fix: to avoid duplicates of this variable with the version with a "+" below in case of transport complex: only calculate this in case of edge_esm being used:

  out <- mbind(out,
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|LDV|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|non-LDV|Liquids (EJ/yr)"))





  ### FE carriers from specific PE origin

  p_share_coal_liq <- dimSums(mselect(vm_prodSe, all_enty="pecoal", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)
  p_share_oil_liq <- dimSums(mselect(vm_prodSe, all_enty="peoil", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)
  p_share_gas_liq <- dimSums(mselect(vm_prodSe, all_enty="pegas", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)


  p_share_coal_liq[is.na(p_share_coal_liq)] <- 0
  p_share_oil_liq[is.na(p_share_oil_liq)] <- 0
  p_share_gas_liq[is.na(p_share_gas_liq)] <- 0

  p_share_ngas_gas <- dimSums(mselect(vm_prodSe, all_enty="pegas", all_enty1="segafos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="segafos"), dim=3, na.rm = T)
  p_share_coal_gas <- dimSums(mselect(vm_prodSe, all_enty="pecoal", all_enty1="segafos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="segafos"), dim=3, na.rm = T)

  p_share_ngas_gas[is.na(p_share_ngas_gas)] <- 0
  p_share_coal_gas[is.na(p_share_coal_gas)] <- 0

  # origin of fossil liquids and gases
  out <- mbind(out,

               # industry fossil liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Oil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Coal (EJ/yr)"),

               # buildings fossil liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Oil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Coal (EJ/yr)"),

               # transport fossil liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Oil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Coal (EJ/yr)"),

               # industry fossil gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="indst")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Industry|Gases|Fossil|+|Natural Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="indst")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Industry|Gases|Fossil|+|Coal (EJ/yr)"),

               # buildings fossil gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="build")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Buildings|Gases|Fossil|+|Natural Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="build")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Buildings|Gases|Fossil|+|Coal (EJ/yr)"),

               # transport fossil gases
               setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegat",emi_sectors="trans")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Transport|Gases|Fossil|+|Natural Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegat",emi_sectors="trans")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Transport|Gases|Fossil|+|Coal (EJ/yr)"),

               # total fossil liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Oil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Coal (EJ/yr)")


               # # total fossil gases, TODO: does not add up, check another time
               # setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1=c("fegas","fegat"))*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Gases|Fossil|+|Natural Gas (EJ/yr)"),
               # setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1=c("fegas","fegat"))*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Gases|Fossil|+|Coal (EJ/yr)")

  )

  # ---- Bunkers ----
  # creating additional variables with and without bunkers for the transport sector

  ## ESR variables correspond to transport without bunkers by definition
  var_without_Bunkers <- mbind(
    lapply(
      getNames(out)[grep("FE\\|Transport\\|ESR", getNames(out))],
      function(x) {
        setNames(out[, , x], gsub("FE\\|Transport\\|ESR",
                                  "FE\\|Transport\\|w/o Bunkers", x))
      }
    )
  )
  ## Other emission market variables correspond to transport with bunkers by definition
  var_with_Bunkers <- mbind(
    lapply(
      getNames(out)[grep("FE\\|Transport\\|Outside ETS and ESR", getNames(out))],
      function(x) {
        setNames(out[, , x], gsub("FE\\|Transport\\|Outside ETS and ESR",
                                  "FE\\|Transport\\|Bunkers", x))
      }
    )
  )
  out <- mbind(out, var_without_Bunkers, var_with_Bunkers)
  ##
  out <- mbind(out,
               setNames(out[, , "FE|Transport|++|ESR (EJ/yr)"], "FE|Transport|w/o Bunkers (EJ/yr)"),
               setNames(out[, , "FE|Transport|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Bunkers (EJ/yr)")
  )
  out <- mbind(out,
               setNames(out[, , "FE (EJ/yr)"] - out[, , "FE|Transport|Bunkers (EJ/yr)"],
                        "FE|w/o Bunkers (EJ/yr)")
  )



  # ---- read in needed data

  # ---- sets

  # ---- parameter
  pm_cesdata <- readGDX(gdx, "pm_cesdata")[, t, ]

  # ---- variables
  if(tran_mod == "edge_esm") {
    vm_demFeForEs <- readGDX(gdx,name = c("vm_demFeForEs"), field="l", restore_zeros=FALSE,format= "first_found",react = "silent")[,t,]*TWa_2_EJ
  }

  # CES nodes, convert from TWa to EJ
  vm_cesIO <- readGDX(gdx, name=c("vm_cesIO"), field="l", restore_zeros=FALSE,format= "first_found")[,t,]*TWa_2_EJ

  if(any_process_based){
    vm_outflowPrc <- readGDX(gdx, name=c("vm_outflowPrc"), field="l", restore_zeros=FALSE, format="first_found", react='silent')[,t,]
    o37_demFePrc <- readGDX(gdx, name=c("o37_demFePrc"), restore_zeros=FALSE,format= "first_found")[,t,] * TWa_2_EJ
    o37_demFePrc [is.na(o37_demFePrc )] = 0.
    o37_ProdIndRoute <- readGDX(gdx, name=c("o37_ProdIndRoute"), restore_zeros=FALSE, format="first_found", react='silent')[,t,]
    o37_ProdIndRoute[is.na(o37_ProdIndRoute)] = 0.
    o37_demFeIndRoute <- readGDX(gdx, name=c("o37_demFeIndRoute"), restore_zeros=FALSE, format="first_found", react='silent')[,t,] * TWa_2_EJ
    o37_demFeIndRoute[is.na(o37_demFeIndRoute)] = 0.

    v37_matFlow <- readGDX(gdx, name=c("v37_matFlow"), field="l", restore_zeros=FALSE,format= "first_found")[,t,] 
    v37_matFlow [is.na(v37_matFlow )] = 0.
    # mapping of process to output materials
    tePrc2ue <- readGDX(gdx, "tePrc2ue", restore_zeros=FALSE)
  }

  # ---- transformations
  # Correct for offset quantities in the transition between ESM and CES for zero quantities
  if (any(grep('\\.offset_quantity$', getNames(pm_cesdata)))) {
    pf <- paste0(getNames(vm_cesIO), '.offset_quantity')
    offset <- collapseNames(pm_cesdata[,,pf]) * TWa_2_EJ
    vm_cesIO <- vm_cesIO + offset[,t,getNames(vm_cesIO)]
  }


  # ---- Buildings Module ----

  p36_floorspace <- readGDX(gdx, "p36_floorspace", react = "silent")[, t, ]
  if (!is.null(p36_floorspace)) {
    if (dim(p36_floorspace)[3] > 1) {
      out <- mbind(out,
                   setNames(p36_floorspace[, , "buildings"],   "ES|Buildings|Floor Space (bn m2)"),
                   setNames(p36_floorspace[, , "residential"], "ES|Buildings|Residential|Floor Space (bn m2)"),
                   setNames(p36_floorspace[, , "commercial"],  "ES|Buildings|Commercial|Floor Space (bn m2)"))
    } else {
      out <- mbind(out, setNames(p36_floorspace, "ES|Buildings|Floor Space (bn m2)"))
    }
  }

  if (buil_mod == "simple") {
    # PPF in REMIND and the respective reporting variables
    carrierBuild <- c(
      feelcb  = "FE|Buildings|non-Heating|Electricity|Conventional (EJ/yr)",
      feelrhb = "FE|Buildings|Heating|Electricity|Resistance (EJ/yr)",
      feelhpb = "FE|Buildings|Heating|Electricity|Heat pump (EJ/yr)",
      feheb   = "FE|Buildings|Heating|District Heating (EJ/yr)",
      fesob   = "FE|Buildings|Heating|Solids (EJ/yr)",
      fehob   = "FE|Buildings|Heating|Liquids (EJ/yr)",
      fegab   = "FE|Buildings|Heating|Gases (EJ/yr)",
      feh2b   = "FE|Buildings|Heating|Hydrogen (EJ/yr)")
    # all final energy (FE) demand in buildings without coventional electricity
    # is summed as heating
    carrierBuildHeating <- tail(carrierBuild, -1)

    # FE demand in buildings for each carrier
    # (electricity split: heat pumps, resistive heating, rest)
    for (c in names(carrierBuild)) {
      out <- mbind(out, setNames(dimSums(vm_cesIO[,, c], dim = 3, na.rm = TRUE),
                                 carrierBuild[c]))
    }

    # sum of heating FE demand
    out <- mbind(out,
                 setNames(dimSums(vm_cesIO[,, names(carrierBuildHeating)], dim = 3, na.rm = TRUE),
                          "FE|Buildings|Heating (EJ/yr)"))

    # UE demand in buildings for each carrier
    # this buildings realisation only works on a FE level but the UE demand is
    # estimated here assuming the FE-UE efficiency of the basline (from EDGE-B)
    p36_uedemand_build <- readGDX(gdx, "p36_uedemand_build", react = "silent")[, t, ]
    if (!is.null(p36_uedemand_build)) {
      pm_fedemand <- readGDX(gdx, "pm_fedemand")[, t, ]
      feUeEff_build <- p36_uedemand_build[,, names(carrierBuild)] /
        pm_fedemand[,, names(carrierBuild)]
      feUeEff_build[is.na(feUeEff_build) | is.infinite(feUeEff_build)] <- 1
      # assume efficiency for all gases also for H2
      feUeEff_build[,, "feh2b"] <- setNames(feUeEff_build[,, "fegab"], "feh2b")
      # apply efficiency to get UE levels
      uedemand_build <- vm_cesIO[,, names(carrierBuild)] * feUeEff_build
      getItems(uedemand_build, 3) <-
        gsub("^FE", "UE", carrierBuild)[getItems(uedemand_build, 3)]
      out <- mbind(out, uedemand_build)

      # sum of heating UE demand
      out <- mbind(out,
                   setNames(dimSums(out[,, gsub("^FE", "UE", carrierBuildHeating)],
                                    dim = 3, na.rm = TRUE),
                            "UE|Buildings|Heating (EJ/yr)"))

      # sum of buildings UE demand
      out <- mbind(out,
                   setNames(dimSums(out[,, gsub("^FE", "UE", carrierBuild)],
                                    dim = 3, na.rm = TRUE),
                            "UE|Buildings (EJ/yr)"))
    }
  }

  # Industry Module ----
  ## FE demand ----

  # Big ol' table of variables to report, along with indices into
  # o37_demFeIndSub to select the right sets.  Indices can be either literal
  # strings or character vector objects.  NULL indices are not included in the
  # mselect() call.
  mixer <- tribble(
    ~variable,                                                 ~all_enty,   ~all_enty1,   ~secInd37,
    "FE|Industry|+++|Cement (EJ/yr)",                          NULL,        NULL,         "cement",
    "FE|Industry|Cement|+|Solids (EJ/yr)",                     NULL,        "fesos",      "cement",
    "FE|Industry|Cement|Solids|+|Fossil (EJ/yr)",              entySEfos,   "fesos",      "cement",
    "FE|Industry|Cement|Solids|+|Biomass (EJ/yr)",             entySEbio,   "fesos",      "cement",
    "FE|Industry|Cement|+|Liquids (EJ/yr)",                    NULL,        "fehos",      "cement",
    "FE|Industry|Cement|Liquids|+|Fossil (EJ/yr)",             entySEfos,   "fehos",      "cement",
    "FE|Industry|Cement|Liquids|+|Biomass (EJ/yr)",            entySEbio,   "fehos",      "cement",
    "FE|Industry|Cement|Liquids|+|Hydrogen (EJ/yr)",           entySEsyn,   "fehos",      "cement",
    "FE|Industry|Cement|+|Gases (EJ/yr)",                      NULL,        "fegas",      "cement",
    "FE|Industry|Cement|Gases|+|Fossil (EJ/yr)",               entySEfos,   "fegas",      "cement",
    "FE|Industry|Cement|Gases|+|Biomass (EJ/yr)",              entySEbio,   "fegas",      "cement",
    "FE|Industry|Cement|Gases|+|Hydrogen (EJ/yr)",             entySEsyn,   "fegas",      "cement",
    "FE|Industry|Cement|+|Hydrogen (EJ/yr)",                   NULL,        "feh2s",      "cement",
    "FE|Industry|Cement|+|Electricity (EJ/yr)",                NULL,        "feels",      "cement",

    "FE|Industry|+++|Chemicals (EJ/yr)",                       NULL,        NULL,         "chemicals",
    "FE|Industry|Chemicals|+|Solids (EJ/yr)",                  NULL,        "fesos",      "chemicals",
    "FE|Industry|Chemicals|Solids|+|Fossil (EJ/yr)",           entySEfos,   "fesos",      "chemicals",
    "FE|Industry|Chemicals|Solids|+|Biomass (EJ/yr)",          entySEbio,   "fesos",      "chemicals",
    "FE|Industry|Chemicals|+|Liquids (EJ/yr)",                 NULL,        "fehos",      "chemicals",
    "FE|Industry|Chemicals|Liquids|+|Fossil (EJ/yr)",          entySEfos,   "fehos",      "chemicals",
    "FE|Industry|Chemicals|Liquids|+|Biomass (EJ/yr)",         entySEbio,   "fehos",      "chemicals",
    "FE|Industry|Chemicals|Liquids|+|Hydrogen (EJ/yr)",        entySEsyn,   "fehos",      "chemicals",
    "FE|Industry|Chemicals|+|Gases (EJ/yr)",                   NULL,        "fegas",      "chemicals",
    "FE|Industry|Chemicals|Gases|+|Fossil (EJ/yr)",            entySEfos,   "fegas",      "chemicals",
    "FE|Industry|Chemicals|Gases|+|Biomass (EJ/yr)",           entySEbio,   "fegas",      "chemicals",
    "FE|Industry|Chemicals|Gases|+|Hydrogen (EJ/yr)",          entySEsyn,   "fegas",      "chemicals",
    "FE|Industry|Chemicals|+|Hydrogen (EJ/yr)",                NULL,        "feh2s",      "chemicals",
    "FE|Industry|Chemicals|+|Electricity (EJ/yr)",             NULL,        "feels",      "chemicals",

    "FE|Industry|Steel|+|Solids (EJ/yr)",                      NULL,        "fesos",      "steel",
    "FE|Industry|+++|Steel (EJ/yr)",                           NULL,        NULL,         "steel",
    "FE|Industry|Steel|Solids|+|Fossil (EJ/yr)",               entySEfos,   "fesos",      "steel",
    "FE|Industry|Steel|Solids|+|Biomass (EJ/yr)",              entySEbio,   "fesos",      "steel",
    "FE|Industry|Steel|+|Liquids (EJ/yr)",                     NULL,        "fehos",      "steel",
    "FE|Industry|Steel|Liquids|+|Fossil (EJ/yr)",              entySEfos,   "fehos",      "steel",
    "FE|Industry|Steel|Liquids|+|Biomass (EJ/yr)",             entySEbio,   "fehos",      "steel",
    "FE|Industry|Steel|Liquids|+|Hydrogen (EJ/yr)",            entySEsyn,   "fehos",      "steel",
    "FE|Industry|Steel|+|Gases (EJ/yr)",                       NULL,        "fegas",      "steel",
    "FE|Industry|Steel|Gases|+|Fossil (EJ/yr)",                entySEfos,   "fegas",      "steel",
    "FE|Industry|Steel|Gases|+|Biomass (EJ/yr)",               entySEbio,   "fegas",      "steel",
    "FE|Industry|Steel|Gases|+|Hydrogen (EJ/yr)",              entySEsyn,   "fegas",      "steel",
    "FE|Industry|Steel|+|Hydrogen (EJ/yr)",                    NULL,        "feh2s",      "steel",
    "FE|Industry|Steel|+|Electricity (EJ/yr)",                 NULL,        "feels",      "steel",

    "FE|Industry|+++|Other Industry (EJ/yr)",                  NULL,        NULL,         "otherInd",
    "FE|Industry|Other Industry|+|Solids (EJ/yr)",             NULL,        "fesos",      "otherInd",
    "FE|Industry|Other Industry|Solids|+|Fossil (EJ/yr)",      entySEfos,   "fesos",      "otherInd",
    "FE|Industry|Other Industry|Solids|+|Biomass (EJ/yr)",     entySEbio,   "fesos",      "otherInd",
    "FE|Industry|Other Industry|+|Liquids (EJ/yr)",            NULL,        "fehos",      "otherInd",
    "FE|Industry|Other Industry|Liquids|+|Fossil (EJ/yr)",     entySEfos,   "fehos",      "otherInd",
    "FE|Industry|Other Industry|Liquids|+|Biomass (EJ/yr)",    entySEbio,   "fehos",      "otherInd",
    "FE|Industry|Other Industry|Liquids|+|Hydrogen (EJ/yr)",   entySEsyn,   "fehos",      "otherInd",
    "FE|Industry|Other Industry|+|Gases (EJ/yr)",              NULL,        "fegas",      "otherInd",
    "FE|Industry|Other Industry|Gases|+|Fossil (EJ/yr)",       entySEfos,   "fegas",      "otherInd",
    "FE|Industry|Other Industry|Gases|+|Biomass (EJ/yr)",      entySEbio,   "fegas",      "otherInd",
    "FE|Industry|Other Industry|Gases|+|Hydrogen (EJ/yr)",     entySEsyn,   "fegas",      "otherInd",
    "FE|Industry|Other Industry|+|Hydrogen (EJ/yr)",           NULL,        "feh2s",      "otherInd",
    "FE|Industry|Other Industry|+|Heat (EJ/yr)",               NULL,        "fehes",      "otherInd",
    "FE|Industry|Other Industry|+|Electricity (EJ/yr)",        NULL,        "feels",      "otherInd")

  # Convert a mixer table into a list that can be passed to mselect() to
  # select specified dimensions from a magpie object
  .mixer_to_selector <- function(mixer) {
    selector <- list()
    for (i in seq_len(nrow(mixer))) {
      selector <- c(
        selector,

        list(mixer[i,] %>%
               as.list() %>%
               # exclude list entries that are NULL
               Filter(f = function(x) { !is.null(x[[1]]) }) %>%
               # coerce character vector elements one level up
               lapply(unlist))
      )
    }

    return(selector)
  }

  # call mselect(), dimSums(), setNames(), and multiply by factor
  .select_sum_name_multiply <- function(object, selector, factor = 1) {
    lapply(selector, function(x) {  # for each element in <selector>
      # call mselect() on <object>, but without the 'variable' element
      ( mselect(object, x[setdiff(names(x), 'variable')]) %>%
          dimSums(dim = 3) %>%
          setNames(x[['variable']])
        * factor
      )
    })
  }

  # calculate and bind to out
  out <- mbind(
    c(list(out), # pass a list of magpie objects
      .select_sum_name_multiply(o37_demFeIndSub, .mixer_to_selector(mixer))
    ))

  # subsectors realisation specific
  if (indu_mod == 'subsectors') {

    if (steel_process_based) {

      # technologies and operation modes that belong to primary and secondary steel
      teOpmoSteelPrimary <- tePrc2ue %>%
        filter(.data$all_in == "ue_steel_primary")
      # HotFix TODO::QIANZHI
      if("all_te" %in% colnames(teOpmoSteelPrimary)){
        teSteelPrimary <- teOpmoSteelPrimary %>% pull(all_te)
      } else if("tePrc" %in% colnames(teOpmoSteelPrimary)){
        teSteelPrimary <- teOpmoSteelPrimary %>% pull(tePrc)
      } 
      opmoSteelPrimary <- teOpmoSteelPrimary %>% pull('opmoPrc')
      teOpmoSteelSecondary <- tePrc2ue %>%
        filter(.data$all_in == "ue_steel_secondary")
      if("all_te" %in% colnames(teOpmoSteelSecondary)){
        teSteelSecondary <- teOpmoSteelSecondary %>% pull(all_te)
      } else if("tePrc" %in% colnames(teOpmoSteelSecondary)){
        teSteelSecondary <- teOpmoSteelSecondary %>% pull(tePrc)
      } 
      opmoSteelSecondary <- teOpmoSteelSecondary %>% pull('opmoPrc')

      # Electricity uses by primary/secondary steel
      mixer <- tribble(
        ~variable,                                               ~all_enty,        ~all_te,           ~opmoPrc,
        "FE|Industry|Steel|Primary|Electricity (EJ/yr)",         "feels",          teSteelPrimary,    opmoSteelPrimary,
        "FE|Industry|Steel|Secondary|Electricity (EJ/yr)",       "feels",          teSteelSecondary,  opmoSteelSecondary,
        "FE|Industry|Steel|++|Primary (EJ/yr)",                  NULL,             teSteelPrimary,    opmoSteelPrimary,
        "FE|Industry|Steel|++|Secondary (EJ/yr)",                NULL,             teSteelSecondary,  opmoSteelSecondary)

      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(o37_demFePrc, .mixer_to_selector(mixer))
        ))

      # Electricity use by route
      mixer <- tribble(
        ~variable,                                      ~all_enty,  ~all_te,  ~route,           ~secInd37,
        "FE|Industry|Steel|+++|BF-BOF (EJ/yr)",         NULL,       NULL,     "bfbof",          "steel",
        "FE|Industry|Steel|+++|BF-BOF-CCS (EJ/yr)",     NULL,       NULL,     "bfbof_ccs",      "steel",
        "FE|Industry|Steel|+++|DRI-NG-EAF (EJ/yr)",     NULL,       NULL,     "idreaf_ng",      "steel",
        "FE|Industry|Steel|+++|DRI-NG-EAF-CCS (EJ/yr)", NULL,       NULL,     "idreaf_ng_ccs",  "steel",
        "FE|Industry|Steel|+++|DRI-H2-EAF (EJ/yr)",     NULL,       NULL,     "idreaf_h2",      "steel",
        "FE|Industry|Steel|+++|SCRAP-EAF (EJ/yr)",      NULL,       NULL,     "seceaf",         "steel")

      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(o37_demFeIndRoute, .mixer_to_selector(mixer))
        ))

    } else {

      # mapping of industrial output to energy production factors in CES tree
      ces_eff_target_dyn37 <- readGDX(gdx, "ces_eff_target_dyn37")

      # energy production factors for primary and secondary steel
      en.ppfen.primary.steel <- ces_eff_target_dyn37 %>%
        filter(.data$all_in == "ue_steel_primary") %>%
        pull('all_in1')
      en.ppfen.sec.steel <- ces_eff_target_dyn37 %>%
        filter(.data$all_in == "ue_steel_secondary") %>%
        pull('all_in1')

      mixer <- tribble(
        ~variable,                                                                                     ~all_in,
        "FE|Industry|Steel|Primary|Electricity (EJ/yr)",                                               "feel_steel_primary",
        "FE|Industry|Steel|Secondary|Electricity (EJ/yr)",                                             "feel_steel_secondary",
        "FE|Industry|Steel|++|Primary (EJ/yr)",                                                        en.ppfen.primary.steel,
        "FE|Industry|Steel|++|Secondary (EJ/yr)",                                                      en.ppfen.sec.steel)

      # calculate and bind to out
      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer))
        ))
    }

    if (chemicals_process_based) {

      routes_otherChem  <- c("otherChem_old", "otherChem_elec", "otherChem_h2")
      routes_HVC        <- c("HVC_stCrLiq", "HVC_stCrNg", "HVC_meSol", "HVC_meNg", "HVC_meLiq", "HVC_meSol_gh2", "HVC_meSol_cc", "HVC_meNg_cc", "HVC_meLiq_cc", "HVC_meh2")
      routes_fertilizer <- c("fertilizer_amSol", "fertilizer_amNg", "fertilizer_amLiq", "fertilizer_amSol_cc", "fertilizer_amNg_cc", "fertilizer_amLiq_cc", "fertilizer_amh2")
      routes_meFinal    <- c("meFinal_sol", "meFinal_ng", "meFinal_liq", "meFinal_sol_gh2", "meFinal_sol_cc", "meFinal_ng_cc", "meFinal_liq_cc", "meFinal_h2")
      routes_ammoFInal  <- c("amFinal_sol", "amFinal_ng", "amFinal_liq", "amFinal_sol_cc", "amFinal_ng_cc", "amFinal_liq_cc", "amFinal_h2")

      mixer <- tribble(
        ~variable,                                        ~all_enty, ~all_te, ~route,            ~secInd37,
       "FE|Industry|Chemicals|++|Other Chemicals (EJ/yr)", NULL,      NULL,    routes_otherChem,  "chemicals",
       "FE|Industry|Chemicals|++|HVC (EJ/yr)",             NULL,      NULL,    routes_HVC,        "chemicals",
       "FE|Industry|Chemicals|++|Fertilizer (EJ/yr)",      NULL,      NULL,    routes_fertilizer, "chemicals",
       "FE|Industry|Chemicals|++|Methanol Final (EJ/yr)",  NULL,      NULL,    routes_meFinal,    "chemicals",
       "FE|Industry|Chemicals|++|Ammonia Final (EJ/yr)",   NULL,      NULL,    routes_ammoFInal,  "chemicals")

      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(o37_demFeIndRoute, .mixer_to_selector(mixer))
        ))

      mixer <- tribble(
        ~variable,                                                                          ~all_enty, ~all_te, ~route,                 ~secInd37,
       "FE|Industry|Chemicals|Other Chemicals|+|Conventional (EJ/yr)",                      NULL,      NULL,    "otherChem_old",        "chemicals",
       "FE|Industry|Chemicals|Other Chemicals|+|Electrified (EJ/yr)",                       NULL,      NULL,    "otherChem_elec",       "chemicals",
       "FE|Industry|Chemicals|Other Chemicals|+|H2-based (EJ/yr)",                          NULL,      NULL,    "otherChem_h2",         "chemicals",

       "FE|Industry|Chemicals|HVC|+|From liquids-based steam cracker (EJ/yr)",              NULL,      NULL,    "HVC_stCrLiq",          "chemicals",
       "FE|Industry|Chemicals|HVC|+|From gas-based steam cracker (EJ/yr)",                  NULL,      NULL,    "HVC_stCrNg",           "chemicals",
       "FE|Industry|Chemicals|HVC|+|From solids-based methanol (EJ/yr)",                    NULL,      NULL,    "HVC_meSol",            "chemicals",
       "FE|Industry|Chemicals|HVC|+|From gas-based methanol (EJ/yr)",                       NULL,      NULL,    "HVC_meNg",             "chemicals",
       "FE|Industry|Chemicals|HVC|+|From liquids-based methanol (EJ/yr)",                   NULL,      NULL,    "HVC_meLiq",            "chemicals",
       "FE|Industry|Chemicals|HVC|+|From solids and green hydrogen-based methanol (EJ/yr)", NULL,      NULL,    "HVC_meSol_gh2",        "chemicals",
       "FE|Industry|Chemicals|HVC|+|From solids-based methanol with CC (EJ/yr)",            NULL,      NULL,    "HVC_meSol_cc",         "chemicals",
       "FE|Industry|Chemicals|HVC|+|From gas-based methanol with CC (EJ/yr)",               NULL,      NULL,    "HVC_meNg_cc",          "chemicals",
       "FE|Industry|Chemicals|HVC|+|From liquids-based methanol with CC (EJ/yr)",           NULL,      NULL,    "HVC_meLiq_cc",         "chemicals",
       "FE|Industry|Chemicals|HVC|+|From green hydogren-based methanol (EJ/yr)",            NULL,      NULL,    "HVC_meh2",             "chemicals",

       "FE|Industry|Chemicals|Fertilizer|+|From solids-based ammonia (EJ/yr)",              NULL,      NULL,    "fertilizer_amSol",     "chemicals",
       "FE|Industry|Chemicals|Fertilizer|+|From gas-based ammonia (EJ/yr)",                 NULL,      NULL,    "fertilizer_amNg",      "chemicals",
       "FE|Industry|Chemicals|Fertilizer|+|From liquids-based ammonia (EJ/yr)",             NULL,      NULL,    "fertilizer_amLiq",     "chemicals",
       "FE|Industry|Chemicals|Fertilizer|+|From solids-based ammonia with CC (EJ/yr)",      NULL,      NULL,    "fertilizer_amSol_cc",  "chemicals",
       "FE|Industry|Chemicals|Fertilizer|+|From gas-based ammonia with CC (EJ/yr)",         NULL,      NULL,    "fertilizer_amNg_cc",   "chemicals",
       "FE|Industry|Chemicals|Fertilizer|+|From liquids-based ammonia with CC (EJ/yr)",     NULL,      NULL,    "fertilizer_amLiq_cc",  "chemicals",
       "FE|Industry|Chemicals|Fertilizer|+|From green hydrogen-based ammonia (EJ/yr)",      NULL,      NULL,    "fertilizer_amh2",      "chemicals",

       "FE|Industry|Chemicals|Methanol Final|+|Solids-based (EJ/yr)",                       NULL,      NULL,    "meFinal_sol",          "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Gas-based (EJ/yr)",                          NULL,      NULL,    "meFinal_ng",           "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Liquids-based (EJ/yr)",                      NULL,      NULL,    "meFinal_liq",          "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Solids and green hydrogen-based (EJ/yr)",    NULL,      NULL,    "meFinal_sol_gh2",      "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Solids-based with CC (EJ/yr)",               NULL,      NULL,    "meFinal_sol_cc",       "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Gas-based with CC (EJ/yr)",                  NULL,      NULL,    "meFinal_ng_cc",        "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Liquids-based with CC (EJ/yr)",              NULL,      NULL,    "meFinal_liq_cc",       "chemicals",
       "FE|Industry|Chemicals|Methanol Final|+|Green hydrogen-based (EJ/yr)",               NULL,      NULL,    "meFinal_h2",           "chemicals",

       "FE|Industry|Chemicals|Ammonia Final|+|Solids-based (EJ/yr)",                        NULL,      NULL,    "amFinal_sol",          "chemicals",
       "FE|Industry|Chemicals|Ammonia Final|+|Gas-based (EJ/yr)",                           NULL,      NULL,    "amFinal_ng",           "chemicals",
       "FE|Industry|Chemicals|Ammonia Final|+|Liquids-based (EJ/yr)",                       NULL,      NULL,    "amFinal_liq",          "chemicals",
       "FE|Industry|Chemicals|Ammonia Final|+|Solids-based with CC (EJ/yr)",                NULL,      NULL,    "amFinal_sol_cc",       "chemicals",
       "FE|Industry|Chemicals|Ammonia Final|+|Gas-based with CC (EJ/yr)",                   NULL,      NULL,    "amFinal_ng_cc",        "chemicals",
       "FE|Industry|Chemicals|Ammonia Final|+|Liquids-based with CC (EJ/yr)",               NULL,      NULL,    "amFinal_liq_cc",       "chemicals",
       "FE|Industry|Chemicals|Ammonia Final|+|Green hydrogen-based (EJ/yr)",                NULL,      NULL,    "amFinal_h2",           "chemicals")

      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(o37_demFeIndRoute, .mixer_to_selector(mixer))
        ))

    } else {

      # mapping of industrial output to energy production factors in CES tree
      ces_eff_target_dyn37 <- readGDX(gdx, "ces_eff_target_dyn37")

      # energy production factors for primary and secondary steel
      en.ppfen.chemicals <- ces_eff_target_dyn37 %>%
        filter(.data$all_in == "ue_chemicals") %>%
        pull('all_in1')

      mixer <- tribble(
        ~variable,                                                                                     ~all_in,
        "FE|Industry|Chemicals|Electricity|+|Mechanical work and low-temperature heat (EJ/yr)",        "feelwlth_chemicals",
        "FE|Industry|Chemicals|Electricity|+|High-temperature heat (EJ/yr)",                           "feelhth_chemicals")

      # calculate and bind to out
      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer))
        ))
    }


    mixer <- tribble(
      ~variable,                                                                                     ~all_in,
      "FE|Industry|Other Industry|Electricity|+|Mechanical work and low-temperature heat (EJ/yr)",   "feelwlth_otherInd",
      "FE|Industry|Other Industry|Electricity|+|High-temperature heat (EJ/yr)",                      "feelhth_otherInd")

    # calculate and bind to out
    out <- mbind(
      c(list(out), # pass a list of magpie objects
        .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer))
      ))
  }

  ## Industry Production/Value Added ----
  # reporting of industry production and value added as given by CES nodes
  # (only available in industry subsectors)
  if (indu_mod == 'subsectors') {
    mixer <- tribble(
      ~variable,                                                    ~all_in,
      "Production|Industry|Cement (Mt/yr)",                         "ue_cement",
      "Production|Industry|Steel (Mt/yr)",                          c("ue_steel_primary", "ue_steel_secondary"),
      "Production|Industry|Steel|Primary (Mt/yr)",                  "ue_steel_primary",
      "Production|Industry|Steel|Secondary (Mt/yr)",                "ue_steel_secondary",
      "Value Added|Industry|Chemicals (billion US$2017/yr)",        "ue_chemicals",
      "Value Added|Industry|Other Industry (billion US$2017/yr)",   "ue_otherInd")

    # calculate and bind to out
    out <- mbind(
      c(list(out), # pass a list of magpie objects
        # as vm_cesIO was multiplied by TWa_2_EJ in the beginning of the
        # script, needs to be converted back to REMIND units here and then
        # scaled by 1e3 for obtaining Mt or billion US$2017
        .select_sum_name_multiply(vm_cesIO, .mixer_to_selector(mixer),
                                  factor=1e3 / TWa_2_EJ),
        # report CES node of total industry as internal variable (for model
        # diagnostics) to represent total industry activity
        list(setNames(mselect(vm_cesIO, all_in = "ue_industry"),
                      "Internal|Activity|Industry (arbitrary unit/yr)"))
      )
    )


    # reporting of process-based industry production per process-route
    if (steel_process_based) {
      mixer <- tribble(
        ~variable,                                            ~mat,          ~route,
        "Production|Industry|Steel|+|BF-BOF (Mt/yr)",         "prsteel",     "bfbof",
        "Production|Industry|Steel|+|BF-BOF-CCS (Mt/yr)",     "prsteel",     "bfbof_ccs",
        "Production|Industry|Steel|+|DRI-NG-EAF (Mt/yr)",     "prsteel",     "idreaf_ng",
        "Production|Industry|Steel|+|DRI-NG-EAF-CCS (Mt/yr)", "prsteel",     "idreaf_ng_ccs",
        "Production|Industry|Steel|+|DRI-H2-EAF (Mt/yr)",     "prsteel",     "idreaf_h2",
        "Production|Industry|Steel|+|SCRAP-EAF (Mt/yr)",      "sesteel",     "seceaf"
      )


        out <- mbind(c(list(out),
                     .select_sum_name_multiply(o37_ProdIndRoute, .mixer_to_selector(mixer), factor=1e3))) # factor 1e3 converts Gt/yr to Mt/yr
    }

    #TOCHECK:QIANZHI
    if (chemicals_process_based) {

      mixer <- tribble(
        ~variable,                                                         ~all_enty,
        "Production|Industry|Chemicals|Other Chemicals (Mt/yr)",           "otherChem",
        "Production|Industry|Chemicals|HVC (Mt/yr)",                       "HVC",
        "Production|Industry|Chemicals|Methanol Final (Mt/yr)",            "methFinal",
        "Production|Industry|Chemicals|Ammonia Final (Mt/yr)",             "ammoFinal",
        "Production|Industry|Chemicals|Methanol (Mt/yr)",                  "methanol",
        "Production|Industry|Chemicals|Ammonia (Mt/yr)",                   "ammonia",

        # TODO: Missing - OtherChem, Fertilizer, HVC

      out <- mbind(
        c(list(out), # pass a list of magpie objects
          .select_sum_name_multiply(v37_matFlow, .mixer_to_selector(mixer), factor=1e3)
        ))


        # TODO: think about routes in chemicals
     mixer <- tribble(
       ~variable,                                                                                   ~mat,         ~route, 
       "Production|Industry|Chemicals|Other Chemicals|+|Conventional (Mt/yr)",                      "OtherChem",  "otherChem_old",
       "Production|Industry|Chemicals|Other Chemicals|+|Electrified (Mt/yr)",                       "OtherChem",  "otherChem_elec",
       "Production|Industry|Chemicals|Other Chemicals|+|H2-based (Mt/yr)",                          "OtherChem",  "otherChem_h2",

       "Production|Industry|Chemicals|HVC|+|From liquids-based steam cracker (Mt/yr)",              "HVC",        "HVC_stCrLiq",
       "Production|Industry|Chemicals|HVC|+|From gas-based steam cracker (Mt/yr)",                  "HVC",        "HVC_stCrNg",
       "Production|Industry|Chemicals|HVC|+|From solids-based methanol (Mt/yr)",                    "HVC",        "HVC_meSol",    
       "Production|Industry|Chemicals|HVC|+|From gas-based methanol (Mt/yr)",                       "HVC",        "HVC_meNg",
       "Production|Industry|Chemicals|HVC|+|From liquids-based methanol (Mt/yr)",                   "HVC",        "HVC_meLiq",
       "Production|Industry|Chemicals|HVC|+|From solids and green hydrogen-based methanol (Mt/yr)", "HVC",        "HVC_meSol_gh2",
       "Production|Industry|Chemicals|HVC|+|From solids-based methanol with CC (Mt/yr)",            "HVC",        "HVC_meSol_cc",
       "Production|Industry|Chemicals|HVC|+|From gas-based methanol with CC (Mt/yr)",               "HVC",        "HVC_meNg_cc",
       "Production|Industry|Chemicals|HVC|+|From liquids-based methanol with CC (Mt/yr)",           "HVC",        "HVC_meLiq_cc",
       "Production|Industry|Chemicals|HVC|+|From green hydogren-based methanol (Mt/yr)",            "HVC",        "HVC_meh2",

       "Production|Industry|Chemicals|Fertilizer|+|From solids-based ammonia (Mt/yr)",              "Fertilizer", "fertilizer_amSol",
       "Production|Industry|Chemicals|Fertilizer|+|From gas-based ammonia (Mt/yr)",                 "Fertilizer", "fertilizer_amNg",
       "Production|Industry|Chemicals|Fertilizer|+|From liquids-based ammonia (Mt/yr)",             "Fertilizer", "fertilizer_amLiq",
       "Production|Industry|Chemicals|Fertilizer|+|From solids-based ammonia with CC (Mt/yr)",      "Fertilizer", "fertilizer_amSol_cc",
       "Production|Industry|Chemicals|Fertilizer|+|From gas-based ammonia with CC (Mt/yr)",         "Fertilizer", "fertilizer_amNg_cc",
       "Production|Industry|Chemicals|Fertilizer|+|From liquids-based ammonia with CC (Mt/yr)",     "Fertilizer", "fertilizer_amLiq_cc",
       "Production|Industry|Chemicals|Fertilizer|+|From green hydrogen-based ammonia (Mt/yr)",      "Fertilizer", "fertilizer_amh2",    

       "Production|Industry|Chemicals|Methanol Final|+|Solids-based (Mt/yr)",                       "MethFinal",  "meFinal_sol",
       "Production|Industry|Chemicals|Methanol Final|+|Gas-based (Mt/yr)",                          "MethFinal",  "meFinal_ng",
       "Production|Industry|Chemicals|Methanol Final|+|Liquids-based (Mt/yr)",                      "MethFinal",  "meFinal_liq",
       "Production|Industry|Chemicals|Methanol Final|+|Solids and green hydrogen-based (Mt/yr)",    "MethFinal",  "meFinal_sol_gh2",
       "Production|Industry|Chemicals|Methanol Final|+|Solids-based with CC (Mt/yr)",               "MethFinal",  "meFinal_sol_cc",
       "Production|Industry|Chemicals|Methanol Final|+|Gas-based with CC (Mt/yr)",                  "MethFinal",  "meFinal_ng_cc",
       "Production|Industry|Chemicals|Methanol Final|+|Liquids-based with CC (Mt/yr)",              "MethFinal",  "meFinal_liq_cc",
       "Production|Industry|Chemicals|Methanol Final|+|Green hydrogen-based (Mt/yr)",               "MethFinal",  "meFinal_h2",

       "Production|Industry|Chemicals|Ammonia Final|+|Solids-based (Mt/yr)",                        "AmmoFinal",  "amFinal_sol",
       "Production|Industry|Chemicals|Ammonia Final|+|Gas-based (Mt/yr)",                           "AmmoFinal",  "amFinal_ng",
       "Production|Industry|Chemicals|Ammonia Final|+|Liquids-based (Mt/yr)",                       "AmmoFinal",  "amFinal_liq",
       "Production|Industry|Chemicals|Ammonia Final|+|Solids-based with CC (Mt/yr)",                "AmmoFinal",  "amFinal_sol_cc",
       "Production|Industry|Chemicals|Ammonia Final|+|Gas-based with CC (Mt/yr)",                   "AmmoFinal",  "amFinal_ng_cc",
       "Production|Industry|Chemicals|Ammonia Final|+|Liquids-based with CC (Mt/yr)",               "AmmoFinal",  "amFinal_liq_cc",
       "Production|Industry|Chemicals|Ammonia Final|+|Green hydrogen-based (Mt/yr)",                "AmmoFinal",  "amFinal_h2",

       "Production|Industry|Chemicals|Methanol|+|Solids-based (Mt/yr)",                             "Methanol",   c("HVC_meSol",    "meFinal_sol"),
       "Production|Industry|Chemicals|Methanol|+|Gas-based (Mt/yr)",                                "Methanol",   c("HVC_meNg",     "meFinal_ng"),
       "Production|Industry|Chemicals|Methanol|+|Liquids-based (Mt/yr)",                            "Methanol",   c("HVC_meLiq",    "meFinal_liq"),
       "Production|Industry|Chemicals|Methanol|+|Solids and green hydrogen-based (Mt/yr)",          "Methanol",   c("HVC_meSol_gh2","meFinal_sol_gh2"),
       "Production|Industry|Chemicals|Methanol|+|Solids-based with CC (Mt/yr)",                     "Methanol",   c("HVC_meSol_cc", "meFinal_sol_cc"),
       "Production|Industry|Chemicals|Methanol|+|Gas-based with CC (Mt/yr)",                        "Methanol",   c("HVC_meNg_cc",  "meFinal_ng_cc"),
       "Production|Industry|Chemicals|Methanol|+|Liquids-based with CC (Mt/yr)",                    "Methanol",   c("HVC_meLiq_cc", "meFinal_liq_cc"),
       "Production|Industry|Chemicals|Methanol|+|Green hydrogen-based (Mt/yr)",                     "Methanol",   c("HVC_meh2",     "meFinal_h2"),

       "Production|Industry|Chemicals|Ammonia|+|Solids-based (Mt/yr)",                              "Ammonia",    c("fertilizer_amSol",   "amFinal_sol"),
       "Production|Industry|Chemicals|Ammonia|+|Gas-based (Mt/yr)",                                 "Ammonia",    c("fertilizer_amNg",    "amFinal_ng"),
       "Production|Industry|Chemicals|Ammonia|+|Liquids-based (Mt/yr)",                             "Ammonia",    c("fertilizer_amLiq",   "amFinal_liq"),
       "Production|Industry|Chemicals|Ammonia|+|Solids-based with CC (Mt/yr)",                      "Ammonia",    c("fertilizer_amSol_cc","amFinal_sol_cc"),
       "Production|Industry|Chemicals|Ammonia|+|Gas-based with CC (Mt/yr)",                         "Ammonia",    c("fertilizer_amNg_cc", "amFinal_ng_cc"),
       "Production|Industry|Chemicals|Ammonia|+|Liquids-based with CC (Mt/yr)",                     "Ammonia",    c("fertilizer_amLiq_cc","amFinal_liq_cc"),
       "Production|Industry|Chemicals|Ammonia|+|Green hydrogen-based (Mt/yr)",                      "Ammonia",    c("fertilizer_amh2",    "amFinal_h2"))

     out <- mbind(c(list(out),
                  .select_sum_name_multiply(o37_ProdIndRoute, .mixer_to_selector(mixer), factor=1e3))) # factor 1e3 converts Gt/yr to Mt/yr

    }
  }


  #--- Transport reporting ---

  if (tran_mod == "edge_esm") {
    ## define the set that contains fe2es for transport
    fe2es_dyn35 <- readGDX(gdx,c("fe2es_dyn35"), format = "first_found")

    vm_demFeForEs_trnsp = vm_demFeForEs[fe2es_dyn35]

    out <- mbind(out,
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Electricity (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Electricity (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,c("esdie_pass_", "espet_pass_"),pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Gases (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Gases (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Hydrogen (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Hydrogen (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Electricity (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Electricity (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"espet_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Petrol Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_lo",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_lo",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Gases (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Gases (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Hydrogen (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Hydrogen (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight (EJ/yr)"),
                 setNames(dimSums(vm_demFeForEs_trnsp[,,"_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass (EJ/yr)"),
                 setNames(dimSums(vm_cesIO[,,"entrp_frgt_",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # remove EJ conversion factor, conv. trillion to billion tkm
                          "ES|Transport|Freight (billion tkm/yr)"),
                 setNames(dimSums(vm_cesIO[,,"entrp_pass_",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
                          "ES|Transport|Pass (billion pkm/yr)"),
                 setNames(dimSums(vm_cesIO[,,"entrp_frgt_sm",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion tkm
                          "ES|Transport|Freight|Short-Medium distance (billion tkm/yr)"),
                 setNames(dimSums(vm_cesIO[,,"entrp_pass_sm",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
                          "ES|Transport|Pass|Short-Medium distance (billion pkm/yr)"),
                 setNames(dimSums(vm_cesIO[,,"entrp_frgt_lo",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion tkm
                          "ES|Transport|Freight|Long distance (billion tkm/yr)"),
                 setNames(dimSums(vm_cesIO[,,"entrp_pass_lo",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
                          "ES|Transport|Pass|Long distance (billion pkm/yr)"))


    # calculate total diesel and petrol liquids across all modes, needed in reportPrices
    out <- mbind(out,
                 setNames(out[,,"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"]+
                            out[,,"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"] +
                            out[,,"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"] +
                            out[,,"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"],
                          "FE|Transport|Diesel Liquids (EJ/yr)"))

  }


  #--- CDR ---

  v33_FEdemand  <- readGDX(gdx, name=c("v33_FEdemand"), field="l", restore_zeros=F)[,t,] * TWa_2_EJ
  # KK: Mappings from gams set names to names in mifs. If new CDR methods are added to REMIND, please add
  # the method to CDR_te_list: "<method name in REMIND>"="<method name displayed in reporting>"
  # If a final energy carrier not included in CDR_FE_list is used, please also add it to the list.
  CDR_te_list <- list("dac"="DAC", "weathering"="EW", "oae_ng"="OAE, traditional calciner", "oae_el"="OAE, electric calciner")
  CDR_FE_list <- list("feels"="Electricity", "fegas"="Gases", "fehes"="Heat", "feh2s"="Hydrogen", "fedie"="Diesel")

  # loop to compute variables "FE|CDR|++|<CDR technology> (EJ/yr)" and "FE|CDR|<CDR technology>|+|<FE type> (EJ/yr)",
  # e.g., "FE|CDR|++|DAC (EJ/yr)" and "FE|CDR|DAC|+|Electricity (EJ/yr)"
  for (CDR_te in getItems(v33_FEdemand, dim="all_te")) {
    out <- mbind(out, setNames(dimSums(mselect(v33_FEdemand, all_te=CDR_te)),
                               sprintf("FE|CDR|++|%s (EJ/yr)", CDR_te_list[[CDR_te]])))
    # loop over all FE technologies used by a given CDR technology CDR_te
    for (CDR_FE in getItems(mselect(v33_FEdemand, all_te=CDR_te), dim="all_enty")) {
      variable_name <- sprintf("FE|CDR|%s|+|%s (EJ/yr)", CDR_te_list[[CDR_te]], CDR_FE_list[[CDR_FE]])
      out <- mbind(out, setNames(dimSums(mselect(v33_FEdemand, all_te=CDR_te, all_enty=CDR_FE)),
                                 variable_name))
    }
  }




  #--- Additional Variables

  out <- mbind(out,
               setNames(out[,,"FE (EJ/yr)"], "FE|Gross with CDR (EJ/yr)"),
               setNames(out[,,"FE (EJ/yr)"] - out[,,"FE|++|CDR (EJ/yr)"], "FE|Net without CDR (EJ/yr)")
  )

  # Add fuel computation
  out = mbind(out,
              setNames(out[,,"FE|++|Buildings (EJ/yr)"] - out[,,"FE|Buildings|+|Electricity (EJ/yr)"] - out[,,"FE|Buildings|+|Heat (EJ/yr)"], "FE|Buildings|Fuels (EJ/yr)"),
              setNames(out[,,"FE|++|Industry (EJ/yr)"]  - out[,,"FE|Industry|+|Electricity (EJ/yr)"]  - out[,,"FE|Industry|+|Heat (EJ/yr)"] , "FE|Industry|Fuels (EJ/yr)"),
              setNames(out[,,"FE|++|Transport (EJ/yr)"] - out[,,"FE|Transport|+|Electricity (EJ/yr)"]                                       , "FE|Transport|Fuels (EJ/yr)"),
              setNames(out[,,"FE (EJ/yr)"]             - out[,,"FE|+|Electricity (EJ/yr)"]           - out[,,"FE|+|Heat (EJ/yr)"]          , "FE|Fuels (EJ/yr)")
  )



  # split sectoral biomass in modern and traditional for exogains
  # allocate tradional biomass to buildings first and only consider industry if
  # all biomass in buildings is traditional. All fossil solids are coal.
  out <- mbind(out, setNames(asS4(pmin(out[, , "FE|Solids|Biomass|+|Traditional (EJ/yr)"],
                                       out[, , "FE|Buildings|Solids|+|Biomass (EJ/yr)"])),
                             "FE|Buildings|Solids|Biomass|+|Traditional (EJ/yr)"))
  out <- mbind(out, setNames(out[, , "FE|Solids|Biomass|+|Traditional (EJ/yr)"] -
                               out[, , "FE|Buildings|Solids|Biomass|+|Traditional (EJ/yr)"] ,
                             "FE|Industry|Solids|Biomass|+|Traditional (EJ/yr)" ))
  out <- mbind(out,
               setNames(out[, , "FE|Buildings|Solids|+|Biomass (EJ/yr)"] - out[, , "FE|Buildings|Solids|Biomass|+|Traditional (EJ/yr)"],
                        "FE|Buildings|Solids|Biomass|+|Modern (EJ/yr)"),
               setNames(out[, , "FE|Industry|Solids|+|Biomass (EJ/yr)"] - out[, , "FE|Industry|Solids|Biomass|+|Traditional (EJ/yr)"],
                        "FE|Industry|Solids|Biomass|+|Modern (EJ/yr)"),
               setNames(out[, , "FE|Buildings|Solids|+|Fossil (EJ/yr)"], "FE|Buildings|Solids|Coal (EJ/yr)"),
               setNames(out[, , "FE|Industry|Solids|+|Fossil (EJ/yr)"], "FE|Industry|Solids|Coal (EJ/yr)")
  )


  #### Non-energy Use Reporting ----

  # in case the current non-energy use implementation creates negative values, set them to 0
  if (any(out < 0, na.rm = TRUE)) {
    out[out < 0] <- 0
  }

  # report feedstocks use by carrier
  # FE non-energy use variables
  out <- mbind(out,
               setNames(dimSums(vm_demFENonEnergySector, dim=3),
                        "FE|Non-energy Use (EJ/yr)"),

               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst"), dim=3),
                        "FE|Non-energy Use|+|Industry (EJ/yr)"),

               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fesos"), dim=3),
                        "FE|Non-energy Use|Industry|+|Solids (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos"), dim=3),
                        "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas"), dim=3),
                        "FE|Non-energy Use|Industry|+|Gases (EJ/yr)")
  )


  # FE non-energy use per SE origin
  out <- mbind(out,
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fesos", all_enty = "sesofos"), dim=3),
                        "FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fesos", all_enty = "sesobio"), dim=3),
                        "FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos", all_enty = "seliqfos"), dim=3),
                        "FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos", all_enty = "seliqbio"), dim=3),
                        "FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fehos", all_enty = "seliqsyn"), dim=3),
                        "FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas", all_enty = "segafos"), dim=3),
                        "FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas", all_enty = "segabio"), dim=3),
                        "FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),
               setNames(dimSums(mselect(vm_demFENonEnergySector, emi_sectors="indst",all_enty1="fegas", all_enty = "segasyn"), dim=3),
                        "FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)")
  )

  ### FE without non-energy use
  out <- mbind(out,

               #total
               setNames(dimSums(vm_demFeSector_woNonEn,dim=3),
                        "FE|w/o Non-energy Use (EJ/yr)"),

               #Liquids
               setNames(dimSums(vm_demFeSector_woNonEn[,,c("fepet","fedie","fehos")],dim=3),                                                "FE|w/o Non-energy Use|Liquids (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"seliqbio"],dim=3),                                                       "FE|w/o Non-energy Use|Liquids|+|Biomass (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"seliqfos"],dim=3),                                                       "FE|w/o Non-energy Use|Liquids|+|Fossil (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"seliqsyn"] ,dim=3),                                                      "FE|w/o Non-energy Use|Liquids|+|Hydrogen (EJ/yr)"),

               # Gases
               setNames(dimSums(vm_demFeSector_woNonEn[,,c("fegas","fegat")],dim=3),                                       "FE|w/o Non-energy Use|Gases (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"segabio"],dim=3),                                                       "FE|w/o Non-energy Use|Gases|+|Biomass (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"segafos"],dim=3),                                                       "FE|w/o Non-energy Use|Gases|+|Fossil (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"segasyn"] ,dim=3),                                                      "FE|w/o Non-energy Use|Gases|+|Hydrogen (EJ/yr)"),

               # Solids
               setNames(dimSums(vm_demFeSector_woNonEn[,,"fesos"],dim=3),                                                         "FE|w/o Non-energy Use|Solids (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"sesobio"],dim=3),                                                       "FE|w/o Non-energy Use|Solids|+|Biomass (EJ/yr)"),
               setNames(dimSums(vm_demFeSector_woNonEn[,,"sesofos"],dim=3),                                                       "FE|w/o Non-energy Use|Solids|+|Fossil (EJ/yr)")
  )

  #FE per sector and per emission market (ETS and ESR)
  out <- mbind(out,

               #industry
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,emi_sectors="indst")  ,dim=3,na.rm=T)),                                      "FE|w/o Non-energy Use|Industry (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|w/o Non-energy Use|Industry|ESR (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ETS (EJ/yr)"),

               # industry liquids
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|w/o Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|w/o Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|w/o Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|w/o Non-energy Use|Industry|ESR|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Liquids|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ETS|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Liquids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Liquids|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Liquids|+|Hydrogen (EJ/yr)"),

               # industry solids
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|Solids (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ESR|Solids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Solids|+|Fossil (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|w/o Non-energy Use|Industry|ETS|Solids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Solids|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Solids|+|Fossil (EJ/yr)"),

               # industry gases
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segabio",emi_sectors="indst"),dim=3,na.rm=T)),  "FE|w/o Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segafos",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|w/o Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|w/o Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|w/o Non-energy Use|Industry|ESR|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ESR|Gases|+|Hydrogen (EJ/yr)"),

               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                  "FE|w/o Non-energy Use|Industry|ETS|Gases (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Gases|+|Biomass (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Gases|+|Fossil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector_woNonEn,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|w/o Non-energy Use|Industry|ETS|Gases|+|Hydrogen (EJ/yr)")


  )

  tryCatch(
    expr = {
      out <- mbind(
        out,
        setNames(
          out[, , "FE|Industry|Chemicals|+|Solids (EJ/yr)"] - out[, , "FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
          "FE|w/o Non-energy Use|Industry|Chemicals|Solids (EJ/yr)"
        ),
        setNames(
          out[, , "FE|Industry|Chemicals|+|Liquids (EJ/yr)"] - out[, , "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
          "FE|w/o Non-energy Use|Industry|Chemicals|Liquids (EJ/yr)"
        ),
        setNames(
          out[, , "FE|Industry|Chemicals|+|Gases (EJ/yr)"] - out[, , "FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
          "FE|w/o Non-energy Use|Industry|Chemicals|Gases (EJ/yr)"
        ),
        setNames(
          out[, , "FE|Industry|+++|Chemicals (EJ/yr)"] - out[, , "FE|Non-energy Use|+|Industry (EJ/yr)"],
          "FE|w/o Non-energy Use|Industry|Chemicals (EJ/yr)"
        )
      )
    },
    error = function(e) {
      warning(e)
    }
  )


  ### FE w/o non-energy and w/o bunkers ----

  out <- mbind(
    out,
    # Total
    setNames(
      out[, , "FE|w/o Non-energy Use (EJ/yr)"] - out[, , "FE|Transport|Bunkers (EJ/yr)"],
      "FE|w/o Non-energy Use w/o Bunkers (EJ/yr)"),

    # Liquids
    setNames(
      out[, , "FE|w/o Non-energy Use|Liquids (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|+|Liquids (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Liquids (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Liquids|+|Fossil (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|Liquids|+|Fossil (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Fossil (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Liquids|+|Biomass (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|Liquids|+|Biomass (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Biomass (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Liquids|+|Hydrogen (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|Liquids|+|Hydrogen (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Hydrogen (EJ/yr)"),

    # Gases
    setNames(
      out[, , "FE|w/o Non-energy Use|Gases (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|+|Gases (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Gases (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Gases|+|Fossil (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|Gases|+|Fossil (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Gases|Fossil (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Gases|+|Biomass (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|Gases|+|Biomass (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Gases|Biomass (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Gases|+|Hydrogen (EJ/yr)"]
      - out[, , "FE|Transport|Bunkers|Gases|+|Hydrogen (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Gases|Hydrogen (EJ/yr)"),

    # Solids (same as there are no solids in Bunkers)
    setNames(
      out[, , "FE|w/o Non-energy Use|Solids (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Solids (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Solids|+|Fossil (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Solids|Fossil (EJ/yr)"),
    setNames(
      out[, , "FE|w/o Non-energy Use|Solids|+|Biomass (EJ/yr)"],
      "FE|w/o Bunkers|w/o Non-energy Use|Solids|Biomass (EJ/yr)")
  )



  ### Regional Aggregation ----

  # add global values
  out <- mbind(out, dimSums(out, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))




  ### Further Variable Calculations ----

  # add per sector electricity share (for SDG targets)
  out <- mbind(out,
               setNames(out[,,'FE|Buildings|+|Electricity (EJ/yr)'] / out[,,'FE|++|Buildings (EJ/yr)'] * 100, 'FE|Buildings|Electricity|Share (%)'),
               setNames(out[,,'FE|Industry|+|Electricity (EJ/yr)']  / out[,,'FE|++|Industry (EJ/yr)']  * 100, 'FE|Industry|Electricity|Share (%)'),
               setNames(out[,,'FE|Transport|+|Electricity (EJ/yr)'] / out[,,'FE|++|Transport (EJ/yr)'] * 100, 'FE|Transport|Electricity|Share (%)'),
               setNames(out[,,'FE|+|Electricity (EJ/yr)'] / out[,,'FE (EJ/yr)'] * 100, 'FE|Electricity|Share (%)')
  )
  # add per sector fuel share
  out <- mbind(out,
               setNames(out[,,'FE|Buildings|Fuels (EJ/yr)'] / out[,,'FE|++|Buildings (EJ/yr)'] * 100, 'FE|Buildings|Fuels|Share (%)'),
               setNames(out[,,'FE|Industry|Fuels (EJ/yr)']  / out[,,'FE|++|Industry (EJ/yr)']  * 100, 'FE|Industry|Fuels|Share (%)'),
               setNames(out[,,'FE|Transport|Fuels (EJ/yr)'] / out[,,'FE|++|Transport (EJ/yr)'] * 100, 'FE|Transport|Fuels|Share (%)'),
               setNames(out[,,'FE|Fuels (EJ/yr)'] / out[,,'FE (EJ/yr)'] * 100, 'FE|Fuels|Share (%)')
  )

  ## specific energy use (FE per product/value added) ----
  if (all(indu_mod == 'subsectors',
          c('FE|Industry|+++|Cement (EJ/yr)',
            'Production|Industry|Cement (Mt/yr)',
            'FE|Industry|Steel|++|Primary (EJ/yr)',
            'Production|Industry|Steel|Primary (Mt/yr)',
            'FE|Industry|Steel|++|Secondary (EJ/yr)',
            'Production|Industry|Steel|Secondary (Mt/yr)',
            'FE|Industry|+++|Chemicals (EJ/yr)',
            'Value Added|Industry|Chemicals (billion US$2017/yr)',
            'FE|Industry|+++|Other Industry (EJ/yr)',
            'Value Added|Industry|Other Industry (billion US$2017/yr)') %>%
          `%in%`(getNames(out)))) {

    out <- mbind(
      out,
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|+++|Cement (EJ/yr)']
          / out[,,'Production|Industry|Cement (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Cement (GJ/t)'),

      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|Steel|++|Primary (EJ/yr)']
          / out[,,'Production|Industry|Steel|Primary (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Primary Steel (GJ/t)'),

      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|Steel|++|Secondary (EJ/yr)']
          / out[,,'Production|Industry|Steel|Secondary (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Secondary Steel (GJ/t)'),

      setNames(
        ( out[,,'FE|Industry|+++|Chemicals (EJ/yr)']
          / out[,,'Value Added|Industry|Chemicals (billion US$2017/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Chemicals (MJ/US$2017)'),

      setNames(
        ( out[,,'FE|Industry|+++|Other Industry (EJ/yr)']
          / out[,,'Value Added|Industry|Other Industry (billion US$2017/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Other Industry (MJ/US$2017)')
    )
  }

  getSets(out)[3] <- "variable"
  return(out)
}

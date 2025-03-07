#' take two REMIND scenario-config*.csv files and print the difference,
#' comparing it to a default.cfg
#' Can also be used for piamInterfaces mapping files with row.names=NULL and expanddata=FALSE.
#'
#' @param fileList vector containing one csv file paths or two paths as c(oldfile, newfile)
#' If one, searches same filename in defaultPath. If NULL, user can select
#' @param remindPath path to REMIND directory containing main.gms. Used for extracting the
#'        default configuration and find a file for comparison if only one config file is supplied in fileList
#' @param row.names column in csv used for row.names. Defaults to 1.
#'        Use NULL for mapping files which either picks the 'variable' column or uses the first two columns.
#' @param renamedCols vector with old and new column names such as c("old1" = "new1", "old2" = "new2")).
#'        Use TRUE as vector element to try to guess automatically: cm_switch <-> c_switch
#' @param renamedRows vector with old and new row names such as c("old3" = "new3", "old3" = "new4", "old5" = "new5"))
#'        the "old" name can also remain in the new file, if you generated a variant
#' @param printit boolean switch (default: TRUE) whether function prints its output
#' @param expanddata fill empty cells with default values. Use FALSE for mapping files
#' @author Oliver Richters
#' @examples
#'
#'  \dontrun{
#'     compareScenConf(fileList = c("scenario_config_old.csv", "scenario_config_new.csv"),
#'     renamedCols = c("old1" = "new1", "old2" = "new2"),
#'     renamedRows = c("old3" = "new3", "old4" = "new4"))
#'   }
#'
#' @return list with $allwarnings and $out
#' @importFrom utils read.csv2
#' @importFrom gms getLine
#' @export
compareScenConf <- function(fileList = NULL, remindPath = "/p/projects/rd3mod/github/repos/remindmodel/remind/develop",
                            row.names = 1, renamedCols = TRUE, renamedRows = NULL, printit = TRUE, expanddata = TRUE) {
  m <- c()
  folder <- getwd()
  # if one file supplied, compare to the same file in remindPath
  if (length(fileList) == 1 && fileList != "") {
    partafterconfig <- gsub(file.path("config", ""), "", file.path(basename(dirname(fileList)), basename(fileList)))
    fileList <- c(file.path(remindPath, "config", partafterconfig), fileList)
  }
  if (length(fileList) != 2) {
    cat("Specify folder with .csv files. The script also searches in subdirectories.\n")
    cat(paste0("Press enter to use current: ", getwd(), ".\n"))
    folder <- getLine()
    if (folder == "") folder <- getwd()
    fileList <- c(Sys.glob(file.path(paste0(folder, "/scenario_config*.csv"))),
                  Sys.glob(file.path(paste0(folder, "/*/scenario_config*.csv"))),
                  Sys.glob(file.path(paste0(folder, "/config/*/scenario_config*.csv")))
    )
    cat("\nPlease choose old and new file, separated with comma:\n")
    cat(paste(seq_along(fileList), fileList, sep = ": "), sep = "\n")
    identifier <- as.numeric(strsplit(getLine(), ",")[[1]])
    if (length(identifier) < 2) stop("You must choose two files!")
    fileList <- fileList[identifier]
  }
  if (! is.null(remindPath) && ! file.exists(file.path(remindPath, "main.gms"))) {
    if (dir.exists("..") && file.exists("../main.gms")) {
      remindPath <- normalizePath("..")
    } else {
      remindPath <- NULL
    }
  }
  m <- c(m, "", paste0("File comparison: ", fileList[[1]], " -> ", fileList[[2]]))
  cfg <- list() # to avoid 'no visible binding' error
  if (! is.null(remindPath)) {
    cfg <- gms::readDefaultConfig(remindPath)
    # enable script to match default data not in gms
    cfg$gms[["output"]] <- paste0(cfg$output, collapse = ",")
    for (switch in setdiff(names(cfg), c("output", "gms"))) {
      cfg$gms[[switch]] <- cfg[[switch]]
    }
  }

  if (expanddata) {
    message("Loading R helper functions from ", remindPath, ".") # overwrite readCheckScenarioConfig
    remindRscripts <- list.files(file.path(remindPath, "scripts", "start"), pattern = "\\.R$", full.names = TRUE)
    invisible(sapply(remindRscripts, source))
  } else {
    readCheckScenarioConfig <- function(csvFile, ...) {
      return(read.csv2(csvFile, stringsAsFactors = FALSE, row.names = row.names,
                            comment.char = "#", na.strings = "", dec = "."))
    }
  }
  settings1 <- readCheckScenarioConfig(fileList[[1]], remindPath = remindPath, fillWithDefault = TRUE, testmode = TRUE)
  settings2 <- readCheckScenarioConfig(fileList[[2]], remindPath = remindPath, fillWithDefault = TRUE, testmode = TRUE)

  # for mapping files use "variable" if exists, else combine first two columns
  if (is.null(row.names)) {
    if ("variable" %in% intersect(colnames(settings1), colnames(settings2))) {
      rownames(settings1) <- make.unique(settings1[, "variable"], sep = " ")
      rownames(settings2) <- make.unique(settings2[, "variable"], sep = " ")
    } else {
      rownames(settings1) <- make.unique(paste0(settings1[, 1], ": ", settings1[, 2]))
      rownames(settings2) <- make.unique(paste0(settings2[, 1], ": ", settings2[, 2]))
    }
  }

  # rename columns and rows in old file to new names after some checks
  if (TRUE %in% renamedCols) {
    renamedCols <- c(renamedCols[! renamedCols %in% TRUE], findRenamedCols(settings1, settings2))
  }
  allwarnings <- checkRowsCols(settings1, settings2, renamedCols, renamedRows)
  for (colname in intersect(names(settings1), names(renamedCols))) {
    names(settings1)[names(settings1) == colname] <- renamedCols[colname]
  }

  # print comparison
  scenarios <- unique(c(rownames(settings2), rownames(settings1)))
  switchnames <- sort(unique(c(names(settings1), names(settings2))))
  deletedCols <- setdiff(switchnames, names(settings2))
  addedCols <- setdiff(switchnames, names(settings1))
  m <- c(m, paste0("Columns deleted: ", ifelse(length(deletedCols) > 0, paste(deletedCols, collapse = ", "), "-")))
  m <- c(m, paste0("Columns added:   ", ifelse(length(addedCols) > 0, paste(addedCols, collapse = ", "), "-")))
  m <- c(m, paste0("Renamed columns: ", ifelse(length(renamedCols) > 0,
                   paste(names(renamedCols), renamedCols, sep = " -> ", collapse = ", "), "-")))
  m <- c(m, paste0("Renamed rows:    ", ifelse(length(renamedRows) > 0,
                   paste(names(renamedRows), renamedRows, sep = " -> ", collapse = ", "), "-")))

  for (c in addedCols) {
    settings1[, c] <- if (is.null(cfg$gms[[c]])) NA else cfg$gms[[c]]
  }
  jointCols <- intersect(names(settings1), names(settings2))
  m <- c(m, "", "Changes in the rows:")
  for (s in scenarios) {
    if (s %in% intersect(c(rownames(settings1), renamedRows), rownames(settings2))) {
      # scenario name, oldname -> newname if renamed
      sold <- if (s %in% renamedRows) names(renamedRows[s == renamedRows]) else s
      if (! all(identical(toString(settings1[sold, jointCols]), toString(settings2[s, jointCols])))) {
        m <- c(m, paste0("~ ", ifelse(s %in% renamedRows, paste(sold, "-> "), ""), s, ":"))
        for (c in jointCols) {
          # print only if different, if description was changed print only this fact
          if (! identical(toString(settings1[sold, c]), toString(settings2[s, c]))) {
            defaultinfo <- if (c %in% names(cfg$gms)) paste0(" (default: ", cfg$gms[[c]], ")")
            m <- c(m, paste0("    ", ifelse(c %in% renamedCols, paste(names(which(renamedCols == c)), "-> "), ""), c,
                   ": ", ifelse(c == "description", "was changed",
                                paste0(ifelse(c %in% addedCols, "NA", settings1[sold, c]), " -> ", settings2[s, c])),
                   defaultinfo))
          }
        }
      }
    } else if (! s %in% names(renamedRows)) {
      m <- c(m, ifelse(s %in% rownames(settings1), paste0("- ", s, " was deleted."), paste0("+ ", s, " was added.")))
    }
  }
  if (printit) {
    message(paste0(m, collapse = "\n"), "\n")
    if (length(allwarnings) > 0) warning(paste0(allwarnings, collapse = "\n"), "\n")
    return(list(allwarnings = allwarnings))
  } else {
    return(list(allwarnings = allwarnings, out = m))
  }
}

checkRowsCols <- function(settings1, settings2, renamedCols, renamedRows) {
  allwarnings <- c()
  if (any(! names(renamedCols) %in% names(settings1))) {
    allwarnings <- c(allwarnings, "oldColNotIn1" = paste(
      "Old column name(s) not present in first file:", setdiff(names(renamedCols), names(settings1))))
  }
  if (any(names(renamedCols) %in% names(settings2))) {
    allwarnings <- c(allwarnings, "oldColAlsoIn2" = paste(
      "Old column name(s) also present in second file:", setdiff(names(renamedCols), names(settings2))))
  }
  if (any(! renamedCols %in% names(settings2))) {
    allwarnings <- c(allwarnings, "newColNotIn2" = paste(
      "New column name(s) not present in second file:", setdiff(renamedCols, names(settings2))))
  }
  if (any(renamedCols %in% names(settings1))) {
    allwarnings <- c(allwarnings, "newColAlsoIn1" = paste(
      "New column name(s) also present in first file: ", setdiff(renamedCols, names(settings1))))
  }
  if (any(! names(renamedRows) %in% rownames(settings1))) {
    allwarnings <- c(allwarnings, "newRowNotIn1" = paste(
      "Old row name(s) not present in first file:", setdiff(names(renamedRows), rownames(settings1))))
  }
  if (any(! renamedRows %in% rownames(settings2))) {
    allwarnings <- c(allwarnings, "newRowNotIn2" = paste(
      "New row name(s) not present in second file: ", setdiff(renamedRows, rownames(settings2))))
  }
  if (any(renamedRows %in% rownames(settings1))) {
    allwarnings <- c(allwarnings, "newRowAlsoIn1" = paste(
      "New row name(s) also present in first file: ", setdiff(renamedRows, rownames(settings1))))
  }
  return(allwarnings)
}

findRenamedCols <- function(settings1, settings2) {
  only1 <- setdiff(names(settings1), names(settings2))
  only2 <- setdiff(names(settings2), names(settings1))
  shorten <- function(i) gsub("^cm_", "c_", i)
  shortname <- intersect(shorten(only1), shorten(only2))
  renamedCols <- NULL
  for (s in shortname) {
    renamedCols <- c(renamedCols, setNames(only2[shorten(only2) == s], only1[shorten(only1) == s]))
  }
  return(renamedCols)
}

# The REMIND R package (2nd generation)

R package **remind2**, version **1.169.2**

[![CRAN status](https://www.r-pkg.org/badges/version/remind2)](https://cran.r-project.org/package=remind2) [![R build status](https://github.com/pik-piam/remind2/workflows/check/badge.svg)](https://github.com/pik-piam/remind2/actions) [![codecov](https://codecov.io/gh/pik-piam/remind2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/remind2) [![r-universe](https://pik-piam.r-universe.dev/badges/remind2)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Contains the REMIND-specific routines for data and model
    output manipulation.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remind2")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("compareScenariosRemind2") # compareScenarios in remind2
vignette("remind_summary")          # Adding plots to the REMIND_summary.pdf
vignette("remind2_reporting")       # remind2 reporting
```

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **remind2** in publications use:

Rodrigues R, Baumstark L, Benke F, Dietrich J, Dirnaichner A, Dorndorf T, Duerrwaechter J, Führlich P, Giannousakis A, Hasse R, Hilaire J, Klein D, Koch J, Kowalczyk K, Levesque A, Malik A, Merfort A, Merfort L, Morena-Leiva S, Pehl M, Pietzcker R, Rauner S, Richters O, Rottoli M, Schötz C, Schreyer F, Siala K, Sörgel B, Spahr M, Strefler J, Verpoort P, Weigmann P, Rüter T (2025). "remind2: The REMIND R package (2nd generation)." Version: 1.169.2, <https://github.com/pik-piam/remind2>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {remind2: The REMIND R package (2nd generation)},
  author = {Renato Rodrigues and Lavinia Baumstark and Falk Benke and Jan Philipp Dietrich and Alois Dirnaichner and Tabea Dorndorf and Jakob Duerrwaechter and Pascal Führlich and Anastasis Giannousakis and Robin Hasse and Jérome Hilaire and David Klein and Johannes Koch and Katarzyna Kowalczyk and Antoine Levesque and Aman Malik and Anne Merfort and Leon Merfort and Simón Morena-Leiva and Michaja Pehl and Robert Pietzcker and Sebastian Rauner and Oliver Richters and Marianna Rottoli and Christof Schötz and Felix Schreyer and Kais Siala and Björn Sörgel and Mike Spahr and Jessica Strefler and Philipp Verpoort and Pascal Weigmann and Tonn Rüter},
  date = {2025-03-07},
  year = {2025},
  url = {https://github.com/pik-piam/remind2},
  note = {Version: 1.169.2},
}
```

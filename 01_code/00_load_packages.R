# Load packages -----------------------------------------------------------

packages <- c("dplyr",
              "ggplot2",
              "knitr",
              "stargazer",
              "rvest",
              "tsModel",
              "lmtest",
              "Epi",
              "splines",
              "vcd",
              "rdd",
              "magrittr",
              "ggthemes",
              "ggrepel",
              "memisc",
              "pander"
              )

package_installed <-
  sapply(packages, function(pack)
    pack %in% rownames(installed.packages()))

if (any(!package_installed)) {
  sapply(packages[!package_installed], install.packages)
}

sapply(packages, require, character.only = TRUE)

rm(packages,package_installed)
# on gthub since 19.08.2021
usual.libs <- function()
{
  library(SurvRegCensCov)
  library(flexsurv)
  library(PerformanceAnalytics)
  library(rmarkdown)
  library(ggthemes)
  library(gridExtra)
  library(report)
  library(pwr)
  library(ggrepel)
  library(cowplot)
  library(magick)
  library(png)
  library(incidence)
  library(lubridate)
  library(expss)
  library(caret)
  library(Hmisc)
  library(rms)
  library(readxl)
  library(haven)
  library(summarytools)
  library(psych)
  library(dplyr)
  library(skimr)
  library(gmodels)
  library(epiDisplay)
  library(Hmisc)
  library(ggplot2)
  library(survival)
  library(survminer)
  library(tidyverse)
  library(pROC)
  library(readxl)
  library(haven)
  library(summarytools)
  library(psych)
  library(dplyr)
  library(skimr)
  library(gmodels)
  library(epiDisplay)
  library(Hmisc)
  library(ggplot2)
  library(survival)
  library(survminer)
  library(tidyverse)
  library(pROC)
  library(kableExtra)
  library(car)
  library(boot)
  library(sjPlot)
  library(snakecase)
  library(jtools)
  library(shadowtext)
  library(ggplot2)
  library(caTools)
  library(performance)
  library(see)
  library(mice)
  library(VIM)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  
  # my options
  # tibble options, show more rows
  options(tibble.print_max = 100, tibble.print_min = 100)
  
}

# function to convert 1 <- "yes" and 0 <- "no" in factor fields
conv.f <- function(x) {
  return (factor(x, levels = c(1, 0), labels = c("yes", "no")))
}

conv.f.same <- function(x) {
  return (factor(x, levels= c("yes", "no")))
}

usual.libs.install <- function()
{
  options("Ncpus" = 4)
  load.lib <- c(
    "remotes",
    "pwr",
    "ggrepel",
    "cowplot",
    "magick",
    "png",
    "incidence",
    "lubridate",
    "expss",
    "caret",
    "Hmisc",
    "rms",
    "readxl",
    "haven",
    "summarytools",
    "psych",
    "dplyr",
    "skimr",
    "gmodels",
    "epiDisplay",
    "Hmisc",
    "ggplot2",
    "survival",
    "survminer",
    "tidyverse",
    "pROC",
    "readxl",
    "haven",
    "psych",
    "dplyr",
    "skimr",
    "gmodels",
    "epiDisplay",
    "Hmisc",
    "ggplot2",
    "survival",
    "survminer",
    "tidyverse",
    "pROC",
    "kableExtra",
    "car",
    "boot",
    "sjPlot",
    "snakecase",
    "jtools",
    "shadowtext",
    "ggplot2",
    "caTools",
    "performance",
    "see",
    "mice",
    "VIM",
    "sjPlot",
    "sjmisc",
    "sjlabelled"
  )
  
  
  install.lib <- load.lib[!load.lib %in% installed.packages()]
  for (lib in install.lib)
    install.packages(lib, dependencies = TRUE)
  sapply(load.lib, require, character = TRUE)
  
  
  # my options
  # tibble options, show more rows
  options(tibble.print_max = 100, tibble.print_min = 100)
  
}

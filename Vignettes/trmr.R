## ---- echo = FALSE, include = FALSE--------------------------------------
library(trmr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(chron)
library(tidyverse)
library(stringr)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))


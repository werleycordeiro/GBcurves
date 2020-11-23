# GBcurves


```R
install.packages("devtools") # Step 1
library(devtools) # Step 2
devtools::install_git("https://github.com/werleycordeiro/GBcurves") # Step 3
library(GBcurves) # Step 4

# Example: 

init <- "2020-05-10"
fin <- "2020-05-17"
mty <- c(3,6,12,120,360)
ctry <- "BR"

yields(init = init, fin = fin, mty = mty, ctry = ctry)

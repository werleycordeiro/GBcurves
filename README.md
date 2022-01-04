# The GBcurves Package

The GBcurves for R downloads the Brazilian, Chinese, and Russian yield curves from <https://www.b3.com.br/>, <https://www.chinabond.com.cn/>, and <https://www.cbr.ru/>, respectively.

The canonical link to the package on CRAN which includes examples and
documentation is [https://cran.r-project.org/web/packages/GBcurves/index.html](https://cran.r-project.org/web/packages/GBcurves/index.html).

Development can be tracked via the [github repository](https://github.com/werleycordeiro/GBcurves) 
which contains source code for the package starting with version 0.1.6.

```
install.packages("GBcurves")

# Example: 

init <- "2020-05-10"
fin <- "2020-05-17"
mty <- c(3,6,12,120,360)
ctry <- "BR"

yields(init = init, fin = fin, mty = mty, ctry = ctry)

```

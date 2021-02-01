#' @title Yield Curves Dataset
#'
#' @description This function downloads daily yield curves data of Brazil, China, and Russia. If necessary, it interpolates with spline function for unavailable maturities.
#'
#' @usage yields(init, fin, mty, ctry)
#'
#' @param init Initial date in format "YYYY-MM-DD"
#' @param fin Final date in format "YYYY-MM-DD"
#' @param mty Maturities specified by months
#' @param ctry Countries available: "BR", "CN" or "RU"
#'
#' @return A matrix that contains daily yield curves in percent in each row and maturities in months by columns.
#'
#' @source The dataset of Brazil, China, and Russia are obtained from <http://www.b3.com.br/>, <http://yield.chinabond.com.cn>, and <https://www.cbr.ru>, respectively.
#'
#' @examples
#'
#'init <- "2020-05-10"
#'fin <- "2020-05-17"
#'mty <- c(3,6,12,120,360)
#'ctry <- "BR"
#'
#'\dontrun{
#'yields(init = init, fin = fin, mty = mty, ctry = ctry)
#'}
#' @importFrom magrittr %>%
#' @export

yields = function (init,fin,mty,ctry) {
  # check for internet
  test.internet <- curl::has_internet()
  if (!test.internet) {
    stop('No internet connection found...')
  }

  if ( is.na(as.Date(as.character(init), format = '%Y-%m-%d') ) | is.na(as.Date(as.character(fin), format = '%Y-%m-%d'))) {
    stop(paste('Inputs \"init\" and \"fin\" must be a string with the standard date format (YYYY-MM-DD)'))
  }

  if ( fin < init ) {
    stop(paste('Found that \"fin\" > \"init\". Did you mix the two up?',
               'Please check it.'))
  }

  if ( is.character(mty) ) {
    stop(paste('Input \"mty\" must be a vector with integers'))
  }

  tmp <- c("BR","RU","CN")[c("BR","RU","CN") == ctry]
  if ( identical(tmp, character(0)) ) {
    stop(paste('Input \"ctry\" not valid. It should be \"BR\", \"CN\" or \"RU\"'))
  }

  if ( fin > Sys.Date() ) {
    stop(paste0('\"fin\" must be <= ',Sys.Date(),'.',
               ' Please check it.'))
  }


  if ( ctry == "BR" ) {
    # BR
    if ( init < "2003-08-08" ) {
      stop(paste('\"init\" must be >= "2003-08-08" for Brazil yield curves.',
                 'Please check it.'))
    }

    dates <- format(seq(as.Date(init), as.Date(fin), 'day'), format="%d-%m-%Y", tz="UTC")
    if ( !identical(mty[mty < 1 | mty > 600], numeric(0)) ) {
      stop(paste0('argument \"mty\" must be between 1 and 600 months for ',ctry, ' yield curves'))
    }
    mat <- matrix(NA,length(dates),length(mty))
    # Scraping
    i <- 1
    while ( i <= length(dates) ) {
      tmp <- httr::GET(url = "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp",
                query = list(Data = dates[i]))
      data <- xml2::read_html(tmp) %>% rvest::html_nodes("table") %>% rvest::html_nodes("td") %>% rvest::html_text()
      if ( !length(data) == 0 ) {
        data <- data.frame(matrix(data, ncol=3, byrow=TRUE))
        data[,2] <- as.numeric(gsub(",", ".", gsub("\\.", "", data[,2])))
        data[,3] <- as.numeric(gsub(",", ".", gsub("\\.", "", data[,3])))
        # Spline
        t <- as.integer(as.matrix(data[,1])) / 21
        y <- as.numeric(as.matrix(data[,2]))
        spl <- stats::smooth.spline(y ~ t, all.knots = FALSE)
        t.new <- mty
        new <- stats::predict(spl, t.new)
        mat[i,] <- new$y
      }
      i <- i + 1
    }
    colnames(mat) <- paste0("M",mty)
    rownames(mat) <- dates
    mat <- stats::na.omit(mat)
    attributes(mat)$na.action <- NULL
    return(mat)
  }

  # CN

  if ( ctry == "CN" ) {

    if ( init < "2006-03-01" ) {
      stop(paste('\"init\" must be >= "2006-03-01" for China yield curves.',
                 'Please check it.'))
    }


    dates <- format(seq(as.Date(init), as.Date(fin), 'day'), format="%Y-%m-%d", tz="UTC")
    if ( !identical(mty[mty < 0 | mty > 600], numeric(0)) ) {
        stop(paste0('argument \"mty\" must be between 0 and 600 months for ',ctry, ' yield curves'))
      }
    mat <- matrix(NA,length(dates),length(mty))
    # Scraping
    i <- 1
    while ( i <= length(dates) ) {
      tmp <- tempfile(fileext = ".xlsx")
      url <- paste0("http://yield.chinabond.com.cn/cbweb-mn/yc/downBzqxDetail?ycDefIds=2c9081e50a2f9606010a3068cae70001&&zblx=txy&&workTime=",
                   dates[i],
                   "&&dxbj=0&&qxlx=0,&&yqqxN=N&&yqqxK=K&&wrjxCBFlag=0&&locale=en_US")
      utils::download.file(url = url, destfile = tmp, mode="wb", quiet = TRUE)
      if ( !file.info(tmp)$size == 0 ) {
        mat0 <- readxl::read_excel(tmp)
        # Spline
        tmp1.new <- as.numeric(as.matrix(mat0[,2])) * 12
        # tmp1.new[1] = 1 / 252
        y <- as.numeric(as.matrix(mat0[,3]))
        # spl <- smooth.spline(y ~ tmp1.new,all.knots=TRUE,control.spar = list(tol=1e-4, eps= 0.01))
        spl <- stats::smooth.spline(y ~ tmp1.new, cv = TRUE)
        tmp1 <- mty
        while ( utils::tail(tmp1, n = 1) > utils::tail(tmp1.new, n = 1) ) {# remove latest maturities not observed
          tmp1 <- tmp1[-length(tmp1)]
        }
        new <- stats::predict(spl, tmp1)
        mat[i, ] <- new$y
        if ( length(tmp1) != length(mty) ) mat[i, (length(tmp1)+1):length(mty)] = -Inf
      } else {
        dates[i] <- NA
      }
      i <- i + 1
    }
    colnames(mat) <- paste0("M",mty)
    rownames(mat) <- format(seq(as.Date(init), as.Date(fin), 'day'), format = "%d-%m-%Y", tz = "UTC")
    mat = stats::na.omit(mat)
    attributes(mat)$na.action <- NULL
    mat[mat == -Inf] <- NA
    return(mat)
  }

  # RU

  if ( ctry == "RU" ) {

    if ( init < "2003-01-04" ) {
      stop(paste('\"init\" must be >= "2003-01-04" for Russian yield curves.',
                 'Please check it.'))
    }

    dates <- format(seq(as.Date(init), as.Date(fin), 'day'), format = "%d/%m/%Y", tz = "UTC")
    mty.def <- c(3,6,9,12,24,36,60,84,120,180,240,360)
    if ( !identical(mty[mty < 3 | mty > 360], numeric(0)) ) {
      stop(paste0('argument \"mty\" must be between 3 and 360 months for ',ctry, ' yield curves'))
    }
    mat <- matrix(NA,length(dates),(length(mty) + 1))
    # Scraping
    i <- 1
    while ( i <= length(dates) ) {
      tmp <- httr::GET("https://www.cbr.ru/eng/hd_base/zcyc_params/", query = list(DateTo = dates[i]))
      data <- xml2::read_html(tmp) %>% rvest::html_nodes("table") %>% rvest::html_nodes("td") %>% rvest::html_text()
      if ( data[1] == dates[i] ) {
        if ( NA %in% match(mty,mty.def) ) {
          # Spline
          spl <- stats::smooth.spline( as.numeric(data[2:13]) ~ mty.def, all.knots = FALSE )
          new <- stats::predict(spl, mty)
          mat[i, 2:(length(mty) + 1) ] <- new$y
          mat[i, 1] <- data[1]
        } else {
          mat[i,] <- c(data[1], data[2:13][match(mty,mty.def)])
        }
      }
      i <- i + 1
    }
    mat <- matrix(as.numeric(mat[,2:ncol(mat)]),nrow(mat),(ncol(mat)-1))
    rownames(mat) <- format(seq(as.Date(init), as.Date(fin), 'day'), format="%d-%m-%Y", tz="UTC")
    colnames(mat) <- paste0("M",mty)
    mat <- stats::na.omit(mat)
    attributes(mat)$na.action <- NULL
    return(mat)
  }
}

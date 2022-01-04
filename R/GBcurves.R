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
      #pb = utils::txtProgressBar(min = (1/length(dates)), max = length(dates), style = 3)
      #utils::setTxtProgressBar(pb,i)
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
    message("Interpolation not available")

    if ( init < "2006-03-01" ) {
      stop(paste('\"init\" must be >= "2006-03-01" for China yield curves.',
                 'Please check it.'))
    }

    dates <- format(seq(as.Date(init), as.Date(fin), 'day'), format="%Y-%m-%d", tz="UTC")
    if ( !identical(mty[mty < 0 | mty > 600], numeric(0)) ) {
        stop(paste0('argument \"mty\" must be between 0 and 600 months for ',ctry, ' yield curves'))
      }
    dataset.raw <- list()
    store <- matrix(NA,length(dates),2)


    if(Sys.info()['sysname'] == 'Linux' ||Sys.info()['sysname'] == 'SunOS' ||Sys.info()['sysname'] == 'Darwin'){
      options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;
      en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
    }

    # function check_url

    check_url <- function(url, tmp){
      if( R.Version()$nickname=="Unsuffered Consequences" ){
        try(

            utils::download.file(url = url, destfile = tmp, mode="wb", quiet = TRUE, method = "wininet")

      )

      }else{

        out=tryCatch(
          expr = {

            utils::download.file(url = url, destfile = tmp, mode="wb", quiet = TRUE)

          },
          error = function(e){
            message(paste("It was not possible to download the date ",dates[i],". Try again, or the date ",dates[i]," is not available."))
            return('problem')
          },
          warning = function(w){
            return('problem')
            message('')
          }#,
        )

        return(out)

      }

    }

    # Scraping
    i <- 1
    while ( i <= length(dates) ) {
      tmp <- tempfile(fileext = ".xlsx")
      url <- paste0("http://yield.chinabond.com.cn/cbweb-mn/yc/downBzqxDetail?ycDefIds=2c9081e50a2f9606010a3068cae70001&&zblx=txy&&workTime=",
                    dates[i],
                    "&&dxbj=0&&qxlx=0,&&yqqxN=N&&yqqxK=K&&wrjxCBFlag=0&&locale=en_US")

      test <- check_url(url = url, tmp = tmp) # return 'problem' or 0. If 0, tmp size can be 0 or != 0

      if(test == 0){ # If = 0, great, it downloaded the date, but we don't know the file size ...

          if ( file.info(tmp)$size != 0 ) { # check file size

            mat0 <- readxl::read_excel(tmp)
            tmp1.new <- as.numeric(as.matrix(mat0[,2])) # Maturities from source
            store[i,] <- c(i,length(tmp1.new))
            y <- as.numeric(as.matrix(mat0[,3])) # Yields from source
            dataset.raw[[i]] <- data.frame(tmp1.new,y)

          }else{

            dataset.raw[[i]] <- NA

          }

      }else{

        if ( !is.na( file.info(tmp)$size ) ) { # check file size

          mat0 <- readxl::read_excel(tmp)
          tmp1.new <- as.numeric(as.matrix(mat0[,2])) # Maturities from source
          store[i,] <- c(i,length(tmp1.new))
          y <- as.numeric(as.matrix(mat0[,3])) # Yields from source
          dataset.raw[[i]] <- data.frame(tmp1.new,y)

        }else{

          dataset.raw[[i]] <- NA

        }

      }

      i <- i + 1
      #pb = txtProgressBar(min = (1/length(dates)), max = length(dates), style = 3)
      #setTxtProgressBar(pb,i)

    }

    store <- stats::na.omit(store)
    max.mty <- store[store[,2]==max(store[,2]),]

    if( !is.character(max.mty[1]) && !is.null(max.mty[1]) && !is.na(max.mty[1])){

      max.mty <- as.numeric(max.mty[1])
      mat.final <- matrix(NA, length(dates), nrow(dataset.raw[[max.mty]]) )

      for(j in 1:length(dates)){

        mat.tmp <- if( all(is.na(dataset.raw[[j]])) ){

          rep(NA,ncol(mat.final) )

        }else{

          dataset.raw[[j]][ match(dataset.raw[[max.mty]][,1],dataset.raw[[j]][,1]), 2 ]

        }

        mat.final[j,] <- mat.tmp

      }

      colnames(mat.final) <- paste0("M",dataset.raw[[max.mty]][,1]*12)
      rownames(mat.final) <- dates
      ind <- apply(mat.final, 1, function(x) all(is.na(x)))
      mat.final <- mat.final[ !ind, ]
      return(mat.final)

    }else{
      message("Dataset not currently available. Try again later")
      max.mty <- 1
      mat.final <- matrix(NA, length(dates), max.mty )
      #mat.final <- matrix(NA, length(dates), nrow(dataset.raw[[max.mty]]) )
      rownames(mat.final) <- dates
      return(mat.final)
    }

  }

  # RU

  if ( ctry == "RU" ) {
    message("Interpolation not available")

    if ( init < "2003-01-04" ) {
      stop(paste('\"init\" must be >= "2003-01-04" for Russian yield curves.',
                 'Please check it.'))
    }

    dates <- format(seq(as.Date(init), as.Date(fin), 'day'), format = "%d/%m/%Y", tz = "UTC")
    mty.def <- c(3,6,9,12,24,36,60,84,120,180,240,360)
    if ( !identical(mty[mty < 3 | mty > 360], numeric(0)) ) {
      stop(paste0('argument \"mty\" must be between 3 and 360 months for ',ctry, ' yield curves'))
    }
    mat <- matrix(NA,length(dates),(length(mty.def) + 1))

    # Scraping

    i <- 1
    while ( i <= length(dates) ) {
      tmp <- httr::GET("https://www.cbr.ru/eng/hd_base/zcyc_params/", query = list(DateTo = dates[i]))
      data <- xml2::read_html(tmp) %>% rvest::html_nodes("table") %>% rvest::html_nodes("td") %>% rvest::html_text()
      mat[i,] <- data[1:13]
      #pb = utils::txtProgressBar(min = (1/length(dates)), max = length(dates), style = 3)
      #utils::setTxtProgressBar(pb,i)
      i <- i + 1
    }
    mat <- matrix(as.numeric(mat[,2:ncol(mat)]),nrow(mat),(ncol(mat)-1))
    rownames(mat) <- dates
    colnames(mat) <- paste0("M",mty.def)
    mat <- stats::na.omit(mat)
    attributes(mat)$na.action <- NULL
    mat <- mat[!duplicated(mat), ]
    return(mat)
  }
}

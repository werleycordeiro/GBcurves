check_url <- function(url, tmp){
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

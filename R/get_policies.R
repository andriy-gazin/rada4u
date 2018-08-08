#' get list of all avaliable policies
#'
#' @param key A character. Your API key from rada4you.org
#'
#' @return Dataframe with all available policies
#'
#' @export

get_policies <- function(key) {

  if(is.null(key) | nchar(key) == 0) {

    message('You should provide your API key')

    stop()

  } else {

    request = httr::GET(url = "https://rada4you.org/api/v1/policies.json",
                  query = list(key = key))

    if(httr::status_code(request) != 200) {

    message(paste('Server returned a'), httr::status_code(request))

    } else {

      response <-  httr::content(request, as = 'text')

      response <- jsonlite::fromJSON(response)

    }

  }

  message(paste('Returning', nrow(response), 'policies'))

  return(response)

}

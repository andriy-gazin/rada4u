#' Get short description of the policy
#'
#' @param id Vector of policy ids (should consist at least of one value). Default value is '1', which returns test policy
#'
#' @param key A character. Your API key from rada4you.org
#'
#' @return Dataframe with 4 variables for each policy: id, name, description, provisional
#'
#' @export

get_policy_details <- function(id = 1, key) {

  if(is.null(key) | nchar(key) == 0) {

    message('You should provide your API key')

    stop()

  } else {

    details = data.frame()

    for(i in id) {

      request = httr::GET(url = "https://rada4you.org/",
                          path = paste0("api/v1/policies/", i, ".json"),
                          query = list(key = key))

      if(httr::status_code(request) != 200) {

        message(paste('Server returned a'), httr::status_code(request))

      } else {

        response <-  httr::content(request, as = 'text')

        response <- jsonlite::fromJSON(response)

        detail <-  data.frame(id = response$id,
                            name = response$name,
                            description = response$description,
                            provisional = response$provisional)

        details = rbind.data.frame(details, detail)

        Sys.sleep(1)

      }

    }

  }

  return(details)

}

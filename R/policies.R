#' Отримайте перелік всіх доступних політик
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм, який містить всі доступні політики
#'
#' @export

policies <- function(key) {

  if(is.null(key) | nchar(key) == 0) {

    stop('Надайте валідний ключ API')

  } else {

    request <- httr::GET(url = "https://rada4you.org/api/v1/policies.json",
                  query = list(key = key))

    if(httr::status_code(request) != 200) {

    message(paste('Помилка. Код відповіді сервера:'), httr::status_code(request))

    } else {

      response <-  httr::content(request, as = 'text')

      response <- jsonlite::fromJSON(response)

    }

  }

  message(paste('Знайдено', nrow(response), 'політик'))

  return(response)

}

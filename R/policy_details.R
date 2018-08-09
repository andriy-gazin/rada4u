#' Отримайте короткий опис політики
#'
#' @param id Вектор, який містить id політик (має складатись щонайменше з одного елементу). За замовчуванням має значення '1', що відповідає тестовій політиці
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм, що складається з чотирьох змінних для кожної політики: id, name, description, provisional
#'
#' @export

policy_details <- function(id = 1, key) {

  if(is.null(key) | nchar(key) == 0) {

    stop('Надайте валідний ключ API')

  } else if(is.null(id) | nchar(id) == 0){

    stop('Надайте id політики або вектор id політик')

  } else {

    details = data.frame()

    for(i in id) {

      request = httr::GET(url = "https://rada4you.org/",
                          path = paste0("api/v1/policies/", i, ".json"),
                          query = list(key = key))

      if(httr::status_code(request) != 200) {

        message(paste('Помилка. Код відповіді сервера:'), httr::status_code(request))

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

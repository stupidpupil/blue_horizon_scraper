complete_exchange_format <- function(products_in_exchange_format){
  list(
    provider = list(
      name = jsonlite::unbox("Blue Horizon"),
      url = jsonlite::unbox("https://bluehorizonbloodtests.co.uk")
    ),
    products = products_in_exchange_format  ,
    last_updated = lubridate::now() |> lubridate::format_ISO8601(usetz=TRUE) |> jsonlite::unbox()
  )
}

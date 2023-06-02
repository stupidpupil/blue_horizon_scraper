get_blue_horizon_products <- function(){
	products <- jsonlite::read_json("https://bluehorizonbloodtests.co.uk/products.json?limit=250")$products

  products <- products |>
    purrr::map(function(p){list_from_blue_horizon_product_json(p)})

  return(products)
}
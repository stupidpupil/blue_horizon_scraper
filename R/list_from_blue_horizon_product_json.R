list_from_blue_horizon_product_json <- function(blue_horizon_product_json){

  id <- as.character(blue_horizon_product_json$id)
  title <- blue_horizon_product_json$title
  handle <- blue_horizon_product_json$handle


  price <- blue_horizon_product_json$variants[[1]]$price
  available <- blue_horizon_product_json$variants[[1]]$available

  stopifnot(price |> stringr::str_detect("^\\d+\\.\\d{2}$"))

  price_pence <- price |> stringr::str_remove("\\.") |> as.integer()

  tags <- blue_horizon_product_json$tags


  venous_available <- dplyr::case_when(
    ("vac" %in% tags) ~ TRUE,
    ("2- Vacutainer Blood Sample" %in% tags) ~ TRUE,
    TRUE ~ FALSE
  )

  venous_only <- dplyr::case_when(
    ("1- Finger-Prick Blood Sample" %in% tags) ~ FALSE,
    venous_available ~ TRUE,
    TRUE ~ NA
  )

  if(!venous_available & is.na(venous_only)){
  	return(list(
    id = id,
    title = title,
    handle = handle,
    biomarkers = c(),
    price_pence = price_pence,
    venous_only = venous_only,
    venous_available = venous_available,
    available = available
  	))
  }


	possible_biomarkers <- readr::read_csv("data-raw/biomarkers.csv", col_types="cc")

	product_page <- rvest::read_html(paste0("https://bluehorizonbloodtests.co.uk/products/", handle))

	inclusions <- product_page |> 
		rvest::html_node(".product-block:contains('Inclusions')") |> 
		rvest::html_text() |> stringr::str_trim()


	if(is.na(inclusions)){
	inclusions <- product_page |> 
		rvest::html_node(".product-block:contains('Included Biomarkers')") |> 
		rvest::html_text() |> stringr::str_trim()

	}

	if(is.na(inclusions)){
		inclusions <- ""
	}

	biomarkers <- character(0)

	for (bn in possible_biomarkers$biomarker_handle) {
		if(inclusions |> stringr::str_detect(stringr::regex(paste0("\\b", bn ,"\\b"), ignore_case=TRUE))){
			biomarkers <- c(biomarkers, bn)
		}
	}

	biomarkers <- biomarkers |> unique()


  list(
    id = id,
    title = title,
    handle = handle,
    biomarkers = biomarkers,
    price_pence = price_pence,
    venous_only = venous_only,
    venous_available = venous_available,
    available = available
  )

}
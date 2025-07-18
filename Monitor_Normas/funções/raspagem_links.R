mod_scrape_links <- function(base_url, keywords) {
  page <- rvest::read_html(base_url)
  page %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    na.omit() %>%
    purrr::map_chr(~ ifelse(stringr::str_starts(.x, "http"), .x, xml2::url_absolute(.x, base_url))) %>%
    unique() %>%
    purrr::keep(~ stringr::str_detect(tolower(.x), paste(keywords, collapse = "|")))
}


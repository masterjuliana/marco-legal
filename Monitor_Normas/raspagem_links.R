mod_scrape_links <- function(base_url, keywords) {
  read_html(base_url) %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    map_chr(~ ifelse(str_starts(.x, "http"), .x, url_absolute(.x, base_url))) %>%
    unique() %>%
    keep(~ str_detect(tolower(.x), paste(keywords, collapse = "|")))
}

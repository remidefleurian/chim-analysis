list.files("functions", pattern = "\\.R$", full.names = TRUE) %>%
  walk(source)

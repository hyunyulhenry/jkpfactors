sheet <- readxl::excel_sheets('list.xlsx')
data <- lapply(sheet, function(x) {
  read_excel('list.xlsx', sheet = x)
})

data[[1]] = data[[1]] %>% rbind(c('Jamaica', 'jam'))

data[[1]] = data[[1]] %>% 
  slice(c(which(region == "All Regions"), which(region != "All Regions")))

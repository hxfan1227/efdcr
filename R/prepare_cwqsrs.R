cwqsrs <- c(paste0('cwqsr', 0, 1:9, '.inp'), paste0('cwqsr', 10:21, '.inp'))

wq_abbs <- c(`1` = "Cyanobacteria", `2` = "Diatoms", `3` = "GA", `4` = "RPOC", 
             `5` = "LPOC", `6` = "DOC", `7` = "RPOP", `8` = "LPOP", `9` = "DOP", 
             `10` = "TP", `11` = "RPON", `12` = "LPON", `13` = "DON", `14` = "NH4N", 
             `15` = "NO3N", `16` = "PBS", `17` = "DAS", `18` = "COD", `19` = "DO", 
             `20` = "TAM", `21` = "FC")
wq_flists <- plyr::alply(wq_abbs, 
                         1, 
                         function(x, path){
                           list.files(path = path, pattern = paste0('^', x), full.names = T)
                           },
                         path = 'D:\\')
empty_data <- fread(wq_flists[[which(map_int(wq_flists, length) != 0)[1]]][1])
empty_data[, 2] <- 0
station_names <- str_sub(str_extract_all(wq_flists[[which(map_int(wq_flists, length) != 0)[1]]], '_.*\\.'), 2, -2)
for (i in 1:21) {
  writeLines(cwqsrs_h[[i]], cwqsrs[i])
  if (length(wq_flists[[i]]) == 0){
    for (j in 1:length(station_names)) {
    L1 <- paste(1, colnames(empty_data)[1], 86400, 0, 1, 0, '!', station_names[j])
    write(L1, cwqsrs[i], append = T)
    write('1.0', cwqsrs[i], append = T)
    fwrite(empty_data, file = cwqsrs[i], col.names = F, append = T, sep = ' ')
    }
    next()
  }
  for (j in 1:length(station_names)) {
    data_ <- fread(wq_flists[[i]][j])
    L1 <- paste(1, colnames(data_)[1], 86400, 0, 1, 0, '!', station_names[j])
    write(L1, cwqsrs[i], append = T)
    write('1.0', cwqsrs[i], append = T)
    fwrite(data_, file = cwqsrs[i], col.names = F, append = T, sep = ' ')
  }
}


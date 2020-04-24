library(tidyverse)
library(data.table)
library(plyr)
wq_fname <- 'D:\\Data\\wq_input_2010_2015_dcast.csv'
wq_dt <- fread(wq_fname)
wq_dt[, Date := as.Date(Date)]
vars <- colnames(wq_dt)[-1]
a_ply(vars, 1, dt_to_wq, 
      src.dt = wq_dt, path = 'D:', start.date = '2010-01-01', end.date = '2010-12-31', 
      .progress = 'text')

flist <- list.files('D:\\PhD_Models\\2020', pattern = '^cwqsr', full.names = T)

cwqsrs_h <- alply(1:21, 1, function(x){readLines(flist[x], n = 15)})

TP_flist <- list.files(path = 'D:\\', pattern = '^TP', full.names = T)

writeLines(cwqsrs_h[[10]], cwqsrs[10])
for (i in 1:5) {
  data_ <- fread(TP_flist[i])
  L1 <- paste(1, colnames(data_)[1], 86400, 0, 1, 0, '!', str_sub(str_extract_all(TP_flist[i], '_.*\\.'), 2, -2))
  write(L1, cwqsrs[10], append = T)
  write('1.0', cwqsrs[10], append = T)
  fwrite(data_, file = cwqsrs[10], col.names = F, append = T, sep = ' ')
}

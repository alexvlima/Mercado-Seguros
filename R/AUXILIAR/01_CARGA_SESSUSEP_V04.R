
sessionInfo()

rm(list = ls())

# packages ------------------------------------------------------------------

# trace(utils:::unpackPkgZip, edit=TRUE)

# x_username = 'cap09866'
# x_proxy = 'webproxy:80'
# Sys.setenv(https_proxy = x_proxy, https_proxy_user= paste(x_username,.rs.askForPassword("https: Senha"),sep=':'))
# options(repos = 'http://cran.rstudio.com/', setInternet2 = T)

xpacks = c(
   'magrittr'
  ,'RODBC'
  ,'odbc'
  ,'DBI'
  ,'tools'
  ,'readxl'
  ,'readr'
  ,'tidyr'
  ,'tibble'
  ,'stringr'
  ,'textclean'
  ,'dplyr'
  # ,'lubridate'
  # ,'svMisc'
)

xlibs = rownames(installed.packages())
qcheck = quote(cat('Installed = ',xpacks[i] %in% xlibs,'|',xpacks[i],'\n',sep = ''))
for (i in 1:length(xpacks)) {
  if (xpacks[i] %in% xlibs == F) { 
    eval(qcheck);
    install.packages(xpacks[i])
  }
  eval(qcheck)
}
invisible(lapply(xpacks, library, character.only = T))

# param -----------------------------------------------------------------------------

# choose.dir()

x_dir = 
  "\\\\rootbrasil.intranet\\fileserver\\DIRAT\\USUARIOS\\ALISSON\\SES_SUSEP\\" %>% 
  paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))

# odbcDataSources(type = c("all", "user", "system"))
# odbc::odbc()

x_string =
  'SQL Server' %>%
  paste0('Driver={', ., '};') %>%
  paste0('SERVER=SQL33\\ATUARIAL2;DATABASE=WORK_ALISSON')

w_con = dbConnect(
  odbc(),
  .connection_string = x_string,
  encoding = "latin1",
  timeout = 30
)

x_file_old = 'BaseCompleta_Old.zip'
x_file_new = 'BaseCompleta.zip'
x_path_old = paste0(x_dir,x_file_old)
x_path_new = paste0(x_dir,x_file_new)

x_url_zip = paste0('http://www2.susep.gov.br/redarq.asp?arq=',x_file_new)

x_table_prefix = 'SES_SUSEP_'

x_dir_unzip = 
  paste0(x_dir,'UNZIP')%>% 
  paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))

# download -----------------------------------------------------------------------

# suppressWarnings({ download.file(x_url_zip, x_path_new, method = 'wininet') })

x_new = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_new,.)]
x_old = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_old,.)]

if(length(x_old)==1){
  x_md5sum_old = md5sum(x_path_old) %>% unname
}

if(length(x_new)==1){
  x_md5sum_new = md5sum(x_path_new) %>% unname
}

# load -----------------------------------------------------------------------------------------------------------------

if(x_md5sum_new!=x_md5sum_old){
  
  # dir.create(x_dir_unzip, showWarnings = F)
  # 
  # unlink(paste0(x_dir_unzip,'UNZIP'), recursive=TRUE)
  # 
  # unzip(x_path_new,exdir = x_dir_unzip)
  
  tb_files = dir(x_dir_unzip,pattern='.csv|.CSV') %>% data.frame(ARQUIVO = .)
  # glimpse(tb_files)

  for( xi in tb_files$ARQUIVO){
   
    # xi = tb_files$ARQUIVO[2]
    
    x_name_table_dest = paste0(x_table_prefix,xi) %>% str_replace('.csv','') %>% str_to_upper
    
    suppressWarnings({
      try({ 
        paste0("DROP TABLE IF EXISTS ",x_name_table_dest) 
        })
    })
    
    try({
      tb_content =
        paste0(x_dir_unzip,xi) %>%
        read.table(sep=';',header=T,quote='', fill=TRUE,nrows=0)
        names(tb_content) = names(tb_content) %>% replace_non_ascii %>% str_to_upper
      glimpse(tb_content)
    },silent = T)
    
    dbWriteTable(w_con, x_name_table_dest, tb_content, overwrite = T, append = F)
   
    if ( dbListTables(w_con,table_name = x_name_table_dest) %>% length > 0 ){
      cat('\n',format(Sys.time(),'%H:%M:%S|'),'Loaded: ',xi,' into ',x_name_table_dest,'\n',sep='')
    }
    
  }
  # file.rename(x_file_new, x_file_old)
}


# end ------------------------------------------------------------------------------------------------------------------



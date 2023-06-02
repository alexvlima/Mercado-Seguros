
sessionInfo()

options(digits = 8, scipen = 999, stringsAsFactors=F)

rm(list = ls())

# packages ------------------------------------------------------------------

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
  ,'textreadr'
  ,'readxl'
  ,'readr'
  ,'tidyr'
  ,'tibble'
  ,'stringr'
  ,'textclean'
  ,'dplyr'
  ,'bcputility'
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
# odbcDataSources(type = c("all", "user", "system"))

x_file_new = 'BaseCompleta.zip' ##mesmo nome que esta no link da SUSEP
x_file_old = 'BaseCompleta_loaded.zip'

x_driver = 'SQL Server'
x_server = 'SQL33\\ATUARIAL2'
x_database = 'WORK_ALISSON'
x_table_prefix = 'SES_SUSEP_'

x_dir = 
  "\\\\rootbrasil.intranet\\fileserver\\DIRAT\\USUARIOS\\ALISSON\\SES_SUSEP\\" %>% 
  paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))

x_dir_unzip = paste0(x_dir,'UNZIP') %>% paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))

x_url_zip = 'http://www2.susep.gov.br/redarq.asp?arq=' %>% paste0(x_file_new)

# x_url_doc = 'http://www2.susep.gov.br/menuestatistica/ses/download/Documentacao_das_tabelas.rtf'

x_string = paste0('Driver={',x_driver,'};SERVER=',x_server,';DATABASE=',x_database,'')
w_con = dbConnect(odbc(),.connection_string = x_string, encoding = "latin1", timeout = 10)

x_path_old = paste0(x_dir,x_file_old)
x_path_new = paste0(x_dir,x_file_new)

# download -----------------------------------------------------------------------

# suppressWarnings({ download.file(x_url_doc, paste0(x_dir,'documentacao.rtf'), method = 'wininet') })
# 
# 
# x_temp = 
#   paste0(x_dir,'documentacao.rtf') %>% 
#   read_rtf()

# end1 -----------------------------------------------------------------------------------------------------------------

if(file.exists(x_path_new)==F){
  suppressWarnings({ download.file(x_url_zip, x_path_new, method = 'wininet') })
} else {
  cat('Arquivo já baixado:',x_path_new,'\n') 
}

x_old = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_old,.)]
if(length(x_old)==1){
  x_md5sum_old = md5sum(x_path_old) %>% unname
  cat('md5sum old:',x_md5sum_old,'\n')
}

x_new = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_new,.)]
if(length(x_new)==1){
  x_md5sum_new = md5sum(x_path_new) %>% unname
  cat('md5sum new:',x_md5sum_old,'\n')
}

# load -----------------------------------------------------------------------------------------------------------------


if(x_md5sum_new!=x_md5sum_old){
  
  dir.create(x_dir_unzip, showWarnings = F)

  unlink(paste0(x_dir_unzip,'UNZIP'), recursive=TRUE)

  unzip(x_path_new,exdir = x_dir_unzip)
  
  tb_files = 
    dir(x_dir_unzip,pattern='.csv|.CSV') %>% data.frame(ARQUIVO = .) %>% 
    mutate(TAMANHO = apply(., 1, function(x) file.size(paste0(x_dir_unzip,x))) %>% as.integer %>% .[]/1024) %>% 
    mutate(TAMANHO= round(TAMANHO,3)) %>% 
    arrange(TAMANHO)
  glimpse(tb_files)


  for( xi in tb_files$ARQUIVO){
    
    x_name_table_dest = paste0(x_table_prefix,xi) %>% str_replace('.csv','') %>% str_to_upper
    
    suppressWarnings({
      try({ 
        paste0("DROP TABLE IF EXISTS ",x_name_table_dest) %>% dbSendQuery(w_con,.)
      })
    })
    
    try({
      tb_content =
        paste0(x_dir_unzip,xi) %>%
        read.table(sep=';',header=T,quote='', fill=TRUE,dec = ',',nrows=50000) %>% 
        mutate_at(grep("^(valor|MRA|margem|plajustado)",colnames(.)),funs(as.double)) %>%
        mutate_at(grep("^(coenti|damesano|coramo)",colnames(.)),funs(as.integer)) %>%
        .[0,] %>%
        as.data.frame
      names(tb_content) = names(tb_content) %>% replace_non_ascii %>% str_to_upper
      # glimpse(tb_content)
    },silent = T)
    
    dbWriteTable(w_con, x_name_table_dest, tb_content, overwrite = T, append = F)

    system.time({
      try({
        bcpImport(
          paste0(x_dir_unzip,xi)
          ,driver = "SQL Server"
          ,server = 'SQL33\\ATUARIAL2'
          ,database = 'WORK_ALISSON'
          ,trustedconnection = T
          ,fieldterminator = ';'
          ,rowterminator =  '\\n'
          ,table = x_name_table_dest
          ,overwrite = F
          ,batchsize = 5e4,
          ,regional = T
        )
      })
    })
    
    if ( dbListTables(w_con,table_name = x_name_table_dest) %>% length > 0 ){
      cat('\n',format(Sys.time(),'%H:%M:%S|'),'Carregado: ',xi,' em ',x_name_table_dest,'\n',sep='')
    }
  }
  
  paste0(x_dir,x_file_old) %>% unlink
  file.rename(paste0(x_dir,x_file_new), paste0(x_dir,x_file_old))
}


# end ------------------------------------------------------------------------------------------------------------------
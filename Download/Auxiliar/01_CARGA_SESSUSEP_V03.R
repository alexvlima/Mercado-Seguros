
sessionInfo()

rm(list = ls())

# packages ------------------------------------------------------------------

# trace(utils:::unpackPkgZip, edit=TRUE)

# x_username = 'cap09866'
# x_proxy = 'webproxy:80'
# Sys.setenv(https_proxy = x_proxy, https_proxy_user= paste(x_username,.rs.askForPassword("https: Senha"),sep=':'))
# options(repos = 'http://cran.rstudio.com/', setInternet2 = T)

xpacks = c(
   'RODBC'
  ,'odbc'
  ,'readxl'
  ,'readr'
  ,'tidyr'
  ,'dplyr'
  ,'magrittr'
  ,'lubridate'
  ,'stringr'
  ,'tibble'
  ,'textclean'
  ,'svMisc'
  ,'tools'
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

x_file_old = 'BaseCompleta_old.zip'
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

# x_new = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_new,.)]
# x_old = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_old,.)]
# 
# if(length(x_old)==1){
#   x_md5sum_old = md5sum(x_path_old) %>% unname
#   x_md5sum_old = 'asdas'
# }
# 
# if(length(x_new)==1){
#   x_md5sum_new = md5sum(x_path_new) %>% unname
# }

# if(x_md5sum_new!=x_md5sum_old){
  # dir.create(x_dir_unzip, showWarnings = F)
  # unlink(paste0(x_dir_unzip,'UNZIP'), recursive=TRUE)
  # unzip(x_path_new,exdir = x_dir_unzip)
  

# tb_files =
#   data.frame(NOM_ARQUIVO = dir(x_dir_unzip, recursive = T, pattern = '.CSV|.csv|.txt|.TXT')) %>%
#   mutate(NUM_ID = row_number())
# glimpse(tb_files)


# tb_files = 
#   file.info(list.files(x_dir_unzip, full.names=TRUE)) %>% 
#   mutate(DIR_ARQUIVO = rownames(.),TAM_ARQUIVO = size) %>% 
#   arrange(TAM_ARQUIVO) %>% 
#   select(DIR_ARQUIVO, TAM_ARQUIVO) %>% 
#   `rownames<-`( NULL ) 
  

tb_files2 =
  tb_files %>%
  mutate(PATH = rownames(.), TAMANHO = size) %>%
  separate(PATH, c('DIR', 'ARQUIVO'), sep = '/') %>%
  select(ARQUIVO, TAMANHO) %>%
  arrange(TAMANHO) %>% 
  `rownames<-`(NULL) %>%
  .[]



# loop -----------------------------------------------------------------------------------------------------------------


for( xi in tb_files$NOM_ARQUIVO){

  # xi = tb_files2$ARQUIVO[1]
  
  x_name_table_dest = paste0(x_table_prefix,xi) %>% str_replace('.csv','') %>% str_to_upper
  
  suppressWarnings({
    try({ 
      paste0("DROP TABLE IF EXISTS ",x_name_table_dest) 
      })
  })
  
  try({
    tb_content = paste0(x_dir_unzip,xi) %>% read.table(sep=';',header=T) 
    names(tb_content) = names(tb_content) %>% replace_non_ascii %>% str_to_upper
    glimpse(tb_content)
  },silent = T)
  
    dbWriteTable(w_con, x_name_table_dest, tb_content, overwrite = F, append = T)

    if ( dbListTables(w_con,table_name = x_name_table_dest) %>% length > 0 ){
      cat(format(Sys.time(),'%H:%M:%S|'),'Loaded: ',xi,' into ',x_name_table_dest,'\n',sep='')
    }
  
}

  
# }

# list -------------------------------------------------------------------------------------------------------

# tb_files2 = data.frame(
#    NOM_ARQUIVO = dir(x_dir,recursive = T,pattern = '.CSV|.csv|.txt|.TXT')
# ) %>% 
#   mutate(NUM_ID = row_number()) %>% 
#   .[]
# # glimpse(tb_files2)
# 
# tb_layout = 
#   x_dir %>% 
#   paste0(ifelse(str_sub(x_dir,-1)=='\\','','\\'),'layout.xlsx') %>%
#   read_xlsx(sheet  = 'LAYOUT')

# prepare --------------------------------------------------------------------------------------------------------

# n_rows = -1 #numero de lilnhas a ser carregadas. deixar -1 para carregar todas
# 
# tb_param = 
# rbind(
#  c('PAGO','RE0563B','BORDERO_IRB_PAGO')
# ,c('PEND','RE0555B','BORDERO_IRB_PEND')
# ) %>% 
#   as.data.frame %>% 
#   setNames(c('TIPO','ALIAS','DESTINO'))
# 
# 
# for( xi in tb_param$DESTINO){
#   suppressWarnings({
#     try({ paste0("DROP TABLE IF EXISTS ", xi) %>% dbSendQuery(w_con, .) },silent = T)
#   # try({ paste0("DELETE FROM ", xi) %>% dbSendQuery(w_con, .) },silent = T)
#   })
# }

# load loop ------------------------------------------------------------------------------------------------------------------

# for( xi in 1:nrow(tb_files2)){
#   
#   # xi = 57
# 
#   suppressWarnings({
#     rm(list=c('x_file','x_path','tb_atrib','x_tipo','x_name_dest','tb_header','x_size','x_names','tb_content'))
#   })
# 
#   x_file = tb_files2$NOM_ARQUIVO[xi]
#   x_path = paste0(x_dir,ifelse(str_sub(x_dir,-1)=='\\','','\\'),x_file) 
#   
#   tb_atrib = tb_param %>% filter(str_detect(ALIAS,str_split(x_file,'\\.') %>% unlist %>% .[2]))
#   x_tipo = tb_atrib$TIPO
#   x_name_dest = tb_atrib$DESTINO
#   x_size  = tb_layout %>% filter(TABELA==x_tipo) %>% select(TAMANHO) %>% unlist %>% unname
#   x_names = tb_layout %>% filter(TABELA==x_tipo) %>% select(CAMPO) %>% unlist %>% unname
#   
#   
#   try({
#     tb_header = 
#       x_path %>% 
#       read.table(nrows=8,sep=':',skip=1) %>% 
#       column_to_rownames(var="V1") %>% 
#       t %>% as.data.frame %>% 
#       mutate(across(where(is.character), str_trim)) %>% 
#       mutate(NOM_ARQUIVO = x_file)
#     # glimpse(tb_header)
#   },silent = T)
#   
#   
#   try({
#     tb_content = 
#       x_path %>% 
#       read_fwf(skip=10, col_positions = fwf_widths(x_size), n_max=n_rows,show_col_types = F) %>% 
#       setNames(x_names) %>%
#       mutate(NOM_ARQUIVO = x_file) %>% 
#       as.data.frame %>% 
#       .[]
#     # glimpse(tb_content)
#   },silent = T)
#   
#   
#   # tb_carga = tb_header %>% left_join(tb_content)
#   tb_carga = tb_content %>% left_join(tb_header)
#   
#   names(tb_carga) = 
#     names(tb_carga) %>% 
#     replace_non_ascii() %>% 
#     str_replace_all(c("\\.+" = "_")) %>% 
#     str_replace_all(c("\\ +" = "_")) %>% 
#     str_replace_all(c("/" = "")) %>% 
#     str_replace_all('_$','') %>% 
#     str_replace_all(c("%_" = "")) %>%
#     str_replace_all(c("\\)" = "")) %>%
#     str_replace_all(c("\\(" = "")) %>%
#     # str_replace_all(c("\\:" = "")) %>% 
#     str_to_upper()
#   glimpse(tb_carga)
# 
#   suppressWarnings({
#     try({
#       paste0("DELETE FROM ", x_name_dest, " WHERE NOM_ARQUIVO = '",x_file,"'") %>% dbSendQuery(w_con, .)
#     },silent = T)
#   })
#   
#   dbWriteTable(w_con, x_name_dest, tb_carga, overwrite = F, append = T)
#   
#   if ( dbListTables(w_con,table_name = x_name_dest) %>% length > 0 ){
#     cat(format(Sys.time(),'%H:%M:%S|'),'Loaded: ',x_file,'\n',sep='')
#   }
#   
#   cat('\nID = ',tb_files2$NUM_ID[xi],' (',round(100*xi/nrow(tb_files2),2),'%) \n',sep='')
#   
# }


# end -------------------------------------------------------------------------------------------------------------





# teste ---------------------------------------------------------------------------------------


# teste 2 -------------------------------------------------------------------------------------




  options(digits = 8, scipen = 999, stringsAsFactors=F, setInternet2 = T)
  
  rm(list = ls())

# packages ------------------------------------------------------------------------------------------------------------------

# x_username = 'cap09866'
# x_proxy = 'webproxy:80'
# Sys.setenv(https_proxy = x_proxy, https_proxy_user= paste(x_username,.rs.askForPassword("https: Senha"),sep=':'))

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
    ,'rvest'
    ,'RCurl'
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


# function -------------------------------------------------------------------------------------------------------------

  fx_detect_type = function(x='vazio'){
    # x_arg = quo_name(enquo(x))
    # if(exists(x)){
      v_size = str_squish(x) %>% nchar %>% max %>% pmax(1)
      v_type = coalesce(
        str_squish(x) %>%
          str_detect("^[:digit:]+$") %>% {
            ifelse(sum(.) == length(.),'BIGINT',NA)
          }
        ,
        str_squish(x) %>%
          str_replace(',','') %>%
          str_replace('\\.','') %>%
          str_replace('-','') %>%
          str_replace('E','') %>%
          str_detect("^[:digit:]+$") %>% {
            ifelse(sum(.) == length(.),'FLOAT',NA)
          }
        ,paste0('VARCHAR(',v_size,')')
      )
      return(v_type)
    # } else (
    #   return('VARCHAR(255)')
    # )
  }

# param ---------------------------------------------------------------------------------------------------------------------

  # choose.dir()
  # odbcDataSources(type = c("all", "user", "system"))

  x_dir = 
    "\\\\rootbrasil.intranet\\fileserver\\DIRAT\\GEMOP\\ESTUDOS_ESTATISTICOS\\06_PEFORMANCE\\SUSEP_SES" %>% 
    paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))
  
  x_dir_unzip = 
    paste0(x_dir,'UNZIP') %>% 
    paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))
  
  x_file_dow = 'BaseCompleta.zip' ##mesmo nome que esta no link da SUSEP
  x_file_old = 'BaseCompleta_loaded.zip'
  x_driver = 'SQL Server'
  x_server = 'SQL33\\ATUARIAL2'
  x_database = 'WORK_ALISSON'
  x_table_prefix = 'TEMP_'
  x_name_carga = 'CONTROLE_CARGA'
  x_name_md5 = 'CONTROLE_MD5SUM'
  
  x_url_main = "https://www2.susep.gov.br/menuestatistica/SES/principal.aspx"
  x_url_zip = 'http://www2.susep.gov.br/redarq.asp?arq=' %>% paste0(x_file_dow)
  # x_url_doc = 'http://www2.susep.gov.br/menuestatistica/ses/download/Documentacao_das_tabelas.rtf'

  x_path_old = paste0(x_dir,x_file_old)
  x_path_new = paste0(x_dir,x_file_dow)
  
  x_string = paste0('Driver={',x_driver,'};SERVER=',x_server,';DATABASE=',x_database,'')
  w_con = dbConnect(odbc(),.connection_string = x_string, encoding = "latin1", timeout = 10)
  
  source(paste0(x_dir,'FUN_bcpImport2.R'))
  
# scrap ----------------------------------------------------------------------------------------------------------------

  # x_url_main = "https://cepbrasil.org"
  w_site = try(read_html(x_url_main))
  x_text = w_site %>% html_text2
  
  x_find = 'atualizada até '
  x_pos = str_locate(x_text,pattern = x_find)[[1]]

  x_descr = str_sub(x_text,x_pos,x_pos+nchar(x_find)+999) %>% str_split('\r') %>% unlist %>% .[1]
  
# download ------------------------------------------------------------------------------------------------------------------

# suppressWarnings({ download.file(x_url_doc, paste0(x_dir,'Documentacao_das_tabelas.rtf'), method = 'wininet') })

  if(file.exists(x_path_new)==F){
    suppressWarnings({ download.file(x_url_zip, x_path_new, method = 'wininet') })
  } else {
    cat('Arquivo já baixado:',x_path_new,'\n') 
  }
  
# hash ----------------------------------------------------------------------------------------------------------------------

  x_old = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_old,.)]
  x_md5sum_old = 'empty_old_hash'
  
  if(length(x_old)==1){
    x_md5sum_old = md5sum(x_path_old) %>% unname
    cat('md5sum old:',x_md5sum_old,'\n')
  }

  x_new = dir(x_dir,pattern = '.zip') %>% .[grep(x_file_dow,.)]
  if(length(x_new)==1){
    x_md5sum_new = md5sum(x_path_new) %>% unname
    cat('md5sum new:',x_md5sum_new,'\n')
  }

# load ----------------------------------------------------------------------------------------------------------------------

  system.time({
    if(x_md5sum_new!=x_md5sum_old){
      
      # unlink(x_dir_unzip, recursive=T)
      # dir.create(x_dir_unzip, showWarnings = F)
      # unzip(x_path_new,exdir = x_dir_unzip)
      
      tb_files =
        dir(x_dir_unzip,pattern='.csv|.CSV') %>% data.frame(ARQUIVO = .) %>%
        mutate(TAMANHO_KB = apply(., 1, function(x) file.size(paste0(x_dir_unzip,x))) %>% as.integer %>% .[]/1024) %>%
        mutate(TAMANHO_KB= round(TAMANHO_KB,3)) %>%
        # filter(TAMANHO_KB > 5000) %>% # fitlro para testes
        arrange(TAMANHO_KB) %>%
        mutate(ID = row_number()) %>%
        as.data.frame
       
      for( i in tb_files$ID){
        
        # i=5
        
        xi = tb_files$ARQUIVO[tb_files$ID==i]
        cat(rep('-',100),'\n',format(Sys.time(),'%H:%M:%S|'),'Carregando: ',xi,' [',i,']\n',sep='')
        
        x_name_table_dest = paste0(x_table_prefix,xi) %>% str_replace('.csv','') %>% str_to_upper
        
        suppressWarnings({
          try({ 
            paste0("DROP TABLE IF EXISTS ",x_name_table_dest) %>% dbSendQuery(w_con,.)
            paste0("DROP TABLE IF EXISTS ",sub(x_table_prefix,'',x_name_table_dest)) %>% dbSendQuery(w_con,.)
          })
        })
        
        suppressWarnings({
          try({ 
            tb_sample =
              paste0(x_dir_unzip,xi) %>% 
              read.table(
                 sep=';', header=T, quote='', fill=TRUE, dec = ',', nrows=100000
                ,stringsAsFactors = F, colClasses = 'character', numerals = "no.loss"
              ) %>%
              setNames(names(.) %>% replace_non_ascii %>% str_to_upper) %>% 
              as.data.frame
              
          })
        })
        # glimpse(tb_sample)
        
        tb_types = data.frame(CAMPO = names(tb_sample),TYPE=as.character(NA))
        for(i in 1:nrow(tb_types)){
          tb_types$TYPE[i] = fx_detect_type(tb_sample[,i])
        }
       
        suppressWarnings({
          try({ 
            dbWriteTable(w_con, x_name_table_dest, tb_sample, overwrite = T, append = F, row.names = F)
            paste0("DELETE FROM ",x_name_table_dest) %>% dbSendQuery(w_con,.)
          })
        })
       
        system.time({
          suppressWarnings({
            try({
              bcpImport2(
                 paste0(x_dir_unzip,xi)
                ,driver = x_driver
                ,server = x_server
                ,database = x_database
                ,table = x_name_table_dest
                ,trustedconnection = T
                ,fieldterminator = ';'
                ,rowterminator =  '\\n'
                ,overwrite = F
                ,batchsize = 5e4,
                ,regional = F
                ,init = '-C RAW'
              )
            })
          })
        })
        
        suppressWarnings({
          x_query = 
            paste0(
            "SELECT COLUMN_NAME
            FROM INFORMATION_SCHEMA.COLUMNS
            WHERE 1=1
            AND TABLE_NAME  = '",x_name_table_dest,"'"
            ) %>% 
            dbGetQuery(w_con,.) %>% 
            rename(c('CAMPO'='COLUMN_NAME')) %>% 
            left_join(tb_types) %>% 
            mutate(TYPE = coalesce(TYPE,'VARCHAR(255)')) %>% 
            mutate(ID = row_number()) %>%
            mutate(QUERY = paste0(
              ifelse(ID==1,' ',',')
              ,CAMPO
              ,' = TRY_CAST('
              ,case_when(
                TYPE == 'FLOAT'~paste0('REPLACE(', CAMPO, ",',','.')")
                ,str_sub(TYPE,1,7)=='VARCHAR' ~ paste0('RTRIM(LTRIM(', CAMPO, "))")
                ,T~CAMPO
              )
              ,' AS '
              ,TYPE
              ,')'
            )) %>% 
            select(QUERY) %>% unlist %>% unname %>% paste0(collapse = '\n') %>%
            paste0('SELECT \n',.,'\nINTO ',sub(x_table_prefix,'',x_name_table_dest),'\nFROM ',x_name_table_dest,'\n')
        })
        # cat(x_query)
        
        suppressWarnings({
          try({ 
            dbSendQuery(w_con,x_query)
            n_rows = 0
            n_rows = paste0('SELECT COUNT(*) FROM ',sub(x_table_prefix,'',x_name_table_dest)) %>% 
            dbGetQuery(w_con,.) %>% unlist %>% unname
          })
        })
        
        if ( dbListTables(w_con,table_name = x_name_table_dest) %>% length > 0 & n_rows > 0){
          cat(
            format(Sys.time(), '%H:%M:%S|'),
            'Finalizado: ',
            xi,
            ' carregado em ',
            x_name_table_dest,
            '|linhas = ',
            n_rows,
            '\n\n',
            sep = ''
          )
          paste0("DROP TABLE IF EXISTS ",x_name_table_dest) %>% dbSendQuery(w_con,.)
        }
        
        if (n_rows == 0) {
          cat(
            format(Sys.time(), '%H:%M:%S|'),
            'Erro: ',
            xi,
            ' carregado em ',
            x_name_table_dest,
            '|linhas = ',
            n_rows,
            '\n\n',
            sep = ''
          )
        }
        
      }
      # paste0(x_dir,x_file_old) %>% unlink
      # file.rename(paste0(x_dir,x_file_dow), paste0(x_dir,x_file_old))
    }
  })
  
  tb_controle = 
    tb_files %>% 
    mutate(name = ARQUIVO %>% str_replace('.csv','') %>% str_to_upper) %>% 
    left_join(dbGetQuery(w_con,'SELECT * FROM SYS.TABLES')) %>% 
    select(ARQUIVO, TAMANHO_KB, ID, TABLE_NAME = name, CREATE_DATE = create_date)
  paste0("DROP TABLE IF EXISTS ",x_name_carga) %>% dbSendQuery(w_con,.)
  dbWriteTable(w_con, x_name_carga, tb_controle, overwrite = T, append = F, row.names = F)
  
  tb_md5sum = 
    data.frame(
       ARQUIVO = x_file_old
      ,DESCRICAO = x_descr
      ,MD5SUM = x_md5sum_new
      )
  paste0("DROP TABLE IF EXISTS ",x_name_md5) %>% dbSendQuery(w_con,.)
  dbWriteTable(w_con, x_name_md5, tb_md5sum, overwrite = T, append = F, row.names = F)

# end -----------------------------------------------------------------------------------------------------------------------
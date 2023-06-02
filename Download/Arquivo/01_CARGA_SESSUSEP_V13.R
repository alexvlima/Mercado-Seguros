# init -----------------------------------------------------------------------------------------------------------------

  x_username = 'cap09866'
  x_proxy = 'webproxy:80'
  # Sys.setenv(https_proxy = x_proxy, https_proxy_user= paste(x_username,.rs.askForPassword("https: Senha"),sep=':'))

  options(digits = 8, scipen = 999, stringsAsFactors=F, setInternet2 = T)
  
  rm(list = ls())

# packages ------------------------------------------------------------------------------------------------------------------

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
      v_size = str_squish(x) %>% nchar %>% max %>% pmax(1)
      v_type = coalesce(
        str_squish(x) %>%
          str_detect("^[:digit:]+$") %>% {
            ifelse(sum(.) == length(.),'BIGINT',NA)
          }
        ,
        str_squish(x) %>%
          str_replace_all(',','') %>%
          str_replace_all('\\.','') %>%
          str_replace_all('-','') %>%
          str_replace_all('E','') %>%
          str_detect("^[:digit:]+$") %>% {
            ifelse(sum(.) == length(.),'FLOAT',NA)
          }
        ,paste0('VARCHAR(',v_size,')')
      )
      return(v_type)
  }

# param ---------------------------------------------------------------------------------------------------------------------

  # choose.dir()
  # odbcDataSources(type = c("all", "user", "system"))

  x_dir = 
    "\\\\rootbrasil.intranet\\fileserver\\DIRAT\\GEMOP\\ESTUDOS_ESTATISTICOS\\00_DADOS\\SUSEP_SES" %>% 
    paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))
  
  x_dir_unzip = 
    paste0(x_dir,'UNZIP') %>% 
    paste0(.,ifelse(str_sub(.,-1)=='\\','','\\'))
  
  x_file_dow = 'BaseCompleta.zip' ##mesmo nome que esta no link da SUSEP
  x_file_old = 'BaseCompleta_loaded.zip'
  x_driver = 'SQL Server'
  x_server = 'SQL33\\ATUARIAL2'
  x_database = 'SUSEP_SES'
  x_table_prefix = 'TEMP_'
  x_name_carga = 'CONTROLE_CARGA'
  x_name_versao = 'CONTROLE_VERSAO'
  
  x_url_main = "https://www2.susep.gov.br/menuestatistica/SES/principal.aspx"
  x_url_zip = 'http://www2.susep.gov.br/redarq.asp?arq=' %>% paste0(x_file_dow)
  # x_url_doc = 'http://www2.susep.gov.br/menuestatistica/ses/download/Documentacao_das_tabelas.rtf'

  x_path_old = paste0(x_dir,x_file_old)
  x_path_new = paste0(x_dir,x_file_dow)
  
  w_con = 
    paste0('Driver={',x_driver,'};SERVER=',x_server,';DATABASE=',x_database,'') %>% 
    dbConnect(odbc(),.connection_string = ., encoding = "latin1", timeout = 10)
  
  source(paste0(x_dir,'FUN_bcpImport2.R'))
  
# scrap ----------------------------------------------------------------------------------------------------------------

  w_site = try(read_html(x_url_main))
  x_text = ''
  x_text = try(w_site %>% html_text2)
  
  x_find = 'atualizada até '
  x_pos = str_locate(x_text,pattern = x_find)[[1]]

  x_descr = str_sub(x_text,x_pos,x_pos+nchar(x_find)+999) %>% str_split('\r') %>% unlist %>% .[1] %>% str_to_title
  
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
        
        # i=1
        
        xi = tb_files$ARQUIVO[tb_files$ID==i]
        cat(rep('-',100),'\n',format(Sys.time(),'%H:%M:%S|'),'Carregando: ',xi,' [',i,']\n',sep='')
        
        x_name_dest = xi %>% str_replace('.csv','') %>% str_to_upper
        x_name_temp = paste0(x_table_prefix,x_name_dest)
        
        suppressWarnings({
          try({ 
            paste0("DROP TABLE IF EXISTS ",x_name_temp) %>% dbSendQuery(w_con,.)
            paste0("DROP TABLE IF EXISTS ",x_name_dest) %>% dbSendQuery(w_con,.)
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
            dbWriteTable(w_con, x_name_temp, tb_sample, overwrite = T, append = F, row.names = F)
            paste0("DELETE FROM ",x_name_temp) %>% dbSendQuery(w_con,.)
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
                ,table = x_name_temp
                ,trustedconnection = T
                ,fieldterminator = ';'
                ,rowterminator =  '\\n'
                ,overwrite = F
                ,batchsize = 5e4,
                ,regional = F
                ,init = '-C RAW -F 2'
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
            AND TABLE_NAME  = '",x_name_temp,"'"
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
            paste0('SELECT \n',.,'\nINTO ',x_name_dest,'\nFROM ',x_name_temp,'\n') %>% 
            paste0(ifelse(x_name_temp=='TEMP_SES_DIVERSOS',"WHERE DESCRICAO != 'descricao'",''))
        })
        # cat(x_query)
        
        suppressWarnings({
          try({
            dbSendQuery(w_con,x_query)
            n_rows = 0
            n_rows = paste0('SELECT COUNT(*) FROM ',x_name_dest) %>% 
            dbGetQuery(w_con,.) %>% unlist %>% unname
          })
        })
        
        x_check_table = dbListTables(w_con,table_name = x_name_temp) %>% length > 0 & n_rows > 0
        if ( x_check_table == T){
          cat(
            format(Sys.time(), '%H:%M:%S|'),
            '\nFinalizado: ',
            xi,
            ' carregado em ',
            x_name_temp,
            '|linhas = ',
            n_rows,
            '\n\n',
            sep = ''
          )
          paste0("DROP TABLE IF EXISTS ",x_name_temp) %>% dbSendQuery(w_con,.)
        }
        
        if (n_rows == 0) {
          cat(
            format(Sys.time(), '%H:%M:%S|'),
            '\nErro: ',
            xi,
            ' carregado em ',
            x_name_temp,
            '|linhas = ',
            n_rows,
            '\n\n',
            sep = ''
          )
        }
        
        if(data.frame(TABELA = dbListTables(w_con)) %>% filter(TABELA==x_name_carga) %>% nrow == 0){
          tb_controle = data.frame(
             ID = as.integer(0)
            ,ARQUIVO = as.character('')
            ,TAMANHO_KB = as.double(0)
            ,TABLE_NAME = as.character('')
            ,CREATE_DATE = Sys.time()
            ,DESCRICAO = as.character('')
            ,MD5SUM = as.character('')
            )[0,]
          dbWriteTable(w_con, x_name_carga, tb_controle, overwrite = T, append = F, row.names = F)
        }
        
        suppressWarnings({
          try({ 
            tb_controle = 
              tb_files %>% 
              mutate(
                MD5SUM    = ifelse(x_check_table, x_md5sum_new, NA),
                DESCRICAO = ifelse(x_check_table, x_descr, NA),
                name      = ARQUIVO %>% str_replace('.csv','') %>% str_to_upper
              ) %>% 
              left_join(dbGetQuery(w_con,'SELECT * FROM SYS.TABLES')) %>% 
              select(ID, ARQUIVO, TAMANHO_KB, TABLE_NAME = name, CREATE_DATE = create_date, DESCRICAO, MD5SUM) %>% 
              filter(TABLE_NAME == x_name_dest)
            paste0("DELETE FROM ",x_name_carga," WHERE TABLE_NAME = '",x_name_dest,"'") %>% dbSendQuery(w_con,.)
            dbWriteTable(w_con, x_name_carga, tb_controle, overwrite = F, append = T, row.names = F)
          })
        })
        
      }
     
      suppressWarnings({
        try({
          paste0(x_dir,x_file_old) %>% unlink
          file.rename(paste0(x_dir,x_file_dow), paste0(x_dir,x_file_old))
        })
      })
      
    }
  })
  
  # suppressWarnings({
  #   try({
  #     tb_versao =
  #       data.frame(
  #          ARQUIVO = x_file_old
  #         ,DESCRICAO = x_descr
  #         ,VERSAO = x_md5sum_new
  #         )
  #     paste0("DROP TABLE IF EXISTS ",x_name_versao) %>% dbSendQuery(w_con,.)
  #     dbWriteTable(w_con, x_name_versao, tb_versao, overwrite = T, append = F, row.names = F)
  #   })
  # })

# end -----------------------------------------------------------------------------------------------------------------------

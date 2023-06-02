
  options(digits = 8, scipen = 999, stringsAsFactors=F)
  
  rm(list = ls())

# packages ------------------------------------------------------------------------------------------------------------------

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



# function -------------------------------------------------------------------------------------------------------------

  fx_detect_type = function(x){
    x_arg = quo_name(enquo(x))
    if(exists(x_arg, where = .GlobalEnv)){
      v_size = str_squish(x) %>% nchar %>% max
      v_type =
        paste0(
          str_squish(x) %>%
            str_detect("^[:alnum:]+$") %>% {
              ifelse(sum(.) == length(.),paste0('VARCHAR(',v_size,')'),'')
            }
          ,
          str_squish(x) %>%
            str_detect("^[:digit:]+$") %>% {
              ifelse(sum(.) == length(.),'BIGINT','')
            }
          ,
          str_squish(x) %>%
            str_replace(',','') %>%
            str_replace('\\.','') %>%
            str_replace('-','') %>%
            str_replace('E','') %>%
            str_detect("^[:digit:]+$") %>% {
              ifelse(sum(.) == length(.),'FLOAT','')
            }
        )
      return(v_type)
    } else (
      return('VARCHAR(255)')
    )
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
  x_types = '00_TYPES_V01.xlsx'
  x_driver = 'SQL Server'
  x_server = 'SQL33\\ATUARIAL2'
  x_database = 'WORK_ALISSON'
  x_table_prefix = 'TEMP_'
  
  x_url_zip = 'http://www2.susep.gov.br/redarq.asp?arq=' %>% paste0(x_file_dow)
  
  # x_url_doc = 'http://www2.susep.gov.br/menuestatistica/ses/download/Documentacao_das_tabelas.rtf'
  
  x_string = paste0('Driver={',x_driver,'};SERVER=',x_server,';DATABASE=',x_database,'')
  w_con = dbConnect(odbc(),.connection_string = x_string, encoding = "latin1", timeout = 10)
  
  x_path_old = paste0(x_dir,x_file_old)
  x_path_new = paste0(x_dir,x_file_dow)

  
  
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
  
  
  if(x_md5sum_new!=x_md5sum_old){
    
    tb_types =
      paste0(x_dir,x_types) %>%
      read_xlsx(sheet='types') %>%
      mutate(CAMPO = CAMPO %>% replace_non_ascii %>% str_to_upper) %>%
      mutate(
        TYPE_SQL = case_when(
           TIPO == 'character'~'VARCHAR(255)'
          ,TIPO == 'double'~'FLOAT'
          ,TIPO == 'integer'~'BIGINT'
        )
      ) %>% 
      distinct %>% 
      as.data.frame
    # glimpse(tb_types)
    
    # unlink(paste0(x_dir_unzip,'UNZIP'), recursive=F)
    # dir.create(x_dir_unzip, showWarnings = F)
    # unzip(x_path_new,exdir = x_dir_unzip)
    
    tb_files =
      dir(x_dir_unzip,pattern='.csv|.CSV') %>% data.frame(ARQUIVO = .) %>%
      mutate(TAMANHO = apply(., 1, function(x) file.size(paste0(x_dir_unzip,x))) %>% as.integer %>% .[]/1024) %>%
      mutate(TAMANHO= round(TAMANHO,3)) %>%
      # filter(TAMANHO > 5000) %>% # fitlro para testes
      arrange(TAMANHO) %>%
      mutate(ID = row_number()) %>%
      as.data.frame
  
    for( i in tb_files$ID){
      
      # i=31
      
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
      
      # suppressWarnings({
      #   try({ 
      #     for(xj in names(tb_sample)){
      #       class(tb_sample[, xj]) = tb_types %>% filter(CAMPO==xj) %>% select(TIPO) %>% unlist %>% unname
      #     }
      #   })
      # })
      
      tb_carga = tb_sample %>% mutate(across(where(is.character), str_trim))
      
      suppressWarnings({
        try({ 
          dbWriteTable(w_con, x_name_table_dest, tb_carga, overwrite = T, append = F, row.names = F)
          paste0("DELETE FROM ",x_name_table_dest) %>% dbSendQuery(w_con,.)
        })
      })
     
      system.time({
        suppressWarnings({
          try({
            bcpImport(
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
            )
          })
        })
      })
      
      try({
        n_rows = 0
        n_rows = paste0('SELECT COUNT(*) FROM ',x_name_table_dest) %>% dbGetQuery(w_con,.) %>% unlist %>% unname
      })
      
      suppressWarnings({
        x_query = 
          paste0("
          SELECT COLUMN_NAME
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE 1=1
          AND TABLE_NAME  = '",x_name_table_dest,"'") %>% 
          dbGetQuery(w_con,.) %>% 
          rename(c('CAMPO'='COLUMN_NAME')) %>% 
          left_join(tb_types) %>% 
          mutate(TYPE_SQL = coalesce(TYPE_SQL,'VARCHAR(255)')) %>% 
          mutate(ID = row_number()) %>%
          mutate(QUERY = paste0(
            ifelse(ID==1,' ',',')
            ,CAMPO
            ,' = TRY_CAST('
            ,ifelse(TIPO == 'double', paste0('REPLACE(', CAMPO, ",',','.')"), CAMPO)
            ,' AS '
            ,TYPE_SQL
            ,')'
          )) %>% 
          select(QUERY) %>% unlist %>% unname %>% paste0(collapse = '\n') %>% 
          paste0('SELECT \n',.,'\nINTO ',sub(x_table_prefix,'',x_name_table_dest),'\nFROM ',x_name_table_dest)
      })
      cat(x_query)
      
      suppressWarnings({
        try({ 
          dbSendQuery(w_con,x_query)
          # paste0("DROP TABLE IF EXISTS ",x_name_table_dest) %>% dbSendQuery(w_con,.)
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
    
    paste0(x_dir,x_file_old) %>% unlink
    file.rename(paste0(x_dir,x_file_dow), paste0(x_dir,x_file_old))
  }

# end -----------------------------------------------------------------------------------------------------------------------

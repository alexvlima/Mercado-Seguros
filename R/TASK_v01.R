
# init -------------------------------------------------------------------------------------------------------

x_username = 'cap09866'
x_proxy = 'webproxy:80'

if(Sys.getenv('https_proxy') == ''){
  Sys.setenv(
    https_proxy = x_proxy,
    https_proxy_user = paste(x_username, .rs.askForPassword("https: Senha"), sep =':')
  )
}


options(
  digits = 10,
  scipen = 999,
  encoding = 'latin1',
  stringsAsFactors = F,
  setInternet2 = T,
  repos = 'https://cran.fiocruz.br/'
)


# packs ------------------------------------------------------------------------------------------------------

xpacks = c(
   'taskscheduleR'
  ,'magrittr'
  ,NA
)

xpacks = xpacks[!is.na(xpacks)]
for(i in 1:length(xpacks)) {
  if(xpacks[i] %in% row.names(installed.packages())){
    cat('Jรก Instalado:',xpacks[i],'\n')
  } else {
    cat('Instalando...',xpacks[i],'\n')
    install.packages(xpacks[i])
  }
}

invisible(lapply(xpacks, library, character.only = T))


# save -------------------------------------------------------------------------------------------------------

x_dir_share = "\\\\rootbrasil.intranet\\fileserver\\DIRAT\\GEMOP\\ESTUDOS_ESTATISTICOS\\00_DADOS\\SUSEP_SES\\"

x_dir_local = 'C:\\TEMP_R\\'

x_script_name = '01_CARGA_SESSUSEP_V14.R'
x_fun_name = 'FUN_bcpImport2.R'

dir.create(x_dir_local,showWarnings = F)

invisible(file.copy(
   paste0(x_dir_share,x_script_name)
  ,paste0(x_dir_local,x_script_name)
  ,overwrite = T
))

invisible(file.copy(
   paste0(x_dir_share,x_fun_name)
  ,paste0(x_dir_local,x_fun_name)
  ,overwrite = T
))


# task -------------------------------------------------------------------------------------------------------


# tb_tasks = taskscheduler_ls()

taskscheduler_delete(taskname = "UPLOAD_SUSEP_SES")

taskscheduler_create(
  taskname = "UPLOAD_SUSEP_SES",
  rscript = paste0(x_dir_local,x_script_name),
  schedule = "MONTHLY",
  starttime = '16:13'
)

shell.exec('taskschd.msc')
# tb_tasks = taskscheduler_ls()

# end --------------------------------------------------------------------------------------------------------



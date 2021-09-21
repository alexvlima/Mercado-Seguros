################
### DOWNLOAD ###
################

link <- "http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip"

options(timeout=10000)
download.file(link, "BaseCompleta.zip")

#############
### UNZIP ###
#############

unzip("BaseCompleta.zip", exdir = "BaseCompleta")

################
### DOWNLOAD ###
################

link <- "http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip"

download.file(link, "BaseCompleta.zip")

#############
### UNZIP ###
#############

unzip("BaseCompleta.zip", exdir = "BaseCompleta")

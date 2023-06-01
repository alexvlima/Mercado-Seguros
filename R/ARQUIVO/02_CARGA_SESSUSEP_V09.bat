::limpar o prompt
cls

::desligar o eco automatico dos comandos no prompt
@echo off

::escrever prompt:
@echo Inicio #########################################################################

::criar um mapeamento de rede temporario para a pasta de rede atual, 
::pois batch script nao funciona em pastas de rede
@pushd %~dp0

::escrever no prompt uma linha vazia
@echo.

::executar todos os scripts sql da pasta atual, passando o parâmetro Y (anomes) para o script
for %%X in (*.R) do (
  @echo on
  @echo Executando o script %%X ...
  "D:\Programas\R-4.2.1\bin\R.exe" CMD BATCH -i "%%X"
  @echo off
)

::desconectar o mapeamento de rede criado
@popd

::escrever no prompt
@echo.
@echo Fim ############################################################################

::@echo Pressione qualquer tecla para sair
::pausar o script antes de fechar
pause>nul
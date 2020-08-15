# autor: Gabriel Mendes Borges
# data: 20/04/2020
# artigo (em coautoria com Claudio Crespo)

# Código de leitura e recodificação as bases da PNS 2013 previamente baixadas e salvas em formato rds

# Carrega/instala bibliotecas utilizadas no código
list.of.packages <- c("tidyverse", "gt", "gtsummary", "survey", "rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = TRUE))

# set working directory
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd(system('git rev-parse --show-toplevel', intern=T))

# ler bases da PNS em formato rds
# As bases de microdados estão disponíveis no site do IBGE:
# https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=microdados
# Para transformar as bases para o formato rds, ver links abaixo:
# http://asdfree.com/pesquisa-nacional-de-saude-pns.html#simplified-download-and-importation-45
# #https://gist.github.com/momenezes/50f6209802a9f5d6dba19677bbd909af

# ler bases de pessoas e de morador selecionado
pns_design_all <- 
  read_rds('/Volumes/HD-Mac/PNS/Dados/all questionnaire survey design.rds')
pns_design <- 
  read_rds('/Volumes/HD-Mac/PNS/Dados/long questionnaire survey design.rds')

# recodifica base da PNS de todos os moradores
pns_design_all <- 
  update(
    pns_design_all,
    one = 1,
    
    situacao = 
      factor( v0026 , labels = c('Urbano', 'Rural')),
              
    uf = 
      factor( v0001 , 
              labels = c( 'Rondônia' , 'Acre' , 'Amazonas' , 'Roraima' , 'Pará' , 
                          'Amapá' , 'Tocantins' , 'Maranhão' , 'Piauí' , 'Ceará' , 
                          'Rio Grande do Norte' , 'Paraíba' , 'Pernambuco' , 'Alagoas' ,
                          'Sergipe' , 'Bahia' , 'Minas Gerais' , 'Espírito Santo' , 
                          'Rio de Janeiro' , 'São Paulo' , 'Paraná' , 'Santa Catarina' ,
                          'Rio Grande do Sul' , 'Mato Grosso do Sul' , 'Mato Grosso' , 
                          'Goiás' , 'Distrito Federal' ) ),
    
    idade_cat = 
      factor( 1 + findInterval( as.numeric( c008 ) , c(0, 18,seq(25,60,5)) ) , 
              labels = c('0-18', '18-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60+' )),
    
    idoso = 
      as.numeric(as.numeric( c008) >= 60),

    raca = 
      factor( ifelse(c009==9, NA, c009) , 
              labels = c( 'Branca' , 'Preta' , 'Amarela' , 'Parda' , 'Indigena') ),
    
    forca = factor( vde001 , labels = c(NA, 'na força', 'fora da força') ),
    ocupada = factor( vde002 , labels = c(NA, 'ocupada', 'não ocupada') ),
    
    chavedom = paste0(v0001 , v0024 , upa_pns , v0006_pns)
  )

table(pns_design_all$variables$forca, useNA = "ifany")




# recodifica base da PNS de moradores selecionados
pns_design <- 
  update( 
    pns_design , 
    one = 1,
    
    situacao = 
      factor( v0026 , labels = c('Urbano', 'Rural')),
    
    uf = factor( v0001 , 
                 labels = c( 'Rondônia' , 'Acre' , 'Amazonas' , 'Roraima' , 'Pará' , 
                             'Amapá' , 'Tocantins' , 'Maranhão' , 'Piauí' , 'Ceará' , 
                             'Rio Grande do Norte' , 'Paraíba' , 'Pernambuco' , 'Alagoas' ,
                             'Sergipe' , 'Bahia' , 'Minas Gerais' , 'Espírito Santo' , 
                             'Rio de Janeiro' , 'São Paulo' , 'Paraná' , 'Santa Catarina' ,
                             'Rio Grande do Sul' , 'Mato Grosso do Sul' , 'Mato Grosso' , 
                             'Goiás' , 'Distrito Federal' ) ),
    
    # recodifica variáveis de idade
    idade_cat = factor( 1 + findInterval( as.numeric( c008 ) , c(18,seq(25,60,5)) ) , 
                        labels = c('18-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60+' ) ),
    idade_cat2 = factor( 1 + findInterval( as.numeric( c008 ) , c( 18 , 30 , 40 , 50, 60 ) ) , 
                        labels = c('18-29' , '30-39' , '40-49' , '50-59', '60+' ) ),
    idade_cat80 = factor( 1 + findInterval( as.numeric( c008 ) , c(18,seq(25,80,5)) ) , 
                        labels = c('18-24','25-29','30-34','35-39','40-44','45-49','50-54',
                                   '55-59','60-64', '65-69', '70-74', '75-79', '80+') ),
    idade_cat65 = factor( 1 + findInterval( as.numeric( c008 ) , c(18,seq(25,65,5)) ) , 
                          labels = c('18-24','25-29','30-34','35-39','40-44','45-49','50-54',
                                     '55-59','60-64', '65+') ),
    idoso = as.numeric(as.numeric( c008) >= 60) ,
    # cor ou raça
    raca = 
      factor( ifelse(c009==9, NA, c009) , 
              labels = c( 'Branca' , 'Preta' , 'Amarela' , 'Parda' , 'Indigena') ),
    # Condição em relação à força de trabalho e condição na ocupação
    forca = factor( vde001 , labels = c('na força', 'fora da força') ),
    ocupada = factor( vde002 , labels = c(NA, 'ocupada', 'não ocupada') ),
    #tem plano de saúde?
    plano = factor( i001 , labels = c('sim', 'não') ),
    # peso, altura, IBM e indicador de obesidade
    peso = w00103,
    altura =  w00203/100,
    IBM = peso / (altura^2),
    obesidade = as.numeric(as.numeric( IBM) >= 30) ,
    obesidade_morbida = as.numeric(as.numeric( IBM) >= 40) ,
    # pressão arterial medida por instrumento maior que 140/90
    ha_inst = as.numeric( w00407 >= 140 | w00408 >= 90),
    # ... ou tomou medicamentos por causa da hipertensão arterial (pressão alta)?
    ha_inst_med = as.numeric( w00407 >= 140 |  w00408 >= 90 | q006 == 1),
    #morbidades autorreferidas
    hipertensao = as.numeric( q002 == 1 ),
    diabetes = as.numeric( q030 == 1 ),
    card = as.numeric( q063 == 1 ),
    asma = as.numeric( q074 == 1 ),
    pulmao = as.numeric( q116 == 1 ),
    renal = as.numeric( q127 == 1 ),
    # indicador de grupo de risco segundo diferentes critérios de hipertensão arterial
    risco.comorb = as.numeric(
      obesidade_morbida == 1 | hipertensao == 1 | diabetes ==1 | card == 1 | 
        asma ==1 | pulmao ==1 | renal ==1),
    risco.comorb_ha2 = as.numeric(
      obesidade_morbida == 1 | ha_inst_med == 1 | diabetes ==1 | card == 1 | 
        asma ==1 | pulmao ==1 | renal ==1)
  )

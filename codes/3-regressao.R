# autor: Gabriel Mendes Borges
# data: 20/04/2020
# artigo (em coautoria com Claudio Crespo)

# Código para ajustar uma regressão logística, tendo como variável resposta a 
# presença de pelo menos uma pré-condição médica associada à COVID-19
# e variáveis explicativas características demográficas, socioeconômicas e regionais

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results/"))

# carrega código que lê e recodifica bases de dados
# source("../codes/le_recodifica_bases.R")

# modelo de regressão logística

#sexo e idade sem interação
glm_result_ha2_0 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat + c006,
         family=stats::quasibinomial(link='logit'), pns_design)

#sexo e idade com interação
glm_result_ha2_1 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006,
         family=stats::quasibinomial(link='logit'), pns_design)

anova1 <- anova(glm_result_ha2_0, glm_result_ha2_1,method='Wald')

#sexo, idade, Região
glm_result_ha2_2 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste'), 
         family=stats::quasibinomial(link='logit'), pns_design)

anova2 <- anova(glm_result_ha2_1, glm_result_ha2_2,method='Wald')

#sexo, idade, UF
glm_result_ha2_2b <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           uf, 
         family=stats::quasibinomial(link='logit'), pns_design)

anova(glm_result_ha2_2b); summary(glm_result_ha2_2b)
anova2b <- anova(glm_result_ha2_1, glm_result_ha2_2b,method='Wald')

#sexo, idade, Região, situação do domicilio
glm_result_ha2_2c <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste') + situacao,
         family=stats::quasibinomial(link='logit'), pns_design)

anova2c <- anova(glm_result_ha2_2, glm_result_ha2_2c,method='Wald')

#sexo, idade, Região, cor/raça
glm_result_ha2_3 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste')+
           relevel(raca, ref = 'Branca'),
         family=stats::quasibinomial(link='logit'), pns_design)

anova3 <- anova(glm_result_ha2_2, glm_result_ha2_3, method='Wald')

#sexo, idade, Região, cor/raça, escolaridade
glm_result_ha2_4 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste')+
           relevel(raca, ref = 'Branca')+
           relevel(educ, ref = 'Supc'),
         family=stats::quasibinomial(link='logit'), pns_design)

anova4 <- anova(glm_result_ha2_3, glm_result_ha2_4,method='Wald')

#sexo, idade, Região, cor/raça, escolaridade, força de trabalho
glm_result_ha2_5 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste')+
           relevel(raca, ref = 'Branca')+
           relevel(educ, ref = 'Supc') + forca
         ,
         family=stats::quasibinomial(link='logit'), pns_design)

anova5 <- anova(glm_result_ha2_4, glm_result_ha2_5,method='Wald')

#sexo, idade, Região, cor/raça, escolaridade, ocupada
glm_result_ha2_5b <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste')+
           relevel(raca, ref = 'Branca')+
           relevel(educ, ref = 'Supc') + ocupada
         ,
         family=stats::quasibinomial(link='logit'), pns_design)

anova5b <-anova(glm_result_ha2_4, glm_result_ha2_5b,method='Wald')

#sexo, idade, Região, cor/raça, escolaridade, força, ocupada
glm_result_ha2_6 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste')+
           relevel(raca, ref = 'Branca')+
           relevel(educ, ref = 'Supc') + forca+ocupada
         ,
         family=stats::quasibinomial(link='logit'), pns_design)

anova6 <- anova(glm_result_ha2_5, glm_result_ha2_6,method='Wald')

#sexo, idade, Região, cor/raça, escolaridade, força, plano
glm_result_ha2_7 <- 
  svyglm(risco.comorb_ha2 ~ idade_cat*c006+
           relevel(region, ref = 'Sudeste')+
           relevel(raca, ref = 'Branca')+
           relevel(educ, ref = 'Supc') + forca+plano
         ,
         family=stats::quasibinomial(link='logit'), pns_design)

anova7 <-anova(glm_result_ha2_5, glm_result_ha2_7,method='Wald')


########### ########### ########### ########### ###########
########### ###########  Modelo final ########### #########
########### ########### ########### ########### ###########

# mostra testes de inclusão de cada variável pelo teste de Wald

testes_significancia <- 
  bind_rows(anova1[3:7], anova2[3:7], 
            anova2c[3:7], anova3[3:7],
            anova4[3:7], anova5[3:7],
            anova6[3:7], anova7[3:7]) %>% 
  mutate(p = ifelse(p < 0.001,'< 0,001', 
                    formatC( p, format='f', digits=3, 
                             decimal.mark = ',')),
         Ftest = as.numeric(Ftest)) %>% 
  gt() %>% 
  cols_label(Ftest = 'Teste F', p = 'p-valor') %>% 
  fmt_number(columns = c('Ftest'), decimals = 2)

# Salva resultados dos testes em material suplementar
setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/material_suplementar"))

gtsave(testes_significancia, 'testes_significancia.rtf')
gtsave(testes_significancia, 'testes_significancia.html')

modelo_final <- glm_result_ha2_5
anova_modelo_final <- anova(modelo_final)
# Salva anova para modelo final em material suplementar
capture.output(anova_modelo_final,file='anova_modelofinal.txt')

modelofinal_gt <-  modelo_final %>% 
  tbl_regression(
    exponentiate = T, intercept = T,
    label = list('(Intercept)' ~ 'Intercepto', idade_cat ~ 'Grupos de idade', c006 ~ 'Sexo',
                 'relevel(region, ref = "Sudeste")' ~ 'Grande Região',
                 'relevel(educ, ref = "Supc")' ~ 'Nível educacional',
                 'relevel(raca, ref = "Branca")' ~ 'Cor ou raça',
                 forca ~ 'Condição em relação à força de trabalho')) %>%
  bold_labels() %>% 
  italicize_levels() %>% 
  as_gt(include = -tab_footnote) 

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results"))

# Salva exp(coef), ou Razão de Chances, estimados - Tabela 2 do artigo
modelofinal_gt %>% gtsave('modelofinal_gt.rtf')
modelofinal_gt %>% gtsave('modelofinal_gt.png')
modelofinal_gt %>% gtsave('modelofinal_gt.html')


#R squared
summ(glm_result_ha2_5)
psrsq(glm_result_ha2_5, type="Cox-Snell")
export_summs(glm_result_ha2_0, glm_result_ha2_1)

# autor: Gabriel Mendes Borges
# data: 20/04/2020
# artigo (em coautoria com Claudio Crespo)

# Este código calcula a Tabela 1 do artigo - prevalência de comorbidades por sexo e grupos de idade
# ... e outras tabelas descritivas

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd(system('git rev-parse --show-toplevel', intern=T))

# carrega código que lê e recodifica bases de dados
source("codes/1-le_recodifica_bases.R")

#tamanho da amostra das duas bases
sum( weights( pns_design , "sampling" ) != 0 )
sum( weights( pns_design_all , "sampling" ) != 0 )

count.dom <- as_tibble(pns_design_all$variables) %>% 
  mutate(chavedom = paste0(v0001, v0024, upa_pns, v0006_pns)) %>% 
  group_by(chavedom) %>% 
  summarise(dom.sum = sum(one))


# calcula proporção da população com hipertensão segundo diferentes critérios
# Estas tabela não entram no texto - vão para a pasta de material suplementar

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/material_suplementar"))

t_ha_sexo <- svy( ~ one, pns_design, svymean,na.rm = T, vartype = 'ci')

# por sexo
t_ha_sexo <- svyby( ~ hipertensao+ha_inst+ha_inst_med, ~c006, 
                    pns_design, svymean,na.rm = T, vartype = 'ci')

t2_ha_sexo <- tibble(t_ha_sexo) %>%
  mutate_if(is.numeric, ~formatC( round(., 3)*100, format='f', digits=1, decimal.mark = ',')) %>% 
  mutate(hip = paste0(hipertensao, ' [', ci_l.hipertensao, '; ',ci_u.hipertensao, ']'),
         ha_inst = paste0(ha_inst, ' [', ci_l.ha_inst, '; ',ci_u.ha_inst, ']'),
         ha_inst_med = paste0(ha_inst_med, ' [', ci_l.ha_inst_med, '; ',ci_u.ha_inst_med, ']')) %>% 
  select(c006, hip, ha_inst, ha_inst_med)

tosave_t_ha_sexo <- t2_ha_sexo  %>% gt() %>% 
  cols_label(c006 = 'sexo',hip = 'hipertensão autorreferida', 
             ha_inst = 'hipertensão medida por instrumento',
             ha_inst_med = 'hipertensão medida por instrumento ou uso de medicamentos') %>% 
  tab_header(title = 'Proporção da população com hipertensão 
             arterial (IC 95%) por sexo segundo diferentes') 
# salva resultados em material_suplementar
gtsave(tosave_t_ha_sexo, 't_ha_sexo.rtf')
gtsave(tosave_t_ha_sexo, 't_ha_sexo.html')
# resultados são consistentes com os encontrados por Malta et al (2018)

# por idade
t_ha_idade <- svyby( ~ hipertensao+ha_inst+ha_inst_med, ~idade_cat, 
                     pns_design, svymean,na.rm = T, vartype = 'ci')

t2_ha_idade <- tibble(t_ha_idade) %>%
  mutate_if(is.numeric, ~formatC( round(., 3)*100, format='f', digits=1, decimal.mark = ',')) %>% 
  mutate(hip = paste0(hipertensao, ' [', ci_l.hipertensao, '; ',ci_u.hipertensao, ']'),
         ha_inst = paste0(ha_inst, ' [', ci_l.ha_inst, '; ',ci_u.ha_inst, ']'),
         ha_inst_med = paste0(ha_inst_med, ' [', ci_l.ha_inst_med, '; ',ci_u.ha_inst_med, ']')) %>% 
  select(idade_cat, hip, ha_inst, ha_inst_med)

tosave_t_ha_idade <- t2_ha_idade  %>% gt() %>% 
  cols_label(idade_cat = 'idade',hip = 'hipertensão autorreferida', 
             ha_inst = 'hipertensão medida por instrumento',
             ha_inst_med = 'hipertensão medida por instrumento ou uso de medicamentos') %>% 
  tab_header(title = 'Proporção da população com hipertensão 
             arterial (IC 95%) por idade segundo diferentes') 
# salva resultados em material_suplementar
gtsave(tosave_t_ha_idade, 't_ha_idade.rtf')
gtsave(tosave_t_ha_idade, 't_ha_idade.html')

# tabela de prevalência de comorbidades por sexo e idade 
# critério de hipertensão arterial autorreferida
# é a tabela equivalente à Tabela 1, mudando somente o critério de hipertensão
# Esta tabela não entra no texto - vai para material suplementar
comorb <- svyby(~obesidade_morbida+hipertensao+diabetes+card+asma+pulmao+renal+risco.comorb, 
                ~idade_cat+c006, pns_design , svymean,na.rm = T,
                vartype=c('ci'))


t_comorb <- tibble(comorb) %>%
  mutate_if(is.numeric, ~formatC( round(., 3)*100, format='f', digits=1, decimal.mark = ',')) %>% 
  mutate(
    obesidade_morbida = paste0(obesidade_morbida, ' [', ci_l.obesidade_morbida, '; ',ci_u.obesidade_morbida, ']'),
    hipertensao = paste0(hipertensao, ' [', ci_l.hipertensao, '; ',ci_u.hipertensao, ']'),
    diabetes = paste0(diabetes, ' [', ci_l.diabetes, '; ',ci_u.diabetes, ']'),
    card = paste0(card, ' [', ci_l.card, '; ',ci_u.card, ']'),
    asma = paste0(asma, ' [', ci_l.asma, '; ',ci_u.asma, ']'),
    pulmao = paste0(pulmao, ' [', ci_l.pulmao, '; ',ci_u.pulmao, ']'),
    renal = paste0(renal, ' [', ci_l.renal, '; ',ci_u.renal, ']'),
    risco.comorb = paste0(risco.comorb, ' [', ci_l.risco.comorb, '; ',ci_u.risco.comorb, ']')
  ) %>% 
  select(idade_cat, obesidade_morbida, hipertensao, diabetes, card, asma, pulmao, renal, risco.comorb)

tosave_t_comorb <- t_comorb  %>% gt() %>% 
  cols_label(idade_cat = 'Sexo/Idade',
             hipertensao = 'Hipertensão arterial autorreferida', 
             obesidade_morbida = 'Obesidade mórbida',
             diabetes = 'Diabetes',
             card = 'Doença do coração',
             asma = 'Asma',
             pulmao = 'Doença pulmonar',
             renal = 'Insuficiência renal crônica',
             risco.comorb = 'Pelo menos uma comorbidade'
  ) %>% 
  tab_row_group(group = 'homens',rows = 1:9) %>%
  tab_row_group(group = 'mulheres',rows = 10:18) %>%
  tab_header(title = 'Proporção de pessoas (e intervalos de confiança) com morbidades 
             e fatores de risco associados a COVID-19 por grupos de idade – Brasil, 2013',
             subtitle = "Hipertensão arterial autorreferida") 

gtsave(tosave_t_comorb, 't_comorb.rtf')
gtsave(tosave_t_comorb, 't_comorb.html')

# tabela de prevalência de comorbidades por sexo e idade 
# critério de hipertensão arterial por instrumento ou medicamento
# Tabela 1 do artigo - salva na pasta de resultados

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results"))

comorb_ha2 <- svyby(~obesidade_morbida+ha_inst_med+diabetes+card+asma+pulmao+renal+risco.comorb_ha2, 
                    ~idade_cat+c006, pns_design , svymean,na.rm = T,
                    vartype=c('ci'))

svymean(~obesidade_morbida+ha_inst_med+diabetes+card+asma+pulmao+renal+risco.comorb_ha2, 
                  pns_design , na.rm = T)


t_comorb_ha2 <- tibble(comorb_ha2) %>%
  mutate_if(is.numeric, ~formatC( round(., 3)*100, format='f', digits=1, decimal.mark = ',')) %>% 
  mutate(
    obesidade_morbida = paste0(obesidade_morbida, ' [', ci_l.obesidade_morbida, '; ',ci_u.obesidade_morbida, ']'),
    ha_inst_med = paste0(ha_inst_med, ' [', ci_l.ha_inst_med, '; ',ci_u.ha_inst_med, ']'),
    diabetes = paste0(diabetes, ' [', ci_l.diabetes, '; ',ci_u.diabetes, ']'),
    card = paste0(card, ' [', ci_l.card, '; ',ci_u.card, ']'),
    asma = paste0(asma, ' [', ci_l.asma, '; ',ci_u.asma, ']'),
    pulmao = paste0(pulmao, ' [', ci_l.pulmao, '; ',ci_u.pulmao, ']'),
    renal = paste0(renal, ' [', ci_l.renal, '; ',ci_u.renal, ']'),
    risco.comorb_ha2 = paste0(risco.comorb_ha2, ' [', ci_l.risco.comorb_ha2, '; ',ci_u.risco.comorb_ha2, ']')
  ) %>% 
  select(idade_cat, obesidade_morbida, ha_inst_med, diabetes, card, asma, pulmao, renal, risco.comorb_ha2)

tosave_t_comorb_ha2 <- t_comorb_ha2  %>% gt() %>% 
  cols_label(idade_cat = 'Sexo/Idade',
             ha_inst_med = 'Hipertensão arterial medida por instrumento ou uso de medicamentos', 
             obesidade_morbida = 'Obesidade mórbida',
             diabetes = 'Diabetes',
             card = 'Doença do coração',
             asma = 'Asma',
             pulmao = 'Doença pulmonar',
             renal = 'Insuficiência renal crônica',
             risco.comorb_ha2 = 'Pelo menos uma comorbidade'
  ) %>% 
  tab_row_group(group = 'homens',rows = 1:9) %>%
  tab_row_group(group = 'mulheres',rows = 10:18) %>%
  tab_header(title = 'Proporção de pessoas (e intervalos de confiança) com morbidades 
             e fatores de risco associados a COVID-19 por grupos de idade – Brasil, 2013',
             subtitle = 'Hipertensão arterial medida por instrumento ou uso de medicamentos') 

gtsave(tosave_t_comorb_ha2, 't_comorb_ha2.rtf')
gtsave(tosave_t_comorb_ha2, 't_comorb_ha2.html')


# Gera gráfico para prevalência de pelo menos uma condição associada à COVID-19
to_plot_comorb_ha2 <- tibble(comorb_ha2) %>% 
  mutate(idade_cat = factor(idade_cat, levels = unique(idade_cat)),
         sexo = c006)

comorb_ha2 <- ggplot(to_plot_comorb_ha2, aes(x=idade_cat, y=risco.comorb_ha2*100,  color=sexo, group=sexo)) +
  theme_bw(base_size = 14) + geom_point() + expand_limits(y = 0)+
  geom_line(aes(x=idade_cat, y=risco.comorb_ha2*100,  color=sexo, group=sexo), size=1) +
  geom_line(aes(x=idade_cat, y=ci_u.risco.comorb_ha2*100,  color=sexo, group=sexo), size=0.01) +
  geom_line(aes(x=idade_cat, y=ci_u.risco.comorb_ha2*100,  color=sexo, group=sexo), size=0.01) +
  geom_ribbon(aes(x=idade_cat,ymin=ci_l.risco.comorb_ha2*100,ymax=ci_u.risco.comorb_ha2*100, colour = sexo), 
              alpha=0.1)+
  scale_y_continuous(breaks = seq(0,100, by= 10)) + 
  scale_color_manual(labels = c("homens", "mulheres"), values = c("blue", "red")) +
  labs(x = "idade", y= "Prevalência (%)") +
  facet_grid( ~ sexo) +
  theme(legend.position = "none") 

ggsave("comorb_ha2.png",comorb_ha2, width = 13, height = 7, device = "png")
ggsave("comorb_ha2.eps",comorb_ha2, width = 13, height = 7, device = "eps")

# prevalência para todas as idades por sexo
comorb_ha2_sexo <- svyby(~obesidade_morbida+ha_inst_med+diabetes+card+asma+pulmao+renal+risco.comorb_ha2, 
                          ~c006, pns_design , svymean,na.rm = T,
                          vartype=c('ci'))

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/material_suplementar"))

saveRDS(object = comorb_ha2_sexo, file = "comorb_ha2_sexo.rds")

tosave_t_comorb_ha2_sexo <- tibble(comorb_ha2_sexo) %>%
  mutate_if(is.numeric, ~formatC( round(., 3)*100, format='f', digits=1, decimal.mark = ',')) %>% 
  mutate(
    obesidade_morbida = paste0(obesidade_morbida, ' [', ci_l.obesidade_morbida, '; ',ci_u.obesidade_morbida, ']'),
    ha_inst_med = paste0(ha_inst_med, ' [', ci_l.ha_inst_med, '; ',ci_u.ha_inst_med, ']'),
    diabetes = paste0(diabetes, ' [', ci_l.diabetes, '; ',ci_u.diabetes, ']'),
    card = paste0(card, ' [', ci_l.card, '; ',ci_u.card, ']'),
    asma = paste0(asma, ' [', ci_l.asma, '; ',ci_u.asma, ']'),
    pulmao = paste0(pulmao, ' [', ci_l.pulmao, '; ',ci_u.pulmao, ']'),
    renal = paste0(renal, ' [', ci_l.renal, '; ',ci_u.renal, ']'),
    risco.comorb_ha2 = paste0(risco.comorb_ha2, ' [', ci_l.risco.comorb_ha2, '; ',ci_u.risco.comorb_ha2, ']')
  ) %>% 
  select(c006, obesidade_morbida, ha_inst_med, diabetes, card, asma, pulmao, renal, risco.comorb_ha2) %>% 
  gt() %>% 
  cols_label(c006 = 'Sexo', 
             ha_inst_med = 'Hipertensão arterial', 
             obesidade_morbida = 'Obesidade mórbida',
             diabetes = 'Diabetes',
             card = 'Doença do coração',
             asma = 'Asma',
             pulmao = 'Doença pulmonar',
             renal = 'Insuficiência renal',
             risco.comorb_ha2 = 'Pelo menos uma comorbidade') %>% 
  cols_align(align = "center")
  # tab_row_group(group = 'homens',rows = 1) %>%
  # tab_row_group(group = 'mulheres',rows = 2)

gtsave(tosave_t_comorb_ha2_sexo, 't_comorb_ha2_sexo.html')
gtsave(tosave_t_comorb_ha2_sexo, 't_comorb_ha2_sexo.pdf')
gtsave(tosave_t_comorb_ha2_sexo, 't_comorb_ha2_sexo.png',
       expand = 100)


# Gera tabela descritiva 

#idade
desc_idade_cat_1 <- as_tibble(svyby(~one, ~idade_cat, pns_design, na.rm = T,unwtd.count))[,1:2]
desc_idade_cat_2 <- as_tibble(svytotal(~idade_cat, pns_design , na.rm = T))[,1]
desc_idade_cat_3 <- as_tibble(svyby(~risco.comorb_ha2, ~idade_cat, pns_design, svymean, na.rm = T, vartype=c('ci')))[,2:4]
desc_idade_cat <- bind_cols(desc_idade_cat_1, desc_idade_cat_2, desc_idade_cat_3) %>% 
  rename(var = idade_cat)

#sexo
desc_c006_1 <- as_tibble(svyby(~one, ~c006, pns_design, na.rm = T,unwtd.count))[,1:2]
desc_c006_2 <- as_tibble(svytotal(~c006, pns_design , na.rm = T))[,1]
desc_c006_3 <- as_tibble(svyby(~risco.comorb_ha2, ~c006, pns_design, svymean, na.rm = T, vartype=c('ci')))[,2:4]
desc_c006 <- bind_cols(desc_c006_1, desc_c006_2, desc_c006_3) %>% 
  rename(var = c006)

#região
desc_region_1 <- as_tibble(svyby(~one, ~region, pns_design, na.rm = T,unwtd.count))[,1:2]
desc_region_2 <- as_tibble(svytotal(~region, pns_design , na.rm = T))[,1]
desc_region_3 <- as_tibble(svyby(~risco.comorb_ha2, ~region, pns_design, svymean, na.rm = T, vartype=c('ci')))[,2:4]
desc_region <- bind_cols(desc_region_1, desc_region_2, desc_region_3) %>% 
  rename(var = region)

#raça
desc_raca_1 <- as_tibble(svyby(~one, ~raca, pns_design, na.rm = T,unwtd.count))[,1:2]
desc_raca_2 <- as_tibble(svytotal(~raca, pns_design , na.rm = T))[,1]
desc_raca_3 <- as_tibble(svyby(~risco.comorb_ha2, ~raca, pns_design, svymean, na.rm = T, vartype=c('ci')))[,2:4]
desc_raca <- bind_cols(desc_raca_1, desc_raca_2, desc_raca_3) %>% 
  rename(var = raca) %>% filter(var!="Ignorado")

#escolaridade
desc_educ_1 <- as_tibble(svyby(~one, ~educ, pns_design, na.rm = T,unwtd.count))[,1:2]
desc_educ_2 <- as_tibble(svytotal(~educ, pns_design , na.rm = T))[,1]
desc_educ_3 <- as_tibble(svyby(~risco.comorb_ha2, ~educ, pns_design, svymean, na.rm = T, vartype=c('ci')))[,2:4]
desc_educ <- bind_cols(desc_educ_1, desc_educ_2, desc_educ_3) %>% 
  rename(var = educ)

#forca
desc_forca_1 <- as_tibble(svyby(~one, ~forca, pns_design, na.rm = T,unwtd.count))[,1:2]
desc_forca_2 <- as_tibble(svytotal(~forca, pns_design , na.rm = T))[,1]
desc_forca_3 <- as_tibble(svyby(~risco.comorb_ha2, ~forca, pns_design, svymean, na.rm = T, vartype=c('ci')))[,2:4]
desc_forca <- bind_cols(desc_forca_1, desc_forca_2, desc_forca_3) %>% 
  rename(var = forca)

desc_all <- 
  bind_rows(desc_idade_cat, desc_c006, desc_region, desc_raca, desc_educ, desc_forca)

t_desc <- desc_all %>% 
  mutate(
    risco.comorb_ha2 = formatC(round(risco.comorb_ha2, 3)*100, format='f', digits=1, decimal.mark = ','),
    ci_l = formatC(round(ci_l, 3)*100, format='f', digits=1, decimal.mark = ','),
    ci_u = formatC(round(ci_u, 3)*100, format='f', digits=1, decimal.mark = ',')
    ) %>% 
  mutate_if(is.numeric, ~formatC( round(., 0), format='f', digits=0, decimal.mark = ',', big.mark=".")) %>% 
  mutate(risco.comorb_ha2 = paste0(risco.comorb_ha2, ' [', ci_l, '; ',ci_u, ']')) %>% 
  select(-ci_l, -ci_u)

tosave_t_desc <- t_desc  %>% gt() %>% 
  cols_label(var = 'Categorias',
             counts = 'Amostra (n)', 
             total = 'População estimada',
             risco.comorb_ha2 = 'Proporção da população com menos uma condição pré-existente'
  ) %>% 
  tab_row_group(group = 'Condição em relação à força de trabalho',rows = 26:27) %>%
  tab_row_group(group = 'Nível educacional',rows = 22:25) %>%
  tab_row_group(group = 'Cor ou raça',rows = 17:21) %>%
  tab_row_group(group = 'Grande Região',rows = 12:16) %>%
  tab_row_group(group = 'Sexo',rows = 10:11) %>%
  tab_row_group(group = 'Grupos de idade', rows = 1:9) %>% 
  tab_header(title = 'Proporção de pessoas (e intervalos de confiança) com morbidades 
             e fatores de risco associados a COVID-19 por grupos de idade – Brasil, 2013',
             subtitle = 'Hipertensão arterial medida por instrumento ou uso de medicamentos') 

gtsave(tosave_t_desc, 'tosave_t_desc.rtf')
gtsave(tosave_t_desc, 'tosave_t_desc.html')

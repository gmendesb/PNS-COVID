# autor: Gabriel Mendes Borges
# data: 20/04/2020
# artigo (em coautoria com Claudio Crespo)

# Código para estimar pertencimento ao grupo de risco para 
# todos os moradores da PNS entrevistados com base nos coeficientes estimados 
# da regressão logística ajustada

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results/"))

# carrega código que lê e recodifica bases de dados
# source("../codes/le_recodifica_bases.R")

# prepara base para estimativa do número de pessoas moradoras do mesmo 
# domicílio que pessoas no grupo de risco
base_18_mais <- as_tibble(pns_design_all$variables) %>% filter(idade_cat!='0-18')

base_18_mais$forca <- droplevels(base_18_mais$forca)

# Estima valores preditos com base nas variáveis utilizadas no modelo final
valores_preditos_18_mais <- predict.glm(modelo_final, base_18_mais,  type = 'response')

# Estima valores preditos com base nas variáveis utilizadas no modelo final
# set seed
set.seed(sum(utf8ToInt("COVID-19")))

#cria variável indicadora de pertencimento ao grupo de risco para cada indivíduo
base_18_mais <- base_18_mais %>% 
  mutate(chavedom = paste0(v0001 , v0024 , upa_pns , v0006_pns),
         prob_risco = as.vector(valores_preditos_18_mais),
         risco_ajustado =  rbinom(length(prob_risco), 1, prob = prob_risco),
         risco_ajustado = ifelse(idade_cat == '60+', 1, risco_ajustado))

#cria variável indicadora de existência de pelo menos uma 
#pessoa no grupo de risco no domicílio
base_dom <- base_18_mais %>% group_by(chavedom) %>% 
  summarise(sum_risco = sum(risco_ajustado)) %>% 
  mutate(sum_risco = ifelse(sum_risco >=1 , 1, 0))

# adiciona variável chave do domicílio
pns_design_all <- 
  update(pns_design_all, chavedom = paste0(v0001 , v0024 , upa_pns , v0006_pns))

# concatena base pns_design_all com base contendo 
# indicador de pertenciamento ao grupo de risco
pns_design_all_2 <- pns_design_all
pns_design_all_2$variables <- left_join(pns_design_all$variables, base_dom)

#Calcula percentual de pessoas vivendo com outras pessoas no grupo de risco - Brasil
prop_pess_risco_tot <- svymean( ~ sum_risco, pns_design_all_2, na.rm = T)
round(mean(prop_pess_risco_tot)*100,1)
round(confint(prop_pess_risco_tot)*100, 1)

#Calcula percentual de pessoas vivendo com outras pessoas no grupo de risco por UF
prop_pess_risco <- svyby( ~ sum_risco, ~uf, pns_design_all_2, svymean,na.rm = T,
                          vartype=c('ci'))

uf.cod <- c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE",
            "RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR",
            "SC","RS","MS","MT","GO","DF")

g_prop_pess_risco <- 
  ggplot(prop_pess_risco, aes(y=sum_risco*100, x = uf)) + 
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100), width=.2) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(breaks = seq(0,100, by= 10)) +
  scale_x_discrete(labels = uf.cod) + 
  labs(x = "Unidade da Federação", y= 
         "Proporção de pessoas convivendo com morador no grupo de risco (%)")

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results"))
ggsave("g_prop_pess_risco.eps",g_prop_pess_risco, width = 10, height = 6, device = "eps")


t_prop_pess_risco <- tibble(prop_pess_risco) %>%
  mutate_if(is.numeric, ~formatC( round(., 3)*100, format='f', digits=1, decimal.mark = ',')) %>% 
  mutate(sum_risco = paste0(sum_risco, ' [', ci_l, '; ',ci_u, ']')) %>% 
  select(uf, sum_risco)

tosave_t_prop_pess_risco <- t_prop_pess_risco  %>% gt() %>% 
  cols_label(
    uf = 'Unidade da Federação',
    sum_risco = 'Proporção de pessoas (%)') 

tosave_t_prop_pess_risco

gtsave(tosave_t_prop_pess_risco, 'tosave_t_prop_pess_risco.rtf')
gtsave(tosave_t_prop_pess_risco, 'tosave_t_prop_pess_risco.html')
gtsave(tosave_t_prop_pess_risco, 'tosave_t_prop_pess_risco.png')

# salvar em duas tabelas separadas para apresentação
setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/material_suplementar"))
tosave_t_prop_pess_riscoUF1 <- t_prop_pess_risco[1:16,]  %>% gt() %>% 
  cols_label(uf = 'Unidade da Federação',sum_risco = 'Proporção de pessoas (%)') 

tosave_t_prop_pess_riscoUF2 <- t_prop_pess_risco[17:27,]  %>% gt() %>% 
  cols_label(uf = 'Unidade da Federação',sum_risco = 'Proporção de pessoas (%)') 

gtsave(tosave_t_prop_pess_riscoUF1, 'tosave_t_prop_pess_riscoUF1.png')
gtsave(tosave_t_prop_pess_riscoUF2, 'tosave_t_prop_pess_riscoUF2.png')

#checa total da população
svytotal( ~one, pns_design_all_2, na.rm = T)

# filtra responsáveis para calcular indicadores de domicílios
pns_design_resp <- subset( pns_design_all_2 , c004 == "01" )
svytotal(~one, pns_design_resp, na.rm = T)
svytotal(~as.numeric(v0022), pns_design_resp, na.rm = T)
svyratio( numerator = ~ as.numeric(v0022) , denominator = ~ one , 
          subset( pns_design_resp , sum_risco == 1 ) ,na.rm = TRUE)
svyratio( numerator = ~ as.numeric(v0022) , denominator = ~ one , 
          subset( pns_design_resp , sum_risco == 0 ) ,na.rm = TRUE)

to_pyramid <- t(svyby( ~c008 , ~sum_risco, pns_design_all_2, svytotal,na.rm = T))
to_pyramid_ci <- t(svyby( ~sum_risco , ~c008, pns_design_all_2, svymean,na.rm = T,
                          vartype=c('ci')))

to_pyramid_data <- tibble(idade = c(0:99,0:99), 
       pop = c(unlist(to_pyramid[2:101,1]),
               unlist(to_pyramid[2:101,2])),
       risco = rep(c(0,1), each = 100))




to_pyramid_plot <- 
  ggplot(to_pyramid_data, aes(x=idade, y=pop/1000000,  
                              color=as.character(risco), 
                              group=as.character(risco))) +
  theme_bw(base_size = 14) + geom_line() + expand_limits(y = 0)+
  scale_x_continuous(breaks = seq(0,100, by= 10)) + 
  scale_color_manual(labels = c("morador em domicílio sem pessoas no grupo de risco",
                                "morador em domicílio com pessoas no grupo de risco"),
                     values = c("blue", "red")) +
  labs(x = "idade", y= "População (em milhões)") +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.text=element_text(size=9),  
        legend.title=element_blank())

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results"))
ggsave("to_pyramid_plot.eps",to_pyramid_plot, width = 8, height = 5, device = "eps")

to_pyramid_prop <- to_pyramid_data %>%
  pivot_wider(names_from = risco, names_prefix = 'G',values_from = pop) %>% 
  mutate(prop = G1/(G0+G1)*100)



to_pyramid_prop_ci <- tibble(
  idade = as.numeric(unlist(to_pyramid_ci[1, 1:108])), 
  prop = as.numeric(unlist(to_pyramid_ci[2, 1:108]))*100,
  prop_ci_l = as.numeric(unlist(to_pyramid_ci[3, 1:108]))*100,
  prop_ci_u = as.numeric(unlist(to_pyramid_ci[4, 1:108]))*100)

to_pyramid_prop_plot_ci <- 
  ggplot(filter(to_pyramid_prop_ci, idade <=60)) +
  geom_line(aes(x=idade, y=prop), size=1) +
  theme_bw(base_size = 14) + expand_limits(y = 0)+
  scale_y_continuous(breaks = seq(0,100, by= 10))+ 
  scale_x_continuous(breaks = seq(0,60, by= 5),
                     labels = c(seq(0,55, by= 5), "60+" )) + 
  geom_line(aes(x=idade, y=prop_ci_l), size=0.1, linetype=2) +
  geom_line(aes(x=idade, y=prop_ci_u), size=0.1, linetype=2) +
  geom_ribbon(aes(x=idade,ymin=prop_ci_l,ymax=prop_ci_u), 
              alpha=0.1)+
  labs(x = "idade", y= "Proporção de pessoas convivendo com morador no grupo de risco (%)")

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/results"))
ggsave("to_pyramid_prop_plot_ci.eps",to_pyramid_prop_plot_ci, width = 12, height = 8, device = "eps")
ggsave("to_pyramid_prop_plot_ci.png",to_pyramid_prop_plot_ci, width = 12, height = 8, device = "png")

to_pyramid_prop_plot_ci <- 
  ggplot(filter(to_pyramid_prop_ci, idade <=60), aes(x=idade, y=prop)) +
  theme_bw(base_size = 14) + geom_line() + expand_limits(y = 0)+
  scale_y_continuous(breaks = seq(0,100, by= 10))+ 
  scale_x_continuous(breaks = seq(0,60, by= 5),
                     labels = c(seq(0,55, by= 5), "60+" )) + 
  labs(x = "idade", y= "Proporção (%)")

setwd(paste0(system('git rev-parse --show-toplevel', intern=T),"/material_suplementar"))
ggsave("to_pyramid_prop_plot.eps",to_pyramid_prop_plot, width = 12, height = 8, device = "eps")
ggsave("to_pyramid_prop_plot.png",to_pyramid_prop_plot, width = 12, height = 8, device = "png")


# Calcula proporção de domicílios com idosos na base da PNS
calcula_prop_idoso <- 
  pns_design_all$variables %>% dplyr::select(idade_cat, idoso, chavedom, v0028) %>%
  group_by(chavedom, v0028) %>% summarise(idoso = sum(idoso)) %>% 
  mutate(idoso = ifelse(idoso>0,1,0))

calcula_prop_idoso %>% group_by(idoso) %>% summarise(sum(v0028))
#16733206/(16733206+38513613)*100


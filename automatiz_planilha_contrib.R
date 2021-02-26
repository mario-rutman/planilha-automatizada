library(pacman)
pacman::p_load(tidyverse, DT, readxl, purrr, lubridate)

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
options(scipen=999, digits=3)

# 1. Antes de importar a planilha excluir as colunas que não interessam.
# 2. Importar a planilha em excel.

lista_afre <- read_excel("Ação Junho de 2019.xlsx", 
                         sheet = "Distribuição")

lista_cnpj <- read_excel("Ação Junho de 2019.xlsx", 
                         sheet = "Analítica", col_types = c("numeric", 
                                                            "numeric", "numeric", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric"))

lista_contribuintes_junho <- inner_join(lista_afre, lista_cnpj, by = c("raiz" = "raiz"))


# Só para fazer uma conferência. Resultado OK.
# lista_contribuintes_junho %>% group_by(raiz) %>% summarise(ttt = sum(Total))

# 3. Limpar e arrumar lista_contribuintes.

lc_so_cnpj <- lista_contribuintes_junho %>% 
  # Fazendo a reunião/unpivoting table.
  gather(key = "tributo_natureza", value = "valor", c(8:11), na.rm = TRUE,
         convert = FALSE, factor_key = FALSE) %>% 
  # retirando as observações com valor zero.
  filter(valor != 0.00) %>%
  # tranformando RAIZ e tributo_natureza em factor, para usar forcats.
  # mudando o nome das categorias.
  mutate(tributo_natureza = fct_recode(tributo_natureza,
                                       'ICMS/OP' = "VL_SAL_DEVEDOR_FINAL_ICMS_OP",
                                       'ICMS/ST' =  "VL_SAL_DEVEDOR_FINAL_ICMS_ST",
                                       'FECP/OP' = "VL_SAL_DEVEDOR_FINAL_FECP_OP",
                                       'FECP/ST' = "VL_SAL_DEVEDOR_FINAL_FECP_ST")) %>% 
  mutate(ME_REFERENCIA = fct_recode(ME_REFERENCIA, "Jan" = "1", "Fev" = "2", "Mar" ="3", 
                                    "Abr" = "4",
                                    "Mai" = "5", 'Jun' = "6", 'Jul' = "7", 'Ago' ="8",
                                    'Set' ="9", 'Out' = "10", 'Nov' = "11", 'Dez' = "12")) %>% 
  mutate(ME_REFERENCIA = fct_relevel(ME_REFERENCIA, "Jan", "Fev", "Mar", "Abr", "Mai", "Jun", 
                                     "Jul", "Ago", "Set", "Out", "Nov", "Dez")) %>% 
  arrange(NU_INSCRICAO, AN_REFERENCIA, ME_REFERENCIA) %>% 
  select(c(7,4,5,6,9,10,1,3))


# 3.1 Fazendo um gráfico de barras que virá antes de planilha propriamente dita.

graf_01 <- lista_contribuintes_junho %>%  # botando zeros nos meses. Para então fazer data.
  mutate(ME_REFERENCIA = str_pad(ME_REFERENCIA, width=2, side="left", pad="0")) %>% 
  # Fazendo a reunião/unpivoting table.
  gather(key = "tributo_natureza", value = "valor", c(8:11), na.rm = TRUE,
         convert = FALSE, factor_key = FALSE) %>% 
  # retirando as observações com valor zero.
  filter(valor != 0.00) %>%
  # unindo ano e mês.
  unite("ano_mes", c("AN_REFERENCIA", "ME_REFERENCIA"), sep = "-", remove = TRUE) %>% 
  # tranformando RAIZ e tributo_natureza em factor, para usar forcats.
  # mudando o nome das categorias.
  mutate(tributo_natureza = fct_recode(tributo_natureza,
                                       'ICMS/OP' = "VL_SAL_DEVEDOR_FINAL_ICMS_OP",
                                       'ICMS/ST' =  "VL_SAL_DEVEDOR_FINAL_ICMS_ST",
                                       'FECP/OP' = "VL_SAL_DEVEDOR_FINAL_FECP_OP",
                                       'FECP/ST' = "VL_SAL_DEVEDOR_FINAL_FECP_ST")) %>% 
  select(c(1,6,4,5,8,9)) %>%  # filtar a raiz específica
  # transformando ano_mes em data.
  mutate(data = ymd(ano_mes, truncated = 1L)) %>% # Agrupando para somar por data e tributo_natureza.
  group_by(raiz, NO_RAZAO, data, tributo_natureza) %>% summarize(total = sum(valor))

# Para fazer as contagens de total, número de ocorrências, mdébito mais antigo, novo ou média de idade, 
# vou precisar de outro DF, será o contagens.

contagens <- lista_contribuintes_junho %>%  
  # botando zeros nos meses. Para então fazer data.
  mutate(ME_REFERENCIA = str_pad(ME_REFERENCIA, width=2, side="left", pad="0")) %>% 
  # Fazendo a reunião/unpivoting table.
  gather(key = "tributo_natureza", value = "valor", c(8:11), na.rm = TRUE,
         convert = FALSE, factor_key = FALSE) %>% 
  # retirando as observações com valor zero.
  filter(valor != 0.00) %>%
  # unindo ano e mês.
  unite("ano_mes", c("AN_REFERENCIA", "ME_REFERENCIA"), sep = "-", remove = TRUE) %>%
  # transformando em data
  mutate(data = ymd(ano_mes, truncated = 1L)) %>% 
  # tranformando RAIZ e tributo_natureza em factor, para usar forcats.
  # mudando o nome das categorias.
  mutate(tributo_natureza = fct_recode(tributo_natureza,
                                       'ICMS/OP' = "VL_SAL_DEVEDOR_FINAL_ICMS_OP",
                                       'ICMS/ST' =  "VL_SAL_DEVEDOR_FINAL_ICMS_ST",
                                       'FECP/OP' = "VL_SAL_DEVEDOR_FINAL_FECP_OP",
                                       'FECP/ST' = "VL_SAL_DEVEDOR_FINAL_FECP_ST")) %>% 
  select(c(1,6,4,10,8,9)) 

# 4. Salvando os arquivos principais. Os que vão para o .Rmd.
saveRDS(lc_so_cnpj, file = "lc_so_cnpj.rds")
saveRDS(graf_01, file = "graf_01.rds")
saveRDS(contagens, file = "contagens.rds")

# ggplot(data = graf_01, aes(x = Class, y = total, fill = tributo_natureza)) + geom_col() 

# 5. Objetos para fazer o Rmarkdown com a raíz do CNPJ como parâmetro.

# 5.1 MILAGRE!!! Para colocar vírgulas entre os elementos de um vetor.
paste(sort(unique(lc_so_cnpj$raiz)), collapse = ", ")

# 5.1.1 Vai agora ao .Rmd, cola esse monte de raízes de CNPJ nos parâmetros e testa.


# 5.2 FUNDAMENTAL! A função que vai dar como resultado as 330 planilhas.
render_report_cnpj = function(cnpj) {
  rmarkdown::render(
    "lc_so_cnpj.Rmd", params = list(
      cnpj = cnpj
    ),
    output_file = paste0(cnpj, " jun-2019 ", ".html")
  )
}


# 5.3 O objeto que vai ser aplicado à função. Para fazer o params list. 
paste(sort(unique(lc_so_cnpj$raiz)), collapse = "', '")

params_list_lc <- list(list('19357', '73562', '159233', '285753', '286550', '308178', '468285', '507051', '994786', '1002664', '1342499', '1374269', '1380766', '1438784', '1489122', '1635700', '1789077', '1859823', '1878026', '2074194', '2166846', '2220900', '2239104', '2293899', '2374708', '2411997', '2538878', '2653803', '2660817', '2686151', '2710548', '2714518', '2735229', '2737654', '2829167', '2867613', '2881046', '2895077', '2903357', '3021596', '3029129', '3129519', '3245775', '3246317', '3319508', '3341775', '3375396', '3391094', '3419953', '3530117', '3629159', '3666817', '3752385', '3848700', '4133843', '4178625', '4455547', '4558449', '4603117', '4616370', '4624285', '4671837', '4804307', '4809564', '4809906', '4832585', '4871807', '5013234', '5013856', '5020927', '5027195', '5061231', '5068583', '5098385', '5099739', '5103939', '5132549', '5217376', '5400058', '5411176', '5513322', '5550628', '5660723', '5699139', '5931640', '6052196', '6073075', '6245870', '6248349', '6371763', '6877590', '6954219', '6965885', '7065435', '7084532', '7123390', '7298492', '7418875', '7441212', '7452156', '7465184', '7476685', '7603478', '7625852', '7632090', '7681643', '7716156', '7790530', '7831569', '8226398', '8429004', '8488511', '8647462', '8768016', '8771920', '8888939', '9044235', '9079952', '9088251', '9121078', '9212113', '9296436', '9385405', '9424604', '10275347', '10284459', '10296393', '10312737', '10438370', '10440802', '10455746', '10481246', '10516166', '10527681', '10615571', '10637416', '10645360', '10679738', '10740619', '10839911', '10859506', '10901378', '10993694', '11047649', '11197210', '11204759', '11253257', '11262055', '11277541', '11281477', '11358292', '11368424', '11510832', '11624355', '11752753', '11806723', '11816308', '11908151', '11915401', '11931349', '11984193', '12016067', '12028528', '12052652', '12185533', '12322686', '12400126', '12406134', '12449565', '12506155', '12521162', '12531081', '13139208', '13203242', '13244104', '13439309', '13519875', '13576146', '13647980', '13667224', '13708133', '13762621', '13872662', '13963315', '14004024', '14405171', '14429458', '14493773', '14741172', '14752255', '14803302', '15047946', '15544867', '15796122', '16180770', '16442942', '16575340', '16624584', '16690938', '16737222', '16894778', '17354841', '17615785', '17932121', '17947297', '18073228', '18078456', '18485555', '18700589', '18735625', '19178937', '19181843', '19205846', '19223780', '20094487', '20184606', '20828502', '20919046', '21108878', '21236973', '21288598', '21399053', '21658517', '21896625', '21983257', '22086574', '22432769', '22478735', '22554255', '22645856', '23049617', '23475004', '23478350', '23637158', '23644000', '23881878', '24062169', '24153018', '24165934', '24304215', '25117307', '26136141', '26253976', '26751544', '27175975', '27832955', '27892880', '28293066', '28299386', '28304087', '28610229', '28975472', '28994259', '29041324', '29066826', '29554823', '29588019', '29868734', '29919164', '29941762', '30034516', '30055958', '30077820', '30153506', '30225619', '30267876', '30667695', '30712061', '30712143', '30748487', '30876999', '30881327', '30927990', '31043482', '31205961', '31519570', '31579279', '31712452', '32130304', '32159727', '32193369', '32297572', '32375990', '32500977', '33009911', '33018599', '33039223', '33107905', '33112665', '33143298', '33151747', '33152885', '33223157', '33438250', '33475914', '33499989', '33552902', '33628850', '33927419', '34166223', '34291484', '36456945', '39323886', '40442683', '40442949', '42116400', '42200550', '42286641', '45110319', '45842622', '47100110', '49057722', '57473787', '57582785', '59546515', '60444437', '60500790', '60510583', '60560349', '60933017', '61777009', '67405936', '68699008', '68753482', '72357726', '72529787', '73629255', '73989345', '83310441', '88668298'
))


# 5.4 Aplicação da função render_report a cada parâmetro por meio do purrr.

purrr::pmap(params_list_lc, render_report_cnpj)


# P.S - Para colocar pontos nos números e zeros na frente.
# format(81282696, big.mark=".")
# str_pad(123, width=7, side="left", pad="0")


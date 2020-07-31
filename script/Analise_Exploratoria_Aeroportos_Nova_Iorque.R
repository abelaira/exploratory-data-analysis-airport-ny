
# Exercício Tidyverse – Aeroporto de Nova Iorque
#
# Manipulação e Visualização de dados utilizando os pacotes do tidyverse
# Comentários iniciais
# Imagine que você trabalha na empresa que administra o aeroporto de Nova Iorque. Baseado no grande número de informações geradas, 
# seu chefe lhe pediu insights que possam ajudá-lo, por exemplo, a aumentar o faturamento, diminuir custos e otimizar a operação 
# do aeroporto;

# Principais preocupações da direção da empresa:
  
#  Os atrasos de saída e chegada de aeronaves (soma desses atrasos maior que 90 minutos) geram um custo muito grande para empresa 
#  e a diretoria gostaria de entender um pouco mais sobre esse problema;

#  O aeroporto La Guardia é ideal para voos domésticos e de curta duração pois são mais cheios e aumentam consideravelmente a venda 
#  nas lojas do aeroporto. Pensando nisso, a diretoria quer proibir os voos com duração maior que três horas;

#  A área de manutenção e verificação das aeronaves deseja analisar a participação de cada fabricante de aeronaves nos aeroportos 
#  da cidade de Nova Iorque. Assim, a área precisa da relação entre as informações de cada voo e o fabricante do avião;

#  A área financeira sabe que operar aviões maiores e com mais motores é mais caro e que por isso, esses aviões deveriam ter mais
#  assentos, contudo ela acha que as empresas aéreas não entenderam muito bem isso e que essa relação 
#  (total de assentos/ total de motores) não está clara;

#  Uma grande preocupação na área da aviação é a visibilidade do piloto. Sabe-se que os estudos sobre as condições climáticas 
#  influenciam diretamente nessa variável e “a diretoria” gostaria de entender melhor essa relação.

# O que você tem são informações sobre voos de NYC (por exemplo, EWR, JFK e LGA) em 2013: 336.776 voos no total. Para ajudar a entender 
# o que causa atrasos, por exemplo, também foram disponibilizados outros conjuntos de dados úteis. Você possui as seguintes tabelas 
# de dados:
  
# flights/voos: todos os voos que partiram de Nova Iorque em 2013
# weather/tempo: dados meteorológicos de hora em hora para cada aeroporto
# planes/avioes: informações de construção sobre cada plano
# airports/aeroportos: nomes e localizações de aeroportos
# airlines/companhias aéreas: tradução entre códigos e nomes de transportadora de duas letras
#
# Descrição das variáveis
# Antes de usar as bases importadas faremos uma breve exploração das mesmas.
# 
# Base Airlines:
# carrier: sigla da companhia aéreas;
# name: nome da companhia aérea.
#
# Base Airports:
# faa: código FAA do aerooprto;
# name: nome do aeroporto;
# lat: coordenadas da latitude do aeroporto;
# lon: coordenadas da longitude do aeroporto;
# alt: altitude, em pés;
# tz: deslocamento do fuso horário pela GMT;
# dst: horário de verão (A= horário padrão US, U= Unknown, N= no dst);
# tzone: fuso horário da IANA, conforme determinado pelo webservice do GeoNames.
#
# Base Flights:
# year, month, day: data do voo;
# dep_time: horário de saída do voo;
# sched_dep_time: horário de saída agendado do voo;
# dep_delay: atraso na saída em minutos. Valores negativos representam saída antecipada;
# arr_time: horário de chegada do voo;
# sched_arr_time: horário de chegada agendado do voo;
# arr_delay: atraso na chegada em minutos. Valores negativos representam chegadas antecipadas;
# carrier: sigla da companhia aérea;
# flight: número do voo;
# tailnum: número da cauda do avião;
# origin: origem do voo;
# dest: destino do voo;
# air_time: duração do voo em minutos;
# distance: distância do voo;
# hour, minute: hora e minutos do voo;
# time_hour: data e hora agendada do voo no formato POSIXct.
#
# Base Planes:
# tailnum: número da cauda do avião;
# year: ano de fabricação do avião;
# type: tipo de aeronave;
# manufacturer: fabricante;
# model: modelo;
# engines: número de motores;
# seats: número de assentos;
# engine: tipo de motor;
# speed: média de velocidade voando em mph.
#
# Base Weather:
# origin: estação meteorológica;
# year, month, day, hour: ano, mês, dia e hora de registro;
# temp: temperatura em F;
# dewp: ponto de condensação da água em F;
# humid: humidade relativa;
# wind_dir: direção do vento em graus;
# wind_speed: velocidade do vento em mph;
# wind_gust: velocidade da rajada de vento em mph;
# precip: precipitação em polegadas;
# pressure: pressão em milibares;
# visib: visibilidade em milhas;
# time_hour: data e hora do registro no formato POSIXct.
#
# As 5 tabelas reportadas acima estão na pasta “Exercicio_em_grupo_2”. Observação importante: os arquivos encontram-se em diferentes
# formatos como: CSV ENG, CSV POR, JSON, rdata, SQL etc. Sua primeira tarefa é importar e organizar os dados.

rm(list = ls())
library(tidyverse)

getwd()

# 1. Lendo dados de AirLines
airline1 <- read_delim("./data/airlines_1.csv", delim = ";")
airline1
airline2 <- read_delim(file = "./data/airlines_2.csv", delim = ";")
airline2
airline3 <- read_delim("./data/airlines_2.csv", delim = ";")
airline3
airline <- rbind(airline1, airline2, airline3)
airline <- subset(airline, select = -X1) %>% distinct(); airline

# 2. Lendo dados de Airports
airport <- read_delim("./data/airports.txt", delim = " ", col_names = FALSE, skip = 1)
str(airport); airport
airport <- airport[,-1]; airport
colnames(airport) <- colnames(read_delim("./data/airports.txt", delim = " ", n_max = 0))
airport

# 3. Lendo dados de Flights
flight <- read_csv("./data/flights.csv")
flight <- subset(flight, select = -X1); flight

# 4. Lendo dados de Planes
library(jsonlite)
plane <- jsonlite::fromJSON("./data/planes.json")
head(plane)
colnames(plane)
str(plane)

plane %>% distinct(manufacturer,model) %>% arrange(manufacturer)

plane <- plane %>% mutate(manufacturer = str_trim(str_remove_all(manufacturer,"INDUSTRIE")))
plane <- plane %>% mutate(manufacturer = str_trim(str_replace_all(manufacturer,"CANADAIR LTD","BOMBARDIER INC")))
plane <- plane %>% mutate(manufacturer = str_trim(str_replace_all(manufacturer,"CANADAIR$","BOMBARDIER INC")))
plane <- plane %>% mutate(manufacturer = 
                            str_trim(str_replace_all(manufacturer,"MCDONNELL DOUGLAS AIRCRAFT CO","MCDONNELL DOUGLAS")))
plane <- plane %>% mutate(manufacturer = 
                            str_trim(str_replace_all(manufacturer,"MCDONNELL DOUGLAS CORPORATION","MCDONNELL DOUGLAS")))
plane <- plane %>% mutate(manufacturer = str_trim(str_replace_all(manufacturer,"^DOUGLAS","MCDONNELL DOUGLAS")))

# 5. Lendo dados de Weather
weather <- base::readRDS("./data/weather.rds")
weather

# ====== PROBLEMA 1 =============================================================================================== 
# Os atrasos de saída e chegada de aeronaves (soma desses atrasos maior que 90 minutos) geram um custo muito grande
# para empresa e a diretoria gostaria de entender um pouco mais sobre esse problema.
# -----------------------------------------------------------------------------------------------------------------
head(as.data.frame(flight %>% arrange(carrier, flight)))

# Calculando os minutos de atrasos e coletando um subconjunto com minutos maior que 90 minutos.
flight_delay <- flight %>% mutate(atraso_total = dep_delay + arr_delay)
flight_delay <-  flight_delay %>% filter(atraso_total > 90) %>% 
  select(year,month,day,carrier,tailnum,flight,origin,dest,atraso_total)

head(flight_delay)  

# Investigando as relações dos minutos de atrasos com a data
colours()
ggplot(flight_delay, aes(x = as.factor(year), y = atraso_total)) +
  geom_boxplot(fill = 'cyan', outlier.colour = 'red') +
  labs(x = "Ano", y = "Atraso em Minutos")

ggplot(flight_delay, aes(x = as.factor(year), y = atraso_total)) +
  geom_jitter() +
  labs(x = "Ano", y = "Atraso em Minutos", title = "Tempo de Atraso por Ano")

ggplot(flight_delay, aes(x = as.factor(month), y = atraso_total)) +
  geom_boxplot() +
  labs(x = "Mês", y = "Atraso em Minutos", title = "Tempo de Atraso por Mês")

ggplot(flight_delay, aes(x = as.factor(month))) +
  geom_bar() +
  labs(x = "Mês", y = "Qtd. de Atrasos", title = "Qtd. de Atrasos por Mês")

flight_delay %>% group_by(month) %>% summarise(qtd = n()) %>% 
  ggplot(aes(x = month, y = qtd)) +
  geom_point(shape = 15, colour = 'blue') + geom_line(colour = 'blue') +
  scale_y_continuous(limits = c(1000,5500),
                     breaks = c(1000,2000,3000,4000,5000),
                     labels = c('1.000','2.000','3.000','4.000','5.000')) +
  scale_x_discrete(limits = c('1','2','3','4','5','6','7','8','9','10','11','12'),
                   labels = c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez')) +
  labs(x= "Mês", y = "Qtd. de Atrasos", title = "Qtd. de Atrasos por Mês")

flight_delay %>% group_by(month) %>% summarise(total = sum(atraso_total)) %>% 
  ggplot(aes(x = month, y = total)) +
  geom_point(colour = 'red') + geom_line(colour = 'red') +
  scale_y_continuous(limits = c(200e3,1200e3),
                     breaks = c(200e3,400e3,600e3,800e3,1000e3,1200e3),
                     labels = c('200','400','600','800','1.000','1.200')) +
  scale_x_discrete(limits = c('1','2','3','4','5','6','7','8','9','10','11','12'),
                   labels = c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez')) +
  labs(x = "Mês", y = "Atraso em Minutos (Mil)", title = "Tempo Total de Atraso por Mês")

head(weather)

# Levantando métricas sobre o tempo do ano com exceção do mês de julho
as.data.frame(
  weather %>% filter(month != 7) %>% 
    summarise(temp_mean = mean(temp, na.rm = TRUE),
              dewp_mean = mean(dewp, na.rm = TRUE),
              humid_mean = mean(humid, na.rm = TRUE),
              wind_dir_mean = mean(wind_dir, na.rm = TRUE),
              wind_speed_mean = mean(wind_speed, na.rm = TRUE),
              wind_gust_mean = mean(wind_gust, na.rm = TRUE),
              precip_mean = mean(precip, na.rm = TRUE),
              pressure_mean = mean(pressure, na.rm = TRUE),
              visib_mean = mean(visib, na.rm = TRUE))
)

# Levantando métricas sobre o tempo do mês de Julho.
as.data.frame(
  weather %>% filter(month == 7) %>% 
  summarise(temp_mean = mean(temp),
            dewp_mean = mean(dewp),
            humid_mean = mean(humid),
            wind_dir_mean = mean(wind_dir, na.rm = TRUE),
            wind_speed_mean = mean(wind_speed, na.rm = TRUE),
            wind_gust_mean = mean(wind_gust, na.rm = TRUE),
            precip_mean = mean(precip),
            pressure_mean = mean(pressure, na.rm = TRUE),
            visib_mean = mean(visib))
)

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Como todos os dados são do ano de 2013, investiguei o comportamento por mês. Verifiquei que no mês de
#             Julho existe uma grande concentração de quantidade de atrasos. Essa concentração pela quantidade tam-
#             bém é validada pelo aspecto do total de minutos, que também mostra uma concentração no mês de Julho.
#             Não foi identificada quaisquer influência do tempo que justifique essa alta no mês de julho.
# ----------------------------------------------------------------------------------------------------------------


# Localizado uma relação dos minutos de atraso com as companhias aéreas
head(flight_delay)
(airline)

flight_delay <- flight_delay %>% 
  inner_join(airline, by = c('carrier' = 'carrier')) %>% mutate(airline = name) %>% 
  select(year,month,day,carrier,airline,tailnum,flight,origin,dest,atraso_total) 

head(flight_delay %>% arrange(carrier,flight))

ggplot(flight_delay, aes(x = as.factor(airline))) +
  geom_bar(aes(fill = as.factor(flight_delay$airline)), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,18000),
                     breaks = c(0,2500,5000,7500,10000,12500,15000,17500)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Companhia Aérea", y = "Qtd. Voos Atrasados > 90min", 
       title = "Qtd. de Atrasos por Companhia Aérea")

flight_airline <- group_by(flight_delay, airline) %>% 
  summarise(total_atraso = sum(atraso_total), atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  arrange(desc(total_atraso)) %>% 
  mutate(perc_atraso = total_atraso / sum(total_atraso))

flight_airline

# Grafico para apresentação mostrando um relação da companhia com atraso em minutos e número de atrasos
# OBS: Não consegui colocar a legenda para explicar as duas escalas dos eixos y's
ggplot(flight_airline, aes(x = as.factor(airline))) +
  geom_point(aes(y = total_atraso), colour = 'red', shape = 23, fill = 'red', size = 2) +
  geom_segment(aes(x = as.factor(airline), xend = as.factor(airline), y = 0, yend = qtd_atraso*200), 
               colour = 'blue', size = 5, alpha = 0.2) +
  scale_y_continuous(limits = c(0,4000000), 
                     breaks = c(0,0.5e06,1e06,1.5e06,2e06,2.5e06,3e06,3.5e06,4e06),
                     labels = c("0","0.5","1","1.5","2","2.5","3","3.5","4"),
                     sec.axis = sec_axis(~./200, name = "Qtd. de Atrasos (em mil)", 
                                         breaks = c(0,2500,5000,7500,10000,12500,15000,17500,20000),
                                         labels = c('0','2.5','5','7.5','10','12,5','15','17.5','20'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(0.1,0.1)) +
  labs(x = "Companhia Aérea", y = "Minutos em Atrasos (em milhões)", 
       title = "Atrasos por Companhia Aérea") 

# Representação dos atrasos da empresa "ExpressJet Airlines Inc.", com mais atrasos em minutos, em relação ao total
max(flight_airline$total_atraso)/sum(flight_airline$total_atraso)
min(flight_airline$total_atraso)/sum(flight_airline$total_atraso)

flight_airline

flight_airline_agr <- bind_rows(flight_airline %>% filter(perc_atraso >= 0.05),
                                flight_airline %>% filter(perc_atraso < 0.05) %>% 
                                summarise(airline = "Demais Companhias", total_atraso = sum(total_atraso), 
                                          atraso_medio = mean(atraso_medio), 
                                          qtd_atraso = sum(qtd_atraso), perc_atraso = sum(perc_atraso)))
          
flight_airline_agr
colors()

pie(flight_airline_agr$perc_atraso, 
    labels = paste(flight_airline_agr$airline," (",round(flight_airline_agr$perc_atraso*100,2),"%)",sep = ""), 
    main = "Atrasos por Companhia Aérea", clockwise = TRUE, radius = 1,
    col = c('lightskyblue','lightcoral','lightgoldenrod','cyan4','lightyellow','lightgreen','lightpink'))

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: As companhias "ExpressJet Airlines Inc." e "JetBlue Airways representa mais de 50% dos minutos totais 
#             de atrasos maior que 90 minutos. 
#             Se reduzissêmos para a metade esse valor total de minutos, teríamos um redução dos custos
# ----------------------------------------------------------------------------------------------------------------


# Como as companhias aéreas "ExpressJet Airlines Inc." e "JetBlue Airways" se destacam imensamente das demais, 
# vamos investigar melhor.
# Investigando se esse atraso da "ExpressJet Airlines Inc." e "JetBlue Airways estão relacionados com algum avião
flight_airline_ejb6 <- flight_delay %>% filter(carrier %in% c('B6','EV'))

head(flight_airline_ejb6)
head(plane)

flight_airline_ejb6 <- flight_airline_ejb6 %>% left_join(plane, by = c('tailnum','tailnum')) %>% 
  select(year.x,month,day,carrier,airline,tailnum,model,year.y,flight,origin,dest,atraso_total)
colnames(flight_airline_ejb6) <- 
  c('year_flight','month','day','carrier','airline','tailnum','model','year_model','flight','origin','dest','atraso_total')

head(flight_airline_ejb6)

ggplot(flight_airline_ejb6, aes(x = as.factor(model), y = atraso_total)) +
  geom_boxplot(aes(fill = as.factor(flight_airline_ejb6$model)), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,1200),
                     breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200)) +
  scale_x_discrete(na.translate = FALSE) +
  labs(x = "Modelo do Avião", y = "Minutos de Atraso") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flight_airline_ejb6, aes(x = as.factor(model), y = atraso_total)) +
  geom_jitter(aes(colour = as.factor(flight_airline_ejb6$year_model))) +
  scale_colour_viridis_d(na.value = "gray75") +
  scale_y_continuous(limits = c(0,1200),
                     breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200)) +
  scale_x_discrete(na.translate = FALSE) +
  labs(x = "Modelo do Avião", y = "Minutos de Atraso", colour = "Ano") +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1))

# Avaliação geral e por modelo dos atrasos para a companhia ExpressJet Airlines Inc
# ---------------------------------------------------------------------------------
flight_airline_ejb6 %>% filter(carrier == 'EV') %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(total_atraso,qtd_atraso,atraso_medio,desvioPad)

flight_airline_ejb6 %>% filter(carrier == 'EV') %>% group_by(model) %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(model,total_atraso,qtd_atraso,atraso_medio,desvioPad)

# Retirando o modelo EMB-145XR responsavel pelos maiores atrasos
flight_airline_ejb6 %>% filter(carrier == 'EV' & model != 'EMB-145XR') %>%
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(total_atraso,qtd_atraso,atraso_medio,desvioPad)

# Retirando o modelo da Embraer responsavel pelos maiores atrasos
flight_airline_ejb6 %>% filter(carrier == 'EV' & model != 'EMB-145LR') %>%
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(total_atraso,qtd_atraso,atraso_medio,desvioPad)

# Avaliação geral e por modelo dos atrasos para a companhia JetBlue Airways
# ---------------------------------------------------------------------------------
flight_airline_ejb6 %>% filter(carrier == 'B6') %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(total_atraso,qtd_atraso,atraso_medio,desvioPad)

flight_airline_ejb6 %>% filter(carrier == 'B6') %>% group_by(model) %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(model,total_atraso,qtd_atraso,atraso_medio,desvioPad)

# Retirando o modelo A320-232 responsavel pelos maiores atrasos
flight_airline_ejb6 %>% filter(carrier == 'B6' & model != 'A320-232') %>%
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(total_atraso,qtd_atraso,atraso_medio,desvioPad)

# Retirando o modelo ERJ 190-100 IGW responsavel pelos maiores atrasos
flight_airline_ejb6 %>% filter(carrier == 'B6' & model != 'ERJ 190-100 IGW') %>%
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(total_atraso,qtd_atraso,atraso_medio,desvioPad)


flight_airline_ejb6_plane <- flight_airline_ejb6 %>% group_by(model) %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  mutate(perc_atraso = total_atraso / sum(total_atraso)) %>% 
  arrange(desc(total_atraso)) %>% 
  select(model,total_atraso,qtd_atraso,perc_atraso)

flight_airline_ejb6_plane

flight_airline_ejb6_plane_agr <- bind_rows(flight_airline_ejb6_plane %>% filter(perc_atraso >= 0.05),
  flight_airline_ejb6_plane %>% filter(perc_atraso < 0.05) %>% 
  summarise(model = "Demais Modelos", total_atraso = sum(total_atraso), 
            qtd_atraso = sum(qtd_atraso), perc_atraso = sum(perc_atraso)))

flight_airline_ejb6_plane_agr

pie(flight_airline_ejb6_plane_agr$perc_atraso, 
    labels = paste(flight_airline_ejb6_plane_agr$model," (",
                   round(flight_airline_ejb6_plane_agr$perc_atraso*100,2),"%)",sep = ""), 
    main = "Atrasos por Modelo para ExpressJet e JetBlue", clockwise = TRUE, radius = 1.2,
    col = c('lightskyblue','lightcoral','lightgoldenrod','cyan4','lightpink','lightgreen'))


# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Identifiquei que o modelo da Embraer EMB-145LR e o A320-232 são os que estão impactando mais o total 
#             de minutos em atraso para as companhias "Expressjet Airlines Inc" e "JetBlue Airways, 
#             eles são responsáveis por mais de 50% dos atrasos. 
# ----------------------------------------------------------------------------------------------------------------


# Investigando a relação de atrasos da companhia com os aeroportos
head(flight_delay)
head(airport)
head(flight)

# Verificando a quantidade total de voos por aeroporto de origem
flight %>% group_by(origin) %>% 
  summarise(qtd_flight = n()) %>% 
  mutate(perc_qtd = (qtd_flight / sum(qtd_flight))*100) %>% 
  select(origin,qtd_flight,perc_qtd)


# Avaliando o atraso por aeroporto de origem
flight_delay_airport_origin <- flight_delay %>% group_by(origin) %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  mutate(perc_atraso = total_atraso / sum(total_atraso)) %>% 
  select(origin,total_atraso,qtd_atraso,perc_atraso,atraso_medio,desvioPad)

flight_delay_airport_origin

pie(flight_delay_airport_origin$perc_atraso,
    labels = paste(flight_delay_airport_origin$origin, " (",
                   round(flight_delay_airport_origin$perc_atraso*100,2),"%)"),
    main = "Minutos de Atraso",
    clockwise = TRUE,
    col = c('lightskyblue','lightcoral','lightgoldenrod','cyan4','lightyellow','lightgreen','lightpink'))


# Obtendo um data set de atrasos com as "ExpressJet Airlines Inc." e "JetBlue Airways" no aeroporto de EWR
# --------------------------------------------------------------------------------------------------------
(flight_delay_airport_ewr_ejb6 <- flight_delay %>% filter(origin == 'EWR' & carrier %in% c('EV','B6'))) 
format(sum(flight_delay_airport_ewr_ejb6$atraso_total), big.mark = ",", nsmall = 0)

(airport %>% filter(faa == 'EWR'))$name

# Verificando o percentual de atraso que corresponde a "ExpressJet Airlines Inc." e "JetBlue Airways
# do total no aeroporto de EWR
sum(flight_delay_airport_ewr_ejb6$atraso_total) / flight_delay_airport_origin[1,2] * 100


# Obtendo um data set de atrasos com as "ExpressJet Airlines Inc." e "JetBlue Airways" no aeroporto de JFK
# --------------------------------------------------------------------------------------------------------
flight_delay_airport_jfk_ejb6 <- flight_delay %>% filter(origin == 'JFK' & carrier %in% c('EV','B6')) 

(airport %>% filter(faa == 'JFK'))$name

# Verificando o percentual de atraso que corresponde a "ExpressJet Airlines Inc." e "JetBlue Airways
# do total no aeroporto de EWR
sum(flight_delay_airport_jfk_ejb6$atraso_total) / flight_delay_airport_origin[1,2] * 100


# Obtendo um data set de atrasos com as "ExpressJet Airlines Inc." e "JetBlue Airways" no aeroporto de LGA
# --------------------------------------------------------------------------------------------------------
flight_delay_airport_lga_ejb6 <- flight_delay %>% filter(origin == 'LGA' & carrier %in% c('EV','B6')) 

(airport %>% filter(faa == 'LGA'))$name

# Verificando o percentual de atraso que corresponde a "ExpressJet Airlines Inc." e "JetBlue Airways
# do total no aeroporto de LGA
sum(flight_delay_airport_lga_ejb6$atraso_total) / flight_delay_airport_origin[1,2] * 100

# Investigando um pouco mais o aeroporto de LGA
flight_delay_airport_lga <- flight_delay %>% filter(origin == 'LGA') %>% group_by(airline) %>%
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  mutate(perc_atraso = total_atraso / sum(total_atraso)) %>% 
  arrange(desc(perc_atraso)) %>% 
  select(airline,total_atraso,qtd_atraso,perc_atraso,atraso_medio,desvioPad)

ggplot(flight_delay_airport_lga, aes(x = airline, y = total_atraso)) +
  geom_point(aes(size = flight_delay_airport_lga$perc_atraso,
             color = as.factor(flight_delay_airport_lga$airline))) +
  geom_text(label = paste(round(flight_delay_airport_lga$perc_atraso*100,0),"%",sep = ""),
            family = 'mono', hjust = 0, nudge_x = 0.1) +
  scale_y_continuous(limits = c(0,4.55e5),
                     breaks = c(0,1e5,2e5,3e5,4e5),
                     labels = c('0','100','200','300','400')) +
  guides(color = 'none') +
  labs(x = "Companhias Áereas", y = "Total de Atraso (Mil Minutos)", size = "Percentual de Atraso") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

airline
paste('Atraso (em Minutos) da Delta Air Lines Inc no Aeroporto ',
      (airport %>% filter(faa == 'EWR'))$name,": ",
      (flight_delay %>% filter(origin == 'EWR' & carrier %in% c('DL')) %>% 
         summarise(total = sum(atraso_total)))$total %>% format(big.mark = ','), sep = '')

paste('Atraso (em Minutos) da Delta Air Lines Inc no Aeroporto ',
      (airport %>% filter(faa == 'JFK'))$name,": ",
      (flight_delay %>% filter(origin == 'JFK' & carrier %in% c('DL')) %>% 
         summarise(total = sum(atraso_total)))$total %>% format(big.mark = ','), sep = '')


# Avaliando o atraso por aeroporto de destino
flight_delay %>% group_by(dest) %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  select(dest,total_atraso,qtd_atraso,atraso_medio,desvioPad) %>% 
  ggplot(aes(x = as.factor(dest), y = total_atraso)) +
  geom_point() +
  labs(x = "Aeroporto Destino", y = "Total Atraso (Mil Min.)") +
  scale_y_continuous(limits = c(0,6e5),
                     breaks = c(0,1e5,2e5,3e5,4e5,5e5,6e5),
                     labels = c('0','100','200','300','400','500','600')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = 'mono', face = 'bold')) 

(airport %>% filter(faa == 'ATL'))$name

# Veficando o aeroporto destino ATL com mais detalhe por companhia aérea  
flight_delay %>% filter(dest == 'ATL') %>% group_by(airline) %>% 
  summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
            atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
  mutate(perc_atraso = total_atraso / sum(total_atraso)) %>% 
  select(airline,total_atraso,perc_atraso,qtd_atraso,atraso_medio,desvioPad)

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Cerca de 34% dos atrasos com mais de 90 minutos tem como aeroporto de origem o EWR
#             (Newark Liberty Intl). Desse total, cerca de 85% os responsáveis pelo atraso são as companhias 
#             ExpressJet Airlines Inc e JetBlue Airways. Nos demais aeroportos, JFK (John F Kennedy Intl) e 
#             LGA (La Guardia), essas duas companhias são responsáveis por cerca de 50% dos atrasos em minutos.
# ----------------------------------------------------------------------------------------------------------------

# -------------------------------------------------
# Análise para a companhia ExpressJet Airlines Inc.
# -------------------------------------------------
# quadro com os atrasos para a companhia ExpressJet Airlines Inc
(t_ev <- flight_airline_ejb6 %>% filter(carrier == 'EV') %>% group_by(model) %>% 
   summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
             atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
   select(model,total_atraso,qtd_atraso,atraso_medio,desvioPad))

plane %>% filter(model == 'EMB-145XR') %>% select(-tailnum) %>% distinct()
plane %>% filter(manufacturer == 'BOMBARDIER INC' & seats <= 60) %>% select(-tailnum) %>% distinct()

# total de voos para o modelo EMB-145XR
(t_flight_emb <- flight %>% left_join(plane, by = c('tailnum','tailnum')) %>% 
  filter(model == 'EMB-145XR') %>% nrow())

# total de voos para o modelo CL-600-2B19
(t_flight_bom <- flight %>% left_join(plane, by = c('tailnum','tailnum')) %>% 
  filter(model == 'CL-600-2B19') %>% nrow())


# percentual da quantidade de atrasos severos (> 90 minutos) do total de voos por modelo
(t_ev %>% filter(model == 'EMB-145XR'))$qtd_atraso / t_flight_emb
(t_ev %>% filter(model == 'CL-600-2B19'))$qtd_atraso / t_flight_bom

# severidade dos atrasos medido em minutos
# ----------------------------------------
# quadro com o total de atrasos para o modelo EMB-145XR
(t_delay_emb <- flight %>% mutate(atraso_total = dep_delay + arr_delay) %>% filter(atraso_total > 0) %>% 
  select(year,month,day,carrier,tailnum,flight,origin,dest,atraso_total) %>% 
  left_join(plane, by = c('tailnum','tailnum')) %>% filter(model == 'EMB-145XR') %>%
  group_by(model) %>% summarise(total_atraso = sum(atraso_total), 
                                qtd_atraso = n(),
                                atraso_medio = mean(atraso_total)))
  
# quadro com o total de atrasos para o modelo CL-600-2B19
(t_delay_bom <- flight %>% mutate(atraso_total = dep_delay + arr_delay) %>% filter(atraso_total > 0) %>% 
    select(year,month,day,carrier,tailnum,flight,origin,dest,atraso_total) %>% 
    left_join(plane, by = c('tailnum','tailnum')) %>% filter(model == 'CL-600-2B19') %>%
    group_by(model) %>% summarise(total_atraso = sum(atraso_total), 
                                  qtd_atraso = n(),
                                  atraso_medio = mean(atraso_total)))

# percentual de quantidade de atrasos > 0 do total de voos por modelo 
t_delay_emb$qtd_atraso / t_flight_emb
t_delay_bom$qtd_atraso / t_flight_bom

# Dado que o percentual da quantidade de atrasos severos e também o percentual de atrasos é menor para o 
# modelo da Bombardier, será vantajoso para a empresa que opera os aeroportos que a ExpressJet Airlines Inc
# aponsente esse modelo da Embraer e passar a operar com esse modelo da Bombardier.

# Atraso projetado se o modelo da Bombardier fosse usado no lugar do modelo da Embraer
(x <- t_delay_bom$atraso_medio * (t_delay_bom$qtd_atraso / t_flight_bom) * t_flight_emb)

t_delay_emb$total_atraso

# ganho percentual no total de minutos em atraso
1 - x / t_delay_emb$total_atraso


# -------------------------------------------------
# Análise para a companhia JetBlue Airways
# -------------------------------------------------
# quadro com os atrasos para a companhia JetBlue Airways
(t_b6 <- flight_airline_ejb6 %>% filter(carrier == 'B6') %>% group_by(model) %>% 
    summarise(desvioPad = sd(atraso_total), total_atraso = sum(atraso_total), 
              atraso_medio = mean(atraso_total), qtd_atraso = n()) %>% 
    select(model,total_atraso,qtd_atraso,atraso_medio,desvioPad))

plane %>% filter(model == 'A320-232') %>% select(-tailnum) %>% distinct()

# modelo com mais de 300 lugares
plane %>% filter(model == 'A321-231') %>% select(-tailnum) %>% distinct()

plane %>% filter(manufacturer == 'AIRBUS' & seats <= 250) %>% select(-tailnum) %>% distinct()

# modelo mais semelhante ao A320-232
plane %>% filter(model == 'A321-211') %>% select(-tailnum) %>% distinct()

# total de voos para o modelo A320-232
(t_flight_a320 <- flight %>% left_join(plane, by = c('tailnum','tailnum')) %>% 
    filter(model == 'A320-232') %>% nrow())

# total de voos para o modelo A321-231
(t_flight_a321 <- flight %>% left_join(plane, by = c('tailnum','tailnum')) %>% 
    filter(model == 'A321-231') %>% nrow())


# percentual da quantidade de atrasos severos (> 90 minutos) do total de voos por modelo
(t_b6 %>% filter(model == 'A320-232'))$qtd_atraso / t_flight_a320
(t_b6 %>% filter(model == 'A321-231'))$qtd_atraso / t_flight_a321


# ---------------------------------------------------
# verificando para o modelo A321-211 mais semelhante
(t2_b6 <- flight %>% mutate(atraso_total = dep_delay + arr_delay) %>% filter(atraso_total > 90) %>% 
    select(year,month,day,carrier,tailnum,flight,origin,dest,atraso_total) %>% 
    left_join(plane, by = c('tailnum','tailnum')) %>% filter(model == 'A321-211') %>%
    group_by(model) %>% summarise(total_atraso = sum(atraso_total), 
                                  qtd_atraso = n(),
                                  atraso_medio = mean(atraso_total)))
t2_b6$qtd_atraso / t_flight_a321
# ----------------------------------------------------


# severidade dos atrasos medido em minutos
# ----------------------------------------
# quadro com o total de atrasos para o modelo A320-232
(t_delay_a320 <- flight %>% mutate(atraso_total = dep_delay + arr_delay) %>% filter(atraso_total > 0) %>% 
   select(year,month,day,carrier,tailnum,flight,origin,dest,atraso_total) %>% 
   left_join(plane, by = c('tailnum','tailnum')) %>% filter(model == 'A320-232') %>%
   group_by(model) %>% summarise(total_atraso = sum(atraso_total), 
                                 qtd_atraso = n(),
                                 atraso_medio = mean(atraso_total)))

# quadro com o total de atrasos para o modelo A321-231
(t_delay_a321 <- flight %>% mutate(atraso_total = dep_delay + arr_delay) %>% filter(atraso_total > 0) %>% 
    select(year,month,day,carrier,tailnum,flight,origin,dest,atraso_total) %>% 
    left_join(plane, by = c('tailnum','tailnum')) %>% filter(model == 'A321-231') %>%
    group_by(model) %>% summarise(total_atraso = sum(atraso_total), 
                                  qtd_atraso = n(),
                                  atraso_medio = mean(atraso_total)))

# percentual de quantidade de atrasos > 0 do total de voos por modelo 
t_delay_a320$qtd_atraso / t_flight_a320
t_delay_a321$qtd_atraso / t_flight_a321

# Dado que o percentual da quantidade de atrasos severos e também o percentual de atrasos é menor para o 
# modelo da A321, talvez seja vantajoso para a empresa que opera os aeroportos que a JetBlue Airways
# aponsente esse modelo da A320 e passar a operar com o modelo A321.

# Atraso projetado se o modelo da Bombardier fosse usado no lugar do modelo A321
(x <- t_delay_a321$atraso_medio * (t_delay_a321$qtd_atraso / t_flight_a321) * t_flight_a320)

t_delay_a320$total_atraso

# ganho percentual no total de minutos em atraso
1 - x / t_delay_a320$total_atraso


# ----- CONCLUSÃO 1 -----------------------------------------------------------------------------------------
# Existe uma grande concentração de atrasos nos meses de Junho e Julho, sendo que essa concentração se deve 
# principalmente ao número de voos que aumenta abruptamente nas férias de verão.  
# A companhia ExpressJet Airlines Inc. é responsável por quase a metade dos atrasos ocorridos nos aeroportos 
# administrados pela empresa. Dentre os atrasos dessa companhia, o modelo EMB-145LR é responsável pela metade 
# deles.  
# Cerca de 40% dos atrasos ocorreram no aeroporto de Newark Liberty Intl. e, desse percentual, a companhia 
# ExpressJet Airlines Inc foi responsável por 85% deles.
# ==========================================================================================================

# ===== PROBLEMA 2 =========================================================================================
# O aeroporto La Guardia é ideal para voos domésticos e de curta duração pois são mais cheios e aumentam 
# consideravelmente a venda nas lojas do aeroporto. Pensando nisso, a diretoria quer proibir os voos 
# com duração maior que três horas.
# ---------------------------------------------------------------------------------------------------------

# Agrupando o data set flight em dois grupo, voos menores e iguais a 3h e maiores que 3h, ou 180 minutos, para o 
# aeroporto La Guardia
flight %>% as.data.frame() %>% head()

# Verificando a existência de NA's e comparando com o total da amostra
flight %>% filter(origin == 'LGA') %>% nrow()
flight %>% filter(dest == 'LGA') %>% nrow()
flight %>% filter(is.na(air_time) & origin == 'LGA') %>% nrow()

# Verificando o percentual de NA's em relação ao total
flight %>% filter(is.na(air_time) & origin == 'LGA') %>% nrow() / 
  flight %>% filter(origin == 'LGA') %>% nrow() * 100


flight_lga <- flight %>% filter(origin == 'LGA' | dest == 'LGA') %>%
  mutate(curta_dur = if_else(air_time <= 180, TRUE, FALSE),
         atraso_total = dep_delay + arr_delay) %>% 
  select(year,month,day,carrier,tailnum,origin,dest,air_time,atraso_total,curta_dur)

flight_lga %>% as.data.frame() %>% head()

# Agrupando os voos de acordo com o tipo: Curto, Longo ou Não identificado
(flight_lga_tipo <- flight_lga %>% group_by(curta_dur) %>% 
  summarise(qtd_voo = n(),
            media_hr_voo = mean(air_time)) %>% 
  mutate(perc_qtd = qtd_voo / sum(qtd_voo), 
         tipo_voo = case_when(curta_dur ~ "Curto",
                              curta_dur == FALSE ~ "Longo",
                              is.na(curta_dur) ~ "Não Identificado")) %>% 
  arrange(desc(qtd_voo)) %>% 
  select(tipo_voo,qtd_voo,perc_qtd,media_hr_voo))

pie(flight_lga_tipo$qtd_voo,
    labels = paste(flight_lga_tipo$tipo_voo, " (", round(flight_lga_tipo$perc_qtd * 100,2), "%)", sep = ""),
    clockwise = TRUE, radius = 1,
    main = "Quantidade de voos por tipo para o aeroporto La Guardia",
    col = c('lightskyblue','lightcoral','lightgoldenrod','cyan4','lightyellow','lightgreen','lightpink'))


# Verificando voos curtos (<= 180 minutos) nos demais aeroportos de Nova Iorque
flight_ny <- flight %>% filter(origin %in% c('EWR','JFK') | dest %in% c('EWR','JFK')) %>%
  mutate(curta_dur = if_else(air_time <= 180, TRUE, FALSE),
         atraso_total = dep_delay + arr_delay) %>% 
  select(year,month,day,carrier,tailnum,origin,dest,air_time,atraso_total,curta_dur)

(flight_ny_tipo <- flight_ny %>% group_by(curta_dur) %>% 
  summarise(qtd_voo = n(),
            media_hr_voo = mean(air_time)) %>% 
  mutate(perc_qtd = qtd_voo / sum(qtd_voo), 
         tipo_voo = case_when(curta_dur ~ "Curto",
                              curta_dur == FALSE ~ "Longo",
                              is.na(curta_dur) ~ "Não Identificado")) %>% 
  arrange(desc(qtd_voo)) %>% 
  select(tipo_voo,qtd_voo,perc_qtd,media_hr_voo))

pie(flight_ny_tipo$qtd_voo,
    labels = paste(flight_ny_tipo$tipo_voo, " (", round(flight_ny_tipo$perc_qtd * 100,2), "%)", sep = ""),
    clockwise = TRUE, radius = 1,
    main = "Quantidade de voos por tipo nos aeroportos EWR e JFK",
    col = c('lightskyblue','lightcoral','lightgoldenrod','cyan4','lightyellow','lightgreen','lightpink'))


# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: O aeroporto de La Guardia já está bem concentrado em voos de curta duração (<= 180 minutos).
#             Existe possibilidade de passar os poucos voos de longa duração para os demais aeroportos de 
#             Nova Iorque e trazer mais voos curtos que estão nesses aeroportos para o La Guardia.
# ----------------------------------------------------------------------------------------------------------------

# Verificando quantos voos possuem tempo de voo maior que 180 minutos e menor que 190 minutos.
round(flight_lga %>% filter(!curta_dur & air_time > 180 & air_time <= 190) %>% nrow() /
  flight_lga %>% nrow() * 100, 2)

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Cerca de 2% dos voos possuem tempo de voo entre 180 e 190 minutos. Portanto não há necessidade
#             de uma flexibilização no tempo limite para classificar um voo como curto.
# ----------------------------------------------------------------------------------------------------------------

# Verificando os voos longos por companhia no La Guardia
flight_lga %>% filter(!curta_dur) %>% group_by(carrier) %>% 
  summarise(qtd_voo = n()) %>% 
  mutate(perc_qtd = qtd_voo / sum(qtd_voo)) %>% 
  inner_join(airline, by = c('carrier','carrier')) %>% 
  arrange(name) %>% 
  select(carrier,name,qtd_voo,perc_qtd)

# Verificando os voos curtos por companhia nos demais aeroportos de Nova Iorque
flight_ny %>% filter(curta_dur) %>% group_by(carrier) %>% 
  summarise(qtd_voo = n()) %>% 
  mutate(perc_qtd = qtd_voo / sum(qtd_voo)) %>% 
  inner_join(airline, by = c('carrier','carrier')) %>% 
  arrange(name) %>% 
  select(carrier,name,qtd_voo,perc_qtd)

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Serão cerca de 7 companhais afetadas com a proibição de voos longos no aeroporto de La Guardia,
#             sendo 6 delas (American Airlines Inc, Delta Air Lines Inc, Endeavor Air Inc, Envoy Air,
#             ExpressJet Airlines Inc e JetBlue Airways) terão que trocar voos de longa duração que estão no
#             La Guardia para os demais aeroportos de Nova Iorque e os curtos que estão nesses demais aeroportos
#             de Nova Iorque (Newark Liberty Intl e John F Kennedy Intl) deverão passar para o La Guardia.
#             A sétima companhia, Frontier Airlines Inc, como não possui voo curto nos demais aeroportos de Nova
#             Iorque, deverá transferir suas operações com voos longos para um desses aeroportos.
# ----------------------------------------------------------------------------------------------------------------


# ----- CONCLUSÃO 2 ------------------------------------------------------------------------------------------
# Cerca de 85% dos voos que partem do aeroporto La Guardia são voos de curta duração, o impacto inicial em
# proibir voos com tempo de duração maiores que 180 minutos não irá causar transtornos significativos para
# as companhias, pois das 7 companhias que possuem voos longos no La Guardia, possuem voos curtos nos demais
# aeroportos de Nova Iorque. Podendo fazer uma troca de voos, passando os curtos nos demais aeroportos de Nova
# Iorque para o La Guardia e passando os voos longos para um desses aeroportos.
# Somente a companhia Frontier Airlines Inc será mais fortemente impactada, pois deverá transferir suas operações
# com voos longos para um dos demais aeroportos de Nova Iorque (Newark Liberty Intl ou John F Kennedy Intl).
# ============================================================================================================


# ===== PROBLEMA 3 =========================================================================================
#  A área de manutenção e verificação das aeronaves deseja analisar a participação de cada fabricante de 
# aeronaves nos aeroportos da cidade de Nova Iorque. Assim, a área precisa da relação entre as informações 
# de cada voo e o fabricante do avião.
# ---------------------------------------------------------------------------------------------------------
flight
plane %>% as_tibble()

# Reduzindo o Data Set para somente ter os voos que saem e chegam em Nova Iorque nos aeroportos: LGA, EWR e JFK
flight_plane <- flight %>% filter(origin %in% c('LGA','JFK','EWR') | dest %in% c('LGA','JFK','EWR')) %>% 
  select(year,month,day,carrier,flight,tailnum,origin,dest,distance)

# Incluindo os dados dos fabricantes
flight_plane <- flight_plane %>% inner_join(plane, by = c('tailnum','tailnum')) %>%
  mutate(year_flight = year.x, year_model = year.y) %>% 
  select(year_flight,month,day,carrier,flight,tailnum,manufacturer,model,year_model,origin,dest,distance)

# Agregando os dados
flight_plane_agg <- flight_plane %>% group_by(manufacturer) %>% 
  summarise(distance = sum(distance),
            qtd_voo = n()) %>% 
  arrange(desc(qtd_voo))

flight_plane_agg
flight_plane_agg %>% tail()

# Ordenando o Data Set para exibição pela quantidade de voo
flight_plane_agg_ord <- flight_plane_agg; flight_plane_agg_ord
indice <- order(flight_plane_agg$qtd_voo, decreasing = TRUE); indice
nivel <- flight_plane_agg$manufacturer[indice]; nivel
flight_plane_agg_ord$manufacturer <- factor(flight_plane_agg_ord$manufacturer, levels = nivel, ordered = TRUE)
flight_plane_agg_ord

flight_plane_agg_ord %>% 
  ggplot(aes(x = manufacturer, y = qtd_voo)) +
  geom_bar(aes(fill = flight_plane_agg_ord$manufacturer), stat = "identity", show.legend = FALSE) +
  scale_y_log10(limits = c(1,100000),
                breaks = c(1,3,10,30,100,300,1e3,3e3,1e4,3e4,1e5),
                labels = c('1','3','10','30','100','300','1.000','3.000','10.000','30.000','100.000')) +
  labs(x = "Fabricante", y = "Quantidade de Voos", title = "Voos nos Aeroportos de Nova Iorque") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = 'mono', face = 'bold'))

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Percebe-se uma concentração da quantidade de voos em 5 fabricantes, Airbus, Boeing,
#             Bombardier Inc, Embraer e McDonnell Douglas.
# ----------------------------------------------------------------------------------------------------------------


# Ordenando o Data Set para exibição pela distancia percorrida
flight_plane_agg_ord <- flight_plane_agg; flight_plane_agg_ord
indice <- order(flight_plane_agg$distance, decreasing = TRUE); indice
nivel <- flight_plane_agg$manufacturer[indice]; nivel
flight_plane_agg_ord$manufacturer <- factor(flight_plane_agg_ord$manufacturer, levels = nivel, ordered = TRUE)
flight_plane_agg_ord

flight_plane_agg_ord %>% 
  ggplot(aes(x = manufacturer, y = distance)) +
  geom_col(aes(fill = flight_plane_agg_ord$manufacturer), show.legend = FALSE) +
  scale_y_log10(limits = c(1,1.5e8),
                breaks = c(1,10,100,1000,10000,100000,1000000,10000000,100000000),
                labels = c('1','10','100','1.000','10.000','100.000','1.000.000','10.000.000','100.000.000')) +
  labs(x = "Fabricante", y = "Distância Percorrida (km)", title = "Voos nos Aeroportos de Nova Iorque") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = 'mono', face = 'bold'))


# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Existe uma concentração grande de voos das fabricantes Boeing, Aribus, Bombardier, Embraer e McDonnel Douglas
# nos aeroportos de Nova Iorque quando analisamos pela quantidade de voo realizado no período. Essa concentração
# se mantém nos mesmos fabricantes quando é analisado a distância total percorrida.
# ----------------------------------------------------------------------------------------------------------------

flight_plane_agg_aux <- flight_plane_agg %>% 
  filter(manufacturer %in% c('BOEING','AIRBUS','EMBRAER','BOMBARDIER INC','MCDONNELL DOUGLAS')) %>% 
  union(
    flight_plane_agg %>% filter(!manufacturer %in% c('BOEING','AIRBUS','EMBRAER','BOMBARDIER INC','MCDONNELL DOUGLAS')) %>% 
      summarise(manufacturer = 'Demais Fabricantes',
                distance = sum(distance),
                qtd_voo = sum(qtd_voo)))
flight_plane_agg_aux <- flight_plane_agg_aux %>% mutate(perc_voo = qtd_voo / sum(qtd_voo))

pie(flight_plane_agg_aux$qtd_voo,
    labels = paste(flight_plane_agg_aux$manufacturer, " (", 
                   round(flight_plane_agg_aux$perc_voo * 100, 2),
                   "%)", sep = ''),
    clockwise = TRUE,
    radius = 1.2,
    main = "Quantidade de voos por fabricante",
    col = c('lightskyblue','lightcoral','lightgoldenrod','cyan4','lightyellow','lightgreen','lightpink'))


# ----- CONCLUSÃO 3 ------------------------------------------------------------------------------------------
# Existe uma concentração grande de voos das fabricantes Boeing, Aribus, Bombardier, Embraer e McDonnel Douglas
# nos aeroportos de Nova Iorque quando analisamos pela quantidade de voo realizado no período. Essa concentração
# se mantém nos mesmos fabricantes quando a análise é feito utilizando a distância total, em km, percorrida.
# Esses 5 fabricantes respondem por quase 100% dos voos nos aeroportos de Nova Iorque, sendo que Airbus e
# Boeing respondem por mais da metade desses voos.
# ============================================================================================================


# ===== PROBLEMA 4 =========================================================================================
#  A área financeira sabe que operar aviões maiores e com mais motores é mais caro e que por isso, esses 
# aviões deveriam ter mais assentos, contudo ela acha que as empresas aéreas não entenderam muito bem isso
# e que essa relação (total de assentos / total de motores) não está clara.
# ----------------------------------------------------------------------------------------------------------

head(plane)
head(as.data.frame(flight))
(airline)

flight_plane_airline <- flight %>% select(tailnum,carrier,flight) %>% distinct() %>% 
  left_join(plane, by = c('tailnum','tailnum')) %>% 
  select(tailnum,manufacturer,model,engines,seats,carrier) %>%  
  left_join(airline, by = c('carrier','carrier')) %>% 
  select(tailnum,manufacturer,model,engines,seats,carrier,name) %>% 
  arrange(manufacturer,model)

flight_plane_airline
flight_plane_airline %>% nrow()

# Total de NA's nas colunas seats e engines
sum(if_else(is.na(flight_plane_airline$seats) | is.na(flight_plane_airline$engines),TRUE,FALSE))

# Percentual de NA's nas colunas seats e engines em relação ao total
sum(if_else(is.na(flight_plane_airline$seats) | is.na(flight_plane_airline$engines),TRUE,FALSE)) /
  flight_plane_airline %>% nrow()

# Total de NA's na coluna name airline
sum(if_else(is.na(flight_plane_airline$name),TRUE,FALSE))

# Calculando o Tamanho Relativo = total assentos / total motores
flight_plane_airline <- flight_plane_airline %>% 
  mutate(tamanho = if_else(is.na(seats),0,as.double(seats)) / if_else(is.na(engines),1,as.double(engines)))

# Mostra a dispersão do tamanho 
flight_plane_airline %>%
  ggplot(aes(x = carrier, y = tamanho)) +
  geom_jitter(aes(fill = as.factor(flight_plane_airline$engines),
                  color = as.factor(flight_plane_airline$engines)),
              shape = 21, alpha = 0.4) +
  scale_color_brewer(type = "div", palette = "Spectral", direction = -1, na.value = "gray75") +
  scale_y_continuous(limits = c(0,200),
                     breaks = c(0,25,50,75,100,125,150,175,200)) +
  labs(x = "Companhia Aérea", y = "Total Assento / Total Motores", colour = "Qtd. Motores") +
  theme(legend.position = "bottom")

# Mostra a distribuição do tamanho com a quantidade de voos
flight_plane_airline %>% 
  ggplot(aes(tamanho)) +
  geom_rect(aes(xmin = 0 , xmax = 50, ymin = 1, ymax = 35000, fill = 'red'), show.legend = FALSE) +
  geom_histogram(bins = 100, fill = 'cyan3', color = 'gray60') +
  scale_x_continuous(limits = c(0,200),
                     breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200),
                     labels = c('0','10','20','30','40','50','60','70','80','90','100','110','120','130',
                                '140','150','160','170','180','190','200')) +
  scale_y_log10(limits = c(1,35000),
                breaks = c(1,3,10,30,100,300,1000,3000,10000,30000),
                labels = c('1','3','10','30','100','300','1.000','3.000','10.000','30.000'))

flight_plane_airline

# Visualização por companhai aaerea
flight_plane_airline %>% group_by(carrier, tamanho) %>% summarise(qtd = n()) %>% 
  left_join(airline, by = c('carrier', 'carrier')) %>% 
  select(carrier,name,tamanho,qtd) %>% 
  ggplot(aes(x = carrier, y = tamanho)) +
  geom_point(aes(size = qtd, color = carrier)) +
  scale_y_continuous(limits = c(0,200),
                     breaks = c(0,25,50,75,100,125,150,175,200)) +
  guides(color = 'none') +
  labs(x = "Companhia Aérea", y = "Total Assento / Total Motores", size = "Quantidade de Voo") +
  theme(legend.position = "bottom")

# Percentual de ausência de dados de tamanho para a empresa American Airlines - AA
flight_plane_airline %>% filter(carrier == 'AA' & tamanho == 0) %>% nrow() /
  flight_plane_airline %>% filter(carrier == 'AA') %>% nrow()

# Percentual de ausência de dados de tamanho para a empresa Envoy Air - MQ
flight_plane_airline %>% filter(carrier == 'MQ' & tamanho == 0) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == 'AA') %>% nrow()

# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Apesar de no gráfico as empresas American Airlines (AA) e Envoy Air (MQ) possuírem grande quantidade
#             de voos com uma baixa relação Total de assentos / Total de Motores, isso não quer dizer necessaria-
#             mente que elas estão operando concentradas em voos com aeronaves com baixa relação. Investigando os
#             os dados mais detalhadamente, percebe-se que falta informação nos atributos que são relevantes para
#             a medição da relação como total de assentos e quantidade de motores das aeronaves.
# ----------------------------------------------------------------------------------------------------------------

plane %>% arrange(desc(engines)) %>% head(10)

# Percentual de NA's no atributo tamanho para a empresa JetBlue Airways - B6
flight_plane_airline %>% filter(carrier == 'B6' & tamanho == 0) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == 'B6') %>% nrow()  
# Percentual de tamanho menor que 50 para B6
flight_plane_airline %>% filter(carrier == 'B6' & tamanho <= 50) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == 'B6') %>% nrow()  


# Percentual de NA's no atributo tamanho para a empresa Endeavor Air Inc - 9E
flight_plane_airline %>% filter(carrier == '9E' & tamanho == 0) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == '9E') %>% nrow()  
# Percentual de tamanho menor que 50 para 9E
flight_plane_airline %>% filter(carrier == '9E' & tamanho <= 50) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == '9E') %>% nrow()  


# Percentual de NA's no atributo tamanho para a empresa ExpressJet Airlines Inc - EV
flight_plane_airline %>% filter(carrier == 'EV' & tamanho == 0) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == 'EV') %>% nrow()  
# Percentual de tamanho menor que 50 para EV
flight_plane_airline %>% filter(carrier == 'EV' & tamanho <= 50) %>% nrow() / 
  flight_plane_airline %>% filter(carrier == 'EV') %>% nrow()  


# ----------------------------------------------------------------------------------------------------------------
# OBSERVAÇÃO: Considerando como limite de corte para a relação Total de assentos / Qtd. Motores o valor de 50.
#             Isso quer dizer que a aeronave possui 50 passageiros para cada motor que ela possui.
#             Considerando o limite de corte, temos 3 empresas em situação desfavorável considerando esse limite.
#             JetBlue Airways (B6) que possui 30% de seus voos concentrados em aeronaves com essa relação menor
#             que 50.
#             A Endeavor Air Inc (9E) e ExpressJet Airlines Inc (EV) possuem todos os seus voos sendo feitos por
#             aeronaves com a relação de corte menor que 50.
# ----------------------------------------------------------------------------------------------------------------


# ----- CONCLUSÃO 4 ------------------------------------------------------------------------------------------
# As empresas Endeavor Air Inc (9E) e ExpressJet Airlines Inc (EV) estão operando somente com aeronaves de
# baixa relação Total de assentos / Total de motores. Isso implica em um ganho por passageiro menor que 
# outras companhias que operam com aeronaves com essa relação maior.
# A companhia JetBlue Airways (B6) opera com 30% de seus voos com aeronaves de baixa relação.
# ============================================================================================================



# ===== PROBLEMA 5 =========================================================================================
#  Uma grande preocupação na área da aviação é a visibilidade do piloto. Sabe-se que os estudos sobre as 
# condições climáticas influenciam diretamente nessa variável e “a diretoria” gostaria de entender melhor 
# essa relação.
# ----------------------------------------------------------------------------------------------------------

weather


# ----- CONCLUSÃO 5 ------------------------------------------------------------------------------------------
# 
# ============================================================================================================

  
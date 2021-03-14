######################################################
############ TP - Controle de Qualidade ##############
##### Alunas: Amanda Xavier e Larissa Carolina #######

# Carregando os pacotes:
pac <- c("readxl", "qcc", "tidyverse", "kableExtra")
lapply(pac, require, character.only = TRUE)

# Lendo os dados:
d1<-read_excel("dados.xlsx", sheet = "Bloodsugar")

# Manipulacao dos dados:
dados <- d1 %>% 
  mutate(SubjectID = as.character(SubjectID)) %>% 
  pivot_wider(names_from = SubjectID, values_from = Glucoselevel)

# Carta de controle: Amplitude
qcc(dados[,], type="R")

# Carta de controle: Media
qcc(dados[,], type="xbar")

# Nenhum ponto saiu dos limites de controle, tanto do grafico de R quanto no de Xbar.
# Portanto, seguindo a vida. Vou iniciar a analise de capacidade.

q <- qcc(dados, type="xbar", nsigmas=3, plot=FALSE)

process.capability(q, spec.limits=c(80,120),target=100)

# DÃºvida: Qual e o valor alvo?

t<- d1 %>% 
  mutate(Paciente = case_when(SubjectID==1 ~ "Paciente 1",
                              SubjectID==2 ~ "Paciente 2",
                              SubjectID==3 ~ "Paciente 3",
                              SubjectID==4 ~ "Paciente 4",
                              SubjectID==5 ~ "Paciente 5",
                              SubjectID==6 ~ "Paciente 6",
                              SubjectID==7 ~ "Paciente 7",
                              SubjectID==8 ~ "Paciente 8",
                              SubjectID==9 ~ "Paciente 9")) %>% 
  group_by(Paciente) %>% 
  summarize(
            Média = mean(Glucoselevel),
            Mediana = median(Glucoselevel),
            Máximo = max(Glucoselevel),
            Mínimo = min(Glucoselevel),
            Amplitude = Máximo - Mínimo,
            Desvio = round(sd(Glucoselevel),2)
            ) %>% 
  rename(" " = Paciente)


# Pra usar na formatação condicional da tabela:
max_media <- max(t$Média)
min_media <- min(t$Média)
max_mediana <- max(t$Mediana)
min_mediana <- max(t$Mediana)
max_maximo <- max(t$Máximo)
min_maximo <- min(t$Máximo)
max_minimo <- max(t$Mínimo)
min_minimo <- min(t$Mínimo)
max_amplitude <- max(t$Amplitude)
min_amplitude <- min(t$Amplitude)
max_desvio <- max(t$Desvio)
min_desvio <- min(t$Desvio)

# Tabelinha com formatação condicional
t %>%
  kbl() %>%
  kable_styling(full_width = F) %>% 
  column_spec(2, color = ifelse(t$Média==max_media, "#00008b", ifelse(t$Média==min_media, "#74AEDB", "black")), bold = ifelse(t$Média==max_media, T,ifelse(t$Média==min_media, T, F))) %>% 
  column_spec(3, color = ifelse(t$Mediana==max_mediana, "#00008b", ifelse(t$Mediana==min_mediana, "#74AEDB", "black")), bold = ifelse(t$Mediana==max_mediana, T,ifelse(t$Mediana==min_mediana, T, F))) %>% 
  column_spec(4, color = ifelse(t$Máximo==max_maximo, "#00008b", ifelse(t$Máximo==min_maximo, "#74AEDB", "black")), bold = ifelse(t$Máximo==max_maximo, T,ifelse(t$Máximo==min_maximo, T, F))) %>% 
  column_spec(5, color = ifelse(t$Mínimo==max_minimo, "#00008b", ifelse(t$Mínimo==min_minimo, "#74AEDB", "black")), bold = ifelse(t$Mínimo==max_minimo, T,ifelse(t$Mínimo==min_minimo, T, F))) %>% 
  column_spec(6, color = ifelse(t$Amplitude==max_amplitude, "#00008b", ifelse(t$Amplitude==min_amplitude, "#74AEDB", "black")), bold = ifelse(t$Amplitude==max_amplitude, T,ifelse(t$Amplitude==min_amplitude, T, F))) %>% 
  column_spec(7, color = ifelse(t$Desvio==max_desvio, "#00008b", ifelse(t$Desvio==min_desvio, "#74AEDB", "black")), bold = ifelse(t$Desvio==max_desvio, T,ifelse(t$Desvio==min_desvio, T, F)))

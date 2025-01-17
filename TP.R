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

# Dúvida: Qual e o valor alvo?

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
            M�dia = mean(Glucoselevel),
            Mediana = median(Glucoselevel),
            M�ximo = max(Glucoselevel),
            M�nimo = min(Glucoselevel),
            Amplitude = M�ximo - M�nimo,
            Desvio = round(sd(Glucoselevel),2)
            ) %>% 
  rename(" " = Paciente)


# Pra usar na formata��o condicional da tabela:
max_media <- max(t$M�dia)
min_media <- min(t$M�dia)
max_mediana <- max(t$Mediana)
min_mediana <- max(t$Mediana)
max_maximo <- max(t$M�ximo)
min_maximo <- min(t$M�ximo)
max_minimo <- max(t$M�nimo)
min_minimo <- min(t$M�nimo)
max_amplitude <- max(t$Amplitude)
min_amplitude <- min(t$Amplitude)
max_desvio <- max(t$Desvio)
min_desvio <- min(t$Desvio)

# Tabelinha com formata��o condicional
t %>%
  kbl() %>%
  kable_styling(full_width = F) %>% 
  column_spec(2, color = ifelse(t$M�dia==max_media, "#00008b", ifelse(t$M�dia==min_media, "#74AEDB", "black")), bold = ifelse(t$M�dia==max_media, T,ifelse(t$M�dia==min_media, T, F))) %>% 
  column_spec(3, color = ifelse(t$Mediana==max_mediana, "#00008b", ifelse(t$Mediana==min_mediana, "#74AEDB", "black")), bold = ifelse(t$Mediana==max_mediana, T,ifelse(t$Mediana==min_mediana, T, F))) %>% 
  column_spec(4, color = ifelse(t$M�ximo==max_maximo, "#00008b", ifelse(t$M�ximo==min_maximo, "#74AEDB", "black")), bold = ifelse(t$M�ximo==max_maximo, T,ifelse(t$M�ximo==min_maximo, T, F))) %>% 
  column_spec(5, color = ifelse(t$M�nimo==max_minimo, "#00008b", ifelse(t$M�nimo==min_minimo, "#74AEDB", "black")), bold = ifelse(t$M�nimo==max_minimo, T,ifelse(t$M�nimo==min_minimo, T, F))) %>% 
  column_spec(6, color = ifelse(t$Amplitude==max_amplitude, "#00008b", ifelse(t$Amplitude==min_amplitude, "#74AEDB", "black")), bold = ifelse(t$Amplitude==max_amplitude, T,ifelse(t$Amplitude==min_amplitude, T, F))) %>% 
  column_spec(7, color = ifelse(t$Desvio==max_desvio, "#00008b", ifelse(t$Desvio==min_desvio, "#74AEDB", "black")), bold = ifelse(t$Desvio==max_desvio, T,ifelse(t$Desvio==min_desvio, T, F)))

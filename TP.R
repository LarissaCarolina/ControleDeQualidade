######################################################
############ TP - Controle de Qualidade ##############
##### Alunas: Amanda Xavier e Larissa Carolina #######

# Carregando os pacotes:
pac <- c("readxl", "qcc", "tidyverse")
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
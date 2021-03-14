require(tidyverse)
require(readxl)
require(wesanderson)
require(qcc)

# Você está conduzindo um estudo sobre os níveis de glicose no sangue de 9 pacientes
#que seguem dietas rígidas e rotinas de exercícios.
#Você faz uma leitura de glicose no sangue todos os dias para cada paciente 
#durante 20 dias. A glicose sangüínea deve ser 
#100 +ou - 20 para atender às especificações médicas.

dados<- read_excel("Dados.xlsx", sheet =  "Bloodsugar") %>%
  rename("dia" = "Reading", "id" = "SubjectID" , "Glicose" ="Glucoselevel") %>%
  mutate(dia = as_factor(dia), id = as_factor(id))
##

summary(dados)

## Fixando o tem aclasico para todos
theme_set(theme_classic())


dados %>%
  group_by(id) %>% 
  summarise(Media = mean(Glicose)) %>%
  ggplot(aes(x = id, y = Media)) +
  geom_bar(stat="identity")


dados %>%
  group_by(dia) %>% 
  summarise(Media = mean(Glicose)) %>%
  ggplot(aes(x = dia, y = Media)) +
  geom_bar(stat="identity")
pal= "BottleRocket1"

dados %>%
  ggplot(aes(x = Glicose)) +
  geom_histogram()+
  scale_fill_manual(values = wes_palette("Royal1"))


dados %>%
  ggplot( aes(dia, id )) +
  geom_tile(aes(fill = Glicose), color = "white") +
  scale_fill_gradient(low = "#FBB8B8", high = "#AB0909") +
  ylab("Id") +
  xlab("Dia") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Glicose")

########################

## Analise de capacidade
## Colocando os dados no formato correto

dados <- spread(dados, id, Glicose)

##Para adicionar o bando de dados no R 

## Gráfico da amplitude
qcc(dados[,], type="R")
## Nenhum ponto fora dos limites

## Gráfico da media
qcc(dados[,], type="xbar")
##Nenhum ponto fora dos limites

q <- qcc(dados, type="xbar", nsigmas=3, plot=FALSE)

process.capability(q, spec.limits=c(80,120))


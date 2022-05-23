# Sun May 22 12:14:23 2022 ------------------------------
# Aves dos parques urbanos de São José dos Campos
# Trabalho de monografia Alessandra de Souza

#--- Carregando pacotes
library(tidyverse) # manipulação de dados
library(visdat) # checar missing data

#--- Load data
dir()
# Composição de espécies
composition <- read.csv2("sp_composition.csv")
head(composition)
composition <- composition[,-1]

# Dados de urbanização dos parques
abiotic <- read.csv2("abiotic_variables.csv")
head(abiotic)

# Dados de atributos funcionais para aves
traits <- read.csv2("traits_birds.csv")
View(traits)

#--- Data Transform
# Ajustando problemas de leitura do arquivo 
rownames(composition) <- composition[,1]
composition_ok <- composition[,-1]# erro na leitura, retirando coluna
# Trocando . por _ no nome das espécies para dar match com os traits
names(composition_ok) <- gsub(".", "_", fixed=T, names(composition_ok))
# Ordem alfabética para facilitar
composition_ok <- composition_ok[ , order(names(composition_ok))]

# Riqueza de cada parque e como isso está relacionado ao tamanho do parque?
# Birds richness - sum of each urban park
composition_rich <- composition_ok %>%
  mutate_at(c(names(composition_ok)), as.numeric) %>%
  mutate(Rich = rowSums(composition_ok))

# Ajustando dataset para incluir riqueza e dados abióticos
# transformar tamanho em variável numérica
abiotic$parque..m2. <- as.numeric(abiotic$parque..m2.)

# criar full data set para gráficos
localities <- cbind(composition, Rich = composition_rich$Rich)
abiotic_change <- rename(abiotic, localities = Parque.Urbano) %>% select(-richness)

# New dataset 
all_data <- left_join(localities, abiotic_change, by="localities") 
all_data <- rename(all_data, permm2 = permebealizado..m2.,  imperm2 = impermeabilizado..m2., size = parque..m2.) 

#Conferindo quais espécies da lista possuem traits
# Lista para join
# Arrumando para linhas
birds_list <- composition_ok %>%
  gather(key="sp",value="count", Accipiter_bicolor:Zonotrichia_capensis) %>% distinct(sp, .keep_all = TRUE)

nrow(birds_list) #327 espécies

# Conferindo espécies que saíram no short.traits (sp NA's)
# Anti join: espécies que estão no x, mas não estão no y (x,y)
birds_sp <- left_join(birds_list, traits, by="sp")
birds_sjc_traits <- birds_sp[,-2]

# checando missing data
write.csv(birds_sjc_traits, "traits_birds_sjc")
# Match lista de espécies e traits


  
#--- Data analysis
# RLQ - dados abióticos, traits e composição


#--- Data visualizing
ggplot(all_data, aes(x=log10(size), y=Rich))+
  geom_point() +
  labs(x = "Park size (log10)" , y= "Species richness", 
       title = "Urban birds") +
  stat_smooth(method = lm)+
  theme_bw()

library(tidyverse)
library(modelr)
library(sf)

idh <- read_csv("human-development-index-vs-mean-male-height.csv")
proteinass <- read_csv("share-of-calories-from-animal-protein-vs-mean-male-height.csv")
mort_inf <- read_csv('child-mortality-rate-vs-mean-male-height-cm.csv')
continentes <- read_csv("continents-according-to-our-world-in-data.csv")
alturas <- read_csv("NCD_RisC_Lancet_2020_height_child_adolescent_country.csv")
pbi <- read_csv("gdp-per-capita-in-us-dollar-world-bank.csv")
alturas18 <- read_csv('NCD_RisC_eLife_2016_height_age18_countries.csv')
desnutricion <- read_csv('desnutricion.csv')
life_ex <- read_csv('life-expectancy.csv')
gasto_sanitario <- read_csv('life-expectancy-vs-healthcare-expenditure.csv')

dtf <- read_csv("dataset_final2.csv")
dtf <- dtf %>% select(-c("...1"))
pbi2 <- read_csv("maddison-data-gdp-per-capita-in-2011us-slopechart.csv")

pbi2 <- pbi2 %>% select(-c("Entity","Continent","417485-annotations"))

pbi <- pbi %>%
  rename('PBI_per_capita' = "PBI per capita")

pbi <- pbi %>%
  arrange(Entity)

continentes <- continentes %>%
  select(-c('Entity','Year'))


idh2 <- idh %>% left_join(continentes,by=c('Code'))

idh2 <- idh2 %>%
  select(-c('Continent.x'))

idh3 <- idh3 %>%
  select(-c('altura_promedio'))

idh2 <- idh2 %>%
  rename("IDH" = "Human Development Index",
         "altura_promedio" = "Mean male height (cm)")

mort_inf <- mort_inf %>%
  rename("Tasa_mortalidad" = "Mortality rate, under-5 (per 1,000 live births)")


mort_inf <- mort_inf %>%
  rename("altura_promedio" = "Mean male height (cm)")%>%
  select(-c("Continent"))

mort_inf <- mort_inf %>%
  select(-c("Entity","altura_promedio"))

dtf <- dtf %>% left_join(mort_inf,by=c("Code","Year"))

proteinas <- proteinas %>%
  rename("calorias_proteina" = "Daily caloric intake per person that comes from animal protein")%>%
  select(-c("Continent","Entity","Mean male height (cm)"))




life_ex <- life_ex %>%
  rename("expectativa_vida" = "Life expectancy at birth (historical)")%>%
  select(-c(Entity))





pbi <- pbi %>% 
  select(-c("Entity"))




gasto_sanitario <- gasto_sanitario %>%
  select(-c("Entity","Life expectancy at birth, total (years)",
            "Population (historical estimates)","Continent")) %>%
  rename("gasto_sanitario" = "Current health expenditure per capita, PPP (current international $)")





desnutricion <- desnutricion %>%
  select(-c('Entity')) %>%
  rename("prevalencia_desnutricion(%)" = "Prevalence of undernourishment (% of population)")





write.csv(join6,"df_Casi.csv",fileEncoding = "UTF-8")



alturas18 <- alturas18 %>%
  select(-c(`Mean height upper 95% uncertainty interval (cm)`,
            `Mean height lower 95% uncertainty interval (cm)`)) %>%
  rename("Year" = `Year of birth`,"Code" = "ISO", "Entity" = "Country",
         "altura_promedio" = `Mean height (cm)`) %>%
  filter(Sex == "Men")

alturas18 <- alturas18 %>%
  select(-c("Sex"))



alturas2 <- alturas %>%
  filter(Year > 2014) %>% filter(`Age group` == 19)%>%filter(Sex == "Boys")%>%
  select(-c(`Mean height standard error`,`Mean height lower 95% uncertainty interval`,
            `Age group`,'Sex',`Mean height upper 95% uncertainty interval`))



alturas2 <- alturas2 %>% rename("Entity" = "Country","altura_promedio" = `Mean height`)




iso <- alturas18 %>% select(-c('Year','altura_promedio'))



df2 <- df2 %>% select(-c("ISO"))

iso  <- iso %>% distinct(Entity, Code)
alturas2 <- left_join(alturas2, iso, by = "Entity")

new_df <- new_df %>% arrange('Country')

new_df <- new_df %>% select(-c('Entity'))



conteo_paises <- iso %>%
  group_by(Entity) %>%
  count()

join6 <- join6 %>% select(-c('Years'))

alturas18 <- alturas18 %>% rename('altura_promedio' = `Mean height (cm)`)
alturas2 <- alturas2 %>% rename('altura_promedio' = `Mean height`)


alturas18 <- alturas18 %>%
  mutate(Year = Year +18)


dtf <- dtf%>% rename("prevalencia_desnutricion" = "prevalencia_desnutricion(%)")

dtf <- dtf%>% rename("PBI_per_capita" = `PBI per capita`)


join1 <- join1 %>%select(-c(`Mean height`))

alturas18 <- alturas18 %>% 
  mutate(Entity = ifelse(Entity == 'DR Congo', 'Democratic Republic of the Congo', Entity))

congo <- alturas18 %>% filter(Code == 'COD')

iso <- iso %>% 
  mutate(Entity = ifelse(Code == "COD", "Democratic Republic of the Congo", Entity))


alturas2 <- alturas2 %>% 
  mutate(Entity = case_when(
    Entity == 'Brunei Darussalam' ~ 'Brunei',
    Entity == 'Cabo Verde' ~'Cape Verde',
    Entity == 'China (Hong Kong SAR)'~'Hong Kong',
    Entity == 'Czech Republic' ~ 'Czechia',
    Entity == 'DR Congo'~'Democratic Republic of the Congo',
    Entity == 'Guinea Bissau'~'Guinea-Bissau',
    Entity == 'Lao PDR'~'Laos',
    Entity == 'Macedonia (TFYR)'~'North Macedonia',
    Entity == 'Micronesia (Federated States of)'~'Micronesia (country)',
    Entity == 'Occupied Palestinian Territory'~'Palestine',
    Entity == 'Russian Federation'~'Russia',
    Entity == 'Swaziland'~'Eswatini',
    Entity == 'Syrian Arab Republic'~'Syria',
    Entity == 'Timor-Leste'~'East Timor',
    Entity == 'United States of America'~'United States',
    Entity == 'Viet Nam'~'Vietnam',
    TRUE ~ Entity
  ))


congo2 <- alturas2 %>% filter(Code == "COD")

final1 <- bind_rows(alturas18, alturas2)

iso <- iso %>% mutate(Entity = ifelse(Entity == 'United States', 'USA', Entity))


final1 <- final1 %>% arrange(Entity)

dtf <- dtf %>% rename('Tasa_mortalidad_niños' = "Tasa_mortalidad")
dtfmod <- dtf %>% filter(Year > 2000 & Year <2015)
dtfg <- dtf %>% filter(Year <2015)


idh3 <- idh3 %>% select(-c("Entity"))
join1 <- final1 %>% left_join(idh3,by=(c('Code',"Year")))
join1 <- join1 %>% filter(Code != 'COG')

join2 <- join1 %>% left_join(proteinas,by=c("Code","Year"))

join3 <- join2 %>% left_join(life_ex,by=c("Code","Year"))

join4 <- join3 %>% left_join(pbi,by=c("Code","Year"))

join5 <- join4 %>% left_join(gasto_sanitario,by=c("Code","Year"))

join6 <- join5 %>% left_join(desnutricion,by=c("Code","Year"))

mort_inf <- mort_inf %>% select(-c("Entity","Mean male height (cm)","Continent"))

mort_inf <- mort_inf %>% rename("Tasa_mortalidad_niños" = `Mortality rate, under-5 (per 1,000 live births)`)
write.csv(join7,"dataset_final2.csv",fileEncoding = "UTF-8")

df <- join6 %>% filter(Year < 2015)

dtf$Decada <- floor(dtf$Year / 10) * 10
dtf$Decada <- as.factor(dtf$Decada)

join7 <- join6 %>% filter(Year > 1949)




dtf <- dtf %>% left_join(mort_inf,by=(c("Code","Year")))

dtf <- dtf %>% select(-c("IDH"))

dtf <- dtf %>%
  mutate(Continente = case_when(
    Entity == "American Samoa" ~ "Oceania",
    Entity == "Angola" ~ "Africa",
    Entity == "Andorra" ~ "Europe",
    Entity == "Antigua and Barbuda" ~ "North America",
    Entity == "Bermuda" ~ "North America",
    Entity == "Burkina Faso" ~ "Africa",
    Entity == "Cape Verde" ~ "Africa",
    Entity == "Chad" ~ "Africa",
    Entity == "Comoros" ~ "Africa",
    Entity == "Dominica" ~ "North America",
    Entity == "East Timor" ~ "Oceania",
    Entity == "Equatorial Guinea" ~ "Africa",
    Entity == "Eritrea" ~ "Africa",
    Entity == "Ethiopia" ~ "Africa",
    Entity == "French Polynesia" ~ "Oceania",
    Entity == "Georgia" ~ "Europe",
    Entity == "Greenland" ~ "North America",
    Entity == "Kiribati" ~ "Oceania",
    Entity == "Lebanon" ~ "Asia",
    Entity == "Liberia" ~ "Africa",
    Entity == "Madagascar" ~ "Africa",
    Entity == "Micronesia" ~ "Oceania",
    Entity == "Montenegro" ~ "Europe",
    Entity == "Nauru" ~ "Oceania",
    Entity == "Nigeria" ~ "Africa",
    Entity == "North Korea" ~ "Asia",
    Entity == "Oman" ~ "Asia",
    Entity == "Palau" ~ "Oceania",
    Entity == "Palestina" ~ "Asia",
    Entity == "Puerto Rico" ~ "North America",
    Entity == "Saint Kitts and Nevis" ~ "North America",
    Entity == "Saint Vincent and the Granadines" ~ "North America",
    Entity == "Seychelles" ~ "Africa",
    Entity == "Somalia" ~ "Africa",
    Entity == "Tokelau" ~ "Oceania",
    Entity == "Taiwan" ~ "Asia",
    Entity == "Turkmenistasn" ~ "Asia",
    Entity == "Uzbekistan" ~ "Asia",
    Entity == "Vanuatu" ~ "Oceania",
    TRUE ~ Continente
  ))

dtf <- dtf %>%
  mutate(Continente = case_when(
    Entity == "Bhutan" ~ "Asia",
    Entity == "Bosnia and Herzegovina" ~ "Europe",
    Entity == "Guinea-Bissau" ~ "Africa",
    Entity == "Micronesia (country)" ~ "Oceania",
    Entity == "Palestine" ~ "Asia",
    Entity == "Suriname" ~ "South America",
    Entity == "Turkmenistan" ~ "Asia",
    TRUE ~ Continente
  ))
















################# graficos #############################
idh3 %>% 
  filter(Year == 1995) %>% 
  ggplot() + geom_point(aes(x = IDH, y = altura_promedio,color=Continente))



idh3 <- idh2 %>% filter(complete.cases(Continente))
borrar <- idh3 %>% filter(is.na(altura_promedio))

ggplot(borrar3,aes(x=Continent, y=promedio,fill = Sex)) + 
  geom_bar(stat='identity', position = 'dodge') + 
  coord_cartesian(ylim = c(155,180))+
  labs (title = 'Promedio de altura por continente, año 1985')+
  facet_grid(~Year)

library(ggplot2)

# Crear un gráfico de dispersión con líneas de tendencia por año
ggplot(dtf, aes(x = `PBI per capita`, y = altura_promedio, color = as.factor(Year))) +
  geom_point() +
  labs(x = "PBI per cápita", y = "Altura promedio", title = "Relación entre PBI per cápita y altura promedio") +
  scale_color_discrete(name = "Año") +
  theme_minimal()


library(ggplot2)
library(dplyr)

# Agrupar los datos por décadas y calcular la altura promedio y el PBI per cápita promedio para cada década
dtf_decadas <- dtf %>%
  mutate(Decada = floor(Year / 10) * 10) %>%
  group_by(Decada) %>%
  summarise(AlturaPromedio = mean(altura_promedio, na.rm = TRUE),
            PBIPromedio = mean(`PBI per capita`, na.rm = TRUE))


# Agrupar los datos por décadas y seleccionar los valores de altura promedio y PBI per cápita para cada década
dtf_decadas2 <- dtf %>%
  mutate(Decada = floor(Year / 10) * 10) %>%
  group_by(Decada) %>%
  reframe(altura_promedio = altura_promedio, PBI_per_capita = PBI_per_capita)

# Crear un boxplot para comparar la relación entre PBI per cápita y altura promedio por década
dtf_decadas2 %>% filter(Decada > 1950) %>%
  ggplot(aes(x = as.factor(Decada), y = altura_promedio)) +
  geom_boxplot(fill = 'lightblue') +
  labs(x = "Década", y = "Altura promedio", title = "Altura promedio por década") +
  theme_minimal()


ggplot(dtf_decadas2, aes(x = as.factor(Decada), y = altura_promedio)) +
  geom_boxplot(fill = 'red') +
  labs(x = "Década", y = "Altura promedio", title = "Relación entre PBI per cápita y altura promedio por década") +
  theme_minimal()

ggplot(dtf_decadas2, aes(x = as.factor(Decada), y = PBI_per_capita)) +
  geom_boxplot(fill = 'blue') +
  labs(x = "Década", y = "PBI per cápita", title = "Relación entre PBI per cápita y altura promedio por década") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 18000))

# Graficar el scatter plot con la variable "Decada" como color
ggplot(dtf, aes(x = calorias_proteina, y = altura_promedio, color = Decada)) +
  geom_point() +
  labs(x = "Calorías de Proteína", y = "Altura Promedio", color = "Década") +
  theme_minimal()






##################################### MODELOS #####################

mod1 = lm(altura_promedio ~ PBI_per_capita, data=na.omit(dtfmod))
summary(mod1)

mod2 = lm(altura_promedio ~ Continente * PBI_per_capita + expectativa_vida, data=na.omit(dtf))
summary(mod2)


mod3 = lm(altura_promedio ~ Continente * (log(PBI_per_capita) + expectativa_vida), data=na.omit(dtfmod))
summary(mod3)

mod35 <- lm(altura_promedio ~ Continente * (PBI_per_capita + prevalencia_desnutricion), data = na.omit(dtfmod))
summary(mod35)


mod4 = lm(altura_promedio ~ Continente * (PBI_per_capita+
                                            Tasa_mortalidad_niños), data=na.omit(dtfmod))
summary(mod4)

mod5 = lm(altura_promedio ~ Continente + (PBI_per_capita + expectativa_vida), data=na.omit(dtfmod))
summary(mod5)

mod6 = lm(altura_promedio ~ Continente + (PBI_per_capita+
                                            Tasa_mortalidad_niños), data=na.omit(dtfmod))
summary(mod6)


mod7 = lm(altura_promedio ~ Continente + (PBI_per_capita + expectativa_vida +
                                            Tasa_mortalidad_niños + calorias_proteina+
                                            gasto_sanitario + prevalencia_desnutricion), data=na.omit(dtfmod))
summary(mod7)


mod8 = lm(altura_promedio ~ Continente* (PBI_per_capita+
                                            calorias_proteina), data=na.omit(dtfmod))
summary(mod8)

mod9 = lm(altura_promedio ~ Continente + (PBI_per_capita+
                                            calorias_proteina), data=na.omit(dtfmod))
summary(mod9)


mod10 = lm(altura_promedio ~ Continente * (expectativa_vida + Tasa_mortalidad_niños + 
                                             calorias_proteina), data=na.omit(dtfmod))
summary(mod10)


mod12 = lm(altura_promedio ~ Continente + (expectativa_vida + Tasa_mortalidad_niños + 
                                             calorias_proteina), data=na.omit(dtf))
summary(mod12)


mod12 = lm(altura_promedio ~ Continente + (expectativa_vida + Tasa_mortalidad_niños + 
                                             calorias_proteina+log(PBI_per_capita)+
                                             prevalencia_desnutricion+gasto_sanitario), data=na.omit(dtf))
summary(mod12)

mod13 = lm(altura_promedio ~ Continente * (expectativa_vida* calorias_proteina * 
                                             prevalencia_desnutricion * Tasa_mortalidad_niños),
                                            data=na.omit(dtf))
summary(mod13)

mod_log <- lm(altura_promedio ~ log(PBI_per_capita) * Continente
              ,data=na.omit(dtfmod))

summary(mod_log)

res_log <- dtfmod %>% add_residuals(model = mod_log) %>%
  add_predictions(model = mod_log)


ggplot(res_log,aes(x=pred,y=resid)) +geom_point()+
  geom_hline(aes(yintercept=0)) + geom_smooth() + labs(title = "log")



res10<-dtf %>% add_residuals(model = mod10) %>% add_predictions(model = mod10)

res12<-dtf %>% add_residuals(model = mod12) %>% add_predictions(model = mod12)

res13<-dtf %>% add_residuals(model = mod13) %>% add_predictions(model = mod13)

res3<-dtf %>% add_residuals(model = mod3) %>% add_predictions(model = mod3)

res4<-dtf %>% add_residuals(model = mod4) %>% add_predictions(model = mod4)
res5<-dtf %>% add_residuals(model = mod5) %>% add_predictions(model = mod5)
res6<-dtf %>% add_residuals(model = mod6) %>% add_predictions(model = mod6)

res8<-dtf %>% add_residuals(model = mod8) %>% add_predictions(model = mod8)
res9<-dtf %>% add_residuals(model = mod9) %>% add_predictions(model = mod9)
reslog <-dtf %>% add_residuals(model = mod_log) %>% add_predictions(model = mod_log)

ggplot(res13,aes(x=pred,y=resid)) +geom_point()+
  geom_hline(aes(yintercept=0)) + geom_smooth()+
  theme_minimal()+ labs(title = "Gráfico Residuos modelo 2")




mod_log <- lm(altura_promedio ~ Continente *(log(PBI_per_capita)+
                calorias_proteina+expectativa_vida+Tasa_mortalidad_niños),data=na.omit(dtfmod))
summary(mod_log)

modsen <- lm(altura_promedio ~ Continente * log(PBI_per_capita),data=na.omit(dtf))
summary(modsen)


############################ GRAFICOS #####################################

dtfg <- dtfg %>% filter(!is.na(Continente))


dtfg %>%
  filter(Year == 1980) %>%
  ggplot(aes(x = Tasa_mortalidad_niños, y = expectativa_vida)) +
  geom_point(aes(color = Continente)) +
  guides(alpha = FALSE) +
  labs(title = 'Tasa de mortalidad infantil vs altura promedio (1960)',
       x = 'Tasa de mortalidad infantil',
       y = 'Altura promedio')

dtfg %>%
  filter(Year == 2014) %>%
  ggplot(aes(x = PBI_per_capita, y = expectativa_vida)) +
  geom_point(aes(color = Continente)) +
  guides(alpha = FALSE) +
  labs(title = 'PBI per cápita vs esperanza de vida(2019)',
       x = 'PBI per cápita',
       y = 'Esperanza de vida' + coord_cartesian(xlim=c(0,90000)))


dtfg %>%
  ggplot(aes(x = Tasa_mortalidad_niños, y = altura_promedio)) +
  geom_point(aes(color = Continente,alpha=0.5)) +
  geom_smooth()+facet_wrap(~Continente)

dtfg %>%
  filter(Year == 2014) %>%
  ggplot(aes(x = Tasa_mortalidad_niños, y = altura_promedio)) +
  geom_point(aes(color = Continente))



dtfg %>% filter(Year == 2014) %>% filter(PBI_per_capita<75000)%>%
  ggplot(aes(x=PBI_per_capita,y=Tasa_mortalidad_niños)) +
  geom_point(aes(color = Continente))+geom_smooth()



dtfg %>% filter(Year == 2002) %>%
  ggplot(aes(x =gasto_sanitario,y=altura_promedio)) +
  geom_point(aes(color = Continente))+geom_smooth()


dtfg %>% filter(Year == 1980)%>%
  ggplot(aes(x =calorias_proteina,y=altura_promedio)) +
  geom_point(aes(color = Continente))+geom_smooth()



library(maps)
library(ggplot2)
library(dplyr)

# Cargar los datos del mapa mundial
world_map <- map_data("world")



world_map <- world_map %>% rename("Entity" = 'region') %>%
  left_join(iso,by='Entity') %>% filter(!is.na(Code))

# Realizar la unión entre el mapa mundial y los datos de altura promedio
map_data <- left_join(world_map, dtf_2019, by = "Code")

dtf_2019 <- dtf %>% filter(Year == 2019)


# Crear un mapa mundial coloreado por la altura promedio
map_data %>% filter (PBI_per_capita < 65000) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = PBI_per_capita)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  coord_equal() +
  theme_void() + labs(title='PBI per capita en 2019')




write.csv(dtf,"ult_ver.csv",fileEncoding = "UTF-8")


dtfg %>% filter(Code == "MLI") %>% 
  ggplot(aes(x=Year,y=PBI_per_capita)) + geom_line(color='red')+
  theme_minimal()

df_promedio <- dtfg %>%
  group_by(Continente, Year) %>%
  summarize(promedio_altura = mean(altura_promedio, na.rm = TRUE))


# Obtener los colores únicos de los continentes en tus datos
colores_continente <- unique(df_promedio$Continente)

# Definir un vector de colores para todas las líneas
colores <- c("blue", "red", "green", "purple", "orange")

# Reemplazar el color correspondiente a Oceania en el vector de colores
colores[df_promedio$Continente == "Oceania"] <- "green"
  
# Crear el gráfico con los colores personalizados
ggplot(df_promedio, aes(x = Year, y = pbi, color = Continente)) +
  geom_line(size = 1.15) +
  labs(x = "Año", y = "Altura Promedio", color = "Continente") +
  theme_minimal() +
  scale_color_manual(values = colores)



# Crear el gráfico de densidad
dtf %>% filter(Year == 1960) %>% 
  ggplot(aes(x = altura_promedio)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(x = "Altura promedio", y = "Densidad") +
  theme_minimal() + labs(title="1960")






grid <- data_grid(dtf, PBI_per_capita, Continente) %>% add_predictions(model = modsen)

ggplot(mapping = aes(x = PBI_per_capita, y = altura_promedio, color = Continente)) +
  geom_point(data = dtf,aes(alpha=0.5)) +
  geom_line(data = grid, aes(y = pred, group = Continente))+
  labs(title="PBI per cápita vs altura promedio",x='PBI per cápita',
       y= 'Altura promedio')+
  facet_wrap(~Continente)+
  guides(alpha = FALSE)+theme_minimal()




dtf %>% filter(Year == 1960) %>%
  ggplot(aes(x = Continente, y = altura_promedio)) +
  geom_boxplot(aes(fill = Continente)) +
  labs(x = "Continente", y = "Altura", title = "Altura por continente en 1960") +
  theme_minimal()




top_countries <- dtf %>% filter(Year == 2019)%>%
  arrange(desc(altura_promedio)) %>%
  head(10)

bottom_countries <- dtf %>% filter(Year == 2019) %>%
  arrange(altura_promedio) %>%
  head(10)

ggplot(top_countries, aes(x = reorder(Entity, altura_promedio), y = altura_promedio)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "País", y = "Altura promedio") +
  geom_text(aes(label = round(altura_promedio, 2)), vjust = -0.5) +
  theme_minimal() + coord_cartesian(ylim=c(178,185))+
  labs(title="Altura promedio por continente a través de los años")






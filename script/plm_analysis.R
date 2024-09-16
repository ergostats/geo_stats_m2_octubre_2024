library(haven)
library(tidyverse)
library(plm)
library(broom)

datos <- read_dta("http://dss.princeton.edu/training/Panel101.dta")

# Evolución de cada país en el tiempo

datos %>% 
  count(country)

# Panel balanceado

datos %>% 
  ggplot(aes(x = year, y = y, color = as_factor(country))) +
  geom_line() +
  facet_wrap(.~country)

# Heterogeneidad entre paises (con Y)

datos %>% 
  group_by(country) %>% 
  summarise(media = mean(y),
            std = sd(y)) %>% 
  mutate(lower = media - 1.96*std/sqrt(10),
         upper = media + 1.96*std/sqrt(10)) %>% 
  ggplot(aes(x =  as_factor(country), y = media)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

# Heterogeneidad en los años

datos %>% 
  group_by(year) %>% 
  summarise(media = mean(y),
            std = sd(y)) %>% 
  mutate(lower = media - 1.96*std/sqrt(10),
         upper = media + 1.96*std/sqrt(10)) %>% 
  ggplot(aes(x =  as_factor(year), y = media)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))


# Regresión contra x1

datos <- datos %>% 
  mutate(x1 = as.numeric(zap_formats(x1)),
         y = as.numeric(zap_formats(y))) 


# La relación entre y y x1

modelo <- lm(y ~x1, data = datos)

datos %>% 
  ggplot(aes(x = x1, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")

# Un modelo de efectos fijos pero con variables dummy

modelo1 <- lm(y ~x1 + as_factor(country), data = datos)

modelo1 %>% 
  summary()

attributes(modelo)

datos["ajustados"] <- modelo$fitted.values

datos %>% 
  ggplot( ) +
  geom_point(aes(x = x1, y = y,color = as_factor(country)), alpha = 0.2)  +
  geom_point(aes(x = x1, y = ajustados,color = as_factor(country)))  


# Entonces hacemos un modelo de efectos fijos con n interceptos y modelo within

modelo_plm <- plm(y ~ x1, index = c("country","year"),model = "within", data = datos) 

attributes(modelo_plm)

plm::fixef(modelo_plm)

summary(modelo_plm)

pFtest(modelo_plm, modelo)



datos %>%
  ggplot( aes(x = x1, y = y, group = as_factor(country),color = as_factor(country))) +
  geom_point() +  # Gráfico de dispersión de puntos
  geom_smooth(method = "lm", se = FALSE, lwd = 1.5) +  # Agregar la regresión OLS en rojo
  labs(x = "x1", y = "yhat", title = "Least squares dummy variable model") +  # Etiquetas de los ejes
  theme_minimal() 

# Extraer los elementos ajustados de la regresión 

augment(modelo_plm) %>% 
  ggplot() +
  geom_histogram(aes(.resid))

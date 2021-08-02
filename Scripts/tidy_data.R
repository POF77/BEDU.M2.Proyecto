#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Crear bases de datos
# Fecha creación: 29/07/2021
# Última modificación: 02/08/2021
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# 0. Configuración -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(reshape2)

# 1. Importar datos ------------------------------------------------------------
ent <- "https://raw.githubusercontent.com/POF77/Proyecto.BEDU.ModuloII/main/Datos_crudos/"

df_gas0 <- read_csv(paste0(ent, "historical_emissions_gases.csv"))
df_gases0 <- read_csv(paste0(ent, "historical_emissions.csv"))
df_tempe0 <- read_csv(paste0(ent, "temperature_wb.csv"))
df_salud0 <- read_csv(paste0(ent, "mortality_who.csv"))
df_salud_country_code <- read_csv(paste0(ent, "mortality_who-country_code.csv"))


# 2.1 Tidy df_gas ---------------------------------------------------------
df_gas <- melt(df_gas0)

df_gas <- df_gas %>%
  rename(country = Country, source = `Data source`, year = variable,
         tot_gas = value) %>%
  select(country, year, tot_gas)

# 2.2 Tidy df_gases -------------------------------------------------------
df_gases <- df_gases0

df_gases <- melt(df_gases)

df_gases <- df_gases %>%
  rename(country = Country, year = variable, gas = Gas, source = `Data source`) %>%
  select(country, year, gas, value) %>%
  mutate(id = paste0(country, "-", year))

# Crear variable por gas
df_gases_all <- df_gases %>%
  filter(gas == "All GHG") %>%
  rename(all_gas = value)

df_gases_ch4 <- df_gases %>%
  filter(gas == "CH4") %>%
  rename(ch4 = value)

df_gases_co2 <- df_gases %>%
  filter(gas == "CO2") %>%
  rename(co2 = value)

df_gases_fgas <- df_gases %>%
  filter(gas == "F-Gas") %>%
  rename(fgas = value)

df_gases_n2o <- df_gases %>%
  filter(gas == "N2O") %>%
  rename(n2o = value)

df_gases <- left_join(df_gases_all, df_gases_ch4, by = "id")
df_gases <- left_join(df_gases, df_gases_co2, by = "id")
df_gases <- left_join(df_gases, df_gases_fgas, by = "id")
df_gases <- left_join(df_gases, df_gases_n2o, by = "id")

df_gases <- df_gases %>%
  select(country, year, all_gas, ch4, co2, fgas, n2o)

# 2.3 Tidy df_tempe ------------------------------------------------------------
df_tempe <- df_tempe0 %>%
  rename(country = Country, year = Year, month = Statistics, id_country = ISO3,
         tempe_c = `Temperature - (Celsius)`) %>%
  select(country, year, month, tempe_c)

df_tempe <- df_tempe %>%
  group_by(country, year) %>%
  summarise(mean_year_tempeC = mean(tempe_c))

# 2.4 Tidy df_salud ------------------------------------------------------------
df_salud <- df_salud0 %>%
  rename(country = Country, year = Year, cause = Cause, sex = Sex,
         tot_deaths = Deaths1) %>%
  select(country, year, cause, sex, tot_deaths, Deaths2:Deaths26)

# Integrar nombre de paíes
df_salud <- left_join(df_salud, df_salud_country_code, by = "country")

df_salud <- df_salud %>%
  mutate(country = name) %>%
  select(-name)

# Convertir a factor variable de edad
df_salud$sex <- factor(df_salud$sex,
                       levels = c(1,2),
                       labels = c("male", "female"))

# Sumar muertes respiratorias por país por año
df_salud <- df_salud %>%
  group_by(country, year) %>%
  summarise(year_deaths = sum(tot_deaths))

# 3. Unión de tablas -----------------------------------------------------------
## DF con años - paises completos
paises <- count(df_tempe, country)$country

df0 <- tibble(country = rep(paises, each = 122),
             year = rep((1900:2021), times = 197),
             id = paste0(country, "-", year))

## Agregar variable de ID a tablas
df_gas <- mutate(df_gas, id = paste0(country, "-", year))
df_gases <- mutate(df_gases, id = paste0(country, "-", year))
df_tempe <- mutate(df_tempe, id = paste0(country, "-", year))
df_salud <- mutate(df_salud, id = paste0(country, "-", year))

# Unir tablas
df <- left_join(df0, df_gas, by = "id")
df <- left_join(df, df_gases, by = "id")
df <- left_join(df, df_tempe, by = "id")
df <- left_join(df, df_salud, by = "id")

glimpse(df)
# 4. Ajustes df final ----------------------------------------------------------
df_bruta <- df %>%
  select(country.x:id, tot_gas, all_gas:n2o, mean_year_tempeC, year_deaths) %>%
  rename(country = country.x, year = year.x)

df_neta <- df_bruta %>%
  filter(!is.na(tot_gas) & !is.na(year_deaths))

# 5. Exportar tablas -----------------------------------------------------------
sal <- "C:/Users/Usuario/OneDrive - El Colegio de México A.C/2. Proyectos varios/1. BEDU/Módulo 2.2 Python/Proyecto/Datos_nuevos/"

write.csv(df_bruta, paste0("", "datos_brutos", ".csv"), fileEncoding = "UTF-8")
write.csv(df_neta, paste0("", "datos_netos", ".csv"), fileEncoding = "UTF-8")

library(tidyverse)
library(lubridate)
library(htmlwidgets)
library(DataExplorer)
#install.packages("reshape2")                               
library(reshape2)
#install.packages("ggplot2")           
library(ggplot2)
#install.packages("corrplot")
library(corrplot)
#install.packages("corrgram")
library(corrgram)
#install.packages("tidymodels")
library(tidymodels)
#install.packages("xgboost")
library(xgboost)
library(PerformanceAnalytics)
library(vip)
library(rsample)
#install.packages("ranger")
library(ranger)

#import date
df <- data.frame(read.csv("kc_house_data.csv"))

#verificare NA
#apply(df,2, function(x) is.na(x))
if (any(is.na(df))) {
  print("Conține NA.")
  print(sum(is.na(df)))
} else {
  print("Nu conține NA.")
}

#afisare coloane
names(df)

#eliminare coloane inutile, precum date, lat, long, id
df <- df |> select( -id, -date, -lat, -long)
nr_zipcode <- df |> group_by(zipcode) |> count()

#verificare tip de date 
sapply(df, class)

#transformare date
df <- mutate_all(df, as.numeric)


df$waterfront <- as.factor(df$waterfront)
levels(df$waterfront)
df$view <- as.factor(df$view)
levels(df$view)
df$condition <- as.factor(df$condition)
levels(df$condition)
df$grade <- as.factor(df$grade)
levels(df$grade)


#Deoarece nu exista valori lipsa, putem incepe:

# Analiza exploratorie a datelor.

#rezumat set de date
summary(df)

# informatii privind setul de date. nu exista valori lipsa.
temp <- DataExplorer::introduce(df)
#aceleasi informatii sub forma de grafic.
plot_intro(temp)


#dimensiunile setului de date (coloane, randuri)
nrow(df)
ncol(df)
dim(df)

#numele variabilelor din setul de date
names(df)

#afisare primele 6 linii, respectiv ultimele 6 linii
head(df)
tail(df)

#analiza statistica pentru variabila dependenta "price"
stats_price <- df |>
  summarise(
    n_of_houses = n(), #nr de case
    min_price = min(df$price), #val min
    first_quartile = quantile(df$price, 0.25), #prima quartila
    mean_price = mean(df$price), # media varstei
    median_price = median(df$price), # mediana salariului
    third_quartile = quantile(df$price, .75), # a treia quartila
    max_price = max(df$price), # val max
    trim_mean_price = mean(x = df$price, trim = 2), # 20% media trunchiata
    range_price = paste(range(df$price), collapse = '-'), # range
    iqr_price = IQR(df$price), # interval interquartilic IQ
    var_price = var(df$price), # varianta
    sd_price = sd(df$price), # abaterea standard
    skewness = PerformanceAnalytics::skewness(df$price), #asimetria
    kurtosis = PerformanceAnalytics::kurtosis(df$price) #boltirea
  )

View(stats_price)

# Analiza grafica a variabilelor numerice

#preluare variabile numerice.
numeric_var <- df %>%
  select_if(., is.numeric) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value")

#histograma de frecventa facuta cu ajutorul ggplot.
numeric_var %>%
  ggplot(., aes(x = value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable, scale = "free")+
  guides(fill = "none")+
  theme(axis.text.x = element_text(size = 9))+
  theme(strip.text.x = element_text(size = 12))+
  xlab("")+ylab("frequency")

#  histograme pentru var numerice 
DataExplorer::plot_histogram(df)

# densitatea variabilelor numerice
DataExplorer::plot_density(df)



# Analiza grafica a variabilelor nenumerice   
# tabele de frecventa pentru variabilele nenumerice
df |> select(waterfront) |> table() #frecvenet absolute
df |> select(view) |> table() #frecvenet absolute
df |> select(condition) |> table() #frecvenet absolute
df |> select(grade) |> table() #frecvenet absolute

#selectie variabile nenumerice
factor_var <- df %>%
  select_if(., is.factor) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise(n_val = n()) %>%
  ungroup() %>%
  mutate(percent = round(n_val*100/nrow(df), 2)) %>%
  arrange(variable, value)

#grafice pentru variabilele nenumerice. procente per categorie.
factor_var %>%
  group_by(variable) %>%
  summarise(n_of_var = n()) %>%
  ungroup() %>%
  select(variable) %>%
  inner_join(factor_var) %>%
  ggplot(., aes(x = value, y = n_val, fill = value))+
  geom_col()+
  geom_text(aes(label = paste0(round(percent, 0),'%'),
                vjust = if_else(n_val > 300, 1.5, -0.5)))+
  facet_wrap(~variable, scale = 'free')+
  guides(fill = FALSE)+
  theme(axis.text.x = element_text(size = 10, angle = 30, hjust = 1))+
  theme(strip.text.x = element_text(size = 14))+
  xlab("categories")+ylab("frequency")


#boxplot pt val extreme, nu sunt asa multe valori extreme
data_long <- melt(df) 
ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot()


boxplot(df, by = 'grade')
boxplot(df, by = 'condition')


#cum se modifica pretul in functie de conditie si gradul fiecarei case
ggplot(
  df,
  aes(x= price, y = grade, color = condition))+
  geom_line()+
  labs(title = "Price condition-grade",
       subtitle = paste(min(df$price), max(df$price), sep = ' - '))+
  theme(
    plot.title = element_text(colour = "darkblue", size = 14,
                              face = "bold", hjust = 0.5, lineheight = 1.2),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1)
  )


# Plot in care se observa ca in medie, casele care au un grad egal cu 7, au un pret mai ridicat.
ggplot(df, aes(x = grade, y = mean(price))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "",
       x = "Grade",
       y = "Mean_price")

#analiza de corelatie
corrplot::corrplot(cor(df %>% dplyr::  select_if(is.numeric) , 
                       method = "spearman"), method = "number", type = "upper")

#exista o corelatie puternica intre sqft_lot si sqft_lot15. de asemenea, si intre
# sqft_living si sqft_above
#sqft_living si sqft_living15
#bathrooms si sqft_living


#Asigură reproductibilitatea
set.seed(1234)
#75% din date vor fi folosite pentru antrenament și 25% pentru testare
splits   <- initial_split(df, prop = 0.75)
train_tbl <- training(splits)
test_tbl  <- testing(splits)
# Împărțim setul de antrenament pe foldere pentru validare încrucișată pentru a evita cazurile ce țin de noroc
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1)
cv_train



# Realizăm rețeta. Anticipăm price în funcție de toți ceilalți predictori
recipe_sc <- recipe(price ~ ., data = train_tbl) %>%
  #variabilele de tip factor le convertesc în 0 și 1
  step_dummy(all_nominal(), -all_outcomes()) %>%
  # Înlocuim valorile lipsă dacă există(cu cel mai apropiat vecin)
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  # Scoate predictorii fără variabilitate
  step_zv(all_predictors()) 


# Specificarea Modelului

# Random Forest
rf_spec <- rand_forest(
  # Hiperparammetru mtry pe care îl reglăm(nr. parametri)
  mtry = tune(), 
  trees = 500,
  # Numărul minim de noduri pentru a trece mai departe
  min_n = tune()     
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_spec

#  XGBoost
xgb_spec <- boost_tree(
  trees = 700, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     #complexitatea modelului
  sample_size = tune(), mtry = tune(),         # cât de random va fi
  learn_rate = tune()                         # mărimea pasului
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_spec


# Realizăm workflowurile
#Pentru Random Forest
wf_rf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(recipe_sc)
#Pentru XGBoost
wf_xgb <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(recipe_sc)



#  Generez liste cu valori posibile pentru cei doi hiperparametrii

set.seed(1234)
# 20 de combinații dintre cele 2 valori
rf_grid <- dials::grid_random(
  finalize(mtry(), train_tbl %>% select (-price)),
  min_n(),  
  size = 20)
rf_grid

set.seed(1234)
# 50 de combinații dintre hiperparametrii
xgb_grid <- dials::grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_tbl %>% select (-price)),
  learn_rate(),
  size = 50
)
xgb_grid



# Încărcăm modelele
set.seed(1234)
#pentru fiecare combinație de hiperparametrii construiește arbori pentru validare
rf_resamples <- wf_rf %>% 
  tune_grid(
    resamples = cv_train,
    grid = rf_grid
  )
rf_resamples


set.seed(1234)
xgb_resamples <- wf_xgb %>% 
  tune_grid(
    resamples = cv_train,
    grid = xgb_grid
  )
xgb_resamples

# Explorăm rezultatele și alegem cea mai bună combinație de hiperparametrii

# Metrici de performanță pentru fiecare grid 
rf_resamples %>% collect_metrics()
#r^2 trebuie să fie cel mai mare, rmse cel mai mic
autoplot(rf_resamples)

xgb_resamples %>% collect_metrics()
autoplot(xgb_resamples)



# Cea mai buna combinație pentru cele 5 foldere
best_rf <- rf_resamples %>% select_best("rmse")
best_rf

best_xgb <- xgb_resamples %>% select_best("rmse")
best_xgb

#Finalizăm workflowurile cu cei mai buni hiperparametrii

final_wf_rf <- wf_rf %>% 
  finalize_workflow(best_rf)

final_wf_xgb <- wf_xgb %>% 
  finalize_workflow(best_xgb)


set.seed(1234)
final_rf_train <- final_wf_rf %>%
  fit(data = train_tbl) 
final_rf_train

set.seed(1234)
final_xgb_train <- final_wf_xgb %>%
  fit(data = train_tbl) 
final_xgb_train


#Construim un model cu cea mai bună combinație pentru tot setul de test

set.seed(1234)
test__rf <- final_wf_rf %>% last_fit(splits) 
test__rf %>% collect_metrics() 

set.seed(1234)
test__xgb <- final_wf_xgb %>% last_fit(splits) 
test__xgb %>% collect_metrics() 
#Deci observăm că XGBoost este cel mai bun model de predicție în acest caz ce anticipează cu un procent de 85.6%


#Importanța variabilelor-estimează cei mai importanți predictori în variabilitatea rezultatului

set.seed(1234)
rf_imp_spec <- rf_spec %>%
  finalize_model(best_rf) %>%
  set_engine("ranger", importance = "permutation")

workflow() %>%
  add_recipe(recipe_sc) %>%
  add_model(rf_imp_spec) %>%
  fit(train_tbl) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))
# sqft_living este un predictor cu influență determinantă pentru preț

set.seed(1234)
xgb_imp_spec <- xgb_spec %>%
  finalize_model(best_xgb) %>%
  set_engine("xgboost", importance = "permutation")

workflow() %>%
  add_recipe(recipe_sc) %>%
  add_model(xgb_imp_spec) %>%
  fit(train_tbl) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "red"))
  
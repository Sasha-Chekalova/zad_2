#Чекалова, Вариант 9
#Задание 2. Cоздайте модель множественной линейной регрессии ночных потоков паров за период 2013 года по данным измерений методом турбулентной пульсации. 

rm(list=ls()) 

setwd("C:/R") 

getwd() 

library("tidyr") 

library("tibble") 

library("tidyverse") 

library("stringr") 

library("dplyr") 

library("ggplot2") 

#считаем данные иудалим ненужные строчки 

eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

eddypro = eddypro[-1, ] 

eddypro = select(eddypro, -(roll)) 

eddypro = eddypro %>% mutate_if(is.character, factor)

# назовем столбцы допустимыми символми 

names(eddypro) = names(eddypro) %>% 
  
  str_replace_all("[!]", "emph") %>% 
  
  str_replace_all("[?]", "quest") %>% 
  
  str_replace_all("[*]", "star") %>% 
  
  str_replace_all("[+]", "plus") %>% 
  
  str_replace_all("[-]", "minus") %>%   
  str_replace_all("[@]", "at") %>% 
  
  str_replace_all("[$]", "dollar") %>% 
  
  str_replace_all("[#]", "hash") %>% 
  
  str_replace_all("[/]", "div") %>% 
  
  str_replace_all("[%]", "perc") %>% 
  
  str_replace_all("[&]", "amp") %>% 
  
  str_replace_all("[\\^]", "power") %>% 
  
  str_replace_all("[()]", "L_") 

glimpse(eddypro) 

# отберем данные с 1 января по 31 декабря и по ночному времени 

eddypro = drop_na(eddypro) 

eddypro = filter(eddypro, DOY >= 1 & DOY < 365) 

eddypro = filter(eddypro, daytime==FALSE) 

eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ] 

eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ] 

cor_td= cor(eddypro_numeric) 

cor_td 

cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(h2o_flux) 

vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 

# создадим обучаающую и тестирующую выборки

row_numbers = 1:length(eddypro_numeric$h2o_flux) 

teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7)) 

test = row_numbers[-teach] 

teaching_tbl = eddypro_numeric[teach,] 

testing_tbl = eddypro_numeric[test,] 

#модель 1 

mod1 = lm(h2o_flux~ (.) , data = teaching_tbl) 

summary(mod1) 

coef(mod1) 

resid(mod1) 

confint(mod1) 

anova(mod1) 

plot(mod1) 

#модель 2 

mod2 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H 
          
          + rand_err_H + LE + qc_LE + rand_err_LE + rand_err_co2_flux 
          
          + rand_err_h2o_flux + H_strg + h2o_vminusadv 
          
         + h2o_mixing_ratio + h2o_molar_density + h2o_mole_fraction 
          
          + sonic_temperature + air_temperature 
       
          + air_heat_capacity + air_molar_volume + water_vapor_density + e + es 
          
          + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot 
          
          + wind_dir + yaw + pitch + ustar + TKE + L 
          
          + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ + x_90_perc_ + un_Tau 
          
          + H_scf + un_LE + LE_scf + un_h2o_flux + u_spikes + ts_var 
          
          + co2_var + w_div_h2o_cov + co2_signal_strength_7200 + flowrate, data= teaching_tbl) 

names(teaching_tbl) 

summary(mod2) 

coef(mod2) 

resid(mod2) 

confint(mod2) 

anova(mod2) 

anova(mod2, mod1) 

plot(mod2) 


#корреляционный анализ 

cor_teaching_tbl = select(teaching_tbl, h2o_flux, DOY , Tau , qc_Tau , rand_err_Tau , H , qc_H ,
                          
                           rand_err_H , LE , qc_LE , rand_err_LE , rand_err_co2_flux ,
                          
                           rand_err_h2o_flux , H_strg , h2o_v_minus_adv ,
                          
                           h2o_mixing_ratio , h2o_molar_density , h2o_mole_fraction ,
                          
                           sonic_temperature , air_temperature ,
                          
                           air_heat_capacity , air_molar_volume , water_vapor_density , e , es ,
                          
                           VPD , Tdew , u_unrot , v_unrot , w_unrot , u_rot , v_rot ,
                          
                           wind_dir , yaw , pitch , u_star_ , TKE , L ,
                          
                           x_offset , x_10_perc_ , x_30_perc_ , x_50_perc_ , x_70_perc_ , x_90_perc_ , un_Tau ,
                          
                           H_scf , un_LE , LE_scf , un_h2o_flux , u_spikes , ts_var ,
                          
                           co2_var , w_div_h2o_cov , co2_signal_strength_7200 , flowrate) 

cor_td = cor(cor_teaching_tbl) %>% as.data.frame 

qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod2, teaching_tbl))) 

qplot(h2o_flux , h2o_flux,data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl))) 

# примеры графиков 

qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl))) 

qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl))) 

qplot(co2_flux, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))


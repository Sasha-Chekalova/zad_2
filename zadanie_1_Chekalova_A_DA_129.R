#Чекалова Александра, ДА-129, вариант 9
#Регион 61, 
#регион 61, Ростовская область
# рассчитать урожайность пшеницы в 1999 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 2 года,
#с метеостанций на расстоянии от 0 до 100 км

rm(list=ls())
setwd("C:/sasha")
getwd()
#install.packages("tidyverse")
#install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
#скачиваем станции 
station_data = ghcnd_stations()
#записываем в файл для последующей работы 
#write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")

#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,


#создав таблицу с именем региона и координатами его столицы
rostov = data.frame(id = "rostov", latitude = 47.2313,  longitude = 39.7233)
rostov_around = meteo_nearby_stations(lat_lon_df = rostov, station_data = station_data, limit = 20, var = c("PRCP", "TAVG"), year_min = 1997, year_max = 1998)

# rostov_around это список единственным элементом которого является таблица, содержащая идентификаторы 
#метеостанций отсортированных по их 
# удалленности от Ростова, очевидно что первым элементом таблицы будет идентификатор метеостанции Ростова,
#его то мы и попытаемся получить

rostov_id = rostov_around[["rostov"]][["id"]][1]
#получение всех данных с метеостанций
summary (rostov_id)
str(rostov_around)
all_rostov_data = meteo_tidy_ghcnd(stationid = rostov_id)

#2)чтобы получить таблицу всех метеостанций вокруг Ростова нужно выбрать целиком первый объект из списка
rostov_table = rostov_around[[1]]
summary(rostov_table)
#в таблице оказалось 20 объектов, ранжированных по расстоянию от Ростова  
# отфильтруем все станции, на расстоянии от 0 до 100 км
#Это можно сделать или задав соответствующее условие в [,] или с помощью команды фильтр
ROSTOV_table = filter (rostov_table, distance > 0 & distance < 100 )

#нужно убедится, что этот список включает нужные по условию задачи метеостанции

rostov_stations = rostov_table 
str(rostov_stations)


#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
rostov_stations$id
# в таблице оказалось 20 станций в нужном нам километраже
###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
################## 3. Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте #след. команду
all_rostov_data = meteo_tidy_ghcnd(stationid = rostov_id)
#посмотрим, что же скачивается

?meteo_tidy_ghcnd
summary(all_rostov_data)
#скачиваются данные только с самой первой станции
#Подумаем, какие из этих данных нам нужны
##нам нужны среднесуточные температуры (tavg) выше 10 градусов за  1997-1998 гг.

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_rostov_meteodata = data.frame()    
#Цикл для всех метеостанций

for(i in 1:20) 
{ 
  all_i  = meteo_tidy_ghcnd(stationid =  rostov_around[["rostov"]][["id"]][i])
  
  #выберем нужные свойства 
  all_i = all_i[ ,c("id","date","tavg")] 
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном    #этапах цикла
  print(all_i)
  all_rostov_meteodata=rbind(all_rostov_meteodata, all_i)
}


#Записываем полученные результаты
#write.csv(all_rostov_meteodata,"all_rostov_meteodata.csv")

#2 часть
################## 4. Разбивка даты на составляющие(год, месяц, день года) 
# считываем данные из файла all_rostov_meteodata.csv
all_rostov_meteodata = read.csv("all_rostov_meteodata.csv")
#посмотрим на данные
str(all_rostov_meteodata)
#видим, что дата записана в формате "1963-01-02"
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)
# вытащить год
#проверим, что работает
y = year(all_rostov_meteodata$date); y
all_rostov_meteodata [,"year"]= year(all_rostov_meteodata$date)
#добавим месяц
all_rostov_meteodata [,"month"]= month(all_rostov_meteodata$date) 
#вытащить день от начала года
all_rostov_meteodata [,"day_of_the_year"]= yday(all_rostov_meteodata$date) 
#проверим результат
str(all_rostov_meteodata)    
#отфильтруем данные за 1997-1998
years_rostov_meteodata = filter (all_rostov_meteodata, year > 1996 & year < 1999 )   
#проверим результат
str(years_rostov_meteodata)
summary (years_rostov_meteodata)    
################## 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц
#Изучаем формулу и видим, что единственное, что нужно расчитать
#- это сумму температур больше 10 град. по месячно, остальное в формуле-  константы

#### 1.  температурy нужно поделить на 10
years_rostov_meteodata[,"tavg"]= years_rostov_meteodata$tavg / 10
summary (years_rostov_meteodata)
#### 2. Превратим в нули все NA и где tavg больше 10 градусов

years_rostov_meteodata [is.na(years_rostov_meteodata$tavg), "tavg"] = 0
years_rostov_meteodata [years_rostov_meteodata$tavg<10, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 10 градусов
summary(years_rostov_meteodata)
#### 3. суммарная температура за месяц за 12 лет для всех станций 
# группирую по метеостанциям, годам и месяцам

alldays= group_by(years_rostov_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum

sumT_alldays_rostov = summarize(alldays, tsum = sum(tavg))
#Получилось - все года, все месяца присутствуют
# максимальная суммарная температура за месяц 24.294

summary(sumT_alldays_rostov)
24.294
#Сгруппирем данные по месяцам  
groups_rostov_months = group_by(sumT_alldays_rostov,month)
groups_rostov_months
#найду для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months= summarize(groups_rostov_months , St = mean(tsum))
sumT_months



################## 6. Подготовка к расчету по формуле Урожая
### Ввод констант
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 #  - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры; 
# Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>5℃)
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)

#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам и думаем разумный ли он
Yield = sum(sumT_months$Yi)  
Yield
# Ответ: 20,6 ц/га 









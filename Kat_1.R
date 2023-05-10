#Крутина Екатерина, 132 группа, 7 вариант
#регион 5 - Дагестан
# для региона 5 рассчитайте урожайность пшеницы в 2007 году, взяв 
#для рассчета средние суммы активных температур за текущий год, с 20 ближайших метеостанций 
#на расстоянии до 250 км

rm(list=ls())
setwd("E:/R")
getwd()
#install.packages("tidyverse")
#install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
#скачиваем станции
station_data = ghcnd_stations()
#записываем в файл для последующей работы 
#write.csv(station_data, "stations.csv")
#station_data = read.csv("stations.csv")


#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,

#создав таблицу с именем региона и координатами его столицы

dag = data.frame(id = "dag", latitude = 42.9764,  longitude = 47.5024)

dag_around = meteo_nearby_stations(lat_lon_df = dag, 
                                   station_data = station_data,
                                   limit = 20, var = c("PRCP", "TAVG"), 
                                   year_min = 2006, year_max =2008)

#таблица, содержащая идентификаторы 

#метеостанций отсортированных по их 

# удалленности, очевидно что первым элементом таблицы будет идентификатор метеостанции,

#его то мы и попытаемся получить

? meteo_nearby_stations

dag_id = dag_around[["dag"]][["id"]][1]

#получение всех данных с метеостанций

summary(dag_id)

str(dag_around)
all_dag_data = meteo_tidy_ghcnd(stationid = dag_id)
#2)чтобы получить таблицу всех метеостанций вокруг нужно выбрать целиком первый объект из списка
dag_table = dag_around[[1]]
summary(dag_table)
#в таблице оказалось 10 объектов, ранжированных по расстоянию 
#нужно убедится, что этот список включает нужные по условию задачи метеостанции

dag_table = filter (dag_table, distance > 0 & distance < 251 )

dag_stations = dag_table 

str(dag_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит

dag_stations$id


# Создание цикла, в который загружаются необходимые данные с метеостанций 

# Промежуточный объект, куда скачиваются данные с конкретной метеостанции
all_dag_data = meteo_tidy_ghcnd(stationid = dag_id)

#посмотрим, что же скачивается
?meteo_tidy_ghcnd

summary(all_dag_data)
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций

all_dag_meteodata = data.frame()

#Цикл для всех метеостанций

for(i in 1:10)
  
{
  
  all_i = meteo_tidy_ghcnd(stationid = dag_around[["dag"]][["id"]][i])
  
  
  
  #выберем нужные свойства
  
  all_i = all_i[ ,c("id","date","tavg")]
  
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном 
  #этапах цикла
  
  print(all_i)
  
  all_dag_meteodata=rbind(all_dag_meteodata, all_i)
  
}

#Записываем полученные результаты

write.csv(all_dag_meteodata,"all_dag_meteodata.csv")

#2 часть

#4. Разбивка даты на составляющие(год, месяц, день года)

# считываем данные из файла csv

all_dag_meteodata = read.csv("all_dag_meteodata.csv")
#посмотрим на данные

str(all_dag_meteodata)

#видим, что дата записана в формате "1882-01-01"

#ищем библиотеку из tidyverse, которая может помочь с датой

library(lubridate)

# вытащить год

#проверим, что работает

y = year(all_dag_meteodata$date); y

all_dag_meteodata [,"year"]= year(all_dag_meteodata$date)

#добавим месяц

all_dag_meteodata [,"month"]= month(all_dag_meteodata$date)

#вытащить день от начала года

all_dag_meteodata [,"day_of_the_year"]= yday(all_dag_meteodata$date)

#проверим результат

str(all_dag_meteodata)

#отфильтруем данные 

years_dag_meteodata = filter (all_dag_meteodata, year > 2006 & year <2008)

#проверим результат

str(years_dag_meteodata)

summary (years_dag_meteodata)

################## 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц

#Изучаем формулу и видим, что единственное, что нужно расчитать

#- это сумму температур больше 10 град. по месячно, остальное в формуле- константы



#### 1. температурy нужно поделить на 10

years_dag_meteodata[,"tavg"]= years_dag_meteodata$tavg / 10

summary (years_dag_meteodata)

#### 2. Превратим в нули все NA и где tavg больше 10 градусов



years_dag_meteodata [is.na(years_dag_meteodata$tavg), "tavg"] = 0

years_dag_meteodata [years_dag_meteodata$tavg<5, "tavg"] = 0



#проверяем, что температура получилась в или 0 или больше 10 градусов

summary(years_dag_meteodata)



alldays= group_by(years_dag_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным

#с помощью функции group_by

#просуммирую температуру по этим группам с помощью sum

sumT_alldays_dag = summarize(alldays, tsum = sum(tavg))

#Получилось - все года, все месяца присутствуют


summary(sumT_alldays_dag)


#Сгруппирем данные по месяцам

groups_dag_months = group_by(sumT_alldays_dag,month)

groups_dag_months

#найду для всех метеостанций и ВСЕХ лет среднее по месяцам

sumT_months= summarize(groups_dag_months , St = mean(tsum))

sumT_months

################## 6. Подготовка к расчету по формуле Урожая

### Ввод констант

afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)

bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)

di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)

y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;

Kf = 300 # - коэффициент использования ФАР посевом;

Qj = 1600 # - калорийность урожая культуры;

Lj = 2.2 # - сумма частей основной и побочной продукции;

Ej = 25 # - стандартная влажность культуры;

# Рассчитаем Fi по месяцам

#Fi= afi+bfi∗y∗(St>10℃)

sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)

#Рассчитаем Yi

sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

# Расчитываем урожай как сумму по месяцам и думаем разумный ли он

Yield = sum(sumT_months$Yi)

Yield

# Ответ: 19.24 ц/га 


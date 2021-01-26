library(dplyr)
library(readr)
library(tidyr)
library(curl)
library(lubridate)

old.loc <- Sys.getlocale("LC_ALL")
Sys.setlocale("LC_ALL","pt_PT.UTF-8")
# Sys.setlocale("LC_TIME", "Portugues")

data_url='https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv'
curl_download(data_url,destfile='./data.csv')

pop_url='https://raw.githubusercontent.com/jmigueldelgado/covid-incidence-portugal/master/pop2019.csv'
curl_download(pop_url,destfile='./pop2019.csv')

raw=read_csv('./data.csv')
pop=read_csv('./pop2019.csv',col_names=FALSE)

df=raw %>% dplyr::select(`data`,
    confirmados,
    confirmados_arsnorte,
    confirmados_arscentro,
    confirmados_arslvt,
    confirmados_arsalentejo,
    confirmados_arsalgarve,
    confirmados_acores,
    confirmados_madeira) %>%
    mutate(`data`=dmy(`data`))


colnames(df) = c('data',
      'Portugal',
      'Norte',
      'Centro',
      'Lisboa e Vale do Tejo',
      'Alentejo',
      'Algarve',
      'Açores',
      'Madeira')


dflong = pivot_longer(df,!data,names_to='região',values_to='confirmados')
pop=pop %>%
  rename(`região`=X1,`população`=X2) %>%
  mutate(`região`=as.factor(`região`)) %>%
  bind_rows(tibble(`região`=as.factor('Portugal'),`população`=sum(.$`população`)))


covid=dflong %>%
  left_join(pop) %>%
  group_by(`região`) %>%
  # arrange(asc(`Data`),.by_group=TRUE) %>%
  mutate(`max`=slider::slide_dbl(`confirmados`,max,.before=6,.complete=TRUE)) %>%
  mutate(`min`=slider::slide_dbl(`confirmados`,min,.before=6,.complete=TRUE)) %>%
  mutate(`acumulado 7 dias`=`max`-`min`) %>%
  dplyr::select(-`max`,-`min`) %>%
  mutate(`incidência 7 dias`=`acumulado 7 dias`*100000/`população`)

covid_this_month=dflong %>%
  left_join(pop) %>%
  group_by(`região`) %>%
  # arrange(asc(`Data`),.by_group=TRUE) %>%
  mutate(`max`=slider::slide_dbl(`confirmados`,max,.before=6,.complete=TRUE)) %>%
  mutate(`min`=slider::slide_dbl(`confirmados`,min,.before=6,.complete=TRUE)) %>%
  mutate(`acumulado 7 dias`=`max`-`min`) %>%
  dplyr::select(-`max`,-`min`) %>%
  mutate(`incidência 7 dias`=`acumulado 7 dias`*100000/`população`) %>%
  filter(`data`>today()-30)


covid_this_week=dflong %>%
  group_by(`região`) %>%
  # arrange(asc(`Data`),.by_group=TRUE) %>%
  mutate(`max`=slider::slide_dbl(`confirmados`,max,.before=1,.complete=TRUE)) %>%
  mutate(`min`=slider::slide_dbl(`confirmados`,min,.before=1,.complete=TRUE)) %>%
  mutate(`aumento diário`=`max`-`min`) %>%
  dplyr::select(-`max`,-`min`) %>%
  filter(`data`>today()-14) %>%
  mutate(`data`=as.POSIXct(`data`))


library(ggplot2)
inc=ggplot(covid) +
  geom_line(aes(x=`data`,y=`incidência 7 dias`))+
  geom_hline(yintercept=50, linetype="dashed", color = "orange")+
  xlab("")+
  facet_wrap(~`região`)
ggsave(file='./incidencia.png',plot=inc)

inc_this_month=ggplot(covid_this_month) +
  geom_line(aes(x=`data`,y=`incidência 7 dias`))+
  geom_hline(yintercept=50, linetype="dashed", color = "orange")+
  xlab("") +
  facet_wrap(~`região`)
ggsave(file='./incidencia_ultimos_30_dias.png',plot=inc_this_month)


cases_this_week =  ggplot(covid_this_week) +
  geom_bar(stat='identity',aes(x=`data`,y=`aumento diário`)) +
  scale_x_datetime(date_labels = "%a", date_breaks='2 day') +
  facet_wrap(~`região`)
ggsave(file='./casos_duas_semanas.png',plot=cases_this_week)




latin = readLines("README.md",-1)
latin[4]=paste0("Dados mais recentes de ",tail(covid_this_month,1) %>% pull(data),".")
writeLines(latin,"README.md")

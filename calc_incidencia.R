library(dplyr)
library(readr)
library(tidyr)
library(curl)
library(lubridate)
data_url='https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv'
curl_download(data_url,destfile='./data.csv')

raw=read_csv('./data.csv')
names(raw)

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
pop=read_csv('./pop2019.csv',col_names=FALSE)
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

library(ggplot2)
inc=ggplot(covid) +
  geom_line(aes(x=`data`,y=`incidência 7 dias`))+
  facet_wrap(~`região`)


ggsave(file='./incidencia.png')

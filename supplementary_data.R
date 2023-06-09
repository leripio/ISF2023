# Supplementary data for ISF2023 presentation

library(tidyverse)
library(rbcb)
library(lubridate)
library(odbc)

Sys.setlocale("LC_TIME", "US")

path_dir_new <- 'G:/Research/MACRO/BRASIL/DATA_SCIENCE/Coletas_V3'

source(glue::glue('{path_dir_new}/Funcoes/get_ipca.R')) 
source(glue::glue('{path_dir_new}/Funcoes/plot_item.R')) 

con <- dbConnect(
  odbc(), 
  Driver = "PostgreSQL ANSI(x64)", 
  Server = "PGKPTL01", 
  Database = "Macro", 
  UID = "k_macro", 
  PWD = "kmacro@01", 
  encoding = "latin1"
  )

# Focus accuracy --------------------------------------------------

ipca <- get_series(list('IPCA' = 433))

divulgacao_ipca <- dbSendQuery(
  con,
  "select mes_ref as reference_date, data_divulgacao as release from ds.calendario_ipca"
) %>% 
  dbFetch()

focus_ipca <- get_monthly_market_expectations(
  'IPCA'
)

focus_ipca_error <- focus_ipca %>% 
  filter(
    base == 0
    ) %>% 
  mutate(reference_date = ym(reference_date)) %>% 
  left_join(
    divulgacao_ipca
  ) %>% 
  mutate(t = difftime(date, release, units = "days") %>% as.numeric()) %>% 
  filter(t >= -30) %>% 
  left_join(
    ipca,
    by = c('reference_date' = 'date')
  ) %>% 
  group_by(t) %>% 
  summarise(
    mae = mean(abs(mean - IPCA)),
    avg_sd = mean(sd)
    ) %>% 
  ungroup()

focus_ipca_error %>% 
  ggplot(aes(x = t)) +
  geom_point(aes(y = mae, color = 'MAE'), size = 5) +
  geom_col(aes(y = avg_sd, fill = 'Average SD'), alpha = 0.5) +
  theme_light() +
  scale_color_manual(values = c('#d8b365')) +
  scale_fill_manual(values = c('#5ab4ac')) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  scale_y_continuous(sec.axis = dup_axis(), limits = c(0, 0.20), breaks = seq(0, 0.20, 0.025)) +
  scale_x_continuous(label = function(x) x*(-1), breaks = seq(-30, 0, 2), limits = c(-30, 0)) +
  labs(
    title = 'Focus Survey (BCB): Forecast Error and Dispersion (p.p)',
    color = '',
    x = 'days before official release',
    y = 'p.p',
    fill = ''
  )

ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/focus_error.png', width = 15, height = 9)

# Focus industriais e perfume -------------------------------------------------------

ipca_perfume <- get_ipca(6301011)

ipca_perfume_tidy <- ipca_perfume %>% 
  filter(indicador == 'IPCA') %>% 
  select(reference_date = data, ipca_perfume = variacao)

ipca_ind <- get_series(
  list('IPCA_ind' = 27863)
  )

focus_ipca_ind <- get_monthly_market_expectations(
  'IPCA Bens industrializados'
)

focus_ipca_ind_tidy <- focus_ipca_ind %>% 
  filter(
    base == 0
  ) %>% 
  group_by(reference_date) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(reference_date = ym(reference_date))

focus_ind_error <- focus_ipca_ind_tidy %>% 
  left_join(
    ipca_ind,
    by = c('reference_date' = 'date')
  ) %>% 
  left_join(ipca_perfume_tidy) %>% 
  filter(!is.na(IPCA_ind))

focus_ind_error %>% 
  filter(date >= '2022-08-01' & date <= '2023-04-01') %>% 
  ggplot(aes(x = reference_date)) +
  geom_line(aes(y = mean, color = 'Focus Survey for Ind. Goods (LHS)'), lwd = 1) +
  geom_line(aes(y = IPCA_ind, color = 'CPI for Ind. Goods (LHS)'), lwd = 1) +
  geom_line(aes(y = ipca_perfume/8, color = 'CPI for Perfume (RHS)'), lwd = 1) +
  scale_y_continuous(
    "CPI for Industrial Goods (%MoM)", 
    labels = function(x) paste0(x, '%'),
    sec.axis = sec_axis(
      ~ . * 8, 
      name = "CPI for Perfume (%MoM)",
      labels = function(x) paste0(x, '%')
      )
  ) +
  theme_light() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b/%y') +
  scale_color_manual(values = c('#d8b365', '#5ab4ac', 'black')) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  labs(
    title = 'CPI for Industrial Goods: Market errors mainly due to surprises in Perfume',
    subtitle = 'Last day before official release',
    color = '',
    x = 'Reference date',
    y = 'Mean forecast for Industrial Goods CPI (%MoM)'
  )

ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/ind_goods.png', width = 15, height = 9)

# Perfume ----------------------------------------------------------

coleta_perfume <- plot_item(6301011, dicio_id = '2023-04-27') 

coleta_perfume$dados %>% 
  filter(
    data <= '2023-04-01'
  ) %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = variacao_kptl_media_mean, color = 'Perfume Web-based Index'), lwd = 1) +
  geom_line(aes(y = variacao_fgv_media, color = 'Benchmark'), lwd = 1) +
  geom_ribbon(aes(
    ymin = variacao_kptl_media_min, 
    ymax = variacao_kptl_media_max
  ), 
  alpha = 0.2, 
  fill = 'forestgreen'
  ) +
  geom_point(aes(y = variacao, color = 'CPI Perfume'), size = 3) +
  scale_color_manual(values = c('steelblue3', 'red', 'forestgreen')) +
  theme_light() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b/%y') +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  labs(
    title = 'CPI for Perfume: Web-based Index vs. Official release',
    subtitle = 'Solid line = average of baskets. Shaded area = Min/Max of baskets',
    color = '',
    x = 'End date for data collection',
    y = ''
  ) +
  annotate('text', x = as.Date('2022-08-28'), y = 7, label = 'August') +
  annotate('text', x = as.Date('2022-11-28'), y = -7, label = 'November') +
  annotate('text', x = as.Date('2022-12-28'), y = 10, label = 'December') +
  annotate('text', x = as.Date('2023-01-28'), y = -7, label = 'January')
  
ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/perfume.png', width = 15, height = 9)

# Leite -------------------------------------------------------------------

coleta_leite <- plot_item(1111004) 

coleta_leite$dados %>% 
  filter(
    data <= '2023-04-01'
  ) %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = variacao_kptl_media_mean, color = 'Milk Web Index'), lwd = 1) +
  geom_line(aes(y = variacao_fgv_media, color = 'Benchmark'), lwd = 1) +
  geom_point(aes(y = variacao, color = 'CPI Milk'), size = 3) +
  scale_color_manual(values = c('steelblue3', 'red', 'forestgreen')) +
  theme_light() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b/%y') +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  labs(
    title = 'CPI for Milk: Web-based Index vs. Official release',
    subtitle = '',
    color = '',
    x = '',
    y = ''
  )

ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/leite.png', width = 15, height = 9)

# Carro usado -------------------------------------------------------------

coleta_auto_usado <- plot_item(5102020) 

coleta_auto_usado$dados %>% 
  filter(
    data <= '2023-04-01'
  ) %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = variacao_kptl_media_mean, color = 'Used Cars Web Index'), lwd = 1) +
  geom_line(aes(y = variacao_fgv_media, color = 'Benchmark'), lwd = 1) +
  geom_point(aes(y = variacao, color = 'CPI Used Cars'), size = 3) +
  scale_color_manual(values = c('steelblue3', 'red', 'forestgreen')) +
  theme_light() +
  geom_ribbon(aes(ymin = 0, ymax = -Inf), alpha = 0.2, fill = 'red') +
  geom_ribbon(aes(ymin = 0, ymax = Inf), alpha = 0.2, fill = 'forestgreen') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b/%y') +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  labs(
    title = 'CPI for Used Cars: Web-based Index vs. Official release',
    subtitle = '',
    color = '',
    x = '',
    y = ''
  ) +
  geom_hline(yintercept = 0)

ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/auto_usado.png', width = 15, height = 9)

# Computador --------------------------------------------------------------

coleta_pc <- plot_item(3202028, dicio_id = '2023-03-15') 

coleta_pc$dados %>% 
  filter(
    data <= '2023-04-01'
  ) %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = variacao_kptl_media_mean, color = 'PC Web Index'), lwd = 1) +
  geom_point(aes(y = variacao, color = 'CPI PC'), size = 3) +
  geom_line(aes(y = variacao_fgv_media, color = 'Benchmark'), lwd = 1) +
  scale_color_manual(values = c('steelblue3', 'red', 'forestgreen')) +
  theme_light() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b/%y') +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  labs(
    title = 'CPI for Personal Computer: Web-based Index vs. Official release',
    subtitle = '',
    color = '',
    x = '',
    y = ''
  )

ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/pc.png', width = 15, height = 9)

# Exercício ---------------------------------------------------------------

dados_proj_kp <- dbSendQuery(
  con,
  "select * from inflacao.projecoes"
) %>% 
  dbFetch()
  
focus_kp_total <- focus_ipca_ind %>% 
  filter(base == 0) %>% 
  group_by(reference_date) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(data = reference_date, focus = mean) %>% 
  mutate(data = ym(data)) %>% 
  left_join(
    dados_proj_kp %>% 
      filter(descricao == 'Industriais do BC') %>% 
      group_by(referencia) %>% 
      filter(data == max(data)) %>% 
      ungroup() %>% 
      select(data = referencia, kp = valor)
  ) %>% 
  arrange(data) %>% 
  filter(!is.na(kp)) %>% 
  left_join(
    ipca_ind %>% select(data = date, ipca = IPCA_ind)
  )

focus_kp_total %>% 
  filter(data <= '2023-05-01') %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = kp, color = 'Kapitalo (Ind. Goods)'), lwd = 1) +
  geom_line(aes(y = focus, color = 'Focus (Ind. Goods)'), lwd = 1) +
  geom_line(aes(y = ipca, color = 'CPI (Ind. Goods)'), lwd = 1) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b/%y') +
  scale_color_manual(values = c('black', '#d8b365', '#5ab4ac')) +
  scale_y_continuous(label = function(x) paste0(x, '%')) +
  theme_light() +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 17),
    axis.text = element_text(size = 17),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 17)
  ) +
  annotate('text', x = as.Date('2022-08-01'), y = 0.9, label = 'August') +
  annotate('text', x = as.Date('2022-11-01'), y = 0.08, label = 'November') +
  annotate('text', x = as.Date('2022-12-01'), y = 1.25, label = 'December') +
  annotate('text', x = as.Date('2023-01-01'), y = 0.15, label = 'January') +
  labs(
    title = 'CPI for Ind. Goods: Kapitalo vs. Focus (%MoM)',
    subtitle = '',
    color = '',
    x = '',
    y = ''
  )

ggsave('C:/Users/joao.leripio/Documents/Pessoal/ISF2023/images/bens_ind_kp.png', width = 15, height = 9)


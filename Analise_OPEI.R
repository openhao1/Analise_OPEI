# Instalação de pacotes ####
install.packages("MASS")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("stargazer")
install.packages("ggeffects")
install.packages("purrr")
install.packages("scales")
install.packages("zoo")
install.packages("nnet")
install.packages("tidytext")
install.packages("readr")    
install.packages("stopwords")
install.packages("lexiconPT")

library(MASS)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(stargazer)
library(brant)
library(ggeffects)
library(broom)


# Carregamento da base de dados ####
dados_OPEI <- read_excel("Analise_OPEI.xlsx") # Colar o endereço do arquivo na suaaqui
head(dados_OPEI)
names(dados_OPEI)

# Fatorização e limpeza dos dados ####
summary(dados_OPEI)
dados_OPEI_preps <- dados_OPEI %>%
  rename(dip_pres = "Diplomacia Presidencial") %>%
  mutate(
    posicao_polex_num = as.numeric(posicao_polex),
    posicao_polex_limpas = na_if(posicao_polex_num, 9),
    posicao_fator = factor(posicao_polex_limpas,
                           levels = c("2", "0", "1"),
                           labels = c("Contrário","Neutro","Favorável"),
                           ordered = TRUE),
    
    negociacoes_bilaterais_num = as.numeric(negociacoes_bilaterais),

    negociacoes_bilaterais_limpas = if_else(negociacoes_bilaterais_num %in% c(3, 9), 
                                            NA_real_, 
                                            negociacoes_bilaterais_num),
   
    negociacao_binaria = if_else(is.na(negociacoes_bilaterais_limpas), 0, 1),
    dip_pres_num = as.numeric(dip_pres),
    dip_pres_limpas = if_else(dip_pres_num %in% c(3, 9), 
                              NA_real_, 
                              dip_pres_num),
    dip_pres_binaria = if_else(is.na(dip_pres_limpas), 0, 1),
    cod_classificacao_fator = factor(Cod_Classificacao,
                                     levels = c("GLB", "FSP", "ESP", "VE", "GP"),
                                     labels = c("O Globo", "Folha de S. Paulo", "Estado de S. Paulo", "Valor Economico", "Gazeta do Povo"),
                                            ordered = FALSE),
    
    mandato_fator = factor(Mandato,
                           levels = c("Bolsonaro", "Temer", "Dilma", "Lula III"),
                           labels = c("Bolsonaro", "Temer", "Dilma", "Lula"), 
                           ordered = FALSE)
) %>% # Limpeza de ruídos da Gazeta do Povo
  filter(!(cod_classificacao_fator == "O Globo" & Tipologia == "5.0")) %>% 
  filter(cod_classificacao_fator != "Nome do Jornal Vazio") %>%
  droplevels()

# Checagem das variáveis dummy e categorias
print(head(dados_OPEI_preps$negociacao_binaria,10))
print(head(dados_OPEI_preps$dip_pres_binaria,10))

levels(dados_OPEI_preps$cod_classificacao_fator)
levels(dados_OPEI_preps$mandato_fator)

# Estimação do modelo de regressão logística ordenada ####

# Modelo 1. Sem VCs

reg_log_simples <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria,
                        data = dados_OPEI_preps,
                        Hess = TRUE)
stargazer(reg_log_simples, type = "text")
# Modelo 2. Com VC "Mandato"
reg_log_mandato <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria + mandato_fator,
                        data = dados_OPEI_preps,
                        Hess = TRUE)
stargazer(reg_log_mandato, type = "text")
# Modelo 3. Com VC "Mandato" e "Veículo"
reg_log_jornal <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria + mandato_fator + cod_classificacao_fator,
                               data = dados_OPEI_preps,
                               Hess = TRUE)
stargazer(reg_log_jornal, type = "text")

# Tabelas de regressão em stargazer (texto, html e LaTex)

stargazer(reg_log_simples, reg_log_mandato, reg_log_jornal, type = "text")

stargazer(reg_log_simples, reg_log_mandato, reg_log_jornal, type = "html", 
         title = ("Posição da Imprensa sobre a Política Externa"),
        dep.var.labels = ("Posicionamento da Imprensa acerca da Política Externa"), 
        column.labels = c("Negociação Comercial Bilateral <br>+ Diplomacia Presidencial",
                          "Negociação Comercial Bilateral <br>+ Diplomacia Presidencial <br>+ Mandato",
                          "Negociação Comercial Bilateral <br>+ Diplomacia Presidencial <br>+ Mandato + Veículo"),
        covariate.labels = c("Presença da pauta de negociação comercial bilateral",  
                             "Presença da pauta de diplomacia presidencial",
                             "Mandato: Temer (vs. Bolsonaro)",
                             "Mandato: Dilma (vs. Bolsonaro)",
                             "Mandato: Lula (vs. Bolsonaro)", 
                             "Veículo: Folha de S. Paulo (vs. O Globo)",
                             "Veículo: Estado de S. Paulo (vs. O Globo)",
                             "Veículo: Valor Econômico (vs. O Globo)"))
                                                                                                    
stargazer(reg_log_simples, reg_log_mandato, reg_log_jornal, type = "latex", 
          title = ("Posição da Imprensa sobre a Política Externa"),
          dep.var.labels = ("Posicionamento da Imprensa acerca da Política Externa"), 
          column.labels = c("Negociação Comercial Bilateral",
                            "Negociação Comercial Bilateral + Mandato",
                            "Negociação Comercial Bilateral + Mandato + Veículo"),
          covariate.labels = c("Presença da pauta de negociação comercial bilateral",  
                               "Presença da pauta de diplomacia presidencial",
                               "Mandato: Temer (vs. Bolsonaro)",
                               "Mandato: Dilma (vs. Bolsonaro)",
                               "Mandato: Lula (vs. Bolsonaro)", 
                               "Veículo: Folha de S. Paulo (vs. O Globo)",
                               "Veículo: Estado de S. Paulo (vs. O Globo)",
                               "Veículo: Valor Econômico (vs. O Globo)"))

stargazer(reg_log_simples, type = "text")

# Teste de brant: confirmação de que a regressão pertence ao Partial Proportional Odds Model (PPOM) ####


reg_log_ordinal <- polr(posicao_fator ~ negociacao_binaria + dip_pres_binaria,
                        data = dados_OPEI_preps,
                        Hess = TRUE)

brant(reg_log_ordinal)


# Conversão em probabilidades previstas e plotagem do gráfico ####

ggpredict(reg_log_jornal, terms = c("negociacao_binaria", "dip_pres_binaria"))

previsoes_negociacao <- ggpredict(reg_log_jornal, terms = "negociacao_binaria")
previsoes_diplomacia <- ggpredict(reg_log_jornal, terms = "dip_pres_binaria")

ggplot(
  data = previsoes_negociacao, 
  aes(
    x = response.level,  
    y = predicted,       
    color = as.factor(x)
  )
) +
  
  geom_point(
    position = position_dodge(width = 0.3), 
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    position = position_dodge(width = 0.3)
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_color_manual(
    name = "Presença da pauta comercial",
    values = c("1" = "blue","0" = "red"),
    labels = c("1" = "Pauta presente", "0" = "Pauta ausente")) +
  labs(
    title = "Probabilidade prevista da Posição do Artigo",
    subtitle = "Efeito da presença da pauta comercial",
    x = "Posição do texto crítico da imprensa",
    y = "Probabilidade prevista"
  ) +
  
  theme_minimal()


ggplot(
  data = previsoes_diplomacia, 
  aes(
    x = response.level,  
    y = predicted,
    color = as.factor(x)
  )
) +
  
  geom_point(
    position = position_dodge(width = 0.3), 
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    position = position_dodge(width = 0.3)
  ) +
  
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    name = "Presença da pauta da diplomacia presidencial",
    values = c("1" = "blue","0" = "red"),
    labels = c("1" = "Pauta presente", "0" = "Pauta ausente"))  +
  labs(
    title = "Probabilidade prevista da Posição do Artigo",
    subtitle = "Efeito da presença da pauta comercial",
    x = "Posição do texto crítico da imprensa",
    y = "Probabilidade prevista"
  ) +
  
  theme_minimal()

dados_plot_combinado <- bind_rows(
  previsoes_negociacao %>% 
    mutate(
      pauta_tipo = "Negociação comercial bilateral",
      x = as.factor(x)
    ),
  previsoes_diplomacia %>% 
    mutate(
      pauta_tipo = "Diplomacia presidencial",
      x = as.factor(x) 
    )
)
dados_plot_combinado_limpo <- dados_plot_combinado%>%
  filter(!is.na(response.level))

# Junção dos gráficos

grafico_final <- ggplot(
  data = dados_plot_combinado_limpo, 
  aes(
    x = response.level,  
    y = predicted,       
    color = x
  )
) +
  geom_point(
    position = position_dodge(width = 0.3), 
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    position = position_dodge(width = 0.3)
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_color_manual(
    name = "", 
    values = c("red","blue"), 
    labels = c("0" = "Pauta ausente", "1" = "Pauta presente") 
  ) +
  
  labs(
    title = "Gráfico 4 - Efeito das pautas temáticas na postura crítica da imprensa",
    x = "Posição crítica esperada dos artigos",
    y = "Probabilidade prevista (ausência vs. presença)"
  ) +

  scale_x_discrete(
    limits = c("Contrário", "Neutro", "Favorável")
  ) +
  facet_wrap(~ pauta_tipo) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "black"),
    plot.title = element_text(size = 14, face = "bold", family = "serif"),
    axis.title = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 11, family = "serif"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = "black", linewidth = 0.5),
    strip.background = element_rect(color = "black", fill = "grey80", linewidth = 0.5)
  )
plot(grafico_final)
ggsave(
  filename = "grafico_pautas_opei.png", 
  plot = grafico_final,                     
  width = 20,                             
  height = 10,                             
  units = "cm",                          
  dpi = 1200,                              
  bg = "white"                             
)
# Teste de filtro editorial ####

data_OPEI_filtrado <- dados_OPEI_preps %>%
  filter(Identificacao_editorial == "1.0")

reg_log_filtrada <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria,
                        data = data_OPEI_filtrado,
                        Hess = TRUE)
stargazer(reg_log_filtrada, type = "text")

reg_log_veiculo_filtrada <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria + mandato_fator + cod_classificacao_fator,
                                 data = data_OPEI_filtrado,
                                 Hess = TRUE)
stargazer(reg_log_veiculo_filtrada, type = "text")
reg_log_mandato_filtrada <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria + mandato_fator,
                        data = data_OPEI_filtrado,
                        Hess = TRUE)
reg_log_mandato_jornal_filtrada <- polr(posicao_fator ~ factor(negociacao_binaria, ordered = FALSE) + dip_pres_binaria + mandato_fator,
                                 data = data_OPEI_filtrado,
                                 Hess = TRUE)
stargazer(reg_log_mandato_filtrada, type = "text")

stargazer(reg_log_filtrada, reg_log_mandato_filtrada, reg_log_veiculo_filtrada, type = "text")

# Plotagem do gráfico com probabilidades previstas para o filtro editorial

previsoes_filtradas_negociacao <- ggpredict(reg_log_veiculo_filtrada, terms = "negociacao_binaria")
previsoes_filtradas_dip_pres <- ggpredict(reg_log_veiculo_filtrada, terms = "dip_pres_binaria")

dados_plot_filtrado <- bind_rows(
  previsoes_filtradas_negociacao %>% 
    mutate(
      pauta_tipo = "Negociação comercial bilateral",
      x = as.factor(x)
    ),
  previsoes_filtradas_dip_pres %>% 
    mutate(
      pauta_tipo = "Diplomacia presidencial",
      x = as.factor(x) 
    )
)
dados_plot_filtrado_limpo <- dados_plot_filtrado %>%
  filter(!is.na(response.level))

grafico_filtrado <- ggplot(
  data = dados_plot_filtrado_limpo, 
  aes(
    x = response.level,  
    y = predicted,       
    color = x
  )
) +
  geom_point(
    position = position_dodge(width = 0.3), 
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    position = position_dodge(width = 0.3)
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_color_manual(
    name = "", 
    values = c("red","blue"), 
    labels = c("0" = "Pauta ausente", "1" = "Pauta presente") 
  ) +
  
  labs(
    title = "Efeito das pautas na postura crítica dos editoriais da imprensa",
    x = "Posição esperada dos editoriais",
    y = "Probabilidade prevista (ausência vs. presença da pauta)"
  ) +
  
  scale_x_discrete(
    limits = c("Contrário", "Neutro", "Favorável")
  ) +
  facet_wrap(~ pauta_tipo) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(color = "black", fill = "grey90", linewidth = 0.5)
  )
plot(grafico_filtrado)
ggsave(
  filename = "grafico_pautas_editorial_opei.png", 
  plot = grafico_filtrado,                    
  width = 20,                               
  height = 12,                            
  units = "cm",                            
  dpi = 1200,                               
  bg = "white"                            
)

# Tabela de assuntos mais citados ####


tabela_assuntos_mais_citados <- dados_OPEI %>%
  summarise(across(everything(), ~ sum(. %in% c("0", "1.0", "2.0"), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), 
               names_to = "Assunto", 
               values_to = "Total_Citacoes") %>%
  filter(Total_Citacoes > 0) %>%
  arrange(desc(Total_Citacoes))

head(tabela_assuntos_mais_citados, 20)


library(scales)
library(lubridate)
library(zoo)

# Plotagem do gráfico de média móvel - OPEI (2014-2023) ####
dados_suavizados <- dados_OPEI_preps %>%
  filter(!is.na(Ano), !is.na(Mes), !is.na(posicao_fator)) %>%
  mutate(
    Data = make_date(year = as.numeric(as.character(Ano)), 
                     month = as.numeric(as.character(Mes)), day = 1)
  ) %>% 
  count(Data, posicao_fator) %>%
  
  complete(Data, posicao_fator, fill = list(n = 0)) %>%
  
  group_by(Data) %>%
  mutate(percentual = n / sum(n)) %>%
  ungroup() %>%
  
  group_by(posicao_fator) %>%
  arrange(Data) %>%
  mutate(
    percentual_movel = rollmean(percentual, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  
  filter(!is.na(percentual_movel)) %>% 
  group_by(Data) %>%
  mutate(percentual_final = percentual_movel / sum(percentual_movel)) %>%
  ungroup()

ggplot(dados_suavizados, aes(x = Data, y = percentual_final, fill = posicao_fator)) +
  geom_area(alpha = 0.9, color = "black", linewidth = 0.1) +
  
  geom_vline(xintercept = as.Date(c("2016-08-31", "2019-01-01", "2023-01-01")), 
             linetype = "dotted", color = "black", alpha = 0.6) +
  
  scale_fill_manual(
    values = c("Contrário" = "#C0392B", 
               "Neutro"    = "blue", 
               "Favorável" = "#27AE60"),
    breaks = c("Contrário", "Neutro", "Favorável")
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  
  labs(
    title = "Teor crítico da imprensa à PEB (2014-2023)",
    caption = "Fonte: Dados OPEI. Média móvel calculada sobre janela de 3 meses.",
    x = NULL, y = "Teor crítico dos artigos e editoriais", fill = NULL
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 16, color = "black"),
    plot.subtitle = element_text(size = 11, color = "#7F8C8D", margin = margin(b = 20)),
    axis.text = element_text(color = "black", size = 10)
  )


library(purrr)

set.seed(123)
n_total_alvo <- 400

# Amostragem aleatória estratificada com alocação proporcional ####
plano_amostragem <- dados_OPEI_preps %>%
  count(mandato_fator, cod_classificacao_fator) %>%
  mutate(
    proporcao = n / sum(n),
    n_alvo = round(proporcao * n_total_alvo)
  )

# Sorteio
amostra_OPEI_prop <- dados_OPEI_preps %>%
  group_split(mandato_fator, cod_classificacao_fator) %>%
  map2_dfr(plano_amostragem$n_alvo, ~ slice_sample(.x, n = .y))

print(nrow(amostra_OPEI_prop))
table(amostra_OPEI_prop$mandato_fator, amostra_OPEI_prop$cod_classificacao_fator)


ASA_OPEI <- read_excel("dados_OPEI_coleta_manual.xlsx")



library(tidytext)
library(readr)
library(stopwords)
library(lexiconPT)
library(writexl)


data("oplexicon_v3.0")

names(oplexicon_v3.0)

lexicon_pt_tidy <- oplexicon_v3.0 %>%
  select(word = term, value = polarity) %>% # 
  filter(value != 0) # Remove palavras neutras (polarity == 0)

stops_pt <- data.frame(word = stopwords("pt"))


textos_tidy <- ASA_OPEI %>%
  mutate(id_artigo = row_number()) %>%        # Cria um ID único para cada linha/artigo
  unnest_tokens(word, texto_extraido) %>%     # Transforma os textos em palavras individuais
  anti_join(stops_pt, by = "word") %>%        # Remove as stopwords
  inner_join(lexicon_pt_tidy, by = "word")    # Mantém apenas as palavras que têm polaridade no dicionário

sentimento_por_artigo <- textos_tidy %>%
  group_by(id_artigo) %>%                     # Agrupa as palavras de volta por artigo
  summarise(
    sentimento_total = sum(value),            # Soma a polaridade (-1 ou 1) das palavras
    qtd_palavras_sentimentais = n()           # Conta quantas palavras "úteis" o artigo teve
  )


resultado_final <- ASA_OPEI %>%
  mutate(id_artigo = row_number()) %>%        # Recria o ID para poder juntar
  left_join(sentimento_por_artigo, by = "id_artigo") %>% 
  
  # Se um artigo não teve nenhuma palavra no dicionário, o resultado será NA.
  # Vamos substituir esses NAs por 0 (sentimento neutro)
  mutate(sentimento_total = ifelse(is.na(sentimento_total), 0, sentimento_total))

# Tentativa de criação de modelo de Reg. Logística ####
library(nnet)
dados_regressao <- resultado_final %>%
  filter(!is.na(posicao_fator)) %>%
  

  mutate(posicao_fator = factor(posicao_fator, 
                                levels = c("Neutro", "Contrário", "Favorável")))
modelo_logistico <- multinom(posicao_fator ~ sentimento_total, data = dados_regressao)
summary(modelo_logistico)
# Falha na parametrização

# Checagem de parametrização via teste de Shapiro-Wilk
shapiro.test(dados_regressao$sentimento_total)

# Testes estatísticos não-paramétricos ####

# Teste de Kruskal-Wallis
kruskal <- kruskal.test(dados_regressao$sentimento_total ~ dados_regressao$posicao_fator)

install.packages("FSA")
library(FSA)

# Teste de Dunn
resultado_dunn <- dunnTest(sentimento_total ~ posicao_fator, 
                           data = dados_regressao, 
                           method = "bonferroni") 

print(resultado_dunn)

# Organização dos fatores
dados_regressao$posicao_fator <- factor(dados_regressao$posicao_fator, 
                              levels = c("Contrário", "Neutro", "Favorável"))

# Construção do boxplot ####
ggplot(dados_regressao, aes(x = posicao_fator, y = sentimento_total, fill = posicao_fator)) +
  # geom_boxplot desenha as caixas e bigodes
  geom_boxplot(alpha = 0.7, outlier.colour = "black", outlier.shape = 1) +
  
  # scale_fill_manual define as cores sóbrias e intuitivas
  scale_fill_grey(start = 0.4, end = 0.9) +
  
  # Labs define títulos e legendas claros
  labs(title = "Distribuição dos scores de sentimento por categoria de orientação crítica",
       x = "Classificação da análise de conteúdo do OPEI",
       y = "Score de sentimento (OplexiconPT v. 3.0)",
       caption = "Fonte: Elaboração própria com dados da pesquisa.") +
  
  # Estética limpa
  theme_classic() +
  theme(legend.position = "none", # Remove legenda redundante (já está no eixo X)
        text = element_text(size = 10, family = "serif"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))

ggsave(
  filename = "boxplot_sentimento_opei.png",
  plot = last_plot(), # Garante que vai salvar o último gráfico gerado
  width = 16,         # Largura em cm
  height = 12,        # Altura em cm
  units = "cm",       # Unidade de medida
  dpi = 300           # Alta resolução para impressão
)


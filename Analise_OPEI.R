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
library(ggplot2)  

# Carregamento da base de dados ####
dados_OPEI <- read_excel("Analise_OPEI.xlsx") # Colar o endereço do arquivo na suaaqui
head(dados_OPEI)
names(dados_OPEI)

# Fatorização e limpeza dos dados ####
library(dplyr)
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

# Teste de brant: confirmação de que a regressão pertence ao Proportional Odds Model (POM) ####


reg_log_ordinal <- polr(posicao_fator ~ negociacao_binaria + dip_pres_binaria,
                        data = dados_OPEI_preps,
                        Hess = TRUE)

brant(reg_log_ordinal)


# Conversão em probabilidades prTRUE# Conversão em probabilidades previstas e plotagem do gráfico ####

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


library(nnet)

dados_regressao <- resultado_final %>%
  # Removemos linhas que por acaso não tenham recebido classificação humana
  filter(!is.na(posicao_fator)) %>%
  
  # Transformamos a coluna em uma categoria (fator) usando os textos exatos.
  # "Neutro" vem primeiro para ser a nossa referência na regressão.
  mutate(posicao_fator = factor(posicao_fator, 
                                levels = c("Neutro", "Contrário", "Favorável")))

# 2. Criar o modelo de Regressão Logística Multinomial
# Lemos: "A posicao_fator é explicada (~) pelo sentimento_total"
modelo_logistico <- multinom(posicao_fator ~ sentimento_total, data = dados_regressao)

# 3. Ver o resumo estatístico do modelo
# Aqui você verá os coeficientes que mostram a força da relação
summary(modelo_logistico)

# 4. Validação (Matriz de Confusão)
# O modelo tenta adivinhar a posição (Contrário/Neutro/Favorável) usando apenas a nota
dados_regressao$previsao_modelo <- predict(modelo_logistico, dados_regressao)

# Carregar os pacotes necessários para o gráfico
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Descobrir a nota mínima e máxima de sentimento no seu banco de dados
nota_minima <- min(dados_regressao$sentimento_total, na.rm = TRUE)
nota_maxima <- max(dados_regressao$sentimento_total, na.rm = TRUE)

# 2. Criar uma sequência de 100 notas, indo da mínima até a máxima
dados_grafico <- data.frame(
  sentimento_total = seq(from = nota_minima, to = nota_maxima, length.out = 100)
)

# 3. Pedir para o modelo calcular a PROBABILIDADE para essa sequência de notas
probabilidades <- predict(modelo_logistico, newdata = dados_grafico, type = "probs")

# 4. Juntar as notas com as probabilidades calculadas
dados_plot <- cbind(dados_grafico, probabilidades)

# 5. Transformar a tabela para o formato longo (exigência do ggplot2)
dados_plot_longo <- dados_plot %>%
  pivot_longer(
    cols = c("Neutro", "Contrário", "Favorável"), # As três colunas geradas pelo predict
    names_to = "posicao_fator",                   # Nome da nova coluna com a categoria
    values_to = "Probabilidade"                   # Nome da nova coluna com o valor percentual
  )

# 6. Criar o gráfico com ggplot2
grafico_regressao <- ggplot(dados_plot_longo, aes(x = sentimento_total, y = Probabilidade, color = posicao_fator)) +
  geom_line(size = 1.2) + # Cria as linhas do gráfico, levemente mais grossas
  
  # Personalizando as cores das linhas para fazer sentido
  scale_color_manual(values = c("Contrário" = "#E06666", 
                                "Neutro" = "#B7B7B7", 
                                "Favorável" = "#6D9EEB")) +
  
  # Adicionando títulos e rótulos
  labs(
    title = "Probabilidade de Classificação vs. Análise de Sentimento",
    subtitle = "Modelo de Regressão Logística Multinomial",
    x = "Pontuação de Sentimento (Léxico)",
    y = "Probabilidade do Modelo (0 a 1)",
    color = "Análise Humana"
  ) +
  
  # Um tema limpo e bonito para relatórios
  theme_minimal() +
  theme(legend.position = "bottom") # Coloca a legenda na parte de baixo

# Exibir o gráfico na tela
print(grafico_regressao)

# 1. Executar o modelo ANOVA
# Lemos: "O sentimento_total é influenciado (~) pela posicao_fator?"
modelo_anova <- aov(sentimento_total ~ posicao_fator, data = dados_regressao)

# 2. Ver o resultado geral da ANOVA
print("=== RESULTADO GLOBAL DA ANOVA ===")
summary(modelo_anova)

# 3. Teste de Tukey (Comparações múltiplas)
# Isso vai criar uma tabela cruzando as categorias duas a duas
teste_tukey <- TukeyHSD(modelo_anova)
print("=== RESULTADO DO TESTE DE TUKEY (Onde está a diferença?) ===")
print(teste_tukey)

# 4. Construção do Gráfico Boxplot
grafico_anova <- ggplot(dados_regressao, aes(x = posicao_fator, y = sentimento_total, fill = posicao_fator)) +
  # geom_boxplot desenha as caixas. O alpha=0.7 deixa as cores um pouco transparentes
  geom_boxplot(alpha = 0.7, outlier.color = "black", outlier.shape = 16) +
  
  # Usamos as mesmas cores do gráfico anterior para manter a consistência visual na dissertação
  scale_fill_manual(values = c("Contrário" = "#E06666", 
                               "Neutro" = "#B7B7B7", 
                               "Favorável" = "#6D9EEB")) +
  
  # Adicionando títulos metodológicos e eixos claros
  labs(
    title = "Distribuição do sentimento por categoria de orientação crítica da imprensa à PEB",
    subtitle = "Análise de Variância (ANOVA)",
    x = "Categorias da Análise do OPEI",
    y = "Nota de Sentimento Automatizada (Máquina)"
  ) +
  
  # Tema limpo
  theme_minimal() +
  
  # Escondemos a legenda lateral porque o eixo X (embaixo) já tem os nomes das categorias
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12))

# Exibir o gráfico na tela
print(grafico_anova)

media_sentimento <- mean(dados_regressao$sentimento_total, na.rm = TRUE)
desvio_sentimento <- sd(dados_regressao$sentimento_total, na.rm = TRUE)
hist(dados_regressao$sentimento_total)


require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer

theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",") # formatação em português (vírgula pra decimais e ponto para milhares)

Dados = read.xlsx("DadosAviao.xlsx")
Dados

DadosQuali = Dados %>% select(Limpeza,Genero, Cliente, Idade,
                              Tipo_Viagem, Classe, Distancia,
                              Atraso_Partida, Atraso_Chegada)

tbl_summary(data = DadosQuali,
            by = Limpeza,
            percent = "row")


chisq.test(Dados$Genero,Dados$Limpeza)$expected
chisq.test(Dados$Cliente,Dados$Limpeza)$expected
chisq.test(Dados$Idade,Dados$Limpeza)$expected
chisq.test(Dados$Tipo_Viagem,Dados$Limpeza)$expected
chisq.test(Dados$Classe,Dados$Limpeza)$expected
chisq.test(Dados$Distancia,Dados$Limpeza)$expected
chisq.test(Dados$Atraso_Partida,Dados$Limpeza)$expected
chisq.test(Dados$Atraso_Chegada,Dados$Limpeza)$expected


# Todos Qui-quadrado

tbl_summary(data = DadosQuali,
            by = Limpeza,
            percent = "row")%>%
  add_p()

# variável significativa Atraso partida

chisq.test(Dados$Atraso_Partida,Dados$Limpeza)$stdres

# Função que calcula o coeficiente de Cramer
cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}

# Código da tabela
tbl_summary(data = DadosQuali,
            by = Limpeza,
            percent = "row",
            label = list(
              Genero ~ "Gênero",
              Cliente ~ "Tipo de Cliente",
              Idade ~ "Idade",
              Tipo_Viagem ~"Tipo de Viagem",
              Classe ~ "Classe",
              Distancia ~ "Distância",
              Atraso_Partida ~"Atraso na Partida",
              Atraso_Chegada ~"Atraso na Chegada"
            )
)%>%
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun)%>%
  modify_spanning_header(all_stat_cols() ~ "**Satisfação com a limpeza**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)")%>%
  modify_bold(
    columns = stat_1,  # primeira coluna
    rows = (variable == "Atraso_Partida" & label == "Com atraso"))%>%
  modify_bold(
    columns = stat_1,  # primeira coluna
    rows = (variable == "Atraso_Partida" & label=="Sem atraso"))%>%
  modify_footnote(everything() ~ NA)%>%
  as_gt() %>%                   
  fmt_markdown(columns = label) %>%
  tab_options(
    table.font.size = "20px",    
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )


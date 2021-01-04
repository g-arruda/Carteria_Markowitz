
# Bibliotecas utilizadas

library(tidyverse) #Usada para a limpeza dos dados
library(tidyquant) #Usada para a extração e manipulação dos dados 
library(tseries) #Usada para a Otimização do portifolio


# Extraindo os dados --------------------

#Ações que compoem o IBOVESPA em nov/2020
ibov <- read.csv2('D:/projetos_r/carteira_markowitz/ibovespa.csv',
                  header = TRUE, stringsAsFactors = FALSE)

ibov[, 1] <- paste0(ibov[, 1], '.SA')


# Criando uma função para extrair a cotação das ações

get_data <- function(ibov, period = c('close', 'adjusted')){
   
      x <- tq_get(ibov$code,
                  get = 'stock.prices',
                  from = '2017-05-02',
                  to = "2020-12-04") %>%
         na.omit()
      
      y <- x %>%
         select(date, symbol, period) %>%
         pivot_wider(id_cols = date,
                     names_from = symbol,
                     values_from = period) #Separando cada ação em uma coluna
      
      z <- xts(y[, -1], order.by = y$date) #tranformar o df em XTS
      
      
return(z)

}

price_matrix <- get_data(ibov, 'adjusted') #Matriz com o historico de cada ação

price_matrix$TIMS3.SA <- NULL #Por falta de dados, não será usada a ação da TIM




#Olhando as Ações ------------



asset_sector_list <- function(ibov){
   
   
   x <- vector(mode = 'list', length = length(unique(ibov$sector)))
   sector_names <- unique(ibov$sector)
   
   for (i in 1:length(x)) {
      
      x[[i]] <- ibov %>% filter(sector == sector_names[i]) %>%
         select(code)
      
   }
   
   names(x) <- sector_names
   
return(x)
   
}

sector_list <- asset_sector_list(ibov)


plot_by_sector <- function(data_frame, ibov, setor, returns = FALSE){
   
   if (returns == FALSE) {
      
      x <- fortify.zoo(data_frame) %>%
         pivot_longer(cols = -1,
                      names_to = 'Ação',
                      values_to = 'Cotação') %>%
         left_join(ibov, by = c('Ação' = 'code')) %>%
         filter(sector == setor)
      
      y <- x %>%
         ggplot(aes(Index, Cotação, color = Ação)) +
         geom_line() +
         labs(x = NULL, y = NULL, title = paste('Ações do Índice Bovespa do setor de',
                                                setor),
              caption = paste('Dados de',
                              min(format(x$Index, '%d/%m/%Y')),
                              'até',
                              max(format(x$Index, '%d/%m/%Y')))) +
         scale_y_continuous(labels = scales::dollar_format(prefix = 'R$'))
      
      y
   
      
   } else {
      
      x <- fortify.zoo(data_frame) %>%
         pivot_longer(cols = -1,
                      names_to = 'Ação',
                      values_to = 'Retorno') %>%
         left_join(ibov, by = c('Ação' = 'code')) %>%
         filter(sector == setor)
      
      y <- x %>%
         group_by(sector) %>%
         na.omit() %>%
         mutate(cum_return = cumsum(Retorno)) %>%
         ggplot(aes(Index, cum_return, color = Ação)) +
         geom_line() +
         labs(x = NULL, y = NULL, title = paste('Retorno das Ações do Índice Bovespa do setor de',
                                                'setor'),
              caption = paste('Dados de',
                              min(format(x$Index, '%d/%m/%Y')),
                              'até',
                              max(format(x$Index, '%d/%m/%Y')))) +
         scale_y_continuous(labels = scales::percent)
      
      y
      
   }   
   
}

### Olhando o preço das ações por setor
plot_by_sector(price_matrix, ibov, setor = 'Maquinas e Equipamentos')
plot_by_sector(price_matrix, ibov, setor = 'Transporte')
plot_by_sector(price_matrix, ibov, setor = 'Frigorifico')
plot_by_sector(price_matrix, ibov, setor = 'Varejo')
plot_by_sector(price_matrix, ibov, setor = 'Construção Civil')
plot_by_sector(price_matrix, ibov, setor = 'Viagens')
plot_by_sector(price_matrix, ibov, setor = 'Outros')
plot_by_sector(price_matrix, ibov, setor = 'Financeiro')
plot_by_sector(price_matrix, ibov, setor = 'Extração')
plot_by_sector(price_matrix, ibov, setor = 'Químico')
plot_by_sector(price_matrix, ibov, setor = 'Siderurgia/Metalurgia')
plot_by_sector(price_matrix, ibov, setor = 'Energia')
plot_by_sector(price_matrix, ibov, setor = 'Tecnologia')
plot_by_sector(price_matrix, ibov, setor = 'Telecomunicação')
plot_by_sector(price_matrix, ibov, setor = 'Saneamento')
plot_by_sector(price_matrix, ibov, setor = 'Energia Elétrica')



### Olhando o retorno acumulado das ações por setor

plot_by_sector(return_matrix, ibov, setor = 'Maquinas e Equipamentos', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Transporte', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Frigorifico', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Varejo', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Construção Civil', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Viagens', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Outros', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Financeiro', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Extração', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Químico', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Siderurgia/Metalurgia', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Energia', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Tecnologia', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Telecomunicação', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Saneamento', returns=TRUE)
plot_by_sector(return_matrix, ibov, setor = 'Energia Elétrica', returns=TRUE)



# Função que traz o retorno diario de cada ação

gen_return <- function(price_matrix, method){
   
   x <- price_matrix %>%
      Return.calculate(method = method)
   
return(x[-1,])

}


#Usando o retorno logarítmico para gerar uma matriz de retornos
return_matrix <- gen_return(price_matrix, method = 'log')




#Função que retorna o valor esperado de cada ação

gen_expect_return <- function(return_matrix){
      
      #Aplicando a função 'mean' para cada coluna
      expect_return <- apply(return_matrix, 2, mean, na.rm = TRUE)
      
   return(expect_return)

}

expect_return <- gen_expect_return(return_matrix)




#Função que retorna o risco(desvio padrão) de cada ação

gen_assets_risk <- function(return_matrix){
   
      #aplicando a função 'sd' para cada coluna   
      assets_risk <- apply(return_matrix, 2, sd, na.rm = TRUE)
   
   return(assets_risk)
   
}




assets_risk <- gen_assets_risk(return_matrix)



#Função que calcula o Índice de Sharpe de cada ação

calc_sharpe_ratio <- function(return_matrix, Rf){
   
   x <- SharpeRatio(return_matrix,
                    Rf = Rf,       #'Rf' deve ser a tx livre de risco diária
                    FUN = 'VaR')
   
   return(x)
   
}


sharpe_matrix <- calc_sharpe_ratio(return_matrix, Rf = 0.00031888)



#Função que gera um data frame das informações resumidas de cada ação

gen_summary_df <- function(expect_return, assets_risk, sharpe_matrix){
   
   x <- rbind(expect_return, assets_risk, sharpe_matrix) %>%
      t() %>%
      as.data.frame()
   
   colnames(x) <- c('Expect Return', 'Risk', 'Sharpe')
   
   x <- rownames_to_column(x, 'code')
   
   y <- inner_join(x, ibov,
                   by = 'code')
   
return(y)

}

summary_df <- gen_summary_df(expect_return, assets_risk, sharpe_matrix)



#Montando a carteira --------------



#Função que escolhe de forma aleatoria as ações que irá compor a carteira

gen_port_assets <- function(ibov, size){
   
   
   x <- sample(unique(ibov$sector), size = size, replace = FALSE)
   
   
   y <- ibov %>%
      filter(sector %in% x) %>%
      group_by(sector) %>%
      sample_n(1)

   
return(ibov[which(ibov$code %in% y$code), ])
   

}

pf_assets <- gen_port_assets(ibov, size = 4)




#Função que cria uma matriz com os retornos diários das ações da carteira
pf_assets_return <- function(pf_assets, return_matrix){
   
   x <- pf_assets$code
   
   y <- return_matrix[, x] %>% na.omit()
   
return(y)   

}

pf_assets_return_matrix <- pf_assets_return(pf_assets,
                                            return_matrix)



#Matrix de covariancia
cov_matrix <- cov(pf_assets_return_matrix, use = "pairwise.complete.obs")

#Matriz de correlação
cor_matrix <- cor(pf_assets_return_matrix, use = "pairwise.complete.obs")



#Função que será usada para calcular o retorno esperado da carteira

calc_pf_return <- function(pf_assets_return_matrix, weights){
   weights <- as.matrix(weights, ncol = 1)
   pf_return_vec <- pf_assets_return_matrix %*% weights
   pf_return <- (prod(1 + pf_return_vec)) ^ (252/length(pf_return_vec)) - 1 
}




pf_return <- calc_pf_return(pf_assets_return_matrix,
                            weights = rep(0.25,
                                          ncol(pf_assets_return_matrix)))



#Função que será usada para calcular o risco esperado da carteira

calc_pf_vol <- function(cov_matrix, weights){
   weights <- as.matrix(weights, ncol = 1)
   cov_matrix <- as.matrix(cov_matrix)
   pf_var <- t(weights) %*% cov_matrix %*% weights
   pf_vol <- sqrt(pf_var) * sqrt(252)
   pf_vol <- as.numeric(pf_vol)
}


pf_vol <- calc_pf_vol(cov_matrix,
                      weights = rep(0.25,
                                    ncol(pf_assets_return_matrix)))



# Monte Carlos -------------


#Função que gera carteiras aleatórias

gen_random_pf <- function(pf_assets_return_matrix, n_pf_simulated, Rf){
   reps = n_pf_simulated
   pf_return <- numeric()
   pf_vol <- numeric()
   carteira <- matrix(NA, nrow = reps, ncol = 3)
   
   for (i in 1:reps) {
      
      #Apenas pesos positivos, não permitindo 'shorts'
      weights <- runif(ncol(pf_assets_return_matrix), min = 0, max = 1) 
      
      #Garantindo que 100% do capital será alocado
      weights <- weights / sum(weights)
      
      pf_return <- calc_pf_return(pf_assets_return_matrix, weights)
      pf_vol <- calc_pf_vol(cov_matrix, weights)
      pf_sharpe <- (pf_return - Rf) / pf_vol
      carteira[i, 1] <- pf_return
      carteira[i, 2] <- pf_vol
      carteira[i, 3] <- pf_sharpe
   }
   colnames(carteira) <- c("return", "vol", 'Sharpe')
   carteira <- as.data.frame(carteira)
   
   return(carteira)
}

random_pf <- gen_random_pf(pf_assets_return_matrix,
                           n_pf_simulated = 30000,
                           Rf = 0.00031888)



#Visualizando a fronteira eficiente

random_pf %>%
   ggplot(aes(vol, return, colour = Sharpe)) +
   geom_point()



#----------------------------

## Otimização com o pacote tseries


#Função que retorna a carteira ótima a partir da métrica de média-variancia 

gen_optmal_portfolio <- function(pf_assets_return_matrix, shorts = FALSE){

   op <- portfolio.optim(as.matrix(pf_assets_return_matrix),
                         riskless = FALSE,
                         shorts = shorts)
   

   weight <- op$pw
   names(weight) <- colnames(pf_assets_return_matrix)
   weight <- round(weight, 2)
   
   
   res <- list(weight = weight,
               return = op$pm,
               risk = op$ps)
   
   return(res)
}


optmal_portfolio <- gen_optmal_portfolio(pf_assets_return_matrix)



#Criando um df para melhor visualização da carteira

assets_matrix <- function(pf_assets_return_matrix, optmal_portfolio){
   
   x <- tibble(Return = gen_expect_return(pf_assets_return_matrix) * 100,
               Risk = gen_assets_risk(pf_assets_return_matrix) * 100,
               Weight = optmal_portfolio$weight * 100) %>%
      round(2)
   
   y <- cbind(Ticker = colnames(pf_assets_return_matrix), x) %>%
      arrange(desc(Weight))
   
   
   y[, -1] <- apply(y[, -1], 2, paste0, '%')
   
   
   return(y)
}

portfolio_composition <- assets_matrix(pf_assets_return_matrix,
                                       optmal_portfolio)

portfolio_composition

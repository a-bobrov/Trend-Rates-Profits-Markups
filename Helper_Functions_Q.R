# **************************************
# Author: Anton Bobrov
# Anton.Bobrov@sf.frb.org
# Sensitivity Paper FRED VERSION Helper Functions
# Last modified: 07/03/2024
# **************************************
load.data.FRED <- function(){
  GS10 <- fredr(series_id = "GS10", frequency = "q") %>%
    select(c(1,3)) %>% dplyr::rename("GS10" = "value") %>%
    mutate(date = as.Date(date))
  
  CPI <- fredr(series_id = "CPIAUCSL", frequency = "q", units = "pc1") %>%
    select(c(1,3)) %>% dplyr::rename("CPI_infl" = "value") %>%
    mutate(date = as.Date(date))
  
  CP <- fredr(series_id = "CP", frequency = "q") %>%
    select(c(1,3)) %>% dplyr::rename("CP" = "value") %>%
    mutate(date = as.Date(date))
  
  GDP <- fredr(series_id = "GDP", frequency = "q") %>%
    select(c(1,3)) %>% dplyr::rename("GDP" = "value") %>%
    mutate(date = as.Date(date))
  
  BAA <- fredr(series_id = "BAA", frequency = "q") %>%
    select(c(1,3)) %>% dplyr::rename("BAA" = "value") %>%
    mutate(date = as.Date(date))
  
  IVA <- fredr("NCBIVDQ027S", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("IVA" = "value") %>%
    mutate(date = as.Date(date))
  
  ### Current Account
  WL <- fredr("BOGZ1FU106025005Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("WL" = "value") %>%
    mutate(date = as.Date(date),
           WL = WL * 4) 
  
  Delta <- fredr("BOGZ1FU106300003Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("Delta" = "value") %>%
    mutate(date = as.Date(date),
           Delta = Delta * 4) 
  
  Tau <- fredr("BOGZ1FU106220001Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("Tau" = "value") %>%
    mutate(date = as.Date(date),
           Tau = Tau * 4) 
  
  GVA <- fredr("BOGZ1FU106902501Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("GVA" = "value") %>%
    mutate(date = as.Date(date),
           GVA = GVA * 4)
  
  production_tax <- fredr("BOGZ1FU106240101Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("production_tax" = "value") %>%
    mutate(date = as.Date(date),
           production_tax = production_tax * 4)
  
  ### Capital Account
  Nu_inv <- fredr("BOGZ1FR105020015Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_inventories" = "value") %>%
    mutate(date = as.Date(date),
           Nu_inventories = Nu_inventories - IVA$IVA)  # Nu_inventories (replace Nu_inventories = Nu_inventories - IVA // note: gains include IVA)
  
  K_inv <- fredr("BOGZ1LM105020015Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("K_inventories" = "value") %>%
    mutate(date = as.Date(date)) 
  
  Nu_equip <- fredr("BOGZ1FR105015205Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_equipment" = "value") %>%
    mutate(date = as.Date(date))
  
  K_equip <- fredr("BOGZ1LM105015205Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("K_equipment" = "value") %>%
    mutate(date = as.Date(date))
  
  Nu_ipp <- fredr("BOGZ1FR105013765Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_ipp" = "value") %>%
    mutate(date = as.Date(date)) 
  
  K_ipp <- fredr("NCBNIPPCCB", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("K_ipp" = "value") %>%
    mutate(date = as.Date(date),
           K_ipp = K_ipp * 1000) 
  
  Nu_struct <- fredr("BOGZ1FR105013665Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_structures" = "value") %>%
    mutate(date = as.Date(date))
  
  K_struct <- fredr("RCSNNWMVBSNNCB", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("K_structures" = "value") %>%
    mutate(date = as.Date(date),
           K_structures = K_structures * 1000)
  
  B_struct <- fredr("HCVSNNWHCBSNNCB", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("B_structures" = "value") %>%
    mutate(date = as.Date(date),
           B_structures = B_structures * 1000)
  
  B_land <- fredr("BOGZ1FL105010023Q", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("B_land" = "value") %>%
    mutate(date = as.Date(date))
  
  ### Financial Account
  debt_securities <- fredr("NCBDBIQ027S", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("debt_securities" = "value") %>%
    mutate(date = as.Date(date))
  
  loans <- fredr("NCBLL", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("loans" = "value") %>%
    mutate(date = as.Date(date))
  
  E <- fredr("NCBCEL", frequency = "q")  %>%
    select(c(1,3)) %>% dplyr::rename("E" = "value") %>%
    mutate(date = as.Date(date),
           E = E * 1000)
  
  df_list <- list(GS10, CP, GDP, CPI, BAA, debt_securities, loans, E, GVA, production_tax, WL, Delta, 
                  Tau, B_land, B_struct, IVA, K_equip, K_inv, K_ipp, K_struct, 
                  Nu_equip, Nu_inv, Nu_ipp, Nu_struct)
  return(df_list)
}

load.data.Barkai <- function() {
  figure2b_data <- read.csv("data/Barkai (2020)/figure2b.csv") %>% filter(variable == "Expected Capital Inflation")
  figure3_data <- read.csv("data/Barkai (2020)/figure3.csv")
  half_merge <- merge(figure2b_data, figure3_data, by = "year")
  
  Barkai_int_rates <- fredr(series_id = "GS10", frequency = "a") %>%
    select(c(1,3)) %>% dplyr::rename("GS10" = "value") %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= "1981-01-01" & date < "2016-01-01") %>%
    mutate(expected_capital_inflation = figure2b_data[figure2b_data$variable == "Expected Capital Inflation", "value"] * 100,
           year = year(date))
  
  full_merge <- merge(Barkai_int_rates, half_merge, by = "year") %>% 
    select(year, GS10, expected_capital_inflation, Y, wL, capital_costs, profits, topils, R, lagged_K_current) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))

  
  return(full_merge)
}

gen.vars <- function(df_list){
  #Combine 
  master <- df_list %>% reduce(full_join, by='date') %>%
    na.omit() %>%
    mutate(K_land = K_structures / B_structures * B_land,
           Nu_land = Nu_structures / K_structures * K_land,
           Nu = (Nu_inventories + Nu_equipment + Nu_ipp + Nu_structures + Nu_land) * 4,
           K = K_inventories + K_equipment + K_ipp + K_structures + K_land,
           D = debt_securities + loans,
           nu = rollmean(Nu / K, 12, fill = NA, align = "right") * 100,
           year = year(date),
           delta = Delta / K,
           tau = Tau / K,
           R_d = BAA / 100,
           R_e = GS10 / 100 + 0.05,
           rho = D / (D + E) * R_d + E / (D + E) * R_e,
           R_c = rho - nu / 100 + delta + tau,
           Y = GVA - production_tax,
           R = (Y - WL) / K,
           profit_rate = (R - R_c) * K/Y,
           volatility = roll_sd(x = (GS10 - nu), width = 20, min_obs = 12),
           mu = (2 / (2 - profit_rate) ),
           mu_growth = ((mu / lag(mu))^12 - 1) * 100,
           corp_profit = CP/GDP * 100)
  
  return(master)
}

generate.figures <- function(){
  #Figure 1: Real Interest Rate: i - nu
  figure1a <- ggplot(data = master, aes(x = date, y = GS10 - nu)) + 
    geom_line(color = "blue", size  = 0.8) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size  = 0.8) + 
    labs(x = "", y = "Real Interest Rate")
  ggsave("output/figure1a.jpeg", figure1a,  width = 1800, height = 1200, units = "px")
  
  #Figure 1c: Volatility of Real Financial Market Returns
  figure1c <- ggplot(data = master, aes(x = date, y = volatility)) + 
    geom_line(color = "blue", size  = 0.8) + 
    labs(x = "", y = "Volatility of Real Finacial Market Returns") 
    #scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  ggsave("output/figure1c.jpeg", figure1c, width = 1800, height = 1200, units = "px")
  
  
  #Figure 2: Cost of Capital: R_C
  figure2a <- ggplot(data = master, aes(x = date, y = R_c)) + 
    geom_line(color = "blue", size  = 0.8) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size  = 0.8) +
    labs(x = "", y = "Cost of Capital")
  ggsave("output/figure2a.jpeg", figure2a,  width = 1800, height = 1200, units = "px")
  
  #Figure 3: Economic Profits, Markups
  figure3a <- ggplot(data = master, aes(x = date, y = profit_rate)) + 
    geom_line(color = "blue", size  = 0.8) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size  = 0.8) +
    labs(x = "", y = "Economic Profit Share")
  ggsave("output/figure3a.jpeg", figure3a,  width = 1800, height = 1200, units = "px")
  
  figure3c <- ggplot(data = master, aes(x = date, y = 2 / (2 - profit_rate))) + 
    geom_line(color = "blue", size  = 0.8) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size  = 0.8) +
    labs(x = "", y = "Markup")
  ggsave("output/figure3c.jpeg", figure3c,  width = 1800, height = 1200, units = "px")
  
  ### Long Differences 
  
  figure1d <- ggplot(data = master, aes(x = date, y = (GS10 - nu) - lag(GS10 - nu, 15))) + 
    geom_line(color = "blue", size  = 0.8) + 
    labs(x = "End of Window", y = "Real Interest Rate 15 Year Long Difference") + 
    scale_x_date(limits = c(as.Date("1970-01-01"), as.Date("2022-01-01")))
  ggsave("output/figure1d.jpeg", figure1d,  width = 1800, height = 1200, units = "px")
  
  figure2c <- ggplot(data = master, aes(x = date, y = R_c - lag(R_c, 15))) + 
    geom_line(color = "blue", size  = 0.8) + 
    labs(x = "End of Window", y = "Cost of Capital 15 Year Long Difference") + 
    scale_x_date(limits = c(as.Date("1970-01-01"), as.Date("2022-01-01")))
  ggsave("output/figure2c.jpeg", figure2c,  width = 1800, height = 1200, units = "px")
  
  figure3f <- ggplot(data = master, aes(x = date, y = profit_rate - lag(profit_rate, 15))) + 
    geom_line(color = "blue", size  = 0.8) + 
    labs(x = "End of Window", y = "Profit Share 15 Year Long Difference") +
    scale_x_date(limits = c(as.Date("1970-01-01"), as.Date("2022-01-01")))
  ggsave("output/figure3f.jpeg", figure3f,  width = 1800, height = 1200, units = "px")
  
  figure3g <- ggplot(data = master, aes(x = date, y = 2 / (2 - profit_rate) - lag(2 / (2 - profit_rate), 15))) + 
    geom_line(color = "blue", size  = 0.8) + 
    labs(x = "End of Window", y = "Markup 15 Year Long Difference") + 
    scale_x_date(limits = c(as.Date("1970-01-01"), as.Date("2022-01-01")))
  ggsave("output/figure3g.jpeg", figure3g,  width = 1800, height = 1200, units = "px")
  
  ### Greedflation
  #test 
  ggplot(data = master %>% filter(date > as.Date("2010-01-01")), aes(x = date, y =corp_profit ) )+
    geom_line(aes(color = "blue"), size  = 0.8, show.legend = T) + 
    geom_line(size  = 0.8, aes(x = date, y = CPI_infl * 2, color = "red"),  show.legend = T) + 
    labs(x = "", y = "Profits, Inflation Rate", color = "") + 
    scale_color_manual(values = c("blue", "red"), labels = c("Corporate Profits/GDP", "CPI Inflation")) + 
    scale_y_continuous(
      # Features of the first axis
      name = "First Axis",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./2, name="Second Axis")
    ) +
    theme(legend.position = c(0.3, 0.8), legend.spacing.y = unit(0.25, "cm")  )
  
  figure4a <- ggplot(data = master %>% filter(date > as.Date("1980-01-01")), aes(x = date, y =corp_profit ) )+
    geom_line(aes(color = "blue"), size  = 0.8, show.legend = T) + 
    geom_line(size  = 0.8, aes(x = date, y = CPI_infl, color = "red"),  show.legend = T) + 
    labs(x = "", y = "Profits, Inflation Rate", color = "") + 
    scale_color_manual(values = c("blue", "red"), labels = c("Corporate Profits/GDP", "CPI Inflation")) + 
    theme(legend.position = c(0.3, 0.8), legend.spacing.y = unit(0.25, "cm")  )

  
  ggsave("output/figure4a.jpeg", figure4a, width = 1800, height = 1200, units = "px") 
  
  tsData <- master[, c(5, 45)]
  rolling_corr <- rollapply(tsData, width=40, function(x) cor(x[,1],x[,2]), by.column=FALSE)
  
  master$greedflation  <- c(rep(NA, nrow(master) - length(rolling_corr)), rolling_corr)
  
  #Sensitivity of Correlation
  years = seq(1959, 2019, 1)
  correlation = numeric(61)
  correlation_nocovid = numeric(60)
  for (i in 1:61){
     subset <- master %>% filter(date >= as.Date(paste0(years[i], "-01-01")))
      if (i < 2019){
        subset_nocovid <- subset %>% filter(date < as.Date("2019-01-01") )
      }
      correlation[i] <- cor(subset$CPI_infl, subset$corp_profit)
      correlation_nocovid[i] <- cor(subset_nocovid$CPI_infl, subset_nocovid$corp_profit)

  }
  
  correlation_df <- data.frame(years, correlation, correlation_nocovid)

  figure4b <- ggplot(correlation_df, aes(x = years, y = correlation )) + 
    geom_line(aes(color = "2023"), size  = 0.8) +
    geom_line(aes(x = years, y = correlation_nocovid, color = "2018"), size  = 0.8)+
    labs(x = "Start Year", color = "Sample End Year", y = "Correlation")  +
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position = c(0.2, 0.7))
  ggsave("output/figure4b.jpeg", figure4b, width = 1800, height = 1200, units = "px")
  
  figure4c <- ggplot(master %>% filter(date >= as.Date("1963-01-01")), aes(x = date, y = greedflation )) + 
    geom_line(aes(color = "Greedflation"), size  = 0.8) + 
    geom_line(size = 0.8, aes(x = date, y = CPI_infl / 15, color = "CPI Inflation")) +
    scale_y_continuous(
      #First axis
      name = "10 Year Rolling Correlation",
      breaks = seq(-1, 1, by = 0.25),
      #breaks = 10,
      #Second axis
      sec.axis = sec_axis(~.*15, name = "CPI Inflation", breaks = seq(-15, 15, 3.75))
    ) +
    labs(x = "", color = "", y = " 10 Year Rolling Correlation")  +
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position = c(0.7, 0.8))
  ggsave("output/figure4c.jpeg", figure4c, width = 1800, height = 1200, units = "px")
  
  ## Coefficient Plots (Beginning of Trend)
  coeff_1b <- numeric(10)
  coeff_2b <- numeric(10)
  coeff_3b <- numeric(10)
  coeff_3d <- numeric(10)
  position <-  1
  
  for (i in seq(1980, 1989, 1)){
    coeff_1b[position] <- lm(data = master %>% filter(year %in% i:2019), 
                             formula = (GS10 - nu) ~ year)$coeff[2]
    
    coeff_2b[position] <- lm(data = master %>% filter(year %in% i:2019), 
                             formula = R_c ~ year)$coeff[2]
    
    coeff_3b[position] <- lm(data = master %>% filter(year %in% i:2019), 
                             formula = profit_rate ~ year)$coeff[2]
    
    coeff_3d[position] <- lm(data = master %>% filter(year %in% i:2019), 
                             formula = 2/ (2 - profit_rate) ~ year)$coeff[2]
    position = position + 1

  }
  
  start_date <- data.frame(date = seq(as.Date("1980-01-01"), as.Date("1989-01-01"), by = "year")
                           , coeff_1b, coeff_2b, coeff_3b, coeff_3d)
  
  figure1b <- ggplot(start_date, aes(x = date, y = ((coeff_1b  / coeff_1b[1]) - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "Start Year", color = "", y = "Real Interest Rate (% Change)") 
  ggsave("output/figure1b.jpeg", figure1b, width = 1800, height = 1200, units = "px")
  
  figure2b <- ggplot(start_date, aes(x = date, y = (coeff_2b  / coeff_2b[1] - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "Start Year", color = "", y = "Cost of Capital Coefficient (% Change)") 
  ggsave("output/figure2b.jpeg", figure2b, width = 1800, height = 1200, units = "px")
  
  figure3b <- ggplot(start_date, aes(x = date, y = (coeff_3b  / coeff_3b[1] - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "Start Year", color = "", y = "Profit Share Coefficient (% Change)") 
  ggsave("output/figure3b.jpeg", figure3b, width = 1800, height = 1200, units = "px")
  
  figure3d <- ggplot(start_date, aes(x = date, y = (coeff_3d  / coeff_3d[1] - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "Start Year", color = "", y = "Markup Coefficient (% Change)") 
  ggsave("output/figure3d.jpeg", figure3d, width = 1800, height = 1200, units = "px")
  
  
  ## Coefficient Plots (End of Trend)
  coeff_1_apdx <- numeric(10)
  coeff_2_apdx <- numeric(10)
  coeff_3a_apdx <- numeric(10)
  coeff_3c_apdx <- numeric(10)
  position <-  1
  
  for (i in seq(2014, 2023, 1)){
    coeff_1_apdx[position] <- lm(data = master %>% filter(year %in% 1984:i), 
                             formula = (GS10 - nu) ~ year)$coeff[2]
    
    coeff_2_apdx[position] <- lm(data = master %>% filter(year %in% 1984:i), 
                             formula = R_c ~ year)$coeff[2]
    
    coeff_3a_apdx[position] <- lm(data = master %>% filter(year %in% 1984:i), 
                             formula = profit_rate ~ year)$coeff[2]
    
    coeff_3c_apdx[position] <- lm(data = master %>% filter(year %in% 1984:i), 
                             formula = 2/ (2 - profit_rate) ~ year)$coeff[2]
    position = position + 1
    
  }
  
  end_date <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-01-01"), by = "year")
                           , coeff_1_apdx, coeff_2_apdx, coeff_3a_apdx, coeff_3c_apdx)
  
  figure1_apdx <- ggplot(end_date, aes(x = date, y = ((coeff_1_apdx  / coeff_1_apdx[1]) - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "End Year", color = "", y = "Real Interest Rate (% Change)") 
  ggsave("output/figure1_apdx.jpeg", figure1_apdx, width = 1800, height = 1200, units = "px")
  
  figure2_apdx <- ggplot(end_date, aes(x = date, y = (coeff_2_apdx  / coeff_2_apdx[1] - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "End Year", color = "", y = "Cost of Capital Coefficient (% Change)") 
  ggsave("output/figure2_apdx.jpeg", figure2_apdx, width = 1800, height = 1200, units = "px")
  
  figure3a_apdx <- ggplot(end_date, aes(x = date, y = (coeff_3a_apdx  / coeff_3a_apdx[1] - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "End Year", color = "", y = "Profit Share Coefficient (% Change)") 
  ggsave("output/figure3a_apdx.jpeg", figure3a_apdx, width = 1800, height = 1200, units = "px")
  
  figure3c_apxdx <- ggplot(end_date, aes(x = date, y = (coeff_3c_apdx  / coeff_3c_apdx[1] - 1) * 100)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "End Year", color = "", y = "Markup Coefficient (% Change)") 
  ggsave("output/figure3c_apxdx.jpeg", figure3c_apxdx, width = 1800, height = 1200, units = "px")
  
  
  
  
  #Greedflation beta graph
  ## Coefficient Plots (Beginning of Trend)
  coeff_4d <- numeric(11)
  position <-  1
  
  for (i in seq(1970, 2020, 5)){
    coeff_4d[position] <- lm(data = master %>% filter(year %in% i:2023), formula = CPI_infl ~ corp_profit)$coeff[2]
    position = position + 1
  }
  
  greed_beta <-  data.frame(year = seq(1970, 2020, 5), coeff_4d)
  
  figure4d <- ggplot(greed_beta, aes(x = year, y = coeff_4d)) + 
    geom_point(color = "red", size  = 0.9) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size  = 0.8) +
    labs(x = "Start Year", color = "", y = "Greedflation Beta") 
  ggsave("output/figure4d.jpeg", figure4d, width = 1800, height = 1200, units = "px")
  
  master$influence <- c(rep(NA, 11), cooks.distance(lm(data = master, formula = (GS10 - nu) ~ date)))
  
  figure4e <- ggplot(master, aes(x = date, y = influence )) + 
    geom_line(color = "blue", size = 0.8) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size  = 0.8) + 
    labs(x = "" , y = "Point Influence")
  ggsave("output/figure4e.jpeg", figure4e, width = 1800, height = 1200, units = "px")
  
  #Comparison Plots -- Appendix
  #Expected Capital Inflation
  comp_inflation <- ggplot() + 
    geom_line(data = master, aes(x = date, y = nu), color = "blue", size  = 0.8) + 
    geom_line(data = Barkai, aes(x = date, y = expected_capital_inflation, color = "Barkai (2020)"), size  = 0.8) +
    labs(x = "", y = "Expected Capital Inflation", color = "") + 
    theme(legend.position = c(0.75, 0.758)) + 
    scale_color_manual(values = c("red")) 
  ggsave("output/comp_inflation.jpeg", comp_inflation, width = 1800, height = 1200, units = "px")
  
  #Cost of Capital
  comp_capital_cost <- ggplot() +
    geom_line(data = Barkai, aes(x = date, y = R, color = "Barkai (2020)"), size  = 0.8) + 
    geom_line(data = master, aes(x = date, y = R_c), color = "blue", size  = 0.8) +
    labs(x = "", y = "Cost of Capital", color = "") +
    theme(legend.position = c(0.2, 0.9)) + 
    scale_color_manual(values = c("red")) 
  ggsave("output/comp_capital_cost.jpeg", comp_capital_cost, width = 1800, height = 1200, units = "px")
  
  #Economic Profit Share
  comp_profit <- ggplot() +
    geom_line(data = Barkai, aes(x = date, y = profits / Y, color = "Barkai (2020)"), size  = 0.8) + 
    geom_line(data = master, aes(x = date, y = profit_rate), color = "blue", size  = 0.8) +
    labs(x = "", y = "Economic Profit Share", color = "") +
    theme(legend.position = c(0.2, 0.9)) + 
    scale_color_manual(values = c("red")) 
  ggsave("output/comp_profit.jpeg", comp_profit, width = 1800, height = 1200, units = "px")
  
  #compare Real Interest Rate
  comp_interest_rate <- ggplot() +
    geom_line(data = Barkai, aes(x = date, y = GS10 - expected_capital_inflation, color = "Barkai (2020)"), size  = 0.8) + 
    geom_line(data = master, aes(x = date, y = GS10 - nu), color = "blue", size  = 0.8) + 
    labs(x = "", y = "Real Interest Rate", color = "") + 
    theme(legend.position = c(0.75, 0.9)) + 
    scale_color_manual(values = c("red"))  
  ggsave("output/comp_interest_rate.jpeg", comp_interest_rate, width = 1800, height = 1200, units = "px")
  
  
}

# **************************************
# Author: Anton Bobrov
# Anton.Bobrov@sf.frb.org
# Sensitivity Paper FRED VERSION Helper Functions
# Last modified: 06/06/2022
# **************************************
load.data.FRED <- function(){
  GS10 <- fredr(series_id = "GS10", frequency = "a") %>%
    select(c(1,3)) %>% dplyr::rename("GS10" = "value") %>%
    mutate(date = as.Date(date))
  
  BAA <- fredr(series_id = "BAA", frequency = "a") %>%
    select(c(1,3)) %>% dplyr::rename("BAA" = "value") %>%
    mutate(date = as.Date(date))
  
  IVA <- fredr("NCBIVDA027N", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("IVA" = "value") %>%
    mutate(date = as.Date(date))
  
  ### Current Account
  WL <- fredr("BOGZ1FU106025005A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("WL" = "value") %>%
    mutate(date = as.Date(date)) 
  
  Delta <- fredr("BOGZ1FU106300003A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("Delta" = "value") %>%
    mutate(date = as.Date(date)) 
  
  Tau <- fredr("BOGZ1FU106220001A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("Tau" = "value") %>%
    mutate(date = as.Date(date)) 
  
  GVA <- fredr("BOGZ1FU106902501A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("GVA" = "value") %>%
    mutate(date = as.Date(date))
  
  production_tax <- fredr("BOGZ1FU106240101A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("production_tax" = "value") %>%
    mutate(date = as.Date(date))
  
  ### Capital Account
  Nu_inv <- fredr("BOGZ1FR105020015A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_inventories" = "value") %>%
    mutate(date = as.Date(date),
           Nu_inventories = Nu_inventories - IVA$IVA)  # Nu_inventories (replace Nu_inventories = Nu_inventories - IVA // note: gains include IVA)
  
  K_inv <- fredr("BOGZ1LM105020015A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("K_inventories" = "value") %>%
    mutate(date = as.Date(date)) 
  
  Nu_equip <- fredr("BOGZ1FR105015205A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_equipment" = "value") %>%
    mutate(date = as.Date(date))
  
  K_equip <- fredr("BOGZ1LM105015205A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("K_equipment" = "value") %>%
    mutate(date = as.Date(date))
  
  Nu_ipp <- fredr("BOGZ1FR105013765A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_ipp" = "value") %>%
    mutate(date = as.Date(date)) 
  
  K_ipp <- fredr("BOGZ1FL105013765A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("K_ipp" = "value") %>%
    mutate(date = as.Date(date)) 
  
  Nu_struct <- fredr("BOGZ1FR105013665A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("Nu_structures" = "value") %>%
    mutate(date = as.Date(date))
  
  K_struct <- fredr("BOGZ1FL105013665A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("K_structures" = "value") %>%
    mutate(date = as.Date(date))
  
  B_struct <- fredr("BOGZ1FL105013613A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("B_structures" = "value") %>%
    mutate(date = as.Date(date))
  
  B_land <- fredr("BOGZ1FL105010023A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("B_land" = "value") %>%
    mutate(date = as.Date(date))
  
  ### Financial Account
  debt_securities <- fredr("NCBDBIA027N", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("debt_securities" = "value") %>%
    mutate(date = as.Date(date))
  
  loans <- fredr("BOGZ1FL104123005A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("loans" = "value") %>%
    mutate(date = as.Date(date))
  
  E <- fredr("BOGZ1LM103164103A", frequency = "a")  %>%
    select(c(1,3)) %>% dplyr::rename("E" = "value") %>%
    mutate(date = as.Date(date))
  
  df_list <- list(GS10, BAA, debt_securities, loans, E, GVA, production_tax, WL, Delta, 
                  Tau, B_land, B_struct, IVA, K_equip, K_inv, K_ipp, K_struct, 
                  Nu_equip, Nu_inv, Nu_ipp, Nu_struct)
  return(df_list)
}

load.data.Barkai <- function() {
  figure2b_data <- read.csv("figure2b.csv") %>% filter(variable == "Expected Capital Inflation")
  figure3_data <- read.csv("figure3.csv")
  half_merge <- merge(figure2b_data, figure3_data, by = "year")
  
  Barkai_int_rates <- fredr(series_id = "GS10", frequency = "a") %>%
    select(c(1,3)) %>% dplyr::rename("GS10" = "value") %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= "1981-01-01" & date < "2016-01-01") %>%
    mutate(expected_capital_inflation = figure2b_data[figure2b_data$variable == "Expected Capital Inflation", "value"] * 100,
           year = year(date))
  
  full_merge <- merge(Barkai_int_rates, half_merge, by = "year") %>% 
    select(year, GS10, expected_capital_inflation, Y, wL, capital_costs, profits, topils, R, lagged_K_current)
  
  return(full_merge)
}

gen.vars <- function(df_list){
  #Combine 
  master <- df_list %>% reduce(full_join, by='date') %>%
    na.omit() %>%
    mutate(K_land = K_structures / B_structures * B_land,
           Nu_land = Nu_structures / K_structures * K_land,
           Nu = Nu_inventories + Nu_equipment + Nu_ipp + Nu_structures + Nu_land,
           K = K_inventories + K_equipment + K_ipp + K_structures + K_land,
           D = debt_securities + loans,
           K = (lag(K) + K) / 2, #stocks to mid-year
           D = (lag(D) + D) / 2, #stocks to mid-year
           E = (lag(E) + E) / 2, #stocks to mid-year
           nu = rollmean(Nu / K, 4, fill = NA, align = "right") * 100,
           year = year(date),
           delta = Delta / K,
           tau = Tau / K,
           R_d = BAA / 100,
           R_e = GS10 / 100 + 0.05,
           rho = D / (D + E) * R_d + E / (D + E) * R_e,
           R_c = rho - nu / 100 + delta + tau,
           Y = GVA - production_tax,
           R = (Y - WL) / K,
           profit_rate = (R - R_c) * K/Y)
  
  return(master)
}

generate.figures <- function(){
  #Figure 1: Real Interest Rate: i - nu
  figure1a <- ggplot(data = master %>% filter(year < 2020), aes(x = date, y = GS10 - nu)) + 
    geom_line(color = "blue", size = 0.6) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size = 0.6) + 
    labs(x = "", y = "Real Interest Rate")
  
  ggsave("figure1a.jpeg", figure1a,  width = 900, height = 600, units = "px")
  
  #Figure 2: Cost of Capital: R_C
  figure2a <- ggplot(data = master %>% filter(year < 2020), aes(x = date, y = R_c)) + 
    geom_line(color = "blue", size = 0.6) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size = 0.6) +
    labs(x = "", y = "Cost of Capital")
  ggsave("figure2a.jpeg", figure2a,  width = 900, height = 600, units = "px")
  
  #Figure 3: Economic Profits, Markups
  figure3a <- ggplot(data = master %>% filter(year < 2020), aes(x = date, y = profit_rate)) + 
    geom_line(color = "blue", size = 0.6) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size = 0.6) +
    labs(x = "", y = "Economic Profit Share")
  ggsave("figure3a.jpeg", figure3a,  width = 900, height = 600, units = "px")
  
  figure3c <- ggplot(data = master %>% filter(year < 2020), aes(x = date, y = 2 / (2 - profit_rate))) + 
    geom_line(color = "blue", size = 0.6) + 
    geom_line(data = master %>% filter(year > 1983 & year < 2015), color = "red", size = 0.6) +
    labs(x = "", y = "Markup")
  ggsave("figure3c.jpeg", figure3c,  width = 900, height = 600, units = "px")
  
  #Comparison Plots
  #Expected Capital Inflation
  comp_inflation <- ggplot() + 
    geom_line(data = master %>% filter(year < 2020), aes(x = year, y = nu), color = "blue", size = 0.6) + 
    geom_line(data = Barkai, aes(x = year, y = expected_capital_inflation, color = "Barkai"), size = 0.6) +
    labs(x = "", y = "Expected Capital Inflation", color = "") + 
    theme(legend.position = c(0.75, 0.758)) + 
    scale_color_manual(values = c("red")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  ggsave("comp_inflation.jpeg", comp_inflation, width = 900, height = 600, units = "px")
  
  #Cost of Capital
  comp_capital_cost <- ggplot() +
    geom_line(data = Barkai, aes(x = year, y = R, color = "Barkai"), size = 0.6) + 
    geom_line(data = master %>% filter(year < 2020), aes(x = year, y = R_c), color = "blue", size = 0.6) +
    labs(x = "", y = "Cost of Capital", color = "") +
    theme(legend.position = c(0.2, 0.9)) + 
    scale_color_manual(values = c("red")) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  ggsave("comp_capital_cost.jpeg", comp_capital_cost, width = 900, height = 600, units = "px")
  
  #Economic Profit Share
  comp_profit <- ggplot() +
    geom_line(data = Barkai, aes(x = year, y = profits / Y, color = "Barkai"), size = 0.6) + 
    geom_line(data = master %>% filter(year < 2020), aes(x = year, y = profit_rate), color = "blue", size = 0.6) +
    labs(x = "", y = "Economic Profit Share", color = "") +
    theme(legend.position = c(0.2, 0.9)) + 
    scale_color_manual(values = c("red")) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  ggsave("comp_profit.jpeg", comp_profit, width = 900, height = 600, units = "px")
  
  #compare Real Interest Rate
  comp_interest_rate <- ggplot() +
    geom_line(data = Barkai, aes(x = year, y = GS10 - expected_capital_inflation, color = "Barkai"), size = 0.6) + 
    geom_line(data = master %>% filter(year < 2020), aes(x = year, y = GS10 - nu), color = "blue", size = 0.6) + 
    labs(x = "", y = "Real Interest Rate", color = "") + 
    theme(legend.position = c(0.75, 0.9)) + 
    scale_color_manual(values = c("red")) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  ggsave("comp_interest_rate.jpeg", comp_interest_rate, width = 900, height = 600, units = "px")
  
  
  ## Coefficient Plots
  coeff_1b <- numeric(13)
  coeff_2b <- numeric(13)
  coeff_3b <- numeric(13)
  coeff_3d <- numeric(13)
  position <-  1
  
  for (i in seq(1978, 1990, 1)){
    coeff_1b[position] <- lm(data = master %>% filter(year %in% i:2014), formula = (GS10 - nu) ~ year)$coeff[2]
    coeff_2b[position] <- lm(data = master %>% filter(year %in% i:2014), formula = R_c ~ year)$coeff[2]
    coeff_3b[position] <- lm(data = master %>% filter(year %in% i:2014), formula = profit_rate ~ year)$coeff[2]
    coeff_3d[position] <- lm(data = master %>% filter(year %in% i:2014), formula = 2/ (2 - profit_rate) ~ year)$coeff[2]
    position = position + 1
  }
  
  start_date <- data.frame(year = seq(1978, 1990, 1), coeff_1b, coeff_2b, coeff_3b, coeff_3d)
  
  figure1a <- ggplot(start_date, aes(x = year, y = (coeff_1b[7] - coeff_1b)  / coeff_1b[5] * 100)) + 
    geom_point(color = "red", size = 0.7) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size = 0.6) +
    labs(x = "Start Year", color = "", y = "Real Interest Rate (% Change)") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
  ggsave("figure1a.jpeg", figure1a, width = 900, height = 600, units = "px")
  
  figure2b <- ggplot(start_date, aes(x = year, y = (coeff_2b[7] - coeff_2b)  / coeff_2b[5] * 100)) + 
    geom_point(color = "red", size = 0.7) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size = 0.6) +
    labs(x = "Start Year", color = "", y = "Cost of Capital Coefficient (% Change)") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
  ggsave("figure2b.jpeg", figure2b, width = 900, height = 600, units = "px")
  
  figure3b <- ggplot(start_date, aes(x = year, y = (coeff_3b[7] - coeff_3b)  / coeff_3b[5] * 100)) + 
    geom_point(color = "red", size = 0.7) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size = 0.6) +
    labs(x = "Start Year", color = "", y = "Profit Share Coefficient (% Change)") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
  ggsave("figure3b.jpeg", figure3b, width = 900, height = 600, units = "px")
  
  figure3d <- ggplot(start_date, aes(x = year, y = (coeff_3d[7] - coeff_3d)  / coeff_3d[5] * 100)) + 
    geom_point(color = "red", size = 0.7) +
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', se = F, color = "red", size = 0.6) +
    labs(x = "Start Year", color = "", y = "Markup Coefficient (% Change)") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
  ggsave("figure3d.jpeg", figure3d, width = 900, height = 600, units = "px")
  
  
}

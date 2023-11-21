compare_logits <- function(sc_ip, season1, season2){
  require(dplyr)
  require(ggplot2)
  require(stringr)
  require(lubridate)
  sc_ip %>% 
    mutate(Season = year(game_date),
           HR = ifelse(events == "home_run", 1, 0)) %>% 
    filter(Season %in% c(season1, season2))-> sc_ip2
  
  LS_breaks <- seq(95, 110, by = 5)
  LA_breaks <- seq(20, 40, by = 5)
  
  sc_ip2 %>% 
    mutate(LS = cut(launch_speed, breaks = LS_breaks),
           LA = cut(launch_angle, breaks = LA_breaks)) ->
    sc_ip2
  
  sc_ip2 %>% 
    group_by(Season) %>% 
    summarize(Total_BIP = n()) -> ST
  
  sc_ip2 %>% 
    filter(is.na(LA) == FALSE,
           is.na(LS) == FALSE) %>% 
    group_by(Season, LA, LS) %>% 
    summarize(BIP = n(),
              HR = sum(HR),
              .groups = "drop") %>% 
    inner_join(ST, by = "Season") %>% 
    mutate(BIP_Rate = 100 * BIP / Total_BIP,
           HR_Rate = 100 * HR / BIP,
           L_BIP_Rate = log(BIP_Rate) - log(100 - BIP_Rate),
           L_HR_Rate = log(HR_Rate) - log(100 - HR_Rate)) -> S
  
  S1 <- filter(S, Season == season1)
  S2 <- filter(S, Season == season2)
  inner_join(S1, S2, by = c("LA", "LS")) -> S12
  compute_mid <- function(y){
    y1 <- gsub("[,(]", " ", y)
    y2 <- gsub("[][]", "", y1)
    y3 <- gsub("^ ", "", y2)
    mean(as.numeric(str_split(y3, " ")[[1]]))
  }
  S12$la <- sapply(S12$LA, compute_mid)
  S12$ls <- sapply(S12$LS, compute_mid)
  
  BIP_Rate_plot <- ggplot(S12, aes(la, ls, 
                                   label = round(L_BIP_Rate.y - L_BIP_Rate.x, 2))) + 
    geom_text(size = 8, 
              aes(color = L_BIP_Rate.y - L_BIP_Rate.x > 0)) +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    theme(text=element_text(size=18)) +
    labs(y = "Exit Velocity",
         x = "Launch Angle") +
    guides(color = "none") +
    ggtitle(paste("Logit BIP Rate(", season2, ') minus ',
                  "Logit BIP Rate(", season1, ")", sep = "")) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  
  HR_Rate_plot <- ggplot(S12, aes(la, ls, 
                                  label = round(L_HR_Rate.y - L_HR_Rate.x, 2))) + 
    geom_text(size = 8, 
              aes(color = L_HR_Rate.y - L_HR_Rate.x > 0)) +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    theme(text=element_text(size=18)) +
    labs(y = "Exit Velocity",
         x = "Launch Angle") +
    guides(color = "none") +
    ggtitle(paste("Logit HR Rate(", season2, ') minus ',
                  "Logit HR Rate(", season1, ")", sep = "")) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  
  list(BIP_Rate_plot = BIP_Rate_plot,
       HR_Rate_plot = HR_Rate_plot) 
}
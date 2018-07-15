# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)
library(zoo)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 14, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

theme_blog <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text = element_text(size = 12, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 11)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

theme_side <- theme_economist() + 
  theme(plot.title = element_text(size = 22, face = "bold")) +
  theme(plot.subtitle = element_text(size = 14)) +
  theme(axis.text = element_text(size = 14, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 11)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2)) + 
  theme(legend.position = "bottom", legend.text = element_text(size=14), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) 

theme_full <- theme_economist() + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.subtitle = element_text(size = 8)) +
  theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 7)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2)) + 
  theme(legend.position = "bottom", legend.text = element_text(size=8), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) 

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Dropbox/01a. Resources/R/gas/" # parent directory for the data

p <- read_csv(paste0(d,"data/gas_prices.csv"), skip = 0)

vic_prod <- read_csv(paste0(d,"data/vic_prod.csv"), skip = 0)

gas_flows <- read_csv(paste0(d,"data/gas_flows.csv"), skip = 1)

# INPUTS ------

asia <- c("NE Asian", "Japan import", "Japan spot", "Singapore sling")

aust <- c("Wallumbilla", "Gladstone", "Adelaide", "Brisbane", "Sydney", "Victoria")

aust_dom <- c("Adelaide", "Brisbane", "Sydney", "Victoria")

# TIDY ----

p$Month <- as.Date(p$Month, "%d/%m/%Y")

p[, 2:15] <- lapply(p[2:15], as.numeric)

p_db <- gather(p, key = "index", value = "price", 2:15)

# TRANSORM ---- 

p$ns_gap <- p$Victoria - p$Brisbane

p$ba_gap <- p$Adelaide - p$Brisbane

p$bs_gap <- p$Sydney - p$Brisbane

p$tv_gap <- p$Victoria - p$`NE Asian`

f_roll <- function(x) {
  rollmean(p[[x]], 12, align = "right")
}

roll <- map(2:19, f_roll)

names(roll) <- colnames(p[,2:19])

roll <- as.data.frame(roll)

roll <- cbind(p[12:94,1], roll)

roll <- as.tibble(roll)

roll_db <- gather(roll, key = "index", value = "price", 2:19)

f_roll3 <- function(x) {
  rollmean(p[[x]], 3, align = "right")
}

roll3 <- map(2:19, f_roll3)

names(roll3) <- colnames(p[,2:19])

roll3 <- as.data.frame(roll3)

roll3 <- cbind(p[3:94,1], roll3)

roll3 <- as.tibble(roll3)

roll3_db <- gather(roll3, key = "index", value = "price", 2:19)

# gas flows

net_flow <- gas_flows %>% 
  select(c("Date", contains("Net Flows"))) %>% 
  rename(APLNG = `APLNG Net Flows`,
         QCLNG = `QCLNG Net Flows`,
         GLNG = `GLNG Net Flows`
  )

net_flow$net <- net_flow$APLNG + net_flow$QCLNG + net_flow$GLNG

net_flow$Date <- as.Date(net_flow$Date, "%d/%m/%y")

net_flow_12 <- net_flow %>% 
  mutate(rollsum = rollsum(net, 365, align = "right", fill = NA) / 1000) %>% 
  mutate(a_12 = rollsum(APLNG, 365, align = "right", fill = NA) / 1000) %>% 
  mutate(q_12 = rollsum(QCLNG, 365, align = "right", fill = NA) / 1000) %>% 
  mutate(g_12 = rollsum(GLNG, 365, align = "right", fill = NA) / 1000) 

net_flow_g <- gather(net_flow[,1:4], key = "jv", value = "net_flow", 2:4)

net_flow_12_g <- gather(net_flow_12[,c(1, 7:9)], key = "jv", value = "net_flow", 2:4)

# PLOTS ----

# all price indices

f_p <- function(ndex) { 
  p_db %>% 
    filter(index == ndex & !is.na(price)) %>% 
    ggplot(aes(x = Month, y = price)) + 
    geom_line(color = "#2166ac", size = stroke_size) +
    theme_mc + 
    labs(title = ndex, subtitle = "$ per GJ, average monthly", x ="", y = "") 
}

p_p <- map(unique(p_db$index), f_p)

names(p_p) <- unique(p_db$index)
  
# all price indices 3 ma

f_p3 <- function(ndex) { 
  
  labs1  <- roll3_db %>% 
  filter(index == ndex & !is.na(price) & Month > "2015-01-01") %>% 
  tail(1)
  
  labs2 <- roll3_db %>% 
    filter(index == ndex & !is.na(price) & Month > "2015-01-01") %>% 
    filter(price == max(price))
  
  ymax <- labs2$price + 1
  
  labs <- rbind(labs1, labs2)
  
  roll3_db %>% 
    filter(index == ndex & !is.na(price) & Month > "2015-01-01") %>% 
    ggplot(aes(x = Month, y = price)) + 
    geom_line(color = "#2166ac", size = stroke_size) +
    theme_mc + 
    labs(title = ndex, subtitle = "$ per GJ, quarterly moving average", x ="", y = "") +
    geom_text(data = labs, aes(x = Month, y = price, label = round(price,2), vjust = ifelse(price > 9, -1, 2)), hjust = 1, size = 4) + 
    geom_point(data = labs, color = line_color, size = 2.5) +
    ylim(0, ymax)
    
}

p_p3 <- map(unique(roll3_db$index), f_p3)

names(p_p3) <- unique(roll3_db$index)

# international comparisons

f_p3_inter <- function(ndex) { 
  
  labs1  <- roll3_db %>% 
    filter(index == ndex & !is.na(price) & Month > "2015-01-01") %>% 
    tail(1)
  
  labs2 <- roll3_db %>% 
    filter(index == ndex & !is.na(price) & Month > "2015-01-01") %>% 
    filter(price == max(price))
  
  ymax <- labs2$price + 1
  
  labs <- rbind(labs1)
  
  roll3_db %>% 
    filter(index == ndex & !is.na(price) & Month > "2015-01-01") %>% 
    ggplot(aes(x = Month, y = price)) + 
    geom_line(color = "#2166ac", size = stroke_size) +
    theme_mc + 
    labs(title = ndex, subtitle = "$ per GJ, quarterly moving average", x ="", y = "") +
    geom_text(data = labs, aes(x = Month, y = price, label = round(price,2)), vjust = 2, hjust = 0.6, size = 3) + 
    geom_point(data = labs, color = line_color, size = 2.5) +
    ylim(0, ymax)
  
}

p_p3_inter <- map(unique(roll3_db$index), f_p3_inter)

names(p_p3_inter) <- unique(roll3_db$index)

# adgsm line

adgsm_date <- as.Date("2017-03-15")

p_p3$Wallumbilla <- p_p3$Wallumbilla + geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) + annotate(geom = "text", label = "1st meeting", x = adgsm_date + 90, y = 2.5, size = 4 )

p_p3$Adelaide <- p_p3$Adelaide + geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) + annotate(geom = "text", label = "1st meeting", x = adgsm_date + 90, y = 2.5, size = 4 )

p_p3$Sydney <- p_p3$Sydney + geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) + annotate(geom = "text", label = "1st meeting", x = adgsm_date + 90, y = 2.5, size = 4 )

p_p3$Brisbane <- p_p3$Brisbane + geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) + annotate(geom = "text", label = "1st meeting", x = adgsm_date + 90, y = 2.5, size = 4 )

p_p3$Victoria <- p_p3$Victoria + geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) + annotate(geom = "text", label = "1st meeting", x = adgsm_date + 90, y = 2.5, size = 4 )



# aus v asia 
  
  f_aus3 <- function(ndex) { 
    
    labs1  <- roll3_db %>% 
      filter(index %in% c(ndex) & !is.na(price) & Month > "2015-01-01") %>% 
      tail(1)
    
    labs2  <- roll3_db %>% 
      filter(index %in% c("NE.Asian") & !is.na(price) & Month > "2015-01-01") %>% 
      tail(1)
    
    labs3 <- roll3_db %>% 
      filter(index == ndex & !is.na(price) & Month > "2016-10-01") %>% 
      filter(price == max(price))
    
    labs <- rbind(labs1, labs2, labs3)
    
    s_col <- c("#2166ac", NE.Asian = "grey")
    
    roll3_db %>% 
      filter(index %in% c(ndex, "NE.Asian") & !is.na(price) & Month > "2015-01-01") %>% 
      ggplot(aes(x = Month, y = price)) + 
      geom_line(aes(color = index), size = stroke_size) +
      theme_mc + 
      theme(legend.position = "bottom", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) +
      scale_color_manual(values = c("grey", "#2166ac")) +
      labs(title = ndex, subtitle = "$ per GJ, quarterly moving average", x ="", y = "") +
      geom_text(data = labs, aes(x = Month, y = price, label = round(price,2)), vjust = -1, hjust = 1, size = 5) + 
      geom_point(data = labs, color = line_color, size = 2.5)
    
  }
  
p_aust <- map(aust, f_aus3)

names(p_aust) <- unique(aust)

# gladstone exports

p_glad_roll <- p_db %>% 
  filter(index == "Gladstone exports" & !is.na(price)) %>% 
  mutate(rollsum = rollsum(price, 12, align = "right", fill = NA)) %>% 
  select(Month, rollsum) 

p_glad_lab <- tail(p_glad_roll, 1)

p_glad <- p_glad_roll %>% 
  filter(!is.na(rollsum)) %>% 
  ggplot(aes(x = Month, y = rollsum)) + 
  geom_line(color = "#2166ac", size = stroke_size) + 
  theme_mc + 
  labs(title = "Gladstone exports", subtitle = "million tonnes", x ="", y = "") +    geom_text(data = p_glad_lab, aes(x = Month, y = rollsum, label = round(rollsum,2)), vjust = -1, size = 3) + 
  geom_point(data = p_glad_lab, color = line_color, size = 2.5) +
  ylim(0,22)



# north - south difference

gap_list <- c("ns_gap", "ba_gap", "bs_gap", "tv_gap")

f_gap <- function(x) {
  roll_db %>% 
  filter(index == x & !is.na(price)) %>% 
  ggplot(aes(x = Month, y = price)) + 
  geom_line(color = "#2166ac", size = stroke_size) + 
  theme_mc + 
  labs(title = "", subtitle = "$ per GJ, 12 month moving average", x ="", y = "") 
}

p_gap <- map(gap_list, f_gap)

p_gap$vic_bris <- p_gap[[1]] + labs(title = "Victoria price less Brisbane price") 

p_gap$adl_bris <- p_gap[[2]] + labs(title = "Adelaide - Brisbane") 

p_gap$syd_bris <- p_gap[[3]] + labs(title = "Sydney - Brisbane") 

p_gap$vic_asia <- p_gap[[4]] + labs(title = "Victoria price less NE Asia price") 

# victorian production 

p_vic <- vic_prod %>% 
  ggplot(aes(x = year, y = prod))  + 
  geom_bar(stat="identity", fill = "#2166ac") + 
  theme_mc +
  labs(title = "Victorian offshore production", subtitle = "PJ / year", caption = "AEMO, only includes 2P developed reserves") +
  geom_text(aes(label = paste(round(prod,0)), vjust=-1), size=3) +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank()) +
  ylim(0,500)

# net flows

p_net <- net_flow %>% 
  filter(Date > "2015-06-30") %>% 
  ggplot(aes(x = Date, y = net)) + 
  geom_line(color = "#2166ac", size = stroke_size) +
  theme_mc + 
  labs(title = "Net domestic gas from Queensland CSG", subtitle = "TJ / day", x ="", y = "") 

p_net_12 <- net_flow_12 %>% 
  filter(Date > "2016-06-30") %>% 
  ggplot(aes(x = Date, y = rollsum)) + 
  geom_line(color = "#2166ac", size = stroke_size) +
  theme_mc + 
  labs(title = "Net domestic gas from Queensland CSG", subtitle = "PJ / year, 12 month rolling sum", x ="", y = "") +
  geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) +
annotate(geom = "text", label = "1st meeting", x = adgsm_date + 50, y = 110, size = 4 ) + 
  ylim(0, 120)

p_net_jv  <- net_flow_g %>% 
  filter(Date > "2015-06-30") %>% 
  ggplot(aes(x = Date, y = net_flow, color = jv)) + 
  geom_line(size = stroke_size) +
  theme_mc + 
  labs(title = "Net domestic gas from Queensland CSG", subtitle = "TJ / day", x ="", y = "") +
  theme(legend.position = "right", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank()) +
  scale_color_manual(values = c(APLNG = "#ad172b", QCLNG = "#fbce07", GLNG = "#0088ce"), labels = c("APLNG", "GLNG", "QCLNG"))

p_net_jv_12 <- net_flow_12_g %>% 
  filter(Date > "2016-06-30") %>% 
  ggplot(aes(x = Date, y = net_flow, color = jv)) + 
  geom_line(size = stroke_size) +
  theme_mc + 
  labs(title = "Net domestic gas from Queensland CSG", subtitle = "PJ / year, 12 month rolling sum", x ="", y = "") +
  geom_vline(xintercept = adgsm_date, color = "red", linetype = 2) +
  annotate(geom = "text", label = "1st meeting", x = adgsm_date + 70, y = 160, size = 3 ) +
  theme(legend.position = "right", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank()) +
  scale_color_manual(values = c(a_12 = "#ad172b", q_12 = "#fbce07", g_12 = "#0088ce"), labels = c("APLNG", "GLNG", "QCLNG"))


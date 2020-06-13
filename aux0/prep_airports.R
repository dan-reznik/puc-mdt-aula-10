library(tidyverse)
library(tidygraph)
library(igraph)
library(geosphere)

# Flights / map geosphere
df_airports <- read_csv("data_sb/Dataset3-Airlines-NODES.csv")
df_flights <- read_csv("data_sb/Dataset3-Airlines-EDGES.csv")

# Selecionar apenas os aeroportos que possuem mais de 10 conexões no mapa.
big_id <- df_flights %>%
  count(Source, sort = T) %>%
  filter(n > 10) %>%
  pull(Source)

df_airports <- df_airports %>%
  filter(df_airports$ID %in% big_id)

df_flights  <- df_flights %>%
  filter(df_flights$Source %in% big_id & 
           df_flights$Target %in% big_id)

#Criando variaveis group e centrality a partir do grafo do data frame
graph_flights_grouped <- df_flights %>%
  select(from = Source, to = Target) %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(group = group_fast_greedy() %>% as_factor,
         centrality = centrality_betweenness(),
         degree = tidygraph::centrality_degree())

df_airports_grouped <- df_airports %>%
  add_column(group = V(graph_flights_grouped)$group, centrality = V(graph_flights_grouped)$centrality %>% unname())

df_flights_grouped <- df_flights %>%
  left_join(df_airports_grouped %>% select(ID, group), by = c("Source" = "ID")) %>%
  rename(group_id_1 = group) %>%
  left_join(df_airports_grouped %>% select(ID, group), by = c("Target" = "ID")) %>%
  rename(group_id_2 = group)

##criando curved lines

curved_lines <- list()
curved_lines_color_ind <- c()
for(i in 1:nrow(df_flights_grouped))  {
  #Origem
  node1 <- df_airports_grouped %>% 
    filter(df_airports_grouped$ID == df_flights_grouped[i,]$Source)
  
  #destino
  node2 <- df_airports_grouped %>%
    filter(df_airports_grouped$ID == df_flights_grouped[i,]$Target)
  
  #numero da cor na paleta
  curved_lines_color_ind[i] = round(100*df_flights_grouped[i,]$Freq / max(df_flights_grouped$Freq))
  
  # Cria linhas curvas
  curved_lines[i] <-
    gcIntermediate(
      p1 = c(node1$longitude, node1$latitude)
      , p2 = c(node2$longitude, node2$latitude)
      , breakAtDateLine = TRUE
      , n = 12
      , addStartEnd = TRUE
      , sp = TRUE
    )
}
df_flights_grouped <- df_flights_grouped %>%
  mutate(curved_line = curved_lines,
         curved_line_color_ind = curved_lines_color_ind)

df_flights_grouped %>%
  write_rds("data_sb/df_flights_grouped.rds")

df_airports_grouped %>%
  write_rds("data_sb/df_airports_grouped.rds")

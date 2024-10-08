---
title: "Gerrymandering Redistricting in R"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

This R Markdown document explains a redistricting analysis script. The script simulates redistricting attempts and analyzes their impact on party seat distribution.

## Required Libraries

```{r libraries, message=FALSE}
library(igraph)
library(ggplot2)
```

These libraries are essential for graph manipulation (`igraph`) and data visualization (`ggplot2`).

## Creating the Initial Graph

```{r create_graph}
adj_matrix <- matrix(c(0,0,1,0,0,0,1,0,0,0,0,1,0,1,0,
                       0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,
                       1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,
                       0,1,0,0,1,0,0,0,1,0,1,0,0,0,0,
                       0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,
                       0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,
                       1,1,0,0,0,0,0,0,0,0,1,0,0,1,0,
                       0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,
                       0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,
                       0,1,1,0,1,0,0,0,0,0,0,0,1,0,0,
                       0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,
                       1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,
                       0,0,0,0,1,0,0,0,1,1,0,1,0,0,0,
                       1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,
                       0,1,1,0,0,0,0,0,0,0,1,1,0,1,0), 
                     nrow = 15, byrow = TRUE)

g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")

V(g)$party <- c("A","A","B","A","B","A","B","B","A","B","B","A","A","A","B")
V(g)$seats <- rep(15, 15)
V(g)$district <- c(1, 2, 1, 3, 3, 3, 1, 3, 3, 2, 1, 2, 2, 1, 2)
V(g)$pop <- rep(10, 15)
V(g)$name <- LETTERS[1:15]
```

This code creates the initial graph structure. It defines an adjacency matrix and uses it to create an undirected graph. Each vertex (node) in the graph represents a geographical unit with properties like party affiliation, number of seats, district, and population.

## Helper Functions

### Count Cross-District Edges

```{r count_cross_district_edges}
count_cross_district_edges <- function(g, node) {
  neighbors <- neighbors(g, node)
  return(sum(V(g)$district[neighbors] != V(g)$district[node]))
}
```

This function counts the number of edges that connect a given node to nodes in different districts.

### Calculate Party Seats

```{r calculate_party_seats}
calculate_party_seats <- function(g) {
  districts <- unique(V(g)$district)
  party_A_seats <- party_B_seats <- 0
  
  for (d in districts) {
    district_nodes <- V(g)[V(g)$district == d]
    party_A_votes <- sum(district_nodes$seats[district_nodes$party == "A"])
    party_B_votes <- sum(district_nodes$seats[district_nodes$party == "B"])
    district_seats <- sum(district_nodes$seats)
    
    if (party_A_votes > party_B_votes) {
      party_A_seats <- party_A_seats + district_seats
    } else if (party_B_votes > party_A_votes) {
      party_B_seats <- party_B_seats + district_seats
    } else {
      # In case of a tie, split the seats
      party_A_seats <- party_A_seats + district_seats / 2
      party_B_seats <- party_B_seats + district_seats / 2
    }
  }
  
  return(c(party_A_seats, party_B_seats))
}
```

This function calculates the number of seats each party would win based on the current district configuration.

### Attempt District Change

```{r attempt_district_change}
attempt_district_change <- function(g, q) {
  while (TRUE) {
    node <- sample(V(g), 1)
    current_district <- V(g)$district[node]
    neighbor_districts <- unique(V(g)$district[neighbors(g, node)])
    neighbor_districts <- neighbor_districts[neighbor_districts != current_district]
    
    if (length(neighbor_districts) > 0) {
      # It's a boundary node, proceed with probability check
      if (runif(1) < q) {
        proposed_district <- sample(neighbor_districts, 1)
        
        # Check if removing the node would disconnect its current district
        current_district_nodes <- V(g)[V(g)$district == current_district]
        if (length(current_district_nodes) > 1) {
          test_graph <- induced_subgraph(g, current_district_nodes)
          subgraph_node <- which(V(test_graph)$name == V(g)$name[node])
          test_graph <- delete_vertices(test_graph, subgraph_node)
          if (!is_connected(test_graph)) {
            cat("Change rejected: Would disconnect district\n")
            return(g)
          }
        } else {
          cat("Change rejected: Last node in district\n")
          return(g)
        }
        
        total_population <- sum(V(g)$pop)
        num_districts <- length(unique(V(g)$district))
        avg_district_population <- total_population / num_districts
        max_allowed_population <- avg_district_population * 1.30
        
        proposed_district_population <- sum(V(g)$pop[V(g)$district == proposed_district]) + V(g)$pop[node]
        if (proposed_district_population > max_allowed_population) {
          cat("Change rejected: Would exceed 30% population threshold\n")
          return(g)
        }
        
        current_cross_edges <- count_cross_district_edges(g, node)
        old_district <- V(g)$district[node]
        V(g)$district[node] <- proposed_district
        proposed_cross_edges <- count_cross_district_edges(g, node)
        
        acceptance_prob <- (1 - q)^(proposed_cross_edges - current_cross_edges)
        
        if (runif(1) < acceptance_prob) {
          cat("Node", V(g)$name[node], "changed from District", old_district, "to District", proposed_district, "\n")
        } else {
          V(g)$district[node] <- old_district
          cat("Proposed change for Node", V(g)$name[node], "rejected\n")
        }
        
        return(g)
      } else {
        cat("No change proposed for Node", V(g)$name[node], "\n")
        return(g)
      }
    }
    # If not a boundary node, the loop continues to select another node
  }
}
```

This function attempts to change the district of a randomly selected node. It includes checks for district connectivity, population balance, and uses a probabilistic acceptance criterion.

### Visualize Graph

```{r visualize_graph}
visualize_graph <- function(g) {
  num_districts <- max(V(g)$district)
  colors <- rainbow(num_districts)
  V(g)$color <- colors[V(g)$district]
  
  plot(g, 
       layout = layout_with_fr(g),
       vertex.size = V(g)$seats,
       vertex.label = paste(V(g)$name, V(g)$party, V(g)$seats, sep="\n"),
       vertex.label.color = "black",
       vertex.label.cex = 0.7,
       edge.width = 2,
       main = paste("Adjacency Graph with", num_districts, "Districts"))
}
```

This function creates a visual representation of the graph, with nodes colored by district and sized by the number of seats.

## Main Simulation

```{r simulation, eval=FALSE}
# Visualize initial state
cat("Initial state:\n")
visualize_graph(g)

num_attempts <- 1000
q <- 0.3
seat_history <- matrix(nrow = num_attempts, ncol = 2)

for(i in 1:num_attempts) {
  g <- attempt_district_change(g, q)
  seat_history[i,] <- calculate_party_seats(g)
}

# Visualize final state
cat("\nFinal state:\n")
visualize_graph(g)
```

This section runs the main simulation. It starts by visualizing the initial state, then performs 1000 redistricting attempts, recording the seat distribution after each attempt.

## Results Visualization

```{r results_viz, eval=FALSE}
seat_history_df <- data.frame(
  Attempt = rep(1:num_attempts, 2),
  Party = rep(c("A", "B"), each = num_attempts),
  Seats = c(seat_history[,1], seat_history[,2])
)

ggplot(seat_history_df, aes(x = Seats, fill = Party)) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~Party, ncol = 1) +
  theme_minimal() +
  labs(title = "Distribution of Party Seats Across Redistributions",
       x = "Number of Seats", y = "Frequency")
```

Finally, this code creates a histogram showing the distribution of seat outcomes for each party across all redistricting attempts.


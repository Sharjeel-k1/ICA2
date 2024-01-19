# ICA2
# Clojure Search Engine

This Clojure program is a travel search engine that helps users find optimal travel plans between two cities based on specified constraints, such as budget and maximum number of flights. The program utilizes a graph data structure to represent cities and their connections through edges.

## Usage

To use the travel search engine, follow these steps:

1. **Define the Graph:**
   - The program uses a graph structure with vertices and edges to represent cities and connections between them. You can manually insert vertices and edges by updating the `doseq` block at the end of the program.

2. **Set Constraints:**
   - Adjust constraints such as the budget, maximum number of flights, and customer details in the `prepare-travel-plan` function call at the end of the program.

3. **Run the Program:**
   - Execute the program using a Clojure environment. The result will display the optimal travel plan based on the specified constraints.

## Functions and Components

- **Graph Structure:** Vertices and edges are represented using Clojure records (`Vertex` and `Edge`). The graph itself is defined as a record named `Graph`.

- **Graph Manipulation:** Functions like `graph-add-vertex!` and `graph-add-edge!` are provided to add vertices and edges to the graph.

- **Breadth-First Search (BFS):** The `bfs-find-plans` function performs BFS to find travel plans satisfying the given constraints.

- **Budget Prediction:** The `get-predicted-budget` function estimates the budget based on historical data and customer type.

- **Customer Classification:** The `people-classification` function classifies customers based on specific conditions.

- **Profit Calculation:** The program calculates and displays the total profit earned from selling tickets.

## Example

An example travel plan is provided at the end of the program. Adjust the departure city, destination city, and customer details to explore different scenarios.

```clojure
(prepare-travel-plan  "Berlin"
                     "Napoli"
                     [["Harry Adams", 1991]
                      ["Elsie Adams", 1976]
                      ["Alfie Adams", 2017]
                      ["Elsie Adams", 2016]]
                     true)
```

## Important Note

Ensure that you have the necessary data files, such as the historical data file ("sales_team_5.csv"), in the specified path before running the program.

Feel free to explore and customize the program to meet your specific travel search requirements.

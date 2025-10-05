# api.R - VRP Plumber API
# Install required packages first:
# install.packages(c("plumber", "jsonlite", "TSP"))

library(plumber)
library(jsonlite)
library(TSP)

#* Enable CORS (allows your website to call this API)
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* Solve VRP Problem
#* @param locations:list List of location objects with lat, lng, name
#* @param numVehicles:int Number of vehicles
#* @param depotIndex:int Index of depot location
#* @post /solve-vrp
function(locations, numVehicles, depotIndex = 0) {
  
  # Parse input
  locs <- fromJSON(locations)
  num_vehicles <- as.integer(numVehicles)
  depot_idx <- as.integer(depotIndex) + 1  # R uses 1-based indexing
  
  # Calculate distance matrix using Haversine formula
  calc_distance <- function(lat1, lon1, lat2, lon2) {
    R <- 6371  # Earth radius in km
    lat1_rad <- lat1 * pi / 180
    lat2_rad <- lat2 * pi / 180
    delta_lat <- (lat2 - lat1) * pi / 180
    delta_lon <- (lon2 - lon1) * pi / 180
    
    a <- sin(delta_lat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(delta_lon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    d <- R * c
    
    return(d)
  }
  
  # Build distance matrix
  n <- nrow(locs)
  dist_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist_matrix[i, j] <- calc_distance(
          locs$lat[i], locs$lng[i],
          locs$lat[j], locs$lng[j]
        )
      }
    }
  }
  
  # Solve VRP using nearest neighbor heuristic
  solve_vrp <- function(dist_mat, depot, n_vehicles) {
    unvisited <- setdiff(1:nrow(dist_mat), depot)
    routes <- list()
    
    for (v in 1:n_vehicles) {
      if (length(unvisited) == 0) break
      
      route <- c(depot)
      current <- depot
      
      # Distribute customers evenly
      customers_per_vehicle <- ceiling(length(unvisited) / (n_vehicles - v + 1))
      
      for (i in 1:customers_per_vehicle) {
        if (length(unvisited) == 0) break
        
        # Find nearest unvisited customer
        distances <- dist_mat[current, unvisited]
        nearest_idx <- which.min(distances)
        nearest <- unvisited[nearest_idx]
        
        route <- c(route, nearest)
        current <- nearest
        unvisited <- unvisited[-nearest_idx]
      }
      
      route <- c(route, depot)  # Return to depot
      
      # Calculate route distance
      route_dist <- 0
      for (i in 1:(length(route)-1)) {
        route_dist <- route_dist + dist_mat[route[i], route[i+1]]
      }
      
      routes[[v]] <- list(
        vehicle = v,
        stops = route - 1,  # Convert back to 0-based indexing for JavaScript
        distance = round(route_dist, 2)
      )
    }
    
    return(routes)
  }
  
  # Solve and return results
  routes <- solve_vrp(dist_matrix, depot_idx, num_vehicles)
  total_distance <- sum(sapply(routes, function(r) r$distance))
  
  result <- list(
    routes = routes,
    totalDistance = round(total_distance, 2),
    status = "success"
  )
  
  return(result)
}

#* Health check endpoint
#* @get /health
function() {
  list(status = "ok", message = "VRP API is running")
}

#* Get API info
#* @get /
function() {
  list(
    name = "VRP Solver API",
    version = "1.0",
    endpoints = list(
      "/health" = "Health check",
      "/solve-vrp" = "Solve VRP problem (POST)"
    )
  )
}

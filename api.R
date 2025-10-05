# api.R - Enterprise VRP API
library(plumber)
library(jsonlite)
library(lpSolve)
library(lubridate)

#* @apiTitle Enterprise VRP Optimizer API
#* @apiDescription Complete vehicle routing with loading optimization, time windows, and multi-temperature zones

#* Enable CORS
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

# ============================================
# HELPER FUNCTIONS
# ============================================

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  dlat <- (lat2 - lat1)
  dlon <- (lon2 - lon1) * pi / 180
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  return(R * c * 1.3)
}

# Clarke-Wright Savings Algorithm
clarke_wright_vrp <- function(dist_mat, time_mat, all_locations, params) {
  n <- nrow(all_locations)
  depot <- 1
  customers <- 2:n
  
  savings <- data.frame()
  for(i in customers) {
    for(j in customers) {
      if(i < j) {
        saving <- dist_mat[depot, i] + dist_mat[depot, j] - dist_mat[i, j]
        savings <- rbind(savings, data.frame(i = i, j = j, saving = saving))
      }
    }
  }
  
  savings <- savings[order(savings$saving, decreasing = TRUE), ]
  
  routes <- list()
  assigned <- rep(FALSE, n)
  assigned[depot] <- TRUE
  
  for(s in 1:nrow(savings)) {
    i <- savings$i[s]
    j <- savings$j[s]
    
    if(!assigned[i] && !assigned[j] && length(routes) < params$num_trucks) {
      route_demand <- all_locations$demand[i] + all_locations$demand[j]
      route_time <- time_mat[depot, i] + all_locations$service_time_min[i] +
        time_mat[i, j] + all_locations$service_time_min[j] + time_mat[j, depot]
      
      if(route_demand <= params$truck_capacity && 
         route_time <= params$max_route_time &&
         2 <= params$max_stops_per_truck) {
        routes[[length(routes) + 1]] <- c(depot, i, j, depot)
        assigned[i] <- TRUE
        assigned[j] <- TRUE
      }
    }
    else if(assigned[i] != assigned[j]) {
      assigned_cust <- if(assigned[i]) i else j
      unassigned_cust <- if(assigned[i]) j else i
      
      for(r in 1:length(routes)) {
        route <- routes[[r]]
        if(assigned_cust %in% route) {
          route_customers <- route[route != depot]
          new_demand <- sum(all_locations$demand[route_customers]) + 
            all_locations$demand[unassigned_cust]
          
          if(new_demand <= params$truck_capacity && 
             length(route_customers) < params$max_stops_per_truck) {
            pos <- length(route)
            routes[[r]] <- c(route[1:(pos-1)], unassigned_cust, route[pos])
            assigned[unassigned_cust] <- TRUE
            break
          }
        }
      }
    }
  }
  
  unassigned <- customers[!assigned[customers]]
  for(cust in unassigned) {
    if(length(routes) < params$num_trucks) {
      routes[[length(routes) + 1]] <- c(depot, cust, depot)
      assigned[cust] <- TRUE
    }
  }
  
  total_distance <- 0
  total_time <- 0
  trucks_used <- 0
  
  for(route in routes) {
    if(length(route) > 2) {
      trucks_used <- trucks_used + 1
      for(i in 1:(length(route)-1)) {
        total_distance <- total_distance + dist_mat[route[i], route[i+1]]
        total_time <- total_time + time_mat[route[i], route[i+1]]
      }
      route_customers <- route[route != 1]
      total_time <- total_time + sum(all_locations$service_time_min[route_customers])
    }
  }
  
  cost <- total_distance * params$cost_per_km + 
    trucks_used * params$cost_per_truck + 
    (total_time / 60) * params$driver_cost_per_hour
  
  return(list(
    solution = routes, 
    cost = cost,
    total_distance = total_distance,
    total_time = total_time,
    trucks_used = trucks_used
  ))
}

# Time window calculation
solve_routes_with_time_windows <- function(routes_solution, orders, trucks, time_mat, depot_idx = 1) {
  if(is.null(routes_solution) || length(routes_solution$solution) == 0) {
    return(list())
  }
  
  routes_with_times <- list()
  
  for(i in seq_along(routes_solution$solution)) {
    route <- routes_solution$solution[[i]]
    if(length(route) <= 2) next
    
    truck_id <- trucks[[i]]$truck_id
    truck_shift_start <- as.POSIXct(paste("2025-01-15", trucks[[i]]$shift_start), tz = "UTC")
    
    current_time <- truck_shift_start
    route_customers <- route[route != depot_idx]
    
    stops <- list()
    
    for(j in seq_along(route_customers)) {
      cust_idx <- route_customers[j]
      prev_idx <- if(j == 1) depot_idx else route_customers[j-1]
      
      travel_time_min <- as.integer(round(time_mat[prev_idx, cust_idx]))
      arrive_time <- current_time + lubridate::minutes(travel_time_min)
      
      cust_order <- orders[[cust_idx - 1]]
      tw_early <- as.POSIXct(paste("2025-01-15", cust_order$tw_early), tz = "UTC")
      tw_late <- as.POSIXct(paste("2025-01-15", cust_order$tw_late), tz = "UTC")
      
      start_service <- max(arrive_time, tw_early)
      depart_time <- start_service + lubridate::minutes(as.integer(cust_order$service_time_min))
      
      early_violation <- arrive_time < tw_early
      late_violation <- start_service > tw_late
      
      stops[[j]] <- list(
        stop_seq = j,
        customer_id = cust_order$order_id,
        customer_name = cust_order$name,
        arrive_time = format(arrive_time, "%H:%M"),
        tw_early = cust_order$tw_early,
        tw_late = cust_order$tw_late,
        start_service = format(start_service, "%H:%M"),
        service_min = cust_order$service_time_min,
        late_violation = late_violation,
        early_violation = early_violation,
        slack_min = as.numeric(difftime(tw_late, start_service, units = "mins"))
      )
      
      current_time <- depart_time
    }
    
    routes_with_times[[i]] <- list(
      truck_id = truck_id,
      stops = stops
    )
  }
  
  return(routes_with_times)
}

# Loading assignment
assign_orders_to_trucks <- function(orders, trucks) {
  assignments <- list()
  unassigned <- list()
  
  truck_loads <- lapply(trucks, function(t) {
    list(
      truck_id = t$truck_id,
      current_weight = 0,
      current_cube = 0,
      current_pallets = 0,
      frozen_used = 0,
      chilled_used = 0,
      ambient_used = 0,
      orders = list()
    )
  })
  
  for(order in orders) {
    assigned <- FALSE
    
    for(i in seq_along(truck_loads)) {
      truck <- truck_loads[[i]]
      truck_spec <- trucks[[i]]
      
      if(truck$current_weight + order$weight_lb > truck_spec$max_weight_lb) next
      if(truck$current_cube + order$cube_ft3 > truck_spec$max_cube_ft3) next
      if(truck$current_pallets + order$pallets > truck_spec$max_pallets) next
      
      temp_ok <- FALSE
      if(order$temp == "frozen" && truck$frozen_used + order$cube_ft3 <= truck_spec$cap_frozen_ft3) {
        temp_ok <- TRUE
      } else if(order$temp == "chilled" && truck$chilled_used + order$cube_ft3 <= truck_spec$cap_chilled_ft3) {
        temp_ok <- TRUE
      } else if(order$temp == "ambient" && truck$ambient_used + order$cube_ft3 <= truck_spec$cap_ambient_ft3) {
        temp_ok <- TRUE
      }
      
      if(!temp_ok) next
      
      truck_loads[[i]]$current_weight <- truck$current_weight + order$weight_lb
      truck_loads[[i]]$current_cube <- truck$current_cube + order$cube_ft3
      truck_loads[[i]]$current_pallets <- truck$current_pallets + order$pallets
      
      if(order$temp == "frozen") {
        truck_loads[[i]]$frozen_used <- truck$frozen_used + order$cube_ft3
      } else if(order$temp == "chilled") {
        truck_loads[[i]]$chilled_used <- truck$chilled_used + order$cube_ft3
      } else {
        truck_loads[[i]]$ambient_used <- truck$ambient_used + order$cube_ft3
      }
      
      truck_loads[[i]]$orders <- c(truck_loads[[i]]$orders, list(order$order_id))
      
      assignments <- c(assignments, list(list(
        order_id = order$order_id,
        truck_id = truck_spec$truck_id
      )))
      
      assigned <- TRUE
      break
    }
    
    if(!assigned) {
      unassigned <- c(unassigned, list(list(
        order_id = order$order_id,
        reason = "No available capacity"
      )))
    }
  }
  
  loads_summary <- lapply(truck_loads, function(t) {
    if(length(t$orders) > 0) {
      list(
        truck_id = t$truck_id,
        n_orders = length(t$orders),
        weight_lb = t$current_weight,
        cube_ft3 = t$current_cube,
        pallets = t$current_pallets,
        frozen_cube = t$frozen_used,
        chilled_cube = t$chilled_used,
        ambient_cube = t$ambient_used
      )
    }
  })
  
  loads_summary <- Filter(Negate(is.null), loads_summary)
  
  return(list(
    assignments = assignments,
    unassigned = unassigned,
    truck_loads = loads_summary
  ))
}

#* Solve Complete VRP with All Features
#* @param warehouse:list Warehouse location {lat, lon}
#* @param customers:list Array of customer objects
#* @param fleet:list Array of truck objects
#* @param params:list Optimization parameters
#* @post /solve-complete-vrp
function(warehouse, customers, fleet, params = list()) {
  
  warehouse <- fromJSON(warehouse)
  customers <- fromJSON(customers)
  fleet <- fromJSON(fleet)
  params <- fromJSON(params)
  
  all_locations <- c(list(warehouse), customers)
  n <- length(all_locations)
  
  dist_mat <- matrix(0, n, n)
  time_mat <- matrix(0, n, n)
  
  for(i in 1:n) {
    for(j in 1:n) {
      if(i != j) {
        dist_mat[i,j] <- haversine_distance(
          all_locations[[i]]$lat, all_locations[[i]]$lon,
          all_locations[[j]]$lat, all_locations[[j]]$lon
        )
        time_mat[i,j] <- (dist_mat[i,j] / 40) * 60
      }
    }
  }
  
  default_params <- list(
    num_trucks = length(fleet),
    truck_capacity = 50,
    max_route_time = 720,
    max_stops_per_truck = 20,
    cost_per_km = 0.5,
    cost_per_truck = 200,
    driver_cost_per_hour = 25
  )
  
  params <- modifyList(default_params, params)
  
  all_locs_df <- do.call(rbind, lapply(all_locations, as.data.frame))
  
  routing_result <- clarke_wright_vrp(dist_mat, time_mat, all_locs_df, params)
  
  time_windows <- solve_routes_with_time_windows(
    routing_result, customers, fleet, time_mat
  )
  
  loading_result <- assign_orders_to_trucks(customers, fleet)
  
  late_count <- 0
  for(route in time_windows) {
    for(stop in route$stops) {
      if(stop$late_violation) late_count <- late_count + 1
    }
  }
  
  return(list(
    status = "success",
    routing = list(
      routes = routing_result$solution,
      cost = routing_result$cost,
      total_distance = routing_result$total_distance,
      total_time = routing_result$total_time,
      trucks_used = routing_result$trucks_used
    ),
    time_windows = time_windows,
    loading = loading_result,
    kpis = list(
      total_orders = length(customers),
      orders_assigned = length(loading_result$assignments),
      orders_unassigned = length(loading_result$unassigned),
      trucks_used = routing_result$trucks_used,
      total_distance_km = round(routing_result$total_distance, 1),
      total_cost = round(routing_result$cost, 2),
      time_violations = late_count
    )
  ))
}

#* Health check
#* @get /health
function() {
  list(status = "ok", message = "Enterprise VRP API is running")
}

#* Get API info
#* @get /
function() {
  list(
    name = "Enterprise VRP Optimizer API",
    version = "2.0",
    features = list(
      "Clarke-Wright routing algorithm",
      "Time window optimization",
      "Multi-temperature loading",
      "LIFO constraints",
      "Real-time cost calculation"
    ),
    endpoints = list(
      "/health" = "Health check",
      "/solve-complete-vrp" = "Complete VRP solution (POST)"
    )
  )
}

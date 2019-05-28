############### RAFFLE FUNCTION ##################

## Load helpers

source("R/01_helper_functions.R")

###

strr_raffle <- function(
  points, polys, poly_ID, units, distance = 200, diagnostic = FALSE,
  cores = 1) {
  
  # Check that cores is an integer > 0
  cores <- floor(cores)
  if (cores <= 0) {
    stop("The argument `cores` must be a positive integer.")
  }
  
  # Check that distance > 0
  if (distance <= 0) {
    stop("The argument `distance` must be a positive number.")
  }
  
  # Convert points and polys from sp
  if (is(points, "Spatial")) {
    points <- st_as_sf(points)
  }
  if (is(polys, "Spatial")) {
    polys <- st_as_sf(polys)
  }
  
  # Check that points and polys are sf
  if (is(points, "sf") == FALSE) {
    stop("The object `points` must be of class sf or sp.")
  }
  if (is(polys, "sf") == FALSE) {
    stop("The object `polys` must be of class sf or sp.")
  }
  
  # Convert points and polys to tibble
  points <- as_tibble(points) %>% st_as_sf()
  polys  <- as_tibble(polys)  %>% st_as_sf()
  
  # Transform polys CRS to match points
  if (st_crs(points) != st_crs(polys)) {
    polys <- st_transform(polys, st_crs(points))
  }
  
  # Quote variables
  poly_ID  <- enquo(poly_ID)
  units    <- enquo(units)
  
  # Initialize helper fields
  points <-
    points %>%
    mutate(.point_x = st_coordinates(.)[,1],
           .point_y = st_coordinates(.)[,2],
           .point_ID = seq_len(nrow(points)))
  
  # Clean up polys and initialize poly_area field
  polys <-
    polys %>%
    filter(!! units > 0) %>%
    st_set_agr("constant") %>% # Prevent warnings from the st operations
    mutate(
      !! poly_ID := as.character(!! poly_ID), # Make sure poly_ID is not factor
      poly_area = st_area(.) # Calculate polygon areas
    ) %>%
    st_set_agr("constant")
  
  # Generate buffers and intersect with polygons
  intersects <-
    points %>%
    st_buffer(dist = distance, nQuadSegs = 10) %>%
    st_set_agr("constant") %>%
    st_intersection(polys)
  
  # Estimate int_units
  intersects <-
    intersects %>%
    mutate(
      int_units = as.numeric(!! units * st_area(.) / .data$poly_area)) %>%
    st_set_agr("constant")
  
  # Transform intersects relative to point coordinates
  intersects <-
    intersects %>%
    mutate(geometry = purrr::pmap(
      list(.data$geometry, .data$.point_x, .data$.point_y), ~{
        ..1 - c(..2, ..3)
      }) %>%
        st_sfc())
  
  # Multi-threaded version of integration
  if (cores >= 2) {
    
    clusters <- pbapply::splitpb(nrow(intersects), cores, nout = 100)
    intersects_list <- lapply(clusters, function(x) intersects[x,])
    cl <- parallel::makeForkCluster(cores)
    
    intersects <-
      intersects_list %>%
      pbapply::pblapply(raffle_integrate, cl = cl) %>%
      do.call(rbind, .)
    
    # Single-threaded version of integration
  } else {
    intersects <- raffle_integrate(intersects)
  }
  
  # Initialize results object
  results <-
    intersects %>%
    st_drop_geometry() %>%
    group_by(.data$.point_ID)
  
  # Choose winners
  if (diagnostic == TRUE) {
    results <-
      results %>%
      summarize(
        winner = as.character(
          base::sample(!! poly_ID, 1, prob = .data$probability)),
        candidates = list(matrix(
          c(!! poly_ID, (.data$probability) / sum(.data$probability)),
          ncol = 2))
      )
  } else {
    results <-
      results %>%
      summarize(
        winner = as.character(
          base::sample(!! poly_ID, 1, prob = .data$probability))
      )
  }
  
  # Join winners to point file
  points <-
    left_join(points, results, by = ".point_ID") %>%
    select(-.data$.point_ID, -.data$.point_x, -.data$.point_y)
  points
}



raffle_pdf <- function(x) {
  dnorm(sqrt(x[,1]^2 + x[,2]^2), mean = 100, sd = 50, log = FALSE) *
    (1 / (2 * pi))
}


raffle_integrate <- function(intersects) {
  intersects %>%
    mutate(probability = purrr::map2_dbl(.data$geometry, .data$int_units, ~{
      polyCub::polyCub.midpoint(as(.x, "Spatial"), raffle_pdf) * .y
    }))
}

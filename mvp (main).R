library(ggplot2)
library(tidyr)
library(dplyr)

mvp <- function(data, percentiles, selected_methods, plot = FALSE, weights = NULL) {
  # Sort data 
  sorted_data <- sort(data)  
  n <- length(data)
  
  # Define available methods
  all_methods <- list(
    "normal.cdf" = function(mu, sigma, percent) {
      p <- percent / 100
      z_p <- qnorm(p)
      x_p <- mu + z_p * sigma
      return(x_p)
    },
    
    "normal.est" = function(sorted_data, percent) {
      m <- mean(sorted_data)
      s <- sd(sorted_data)
      p <- percent / 100
      z_p <- qnorm(p)
      x_p <- m + z_p * s
      return(x_p)
    },
    
    "inv.ecdf" = function(sorted_data, percent) {
      n <- length(sorted_data)
      np <- n * (percent / 100)
      j <- floor(np)
      g <- np - j
      if (j == 0) {
        return(sorted_data[1])
      } else if (g == 0) {
        return(sorted_data[j])
      } else {
        return(sorted_data[min(j + 1, n)])
      }
    },
    
    "inv.ecdf.AD" = function(data, percent) {
      n <- length(sorted_data)
      np <- n * (percent / 100)
      j <- floor(np)
      g <- np - j
      if (g > 0) {
        return(sorted_data[j + 1])
      } else {
        if (j < 1) j <- 1
        return((sorted_data[j] + sorted_data[j + 1]) / 2)
      }
    },
    
    "nearest.os" = function(data, percent) {
      n <- length(sorted_data)
      np <- n * (percent / 100)
      j <- floor(np)
      g <- np - j
      if (j == 0) {
        return(sorted_data[1])
      }
      if (g < 0.5 || (g == 0.5 && j %% 2 == 0)) {
        return(sorted_data[j])
      } else {
        return(sorted_data[j + 1])
      }
    },
    
    "pli.asym" = function(data, percent) {
      data <- sort(sorted_data)
      n <- length(sorted_data)
      p <- percent / 100
      np <- n * p
      j <- floor(np)
      g <- np - j
      
      if (j == n) {
        return(sorted_data[n])
      } else {
        return((1 - g) * sorted_data[j] + g * sorted_data[j + 1])
      }
    },
    
    "pli.onto" = function(data, percent) {
      n <- length(sorted_data)
      R <- percent / 100 * (n - 1) + 1
      if (R %% 1 == 0) {
        return(sorted_data[floor(R)])
      } else {
        R_floor <- floor(R)
        R_ceil <- ceiling(R)
        x1 <- R_floor
        y1 <- sorted_data[x1]
        x2 <- R_ceil
        y2 <- sorted_data[x2]
        y <- y1 + (R - x1) / (x2 - x1) * (y2 - y1)
        return(y)
      }
    },
    
    "pli.into" = function(data, percent) {
      n <- length(sorted_data)
      p <- percent / 100
      rank <- p * (n + 1)
      j <- floor(rank)
      g <- rank - j
      
      if (j == 0) {
        result <- sorted_data[1]
      } else if (j == n) {
        result <- sorted_data[n]
      } else {
        result <- (1 - g) * sorted_data[j] + g * sorted_data[j + 1]
      }
      return(result)
    },
    
    "pli.AM" = function(data, percent) {
      n <- length(sorted_data)
      p <- percent / 100
      p_i <- (1:n - 3/8) / (n + 1/4)
      m <- p * (n + 1/4) + 3/8
      j <- floor(m)
      g <- m - j
      
      if (j == 0) {
        result <- sorted_data[1]
      } else if (j >= n) {
        result <- sorted_data[n]
      } else {
        result <- (1 - g) * sorted_data[j] + g * sorted_data[j + 1]
      }
      return(result)
    },
    
    "pli.UM" = function(data, percent) {
      n <- length(sorted_data)
      p <- percent / 100
      p_i <- (1:n - 1/3) / (n + 1/3)
      k <- max(which(p_i <= p))
      gamma <- (p - p_i[k]) / (p_i[k + 1] - p_i[k])
      return((1 - gamma) * sorted_data[k] + gamma * sorted_data[k + 1])
    },
    
    "inv.wt.ecdf.AD" = function(sorted_data, weights, percent) {
      # Sort the data and weights together
      sorted_indices <- order(sorted_data)
      sorted_data <- sorted_data[sorted_indices]
      weights <- weights[sorted_indices]
      
      # Compute cumulative weights
      cumulative_weights <- cumsum(weights) / sum(weights)
      
      # Find the closest cumulative weight to the given percent
      P <- percent / 100
      
      # Locate the index where cumulative weight exceeds or equals the given percentile
      idx <- which(cumulative_weights >= P)[1]
      
      if (is.na(idx)) {
        return(sorted_data[length(sorted_data)])  # Return the last value if the percent is at the end
      }
      
      return(sorted_data[idx])
    }
    
  )
  
  # Decision tree for method suggestions
  suggest_methods <- function() {
    # Ask if the data is weighted
    cat("Is your data weighted? (yes/no): ")
    is_weighted <- tolower(readline()) == "yes"
    
    if (is_weighted) {
      # Suggest a weighted method if the data is weighted
      suggested_methods <- c("inv.wt.ecdf.AD")
      cat("Based on your inputs, the suggested method is 'inv.wt.ecdf.AD'.\n")
    } else {
      # Ask if the data follows a normal distribution
      cat("Does the data follow a normal distribution? (yes/no): ")
      normal_distribution <- tolower(readline()) == "yes"
      
      if (normal_distribution) {
        # Ask if the mean and standard deviation are known
        cat("Are the mean and standard deviation known? (yes/no): ")
        mean_sd_known <- tolower(readline()) == "yes"
        
        if (mean_sd_known) {
          suggested_methods <- c("normal.cdf")
          cat("Based on your inputs, the suggested method is 'normal.cdf'.\n")
        } else {
          suggested_methods <- c("normal.est")
          cat("Based on your inputs, the suggested method is 'normal.est'.\n")
        }
      } else {
        # Ask if the data is continuous or discrete
        cat("Is the data continuous or discrete? (continuous/discrete): ")
        continuous <- tolower(readline()) == "continuous"
        
        if (continuous) {
          suggested_methods <- c("pli.UM", "pli.AM", "pli.asym", "pli.onto", "pli.into")
          cat("Based on your inputs, suggested methods are: 'pli.UM', 'pli.AM', 'pli.asym', 'pli.onto', 'pli.into'.\n")
        } else {
          suggested_methods <- c("nearest.os", "inv.ecdf", "inv.ecdf.AD")
          cat("Based on your inputs, suggested methods are: 'nearest.os', 'inv.ecdf', 'inv.ecdf.AD'.\n")
        }
      }
    }
    
    # Prompt the user to select methods
    cat("Please enter the method(s) you want to use (separated by commas if multiple):\n")
    selected_methods <- strsplit(tolower(readline()), ",\\s*")[[1]]
    
    # Convert user input to the correct method names
    correct_case_methods <- names(all_methods)
    selected_methods <- correct_case_methods[tolower(correct_case_methods) %in% selected_methods]
    
    # Check if no valid method is selected
    if (length(selected_methods) == 0) {
      stop("No valid method selected. Please check the method names.")
    }
    
    return(selected_methods)
  }
  
  if (is.null(selected_methods)) {
    selected_methods <- suggest_methods()
  }
  
  if (is.null(weights)) {
    weights <- rep(1, length(sorted_data))
  }
  
  # Apply the selected methods
  results_list <- list()
  for (method in selected_methods) {
    if (method %in% names(all_methods)) {
      if (method == "inv.wt.ecdf.AD") {
        # Handle weighted method
        results <- sapply(percentiles, function(p) all_methods[[method]](sorted_data, weights, p))
      } else if (method == "normal.cdf") {
        # Ask for mean and SD for normal.cdf method
        cat("Please enter the mean (mu): ")
        mu <- as.numeric(readline())
        cat("Please enter the standard deviation (sigma): ")
        sigma <- as.numeric(readline())
        
        # Pass mean and SD to the normal.cdf method
        results <- sapply(percentiles, function(p) all_methods[[method]](mu, sigma, p))
      } else {
        results <- sapply(percentiles, function(p) all_methods[[method]](sorted_data, p))
      }
      results_list[[method]] <- results
    } else {
      cat("Method", method, "is not recognized.\n")
    }
  }
  
  for (method in names(results_list)) {
    result_values <- results_list[[method]]
    
    cat("$", method, "\n", sep = "")
    cat("   percent:    ", paste(percentiles, collapse = "  "), "\n", sep = "")
    cat("percentile: ", sprintf("%6.2f", result_values), "\n", sep = "")
  }
  
  if (plot) {
    # Define method groups
    normal_methods <- c("normal.cdf", "normal.est")
    ecdf_methods <- c("inv.ecdf", "inv.ecdf.AD", "nearest.os", "inv.wt.ecdf.AD")
    
    # Base plot with X and Y axis labels
    p <- ggplot() + 
      labs(x = "Percentile", y = "") +
      annotate("text", x = min(sorted_data) - 5, y = 104, label = "Percentage", hjust = 0, vjust = -0.5) +
      theme_minimal()
    
    # Define color palette
    colors <- c("blue", "red", "black", "darkgreen", "grey", "pink", "orange", 
                "darkviolet", "brown", "darkslategray", "coral3")
    legend_labels <- c(
      "Normal CDF (Known Mean and SD)" = "normal.cdf",
      "Normal Estimation (Unknown Mean and SD)" = "normal.est",
      "Inverse ECDF" = "inv.ecdf",
      "Inverse ECDF with Averaging at Discontinuities" = "inv.ecdf.AD",
      "Nearest Order Statistics" = "nearest.os",
      "Linear Interpolation of ECDF" = "pli.asym",
      "Symmetric Onto Linear Interpolation" = "pli.onto",
      "Symmetric Into Linear Interpolation" = "pli.into",
      "Linear Interpolation of Approximate Medians" = "pli.AM",
      "Linear Interpolation of Unbiased Medians" = "pli.UM",
      "Inverse Weighted ECDF with Averaging at Discontinuities" = "inv.wt.ecdf.AD"
    )
    
    # Create a sequence of percentiles for plotting
    plot_percentiles <- seq(0, 100, by = 1)
    
    # Plot each selected method
    for (i in seq_along(selected_methods)) {
      method <- selected_methods[i]
      
      # Calculate percentiles for the selected method
      if (method %in% names(all_methods)) {
        if (method == "inv.wt.ecdf.AD") {
          percentiles_values <- sapply(percentiles, function(p) all_methods[[method]](sorted_data, weights, p))
          plot_percentiles_values <- sapply(plot_percentiles, function(p) all_methods[[method]](sorted_data, weights, p))
          
        } else if (method == "normal.cdf") {
          # Apply the normal.cdf method with mu and sigma
          percentiles_values <- sapply(percentiles, function(p) all_methods[[method]](mu, sigma, p))
          plot_percentiles_values <- sapply(plot_percentiles, function(p) all_methods[[method]](mu, sigma, p))
          
        } else {
          percentiles_values <- sapply(percentiles, function(p) all_methods[[method]](sorted_data, p))
          plot_percentiles_values <- sapply(plot_percentiles, function(p) all_methods[[method]](sorted_data, p))
        }
      } else {
        stop(paste("Method", method, "is not recognized."))
      }
      
      # Create a dataframe with percentiles and their corresponding values
      plot_data <- data.frame(
        Percentile = plot_percentiles,  # The percentile values for plotting
        Value = plot_percentiles_values, # Corresponding values for plotting
        Method = method                  # Add the method name to group the data
      )
      
      # Use aes() to map color to Method inside geom_line
      p <- p + geom_line(data = plot_data, aes(x = Value, y = Percentile, color = Method))
      
      # Highlight points for percentiles
      highlight_data <- data.frame(
        Value = percentiles_values,  # Corresponding values for highlighting
        Percentile = percentiles,    # The user-provided percentiles
        Method = method              # Add method name for grouping
      )
      
      # Highlight points and add vertical and horizontal dashed lines
      if (length(selected_methods) == 1){
        p <- p +  geom_text(data = highlight_data, aes(x = Value, y = -8, label = round(Value, 2)), 
                            vjust = -1, hjust = 0.5, size = 3, color = "black")  # Annotate the Value on the Y axis
      }
      
      p <- p + 
        geom_point(data = highlight_data, aes(x = Value, y = Percentile, color = Method)) +  # Highlight percentile points
        geom_text(data = highlight_data, aes(x = min(sorted_data) - 7, y = Percentile + 2, label = round(Percentile, 1)), 
                  vjust = 1.5, hjust = 0.5, size = 3, color = "black") +  # Annotate the Percentile on the X axis
        geom_segment(data = highlight_data, aes(x = Value, xend = Value, y = 0, yend = Percentile), 
                     linetype = "dashed", color = "black") +
        geom_segment(data = highlight_data, 
                     aes(x = min(sorted_data) - 5, xend = Value, y = Percentile, yend = Percentile), 
                     linetype = "dashed", color = "black") +
        geom_segment(data = highlight_data, 
                     aes(x = Value, xend = Value, y = Percentile / 2, yend = Percentile / 2 - 0.1), 
                     linetype = "solid", color = "black", 
                     arrow = arrow(type = "closed", length = unit(0.15, "cm"))) +
        geom_segment(data = highlight_data, 
                     aes(x = (min(sorted_data) - 5 + Value) / 2, xend = (min(sorted_data) - 5 + Value) / 2 + 0.1, 
                         y = Percentile, yend = Percentile), 
                     linetype = "solid", color = "black", 
                     arrow = arrow(type = "closed", length = unit(0.15, "cm"))) 
    }
    
    # Manually define the colors and legend labels
    p <- p + scale_color_manual(values = colors, labels = legend_labels) +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    # Add X and Y axes 
    p <- p + 
      annotate("segment", x = min(sorted_data) - 5, xend = max(sorted_data) + 5, y = 0, yend = 0,
               arrow = arrow(type = "closed", length = unit(0.3, "cm")), linewidth = 1, color = "black") +  # X-axis arrow
      annotate("segment", x = min(sorted_data) - 5, xend = min(sorted_data) - 5, y = 0, yend = 102,
               arrow = arrow(type = "closed", length = unit(0.3, "cm")), linewidth = 1, color = "black")   # Y-axis arrow
    
    # Draw the plot
    print(p)
  }
  
  invisible(results_list)
}
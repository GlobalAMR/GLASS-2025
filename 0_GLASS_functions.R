######################################################
# FUNCTIONS USED IN GLASS REPORT
######################################################
library(dplyr)

# CHAPTER 3
######################################################

plot_bci_map <- function(shapefile, bci_data, year, specimen, palette, na_color = "lightgrey") {

  # Load the shapefile
  world <- shapefile
  #world <- st_transform(world, crs = 4326)

  # Filter the BCI data for the specified year and specimen
  filtered_bci <- bci_data %>%
    filter(Specimen == specimen & Year == year)

  # Ensure the ISO3 column is named correctly for merging
  world <- world %>%
    #rename(Iso3 = "ISO_A3")
    rename(Iso3 = "ISO_3_CODE")
  
  # Merge the filtered data with the shapefile data
  merged_data <- world %>%
    left_join(filtered_bci, by = c("Iso3")) #%>%
    #st_make_valid() #%>%
    #filter(st_is_valid(geometry))
  
  merged_data <- left_join(merged_data%>%select(-c(WHORegionCode)), cdata%>%select(Iso3, WHORegionCode), by="Iso3")
  
  #table(merged_data$WHORegionCode, useNA="always")
  #merged_data$REGION_UN[which(is.na(merged_data$WHORegionCode))]
  # merged_data <- merged_data %>%
  #   mutate(
  #     REGION_CODE = case_when(
  #       REGION_UN == "Europe" ~ "EUR",
  #       REGION_UN == "Americas" ~ "AMR",
  #       REGION_UN == "Africa" ~ "AFR",
  #       TRUE ~ NA_character_  # Handle cases not matching any condition
  #     )
  #   ) #%>%
  #   #filter(!is.na(WHORegionCode))
  
  merged_data <- merged_data %>%
    mutate(
    centroid_coords = st_coordinates(st_point_on_surface(geometry)),
    lon = centroid_coords[,1],
    lat = centroid_coords[,2]
    )

  # breaks <- c(0, 100, 500, 1000, 1500, 2000, 2500, Inf)  # Define the bin edges, with 'Inf' for values above 2500
  # labels <- c("<100", "101-500", "501-1000", "1001-1500", "1501-2000", "2000-2500", ">2500")  # Labels for each bin

  breaks <- c(0, 10, 50, 100, 500, 1000, 2000, Inf)
  labels <- c("<10", "10-50", "51-100", "101-500", "501-1000", "1001-2000", ">2000")
  
  # Create categories based on uneven bins
  merged_data <- merged_data %>%
    mutate(BCI_cat = cut(BCI_permillion,
                         breaks = breaks,
                         labels = labels,
                         include.lowest = TRUE))

  # Define sizes for each category
  # size_mapping <- c("<100" = 2, "101-500" = 4, "501-1000" = 6, "1001-1500" = 8, "1501-2000" = 10,
  #                   "2000-2500" = 12, ">2500" = 14)
  size_mapping <- c("<10" = 1, "10-50" = 4, "51-100" = 6, "101-500" = 8, "500-1000" = 10, 
                    "1001-2000" = 12, ">2000"=14)
  merged_data <- merged_data %>%
    mutate(size = size_mapping[BCI_cat])
  
  specimen_title = ifelse(specimen=="BLOOD", "Bloodstream",
                          ifelse(specimen=="URINE", "Urinary tract",
                                 ifelse(specimen == "STOOL", "Gastrointestinal tract",
                                        ifelse(specimen=="UROGENITAL", "Gonorrhoa",NA))))

  # Plot the map with BCI bubbles
  ggplot(data = merged_data) +
    geom_sf(fill = "white", col="black") +  # Map background
    geom_point(aes(x = lon, y = lat, size = size,color = WHORegionCode),
              alpha = 0.7) +  # BCI bubbles on the map
    scale_size_continuous(name = "BCI per million population",
                         breaks = size_mapping,  # Ensure legend shows all categories
                         labels = labels) +  # Labels for the legend
    theme_void() +
   scale_colour_manual(values = palette) +
    labs(
      title = paste0(specimen_title),
      #subtitle = paste0(specimen_title),
      size = "BCI isolates/million population"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),  # Center the title
      plot.subtitle = element_text(hjust = 0.5),  # Optionally center the subtitle
      legend.position = "right",  # Ensure legend is visible
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin=grid::unit(c(0,0,0,0), "mm")  # Minimize margins
    )+
    guides(
      #size = guide_legend(title = "BCI isolates/million population"),
      colour = "none")+
    coord_sf(expand = FALSE)
    }

plot_bci_map_chlor <- function(shapefile, bci_data, year, specimen, palette,
                                               breaks, labels, na_color = "lightgrey") {
  
  # Load the shapefile
  world <- shapefile
  #world <- st_transform(world, crs = 4326)
  
  # Filter the BCI data for the specified year and specimen
  filtered_bci <- bci_data %>%
    filter(Specimen == specimen & Year == year)
  
  
  # Ensure the ISO3 column is named correctly for merging
  world <- world %>%
    #rename(Iso3 = "ISO_A3")
    rename(Iso3 = "ISO_3_CODE")
  
  # Merge the filtered data with the shapefile data
  merged_data <- world %>%
    left_join(filtered_bci, by = c("Iso3")) #%>%
  
  merged_data <- left_join(merged_data%>%select(-c(WHORegionCode)), cdata%>%select(Iso3, WHORegionCode), by="Iso3")
  
  merged_data <- merged_data %>%
    mutate(var = ifelse(category =="Disputed Land", "Not applicable", var),
           var = factor(var, levels=c("Not applicable", "1", "2","3","4","5","6","7","8"))
    )
  

  breaks = breaks
  labels = ifelse(labels=="0", "", labels)
  specimen_title = ifelse(specimen=="BLOOD", "Bloodstream",
                          ifelse(specimen=="URINE", "Urinary tract",
                                 ifelse(specimen == "STOOL", "Gastrointestinal tract",
                                        ifelse(specimen=="UROGENITAL", "Gonorrhoa",NA))))
  
  # Plot the map with BCI bubbles
  ggplot() +
    geom_sf(data = merged_data %>% filter(category !="Disputed Border"),aes(fill = var), color = "black",size=0.2) +  # Fill countries based on BCI categories
    geom_sf(data = merged_data %>% filter(category =="Disputed Border"),
            linetype = "dashed", 
            color = "black",
            size=0.2)+
    scale_fill_manual(
      values = c(
        "grey",
        palette
      ),
      labels = c(
        "Not applicable",
        labels[1], labels[2], labels[3], labels[4],
        labels[5], labels[6], labels[7], labels[8],
        "No data"
      ),
      na.value = na_color,  # Color for NA values
      na.translate = TRUE,  # Include NA values in the legend
      drop = FALSE          # Retain all legend levels
    ) +
    theme_void() +
    labs(
      title = paste0(specimen_title),
      fill = ""  # Legend title
    ) +
    theme(
      legend.position = "right",         # Move the legend to the bottom
      legend.spacing.y = unit(1, "cm"),     # Remove vertical spacing
      title = element_text(size = 20),
      plot.title = element_text(hjust = 0.5, vjust = 1.5, margin = margin(b = 20)),  # Center the title
      plot.subtitle = element_text(hjust = 0.5),            # Center the subtitle
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = grid::unit(c(0, 1, 0, 1), "mm"),         # Top, Right, Bottom, Left,
      panel.border = element_rect(color = "white", size = 0.5) 
    ) +
    guides(
      fill = guide_legend(                       # Place all legend items in one row
        label.position = "right",       # Position labels below the keys
        label.vjust = 1.4,#,                  # Adjust vertical alignment of labels,
        #keywidth = unit(1, "cm"),  # Set the width of the legend keys
        #keyheight = unit(1, "cm"),
        override.aes = list(color = c(NA, NA, NA, NA, NA, NA,NA, NA,NA,"black")))
      ) +
    coord_sf(expand = FALSE)
}

create_bubble_chart <- function(data, specimen, outer_size = 30, inner_size = 20, colors = facet_colors3) {
  
  # Filter data for the specified specimen type
  data_filtered <- data %>% filter(Specimen == specimen) %>%
    mutate(
      Region2 = str_wrap(Region, width = 15),
      Region2 = relevel(factor(Region2), ref="Global")
    )
  
  # Create the plot
  ggplot(data_filtered, aes(x = Region2, y = -1, fill=Region)) +
    # Outer bubble (same size for all regions)
    geom_point(size = outer_size, 
               shape = 21, alpha = 0.3, color = "black") +  
    # Inner bubble (positioned within the outer one, same size for all regions)
    geom_point(size = inner_size,  
               shape = 21, alpha = 0.9, color = "white", y = -1.05) +  # Keep same y-position to center inner bubble
    # Text for BCITotal (inside outer bubble)
    scale_fill_manual(values=facet_colors3) +
    geom_text(aes(label = BCITotal), 
              color = "black", 
              vjust = -1.6, # Center vertically within outer bubble
              size = 4) +  
    # Text for BCI2022 (inside inner bubble)
    geom_text(aes(label = paste0(BCI2022, "\n", round(BCI2022prop*100,1), "%")), 
              color = "black", 
              vjust = 1.02, # Center vertically within inner bubble
              size = 4) +  
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL,
      title = NULL
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_blank(),  # Hide y-axis text
      axis.ticks.y = element_blank(), # Hide y-axis ticks
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(0, 0, 0, 0), "cm")  # Remove margins
    ) + 
    ylim(-1.2, 0)
}


# THe below doesn't work yet
plot_bci_map_chlor_whomap <- function(data, year, specimen, breaks, labels, low.col = "#BDD7E7", high.col = "#08519C",
                         line.col = "white", water.col = "white", legend.title = "", legend.pos = NULL) {
  # Filter data for specified year and specimen
  d <- data %>% filter(Year == year, Specimen == specimen)
  
  # Create categories based on uneven bins
  d <- d %>%
    mutate(BCI_cat = cut(BCI_permillion,
                         breaks = breaks,
                         labels = labels,
                         include.lowest = TRUE))
  
  # Create the map
  p <- whomap(data.frame(iso3 = d$Iso3, var = d$BCI_cat), low.col = low.col, 
              high.col = high.col, line.col = line.col, map.title = "", legend.title = legend.title,
              water.col = water.col, na.label = "No data", disclaimer = FALSE, legend.pos = legend.pos, recentre = 12) +
    labs(
      title = "Bacterial Culture Isolates per 1,000,000 Population",
      subtitle = paste0(specimen, " - ", year)
    )
  
  return(p)
}

plot_bci_map_combined <- function(shapefile, bci_data, year, specimen, who_region_data, colour = NULL, na_color = "lightgrey") {
  
    # Load the shapefile
    world <- shapefile
    world <- st_transform(world, crs = 4326)

    # Filter the BCI data for the specified year and specimen
    filtered_bci <- bci_data %>%
      filter(Specimen == specimen & Year == year)

    # Ensure the ISO3 column is named correctly for merging
    world <- world %>%
      rename(Iso3 = "ISO_A3")

    # Merge the filtered data with the shapefile data
    merged_data <- world %>%
      left_join(filtered_bci, by = "Iso3")%>%
      st_make_valid()

    merged_data <- merged_data %>%
      mutate(
      centroid_coords = st_coordinates(st_centroid(geometry)),
      lon = centroid_coords[,1],
      lat = centroid_coords[,2]
      )

    # Define uneven categories
    breaks <- c(0, 100, 500, 1000, 1500, 2000, 2500, Inf)  # Define the bin edges, with 'Inf' for values above 2500
    labels <- c("<100", "101-500", "501-1000", "1001-1500", "1501-2000", "2000-2500", ">2500")  # Labels for each bin

    # Create categories based on uneven bins
    merged_data <- merged_data %>%
      mutate(BCI_cat = cut(BCI_permillion,
                           breaks = breaks,
                           labels = labels,
                           include.lowest = TRUE))

    # Define sizes for each category
    size_mapping <- c("<100" = 2, "101-500" = 4, "501-1000" = 6, "1001-1500" = 8, "1501-2000" = 10,
                      "2000-2500" = 12, ">2500" = 14)
    merged_data <- merged_data %>%
      mutate(size = size_mapping[BCI_cat])

    # Plot the map with BCI bubbles
    map_plot <- ggplot(data = merged_data) +
      geom_sf(fill = "lightgrey", color = "white") +  # Map background
      geom_point(aes(x = lon, y = lat, size = size),
                 color = colour, alpha = 0.7) +  # BCI bubbles on the map
      scale_size_continuous(name = "BCI per million population",
                            breaks = size_mapping,  # Ensure legend shows all categories
                            labels = labels) +  # Labels for the legend
      theme_minimal() +
      labs(
        title = "Bacterial Culture Isolates per 1,000,000 Population",
        subtitle = paste0(specimen, " - ", year),
        size = "BCI isolates/million population"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),  # Center the title
        plot.subtitle = element_text(hjust = 0.5),  # Optionally center the subtitle
        legend.position = "right"  # Ensure legend is visible
      )
  
  
  # Bubble Plot for WHO region data
  bubble_plot <- ggplot(who_region_data %>% filter(Specimen == specimen), 
                        aes(x = WHORegionCode, y = -1)) +
    # Outer bubble (same size for all regions)
    geom_point(size = 30, 
               shape = 21, alpha = 0.7, color = "black", 
               fill = colour) +  
    # Inner bubble (positioned within the outer one, same size for all regions)
    geom_point(size = 20,  
               shape = 21, alpha = 0.9, color = "white", 
               fill = "darkred", y = -1.08) +  # Keep same y-position to center inner bubble
    # Text for BCITotal (inside outer bubble)
    geom_text(aes(label = BCITotal), 
              color = "black", 
              vjust = -1.6, # Center vertically within outer bubble
              size = 4) +  
    # Text for BCI2022 (inside inner bubble)
    geom_text(aes(label = BCI2022), 
              color = "white", 
              vjust = 1.02, # Center vertically within inner bubble
              size = 4) +  
    theme_minimal() +
    labs(
      x = NULL,
      y = NULL,
      title = NULL
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_blank(),  # Hide y-axis text
      axis.ticks.y = element_blank(), # Hide y-axis ticks
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(0, 0, 0, 0), "cm")  # Remove margins
    ) + 
    ylim(-1.2, 0)  # Adjust y-limits to remove white space and fit bubbles
  
  # Combine the map plot and bubble plot using patchwork
  combined_plot <- map_plot / bubble_plot +
    plot_layout(heights = c(2, 1))  # Adjust height ratio to make the plots closer
  
  return(combined_plot)
}


# REGIONAL BCI PER MILLION SLOPES AND PREDICTIONS (Fig 3.4)
#-------------------------------------------------------------------
# EXTRACT COUNTRY-LEVEL SLOPES
summarize_regional_and_global_slopes <- function(fit, year_popdata, pdata, cdata, region_var = "WHORegionName", specimen) {
  # Extract posterior samples for fixed and random effects
  posterior_samples <- as_draws_df(fit)
  
  # Extract fixed effect for Year_c
  fixed_effect_samples <- posterior_samples$b_Year_c
  
  # Extract column names for random effects of Year_c
  random_effect_columns <- grep("^r_Iso3\\[.*?,Year_c\\]$", colnames(posterior_samples), value = TRUE)
  
  # Subset the random effects for Year_c
  random_effect_samples <- posterior_samples[, random_effect_columns]
  
  # Combine fixed and random slopes for each posterior draw
  posterior_country_slopes <- sweep(as.matrix(random_effect_samples), 1, fixed_effect_samples, FUN = "+")
  colnames(posterior_country_slopes) <- gsub("r_Iso3\\[|,Year_c\\]", "", colnames(posterior_country_slopes))  # Clean column names
  
  # Filter population data for the specified year
  pdata2022 <- pdata %>%
    filter(Year == year_popdata) %>%
    dplyr::select(Iso3, TotalPopulation)
  
  # Join with region data
  pdata2022 <- left_join(pdata2022, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = "Iso3")
  
  # Create long format of the country-level slopes
  posterior_country_long <- posterior_country_slopes %>%
    as.data.frame() %>%
    mutate(draw = 1:nrow(.)) %>%
    pivot_longer(
      cols = -draw,
      names_to = "Iso3",
      values_to = "country_slope"
    )
  
  # Join with population data
  posterior_country_long <- left_join(posterior_country_long, pdata2022, by = "Iso3")
  
  # Compute weighted mean slopes for each region
  posterior_regional_slopes <- posterior_country_long %>%
    group_by(draw, !!sym(region_var)) %>%
    summarise(
      weighted_mean_slope = sum(country_slope * TotalPopulation) / sum(TotalPopulation),
      .groups = "drop"
    )
  
  # summarise posterior trends for each region
  regional_trend_summary <- posterior_regional_slopes %>%
    group_by(!!sym(region_var)) %>%
    summarise(
      mean_slope = mean(weighted_mean_slope),
      median_slope = median(weighted_mean_slope),
      Q2.5 = quantile(weighted_mean_slope, 0.025),
      Q97.5 = quantile(weighted_mean_slope, 0.975),
      .groups = "drop"
    )
  
  # Compute the global weighted mean slope
  posterior_global_trends <- posterior_country_long %>%
    group_by(draw) %>%
    summarise(
      global_weighted_mean_slope = sum(country_slope * TotalPopulation) / sum(TotalPopulation),
      .groups = "drop"
    )
  
  # summarise global trend
  global_trend_summary <- posterior_global_trends %>%
    summarise(
      mean_slope = mean(global_weighted_mean_slope),
      median_slope = median(global_weighted_mean_slope),
      Q2.5 = quantile(global_weighted_mean_slope, 0.025),
      Q97.5 = quantile(global_weighted_mean_slope, 0.975)
    ) %>%
    mutate(WHORegionName = "Global") %>%
    dplyr::select(WHORegionName, mean_slope, median_slope, Q2.5, Q97.5)
  
  # Combine regional and global summaries
  regional_trend_summary <- bind_rows(regional_trend_summary, global_trend_summary)
  regional_trend_summary$specimen = specimen
  
  return(regional_trend_summary)
}


# ESTIMATE EXPECTED REGIONAL TRENDS (Fig 3.4)
#------------------------------------------------------------------------------------
# Define centering year for Year_c
generate_predictions_summary <- function(fit, data, year_range, pdata, cdata, specimen, specimen_filter, pathogen=T) {

  # Filter population data for the specified year
  # pdata2022 <- pdata %>%
  #   filter(Year %in% year_range) %>%
  #   dplyr::select(Iso3, TotalPopulation)
  #
  # # Join with region data
  # pdata2022 <- left_join(pdata2022, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = "Iso3")
  #

  pdata = left_join(pdata, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = c("Iso3"))

  posterior_samples <- as_draws_df(fit)

  # Extract random and fixed effects from posterior samples
  random_intercept_columns <- grep("^r_Iso3\\[.*?,Intercept\\]$", colnames(posterior_samples), value = TRUE)
  random_slope_columns <- grep("^r_Iso3\\[.*?,Year_c\\]$", colnames(posterior_samples), value = TRUE)

  # Extract random intercepts and slopes
  random_intercepts <- posterior_samples[, random_intercept_columns]
  random_slopes <- posterior_samples[, random_slope_columns]

  # Clean column names for Iso3
  colnames(random_intercepts) <- gsub("r_Iso3\\[|,Intercept\\]", "", colnames(random_intercepts))
  colnames(random_slopes) <- gsub("r_Iso3\\[|,Year_c\\]", "", colnames(random_slopes))

  # Combine fixed and random effects
  posterior_country_intercepts <- sweep(as.matrix(random_intercepts), 1, posterior_samples$b_Intercept, FUN = "+")
  posterior_country_slopes <- sweep(as.matrix(random_slopes), 1, posterior_samples$b_Year_c, FUN = "+")

  # Reshape intercepts and slopes to long format
  posterior_country_long <- posterior_country_intercepts %>%
    as.data.frame() %>%
    mutate(draw = 1:nrow(.)) %>%
    pivot_longer(-draw, names_to = "Iso3", values_to = "country_intercept") %>%
    left_join(
      posterior_country_slopes %>%
        as.data.frame() %>%
        mutate(draw = 1:nrow(.)) %>%
        pivot_longer(-draw, names_to = "Iso3", values_to = "country_slope"),
      by = c("Iso3", "draw")
    )

  # Filter the dataset for the current drug-bug combination (only when generating predictions for pathogen specific data)
  if(pathogen){
  data_subset <- data %>% filter(combined == drug_bug)
  }else{
  data_subset = data %>% filter(Specimen == specimen_filter)
  }

  data_subset$Year_c = scale(data_subset$Year, center=TRUE, scale=FALSE)[,1] # Just center
  unique_years <- unique(data_subset[, c("Year", "Year_c")])


  # Generate predictions for each year
  posterior_country_long <- posterior_country_long %>%
    mutate(Year = list(seq(min(year_range), max(year_range)))) %>%
    unnest(Year) %>%
    left_join(unique_years)

  # Merge with population and region data
  posterior_country_long <- posterior_country_long %>%
    left_join(pdata, by = c("Iso3", "Year"))


  posterior_country_long = posterior_country_long %>%
    mutate(
      prediction_log = country_intercept + country_slope * Year_c,
      prediction = exp(prediction_log) * 1e6  # Adjust scale as needed
    )

  # Calculate regional predictions
  posterior_regional_predictions <- posterior_country_long %>%
    group_by(draw, WHORegionName, Year) %>%
    summarise(
      weighted_prediction = sum(prediction * TotalPopulation) / sum(TotalPopulation),
      .groups = "drop"
    )

  # Calculate global predictions
  posterior_global_predictions <- posterior_country_long %>%
    group_by(draw, Year) %>%
    summarise(
      weighted_prediction = sum(prediction * TotalPopulation) / sum(TotalPopulation),
      .groups = "drop"
    )

  # summarise posterior distributions for regions
  regional_trend_summary <- posterior_regional_predictions %>%
    group_by(WHORegionName, Year) %>%
    summarise(
      mean_prediction = mean(weighted_prediction),
      median_prediction = median(weighted_prediction),
      Q2.5 = quantile(weighted_prediction, 0.025),
      Q97.5 = quantile(weighted_prediction, 0.975),
      .groups = "drop"
    )

  # summarise posterior distributions globally
  global_trend_summary <- posterior_global_predictions %>%
    group_by(Year) %>%
    summarise(
      mean_prediction = mean(weighted_prediction),
      median_prediction = median(weighted_prediction),
      Q2.5 = quantile(weighted_prediction, 0.025),
      Q97.5 = quantile(weighted_prediction, 0.975),
      .groups = "drop"
    ) %>%
    mutate(WHORegionName = "Global") %>%
    dplyr::select(WHORegionName, Year, mean_prediction, median_prediction, Q2.5, Q97.5)

  # Combine regional and global summaries
  regional_trend_summary <- bind_rows(regional_trend_summary, global_trend_summary)

  # Ensure consistent factor levels for WHORegionName
  regional_trend_summary <- regional_trend_summary %>%
    mutate(
      WHORegionName = factor(WHORegionName,
                             levels = c("Global",
                                        "African Region",
                                        "Region of the Americas",
                                        "South-East Asia Region",
                                        "European Region",
                                        "Eastern Mediterranean Region",
                                        "Western Pacific Region"))
    )
  regional_trend_summary$specimen = specimen

  return(regional_trend_summary)
}


# ALTERNATIVE USING TIDYBAYES (DOES NOT WORK PROPERLY YET)
# generate_predictions_summary <- function(fit, data, year_range, pdata, cdata, specimen, specimen_filter, pathogen = TRUE) {
#   
#   # Prepare population data
#   pdata <- left_join(pdata, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = c("Iso3"))
#   
#   # Filter data for the current drug-bug combination or specimen
#   if (pathogen) {
#     data_subset <- data %>% filter(combined == drug_bug)
#   } else {
#     data_subset <- data %>% filter(Specimen == specimen_filter)
#   }
#   
#   # Center Year for consistency with the model
#   data_subset$Year_c <- scale(data_subset$Year, center = TRUE, scale = FALSE)[, 1] # Centering Year
#   unique_years <- unique(data_subset[, c("Year", "Year_c")])
#   
#   # Create new data for predictions
#   # Countries to predict for
#   iso3 <- unique(data_subset$Iso3)
#   year <- year_range
#   
#   # Create expanded dataset for prediction
#   d <- expand.grid(
#     Iso3 = iso3,
#     Year = year
#   )
#   d = left_join(d,unique_years)
#   
#   d2 <- left_join(d, pdata, by = c("Year","Iso3"))
#   new_data = d2
#   
#   # Generate posterior predictions using `epred_draws`
#   tidy_posterior_pred <- fit %>%
#     epred_draws(newdata = new_data)
#   
#   
#   # Compute predictions on the original scale (if required)
#   # tidy_posterior_pred <- tidy_posterior_pred %>%
#   #   mutate(prediction = .epred * 1e6) # Scale as needed
#   
#   # Regional weighted predictions
#   posterior_regional_predictions <- tidy_posterior_pred %>%
#     group_by(.draw, WHORegionName, Year) %>%
#     summarise(
#       weighted_prediction = sum(.epred * TotalPopulation) / sum(TotalPopulation),
#       .groups = "drop"
#     )
#   
#   # Global weighted predictions
#   posterior_global_predictions <- tidy_posterior_pred %>%
#     group_by(.draw, Year) %>%
#     summarise(
#       weighted_prediction = sum(.epred * TotalPopulation) / sum(TotalPopulation),
#       .groups = "drop"
#     ) %>%
#     mutate(WHORegionName = "Global")
#   
#   # summarise posterior distributions for regions
#   regional_trend_summary <- posterior_regional_predictions %>%
#     group_by(WHORegionName, Year) %>%
#     summarise(
#       mean_prediction = mean(weighted_prediction),
#       median_prediction = median(weighted_prediction),
#       Q2.5 = quantile(weighted_prediction, 0.025),
#       Q97.5 = quantile(weighted_prediction, 0.975),
#       .groups = "drop"
#     )
#   
#   # summarise posterior distributions globally
#   global_trend_summary <- posterior_global_predictions %>%
#     group_by(Year) %>%
#     summarise(
#       mean_prediction = mean(weighted_prediction),
#       median_prediction = median(weighted_prediction),
#       Q2.5 = quantile(weighted_prediction, 0.025),
#       Q97.5 = quantile(weighted_prediction, 0.975),
#       .groups = "drop"
#     ) %>%
#     mutate(WHORegionName = "Global") %>%
#     dplyr::select(WHORegionName, Year, mean_prediction, median_prediction, Q2.5, Q97.5)
#   
#   # Combine regional and global summaries
#   regional_trend_summary <- bind_rows(regional_trend_summary, global_trend_summary)
#   
#   # Ensure consistent factor levels for WHORegionName
#   regional_trend_summary <- regional_trend_summary %>%
#     mutate(
#       WHORegionName = factor(
#         WHORegionName,
#         levels = c(
#           "Global", "African Region", "Region of the Americas",
#           "South-East Asia Region", "European Region",
#           "Eastern Mediterranean Region", "Western Pacific Region"
#         )
#       )
#     )
#   regional_trend_summary$specimen <- specimen
#   
#   return(regional_trend_summary)
# }
# 


generate_absolute_change_summary <- function(fit,data, year_range, pdata, cdata, specimen, specimen_filter =NULL, pathogen=T) {
  
  # Filter population data for the specified year
  # pdata2022 <- pdata %>%
  #   filter(Year == year_range[2]) %>%
  #   dplyr::select(Iso3, TotalPopulation)
  # 
  # # Join with region data
  # pdata2022 <- left_join(pdata2022, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = "Iso3")
  
  pdata = left_join(pdata, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = c("Iso3"))
  
  # Extract posterior samples
  posterior_samples <- as_draws_df(fit)
  
  # Extract random and fixed effects
  random_intercept_columns <- grep("^r_Iso3\\[.*?,Intercept\\]$", colnames(posterior_samples), value = TRUE)
  random_slope_columns <- grep("^r_Iso3\\[.*?,Year_c\\]$", colnames(posterior_samples), value = TRUE)
  
  # Extract random intercepts and slopes
  random_intercepts <- posterior_samples[, random_intercept_columns]
  random_slopes <- posterior_samples[, random_slope_columns]
  
  # Clean column names for Iso3
  colnames(random_intercepts) <- gsub("r_Iso3\\[|,Intercept\\]", "", colnames(random_intercepts))
  colnames(random_slopes) <- gsub("r_Iso3\\[|,Year_c\\]", "", colnames(random_slopes))
  
  # Combine fixed and random effects
  posterior_country_intercepts <- sweep(as.matrix(random_intercepts), 1, posterior_samples$b_Intercept, FUN = "+")
  posterior_country_slopes <- sweep(as.matrix(random_slopes), 1, posterior_samples$b_Year_c, FUN = "+")
  
  # Reshape intercepts and slopes to long format
  posterior_country_long <- posterior_country_intercepts %>%
    as.data.frame() %>%
    mutate(draw = 1:nrow(.)) %>%
    pivot_longer(-draw, names_to = "Iso3", values_to = "country_intercept") %>%
    left_join(
      posterior_country_slopes %>%
        as.data.frame() %>%
        mutate(draw = 1:nrow(.)) %>%
        pivot_longer(-draw, names_to = "Iso3", values_to = "country_slope"),
      by = c("Iso3", "draw")
    )
  
  # Filter the dataset for the current drug-bug combination (only when generating predictions for pathogen specific data)
  if(pathogen){
    data_subset <- data %>% filter(combined == drug_bug)
  }else{
    data_subset = data %>% filter(Specimen == specimen_filter)
  }
  
  data_subset$Year_c = scale(data_subset$Year, center=TRUE, scale=FALSE)[,1] # Just center 
  unique_years <- unique(data_subset[, c("Year", "Year_c")])
  
  
  # Generate predictions for each year
  posterior_country_long <- posterior_country_long %>%
    mutate(Year = list(seq(min(year_range), max(year_range)))) %>%
    unnest(Year) %>%
    left_join(unique_years) 
  
  # Merge with population and region data
  posterior_country_long <- posterior_country_long %>%
    left_join(pdata, by = c("Iso3", "Year"))
  
  posterior_country_long = posterior_country_long %>%
    mutate(
      prediction_log = country_intercept + country_slope * Year_c,
      prediction = exp(prediction_log) * 1e6  # Adjust scale as needed
    )
  
  # Calculate yearly differences
  posterior_country_long <- posterior_country_long %>%
    group_by(draw, Iso3) %>%
    arrange(Year) %>%
    mutate(yearly_difference = prediction - lag(prediction)) %>%
    ungroup()
  
  # Calculate regional yearly absolute changes
  posterior_regional_changes <- posterior_country_long %>%
    group_by(draw, WHORegionName) %>%
    summarise(
      weighted_absolute_change = sum(yearly_difference * TotalPopulation, na.rm = TRUE) / sum(TotalPopulation, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate global yearly absolute changes
  posterior_global_changes <- posterior_country_long %>%
    group_by(draw) %>%
    summarise(
      weighted_absolute_change = sum(yearly_difference * TotalPopulation, na.rm = TRUE) / sum(TotalPopulation, na.rm = TRUE),
      .groups = "drop"
    )
  
  # summarise posterior distributions for regions
  regional_change_summary <- posterior_regional_changes %>%
    group_by(WHORegionName) %>%
    summarise(
      mean_absolute_change = mean(weighted_absolute_change, na.rm = TRUE),
      median_absolute_change = median(weighted_absolute_change, na.rm = TRUE),
      Q2.5 = quantile(weighted_absolute_change, 0.025, na.rm = TRUE),
      Q97.5 = quantile(weighted_absolute_change, 0.975, na.rm = TRUE),
      min = min(weighted_absolute_change),
      max = max(weighted_absolute_change),
      .groups = "drop"
    )
  
  # summarise posterior distributions globally
  global_change_summary <- posterior_global_changes %>%
    summarise(
      mean_absolute_change = mean(weighted_absolute_change, na.rm = TRUE),
      median_absolute_change = median(weighted_absolute_change, na.rm = TRUE),
      Q2.5 = quantile(weighted_absolute_change, 0.025, na.rm = TRUE),
      Q97.5 = quantile(weighted_absolute_change, 0.975, na.rm = TRUE),
      min = min(weighted_absolute_change),
      max = max(weighted_absolute_change),
      .groups = "drop"
    ) %>%
    mutate(WHORegionName = "Global") %>%
    dplyr::select(WHORegionName, mean_absolute_change, median_absolute_change, Q2.5, Q97.5, min, max)
  
  # Combine regional and global summaries
  regional_change_summary <- bind_rows(regional_change_summary, global_change_summary)
  
  # Ensure consistent factor levels for WHORegionName
  regional_change_summary <- regional_change_summary %>%
    mutate(
      WHORegionName = factor(WHORegionName, 
                             levels = c("Global", 
                                        "African Region",
                                        "Region of the Americas",
                                        "South-East Asia Region",
                                        "European Region",
                                        "Eastern Mediterranean Region",
                                        "Western Pacific Region"))
    )
  regional_change_summary$specimen = specimen
  
  return(regional_change_summary)
}



# DESCRIPTION OF DATA
#########################################################
# SpecimenwithAST at specimen level - CTA
plot_bci_spec_trend_regional <- function(data, specimen, title = specimen, subtitle = "Regional") {
  data %>%
    filter(Specimen == specimen) %>%
    ggplot(aes(x = Year, y = BCI_permillion, color = Specimen, group = Specimen)) +
    geom_jitter(aes(size = BCI_permillion), width = 0.2, height = 0.2, alpha = 0.2) +  # Jittered points
    geom_smooth(size = 1.5, se = FALSE, method = "loess") +  # Smoothed lines (Loess)
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1.5, se = FALSE, colour = "seagreen") +  # Quadratic trend
    geom_smooth(method = "lm", formula = y ~ x, size = 1.5, se = FALSE, colour = "orange") +  # Linear trend
    facet_wrap(~ WHORegionCode, scales = "free_y", ncol = 6) +
    scale_x_continuous(breaks = c(2018, 2020, 2022)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = "BCI per million",
      color = "Region",
      fill = ""
    ) +
    theme_minimal() +
    #ylim(0, 1000) +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      title = element_text(size = 20),
      axis.text.x = element_text(size = 14),
      axis.text.y = ggtext::element_markdown(size = 18),
      strip.text.y.left = element_text(size = 18, angle = 0, vjust = 1.01, hjust = 1),
      strip.text = element_text(size = 20),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.major = element_line(linetype = "dotted"),
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "bottom",
      legend.text = element_text(size = 14)
    )
}


# SpecimenwithAST at specimen level - CTA
plot_bci_spec_trend_cta <- function(data, specimen, title = specimen, subtitle = "Regional") {
  data %>%
    filter(Specimen == specimen) %>%
    ggplot(aes(x = Year, y = BCI_permillion, color = Iso3, group = Iso3)) +
    geom_jitter(aes(size = BCI_permillion), width = 0.2, height = 0.2, alpha = 0.2) +  # Jittered points
    geom_line() +
    facet_wrap(~ WHORegionCode, scales = "free_y", ncol = 6) +
    scale_x_continuous(breaks = c(2018, 2020, 2022)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = "BCI per million",
      color = "Region",
      fill = ""
    ) +
    theme_minimal() +
    #ylim(0, 1000) +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      title = element_text(size = 20),
      axis.text.x = element_text(size = 14),
      axis.text.y = ggtext::element_markdown(size = 18),
      strip.text.y.left = element_text(size = 18, angle = 0, vjust = 1.01, hjust = 1),
      strip.text = element_text(size = 20),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.major = element_line(linetype = "dotted"),
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "none",
      legend.text = element_text(size = 14)
    )
}

plot_bci_spec_trend_cta_fitted <- function(data, specimen, title = specimen, subtitle = "Regional") {
  data %>%
    filter(Specimen == specimen) %>%
    ggplot(aes(x = Year, y = BCI_permillion, color = Iso3, group = Iso3)) +
    geom_jitter(aes(size = BCI_permillion), width = 0.2, height = 0.2, alpha = 0.2) +  # Jittered points
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  # Linear trend
    facet_wrap(~ WHORegionCode, scales = "free_y", ncol = 6) +
    scale_x_continuous(breaks = c(2018, 2020, 2022)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = "BCI per million",
      color = "Region",
      fill = ""
    ) +
    theme_minimal() +
    #ylim(0, 1000) +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      title = element_text(size = 20),
      axis.text.x = element_text(size = 14),
      axis.text.y = ggtext::element_markdown(size = 18),
      strip.text.y.left = element_text(size = 18, angle = 0, vjust = 1.01, hjust = 1),
      strip.text = element_text(size = 20),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.major = element_line(linetype = "dotted"),
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "none",
      legend.text = element_text(size = 14)
    )
}






# Testing rate vs AMR 
plot_amr_test <- function(data, pathogen, specimen, palette,xaxis) {
  scale = ifelse(xaxis =="BCI_1000000pop", "crude", "standardised")
  ggplot(data %>% filter(PathogenName == pathogen, 
                         Specimen == specimen & InReport == "Yes"), 
         aes(x = !!sym(xaxis), y = amr_rate, col = WHORegionCode, group = AntibioticName)) +
    geom_point(size = 2, alpha = 0.5) +  
    geom_smooth(method = 'lm', formula = y ~ poly(x, 2), col = "black") +
    scale_colour_manual(values = palette) +
    facet_wrap(.~ AntibioticName, ncol = 4, scales = "free") +
    theme_minimal() + 
    labs(
      title = paste0("Testing rates vs AMR prevalence","(" ,scale, ")"),
      subtitle = paste0(pathogen, " - ", specimen),
      x = "BCI per million population",
      y = "Resistance %"
    )
}


# Plot sum of isolates by region
plot_isolates_db_asRegion <- function(data, year, pathogen, specimen, in_report, xlim_max = 1000, ncol_facet = 4, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year == year, PathogenName == pathogen, Specimen == specimen, InReport == in_report)
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = s_interpretableAST, y = DemographicsOrigin, col = WHORegionCode)) +
    geom_point(size = 2) +
    facet_wrap(. ~ AntibioticName, scales = "free_x", ncol = ncol_facet) +
    theme_minimal() + 
    geom_vline(xintercept = 100, col = "red", linetype = 2) + 
    geom_vline(xintercept = 50, col = "seagreen", linetype = 2) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    scale_color_manual(values = palette) +
    xlim(0, xlim_max) + 
    labs(
      title = "Number of isolates with interpretable AST - by region, age & sex",
      subtitle = paste0(pathogen, " - ", specimen), 
      x = "Number of Isolates",
      y = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}

plot_isolates_db_as <- function(data, year, specimen, in_report, exclude_antibiotic = NULL, ncol_facet = 3, xlim_max = 1000) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year == year, InReport == in_report, Specimen %in% specimen) %>%
    filter(!AntibioticName %in% ifelse(is.null(exclude_antibiotic), character(0), exclude_antibiotic))
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = s_interpretableAST, y = DemographicsOrigin, col = AntibioticName)) +
    geom_point(size = 3) +
    theme_minimal() +
    facet_wrap(. ~ PathogenName, ncol = ncol_facet, scales = "free") +
    geom_vline(xintercept = 100, col = "red", linetype = 2) + 
    geom_vline(xintercept = 50, col = "seagreen", linetype = 2) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    labs(
      title = "Number of isolates with interpretable AST - by age & sex",
      subtitle = specimen, 
      x = "Number of Isolates",
      y = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}

# plot country-level isolates by age and sex per region - boxplot
plot_isolatesdb_as_regionCOUNTRY <- function(data, year, pathogen, specimen, in_report, exclude_antibiotics = NULL, 
                                             ylim_max, facet_colors, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year %in% year, PathogenName == pathogen, Specimen == specimen, InReport == in_report) %>%
    filter(!AntibioticName %in% ifelse(is.null(exclude_antibiotics), character(0), exclude_antibiotics))
  
  # Create the plot
  p <- ggplot(filtered_data, aes(y = InterpretableAST, x = AgeCat10, fill = Sex)) +
    geom_jitter(aes(alpha = 0.6, col = Sex)) +
    geom_boxplot(na.rm = TRUE) +
    facet_grid2(AntibioticName ~ WHORegionCode, scales = "free_y",
                strip = strip_themed(
                  background_x = list(
                    element_rect(fill = facet_colors["AFR"]),
                    element_rect(fill = facet_colors["AMR"]),
                    element_rect(fill = facet_colors["EMR"]),
                    element_rect(fill = facet_colors["EUR"]),
                    element_rect(fill = facet_colors["SEA"]),
                    element_rect(fill = facet_colors["WPR"])
                  )
                )) +
    theme_minimal() +
    ylim(0, ylim_max) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text = element_text(size = 12)
    ) +
    scale_fill_manual(values = palette) +
    scale_alpha(guide = "none") + 
    labs(
      title = "Number of isolates with interpretable AST - by region, age & sex",
      subtitle = paste0(pathogen, " - ", specimen), 
      y = "Number of isolates",
      x = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}


# plot raw AMR rates by age and sex per region
plot_AMRdb_as_region <- function(data, year, pathogen, specimen, in_report, exclude_antibiotics = NULL, facet_colors, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year %in% year, PathogenName == pathogen, Specimen == specimen, InReport == in_report)
  
  # Apply the exclude antibiotics filter if exclude_antibiotics is not NULL
  if (!is.null(exclude_antibiotics)) {
    filtered_data <- filtered_data %>%
      filter(!AntibioticName %in% exclude_antibiotics)
  }
  
  # Create the plot
  p <- ggplot(filtered_data, aes(y = amr_rate, x = AgeCat10, fill = Sex)) +
    geom_jitter(aes(alpha = 0.6, col = Sex, size=BCI_1000000pop)) +
    geom_boxplot(na.rm = TRUE) +
    geom_smooth(aes(group = Sex, color = Sex), method = "loess", se = TRUE, linetype = "solid") +
    facet_grid2(AntibioticName ~ WHORegionCode,
                strip = strip_themed(
                  background_x = list(
                    element_rect(fill = facet_colors["AFR"]),
                    element_rect(fill = facet_colors["AMR"]),
                    element_rect(fill = facet_colors["EMR"]),
                    element_rect(fill = facet_colors["EUR"]),
                    element_rect(fill = facet_colors["SEA"]),
                    element_rect(fill = facet_colors["WPR"])
                  )
                )) +
    theme_minimal() +
    ylim(0,1) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text = element_text(size = 12)
    ) +
    scale_fill_manual(values = palette) +
    scale_alpha(guide = "none") + 
    labs(
      title = "Prevalence of resistance - by region, age & sex",
      subtitle = paste0(pathogen, " - ", "Carbapenems", " (", specimen, ")"), 
      y = "Proportion of resistant isolates",
      x = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}


# Plot map of raw AMR rates - by drug-bug combination
######################################################
plot_amr_map <- function(shapefile, amr_data, year, palette, title = NULL, estimated = NULL,
                         specimen, pathogen_name,antibiotic_name,antibiotic_label=NULL, na_color = "white",
                         show_fill=TRUE, subtitle=TRUE){
  
  # Load the shapefile
  world <- shapefile
  
  # Filter the AMR data
  filtered_rrates <- amr_data %>%
    filter(Specimen == specimen & PathogenName == pathogen_name & AntibioticName == antibiotic_name & Year==year)
  
  pathogen_label <- as.character(unique(filtered_rrates$PathogenName2[filtered_rrates$PathogenName==pathogen_name]))
  print(pathogen_label)
  
  # Ensure the ISO3 column is named correctly for merging
  world <- world %>%
    rename(Iso3 = "ISO_3_CODE")
  
  # Merge the filtered data with the shapefile data
  merged_data <- world %>%
    left_join(filtered_rrates, by = "Iso3")
  
  # Set specimen title
  Specimentitle <- ifelse(specimen == "BLOOD", "bloodstream infections",
                          ifelse(specimen == "URINE", "urinary tract infections",
                                 ifelse(specimen == "STOOL", "gastrointestinal tract infections",
                                        ifelse(specimen == "UROGENITAL", "Gonorrhoea", Specimen))))
  
  # Set main title
  maintitle <- ifelse(title == "Yes" & estimated == "Yes", 
                      paste("Map of estimated CTA-level prevalence of antibacterial resistance in", year),
                      ifelse(title == "Yes" & estimated == "No", 
                             paste("Map of crude CTA-level prevalence of antibacterial resistance in", year),
                             "")
  )
  
  # Create the base plot
  p <- ggplot()
  
  # Conditionally add geom_sf based on the `estimated` parameter
    if (estimated == "No") {
      p <- p + geom_sf(data = subset(merged_data, !is.na(amr_rate)), aes(fill = amr_rate * 100))
    } else {
      p <- p + geom_sf(data = subset(merged_data, !is.na(w_prev)), aes(fill = w_prev * 100))
    }
  
  antibiotic_label=ifelse(antibiotic_label=="Third-generation cephalosporins", "3rd gen cephalosporin resistance",
                          ifelse(antibiotic_label =="Methicillin resistance","Methicillin resistance", antibiotic_label))
  
  
  disputed_areas = merged_data %>%filter(category=="Disputed Land")
  # Create the base plot
  p <- p +
    scale_fill_gradientn(colors = palette, limits = c(0, 100), oob = scales::squish, name ="Percentage of \nresistant isolates", guide = guide_colorbar(order = 1)) +
    new_scale_fill() +
    geom_sf(data = subset(merged_data, is.na(amr_rate)), aes(fill = "No data")) +				
    geom_sf(data = disputed_areas, aes(fill = "Not applicable")) +
    scale_fill_manual(
      values = c("No data" = "white", "Not applicable" = "lightgrey"),
      labels = c("No data", "Not applicable"), guide = guide_legend(order = 2)) +
    # scale_fill_gradientn(
    #   colors = palette,
    #   na.value = na_color,
    #   limits = c(0, 100),
    #   guide = guide_colorbar(
    #     title.position = "top", # Move legend title to top
    #     title.hjust = 0.5       # Center align the legend title
    #   )
    # ) +
    theme_void() +
    labs(
      title = maintitle,
      # Use `expression()` to italicize pathogen_name
      subtitle = ifelse(subtitle==TRUE, bquote(italic(.(pathogen_label)) ~ "-" ~ .(tolower(antibiotic_label))), ""),
      fill = ""
    ) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 35, margin = margin(b = 30)),  # Adjust title spacing
      plot.subtitle = element_text(hjust = 0.5, size = 20),  # Center the subtitle
      #legend.position = ifelse(show_fill, "bottom","none"),                          # Place legend at bottom
      legend.position = "none",
      legend.title = element_text(size = 10,vjust=1),              # Adjust legend title size
      legend.text = element_text(size = 12),                # Adjust legend text size
      legend.spacing.y = unit(10, "pt") 
    ) +
    coord_sf(expand = FALSE)
  p
  # Return the final plot
  return(p)
}


# Function to plot trends over time
#################################################################################
plot_AMRdb_trend_regiontogether <- function(data, specimen, pathogen, year, 
                                            exclude_antibiotics, color_palette) {
  
  # Filter and summarise the data
  m_fit <- data %>%
    filter(Specimen == specimen & PathogenName == pathogen & Year %in% year) 
    
    # Apply the exclude antibiotics filter if exclude_antibiotics is not NULL
    if (!is.null(exclude_antibiotics)) {
      m_fit <- m_fit %>%
        filter(!AntibioticName %in% exclude_antibiotics)
    }
    
    m_fit <- m_fit %>% group_by(WHORegionCode, Year, Specimen, PathogenName, Antibiotic, AntibioticName) %>%
    mutate(
      ModelFit = median(amr_rate, na.rm = TRUE),
      ModelFit_lower = quantile(amr_rate, probs = 0.25, na.rm = TRUE),
      ModelFit_upper = quantile(amr_rate, probs = 0.75, na.rm = TRUE),
      .groups = 'drop'  # To avoid unnecessary grouping in the result
    )
  
  # Create the plot
  p <- ggplot(data = m_fit, aes(x = Year, y = amr_rate, color = WHORegionCode, group = WHORegionCode)) +
    geom_jitter(aes(size = BCI_1000000pop), width = 0.2, height = 0.2, alpha = 0.2) +  # Jittered points
    geom_smooth(size = 1.5, se = FALSE) +  # Smoothed lines
    facet_wrap(~AntibioticName, scales = "free_y", ncol = 4) +  # Faceting by AntibioticName
    ylim(0, 1) +  # Limit y-axis from 0 to 1
    labs(title = paste0("Trends in prevalence of resistance (", min(year), " - ", max(year), ")"),
         subtitle = paste0(specimen, ": ", pathogen, "-", "Carbapenems"), 
         x = "Year",
         y = "Proportion of resistant isolates",
         color = "Region",
         fill = "Region") +
    scale_color_manual(values = color_palette) +  # Custom color palette for lines and points
    scale_fill_manual(values = color_palette) +   # Custom color palette for ribbons
    theme_minimal() +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          strip.text = element_text(size = 10))
  
  return(p)
}

# plot raw AMR rates by age and sex per region
plot_AMRdb_trend_region <- function(data, year, pathogen, specimen, in_report, exclude_antibiotics = NULL, facet_colors, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year %in% year, PathogenName == pathogen, Specimen == specimen, InReport == in_report) %>%
    filter(!AntibioticName %in% ifelse(is.null(exclude_antibiotics), character(0), exclude_antibiotics))
  
  # Create the plot
  p <- ggplot(filtered_data, aes(y = amr_rate, x = as.factor(Year))) +
    geom_jitter(aes(alpha = 0.6, col = WHORegionCode, size = BCI_1000000pop)) +
    geom_boxplot(na.rm = TRUE) +
    geom_smooth(aes(group = WHORegionCode, color = WHORegionCode), method = "loess", se = T, linetype = "solid") +
    facet_grid2(AntibioticName ~ WHORegionCode,
                strip = strip_themed(
                  background_x = list(
                    element_rect(fill = facet_colors["AFR"]),
                    element_rect(fill = facet_colors["AMR"]),
                    element_rect(fill = facet_colors["EMR"]),
                    element_rect(fill = facet_colors["EUR"]),
                    element_rect(fill = facet_colors["SEA"]),
                    element_rect(fill = facet_colors["WPR"])
                  )
                )) +
    theme_minimal() +
    ylim(0,1) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text = element_text(size = 12)
    ) +
    scale_colour_manual(values = palette) +
    scale_alpha(guide = "none") + 
    labs(
      title = paste0("Prevalence of resistance over time - by region (", min(year), "-", max(year), ")"),
      subtitle = paste0(pathogen, " - ", specimen), 
      y = "Proportion of resistant isolates",
      x = "Year"
    )
  
  # Return the plot
  return(p)
}


# Calculate inverse variance weights to get weighted estimates
###################################################################
# The function can be used to
# - weight for country size or not by defining pop = Yes or No.
# - Use arbitrary correction for countryi size by defining Cor = number
#   By dividing the population by a large number, it is avoided to workd with large numbers
# - Normalise the weights or not. When using normalised weights, it is avoided that one country has
#   a disproportionaly large effect on the estimate

# ASTdata = adataAC
# year = c("2021")
# pop = "yes"
# pathogen = "Klebsiella pneumoniae"
# s="BLOOD"

ivw <- function(ASTdata, dbdata, year, pathogen, cor = 1000000, pop = c("yes", "no"), normalise_w = c("yes", "no")) {
  rates <- NULL
  
  # Ensure pop and normalise_w are valid
  #pop <- match.arg(pop)
  #normalise_w <- match.arg(normalise_w)
  
  # Loop over the different isolates and drugs
  d <- ASTdata %>% filter(PathogenName == pathogen & Year %in% year)
  for (s in unique(d$Specimen)) {
    db <- dbdata %>% filter(Pathogen == pathogen & Specimen == s)
    data_specimen <- d %>% filter(Specimen == s)
    ra <- NULL
    
    for (a in unique(data_specimen$AntibioticName)) {
      data <- data_specimen %>% filter(AntibioticName == a)
      
      # Calculate the resistance rate for each country
      # Apply continuity correction by adding 0.5 to numerator and 1 to denominator
      data$p_hat <- (data$Resistant + 0.5) / (data$InterpretableAST + 1)
      
      # Calculate the variance of the resistance rate for each country
      data$var_p_hat <- data$p_hat * (1 - data$p_hat) / (data$InterpretableAST + 1)
      
      # Calculate the inverse variance weights for each country; also weighting for country population number
      # Divide by country pop by arbitrary cor = large number to avoid having to work with large numbers
      if (pop == "yes") {
        data$w <- data$TotalPopulation / cor / data$var_p_hat
      } else {
        data$w <- 1 / data$var_p_hat
      }
      
      data$w_norm <- data$w / sum(data$w)
      
      # Calculate the weighted average resistance rate
      p_IVW <- if (normalise_w == "no") {
        sum(data$w * data$p_hat) / sum(data$w)
      } else {
        sum(data$w_norm * data$p_hat)
      }
      
      # Calculate the variance of the combined resistance rate
      var_p_IVW <- if (normalise_w == "no") {
        1 / sum(data$w)
      } else {
        1 / sum(data$w_norm / data$var_p_hat)
      }
      
      # Calculate the standard error of the combined resistance rate
      SE_IVW <- sqrt(var_p_IVW)
      
      # Calculate the 95% confidence interval
      z_score <- 1.96
      CI_lower <- p_IVW - z_score * SE_IVW
      CI_upper <- p_IVW + z_score * SE_IVW
      
      
      # Store results
      Year <- year
      Specimen <- s
      PathogenName <- pathogen
      AbTargets <- a
      r <- data.frame(Year, Specimen, PathogenName, AbTargets, p_IVW, SE_IVW, CI_lower, CI_upper)
      
      # r <- data.frame(Year, Specimen, PathogenName, AbTargets, p_IVW, SE_IVW, CI_lower, CI_upper,
      #                 med_IVW = weighted_median, Q1_IVW = weighted_percentile_25,
      #                 Q3_IVW = weighted_percentile_75)
      ra <- rbind(ra, r)
      
      print(a)
    }
    
    rates <- rbind(rates, ra)
    print(s)
  }
  
  # Round the results
  rates[, c("p_IVW", "SE_IVW", "CI_lower", "CI_upper")] <- round(rates[, c("p_IVW", "SE_IVW", "CI_lower", "CI_upper")], 4)
  rates[, c("p_IVW", "CI_lower", "CI_upper")] <- rates[, c("p_IVW", "CI_lower", "CI_upper")] * 100
  
  return(data.frame(rates))
}


# Use Bayesian regression for estimating AMR rates
################################################################

# Function to identify all unique levels for each factor to generate a list of dataframes
# that ensures that all levels are present in each of the different datasets for each
# of the drug bug combinations. This as otherwise brm_multiple does not work
# Hence the model does not need to compile each time.
# 
# get_unique_levels <- function(datasets) {
#   combined_levels <- list()
#   
#   for (df in datasets) {
#     for (col in names(df)) {
#       if (is.character(df[[col]])) {
#         combined_levels[[col]] <- union(combined_levels[[col]], unique(df[[col]]))
#       }
#     }
#   }
#   
#   return(combined_levels)
# }
# 
# # Function to apply unique levels to each factor column
# apply_unique_levels <- function(df, combined_levels) {
#   for (col in names(df)) {
#     if (is.character(df[[col]])) {
#       df[[col]] <- factor(df[[col]], levels = combined_levels[[col]])
#     }
#   }
#   return(df)
# }


# Function to create adjacency matrix for all years
# Counts 1 when countries are neighbours, 0 otherwise
create_combined_adj_matrix <- function(data, world_data) {
  # Get unique countries across all years
  all_countries <- unique(data$Iso3)
  # Filter world data
  world <- world_data %>% filter(iso_a3_eh %in% all_countries)
  
  # Convert to spatial polygons
  world_sp <- as(world, "Spatial")
  
  # Compute neighboring countries
  neighbors <- poly2nb(world_sp)
  
  # Create adjacency matrix
  adj_matrix <- nb2mat(neighbors, style = "B", zero.policy = TRUE)
  rownames(adj_matrix) <- colnames(adj_matrix) <- world$iso_a3_eh
  
  return(adj_matrix)
}

# Function to expand adjacency matrix to match the dataset structure (i.e. by age- and sex)
expand_adj_matrix <- function(adj_matrix, data) {
  # Extract unique combinations of country, year, age category, and sex
  unique_combinations <- data %>%
    select(Iso3, Year, AgeCat10, Sex) %>%
    distinct() %>%
    arrange(Iso3, Year, AgeCat10, Sex)  # Ensure ordering
  
  # Number of unique combinations
  num_combinations <- nrow(unique_combinations)
  
  # Create an expanded matrix with zeros
  expanded_matrix <- Matrix(0, nrow = num_combinations, ncol = num_combinations, sparse = TRUE)
  colnames(expanded_matrix) = unique_combinations$Iso3
  rownames(expanded_matrix) = unique_combinations$Iso3
  
  # Get the row and column names of the adjacency matrix
  row_names <- rownames(adj_matrix)
  col_names <- colnames(adj_matrix)
  
  # Populate the expanded matrix
  for (i in seq_len(nrow(adj_matrix))) {
    #print(i)
    row_iso3 <- row_names[i]
    neighbors <- which(adj_matrix[i, ] == 1)
    
    for (neighbor in neighbors) {
      col_iso3 <- col_names[neighbor]
      
      # Get the indices in the expanded matrix
      row_indices <- which(unique_combinations$Iso3 == row_iso3)
      col_indices <- which(unique_combinations$Iso3 == col_iso3)
      
      # Set values to 1 for neighbors
      expanded_matrix[row_indices, col_indices] <- 1
    }
  }
  
  return(expanded_matrix)
}


# Define the function to fit the model
# get_fit_model <- function(model_fit) {
#   
#   # Extract posterior samples for country-specific effects using as_draws_matrix
#   post_samples <- as_draws_matrix(model_fit)
#   
#   # Extract the overall intercept
#   intercept_samples <- post_samples[, "b_Intercept"]
#   
#   # Extract country-specific random effects
#   country_effects <- post_samples[, grep("^r_Iso3\\[", colnames(post_samples))]
#   
#   # Calculate the log-odds for each country by adding the overall intercept to the random effects
#   log_odds <- sweep(as.matrix(country_effects), 1, intercept_samples, FUN = "+")
#   
#   # Transform log-odds to probabilities
#   resistance_rates <- plogis(log_odds)
#   
#   # summarise the resistance rates
#   resistance_rates_summary <- apply(resistance_rates, 2, quantile, probs = c(0.025, 0.5, 0.975))
#   
#   # Convert the summary to a dataframe
#   resistance_rates_df <- as.data.frame(t(resistance_rates_summary))
#   colnames(resistance_rates_df) <- c("low", "med50", "high")
#   resistance_rates_df$Country <- rownames(resistance_rates_df)
#   
#   # Return the model fit and the resistance rates summary as a dataframe
#   return(list(fit = fit, summary = resistance_rates_df))
# }
# 
# # Model with age and sex
# # Extract posterior predictions for each combination of country, age, and sex
# get_posterior_predictions <- function(model_fit, population_data, Year) {
#   
#   # Create a new dataset that includes all combinations of country, age, and sex
#   new_data <- population_data %>%
#     distinct(Iso3, Year, AgeCat10, Sex, InterpretableAST, Resistant, TotalPopulation) 
#   
#   # Get posterior samples for each combination
#   posterior_samples <- posterior_epred(model_fit, newdata = new_data, re_formula = NULL)
#   
#   # Average the predictions over the posterior samples
#   new_data$predicted_median <- apply(posterior_samples, 2, median)
# #  new_data$predicted_low <- apply(posterior_samples, 2, quantile(probs = 0.025))
#   
#   # Standardize predictions using population data
#   new_data = new_data %>%
#   mutate(
#     exp_resistant = predicted_median * TotalPopulation
#   )
#   new_data
#   return(new_data)
# }


# Plot Model AMR rates by drug bug combination
################################################################################################
plot_model_AMRdb_region <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50*100, col=WHORegionCode)) +
    geom_errorbar(aes(ymin = low25*100, ymax = high75*100), size=2, width = 0.5) +  # Error bars
    geom_point(size = 4) +  # Points
    geom_point(aes(x=AntibioticName, y=rawAMRmed50*100), size = 4, col="black",alpha=0.2, shape=17) +  # Points
    geom_errorbar(aes(ymin = rawAMR25*100, ymax = rawAMR75*100), alpha = 0.2, size=2,width=0.5, col="black",
                  linetype=1) +
    #geom_point(aes(x=AntibioticName, y=median/100), size = 4, col="red") +
    #geom_errorbar(aes(ymin = Q1/100, ymax = Q3/100), alpha = 0.2, size=2,width=0.5, col="red") +
    scale_color_manual(values = brewer.pal(7, "Set1")) +  # Custom color scale
    labs(
      title = paste0("Model estimates: ", model_estimates$PathogenName, " - ",
                     model_estimates$Specimen),
      x = " ",
      y = "Resistance % - Median [IQR]"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = "black"),  # X-axis label color
      axis.title = element_text(size = 12, color = "black"),  # Axis title styling
      plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
      axis.text = element_text(size = 10),  # Axis text size
      panel.grid.major = element_line(color = "grey80"),  # Major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = "black")  # Background styling
    ) +
    facet_wrap(. ~ WHORegionCode, ncol=7) + 
    coord_flip()
}


# Plot AMR rates by Specimen and pathogen
#######################################################################
plot_model_AMRdb <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50, col = Total)) +
  geom_errorbar(aes(ymin = low25, ymax = high75), alpha=0.2, width = 0.2, size=2) +  # Error bars
  geom_point(size = 4) +  # Points
  scale_color_manual(values = brewer.pal(3, "Set1")) +  # Custom color scale
  labs(
    title = paste0("Model estimates: ", model_estimates$PathogenName," - ", model_estimates$Specimen),
    x = " ",
    y = "Resistance % \n Median [IQR]"
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    axis.text.x = element_text(color = "black"),  # X-axis label color
    axis.title = element_text(size = 12, color = "black"),  # Axis title styling
    plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
    axis.text = element_text(size = 10),  # Axis text size
    panel.grid.major = element_line(color = "grey80"),  # Major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = "black")  # Background styling
  ) +
  facet_wrap(.~Specimen, ncol=2) + 
  coord_flip()
}

plot_model_AMRdb_withdata <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50*100)) +
    geom_point(aes(x=AntibioticName, y=median), size = 4, col="darkblue") +
    geom_errorbar(aes(ymin = Q1, ymax = Q3), alpha = 0.2, size=2,width=0.5, col="darkblue") +
    geom_point(aes(x=AntibioticName, y=rawAMRmed50*100), size = 4, col="black") +  # Points
    geom_errorbar(aes(ymin = rawAMR25*100, ymax = rawAMR75*100), alpha = 0.2, size=2,width=0.5, col="black",
                  linetype=1) +
    geom_errorbar(aes(ymin = low25*100, ymax = high75*100), alpha=0.5, size=2, width = 0.5, col="red") +  # Error bars
    geom_point(size = 4, col="red") +  # Points
    #scale_color_manual(values = brewer.pal(3, "Set1")) +  # Custom color scale
    labs(
      title = paste0("Model estimates (All countries): "),
      subtitle = paste0("Red = Model; \nBlack = Data all; \nBlue = Data 75th perc. countries"),
      x = " ",
      y = "Resistance % - Median [IQR]"
    ) +
    theme_minimal() +
    theme(
      legend.position="none",
      axis.text.x = element_text(color = "black"),  # X-axis label color
      axis.title = element_text(size = 12, color = "black"),  # Axis title styling
      plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
      axis.text = element_text(size = 10),  # Axis text size
      panel.grid.major = element_line(color = "grey80"),  # Major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = "black")  # Background styling
    ) +
    facet_wrap(. ~ Specimen, ncol=2) + 
    coord_flip()
}

plot_model_AMRdb_withdata_bar <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50*100)) +
    # Bar plot for model estimates
    geom_bar(aes(x = AntibioticName, y = med50*100), stat = "identity", fill = "red", alpha = 0.7) + 
    # Error bars for the model estimates
    geom_errorbar(aes(ymin = low25*100, ymax = high75*100), alpha = 0.5, size=2, width = 0.5, col="red") +
    
    # Bar plot for raw AMR data
    geom_bar(aes(x = AntibioticName, y = rawAMRmed50*100), stat = "identity", fill = "black", alpha = 0.5) + 
    # Error bars for raw AMR data
    geom_errorbar(aes(ymin = rawAMR25*100, ymax = rawAMR75*100), alpha = 0.2, size=2, width=0.5, col="black", linetype=1) +
    
    # Bar plot for other data (75th percentile countries)
    geom_bar(aes(x = AntibioticName, y = median), stat = "identity", fill = "darkblue", alpha = 0.5) + 
    # Error bars for 75th percentile countries
    geom_errorbar(aes(ymin = Q1, ymax = Q3), alpha = 0.2, size=2, width=0.5, col="darkblue") +
    
    labs(
      title = paste0("Model estimates (All countries): "),
      subtitle = paste0("Red = Model; \nBlack = Data all; \nBlue = Data 75th perc. countries"),
      x = " ",
      y = "Resistance % - Median [IQR]"
    ) +
    theme_minimal() +
    theme(
      legend.position="none",
      axis.text.x = element_text(color = "black"),  # X-axis label color
      axis.title = element_text(size = 12, color = "black"),  # Axis title styling
      plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
      axis.text = element_text(size = 10),  # Axis text size
      panel.grid.major = element_line(color = "grey80"),  # Major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = "black")  # Background styling
    ) +
    facet_wrap(. ~ Specimen, ncol=2) + 
    coord_flip()  # Flip coordinates for horizontal bars
}

# MODEL FUNCTIONS
#-----------------------------------------------------------------------

# Function to plot prior and posterior
plot_prior_posterior <- function(prior_samples, posterior_samples, param_names) {
  
  # Loop through each parameter and plot
  for (param_name in param_names) {
    
    # Combine prior and posterior samples for the current parameter
    plot_data <- bind_rows(
      prior_samples %>% select(!!param_name) %>% mutate(Type = "Prior"),
      posterior_samples %>% select(!!param_name) %>% mutate(Type = "Posterior")
    )
    
    # Calculate summary statistics for prior and posterior
    pi <- plot_data %>% filter(Type == "Prior") %>% 
      summarise(median = median(!!sym(param_name)),
                min = min(!!sym(param_name)),
                max = max(!!sym(param_name)),
                q_low = quantile(!!sym(param_name), prob=0.05),
                q_high = max(!!sym(param_name), prob=0.95))
    
    po <- plot_data %>% filter(Type == "Posterior") %>% 
      summarise(median = median(!!sym(param_name)),
                min = min(!!sym(param_name)),
                max = max(!!sym(param_name)),
                q_low = quantile(!!sym(param_name), prob=0.05),
                q_high = max(!!sym(param_name), prob=0.95))
    
    # Print the summary statistics
    cat("\nParameter:", param_name, "\n")
    cat("Prior Summary: Median =", pi$median, " CI_low =", pi$q_low, " CI_high =", pi$q_high, "\n")
    cat("Posterior Summary: Median =", po$median, " CI_low =", po$q_low, " CI_high =", po$q_high, "\n")
    cat("Posterior Logit Transformation: ", plogis(po$median)," CI_low =", plogis(po$q_low), " CI_high =", plogis(po$q_high), "\n")
    cat("Posterior Exponential Transformation: ", exp(po$median)," CI_low =", exp(po$q_low), " CI_high =", exp(po$q_high), "\n")
    
    # Plot density for the parameter
    p <- ggplot(plot_data, aes_string(x = param_name, fill = "Type", color = "Type")) +
      geom_density(alpha = 0.4) +
      xlim(pi$min-1, pi$max+1) +
      theme_minimal() +
      labs(
        title = paste("Prior vs Posterior for", param_name),
        x = "Parameter Value",
        y = "Density"
      )
    
    return(p)
  }
}

# Function to get number of divergent results - including warm up
n_divergent_iwarmup <- function(x, warm) {
  stopifnot(is(x, "brmsfit"))
  out <- lapply(x$fit@sim$samples, function(y)
    sum(attr(y, "sampler_params")[["divergent__"]]))
  
  lapply(x$fit@sim$samples, function(y)
    print(length(attr(y, "sampler_params")[["divergent__"]])))
  sum(unlist(out))
}

n_divergent <- function(x) {
  stopifnot(is(x, "brmsfit"))
  warmup <- x$fit@sim$warmup2  # This returns a vector with warmup iterations for each chain
  out <- mapply(function(y, w) {
    # Extract the divergent__ parameter only for post-warmup samples of each chain
    divergent_post_warmup <- attr(y, "sampler_params")[["divergent__"]][(w + 1):length(attr(y, "sampler_params")[["divergent__"]])]
    sum(divergent_post_warmup)
  }, x$fit@sim$samples, warmup)
  
  sum(out)
}



# FIGURE FUNCTIONS
# --------------------------------------------------------------------------------

# Figure 4.1

plot_pathogen_antibiotic <- function(df, pathogens, specimen, custom_labels, custom_title = "", palette = NULL) {
  
  # Filter the data for the specified pathogens
  filtered_df <- df %>% filter(PathogenName %in% pathogens, Specimen==specimen)
  
  # Create dummy rows
  dummy_rows <- filtered_df %>%
    group_by(PathogenName, Region) %>%
    mutate(AntibioticName = " ",  # Blank AntibioticName
           Year = 2022,           # Default Year
           drug_bug = "",         # Blank drug_bug
           Specimen = "",         # Blank Specimen
           Grouping = "",         # Blank Grouping
           median = NA,           # NA for median
           Q2.5 = NA,             # NA for Q2.5
           Q97.5 = NA)            # NA for Q97.5
  
  # Combine original data with dummy rows
  combined_df <- bind_rows(filtered_df, dummy_rows)
  
  region_labels <- c(
    "African Region" = "African\nRegion",
    "Region of the Americas" = "Region of the \nAmericas",
    "South-East Asia Region" = "South-East\nAsia Region",
    "European Region" = "European\nRegion",
    "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
    "Western Pacific Region" = "Western\nPacific Region",
    "Global" = "Global"
  )
  
  # Dynamically modify AntibioticName based on pathogen list and custom labels
  df2 <- combined_df %>%
    mutate(AntibioticName = factor(AntibioticName)) %>%  # Convert to factor if not already
    mutate(AntibioticName2 = map2_chr(AntibioticName, PathogenName, ~ if (.x == " " & .y %in% pathogens) {
      paste0(glue("<b><i>{custom_labels[which(pathogens == .y)]}</i></b>"),"                  ")
    } else {
      as.character(.x)
    }))
  
  
  # Set factor levels for AntibioticName2
  df3 <- df2 %>%
    mutate(
      AntibioticName2 = factor(AntibioticName2, 
                               levels = c(custom_labels, 
                                          setdiff(unique(df2$AntibioticName2), custom_labels)))
    )
  
  # Create a custom labeller to hide PathogenName in facet labels
  # custom_labeller <- labeller(
  #   Grouping = label_value,    # Show only the Grouping labels
  #   PathogenName = function(labels) { "" }  # Hide PathogenName labels
  # )
  # 
  custom_labeller <- labeller(
    Grouping = label_value,    # Show only the Grouping labels
    Region = as_labeller(region_labels),
    PathogenName = function(labels) { "" } 
  )
  
  # Plot
  p <- ggplot(df3, aes(x = fct_rev(AntibioticName2), y = median * 100, fill = Region,alpha = n_cutoff)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
    geom_errorbar(aes(ymin = Q2.5 * 100, ymax = Q97.5 * 100), width = 0.2, position = position_dodge(0.7)) +  
    facet_grid(PathogenName + Grouping ~ Region, scales = "free_y", switch = "y", space = "free",
               labeller = custom_labeller) +  
    coord_flip() +
    scale_alpha_manual(values = c(0.2, 0.7)) +  # Define alpha values for discrete levels
    scale_fill_manual(values = palette) +  # Use the passed palette
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits=c(0,100)) +  
    theme(
      plot.background = element_rect(
        fill = "white",
        colour = "white"),
      title = element_text(size = 20),
      axis.text.x = element_text(size = 14),
      #axis.text.y = element_text(size = 18),  
      axis.text.y = ggtext::element_markdown(size = 18),
      strip.text.y.left = element_text(size = 18, angle = 0, vjust = 1.01, hjust = 1),  
      strip.text = element_text(size = 20),   
      strip.text.y = element_blank(),   
      strip.placement = "outside",            
      strip.background = element_blank(),      
      panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "none"
    ) +
    labs(
      x = "",
      y = "Percentage resistant (%)",
      title = custom_title
    ) +
    guides(alpha = "none")
  
  return(p)
}


# CTA FOREST PLOT PREVALENCE
#--------------------------------------------------------------

# Plot function for individual CTA prevalence estimates in 2022
#----------------------------------------------------------------
CTA_w_prev_plot <- function(w_pred, drug_bug, obdata) {
  
  drug_bug <- gsub("Co trimoxazole", "Co-trimoxazole", drug_bug)
  drug_bug <- gsub("Staphylococcus aureus-Methicillin Resistance", "Staphylococcus aureus-Methicillin-resistance", drug_bug)
  drug_bug <- gsub("Escherichia coli-Third generation cephalosporins", "Escherichia coli-Third-generation cephalosporins", drug_bug)
  
  drug_bug_name <- gsub("BLOOD-", "", drug_bug)
  
  d <- obdata %>% filter(Year == 2022, combined == drug_bug, InterpretableAST > 10)
  Iso3_2022 <- unique(d$Iso3)
  
  # Creating subtitle as a character to avoid errors
  subtitle <- if (drug_bug_name == "Escherichia coli-Third-generation cephalosporins") {
    as.character(bquote("3rd-gen cephalosporins resistant " ~ italic("E. coli")))
  } else if (drug_bug_name == "Staphylococcus aureus-Methicillin-resistance") {
    as.character(bquote("Methicillin-resistant " ~ italic("Staphylococcus aureus")))
  } else {
    drug_bug_name
  }
  
  ggplot(w_pred %>% filter(Year == 2022, Iso3 %in% Iso3_2022), 
         aes(x = reorder(Iso3, w_prev), y = w_prev * 100, col = WHORegionCode)) +
    geom_point(aes(y = amr_rate * 100), shape = 18, color = "grey", size = 3) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = w_prev_lower * 100, ymax = w_prev_upper * 100), alpha = 0.2, size = 2, width = 0.5, linetype = 1) +
    theme_minimal() +
    scale_shape_manual(name = "", values = c(17, 19)) +
    scale_color_manual(name = " ", values = c(palette5)) +
    scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
    labs(
      title = "",
      subtitle = subtitle,
      y = "Percentage resistant (%)",
      x = ""
    ) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
      axis.text.y = element_text(size = 15),
      title = element_text(size = 20),
      legend.box = "horizontal",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15)
    ) +
    guides(size = FALSE, color = FALSE)
}

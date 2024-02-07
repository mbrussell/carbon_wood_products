# Smith et al. 2006, Table 4

calc_hwp <- function(volume_harvested, forest_type){

  # Northeast region factors
  p_sfwd_round_vol = 0.948; sfwd_saw_round_vol = 0.991;
  sfwd_pulp_round_vol = 3.079; p_hdwd_round_vol = 0.879;
  hdwd_saw_round_vol= 0.927; hdwd_pulp_round_vol = 2.177

  # Forest type-specific factors
  if(forest_type == "Aspen - birch"){
    p_sfwd = 0.247; p_sfwd_saw = 0.439; p_hdwd_saw = 0.330;
    sg_sfwd = 0.353; sg_hdwd = 0.428}
  else if(forest_type == "Elm - ash - cottonwood"){
    p_sfwd = 0.047; p_sfwd_saw = 0.471; p_hdwd_saw = 0.586;
    sg_sfwd = 0.358;  sg_hdwd = 0.47}
  else if(forest_type == "Maple - beech - birch"){
    p_sfwd = 0.132; p_sfwd_saw = 0.604; p_hdwd_saw = 0.526;
    sg_sfwd = 0.369;  sg_hdwd = 0.518}
  else if(forest_type == "Oak - hickory"){
    p_sfwd = 0.039; p_sfwd_saw = 0.706; p_hdwd_saw = 0.667;
    sg_sfwd = 0.388;  sg_hdwd = 0.534}
  else if(forest_type == "Oak - pine"){
    p_sfwd = 0.511; p_sfwd_saw = 0.777; p_hdwd_saw = 0.545;
    sg_sfwd = 0.371;  sg_hdwd = 0.516}
  else if(forest_type == "Spruce - fir"){
    p_sfwd = 0.87; p_sfwd_saw = 0.508; p_hdwd_saw = 0.301;
    sg_sfwd = 0.353;  sg_hdwd = 0.481}
  else if(forest_type == "White - red - jack pine"){
    p_sfwd = 0.794; p_sfwd_saw = 0.72; p_hdwd_saw = 0.429;
    sg_sfwd = 0.361;  sg_hdwd = 0.51}
  else {carbon <- 0}

  # Step 1 (Example 4 in Smith et al., page 10)
  c_sfwd_saw = volume_harvested * p_sfwd * p_sfwd_saw * sg_sfwd*0.5;
  c_sfwd_pulp = volume_harvested * p_sfwd * (1-p_sfwd_saw) * sg_sfwd*0.5;
  c_hdwd_saw = volume_harvested * (1-p_sfwd) * p_hdwd_saw * sg_hdwd*0.5;
  c_hdwd_pulp = volume_harvested * (1-p_sfwd) * (1-p_hdwd_saw) *
    sg_hdwd*0.5;

  # Step 2 (Example 4 in Smith et al., page 10)
  c_sfwd_saw_round = c_sfwd_saw*p_sfwd_round_vol*sfwd_saw_round_vol;
  c_sfwd_pulp_round = c_sfwd_pulp*p_sfwd_round_vol*sfwd_pulp_round_vol;
  c_hdwd_saw_round = c_hdwd_saw*p_hdwd_round_vol*hdwd_saw_round_vol;
  c_hdwd_pulp_round = c_hdwd_pulp*p_hdwd_round_vol*hdwd_pulp_round_vol

  return(tibble(c_sfwd_saw_round, c_sfwd_pulp_round,
                c_hdwd_saw_round, c_hdwd_pulp_round))
}

# Smith et al. 2006, Table 8

fraction_in_use <- tribble(
  ~Year_after_prod, ~`Softwood lumber`, ~`Hardwood lumber`, ~`Softwood plywood`,
  ~`Oriented strandboard`, ~`Nonstructural panels`, ~`Miscellaneous products`,
  ~`Paper/pulp`,
  0, 1, 1, 1, 1, 1, 1, 1,
  1, 0.973, 0.938, 0.976, 0.983, 0.969, 0.944, 0.845,
  2, 0.947, 0.882, 0.952, 0.967, 0.939, 0.891, 0.713,
  3, 0.922, 0.831, 0.930, 0.952, 0.911, 0.841, 0.603,
  4, 0.898, 0.784, 0.909, 0.937, 0.883, 0.794, 0.509,
  5, 0.875, 0.741, 0.888, 0.922, 0.857, 0.749, 0.430,
  6, 0.854, 0.701, 0.869, 0.908, 0.832, 0.707, 0.360,
  7, 0.833, 0.665, 0.850, 0.895, 0.808, 0.667, 0.299,
  8, 0.813, 0.631, 0.832, 0.881, 0.785, 0.630, 0.243,
  9, 0.795, 0.600, 0.815, 0.869, 0.763, 0.595, 0.192,
  10, 0.777, 0.571, 0.798, 0.856, 0.741, 0.561, 0.149,
  11, 0.760, 0.545, 0.782, 0.844, 0.721, 0.530, 0.115,
  12, 0.743, 0.520, 0.767, 0.832, 0.701, 0.500, 0.088,
  13, 0.728, 0.497, 0.752, 0.821, 0.683, 0.472, 0.068,
  14, 0.712, 0.476, 0.738, 0.810, 0.665, 0.445, 0.052,
  15, 0.698, 0.456, 0.724, 0.799, 0.647, 0.420, 0.040,
  16, 0.684, 0.438, 0.711, 0.789, 0.630, 0.397, 0.030,
  17, 0.671, 0.421, 0.698, 0.778, 0.614, 0.375, 0.023,
  18, 0.658, 0.405, 0.685, 0.768, 0.599, 0.354, 0.018,
  19, 0.645, 0.389, 0.673, 0.759, 0.584, 0.334, 0.013,
  20, 0.633, 0.375, 0.662, 0.749, 0.569, 0.315, 0.009,
  21, 0.622, 0.362, 0.650, 0.740, 0.555, 0.297, 0.006,
  22, 0.611, 0.349, 0.639, 0.731, 0.542, 0.281, 0.005,
  23, 0.600, 0.337, 0.629, 0.722, 0.529, 0.265, 0.004,
  24, 0.589, 0.326, 0.619, 0.713, 0.517, 0.250, 0.003,
  25, 0.579, 0.316, 0.609, 0.705, 0.505, 0.236, 0.002,
  26, 0.569, 0.306, 0.599, 0.697, 0.493, 0.223, 0.002,
  27, 0.560, 0.296, 0.589, 0.689, 0.482, 0.210, 0.001,
  28, 0.551, 0.287, 0.580, 0.681, 0.471, 0.198, 0.001,
  29, 0.542, 0.278, 0.571, 0.673, 0.460, 0.187, 0.001,
  30, 0.533, 0.270, 0.563, 0.666, 0.450, 0.177, 0.001,
  31, 0.525, 0.263, 0.554, 0.658, 0.440, 0.167, 0.000,
  32, 0.517, 0.255, 0.546, 0.651, 0.431, 0.157, 0.000,
  33, 0.509, 0.248, 0.538, 0.644, 0.421, 0.149, 0.000,
  34, 0.501, 0.241, 0.530, 0.637, 0.412, 0.140, 0.000,
  35, 0.494, 0.235, 0.522, 0.630, 0.404, 0.132, 0.000,
  36, 0.487, 0.229, 0.515, 0.623, 0.395, 0.125, 0.000,
  37, 0.480, 0.223, 0.508, 0.617, 0.387, 0.118, 0.000,
  38, 0.473, 0.217, 0.500, 0.610, 0.379, 0.111, 0.000,
  39, 0.466, 0.211, 0.493, 0.604, 0.372, 0.105, 0.000,
  40, 0.459, 0.206, 0.487, 0.598, 0.364, 0.099, 0.000,
  41, 0.453, 0.201, 0.480, 0.592, 0.357, 0.094, 0.000,
  42, 0.447, 0.196, 0.474, 0.586, 0.350, 0.088, 0.000,
  43, 0.441, 0.191, 0.467, 0.580, 0.343, 0.083, 0.000,
  44, 0.435, 0.187, 0.461, 0.574, 0.337, 0.079, 0.000,
  45, 0.429, 0.183, 0.455, 0.568, 0.330, 0.074, 0.000,
  46, 0.423, 0.178, 0.449, 0.563, 0.324, 0.070, 0.000,
  47, 0.418, 0.174, 0.443, 0.557, 0.318, 0.066, 0.000,
  48, 0.413, 0.170, 0.437, 0.552, 0.312, 0.063, 0.000,
  49, 0.407, 0.166, 0.432, 0.546, 0.306, 0.059, 0.000,
  50, 0.402, 0.163, 0.426, 0.541, 0.301, 0.056, 0.000,
  55, 0.378, 0.146, 0.401, 0.516, 0.275, 0.042, 0.000,
  60, 0.356, 0.131, 0.377, 0.493, 0.252, 0.031, 0.000,
  65, 0.336, 0.119, 0.356, 0.471, 0.232, 0.023, 0.000,
  70, 0.318, 0.108, 0.336, 0.450, 0.214, 0.018, 0.000,
  75, 0.301, 0.098, 0.318, 0.431, 0.198, 0.013, 0.000,
  80, 0.286, 0.090, 0.301, 0.413, 0.183, 0.010, 0.000,
  85, 0.271, 0.082, 0.286, 0.395, 0.170, 0.007, 0.000,
  90, 0.258, 0.075, 0.271, 0.379, 0.159, 0.006, 0.000,
  95, 0.246, 0.069, 0.258, 0.364, 0.148, 0.004, 0.000,
  100, 0.234, 0.064, 0.245, 0.349, 0.138, 0.003, 0.000
) %>%
  pivot_longer(`Softwood lumber`:`Paper/pulp`, 
               names_to = "wood_product_type", 
               values_to = "prop_in_use")

# Smith et al. 2006, Table 9 

fraction_landfills <- tribble(
  ~Year_after_prod, ~`Softwood lumber`, ~`Hardwood lumber`, ~`Softwood plywood`,
  ~`Oriented strandboard`, ~`Nonstructural panels`, ~`Miscellaneous products`, 
  ~`Paper/pulp`,
  0, 0, 0, 0, 0, 0, 0, 0,
  1, 0.018, 0.041, 0.016, 0.011, 0.021, 0.037, 0.051,
  2, 0.035, 0.078, 0.032, 0.021, 0.040, 0.072, 0.093,
  3, 0.051, 0.111, 0.046, 0.032, 0.059, 0.104, 0.128,
  4, 0.067, 0.141, 0.060, 0.041, 0.076, 0.134, 0.155,
  5, 0.081, 0.168, 0.073, 0.050, 0.093, 0.163, 0.178,
  6, 0.094, 0.193, 0.085, 0.059, 0.108, 0.189, 0.196,
  7, 0.107, 0.215, 0.096, 0.068, 0.123, 0.213, 0.211,
  8, 0.119, 0.235, 0.107, 0.076, 0.137, 0.236, 0.225,
  9, 0.130, 0.254, 0.118, 0.084, 0.151, 0.257, 0.236,
  10, 0.141, 0.270, 0.128, 0.091, 0.163, 0.277, 0.245,
  11, 0.151, 0.285, 0.137, 0.098, 0.176, 0.296, 0.251,
  12, 0.161, 0.299, 0.146, 0.105, 0.187, 0.313, 0.254,
  13, 0.170, 0.312, 0.155, 0.112, 0.198, 0.329, 0.255,
  14, 0.178, 0.323, 0.163, 0.118, 0.208, 0.344, 0.255,
  15, 0.187, 0.334, 0.171, 0.124, 0.218, 0.357, 0.253,
  16, 0.194, 0.344, 0.178, 0.130, 0.227, 0.370, 0.251,
  17, 0.202, 0.352, 0.185, 0.136, 0.236, 0.382, 0.248,
  18, 0.209, 0.361, 0.192, 0.142, 0.245, 0.393, 0.245,
  19, 0.215, 0.368, 0.199, 0.147, 0.253, 0.403, 0.242,
  20, 0.222, 0.375, 0.205, 0.152, 0.261, 0.413, 0.239,
  21, 0.228, 0.381, 0.211, 0.157, 0.268, 0.422, 0.235,
  22, 0.234, 0.387, 0.217, 0.162, 0.275, 0.430, 0.232,
  23, 0.239, 0.392, 0.222, 0.167, 0.282, 0.438, 0.228,
  24, 0.245, 0.397, 0.227, 0.171, 0.288, 0.445, 0.224,
  25, 0.250, 0.402, 0.233, 0.176, 0.294, 0.451, 0.221,
  26, 0.255, 0.406, 0.238, 0.180, 0.300, 0.457, 0.218,
  27, 0.259, 0.410, 0.242, 0.184, 0.306, 0.463, 0.214,
  28, 0.264, 0.414, 0.247, 0.188, 0.311, 0.468, 0.211,
  29, 0.268, 0.417, 0.251, 0.192, 0.316, 0.473, 0.209,
  30, 0.272, 0.421, 0.256, 0.196, 0.321, 0.477, 0.206,
  31, 0.276, 0.424, 0.260, 0.200, 0.326, 0.481, 0.203,
  32, 0.280, 0.426, 0.264, 0.204, 0.330, 0.485, 0.200,
  33, 0.284, 0.429, 0.268, 0.207, 0.335, 0.488, 0.198,
  34, 0.287, 0.432, 0.272, 0.211, 0.339, 0.491, 0.196,
  35, 0.291, 0.434, 0.275, 0.214, 0.343, 0.494, 0.194,
  36, 0.294, 0.436, 0.279, 0.217, 0.347, 0.497, 0.191,
  37, 0.298, 0.438, 0.282, 0.221, 0.350, 0.499, 0.189,
  38, 0.301, 0.440, 0.286, 0.224, 0.354, 0.502, 0.187,
  39, 0.304, 0.442, 0.289, 0.227, 0.357, 0.504, 0.186,
  40, 0.307, 0.444, 0.292, 0.230, 0.361, 0.506, 0.184,
  41, 0.310, 0.446, 0.295, 0.233, 0.364, 0.507, 0.182,
  42, 0.312, 0.447, 0.298, 0.236, 0.367, 0.509, 0.181,
  43, 0.315, 0.449, 0.301, 0.239, 0.370, 0.510, 0.179,
  44, 0.318, 0.450, 0.304, 0.241, 0.373, 0.512, 0.178,
  45, 0.320, 0.452, 0.307, 0.244, 0.376, 0.513, 0.176,
  46, 0.323, 0.453, 0.309, 0.247, 0.378, 0.514, 0.175,
  47, 0.325, 0.454, 0.312, 0.249, 0.381, 0.515, 0.174,
  48, 0.328, 0.456, 0.315, 0.252, 0.384, 0.516, 0.173,
  49, 0.330, 0.457, 0.317, 0.255, 0.386, 0.516, 0.172,
  50, 0.332, 0.458, 0.320, 0.257, 0.388, 0.517, 0.171,
  55, 0.343, 0.463, 0.331, 0.269, 0.399, 0.520, 0.166,
  60, 0.352, 0.468, 0.342, 0.280, 0.408, 0.521, 0.162,
  65, 0.361, 0.472, 0.351, 0.290, 0.417, 0.521, 0.160,
  70, 0.369, 0.475, 0.360, 0.300, 0.424, 0.521, 0.157,
  75, 0.376, 0.478, 0.368, 0.309, 0.430, 0.521, 0.156,
  80, 0.382, 0.481, 0.375, 0.317, 0.436, 0.521, 0.154,
  85, 0.389, 0.483, 0.382, 0.325, 0.441, 0.520, 0.153,
  90, 0.395, 0.486, 0.388, 0.333, 0.446, 0.519, 0.152,
  95, 0.400, 0.488, 0.394, 0.340, 0.450, 0.519, 0.152,
  100, 0.405, 0.490, 0.400, 0.347, 0.454, 0.518, 0.151,
) %>%
  pivot_longer(`Softwood lumber`:`Paper/pulp`, 
               names_to = "wood_product_type", 
               values_to = "prop_in_landfill")

server <- function(input, output) {
  
  # Reactive values to store updated values
  updated_values <- reactive({
    forest_type <- input$forest_type
    wood_product <- input$wood_product
    units <- input$units
    cuft_cum <- 0.0283168
    bf_cuft <- 83.33
    bf_cords <- 0.5
    cords_grtons <- 2.5
    volume_harvested <- ifelse(units == "Cubic feet", input$volume_harvested*cuft_cum,
                               ifelse(units == "Thousand board feet (MBF)", input$volume_harvested*bf_cuft*cuft_cum,
                                      ifelse(units == "Cords", input$volume_harvested*bf_cuft*cuft_cum*bf_cords,
                                             (input$volume_harvested*bf_cuft*cuft_cum*bf_cords)/cords_grtons)))

    ne_roundwood <- calc_hwp(forest_type = forest_type,
                             volume_harvested = volume_harvested) |>
      pivot_longer(c_sfwd_saw_round:c_hdwd_pulp_round,
                   names_to = "pool",
                   values_to = "c_round") |>
      summarize(sum(c_round)) |>
      pull()
    return(data.frame(ne_roundwood))

  })
  
  updated_values2 <- reactive({
    
    wood_product <- input$wood_product
    
    fractions_summ <- inner_join(fraction_in_use, fraction_landfills, 
                                  by = c("Year_after_prod", "wood_product_type")) %>%
       mutate(prop_atmosphere = 1-(prop_in_use + prop_in_landfill),  
              c_in_use = updated_values()$ne_roundwood * prop_in_use,
              c_landfill = updated_values()$ne_roundwood * prop_in_landfill,
              c_atmosphere = updated_values()$ne_roundwood * prop_atmosphere) %>%
       filter(Year_after_prod > 0 &
                wood_product_type == wood_product) %>%
       group_by(Year_after_prod, wood_product_type) %>%
       summarize(sum_c_in_use = sum(c_in_use),
                 sum_c_landfill = sum(c_landfill),
                 sum_c_atmosphere = sum(c_atmosphere)) 
    
  }) 
  
  updated_values3 <- reactive({
    
    wood_product <- input$wood_product
    
    fractions_long <- inner_join(fraction_in_use, fraction_landfills, 
                                 by = c("Year_after_prod", "wood_product_type")) %>%
      mutate(prop_atmosphere = 1-(prop_in_use + prop_in_landfill),  
             c_in_use = updated_values()$ne_roundwood * prop_in_use,
             c_landfill = updated_values()$ne_roundwood * prop_in_landfill,
             c_atmosphere = updated_values()$ne_roundwood * prop_atmosphere) %>%
      select(-c(prop_in_use, prop_in_landfill, prop_atmosphere)) %>% 
      pivot_longer(c_in_use : c_atmosphere, names_to = "pool", values_to = "carbon") %>% 
      mutate(pool2 = ifelse(pool == "c_in_use", "Products in use", 
                            ifelse(pool == "c_landfill", "Landfill",
                                   "Atmosphere"))) %>% 
      filter(Year_after_prod %in% seq(5, 100, by = 5),
             wood_product_type == wood_product)
    
    
  })
  
  updated_values4 <- reactive({
    
    wood_product <- input$wood_product
    
    half_life <- fraction_in_use %>% 
      select(Year_after_prod, wood_product_type, prop_in_use) %>%
      filter(wood_product_type == wood_product,
             prop_in_use <= 0.50)
  })
  
  updated_values_clean <- reactive({
    forest_type <- input$forest_type
    wood_product <- input$wood_product
    
    fractions_long <- inner_join(fraction_in_use, fraction_landfills, 
                                 by = c("Year_after_prod", "wood_product_type")) %>%
      mutate(prop_atmosphere = 1-(prop_in_use + prop_in_landfill),  
             c_in_use = updated_values()$ne_roundwood * prop_in_use,
             c_landfill = updated_values()$ne_roundwood * prop_in_landfill,
             c_atmosphere = updated_values()$ne_roundwood * prop_atmosphere) %>%
      select(-c(prop_in_use, prop_in_landfill, prop_atmosphere)) %>% 
      pivot_longer(c_in_use : c_atmosphere, names_to = "pool_name", 
                   values_to = "carbon") %>% 
      filter(wood_product_type == wood_product,
             Year_after_prod >= 1) |>
      mutate(pool2 = ifelse(pool_name == "c_in_use", "Products in use", 
                            ifelse(pool_name == "c_landfill", "Landfill",
                                   "Atmosphere")),
             carbon = round(carbon*(44/12)*1.1, 4),
             forest_type = forest_type,
             proportion = round(carbon/sum(carbon), 4), .by = Year_after_prod) |> 
      rename(co2_eq_metric_tonnes = carbon,
             pool = pool2,
             year_after_prod = Year_after_prod) |> 
      select(-pool_name) |> 
      relocate(forest_type, wood_product_type, pool, year_after_prod, 
               co2_eq_metric_tonnes, proportion)
  })
  
  output$main_plot <- renderPlot({
    p_in_use <- ggplot(updated_values2(), aes(x = Year_after_prod,
                                              y = sum_c_in_use*(44/12),
                                              col = wood_product_type)) +
      geom_line(linewidth = 1.5) +
      scale_y_continuous(limits = c (0, max(updated_values2()$sum_c_in_use*(44/12)*1.1))) +
      labs(x = " ",
           y = bquote(atop(~CO[2] ~'-equivalent' , 
                           "(metric tonnes)")),
           col = " ",
           title = "In use") +
      theme_bw() +
      scale_color_brewer(palette = "Dark2") +
      theme(legend.position = "none",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 14),
            title = element_text(size = 12))
    
    p_landfill <- ggplot(updated_values2(), aes(x = Year_after_prod, 
                                          y = sum_c_landfill*(44/12), 
                                          col = wood_product_type)) +
   geom_line(linewidth = 1.5) +
   labs(x = "Years since processing",
        y = " ",
        col = " ",
        title = "In landfills") +
   scale_y_continuous(limits = c (0, max(updated_values2()$sum_c_in_use*(44/12)*1.1))) +
   theme_bw() +
   scale_color_brewer(palette = "Dark2") +
   theme(legend.position = "none",
         axis.text = element_text(size = 10),
         axis.title = element_text(size = 14),
         title = element_text(size = 12))
 
 p_atmosphere <- ggplot(updated_values2(), aes(x = Year_after_prod, 
                                            y = sum_c_atmosphere*(44/12), 
                                            col = wood_product_type)) +
   geom_line(linewidth = 1.5) +
   scale_y_continuous(limits = c (0, max(updated_values2()$sum_c_in_use*(44/12)*1.1))) +
   labs(x = " ",
        y = " ",
        col = " ",
        title = "To atmosphere") +
   theme_bw() +
   scale_color_brewer(palette = "Dark2") +
   theme(legend.text = element_text(size = 15),
         axis.text = element_text(size = 10),
         axis.title = element_text(size = 12),
         title = element_text(size = 12))
 
 p_prop <- ggplot(updated_values3(), aes(x = factor(Year_after_prod),
                                      y = carbon,
                                      fill = pool2)) +
   geom_bar(position = "fill", stat = "identity", col = "black") +
   labs(x = "Years since processing",
        y = "Proportion \nof carbon",
        col = " ",
        title = "Fate of harvested wood products") +
   scale_fill_brewer(palette = "Greens") +
   geom_hline(yintercept = c(0.25, 0.50, 0.75),  col = "black", linewidth = 0.5)+
   theme_bw() +
   scale_x_discrete(breaks = seq(10, 100, by = 10)) +
   theme(legend.title = element_blank(),
         strip.background.x = element_blank(),
         legend.text = element_text(size = 15),
         strip.text = element_text(size = 15),
         axis.text = element_text(size = 10),
         axis.title = element_text(size = 14),
         title = element_text(size = 12))

   (p_in_use + p_landfill + p_atmosphere) / 
     p_prop
})

  output$text_forest_type <- renderText({
    paste("<b>Forest type:</b> ", input$forest_type)
  })
  
  output$text_wood_product <- renderText({
    paste("<b>Wood product:</b> ", input$wood_product)
  })
  
  output$text_half_life <- renderText({
    paste("<b>Half-life for products in use: </b>", updated_values4()[1,1], "years")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('wood-products-carbon-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(updated_values_clean(), con)
    }
  )
}


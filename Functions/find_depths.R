# Calculates sensor depths from pressure transducer readings and offset file.
# Edited: 04 Nov 2025 - switched offset file to long format, simplified arguments.

find_depths <- function(data_file,    # data frame or path to CSV (EDI L1 format)
                        depth_offset, # data frame or path to sensor offset CSV
                        output,       # path to save output, or NULL
                        round_digits = 2,
                        bin_width = 0.25,
                        wide_data = F) {

  if(is.character(data_file)){
    data <- readr::read_csv(data_file, show_col_types = F)
  }else{
    data <- data_file
  }
  
  if(is.character(depth_offset)){
    depth <- readr::read_csv(depth_offset, show_col_types = F)
    
  }else{
    depth <- depth_offset
    
  }
  
  depth <- depth|>
    mutate(
      Offset_end = ymd_hms(ifelse(Offset_end == "current", as.character(Sys.time()),Offset_end)))
  
  #pivot to long and merge offsets to assign depths to each sensor reading
  long <- data |>
    dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
                  starts_with("Depth"),
                  starts_with("Ther"),
                  starts_with("RDO"),
                  starts_with("Lvl")) |>
    dplyr::rename("Depth_m" = contains("Depth")) |>
    tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime", "Depth_m")),
                        names_to = "variable",
                        values_to = "observation",
                        values_drop_na = FALSE) |>
    tidyr::separate_wider_delim(cols = variable,
                                names = c("variable","units","Position"),
                                delim = "_") |>
    dplyr::mutate(Position = as.numeric(Position))
  
  by <- join_by(between(x$DateTime, y$Offset_start, y$Offset_end), Position, Reservoir, Site)
  long_w_depth  <- full_join(long, depth, by)
  
  
  # The pressure sensor was moved to be in line with the bottom thermistor. The top two thermistors had slid closer to each other
  # and were re-secured about a meter a part from each other. Because of this we need to filter before 2021-04-05 13:20:00 EST
  # and after. The top two thermistors exact offset will have to be determined again when the water level is high enough again.
  
  cuts <- tibble::tibble(cuts = as.integer(factor(seq(0,
                                                      ceiling(max(long$Depth_m, na.rm = T)),
                                                      0.25))),
                         depth_bin = seq(0,
                                         ceiling(max(long$Depth_m, na.rm = T)), 0.25))
  
  
  long_depth <- long_w_depth |>
    dplyr::mutate(sensor_depth = Depth_m-Offset) |>
    dplyr::mutate(rounded_depth = round(sensor_depth,
                                        round_digits),
                  cuts = cut(sensor_depth,
                             breaks = cuts$depth_bin,
                             include.lowest = T, right = F, labels = F)) |>
    dplyr::left_join(cuts, by = 'cuts')
  if(wide_data ==T){
    
    final_Temp <- long_depth |>
      dplyr::mutate(observation = ifelse(sensor_depth<0, NA, observation)) |>
      tidyr::pivot_wider(id_cols =  c(Reservoir, Site, DateTime), names_from = c("variable", "units", "Position"),
                         names_sep = "_",
                         values_from = "observation",
                         values_fill = NA)
    
    oth_sensors <- data|>
      select(any_of(c("Reservoir", "Site", "DateTime")),
             starts_with("EXO"),
             starts_with("Flag"),
             contains("Depth"),
             starts_with("RECORD"),
             starts_with("CR"))
    
    final_depths <- merge(final_Temp,oth_sensors, by=(c("Reservoir", "Site", "DateTime"))) |>
      select(colnames(data))
    
    
  } else{
    final_depths <- long_depth |>
      dplyr::filter(!is.na(observation)) |>
      dplyr::filter(!is.na(sensor_depth)) |>
      dplyr::mutate(Depth_m = round(Depth_m, round_digits)) |>
      dplyr::select(Reservoir, Site, Depth_m,
                    DateTime, variable,
                    Position, observation, sensor_depth,
                    rounded_depth,
                    depth_bin)
    
  }
  
  
  
  if(!is.null(output)){
    write.csv(final_depths, output, row.names = FALSE)
  }
  
  return(final_depths)
  
}
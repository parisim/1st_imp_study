################################################################################
######################### Open OpenFace files function #########################
################################################################################

open_open_face = function(participant_id_range_scz, participant_id_range_hc) {
  
  # Create empty lists to store the data frames
  file_data_of_scz = list()
  file_data_of_hc = list()
  
  # Define the base data path (Assuming DAT_PATH is already defined globally)
  if (!exists("DAT_PATH")) {
    stop("DAT_PATH is not defined. Please define the data path.")
  }
  
  # Loop through the participant ID scz 
  for (i in seq_along(participant_id_range_scz)) {
    participant_id_scz = participant_id_range_scz[i]
    path_scz = file.path(DAT_PATH, "scz", "OF")
    file_name_scz = paste0(participant_id_scz, ".csv")
    
    # Read the data file
    file_path_scz = file.path(path_scz, file_name_scz)
    if (file.exists(file_path_scz)) {
      participant_files_data_scz = read.csv(file_path_scz)
      # Add the participant's data frame to the list
      file_data_of_scz[[participant_id_scz]] = participant_files_data_scz
    } else {
      warning("File not found: ", file_path_scz)
    }
  }
  
  # Loop through the participant ID hc 
  for (i in seq_along(participant_id_range_hc)) {
    participant_id_hc = participant_id_range_hc[i]
    path_hc = file.path(DAT_PATH, "hc", "OF")
    file_name_hc = paste0(participant_id_hc, ".csv")
    
    # Read the data file
    file_path_hc = file.path(path_hc, file_name_hc)
    if (file.exists(file_path_hc)) {
      participant_files_data_hc = read.csv(file_path_hc)
      # Add the participant's data frame to the list
      file_data_of_hc[[participant_id_hc]] = participant_files_data_hc
    } else {
      warning("File not found: ", file_path_hc)
    }
  }
  
  # Return the lists of data frames
  return(list(scz = file_data_of_scz, hc = file_data_of_hc))
}

# quarto does not allow to create output files in a subfolder
# subfolders are required to host more content within a project on github
# we perform a small hack to make it happen!
# Below function creates the output in the same folder as the input
# and plays nicely with relative directories!
my_quarto_render <- function(input_file, ...){
  
  # will create tmp_quarto.html in the current directory
  # then will move it to the input directory and name it index.html
  # name index.html is required for github publishing process
  
  tmp_output_file <- file.path("tmp_quarto.html")
  output_file <- file.path(dirname(input_file), "index.html")
  quarto::quarto_render(input_file, output_file = tmp_output_file, ...)
  file.rename(tmp_output_file, output_file)
  output_file
}

# Function to extract title from a .qmd file
extract_title <- function(file_path) {
  # Read the first few lines to locate YAML block
  lines <- readr::read_lines(file_path)
  
  # Extract the YAML content (lines between "---")
  yaml_block <- lines[which(lines == "---")[1] + 1 : which(lines == "---")[2] - 1]
  
  # Parse YAML and extract the title field
  yaml_content <- yaml::yaml.load(paste(yaml_block, collapse = "\n"))
  return(yaml_content$title)
}

# create dashboard indexing qmd file
create_index_qmd_file <- function(quarto_folder, template_qmd, hosting_link) {
  
  # add "/" at the end of the hosting link
  hosting_link <- sub("/?$", "/", hosting_link)
  
  # find all qmd files in sub folders
  qmd_files <- list.files(path = quarto_folder, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
  qmd_files <- qmd_files[dirname(qmd_files) != quarto_folder]
  
  # Extract titles from all qmd files
  titles <- sapply(qmd_files, extract_title)
  
  # extract links for qmd files
  hosted_folders <- sub(paste0(".*/", basename(quarto_folder),"/"), "", dirname(qmd_files))
  links <- paste0(hosting_link, basename(quarto_folder), "/", hosted_folders)
  
  # Ensure the titles and links are of the same length
  stopifnot(length(titles) == length(links))
  
  template_content <- readLines(template_qmd)   # Read the template and copy its content
  file_conn <- file(file.path(quarto_folder, "index.qmd"), "w")  # Open a connection to write to the output file
  writeLines(template_content, file_conn) # Write the template content to the new file
  
  writeLines("", file_conn)
  for (i in seq_along(titles)) {   # Add the links dynamically
    line <- paste0("- [", titles[i], "](", links[i], ")")
    writeLines(line, file_conn)
  }
  
  close(file_conn)    # Close the file connection
}

quarto_folder <- file.path(here::here(), "quarto_dashboards")
template_qmd <- file.path(quarto_folder, "template_index.txt")
hosting_link <- "https://datascientistnz.github.io/edb_dashboards/"

#######################
# could detect and run them all at once...
# my_quarto_render(file.path(
#   here::here(), "quarto_dashboards/network_utilisation/network_utilisation.qmd"))

# my_quarto_render(file.path(
#   here::here(), "quarto_dashboards/edb_peer_grouping/edb_peer_grouping.qmd"))

# my_quarto_render(file.path(
#   here::here(), "quarto_dashboards/edb_cost_analysis/edb_cost_analysis.qmd"))

# my_quarto_render(file.path(
#   here::here(), "quarto_dashboards/edb_perf_areas/edb_perf_areas.qmd"))


# my_quarto_render(file.path(
#   here::here(), "quarto_dashboards/simple_data_visualisation/simple_data_visualisation.qmd"))

my_quarto_render(file.path(
  here::here(), "quarto_dashboards/vegetation_management/vegetation_management.qmd"))

#######################
create_index_qmd_file(quarto_folder, template_qmd, hosting_link)
my_quarto_render(file.path(quarto_folder, "index.qmd"))


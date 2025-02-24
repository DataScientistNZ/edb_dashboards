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

# my_quarto_render(file.path(
#   here::here(), "quarto_dashboards/network_utilisation/network_utilisation.qmd"))


my_quarto_render(file.path(
  here::here(), "quarto_dashboards/edb_peer_grouping/edb_peer_grouping.qmd"))

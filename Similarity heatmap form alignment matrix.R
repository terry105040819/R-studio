Similarity_Heatmap <- function(similarity_matrix_csv_dir,sample_name,output_dir){
  input <- as.matrix(read.csv(similarity_matrix_csv_dir))
  
  result <- as.ggplot(Heatmap(input,colorRamp2(c(0,50, max(input)), c('#9E3D22','#FFF6ED',"#2B5C8A")),
                              column_title = paste(sample_name,"similarity heatmap"),
                              show_column_dend = F,
                              rect_gp = gpar(col = "white", lwd = 2),
                              column_title_gp = gpar(fontsize = 25),
                              column_names_gp = gpar(fontsize = 20),
                              row_names_gp = gpar(fontsize =0),
                              column_names_rot = 45,
                              width = ncol(input)*unit(7, "cm"),
                              height = nrow(input)*unit(7, "cm"),
                              heatmap_legend_param = list(
                                at = c(0, 50, 100),
                                labels = c("0", "50", "100"),
                                title = "Sequence similarity",
                                legend_height = unit(7, "cm"),
                                legend_width = unit(3,"cm"),
                                title_position = "topleft"
                              ),
                              name = "Similarity", cell_fun = function(j, i, x, y, width, height, fill) 
                              {
                                grid.text(sprintf("%.2f", input[i, j]), x, y, gp = gpar(fontsize = 15))
                              }))
  
  ggsave(filename = paste(sample_name,"similarity_heatmap",".png"),plot = result,device = "png",path = output_dir,scale = 1,width = 1600,height = 900,units = "px",dpi = 100)
}

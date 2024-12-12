random_sampling <- function(sampling_file,output_dir,sampling_loop_times,reads_in_file){
  for (i in 1:sampling_loop_times ){
    Subset <- ''
    Subset[[i]] <- as.list(i)
    Subset[[i]]<- sampling_file[sample(nrow(sampling_file),reads_in_file),]
    write_fasta(sequence_file = test[[i]],pwd = output_dir, file_name = i)
  }}

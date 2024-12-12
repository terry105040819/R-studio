write_fasta <- function(sequence_file,pwd,file_name){
  input <- sequence_file
  seq <- input$sequence
  name <- input$sequence_id
  write.fasta(as.list(seq),name,file.out = paste(pwd,file_name, '.fasta',sep = ''))
}

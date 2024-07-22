write_fasta <- function(Igblastdata,pwd,file_name){
  input <- Igblastdata
  seq <- input$sequence
  name <- input$sequence_id
  write.fasta(as.list(seq),name,file.out = paste(pwd,file_name, '.fasta',sep = ''))
}

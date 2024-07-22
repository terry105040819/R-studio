consensus <- function(seq_dir, thresh1=80, thresh2=10){
  mySequences <- readDNAStringSet(seq_dir)
  A <- DNAStringSet(x = mySequences)
  myFirstAlignment <- msa(A,method = "ClustalOmega",type = "DNA")
  msaConsensusSequence(x = myFirstAlignment,ignoreGaps=TRUE,type = 'upperlower',thresh=c(thresh1, thresh2))
  str_replace_all(string = msaConsensusSequence(myFirstAlignment), pattern = '-', replacement = '')
}

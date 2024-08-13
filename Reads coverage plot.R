coverage_depth_plotting <- function(fasta_file_dir,Sample_name,output_dir){
  fasta_file <- read.fasta(file =fasta_file_dir,as.string = F)
  input_2 <- as.data.frame(do.call(rbind,fasta_file))
  input_2 <- apply(input_2[1:100,], 2, paste , collapse = "")
  result <- as.data.frame(matrix('', nrow = 0, ncol = 5))
  for(i in seq_len(length(input_2))){
    x=0
    for(j in c('a', 't', 'c', 'g', '-'))  {
      x=x+1
      result[i, x] <- str_count(input_2[i], pattern = j)
    }
  }
  colname <- c("a","t","c","g","-")
  colnames(result) <- colname
  result$a <- as.numeric(result$a)
  result$t <- as.numeric(result$t)
  result$c <- as.numeric(result$c)
  result$g <- as.numeric(result$g)
  result$`-` <- as.numeric(result$`-`)
  result$depth <- rowMaxs(as.matrix(result[]))
  result$position <- seq(result$a)
  result$coverage <- result$depth/max(result$depth)
  
  depth_cutoff <- data.frame(xmin=min(result$position),
                             xmax=max(result$position),
                             ymin=c(0,50),
                             ymax=c(50,max(result$depth)),
                             col=c("red","green"))
  coverage_cutoff <- data.frame(xmin=min(result$position),
                                xmax=max(result$position),
                                ymin=c(0,0.5),
                                ymax=c(0.5,max(result$coverage)),
                                col=c("red","green"))
  
  depth_plot <- ggplot(data = result)+
    geom_line(aes(x=position,y=depth))+labs(x="Sequence position(nt)",y="Depth(1x)",title = "Consensus depth")+
    scale_x_continuous(limits = c(0,max(result$position)), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,max(result$depth)), expand = c(0, 0)) +
    scale_fill_identity()+
    theme_classic()+
    gg_theme
  
  coverage_plot <- ggplot(data = result)+
    geom_bar(stat = "identity",width = 1.5,aes(x=position,y=coverage))+labs(x="Sequence position(nt)",y="Coverage",title = "Consensus coverage",caption = "*50% coverage cutoff")+
    geom_rect(data=coverage_cutoff, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=col),alpha=0.1)+
    scale_x_continuous(limits =  c(0,max(result$position)),expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1.0), expand = c(0, 0)) +
    scale_fill_identity()+
    theme_classic()+
    gg_theme
  
  
  final_output_plot <- depth_plot/coverage_plot+
    plot_annotation(tag_levels = "A",
                    title = "Nanopore consensus sequence Coverage/Depth",
                    subtitle = Sample_name,
                    caption = "")&theme(plot.title = element_text(size = 30))&
    theme(plot.subtitle = element_text(size = 25))
  
  ggsave(filename = paste(Sample_name,"coverage_plot",".png"),plot = final_output_plot,device = "png",path = output_dir,scale = 1,width = 3360,height = 1440,units = "px",dpi = 100)
}

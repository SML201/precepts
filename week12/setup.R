load(url("https://github.com/SML201/precepts/raw/master/week12/mnist_sample.RData"))

pca <- function(x, space=c("rows", "columns"), 
                center=TRUE, scale=FALSE) {
  space <- match.arg(space)
  if(space=="columns") {x <- t(x)}
  x <- t(scale(t(x), center=center, scale=scale))
  s <- svd(x)
  loading <- s$u
  colnames(loading) <- paste0("Loading", 1:ncol(loading))
  rownames(loading) <- rownames(x)
  pc <- diag(s$d) %*% t(s$v)
  rownames(pc) <- paste0("PC", 1:nrow(pc))
  colnames(pc) <- colnames(x)
  pve <- s$d^2 / sum(s$d^2)
  if(space=="columns") {pc <- t(pc); loading <- t(loading)}
  return(list(pc=pc, loading=loading, pve=pve))
}

plot_image <- function(image_vector, label=NULL){
  DF <- data.frame(x=rep(1:28, 28), y=-sort(rep(1:28, 28))+28, val=image_vector)
  p <- ggplot(DF, aes(x=x, y=y, fill=image_vector)) + 
	 geom_tile(width=1, height=1) +
	 scale_fill_gradient(low="white", high="black") +
	 coord_fixed(ratio=1) + 
	 theme_void() +
	 geom_segment(aes(x=0, xend=28, y=0, yend=0)) +
	 geom_segment(aes(x=0, xend=28, y=28, yend=28)) +
	 geom_segment(aes(x=0, xend=0, y=0, yend=28)) +
	 geom_segment(aes(x=28, xend=28, y=0, yend=28)) +
	 theme(legend.position="none")
  if(!is.null(label)) {
    p <- p + labs(title=as.character(label))
  }
  p
}


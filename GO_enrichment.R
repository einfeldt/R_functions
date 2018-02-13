## Function to perform enrichment analysis of selected genes against gene universe of all genes in transcriptome
GO.enrichment.GOdata <- function(gene_names, annotation_obj, GO_class){
  GO_data_tmp1 <- data.frame(annotation_obj)
  rownames(GO_data_tmp1) <- annotated_transcriptome$TranscriptName
  write.table(GO_data_tmp1, file=paste(species,"_transcriptome_annotations_GO_tmp.txt",sep=""), sep="\t", row.names=T, col.names=F, quote=F)
  GO_data_tmp2 <- readLines(paste(species,"_transcriptome_annotations_GO_tmp.txt",sep=""))
  GO_data_tmp3 <- gsub("\\]---\\[", ", ", GO_data_tmp2)
  writeLines(GO_data_tmp3, paste(species,"_transcriptome_annotations_GO.txt",sep=""))
  gene_ID2GO <- readMappings(file=paste(species,"_transcriptome_annotations_GO.txt",sep=""))
  file.remove(paste(species,"_transcriptome_annotations_GO_tmp.txt",sep=""))
  file.remove(paste(species,"_transcriptome_annotations_GO.txt",sep=""))
  geneUniverse <- names(gene_ID2GO)
  ## Define genes of interest for enrichment analysis
  geneList <- factor(as.integer(geneUniverse %in% gene_names))
  names(geneList) <- geneUniverse
  ## Create topGOdata object with genes of interest, GO annotations, and GO hierarchy
  new("topGOdata", description=deparse(substitute(gene_names)), ontology=GO_class, allGenes=geneList,  annot=annFUN.gene2GO, gene2GO=gene_ID2GO, nodeSize=5)
}
## Alternatively: function to perform enrichment analysis of selected genes against gene universe of all genes in transcriptome that contain at least one RADtag
GO.enrichment.RADtags.GOdata <- function(gene_names, annotation_obj, GO_class){
  GO_data_tmp1 <- data.frame(annotation_obj)
  rownames(GO_data_tmp1) <- annotated_transcriptome$TranscriptName
  # Standardize gene universe to contain only transcripts with hits to RADtags (to remove potential bias from non-specific transcripts)
  GO_data_tmp1a <- data.frame(GO_data_tmp1[annotated_transcriptome$TranscriptName %in% RADtrans$transcriptome_ID,], row.names=rownames(GO_data_tmp1)[annotated_transcriptome$TranscriptName %in% RADtrans$transcriptome_ID])
  write.table(GO_data_tmp1a, file=paste(species,"_transcriptome_annotations_GO_tmp.txt",sep=""), sep="\t", row.names=T, col.names=F, quote=F)
  # Read file, replace separator of GO terms in each entry
  GO_data_tmp2 <- readLines(paste(species,"_transcriptome_annotations_GO_tmp.txt",sep=""))
  GO_data_tmp3 <- gsub("\\]---\\[", ", ", GO_data_tmp2)
  writeLines(GO_data_tmp3, paste(species,"_transcriptome_annotations_GO.txt",sep=""))
  # Create readmappings object
  gene_ID2GO <- readMappings(file=paste(species,"_transcriptome_annotations_GO.txt",sep=""))
  geneUniverse <- names(gene_ID2GO)
  file.remove(paste(species,"_transcriptome_annotations_GO_tmp.txt",sep=""))
  file.remove(paste(species,"_transcriptome_annotations_GO.txt",sep=""))
  ## Define genes of interest for enrichment analysis
  geneList <- factor(as.integer(geneUniverse %in% gene_names))
  names(geneList) <- geneUniverse
  ## Create topGOdata object with genes of interest, GO annotations, and GO hierarchy
  new("topGOdata", description=deparse(substitute(gene_names)), ontology=GO_class, allGenes=geneList,  annot=annFUN.gene2GO, gene2GO=gene_ID2GO, nodeSize=5)
}
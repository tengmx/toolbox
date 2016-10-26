### combine similar tables with at least one key column and one value comlumn
### input: a list of tables with the same columns except the value column
### output: one table with meta columns and all values columns
## refcol, combcol must be integers; reference column must be keys of tables
combine.samemeta.tables <- function(tablist,reftab=NULL,refcol=1,combcol=2){
    if(is.null(reftab)){
        refrow <- c()
        for(i in seq_along(tablist)){
            refrow <- c(refrow,as.character(tablist[[i]][,refcol]))
        }
        refrow <- unique(refrow)
        meta <- as.data.frame(matrix(NA,length(refrow),ncol(tablist[[1]])-2),stringsAsFactors=FALSE)
        value <- matrix(NA,length(refrow),length(tablist))
        refrowfinishes <- NULL
        for(i in seq_along(tablist)){
            value[match(as.character(tablist[[i]][,refcol]),refrow),i] <- tablist[[i]][,combcol]
            refrowsub <- refrow[!is.na(match(refrow,as.character(tablist[[i]][,refcol]))) & is.na(match(refrow,refrowfinishes))]
            meta[match(refrowsub,refrow),] <- tablist[[i]][match(refrowsub,as.character(tablist[[i]][,refcol])),c(-refcol,-combcol)]
            refrowfinishes <- c(refrowfinishes,refrowsub)
        }
        colnames(meta) <- colnames(tablist[[1]])[c(-refcol,-combcol)]
    }else{
        refrow <- tablist[[reftab]][,refcol]
        meta <- tablist[[reftab]][,c(-refcol,-combcol)]
        value <- matrix(NA,length(refrow),length(tablist))
        for(i in seq_along(tablist)){
            value[,i] <- tablist[[i]][match(refrow,as.character(tablist[[i]][,refcol])),combcol]
        }
    }
    colnames(value)<- names(tablist)
    data.frame(refcol=refrow,meta,value,stringsAsFactors=FALSE)
}

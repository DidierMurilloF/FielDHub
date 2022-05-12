owndataDIAGONALS <- "No"
checks <- 4
lines <- 725
kindExpt <- "SUDC"
lines.d <- lines
sameEntries <- FALSE
getData <- function() {
  Option_NCD <- TRUE
  if (owndataDIAGONALS == "Yes") {
    data_entry <- NULL
    data_entry <- na.omit(data_entry)
    if (ncol(data_entry) < 2) validate("Data input needs at least two Columns with the ENTRY and NAME.")
    data_entry_UP <- data_entry[,1:2]
    colnames(data_entry_UP) <- c("ENTRY", "NAME")
    checksEntries <- as.numeric(data_entry_UP[1:checks,1])
    if (kindExpt == "DBUDC") {
      if (ncol(data_entry) < 3) validate("Data input needs at least three Columns with the ENTRY, NAME and BLOCK.")
      data_entry_UP <- data_entry[,1:3] 
      colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
      if (Option_NCD == TRUE) {
        data_entry1 <- data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ]
        Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
        Block_levels <- na.omit(Block_levels)
        data_dim_each_block <- numeric()
        for (i in Block_levels){ 
          data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
        }
        dim_data <- sum(data_dim_each_block)
        input_blocks <- as.numeric(sort(Block_levels))
        if (any(input_blocks < 1) || any(diff(input_blocks) != 1)) {
          validate("Data input does not fit the requirements!")
        }
        selected <- length(Block_levels)
      }
    }
  }else {
    if (kindExpt != "DBUDC") {
      checks <- as.numeric(checks)
      checksEntries <- 1:checks
      lines <- lines.d
      NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                paste(rep("G", lines), (checks + 1):(lines + checks), sep = ""))
      gen.list <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
      data_entry_UP <- gen.list
      colnames(data_entry_UP) <- c("ENTRY", "NAME")
    }else if (kindExpt == "DBUDC") {
      lines.db <- as.numeric(lines.db)
      checks <- as.numeric(checks)
      checksEntries <- 1:checks
      NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                paste(rep("G", lines.db), (checks + 1):(lines.db + checks), sep = ""))
      data_entry_UP <- data.frame(list(ENTRY = 1:(lines.db + checks),	NAME = NAME))
      blocks <- blocks.db
      if (lines.db != sum(blocks)) shiny::validate('Sum of blocks may be equal to number of lines.')
      data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
      colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
      if (sameEntries) {
        if (any(blocks != blocks[1])) shiny::validate("Blocks should have the same size")
        # Names
        ChecksNames <- paste(rep("CH", checks), 1:checks, sep = "")
        nameLines <- rep(c(paste(rep("G", blocks[1]), (2 + 1):(blocks[1] + 2), sep = "")), times = length(blocks))
        NAMES <- c(ChecksNames, nameLines)
        # Entries
        ChecksENTRIS <- 1:checks
        nameEntries <- rep((checks + 1):(blocks[1] + checks), times = length(blocks))
        ENTRIES <- c(ChecksENTRIS, nameEntries)
        data_entry_UP$NAME <- NAMES
        data_entry_UP$ENTRY <- ENTRIES
      }
      if (Option_NCD == TRUE) {
        data_entry1 <- data_entry_UP[(checks + 1):nrow(data_entry_UP), ]
        Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
        Block_levels <- na.omit(Block_levels)
        data_dim_each_block <- numeric()
        for (i in Block_levels){ 
          data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
        }
        dim_data <- sum(data_dim_each_block)
        selected <- length(Block_levels)
      }
    }
    
  }
  dim_data_entry <- nrow(data_entry_UP)
  dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
  list(data_entry = data_entry_UP, 
       dim_data_entry = dim_data_entry, 
       dim_data_1 = dim_data_1)
}

blocks_length <- function(){
  if (kindExpt == "DBUDC") {
    df <- getData()$data_entry
    Block_levels <- suppressWarnings(as.numeric(levels(as.factor(df$BLOCK))))
    Block_levels <- na.omit(Block_levels)
    len_blocks <- length(Block_levels)
    return(len_blocks)
  } else return(NULL)
}

df <- getData()
df

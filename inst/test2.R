library(FielDHub)
library(blocksdesign)
nt <- 16
r <- 4
k <- 4
b <- nt/k
###  Directly Using blocksdesign ####
mydes <- blocksdesign::blocks(
    treatments = nt, 
    replicates = r, 
    blocks = list(r, b), 
    seed = 10)

mydes$Blocks_model
mydes$

M <- mydes$Design

X <- model.matrix(~treatments - 1, data = M)
X
Z = model.matrix(~Level_2 - 1, data = M)
Z

N = t(X) %*% Z
incidence = N %*% t(N)
incidence

C = r * diag(1, nrow = nt, ncol = nt) - incidence / k

e <- eigen(C / r)

e_values <- 1 / e$values[1:(length(e$values) - 1)]
e_values

E <- (nt - 1) / sum(e_values)
E

### Turn the IBD into a Row-Column Design ###








##### Dropping the cycling REP ######
rep_to_drop <- mydes$Design %>%
    dplyr::group_by(Level_1, Level_2) %>%
    mutate(treatments = as.numeric(treatments)) %>%
    dplyr::summarise(dif = sum(diff(sort(treatments)))/(dplyr::n()-1)) %>%
    dplyr::filter(dif == 1) %>%
    dplyr::pull(Level_1) %>%
    unique()

rep_to_drop
if (length(rep_to_drop) > 0) {
    mydes$Design <- mydes$Design %>%
        dplyr::filter(Level_1 != rep_to_drop) %>%
        mutate(Level_1 = rep(paste0("B", 1:(r - 1)), each = nt))
}
mydes$Design

############### Using FielDHub ###############
library(FielDHub)
library(dplyr)
nt <- 36
r <- 3
sq_lattice <- square_lattice(t = nt, k = 4, r = r, seed = 10)
plot(sq_lattice)

df <- sq_lattice$fieldBook
df

M <- df %>%
    mutate(
        ENTRY = as.factor(ENTRY),
        Level_2 = as.factor(paste0(REP, ".", IBLOCK))
    )
M

head(M, 12)

X <- model.matrix(~ENTRY - 1, data = M)

Z = model.matrix(~Level_2 - 1, data = M)

N = t(X) %*% Z
incidence = N %*% t(N)
incidence

C = r * diag(1, nrow = nt, ncol = nt) - incidence / k

e <- eigen(C / r)

e_values <- 1 / e$values[1:(length(e$values) - 1)]
e_values

E <- (nt - 1) / sum(e_values)
E

##### Dropping the cycling REP ######
rep_to_drop <- df %>%
    dplyr::group_by(REP, IBLOCK) %>%
    dplyr::summarise(dif = sum(diff(sort(ENTRY)))/(dplyr::n()-1)) %>%
    dplyr::filter(dif == 1) %>%
    dplyr::pull(REP) %>%
    unique()

rep_to_drop

new_df <- df %>%
    dplyr::filter(REP != rep_to_drop) %>%
    mutate(REP = rep(1:(r - 1), each = nt))

new_df

M <- new_df %>%
    mutate(
        ENTRY = as.factor(ENTRY),
        Level_2 = as.factor(paste0(REP, ".", IBLOCK))
    )
M

head(M, 12)

X <- model.matrix(~ENTRY - 1, data = M)
X
Z = model.matrix(~Level_2 - 1, data = M)
Z

N = t(X) %*% Z
incidence = N %*% t(N)
incidence

C = r * diag(1, nrow = nt, ncol = nt) - incidence / k

e <- eigen(C / r)

e_values <- 1 / e$values[1:(length(e$values) - 1)]
e_values

E <- (nt - 1) / sum(e_values)
E












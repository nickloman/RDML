library(RDML)

args = commandArgs(TRUE)

mode = args[2]
if(mode == 'mdp') {
  fluor_col = 2
} else {
  fluor_col = 3
}

## read plate
lc96=RDML$new(args[1])
tbl=lc96$AsTable()

## add rowids
tbl=cbind(tbl, rowid=row.names(tbl))

#vals = apply(tbl, 1, get_fluor)

printf <- function(...) invisible(print(sprintf(...)))

printf("pos fluor conc")

position_l = vector()
fluor_l = vector()
conc_l = vector()

for(i in 1:nrow(tbl)) {
     row <- tbl[i,] 

     exp_id = row$exp.id
     run_id = row$run.id
     react_id = row$react.id
     target = row$target

     run <- lc96$experiment[[exp_id]]$run[[run_id]]

     mdp <- run$react[[react_id]]$data[[target]][[mode]]

     fluor = mdp[,fluor_col][1]
# conc = (fluor * 12.280) + (-2.097 * I(fluor ^ 2)) + - 0.395
# simple linear model
     intercept = -0.1297055
     coeff = 10.1640332
     conc = intercept + coeff * fluor

     position_l = c(position_l, row$position)
     fluor_l = c(fluor_l, fluor)
     conc_l = c(conc_l, conc) 
}

df = data.frame(position_l, fluor_l, conc_l)
write.table(df)

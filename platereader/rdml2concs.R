library(RDML)
library(ggplot2)

args = commandArgs(TRUE)

standards_file = args[1]
rdml_file = args[2]

setwd('/Users/nick/work/platereader/RDML/platereader')
#standards_file = 'standards.txt'
#rdml_file = '/Volumes/Untitled/8stds_sybr_template_plate1_repeat.rdml'

standards = read.table(standards_file, sep="\t", header=T)

runmode = args[3]
#runmode = 'adp'
if(runmode == 'mdp') {
  fluor_col = 2
} else {
  fluor_col = 3
}

## read plate
lc96=RDML$new(rdml_file)
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

     mdp <- run$react[[react_id]]$data[[target]][[runmode]]

     fluor = mdp[,fluor_col][1]
     fluor_l = c(fluor_l, fluor)
}

print(fluor_l)
tbl = cbind(tbl, fluor_l)
mergedtable = merge(tbl, standards, by="position")

p = ggplot(mergedtable, aes(x=known, y=fluor_l)) + geom_point() + stat_smooth(method='lm')
ggsave("fitted.png")

fit = lm(known ~ fluor_l, data=mergedtable)
coeffs = coefficients(fit)
intercept = coeffs[1]
coeff = coeffs[2]

position_l = vector()
fluor_l = vector()
conc_l = vector()

write.table(tbl)
for(i in 1:nrow(tbl)) {
     row = tbl[i,]
    # simple linear model
     conc = 2.5 * (intercept + coeff * row$fluor)
 
     position_l = c(position_l, row$position)
     fluor_l = c(fluor_l, row$fluor_l)
     conc_l = c(conc_l, conc) 
}

df = data.frame(position_l, fluor_l, conc_l)
write.table(df)


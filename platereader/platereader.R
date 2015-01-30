install.packages("devtools")
library(devtools)
install_github("RDML", "nickloman")

lc96=RDML$new('/Volumes/Untitled/all_stds.rdml')

lc96=RDML$new("/Volumes/Untitled/single Syber green.rdml")



for(n in seq(from=1, to=96, by=12)) {
print(lc96$experiment$`bd8eecf0-cd20-4a02-a4a3-18968cee8649`$run$`1e040d25-245f-4acf-a2f4-c09276059b85`$react[[n]]$data$`FAM@None`$mdp[,2][1])
}


vals=c(0.0450715, 0.0659107, 0.104788, 0.203832, 0.399549, 0.561353, 0.794199, 1.02655)
concs=c(0,0.5,1,2,4,6,8,10)
df=data.frame(concs, vals)
ggplot(df, aes(x=concs, y=vals)) + geom_point() + geom_smooth(method='lm', formula = y ~ x + I(x^2))
fit <- lm(concs ~ vals + I(vals^2), data=df)
(0.25 * 12.280) + -2.097 * I(0.25 ^ 2)  + -0.395

tbl=lc96$AsTable()
tbl=cbind(tbl, rowid=row.names(tbl))
get_fluor <- function(row) {
 lc96$experiment[[row["exp.id"]]]$run[[row["run.id"]]]$react[[row["react.id"]]]$data[[row["target"]]]$mdp   
}

vals = apply(tbl, 1, get_fluor)

printf <- function(...) invisible(print(sprintf(...)))

for(i in 1:nrow(tbl)) {
  row <- tbl[i,] 
  val = unlist(vals[row$rowid])
print(val)
}

  conc = (val * 12.280) + -2.097 * I(val ^ 2)  + -0.395
  printf("%s %s", conc, row["position"])
}

print(v)
cbind(tbl, "conc" = v)

for(row in tbl) {
  row$exp.id
}

lc96$experiment[[expid]]$run[[runid]]


lc96$experiment$`bd8eecf0-cd20-4a02-a4a3-18968cee8649`$run$`1e040d25-245f-4acf-a2f4-c09276059b85`$react[[n]]$data$`FAM@None`$mdp[,2][1])

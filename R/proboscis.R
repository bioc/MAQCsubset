
proboscis = function(es, site=1, ABp=0.001, CDp=.01,
   mmrad=100) {
 require(genefilter)
 mcall = match.call()
 # assumes samples labeled A, B, C, D as in MAQC
  mm = function(x,rad) {
  # moving mean
    start = ceiling(rad/2)
    stop = floor(length(x)-(rad/2))
    sapply(start:stop, function(i) mean(x[(i-floor(rad/2)):(i+floor(rad/2))]))
   }
 ess = es[,es$site==site]
 essab = ess[, ess$samp %in% c("A", "B")]
 essab$samp = factor(essab$samp)
 esscd = ess[, ess$samp %in% c("C", "D")]
 esscd$samp = factor(esscd$samp)
 tt = rowttests( exprs(essab), essab$samp )
 L = which( tt$p < ABp & tt$dm < 0 )
 R = which( tt$p < ABp & tt$dm > 0 )
 ttcd = rowttests( exprs(esscd), esscd$samp )
 ABL = tt$dm[L]
 CDL = ttcd$dm[L]
 ABR = tt$dm[R]
 CDR = ttcd$dm[R]
 NN = list(ttab=tt,ttcd=ttcd,ABL=sort(ABL),cdokL=1*(CDL<0)[order(ABL)],
  ABR=sort(ABR),dcokR=1*(CDR>0)[order(ABR)])
 `A-B` = c(ONR <- mm(NN$ABL,mmrad), mm(NN$ABR,mmrad))
 `P(SCMT|A-B)` = c(mm(NN$cdokL,mmrad), mm(NN$dcokR,mmrad))
 new("proboStruct", call=mcall,
  list("A-B"=`A-B`, "P(SCMT|A-B)"=`P(SCMT|A-B)`,
  leftinds=1:length(ONR)))
}


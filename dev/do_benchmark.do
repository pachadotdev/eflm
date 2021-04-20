clear all 
global route = "C:\Users\u0133260\Documents\pacha_stata"
cd "${route}"
use "trade_data_yotov"



*model <- "log(trade) ~ log(dist) + cntg + lang + clny + exp_year + imp_year"
*reg log_trade log_dist cntg lang  clny  i.exp_year_num i.imp_year_num  
*glm trade log_dist cntg lang  clny  i.exp_year_num i.imp_year_num  , family(poisson) link(log)


gen log_trade = ln(trade)
gen log_dist  = ln(dist)


encode exp_year , gen(exp_year_num) 
encode imp_year , gen(imp_year_num) 


local counter = 1
levelsof year
*Genero matrix para guardar resultados
matrix S = J(r(r),  3, 999)
foreach i in `r(levels)'  {
*tic
timer on `counter'
*reg
qui reg log_trade log_dist cntg lang  clny  i.exp_year_num i.imp_year_num if year >=`i'
*toc
timer off `counter'
timer list
*guardo resultados
mat S[`counter', 1] = e(N)
mat S[`counter', 2] = e(rank)
mat S[`counter', 3] =r(t`counter')
*up counter
local counter = `counter' + 1
}
timer list
mat li S 





**Clear timer
timer clear 
levelsof year
matrix R = J(r(r),  3, 999)
*re-start counter
local counter = 1
levelsof year
foreach i in `r(levels)'  {
*tic
timer on `counter'
*glm
qui glm trade log_dist cntg lang  clny  i.exp_year_num i.imp_year_num  if year >= `i' , family(poisson) link(log) 
*toc
timer off `counter'
timer list
*guardo resultados
mat R[`counter', 1] = e(N)
mat R[`counter', 2] = e(rank)
mat R[`counter', 3] = r(t`counter')
*up counter
local counter = `counter'+1
}
timer list
mat li R


*save excel
matrix colnames R = "Obs" "Rank"  "Time"
matrix rownames R = "reg log_trade log_dist cntg lang  clny  i.exp_year_num i.imp_year_num "
matrix colnames S = "Obs" "Rank"  "Time"
matrix rownames S = "glm trade log_dist cntg lang  clny  i.exp_year_num i.imp_year_num"
 

putexcel set stata_bench.xlsx, sheet(bechmark) replace
putexcel A2 = "command" 
putexcel A2 = matrix(S) ,names 
putexcel A8 = matrix(R)  ,rownames 


exit 

ivprobit migration (chuzubi=iv) prov rgender logincome_1 age edu han party health if hukou==1 [pweight=weight] 
estimates store m1

ivprobit migration (chuzubi=iv) prov rgender logincome_1 age edu han party  siying health if hukou==1 [pweight=weight] 
estimates store m2

ivprobit migration (chuzubi=iv) prov rgender logincome_1 age edu han party parents siying health if hukou==1 [pweight=weight] 
estimates store m3

esttab m1 m2 m3, scalar(r2 r2_a N F) compress ///
star(* 0.1 ** 0.05 *** 0.01) ///
b(%6.3f) t(%6.3f) ///
mtitles(m1 m2 m3)

logit migration logincome_1 i.age chuzubi  i.edu  rgender i.health2  i.prov  party han if hukou==1 [pweight=weight] ,r
estimates store m1

probit migration logincome_1 i.age chuzubi  i.edu han party rgender i.health2  i.prov  i.hunyin if hukou==1 & age<80 [pweight=weight],r
estimates store m2

probit migration logincome_1 i.age chuzubi  i.edu han party rgender i.health2  i.prov  i.hunyin siying if hukou==1 & age<80 [pweight=weight],r
estimates store m3

probit migration logincome_1 i.age chuzubi  i.edu han party rgender i.health2  i.prov  i.hunyin siying parents if hukou==1 & age<80 [pweight=weight],r
estimates store m4

probit migration logincome_1 i.age chengzubi  i.edu han party rgender i.health2  i.prov  i.hunyin siying if hukou==1 & age<80 [pweight=weight],r
estimates store m5

probit migration logincome_1 i.age chengzubi  i.edu han party rgender i.health2  i.prov  i.hunyin siying parents if hukou==1 & age<80 [pweight=weight],r
estimates store m6

esttab m1 m2 m3 m4 m5 m6, scalar(r2 r2_a N F) compress ///
star(* 0.1 ** 0.05 *** 0.01) ///
b(%6.3f) t(%6.3f) ///
mtitles(m1 m2 m3 m4 m5 m6)

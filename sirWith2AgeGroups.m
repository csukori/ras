(* ::Package:: *)

(* ::Section:: *)
(*Parameters*)


(* ::Text:: *)
(*\[CapitalLambda]: continuous recruitment rate*)
(*d1: death rate in the first age group*)
(*d2: death rate in the second age group*)
(*1/c1: the length of the first age group*)
(*\[Beta]11:  contact rate within the first age group*)
(*\[Beta]12: contact rate of the individuals in the first age group with individuals from the second age group*)
(*\[Beta]21: contact rate of the individuals in the second age group with individuals from the first age group*)
(*\[Beta]22:  contact rate within the second age group*)
(*\[Gamma]1: recovery rate in the first age group*)
(*\[Gamma]2: recovery rate in the second age group*)


(* ::Section:: *)
(*Disease-free steady states*)


(* ::Subsection:: *)
(*Continuous aging*)


(* ::Input:: *)
(*(*aging parameter depending on the death rate d (c_1)*)*)
(*c[d_]:=(d)/(Exp[d]-1);*)


(* ::Input:: *)
(*(*disease-free steady states in the continuous aging model*)*)
(*(*s1$d: the size of the first age group in the disease-free steady state*)*)
(*(*ss$d: the size of the second age group in the disease-free steady state*)*)
(*s1$d[\[CapitalLambda]_,d1_]:=\[CapitalLambda]/(c[d1]+d1);*)
(*s2$d[\[CapitalLambda]_,d1_,d2_]:=c[d1]*s1$d[\[CapitalLambda],d1]/(d2);*)


(* ::Subsection:: *)
(*RAS*)


(* ::Input:: *)
(*(*disease-free steady states in the RAS model*)*)
(*(*s1$h: the size of the first age group in the disease-free steady state*)*)
(*(*ss$h: the size of the second age group in the disease-free steady state*)*)
(*s1$h[t_,\[CapitalLambda]_,d1_]:=(\[CapitalLambda]-\[CapitalLambda] Exp[-d1 t])/d1;*)
(*s2$h[t_,d2_]:=Exp[-d2 t];*)


(* ::Section:: *)
(*R0 for continuous aging*)


(* ::Input:: *)
(*(*The general formula for the basic reproduction number of the continuous aging model depending on the parameters*)*)
(*R0$d[\[Beta]11_,\[Beta]12_,\[Beta]21_,\[Beta]22_,\[Gamma]1_,\[Gamma]2_,d1_,d2_,\[CapitalLambda]_]:=(\[Beta]11 s1$d[\[CapitalLambda],d1]/(c[d1]+\[Gamma]1+d1)+\[Beta]21 s2$d[\[CapitalLambda],d1,d2]/c[d1]+\[Beta]22 s2$d[\[CapitalLambda],d1,d2]/(\[Gamma]2+d2)+Sqrt[(\[Beta]11 s1$d[\[CapitalLambda],d1]/(c[d1]+\[Gamma]1+d1)+\[Beta]21 s2$d[\[CapitalLambda],d1,d2]/c[d1]+\[Beta]22 s2$d[\[CapitalLambda],d1,d2]/(\[Gamma]2+d2)^2-4 (\[Beta]11 \[Beta]22 s1$d[\[CapitalLambda],d1] s2$d[\[CapitalLambda],d1,d2])/((c[d1]+\[Gamma]1+d1)(\[Gamma]2+d2)) )])*)


(* ::Section:: *)
(*R0 for RAS*)


(* ::Input:: *)
(*(*aging matrix*)*)
(*A={{0,0},{1,1}};*)


(* ::Input:: *)
(*(*The solution of the differential equation w'=(-V+F/\[Lambda])w with initial conditions {w1[0]=1,w2[0]=0},*)*)
(*sol10[\[Beta]11_,\[Beta]12_,\[Beta]21_,\[Beta]22_,\[Gamma]1_,\[Gamma]2_,d1_,d2_,\[CapitalLambda]_]:=ParametricNDSolveValue[{w1'[t]==(-d1-\[Gamma]1+(s1$h[t,\[CapitalLambda],d1] \[Beta]11)/\[Lambda]) w1[t]+(s1$h[t,\[CapitalLambda],d1] \[Beta]12 w2[t])/\[Lambda],w2'[t]==(s2$h[t,d2] \[Beta]21 w1[t])/\[Lambda]+(-d2-\[Gamma]2+(s2$h[t,d2] \[Beta]22)/\[Lambda]) w2[t],w1[0]==1,w2[0]==0},{w1,w2},{t,0,2},{\[Lambda]}];*)


(* ::Input:: *)
(*(*The solution of the differential equation w'=(-V+F/\[Lambda])w with initial conditions {w1[0]=0,w2[0]=1},*)*)
(*sol01[\[Beta]11_,\[Beta]12_,\[Beta]21_,\[Beta]22_,\[Gamma]1_,\[Gamma]2_,d1_,d2_,\[CapitalLambda]_]:=ParametricNDSolveValue[{w1'[t]==(-d1-\[Gamma]1+(s1$h[t,\[CapitalLambda],d1] \[Beta]11)/\[Lambda]) w1[t]+(s1$h[t,\[CapitalLambda],d1] \[Beta]12 w2[t])/\[Lambda],w2'[t]==(s2$h[t,d2] \[Beta]21 w1[t])/\[Lambda]+(-d2-\[Gamma]2+(s2$h[t,d2] \[Beta]22)/\[Lambda]) w2[t],w1[0]==0,w2[0]==1},{w1,w2},{t,0,2},{\[Lambda]}];*)


(* ::Input:: *)
(*(*The matrix built from the solutions*)*)
(*(*Note, that for simplicity the solutions are now the rows of the matrix instead of columns as used in the article.*)*)
(*solMtx[\[Lambda]_,\[Beta]11_,\[Beta]12_,\[Beta]21_,\[Beta]22_,\[Gamma]1_,\[Gamma]2_,d1_,d2_,\[CapitalLambda]_]:=*)
(*{{(sol10[\[Beta]11,\[Beta]12,\[Beta]21,\[Beta]22,\[Gamma]1,\[Gamma]2,d1,d2,\[CapitalLambda]][\[Lambda]$p]/.\[Lambda]$p->\[Lambda]//Evaluate)[[1]][1], (sol10[\[Beta]11,\[Beta]12,\[Beta]21,\[Beta]22,\[Gamma]1,\[Gamma]2,d1,d2,\[CapitalLambda]][\[Lambda]$p]/.\[Lambda]$p->\[Lambda]//Evaluate)[[2]][1]},*)
(*{(sol01[\[Beta]11,\[Beta]12,\[Beta]21,\[Beta]22,\[Gamma]1,\[Gamma]2,d1,d2,\[CapitalLambda]][\[Lambda]$p]/.\[Lambda]$p->\[Lambda]//Evaluate)[[1]][1], (sol01[\[Beta]11,\[Beta]12,\[Beta]21,\[Beta]22,\[Gamma]1,\[Gamma]2,d1,d2,\[CapitalLambda]][\[Lambda]$p]/.\[Lambda]$p->\[Lambda]//Evaluate)[[2]][1]}};*)


(* ::Input:: *)
(*(*The spectral radius of the matrix A.solMtx*)*)
(*\[Rho][\[Lambda]_,\[Beta]11_,\[Beta]12_,\[Beta]21_,\[Beta]22_,\[Gamma]1_,\[Gamma]2_,d1_,d2_,\[CapitalLambda]_]:=Max[Map[Abs,Eigenvalues[A . solMtx[\[Lambda],\[Beta]11,\[Beta]12,\[Beta]21,\[Beta]22,\[Gamma]1,\[Gamma]2,d1,d2,\[CapitalLambda]]]]];*)


(* ::Section:: *)
(*Examples*)


(* ::Input:: *)
(*(*test values for the parameter \[Lambda]*)*)
(*lambdas=Table[i/100,{i,1,2000}];*)


(* ::Subsection:: *)
(*Case I.*)


(* ::Input:: *)
(*(*parameter values in the first case*)*)
(*\[Beta]11$1=2.7;\[Beta]12$1=6.9;\[Beta]21$1=2.5;\[Beta]22$1=6.1;*)
(*\[Gamma]1$1=3.3;\[Gamma]2$1=0.3;*)
(*d1$1=3.3;d2$1=1;*)
(*\[CapitalLambda]$1=1.3;*)


(* ::Input:: *)
(*(*the reproduction number of the continuous aging model in the first case*)*)
(*R0$d[\[Beta]11$1,\[Beta]12$1,\[Beta]21$1,\[Beta]22$1,\[Gamma]1$1,\[Gamma]2$1,d1$1,d2$1,\[CapitalLambda]$1]*)


(* ::Input:: *)
(*(*Helper graph for finding the reproduction number of the RAS model*)*)
(*(*red line: constant 1 function*)*)
(*(*blue dots: the spectral radius of the matrix A.solMtx with the given value from the lambdas array*)*)
(*(*the reproduction number is where the red line and the blue dots cross each other*)*)
(*Show[ListPlot[Table[{i/100,\[Rho][lambdas[[i]],\[Beta]11$1,\[Beta]12$1,\[Beta]21$1,\[Beta]22$1,\[Gamma]1$1,\[Gamma]2$1,d1$1,d2$1,\[CapitalLambda]$1]},{i,50,500}]],Plot[1,{t,0,5},PlotStyle->Red]]*)


(* ::Input:: *)
(*(*The value of the first parameter that provides 1 as result is the value of the basic reproduction number, that is R0$h=3.27643*)*)
(*\[Rho][3.27643,\[Beta]11$1,\[Beta]12$1,\[Beta]21$1,\[Beta]22$1,\[Gamma]1$1,\[Gamma]2$1,d1$1,d2$1,\[CapitalLambda]$1]*)


(* ::Subsection:: *)
(*Case II.*)


(* ::Input:: *)
(*(*parameter values in the second case*)*)
(*\[Beta]11$2=2.7;\[Beta]12$2=6.9;\[Beta]21$2=2.1;\[Beta]22$2=3.7;*)
(*\[Gamma]1$2=3.3;\[Gamma]2$2=0.3;*)
(*d1$2=3.3;d2$2=1;*)
(*\[CapitalLambda]$2=1.7;*)


(* ::Input:: *)
(*(*the reproduction number of the continuous aging model in the second case*)*)
(*R0$d[\[Beta]11$2,\[Beta]12$2,\[Beta]21$2,\[Beta]22$2,\[Gamma]1$2,\[Gamma]2$2,d1$2,d2$2,\[CapitalLambda]$2]*)


(* ::Input:: *)
(*(*Helper graph for finding the reproduction number of the RAS model*)*)
(*(*red line: constant 1 function*)*)
(*(*blue dots: the spectral radius of the matrix A.solMtx with the given value from the lambdas array*)*)
(*(*the reproduction number is where the red line and the blue dots cross each other*)*)
(*Show[ListPlot[Table[{i/100,\[Rho][lambdas[[i]],\[Beta]11$2,\[Beta]12$2,\[Beta]21$2,\[Beta]22$2,\[Gamma]1$2,\[Gamma]2$2,d1$2,d2$2,\[CapitalLambda]$2]},{i,50,500}]],Plot[1,{t,0,5},PlotStyle->Red]]*)


(* ::Input:: *)
(*(*The value of the first parameter that provides 1 as result is the value of the basic reproduction number, that is R0$h=2.13142*)*)
(*\[Rho][2.13142,\[Beta]11$2,\[Beta]12$2,\[Beta]21$2,\[Beta]22$2,\[Gamma]1$2,\[Gamma]2$2,d1$2,d2$2,\[CapitalLambda]$2]*)


(* ::Subsection:: *)
(*Case III.*)


(* ::Input:: *)
(*(*parameter values in the third case*)*)
(*\[Beta]11$3=2.6;\[Beta]12$3=3;\[Beta]21$3=1.4;\[Beta]22$3=1.2;*)
(*\[Gamma]1$3=0.5;\[Gamma]2$3=0.5;*)
(*d1$3=1.2;d2$3=1.92;*)
(*\[CapitalLambda]$3=1.2;*)


(* ::Input:: *)
(*(*the reproduction number of the continuous aging model in the third case*)*)
(*R0$d[\[Beta]11$3,\[Beta]12$3,\[Beta]21$3,\[Beta]22$3,\[Gamma]1$3,\[Gamma]2$3,d1$3,d2$3,\[CapitalLambda]$3]*)


(* ::Input:: *)
(*(*Helper graph for finding the reproduction number of the RAS model*)*)
(*(*red line: constant 1 function*)*)
(*(*blue dots: the spectral radius of the matrix A.solMtx with the given value from the lambdas array*)*)
(*(*the reproduction number is where the red line and the blue dots cross each other*)*)
(*Show[Plot[1,{t,0,5},PlotStyle->Red],ListPlot[Table[{i/100,\[Rho][lambdas[[i]],\[Beta]11$3,\[Beta]12$3,\[Beta]21$3,\[Beta]22$3,\[Gamma]1$3,\[Gamma]2$3,d1$3,d2$3,\[CapitalLambda]$3]},{i,50,500}]]]*)


(* ::Input:: *)
(*(*The value of the first parameter that provides 1 as result is the value of the basic reproduction number, that is R0$h=0.558438*)*)
(*\[Rho][0.558438,\[Beta]11$3,\[Beta]12$3,\[Beta]21$3,\[Beta]22$3,\[Gamma]1$3,\[Gamma]2$3,d1$3,d2$3,\[CapitalLambda]$3]*)

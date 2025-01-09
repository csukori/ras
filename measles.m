(* ::Package:: *)

(* ::Text:: *)
(*[1]: He Daihai and Earn David J . D . 2016 The cohort effect in childhood disease dynamics . J . R . Soc . Interface .1320160156, http : // doi . org/10.1098/rsif .2016 .0156*)


(* ::Section::Closed:: *)
(*The disease-free periodic solution*)


(* ::Text:: *)
(*n1:  The population size in the first age group*)
(*n2: The population size in the middle age groups*)
(*n2: The population size in the last age groups*)
(*\[Vee]: birth rate (0.02)*)
(*\[Mu]: death rate (0.0)*)


(* ::Text:: *)
(*First, we have to determine the disease-free solution and for that the initial values in each age group. *)
(*n2$0:  the size of the middle age groups at the start of the year*)
(* n3$1: the size of the last age group at the end of the year*)


(* ::Input:: *)
(*DSolve[{n1'[t]== \[Nu] n3[t],n2'[t]==0, n3'[t]== -\[Mu] n3[t], n1[0]==0, n2[0]==n2$0,n3[0]==n2$0+n3$1},{n1,n2,n3},t]*)


(* ::Input:: *)
(*Solve[{(E^-\[Mu] (-1+E^\[Mu]) (n2$0+n3$1) \[Nu])/\[Mu] ==n2$0,20n2$0+n3$1==1},{n2$0,n3$1}]*)


(* ::Input:: *)
(*n2$0 = ((-1+E^\[Mu]) \[Nu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu]);*)
(*n3$1 = (E^\[Mu] \[Mu]+\[Nu]-E^\[Mu] \[Nu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu]);*)


(* ::Input:: *)
(*DSolve[{n1'[t]== \[Nu] n3[t],n2'[t]==0, n3'[t]== -\[Mu] n3[t], n1[0]==0, n2[0]==n2$0,n3[0]==n2$0+n3$1},{n1,n2,n3},t]*)


(* ::Input:: *)
(*n1Test=NDSolve[{n1'[t]== \[Nu] (E^(\[Mu]-t \[Mu]) \[Mu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu])/.\[Nu]->0.02/.\[Mu]->0.02,n1[0]==0,WhenEvent[Mod[t,1]==0,n1[t]->0]},n1,{t,0,100}];*)


(* ::Input:: *)
(*Plot[n1[Mod[t,1]]/.n1Test,{t,0,20}]*)


(* ::Input:: *)
(*n20Test=NDSolve[{n20'[t]== -\[Mu] (E^(\[Mu]-t \[Mu]) \[Mu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu])/.\[Nu]->0.02/.\[Mu]->0.02,n20[0]== \[Mu]/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu])+((-1+E^\[Mu]) \[Nu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu])/.\[Nu]->0.02/.\[Mu]->0.02,WhenEvent[Mod[t,1]==0,n20[t]-> \[Mu]/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu])+((-1+E^\[Mu]) \[Nu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu])/.\[Nu]->0.02/.\[Mu]->0.02]},n20,{t,0,100}];*)


(* ::Input:: *)
(*Plot[n20[Mod[t,1]]/.n20Test,{t,0,20}]*)


(* ::Input:: *)
(*(*The infant, young and 20+ susceptibles*)*)
(*S0[t_]:=(E^(\[Mu]-t \[Mu]) (-1+E^(t \[Mu])) \[Nu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu]);*)
(*Sy[t_]:=((-1+E^\[Mu]) \[Nu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu]);*)
(*S20[t_]:=(E^(\[Mu]-t \[Mu]) \[Mu])/(E^\[Mu] \[Mu]-19 \[Nu]+19 E^\[Mu] \[Nu]);*)


(* ::Input:: *)
(*(*A subtitutional rule to help to build the ODE system from the*)*)
(*diseasefreeSol=Join[{ss[1]->S0[t]},Table[ss[i]->Sy[t],{i,2,20}],{ss[21]->S20[t]}]/.\[Mu]->0.02/.\[Nu]->0.02*)


(* ::Section::Closed:: *)
(*Parameter values*)


(* ::Input:: *)
(*mastervar={s,e,i,r};*)
(*Nagegroups=21;*)


(* ::Input:: *)
(*(*Fixed parameters*)*)
(*\[Nu] = 0.02;(*birth rate*)*)
(*\[Mu] = 0.02; (*death rate*)*)
(*\[Sigma] = 1/(8/365);(*mean latent period 8 days*)*)
(*\[Gamma] = 1/(5/365);(*mean infectious period 5 days*)*)
(*(*Fitted paramters*)*)
(*b0 = 398.6; (*overall transmission factor*)*)
(*b2 = 18.23(*school transmission ratio*)*)


(* ::Subsection:: *)
(*Contact matrix*)


(* ::Input:: *)
(*(*Indicator function describing the school year*)*)
(*schoolYear[t_]:=UnitStep[48/365-t]+(UnitStep[104/365-t]-UnitStep[56/365-t])+(UnitStep[213/365-t]-UnitStep[120/365-t])+(UnitStep[312/365-t]-UnitStep[228/365-t]);*)


(* ::Input:: *)
(*Plot[schoolYear[t],{t,0,1}]*)


(* ::Input:: *)
(*(*parameters describing the contact numbers between the age groups*)*)
(*preSchool =3.375;*)
(*primary=3.915;*)
(*adoloscent=1.755;*)
(*adult=1.08;*)
(*school=19.332;*)


(* ::Input:: *)
(*b22=1.8*3.07*8.76 (*Comes from fitting method performed by [1]]*)*)


(* ::Input:: *)
(*CM0=({*)
(* {preSchool, preSchool, preSchool, preSchool, preSchool, preSchool, primary, primary, primary, primary, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {preSchool, preSchool, preSchool, preSchool, preSchool, preSchool, primary, primary, primary, primary, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {preSchool, preSchool, preSchool, preSchool, preSchool, preSchool, primary, primary, primary, primary, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {preSchool, preSchool, preSchool, preSchool, preSchool, preSchool, primary, primary, primary, primary, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {preSchool, preSchool, preSchool, preSchool, preSchool, preSchool, primary, primary, primary, primary, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {preSchool, preSchool, preSchool, preSchool, preSchool, preSchool, primary, primary, primary, primary, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {primary, primary, primary, primary, primary, primary, school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {primary, primary, primary, primary, primary, primary, school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {primary, primary, primary, primary, primary, primary, school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {primary, primary, primary, primary, primary, primary, school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], school+b22*schoolYear[t], adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adoloscent, adult},*)
(* {adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult, adult}*)
(*});*)


(* ::Input:: *)
(*CM=b0*CM0;*)


(* ::Section::Closed:: *)
(*Variables*)


(* ::Input:: *)
(*Vrul=Table[mastervar[[n]]->V[n],{n,1,Length[mastervar]}]*)


(* ::Input:: *)
(*var=Flatten[Table[mastervar/.Map[#->#[n][t]&,mastervar],{n,1,Nagegroups,1}],1]/.Vrul;*)


(* ::Section::Closed:: *)
(*Construct F and V matrices*)


(* ::Text:: *)
(*F=({*)
(* {Fee, Fei},*)
(* {Fie, Fii}*)
(*})=({*)
(* {0, Fei},*)
(* {0, 0}*)
(*})*)
(*V=({*)
(* {Vee, Vei},*)
(* {Vie, Vii}*)
(*})=({*)
(* {Vee, 0},*)
(* {Vie, Vii}*)
(*})*)


(* ::Input:: *)
(*infectedGroups={e,i};*)


(* ::Input:: *)
(*NulMtx=Table[0,{j,1,Nagegroups},{k,1,Nagegroups}];*)


(* ::Input:: *)
(*(*The change of the latent group due to new infections differentiated respect to the variable i*)*)
(*Fei[t_]:=(Table[s[i][t] CM[[i,j]],{i,1,Nagegroups},{j,1,Nagegroups}]);*)


(* ::Input:: *)
(*(*The columns of the matrix F*)*)
(*frow1[t_]:=Join[NulMtx,NulMtx];*)
(*frow2[t_]:=Join[Fei[t],NulMtx];*)


(* ::Input:: *)
(*(*The matrix F*)*)
(*Fmtx[t_]:=Join[frow1[t],frow2[t],2];*)


(* ::Input:: *)
(*(*The change of the infected groups by any other meening differentiated by the variables e and i*)*)
(*Vee[t_]:=DiagonalMatrix[Table[\[Sigma]+\[Mu],{n,1,Nagegroups}]];*)
(*Vie[t_]:=DiagonalMatrix[Table[-\[Sigma],{n,1,Nagegroups}]];*)
(*Vii[t_]:=DiagonalMatrix[Table[\[Gamma]+\[Mu],{n,1,Nagegroups}]];*)


(* ::Input:: *)
(*(*The columns of the matrix V*)*)
(*vrow1[t_]:=Join[Vee[t],Vie[t]];*)
(*vrow2[t_]:=Join[NulMtx,Vii[t]];*)


(* ::Input:: *)
(*(*The matrix V*)*)
(*Vmtx[t_]:=Join[vrow1[t],vrow2[t],2]*)


(* ::Section:: *)
(*Build the differential equation system with the parameter \[Lambda]*)


(* ::Input:: *)
(*(*The unkown functions in the ode w'(t)=(-V(t)+F(t)/\[Lambda])w(t) *)*)
(*parametricOdeVars = Flatten[Table[{w} /. Map[# -> #[n] &,{ w}], {n, 1, Length[infectedGroups]* Nagegroups, 1}]];*)


(* ::Input:: *)
(*(*A subtitutional rule to help to build the ODE system from the*)*)
(*Srul=Thread[Table[s[i][t],{i,1,Nagegroups,1}]->Table[ss[i]/.diseasefreeSol,{i,1,Nagegroups,1}]];*)


(* ::Input:: *)
(*(*The parametric ODE system to solve*)*)
(*parametricOde=Thread[*)
(*Flatten[Table[{w} /. Map[# -> #[n]'[t] &,{ w}], {n, 1,  Length[infectedGroups]*Nagegroups, 1}]] ==*)
(*       (((-Vmtx[t]+Fmtx[t]/l)/.Srul) . (Flatten[Table[{w} /. Map[# -> #[n][t] &,{ w}], {n, 1, Length[infectedGroups]*Nagegroups, 1}]]))*)
(*  ];*)


(* ::Input:: *)
(*(*The initial conditions to the parametric ODE system*)*)
(*parametricInits =Table[Thread[ *)
(*Flatten[Table[{w} /. Map[# -> #[n][0] &,{ w}], {n, 1,  Length[infectedGroups]*Nagegroups, 1}]] ==IdentityMatrix[ Length[infectedGroups]*Nagegroups][[n]]],{n, 1, Length[infectedGroups]*Nagegroups, 1}];*)


(* ::Input:: *)
(*(*The matrix built from the parametric ODE system with each initial condition*)*)
(*solMtxRel=Table[ParametricNDSolveValue[Join[parametricOde,parametricInits[[j]]],parametricOdeVars,{t,0,2},{l}],{j,1,Length[parametricOdeVars]}];*)


(* ::Input:: *)
(*(*The matrix built from the solutions evaluated at t=1*)*)
(*W[L_]:=Table[Through[solMtxRel[[j]][L][1]],{j,1,Length[solMtxRel]}]*)


(* ::Section:: *)
(*Calculate the basic reproduction number*)


(* ::Input:: *)
(*(*Construct the aging matrix AA*)*)
(*permutation=Table[Mod[j+1,Nagegroups],{j,1,Nagegroups}];*)
(*permutation[[Nagegroups-1]]=Nagegroups;*)
(*A=Transpose[Normal[PermutationMatrix[permutation]]];*)
(*A[[1,Nagegroups]]=0;A[[Nagegroups,Nagegroups]]=1;*)
(*Nul=Table[Table[0,{j,1,Nagegroups}],{k,1,Nagegroups}];*)
(*AA=Join[Join[A,Nul],Join[Nul,A],2];*)


(* ::Input:: *)
(*(*R_0: the value of the \[Lambda] parameter that provides \[Rho](AA.W[\[Lambda]])==1*)*)
(*Max[Re[Eigenvalues[W[17.185047] . Transpose[AA]]//Evaluate]]*)

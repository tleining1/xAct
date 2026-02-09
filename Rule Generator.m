(* ::Package:: *)

IndexCombos=Map[Flatten[#]&,Join[Tuples[{{a,-a},Permutations[{b,-b}],Permutations[{c,-c}]}]]];

NegativeQ[a_]:=MatchQ[a,Times[-1,_]]

PatternAll[a_]:=Module[{neg},
neg=NegativeQ[a];
ReleaseHold[If[neg, Times[-1,Pattern[Evaluate[-1*a],Blank[]]],Pattern[Hold[a],Blank[]]]]
]
(*stress tensor/ no color index/ two free spactime index rules*)
RuleAGP[IndexCombos_]:=Map[With[{Under=Join[Map[PatternAll[#]&,#]]},Gg_[Under[[1]],Under[[2]],Under[[4]]]*Op1_[j__,Under[[5]]]*Op_[Under[[3]]]:>If[Gg==G,epsilong[#[[2]],#[[4]]]CD[#[[3]]]@Op[] CD[#[[5]]]@Op1[j]]
]&,IndexCombos]

RuleGAA[IndexCombos_]:=Map[With[{Under=Join[Map[PatternAll[#]&,#]]},Gg_[Under[[1]],Under[[2]],Under[[4]]]*Op1_[j__,Under[[5]]]*Op_[k__,Under[[3]]]:>If[Gg==G,epsilong[#[[2]],#[[4]]]CD[#[[3]]]@Op[k] CD[#[[5]]]@Op1[j]]
]&,IndexCombos]

RuleDA={CD1_[\[Mu]_]@Op_[\[Nu]_,a_]:>If[Op==A&&CD1==CD,CD1[\[Mu]]@Op[\[Nu]]]}

RuleDP={CD1_[\[Mu]_]@Op_[a_]:>If[Op==\[Phi]&&CD1==CD,CD1[\[Mu]]@Op[]]}

(*conserved current/ 1 color color index/ one free spacetime index*)
RuleGGAAA[\[Mu]_,a_]:=Module[{b,c,d,e,\[Nu],\[Rho],\[Sigma],\[Gamma],\[Delta]},Op_[\[Nu]_,-e_]Gg_[-b_,a,e_]Op_[-\[Nu]_,-d_]Op_[\[Mu],-c_]Gg_[b_,c_,d_]:>If[Gg==G,epsilong[\[Rho],\[Sigma]]CD[-\[Rho]]@Op[\[Nu]]CD[-\[Sigma]]@(epsilong[\[Gamma],\[Delta]]CD[-\[Gamma]]@Op[\[Mu]]CD[-\[Delta]]@Op[-\[Nu]])]]

RuleGADA[\[Mu]_,a_]:=Module[{\[Nu],b,c,\[Rho],\[Sigma]},Op_[\[Nu]_,-c_]Gg_[-b_,a,c_]CD1_[\[Mu]]@Op_[-\[Nu]_,b_]:>If[Gg==G&&CD1==CD,epsilong[\[Rho],\[Sigma]]CD1[-\[Rho]]@Op[\[Nu]]CD1[-\[Sigma]]@CD[\[Mu]]@Op[-\[Nu]]]]

RuleGADA1[\[Mu]_,a_]:=Module[{\[Nu],b,c,\[Rho],\[Sigma]},Op_[\[Nu]_,-c_]Gg_[-b_,a,c_]CD1_[-\[Nu]_]@Op_[\[Mu],b_]:>If[Gg==G&&CD1==CD,epsilong[\[Rho],\[Sigma]]CD1[-\[Rho]]@Op[\[Nu]]CD1[-\[Sigma]]@CD[-\[Nu]]@Op[\[Mu]]]]

RuleGGAPP[\[Mu]_,a_]:=Module[{b,c,d,e,\[Rho],\[Sigma],\[Gamma],\[Delta]},Gg_[b_,a,e_]Gg_[-b_,c_,d_]Op_[\[Mu],-d_]Op1_[-e_]Op1_[-c_]:>epsilong[\[Rho],\[Sigma]]CD[-\[Rho]]@Op1[]CD[-\[Sigma]]@(epsilong[\[Gamma],\[Delta]]CD[-\[Gamma]]@Op1[]CD[-\[Delta]]@Op[\[Mu]])]

RuleGPDP[\[Mu]_,a_]:=Module[{b,c,\[Rho],\[Sigma]},Gg_[b_,a,c_]Op_[-c_]CD1_[\[Mu]]@Op_[-b_]:>If[Gg==G&&CD1==CD,epsilong[\[Rho],\[Sigma]]CD1[-\[Rho]]@Op[]CD1[-\[Sigma]]@CD1[\[Mu]]@Op[]]]

(*General rules*)

RuleGen[IndexCombos_]:=Map[With[{Under=Join[Map[PatternAll[#]&,#]]},Gg_[Under[[1]],Under[[2]],Under[[4]]]*Op1_[j___,Under[[5]]]*Op_[k___,Under[[3]]]:>If[Gg==G,epsilong[#[[2]],#[[4]]]CD[#[[3]]]@Op[k] CD[#[[5]]]@Op1[j]]
]&,IndexCombos]

RuleA={CD1_[\[Mu]_]@Op_[\[Nu]___,a_]:>If[CD1==CD,CD1[\[Mu]]@Op[\[Nu]]]}

(*Rule for mutliple G*)
RuleGG={expr__:>Module[{free,gFree,gIndicies,sharedG,elems,oIndex,outside,A,iIndex,inside},
free=List@@IndicesOf[Free][expr];
gFree=Cases[expr,term_[inds___]/;ContainsAny[{inds},List@@free]&&term==G];
Catch[If[Length[gFree]>1,Throw["Error: too many free indices"],
gIndicies=List@@IndicesOf[G][expr];
sharedG=Flatten[NegSet[Select[gIndicies,MemberQ[#,-#]&]]];
oIndex=Complement[List@@IndicesOf[][gFree],sharedG,free];
outside=gFree[[1]]*Cases[expr,A_[a___,-oIndex[[1]],b___]][[1]];
inside=ToCanonical[(expr/outside)]/.RuleGen[IndexCombos];
iIndex=Complement[sharedG,List@@IndicesOf[][gFree]][[1]];
DefTensor[A[a],ManifoldVariable];
(ToCanonical[outside*A[iIndex]]/.RuleGen[IndexCombos])/.{A[]:>inside}
]
]
]}




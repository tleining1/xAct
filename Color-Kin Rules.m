(* ::Package:: *)

colorNoStruc={expr__:>Module[{fIndicies,dIndicies,colorTerms,otherTerms,kinTerms,cTermIndex},
fIndicies=IndicesOf[Free][expr];
dIndicies=Partition[List@@IndicesOf[Dummy][expr],2];
colorTerms=Map[Module[{term1,term2,term3},
term1=If[Length[Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-#[[1]]]|A_[\[Nu]___,-#[[1]]]]]==0,1,Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-#[[1]]]|A_[\[Nu]___,-#[[1]]]][[1]]];
term2=If[Length[Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-#[[2]]]|A_[\[Nu]___,-#[[2]]]]]==0,1,Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-#[[2]]]|A_[\[Nu]___,-#[[2]]]][[1]]];
{term1,term2}
]&,dIndicies];
otherTerms=(expr/Times@@Flatten[colorTerms])/.{A[\[Mu]_,a_]:>A[\[Mu]],\[Phi][a_]:>\[Phi][]};
cTermIndex=Transpose[{Flatten[colorTerms],Flatten[dIndicies]}];
otherTerms*Times@@Map[#[[1]]/.{CD_[\[Mu]_]@A_[\[Nu]___,-#[[2]]]:>CD[\[Mu]]@A[\[Nu]],A_[\[Nu]___,-#[[2]]]:>A[\[Nu]]}&,cTermIndex]
]};

colorKin4={expr__:>
Module[{Gg,colorTerms,otherTerms,cTermIndex,cTerms},
Gg=Partition[List@@IndicesOf[G][expr],3];
If[Length[Gg]==0,expr/.colorNoStruc,
colorTerms=Map[Module[{term1,term2,term3,gIndex1,gIndex2,gIndex3},
gIndex1=#[[1]];
gIndex2=#[[2]];
gIndex3=#[[3]];
term1=If[Length[Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-gIndex1]|A_[\[Nu]___,-gIndex1]]]==0,1,Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-gIndex1]|A_[\[Nu]___,-gIndex1]][[1]]];
term2=If[Length[Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-gIndex2]|A_[\[Nu]___,-gIndex2]]]==0,1,Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-gIndex2]|A_[\[Nu]___,-gIndex2]][[1]]];
term3=If[Length[Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-gIndex3]|A_[\[Nu]___,-gIndex3]]]==0,1,Cases[expr,CD_[\[Mu]_]@A_[\[Nu]___,-gIndex3]|A_[\[Nu]___,-gIndex3]][[1]]];
{term1,term2,term3}
]&,Gg];
otherTerms=(expr/(Times@@Flatten[colorTerms]*Times@@Flatten[Cases[expr,G[___]]]));
cTermIndex=Transpose[{Flatten[colorTerms],Flatten[Gg]}];
cTerms=Map[{#[[1]]/.{CD_[\[Mu]_]@A_[\[Nu]___,-#[[2]]]:>CD[\[Mu]]@A[\[Nu]],A_[\[Nu]___,-#[[2]]]:>A[\[Nu]]}
,#[[2]]}&,cTermIndex];
otherTerms*cTerms[[1]][[1]]*epsilong[-cTerms[[2]][[2]],-cTerms[[3]][[2]]]CD[cTerms[[2]][[2]]]@cTerms[[2]][[1]] CD[cTerms[[3]][[2]]]@cTerms[[3]][[1]]
]]};

GGRule={expr__:>Module[{X},
If[Complement[IndicesOf[Free][expr],IndicesOf[G][expr]]===IndicesOf[Free][expr],
expr/.Gg_[-a_,-d_,\[Rho]_]Gg_[-b_,-e_,-\[Rho]_]:>If[Gg==G,Module[{\[Sigma]},G[-a,-d,\[Rho]]G[-b,-e,\[Sigma]]]],
expr/.G[a_,-b_,c_]G[-d_,-e_,-c_]:>X[a,-b,-d,-e]/.{X1_[c_,-b_,-a_,-d_]A_[aa___,a_]B_[bb___,b_]D_[dd___,d_]:>
If[X1==X,epsilong[b,c]CD[-b]@B[bb]CD[-c]@(epsilong[a,d]CD[-a]@
A[aa]CD[-d]@D[dd])]}]]};

(* ::Package:: *)

IndexCombos=Map[Flatten[#]&,Join[Tuples[{{a,-a},Permutations[{b,-b}],Permutations[{c,-c}]}]]];

PatternAll[a_]:=Module[{neg},
neg=NegativeQ[a];
ReleaseHold[If[neg, Times[-1,Pattern[Evaluate[-1*a],Blank[]]],Pattern[Hold[a],Blank[]]]]
]

RuleAGP=Map[With[{Under=Join[Map[PatternAll[#]&,#]]},G[Under[[1]],Under[[2]],Under[[4]]]*Op1_[j__,Under[[5]]]*Op_[Under[[3]]]:>epsilong[#[[2]],#[[4]]]CD[#[[3]]]@Op[] CD[#[[5]]]@Op1[j]
]&,IndexCombos]

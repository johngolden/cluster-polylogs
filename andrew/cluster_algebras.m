(* ::Package:: *)

(* ::Input:: *)
(**)


ab[tt__]:=Signature[{tt}] ab@@Sort[{tt}]/;{tt}=!=Sort[{tt}]&&FreeQ[{tt},Pattern]
ab[tt__]:=0/;Length[DeleteDuplicates[{tt}]]<4&&FreeQ[{tt},Pattern]
schouten=ab[i,j,k,l]Z[m]+ab[j,k,l,m]Z[i]+ab[k,l,m,i]Z[j]+ab[l,m,i,j]Z[k]+ab[m,i,j,k]Z[l];
Get["schoutenReplacements[12].mx"];

(* Outputs the conditions under which a matrix is skew-symmetrizable *)

skewSymmetrizable[B_?SquareMatrixQ]:=Reduce[Join[Flatten[Table[dummy[ii]B[[ii,jj]]==-dummy[jj]B[[jj,ii]],{ii,Last[Dimensions[B]]},{jj,Last[Dimensions[B]]}]],Table[dummy[ii]>0,{ii,Last[Dimensions[B]]}],Table[var>0,{var,Variables@Level[B,{-1}]}]],Array[dummy,Last[Dimensions[B]]],Reals]//.Join[{And[tt___,dummy[_]>_]:>And[tt],And[tt___,dummy[_]<_]:>And[tt],And[tt___,dummy[_]>=_]:>And[tt],And[tt___,dummy[_]<=_]:>And[tt],And[tt___,dummy[_]==_]:>And[tt],And[tt___,dummy[_]==_]:>And[tt]},Table[And[tt___,var>0]:>And[tt],{var,Variables@Level[B,{-1}]}]]/.{And[dummy[_]>_]:>True,And[dummy[_]<_]:>True,And[dummy[_]>=_]:>True,And[dummy[_]<=_]:>True,And[dummy[_]==_]:>True,And[dummy[_]==_]:>True}

(* The quiver mutation operator, which acts on the exchange matrix via 
    mutation on index k. These operations can be chained, where indices 
    to the left are mutated on first *)
 
\[Mu][B_?SquareMatrixQ,{k1_,kN__}]:=\[Mu][\[Mu][B,k1],{kN}]
\[Mu][B_?SquareMatrixQ,{k_}]:=\[Mu][B,k]
\[Mu][B_?SquareMatrixQ,k_]:=\[Mu][B,k]=Table[If[ii==k\[Or]jj==k,-B[[ii,jj]],B[[ii,jj]]+If[TrueQ[Simplify[B[[ii,k]]>0,Table[var>0,{var,Variables@Level[B,{-1}]}]]]\[And]TrueQ[Simplify[B[[k,jj]]>0,Table[var>0,{var,Variables@Level[B,{-1}]}]]],B[[ii,k]]*B[[k,jj]],0]-If[TrueQ[Simplify[B[[ii,k]]<0,Table[var>0,{var,Variables@Level[B,{-1}]}]]]\[And]TrueQ[Simplify[B[[k,jj]]<0,Table[var>0,{var,Variables@Level[B,{-1}]}]]],B[[ii,k]]*B[[k,jj]],0]],{ii,First[Dimensions[B]]},{jj,Last[Dimensions[B]]}]

(* Rule to freeze the nodes at a given set of indices *)

freezeNodes[cl[a__][x__][B_?SquareMatrixQ],nodes_]:=Print["cannot freeze nondymaic (or nonexistent) node"]/;Max@@nodes>Length[First[{a}]]
freezeNodes[cl[a_][x_][B_?SquareMatrixQ],nodes_]:=cl[a[[DeleteCases[Range[Length[a]],Alternatives@@nodes]]],a[[nodes]]][x[[DeleteCases[Range[Length[x]],Alternatives@@nodes]]],x[[nodes]]][B[[Join[DeleteCases[Range[Length[a]],Alternatives@@nodes],nodes],Join[DeleteCases[Range[Length[a]],Alternatives@@nodes],nodes]]]]
freezeNodes[cl[a_,af_][x_,xf_][B_?SquareMatrixQ],nodes_]:=cl[a[[DeleteCases[Range[Length[a]],Alternatives@@nodes]]],Join[af,a[[nodes]]]][x[[DeleteCases[Range[Length[x]],Alternatives@@nodes]]],Join[xf,x[[nodes]]]][B[[Join[DeleteCases[Range[Length[a]+Length[af]],Alternatives@@nodes],nodes],Join[DeleteCases[Range[Length[a]+Length[af]],Alternatives@@nodes],nodes]]]]/;Max@@nodes<=Length[a]

freezeNodes[clA[a__][B_?SquareMatrixQ],nodes_]:=Print["cannot freeze nondymaic (or nonexistent) node"]/;Max@@nodes>Length[First[{a}]]
freezeNodes[clA[a_][B_?SquareMatrixQ],nodes_]:=clA[a[[DeleteCases[Range[Length[a]],Alternatives@@nodes]]],a[[nodes]]][B[[Join[DeleteCases[Range[Length[a]],Alternatives@@nodes],nodes],Join[DeleteCases[Range[Length[a]],Alternatives@@nodes],nodes]]]]/;Max@@nodes<=Length[a]
freezeNodes[clA[a_,af_][B_?SquareMatrixQ],nodes_]:=clA[a[[DeleteCases[Range[Length[a]],Alternatives@@nodes]]],Join[af,a[[nodes]]]][B[[Join[DeleteCases[Range[Length[a]+Length[af]],Alternatives@@nodes],nodes],Join[DeleteCases[Range[Length[a]+Length[af]],Alternatives@@nodes],nodes]]]]/;Max@@nodes<=Length[a]

freezeNodes[clX[x__][B_?SquareMatrixQ],nodes_]:=Print["cannot freeze nondymaic (or nonexistent) node"]/;Max@@nodes>Length[First[{x}]]
freezeNodes[clX[x_][B_?SquareMatrixQ],nodes_]:=clX[x[[DeleteCases[Range[Length[x]],Alternatives@@nodes]]],x[[nodes]]][B[[Join[DeleteCases[Range[Length[x]],Alternatives@@nodes],nodes],Join[DeleteCases[Range[Length[x]],Alternatives@@nodes],nodes]]]]
freezeNodes[clX[x_,xf_][B_?SquareMatrixQ],nodes_]:=clA[x[[DeleteCases[Range[Length[x]],Alternatives@@nodes]]],Join[xf,x[[nodes]]]][B[[Join[DeleteCases[Range[Length[x]+Length[xf]],Alternatives@@nodes],nodes],Join[DeleteCases[Range[Length[x]+Length[xf]],Alternatives@@nodes],nodes]]]]/;Max@@nodes<=Length[x]

(* Definitions of some useful starting seeds *)

generateSeedA[clA[a__][B_?SquareMatrixQ]]:=clA[a][B]
generateSeedA[dynkinA[1]]:=clA[{a1}][{{0}}]
generateSeedA[dynkinA[n_]]:=clA[Table[Symbol["a"<>ToString[ii]],{ii,n}]][Append[Prepend[Table[ReplacePart[ReplacePart[ConstantArray[0,n],ii-1->-1],ii+1->1],{ii,2,n-1}],ReplacePart[ConstantArray[0,n],2->1]],ReplacePart[ConstantArray[0,n],n-1->-1]]]
generateSeedA[dynkinD[n_]]:=clA[Table[Symbol["a"<>ToString[ii]],{ii,n}]][Append[Append[Append[Prepend[Table[ReplacePart[ReplacePart[ConstantArray[0,n],ii-1->-1],ii+1->1],{ii,2,n-3}],ReplacePart[ConstantArray[0,n],2->1]],ReplacePart[ConstantArray[0,n],{n-3->-1,n-1->1,n->1}]],ReplacePart[ConstantArray[0,n],{n-2->-1}]],ReplacePart[ConstantArray[0,n],{n-2->-1}]]]/;n>3
generateSeedA[dynkinE[6]]=clA[Table[Symbol["a"<>ToString[ii]],{ii,6}]][{{0,1,0,0,0,0},{-1,0,1,0,0,0},{0,-1,0,1,1,0},{0,0,-1,0,0,0},{0,0,-1,0,0,1},{0,0,0,0,-1,0}}];
generateSeedA[Gr[4,6]]=clA[{ab[1,2,3,5],ab[1,2,4,5],ab[1,3,4,5]},{ab[1,2,3,4],ab[1,2,3,6],ab[1,2,5,6],ab[1,4,5,6],ab[3,4,5,6],ab[2,3,4,5]}][{{0,1,0,-1,1,-1,0,0,0},{-1,0,1,0,0,1,-1,0,0},{0,-1,0,0,0,0,1,-1,1},{1,0,0,0,0,0,0,0,0},{-1,0,0,0,0,0,0,0,0},{1,-1,0,0,0,0,0,0,0},{0,1,-1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0,0}}];
generateSeedA[Gr[4,7]]=clA[{ab[1,2,3,5],ab[1,2,4,5],ab[1,3,4,5],ab[1,2,3,6],ab[1,2,5,6],ab[1,4,5,6]},{ab[1,2,3,4],ab[1,2,3,7],ab[1,2,6,7],ab[1,5,6,7],ab[4,5,6,7],ab[3,4,5,6],ab[2,3,4,5]}][{{0,1,0,1,-1,0,-1,0,0,0,0,0,0},{-1,0,1,0,1,-1,0,0,0,0,0,0,0},{0,-1,0,0,0,1,0,0,0,0,0,-1,1},{-1,0,0,0,1,0,0,1,-1,0,0,0,0},{1,-1,0,-1,0,1,0,0,1,-1,0,0,0},{0,1,-1,0,-1,0,0,0,0,1,-1,1,0},{1,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,-1,0,0,0,0,0,0,0,0,0},{0,0,0,1,-1,0,0,0,0,0,0,0,0},{0,0,0,0,1,-1,0,0,0,0,0,0,0},{0,0,0,0,0,1,0,0,0,0,0,0,0},{0,0,1,0,0,-1,0,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0,0,0,0,0,0}}];
generateSeedA[dynkin1_,dynkin2_,dynkinN__]:=generateSeedA[generateSeedA[dynkin1,dynkin2],dynkinN]
generateSeedA[dynkin1_,dynkin2_]:=comb[generateSeedA[dynkin1],generateSeedA[dynkin2]]/.{comb[clA[a1_][B1_],clA[a2_][B2_]]:>clA[Table[Symbol["a"<>ToString[ii]],{ii,Length[a1]+Length[a2]}]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],comb[clA[a1_,af1_][B1_],clA[a2_][B2_]]:>freezeNodes[clA[Array[dummy,Length[a1]+Length[af1]+Length[a2]]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],Range[Length[a1]+1,Length[a1]+Length[af1]]],comb[clA[a1_][B1_],clA[a2_,af2_][B2_]]:>freezeNodes[clA[Array[dummy,Length[a1]+Length[a2]+Length[af2]]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],Range[Length[a1]+Length[a2]+1,Length[a1]+Length[a2]+Length[af2]]],comb[clA[a1_,af1_][B1_],clA[a2_,af2_][B2_]]:>freezeNodes[clA[Array[dummy,Length[a1]+Length[af1]+Length[a2]+Length[af2]]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],Join[Range[Length[a1]+1,Length[a1]+Length[af1]],Range[Length[a1]+Length[af1]+Length[a2]+1,Length[a1]+Length[af1]+Length[a2]+Length[af2]]]]}/.clA[dummy_,dummyF_][B_]:>clA[Table[Symbol["a"<>ToString[ii]],{ii,Length[dummy]}],Table[Symbol["a"<>ToString[ii]],{ii,Length[dummy]+1,Length[dummy]+Length[dummyF]}]][B]

generateSeedX[clX[a__][B_?SquareMatrixQ]]:=clX[a][B]
generateSeedX[dynkinA[1]]:=clX[{x1}][{{0}}]
generateSeedX[dynkinA[n_]]:=clX[Table[Symbol["x"<>ToString[ii]],{ii,n}]][Append[Prepend[Table[ReplacePart[ReplacePart[ConstantArray[0,n],ii-1->-1],ii+1->1],{ii,2,n-1}],ReplacePart[ConstantArray[0,n],2->1]],ReplacePart[ConstantArray[0,n],n-1->-1]]]
generateSeedX[dynkinD[n_]]:=clX[Table[Symbol["x"<>ToString[ii]],{ii,n}]][Append[Append[Append[Prepend[Table[ReplacePart[ReplacePart[ConstantArray[0,n],ii-1->-1],ii+1->1],{ii,2,n-3}],ReplacePart[ConstantArray[0,n],2->1]],ReplacePart[ConstantArray[0,n],{n-3->-1,n-1->1,n->1}]],ReplacePart[ConstantArray[0,n],{n-2->-1}]],ReplacePart[ConstantArray[0,n],{n-2->-1}]]]/;n>3
generateSeedX[dynkinE[6]]=clX[Table[Symbol["x"<>ToString[ii]],{ii,6}]][{{0,1,0,0,0,0},{-1,0,1,0,0,0},{0,-1,0,1,1,0},{0,0,-1,0,0,0},{0,0,-1,0,0,1},{0,0,0,0,-1,0}}];
generateSeedX[dynkin1_,dynkin2_,dynkinN__]:=generateSeedX[generateSeedX[dynkin1,dynkin2],dynkinN]
generateSeedX[dynkin1_,dynkin2_]:=comb[generateSeedX[dynkin1],generateSeedX[dynkin2]]/.{comb[clX[x1_][B1_],clX[x2_][B2_]]:>clX[Table[Symbol["x"<>ToString[ii]],{ii,Length[x1]+Length[x2]}]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],comb[clX[x1_,xf1_][B1_],clX[x2_][B2_]]:>freezeNodes[clX[Array[dummy,Length[x1]+Length[xf1]+Length[x2]]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],Range[Length[x1]+1,Length[x1]+Length[xf1]]],comb[clX[x1_][B1_],clX[x2_,xf2_][B2_]]:>freezeNodes[clX[Array[dummy,Length[x1]+Length[x2]+Length[xf2]]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],Range[Length[x1]+Length[x2]+1,Length[x1]+Length[x2]+Length[xf2]]],comb[clX[x1_,xf1_][B1_],clX[x2_,xf2_][B2_]]:>freezeNodes[clX[Array[dummy,Length[x1]+Length[xf1]+Length[x2]+Length[xf2]]][ArrayFlatten[ReleaseHold[DiagonalMatrix[Hold/@{B1,B2}]]]],Join[Range[Length[x1]+1,Length[x1]+Length[xf1]],Range[Length[x1]+Length[xf1]+Length[x2]+1,Length[x1]+Length[xf1]+Length[x2]+Length[xf2]]]]}/.clX[dummy_,dummyF_][B_]:>clX[Table[Symbol["x"<>ToString[ii]],{ii,Length[dummy]}],Table[Symbol["x"<>ToString[ii]],{ii,Length[dummy]+1,Length[dummy]+Length[dummyF]}]][B]

generateSeed[seed__]:=generateSeedA[seed]/.{clA[a_][B_]:>cl[a][Table[Symbol["x"<>ToString[ii]],{ii,Length[a]}]][B],clA[a_,af_][B_]:>cl[a][Table[Symbol["x"<>ToString[ii]],{ii,Length[a]+1,Length[a]+Length[af]}]][B]}

(* The mutation rules for A and X variables *)

newVarA[{vars__},B_?SquareMatrixQ,k_]:=newVarA[{vars},B,k]=Factor[(Product[If[TrueQ[Simplify[B[[ii,k]]>0,Table[var>0,{var,Variables@Level[B,{-1}]}]]],Power[{vars}[[ii]],B[[ii,k]]],1],{ii,Length[{vars}]}]+Product[If[TrueQ[Simplify[B[[ii,k]]<0,Table[var>0,{var,Variables@Level[B,{-1}]}]]],Power[{vars}[[ii]],-B[[ii,k]]],1],{ii,Length[{vars}]}])/{vars}[[k]]]
newVarsX[{xVars__},B_?SquareMatrixQ,k_]:=newVarsX[{xVars},B,k]=Table[Factor[If[ii==k,Power[{xVars}[[k]],-1],{xVars}[[ii]]Power[1+Power[{xVars}[[k]],If[TrueQ[Simplify[B[[ii,k]]>0,Table[var>0,{var,Variables@Level[B,{-1}]}]]],1,-1]],B[[ii,k]]]]],{ii,Length[{xVars}]}]

(* Defines a canonical ordering on the A variables to prevent over-couning 
   clusters that only differ by perumtations of indices *)

clBasisA[{a1___,a2_,a3_,a4___}][B_?SquareMatrixQ]:=(clBasisA[{a1,a2,a3,a4}][B]=clBasisA[{a1,a3,a2,a4}][B[[#,#]]]&@Join[Range[Length[{a1}]],{Length[{a1}]+2},{Length[{a1}]+1},Range[Length[{a4}]]+Length[{a1}]+2])/;Sort[{a2,a3}]==={a3,a2}
clBasisA[{a1___,a2_,a3_,a4___},{af__}][B_?SquareMatrixQ]:=(clBasisA[{a1,a2,a3,a4},{af}][B]=clBasisA[{a1,a3,a2,a4},{af}][B[[#,#]]]&@Join[Range[Length[{a1}]],{Length[{a1}]+2},{Length[{a1}]+1},Range[Length[{a4}]+Length[{af}]]+Length[{a1}]+2])/;Sort[{a2,a3}]==={a3,a2}
clBasisA[{a__},{af1___,af2_,af3_,af4___}][B_?SquareMatrixQ]:=(clBasisA[{a},{af1,af2,af3,af4}][B]=clBasisA[{a},{af1,af3,af2,af4}][B[[#,#]]]&@Join[Range[Length[{a}]+Length[{af1}]],{Length[{a}]+Length[{af1}]+2},{Length[{a}]+Length[{af1}]+1},Range[Length[{a}]+Length[{af1}]+3,Length[{a}]+Length[{af1}]+Length[{af4}]+2]])/;Sort[{af2,af3}]==={af3,af2}

clBasisX[{x1___,x2_,x3_,x4___}][B_?SquareMatrixQ]:=(clBasisX[{x1,x2,x3,x4}][B]=clBasisX[{x1,x3,x2,x4}][B[[#,#]]]&@Join[Range[Length[{x1}]],{Length[{x1}]+2},{Length[{x1}]+1},Range[Length[{x4}]]+Length[{x1}]+2])/;Sort[{x3,x2}]==={x3,x2}
clBasisX[{x1___,x2_,x3_,x4___},{xf__}][B_?SquareMatrixQ]:=(clBasisX[{x1,x2,x3,x4},{xf}][B]=clBasisX[{x1,x3,x2,x4},{xf}][B[[#,#]]]&@Join[Range[Length[{x1}]],{Length[{x1}]+2},{Length[{x1}]+1},Range[Length[{x4}]+Length[{xf}]]+Length[{x1}]+2])/;Sort[{x3,x2}]==={x3,x2}
clBasisX[{x__},{xf1___,xf2_,xf3_,xf4___}][B_?SquareMatrixQ]:=(clBasisX[{x},{xf1,xf2,xf3,xf4}][B]=clBasisX[{x},{xf1,xf3,xf2,xf4}][B[[#,#]]]&@Join[Range[Length[{x}]+Length[{xf1}]],{Length[{x}]+Length[{xf1}]+2},{Length[{x}]+Length[{xf1}]+1},Range[Length[{x}]+Length[{xf1}]+Length[{xf4}]+2]])/;Sort[{xf3,xf2}]==={xf3,xf2}

clBasis[{a1___,a2_,a3_,a4___}][x_][B_?SquareMatrixQ]:=(clBasis[{a1,a2,a3,a4}][x][B]=clBasis[{a1,a3,a2,a4}][x[[#]]][B[[#,#]]]&@Join[Range[Length[{a1}]],{Length[{a1}]+2},{Length[{a1}]+1},Range[Length[{a4}]]+Length[{a1}]+2])/;Sort[{a3,a2}]==={a3,a2}
clBasis[{a1___,a2_,a3_,a4___},{f__}][x_,xf_][B_?SquareMatrixQ]:=(clBasis[{a1,a2,a3,a4},{f}][x,xf][B]=clBasis[{a1,a3,a2,a4},{f}][x[[Drop[#,-Length[{f}]]]],xf][B[[#,#]]]&@Join[Range[Length[{a1}]],{Length[{a1}]+2},{Length[{a1}]+1},Range[Length[{a4}]+Length[{f}]]+Length[{a1}]+2])/;Sort[{a3,a2}]==={a3,a2}
clBasis[{a__},{f1___,f2_,f3_,f4___}][x_,xf_][B_?SquareMatrixQ]:=(clBasis[{a},{f1,f2,f3,f4}][x,xf][B]=clBasis[{a},{f1,f3,f2,f4}][x,xf[[Drop[#,Length[{a}]]-Length[{a}]]]][B[[#,#]]]&@Join[Range[Length[{a}]+Length[{f1}]],{Length[{a}]+Length[{f1}]+2},{Length[{a}]+Length[{f1}]+1},Length[{a}]+2+Range[Length[{f1}]+Length[{f4}]]])/;Sort[{f3,f2}]==={f3,f2}

(* Defines a mutation operator that acts on a cluster c using of the above 
   definitions, where again indices are mutated on from left to right *)

mutate[cl[a__][x__][B_?SquareMatrixQ],{k_}]:=mutate[cl[a][x][B],k]
mutate[clA[a__][B_?SquareMatrixQ],{k_}]:=mutate[clA[a][B],k]
mutate[clX[x__][B_?SquareMatrixQ],{k_}]:=mutate[clX[x][B],k]
mutate[cl[a__][x__][B_?SquareMatrixQ],{kInit__,k_}]:=mutate[cl[a][x][B],{kInit,k}]=mutate[mutate[cl[a][x][B],{kInit}],k]
mutate[clA[a__][B_?SquareMatrixQ],{kInit__,k_}]:=mutate[clA[a][B],{kInit,k}]=mutate[mutate[clA[a][B],{kInit}],k]
mutate[clX[x__][B_?SquareMatrixQ],{kInit__,k_}]:=mutate[clX[x][B],{kInit,k}]=mutate[mutate[clX[x][B],{kInit}],k]
mutate[cl[a_][x_][B_?SquareMatrixQ],k_]:=cl[ReplacePart[a,k->newVarA[a,B,k]]][newVarsX[x,B,k]][\[Mu][B,k]]
mutate[clA[a_][B_?SquareMatrixQ],k_]:=clA[ReplacePart[a,k->newVarA[a,B,k]]][\[Mu][B,k]]
mutate[clX[x_][B_?SquareMatrixQ],k_]:=clX[newVarsX[x,B,k]][\[Mu][B,k]]
mutate[cl[a_,af_][x_,xf_][B_?SquareMatrixQ],k_]:=cl[ReplacePart[a,k->newVarA[Join[a,af],B,k]],af][Take[newVarsX[Join[x,xf],B,k],Length[x]],Drop[newVarsX[Join[x,xf],B,k],Length[x]]][\[Mu][B,k]]/;k<=Length[a]
mutate[clA[a_,af_][B_?SquareMatrixQ],k_]:=clA[ReplacePart[a,k->newVarA[Join[a,af],B,k]],af][\[Mu][B,k]]/;k<=Length[a]
mutate[clX[x_,xf_][B_?SquareMatrixQ],k_]:=clX[Take[newVarsX[Join[x,xf],B,k],Length[x]],Drop[newVarsX[Join[x,xf],B,k],Length[x]]][\[Mu][B,k]]/;k<=Length[x]

(* Finds all clusters generated by mutation paths of length jj and shorter *)

allLabeledClustersWithinDistance[clA[a__][B_?SquareMatrixQ],0]:={path[]->clA[a][B]}
allLabeledClustersWithinDistance[clX[x__][B_?SquareMatrixQ],0]:={path[]->clX[x][B]}
allLabeledClustersWithinDistance[cl[a__][x__][B_?SquareMatrixQ],0]:={path[]->cl[a][x][B]}
allLabeledClustersWithinDistance[clA[a__][B_?SquareMatrixQ],jj_]:=allLabeledClustersWithinDistance[clA[a][B],jj]=Module[{currentClusters=allLabeledClustersWithinDistance[clA[a][B],jj-1],previousLengthPaths=Select[allLabeledClustersWithinDistance[clA[a][B],jj-1]/.Rule[p_,_]:>p,Length[#]==jj-1&]},While[Length[previousLengthPaths]>0,currentClusters=DeleteDuplicatesBy[Join[currentClusters,Table[newPath->mutate[Drop[newPath,-1]/.allLabeledClustersWithinDistance[clA[a][B],jj-1],newPath[[-1]]],{newPath,Select[Table[First[previousLengthPaths]/.path[tt___]:>path[tt,ii],{ii,Length[First[{a}]]}],!MatchQ[#,path[__,IND_,IND_]]&]}]],#[[2]]/.clA->clBasisA&];previousLengthPaths=Drop[previousLengthPaths,1]];currentClusters]
allLabeledClustersWithinDistance[clX[x__][B_?SquareMatrixQ],jj_]:=allLabeledClustersWithinDistance[clX[x][B],jj]=Module[{currentClusters=allLabeledClustersWithinDistance[clX[x][B],jj-1],previousLengthPaths=Select[allLabeledClustersWithinDistance[clX[x][B],jj-1]/.Rule[p_,_]:>p,Length[#]==jj-1&]},While[Length[previousLengthPaths]>0,currentClusters=DeleteDuplicatesBy[Join[currentClusters,Table[newPath->mutate[Drop[newPath,-1]/.allLabeledClustersWithinDistance[clX[x][B],jj-1],newPath[[-1]]],{newPath,Select[Table[First[previousLengthPaths]/.path[tt___]:>path[tt,ii],{ii,Length[First[{x}]]}],!MatchQ[#,path[__,IND_,IND_]]&]}]],#[[2]]/.cl->clBasisX&];previousLengthPaths=Drop[previousLengthPaths,1]];currentClusters]
allLabeledClustersWithinDistance[cl[a__][x__][B_?SquareMatrixQ],jj_]:=allLabeledClustersWithinDistance[cl[a][x][B],jj]=Module[{currentClusters=allLabeledClustersWithinDistance[cl[a][x][B],jj-1],previousLengthPaths=Select[allLabeledClustersWithinDistance[cl[a][x][B],jj-1]/.Rule[p_,_]:>p,Length[#]==jj-1&]},While[Length[previousLengthPaths]>0,currentClusters=DeleteDuplicatesBy[Join[currentClusters,Table[newPath->mutate[Drop[newPath,-1]/.allLabeledClustersWithinDistance[cl[a][x][B],jj-1],newPath[[-1]]],{newPath,Select[Table[First[previousLengthPaths]/.path[tt___]:>path[tt,ii],{ii,Length[First[{a}]]}],!MatchQ[#,path[__,IND_,IND_]]&]}]],#[[2]]/.cl->clBasis&];previousLengthPaths=Drop[previousLengthPaths,1]];currentClusters]

(* Finds all planar clusters generated by mutation paths of length jj and shorter *)

allPlanarLabeledClustersWithinDistance[clA[a__][B_?SquareMatrixQ],0]:={path[]->clA[a][B]}
allPlanarLabeledClustersWithinDistance[clA[a__][B_?SquareMatrixQ],jj_]:=allPlanarLabeledClustersWithinDistance[clA[a][B],jj]=Module[{currentClusters=allPlanarLabeledClustersWithinDistance[clA[a][B],jj-1],previousLengthPaths=Select[allPlanarLabeledClustersWithinDistance[clA[a][B],jj-1]/.Rule[p_,_]:>p,Length[#]==jj-1&]},While[Length[previousLengthPaths]>0,currentClusters=DeleteDuplicatesBy[Join[currentClusters,Factor[Table[newPath->mutate[Drop[newPath,-1]/.allPlanarLabeledClustersWithinDistance[clA[a][B],jj-1],newPath[[-1]]],{newPath,Table[First[previousLengthPaths]/.path[tt___]:>path[tt,ii],{ii,DeleteCases[Table[If[Total[#[[kk]]]<6,kk,-1],{kk,Length[First[{a}]]}],Alternatives[-1,First[previousLengthPaths]/.path[tt___,ll_]:>ll]]&@Abs[(First[previousLengthPaths]/.allPlanarLabeledClustersWithinDistance[clA[a][B],jj-1])[[1]]]}]}]//.schoutenReplacements[7]]],#[[2]]/.clA->clBasisA&];previousLengthPaths=Drop[previousLengthPaths,1]];currentClusters]

allPlanarLabeledClustersWithinDistance[Gr[4,n_],0]:={path[]->clA[Gr[4,n]]}
allPlanarLabeledClustersWithinDistance[Gr[4,n_],jj_]:=allPlanarLabeledClustersWithinDistance[Gr[4,n],jj]=Module[{currentClusters=allPlanarLabeledClustersWithinDistance[Gr[4,n],jj-1],previousLengthPaths=Select[allPlanarLabeledClustersWithinDistance[Gr[4,n],jj-1]/.Rule[p_,_]:>p,Length[#]==jj-1&]},While[Length[previousLengthPaths]>0,currentClusters=DeleteDuplicatesBy[Join[currentClusters,Table[newPath->mutate[Drop[newPath,-1]/.allPlanarLabeledClustersWithinDistance[Gr[4,n],jj-1],newPath[[-1]]],{newPath,Table[First[previousLengthPaths]/.path[tt___]:>path[tt,ii],{ii,DeleteCases[Table[If[Total[Abs[#[[1,kk]]]]<6,kk,-1],{kk,Length[#[[0,1]]]}],Alternatives[-1,First[previousLengthPaths]/.path[tt___,ll_]:>ll]]&@(First[previousLengthPaths]/.allPlanarLabeledClustersWithinDistance[Gr[4,n],jj-1])}]}]//.schoutenReplacements[n]],#[[2]]/.clA->clBasisA&];previousLengthPaths=Drop[previousLengthPaths,1]];currentClusters]

(* Finds all clusters *)

allLabeledClusters[clA[a__][B_?SquareMatrixQ]]:=allLabeledClusters[clA[a][B]]=Module[{clusters={clA[a][B]},kk=1},While[Length[clusters]<Length[allLabeledClustersWithinDistance[clA[a][B],kk]],clusters=allLabeledClustersWithinDistance[clA[a][B],kk];kk+=1];clusters]
allLabeledClusters[clX[x__][B_?SquareMatrixQ]]:=allLabeledClusters[clX[x][B]]=Module[{clusters={clX[x][B]},kk=1},While[Length[clusters]<Length[allLabeledClustersWithinDistance[clX[x][B],kk]],clusters=allLabeledClustersWithinDistance[clX[x][B],kk];kk+=1];clusters]
allLabeledClusters[cl[a__][x__][B_?SquareMatrixQ]]:=allLabeledClusters[cl[a][x][B]]=Module[{clusters={cl[a][x][B]},kk=1},While[Length[clusters]<Length[allLabeledClustersWithinDistance[cl[a][x][B],kk]],clusters=allLabeledClustersWithinDistance[cl[a][x][B],kk];kk+=1];clusters]

allClusters[clA[a__][B_?SquareMatrixQ]]:=allClusters[clA[a][B]]=allLabeledClusters[clA[a][B]]/.Rule[path_,cluster_]:>cluster
allClusters[clX[x__][B_?SquareMatrixQ]]:=allClusters[clX[x][B]]=allLabeledClusters[clX[x][B]]/.Rule[path_,cluster_]:>cluster
allClusters[cl[a__][x__][B_?SquareMatrixQ]]:=allClusters[cl[a][x][B]]=allLabeledClusters[cl[a][x][B]]/.Rule[path_,cluster_]:>cluster

(* Finds all planar clusters *)

allPlanarLabeledClusters[clA[a__][B_?SquareMatrixQ]]:=allPlanarLabeledClusters[clA[a][B]]=Module[{clusters={clA[a][B]},kk=1},While[Length[clusters]<Length[allPlanarLabeledClustersWithinDistance[clA[a][B],kk]],clusters=allPlanarLabeledClustersWithinDistance[clA[a][B],kk];kk+=1];clusters]
allPlanarClusters[clA[a__][B_?SquareMatrixQ]]:=allPlanarClusters[clA[a][B]]=allPlanarLabeledClusters[clA[a][B]]/.Rule[path_,cluster_]:>cluster

allPlanarLabeledClusters[Gr[4,n_]]:=allPlanarLabeledClusters[Gr[4,n]]=Module[{clusters={clA[Gr[4,n]]},kk=1},While[Length[clusters]<Length[allPlanarLabeledClustersWithinDistance[Gr[4,n],kk]],clusters=allPlanarLabeledClustersWithinDistance[Gr[4,n],kk];kk+=1];clusters]
allPlanarClusters[Gr[4,n_]]:=allPlanarClusters[Gr[4,n]]=allPlanarLabeledClusters[Gr[4,n]]/.Rule[path_,cluster_]:>cluster

(* Finds all cluster A- and X-coordinates *)

allClusterVarsA[clA[a__][B_?SquareMatrixQ]]:=Sort[DeleteDuplicates[Join@@(allClusters[clA[a][B]]/.clA[aVars__][Bdummy_]:>Flatten[{aVars}])]]
allClusterVarsX[clX[x__][B_?SquareMatrixQ]]:=Sort[DeleteDuplicates[Join@@(allClusters[clX[x][B]]/.clX[xVars__][Bdummy_]:>Flatten[{xVars}])]]
allClusterVars[cl[a__][x__][B_?SquareMatrixQ]]:={Sort[DeleteDuplicates[Join@@(#[[All,1,All]])]],Sort[DeleteDuplicates[Join@@(#[[All,2,All]])]]}&@(allClusters[cl[a][x][B]]/.cl[aVars__][xVars__][Bdummy_]:>{Flatten[{aVars}],Flatten[{xVars}]})

allNonfrozenClusterVarsA[clA[a__][B_?SquareMatrixQ]]:=Sort[DeleteDuplicates[Join@@(allClusters[clA[a][B]]/.clA[aVars__][Bdummy_]:>First[{aVars}])]]
allNonfrozenClusterVarsX[clX[x__][B_?SquareMatrixQ]]:=Sort[DeleteDuplicates[Join@@(allClusters[clX[x][B]]/.clX[xVars__][Bdummy_]:>First[{xVars}])]]
allNonfrozenClusterVars[cl[a__][x__][B_?SquareMatrixQ]]:={Sort[DeleteDuplicates[Join@@(#[[All,1,All]])]],Sort[DeleteDuplicates[Join@@(#[[All,2,All]])]]}&@(allClusters[cl[a][x][B]]/.cl[aVars__][xVars__][Bdummy_]:>{First[{aVars}],First[{xVars}]})

(* Find subalgebras of a certain type in a given algebra *)

findSubalgebras[cl[aAlgVars_][xAlgVars_][algB_?SquareMatrixQ],cl[aSubalgVars_][xSubalgVars_][subalgB_?SquareMatrixQ]]:=findSubalgebras[cl[aAlgVars][xAlgVars][algB],cl[aSubalgVars][xSubalgVars][subalgB]]=Module[{remainingClusters=allLabeledClusters[cl[aAlgVars][xAlgVars][algB]],currentSubalgebras={}},While[Length[remainingClusters]>0,currentSubalgebras=DeleteDuplicatesBy[Join[currentSubalgebras,Select[Table[First[remainingClusters]/.Rule[path_,cl[a_][x_][B_]]:>subalg[cl[a[[list]]][x[[list]]][B[[list,list]]],path,coord@@list,MatchQ[subalgB,B[[list,list]]]],{list,Flatten[Permutations/@Subsets[Range[Length[aAlgVars]],{Length[aSubalgVars]}],1]}],Last[#]&]/.subalg[alg_,path_,coord_,True]:>subalg[alg,path,coord,allNonfrozenClusterVars[freezeNodes[path/.allLabeledClusters[cl[aAlgVars][xAlgVars][algB]],Complement[Range[Length[aAlgVars]],List@@coord]]]]],Last[#]&];remainingClusters=Drop[remainingClusters,1]];currentSubalgebras]

(* Calculate the X-coordinates in terms of A-coordinates *)

xCoordinatesFromA[clA[a_][B_?SquareMatrixQ]]:=Table[Product[Power[a[[jj]],B[[ii,jj]]],{jj,Length[B]}],{ii,Length[a]}]
xCoordinatesFromA[clA[a_,af_][B_?SquareMatrixQ]]:=Table[Product[Power[Join[a,af][[jj]],B[[ii,jj]]],{jj,Length[B]}],{ii,Length[a]}]

xCoordinatesFromA[cl[a_][x_][B_?SquareMatrixQ]]:=Table[Product[Power[a[[jj]],B[[ii,jj]]],{jj,Length[B]}],{ii,Length[a]}]
xCoordinatesFromA[cl[a_,af_][x_,xf_][B_?SquareMatrixQ]]:=Table[Product[Power[Join[a,af][[jj]],B[[ii,jj]]],{jj,Length[B]}],{ii,Length[a]}]

(* Finds all pairs of cluster-adjacent A-coordinates or X-coordinates *)

adjacentPairsA[clA[a__][B_?SquareMatrixQ]]:=Join[ConstantArray[#,2]&/@Sort[DeleteDuplicates[Flatten[#]]],#]&@(DeleteCases[Flatten[Table[If[MemberQ[#,Alternatives[{___,l1,___,l2,___},{___,l2,___,l1,___}]],{l1,l2},{}],{l1,Sort@DeleteDuplicates[Flatten[#]]},{l2,Sort@DeleteDuplicates[Flatten[#]]}],1],{}]&@(allClusters[clA[a][B]]/.clA[aVars__][Bmat_]:>Flatten[{aVars}]))
adjacentPairsA[cl[a__][x__][B_?SquareMatrixQ]]:=Join[ConstantArray[#,2]&/@Sort[DeleteDuplicates[Flatten[#]]],#]&@(DeleteCases[Flatten[Table[If[MemberQ[#,Alternatives[{___,l1,___,l2,___},{___,l2,___,l1,___}]],{l1,l2},{}],{l1,Sort@DeleteDuplicates[Flatten[#]]},{l2,Sort@DeleteDuplicates[Flatten[#]]}],1],{}]&@(allClusters[cl[a][x][B]]/.cl[aVars__][xVars__][Bmat_]:>Flatten[{aVars}]))

adjacentPairsX[generateSeed[dynkinA[1]]]={{1/x1,1/x1},{x1,x1}};
adjacentPairsX[clA[a__][B_?SquareMatrixQ]]:=Join[ConstantArray[#,2]&/@Sort[DeleteDuplicates[Flatten[#]]],#]&@(DeleteCases[Flatten[Table[If[MemberQ[#,Alternatives[{___,l1,___,l2,___},{___,l2,___,l1,___}]],{l1,l2},{}],{l1,Sort@DeleteDuplicates[Flatten[#]]},{l2,Sort@DeleteDuplicates[Flatten[#]]}],1],{}]&@(xCoordinatesFromA/@allClusters[clA[a][B]]))
adjacentPairsX[clX[x__][B_?SquareMatrixQ]]:=Join[ConstantArray[#,2]&/@Sort[DeleteDuplicates[Flatten[#]]],#]&@(DeleteCases[Flatten[Table[If[MemberQ[#,Alternatives[{___,l1,___,l2,___},{___,l2,___,l1,___}]],{l1,l2},{}],{l1,Sort@DeleteDuplicates[Flatten[#]]},{l2,Sort@DeleteDuplicates[Flatten[#]]}],1],{}]&@(allClusters[clX[x][B]]/.clX[xVars__][Bmat_]:>Flatten[{xVars}]))
adjacentPairsX[cl[a__][x__][B_?SquareMatrixQ]]:=Join[ConstantArray[#,2]&/@Sort[DeleteDuplicates[Flatten[#]]],#]&@(DeleteCases[Flatten[Table[If[MemberQ[#,Alternatives[{___,l1,___,l2,___},{___,l2,___,l1,___}]],{l1,l2},{}],{l1,Sort@DeleteDuplicates[Flatten[#]]},{l2,Sort@DeleteDuplicates[Flatten[#]]}],1],{}]&@(allClusters[cl[a][x][B]]/.cl[aVars__][xVars__][Bmat_]:>Flatten[{xVars}]))

(* Catalog all the subalgebras of every algebra *)

allAlgebras={generateSeed[dynkinE[6]],generateSeed[dynkinD[5],dynkinA[1]],generateSeed[dynkinA[5],dynkinA[1]],generateSeed[dynkinD[4],dynkinA[2]],generateSeed[dynkinD[4],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[4],dynkinA[2]],generateSeed[dynkinA[4],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[3],dynkinA[3]],generateSeed[dynkinA[3],dynkinA[2],dynkinA[1]],generateSeed[dynkinA[3],dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[2],dynkinA[2],dynkinA[2]],generateSeed[dynkinA[2],dynkinA[2],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[2],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinD[5]],generateSeed[dynkinA[5]],generateSeed[dynkinD[4],dynkinA[1]],generateSeed[dynkinA[4],dynkinA[1]],generateSeed[dynkinA[3],dynkinA[2]],generateSeed[dynkinA[3],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[2],dynkinA[2],dynkinA[1]],generateSeed[dynkinA[2],dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinD[4]],generateSeed[dynkinA[4]],generateSeed[dynkinA[3],dynkinA[1]],generateSeed[dynkinA[2],dynkinA[2]],generateSeed[dynkinA[2],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[3]],generateSeed[dynkinA[2],dynkinA[1]],generateSeed[dynkinA[1],dynkinA[1],dynkinA[1]],generateSeed[dynkinA[2]],generateSeed[dynkinA[1],dynkinA[1]],generateSeed[dynkinA[1]]};

(* indices generated with the following code:
cluster=generateSeed[dynkinE[6]];
Flatten[Position[If[Length[#[[0]][[1]]]\[GreaterEqual]Length[cluster[[0]][[1]]],0,Length[findSubalgebras[cluster,#]]]&/@allAlgebras,_Integer?Positive]] *)
allSubalgebras[generateSeed[dynkinE[6]]]=allAlgebras[[]];
allSubalgebras[generateSeed[dynkinD[5],dynkinA[1]]]=allAlgebras[[{15,17,18,20,22,24,25,26,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[5],dynkinA[1]]]=allAlgebras[[{16,18,20,21,25,26,27,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinD[4],dynkinA[2]]]=allAlgebras[[{17,19,22,24,26,27,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinD[4],dynkinA[1],dynkinA[1]]]=allAlgebras[[{17,20,23,24,26,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[4],dynkinA[2]]]=allAlgebras[[{18,19,21,25,26,27,28,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[4],dynkinA[1],dynkinA[1]]]=allAlgebras[[{18,20,22,25,26,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[3],dynkinA[3]]]=allAlgebras[[{19,20,26,27,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[3],dynkinA[2],dynkinA[1]]]=allAlgebras[[{19,20,21,22,26,27,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[3],dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{20,22,23,26,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[2],dynkinA[2]]]=allAlgebras[[{21,27,28,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[2],dynkinA[1],dynkinA[1]]]=allAlgebras[[{21,22,27,28,29,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{22,23,28,29,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{23,29,32,34,35}]];
allSubalgebras[generateSeed[dynkinD[5]]]=allAlgebras[[{24,25,26,28,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[5]]]=allAlgebras[[{25,26,27,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinD[4],dynkinA[1]]]=allAlgebras[[{24,26,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[4],dynkinA[1]]]=allAlgebras[[{25,26,28,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[3],dynkinA[2]]]=allAlgebras[[{26,27,28,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[3],dynkinA[1],dynkinA[1]]]=allAlgebras[[{26,28,29,30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[2],dynkinA[1]]]=allAlgebras[[{27,28,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{28,29,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{29,32,34,35}]];
allSubalgebras[generateSeed[dynkinD[4]]]=allAlgebras[[{30,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[4]]]=allAlgebras[[{30,31,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[3],dynkinA[1]]]=allAlgebras[[{30,31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[2]]]=allAlgebras[[{31,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[1],dynkinA[1]]]=allAlgebras[[{31,32,33,34,35}]];
allSubalgebras[generateSeed[dynkinA[1],dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{32,34,35}]];
allSubalgebras[generateSeed[dynkinA[3]]]=allAlgebras[[{33,34,35}]];
allSubalgebras[generateSeed[dynkinA[2],dynkinA[1]]]=allAlgebras[[{33,34,35}]];
allSubalgebras[generateSeed[dynkinA[1],dynkinA[1],dynkinA[1]]]=allAlgebras[[{34,35}]];
allSubalgebras[generateSeed[dynkinA[2]]]=allAlgebras[[{35}]];
allSubalgebras[generateSeed[dynkinA[1],dynkinA[1]]]=allAlgebras[[{35}]];

(* Generate the weight-two (possibly cluster-adjacent) symbols on a given algebra *)

solveIntegrability[ansatz_,vars_]:=Module[{iteration=1,eqnList},eqnList=DeleteCases[If[FreeQ[#,Log],Flatten[#],Join[Flatten[#/.Log[_]->0],Flatten[coeffArrayN[#,Log]]]]&@(Table[ansatz/.CircleDot[t1_,t2_]:>D[t1*D[t2,var1],var2]-D[t1*D[t2,var2],var1],{var1,vars},{var2,vars}]),0];Association@@reduce[Flatten[eqnList/.Table[var->RandomInteger[{1,200}],{ii,Length[eqnList]*20},{var,vars}]]]]

weightTwoAnsatzX[cl[a__][x__][B_?SquareMatrixQ]]:=weightTwoAnsatzX[cl[a][x][B]]=Module[{ansatz,expandedAnsatz,redundancyReplacements,Xcoordinates=allClusterVars[cl[a][x][B]][[2]]},ansatz =Array[c2dummy,Length[#]].#&@Flatten[Table[CircleDot[Log[t1],Log[t2]],{t1,Xcoordinates},{t2,Xcoordinates}]];expandedAnsatz=ansatz//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]];redundancyReplacements=Association@@Table[tt->0,{tt,termsHead[reduce[coeffArrayN[expandedAnsatz]]/.Rule[p_,q_]:>q,c2dummy]}];Array[c2,Length[#]].#&@DeleteCases[Expand[coeffArrayN[ansatz/.redundancyReplacements/.solveIntegrability[expand[expandedAnsatz/.redundancyReplacements],Flatten[{x}]],c2dummy]],0]]
adjacentWeightTwoAnsatzX[cl[a__][x__][B_?SquareMatrixQ]]:=adjacentWeightTwoAnsatzX[cl[a][x][B]]=Module[{ansatz},ansatz=Array[c2dummy,Length[#]].#&@(CircleDot[Log[#1],Log[#2]]&@@@Sort[DeleteDuplicates[{Last[Sort[{#[[1]],1/#[[1]]}]],Last[Sort[{#[[2]],1/#[[2]]}]]}&/@adjacentPairsX[cl[a][x][B]]]]);Array[c2,Length[#]].#&@DeleteCases[Expand[coeffArrayN[ansatz/.solveIntegrability[expand[ansatz],Flatten[{x}]],c2dummy]],0]]

(* Generate the weight-four (possibly cluster-adjacent) symbols on a given algebra *)

restrictMiddlePairs[ansatzTerms_,allowedMiddlePairs_]:=Module[{splitAnsatzTerms,expandedMiddlePairs},
(*splitAnsatzTerms=ansatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],b,c,d];
splitAnsatzTerms=splitAnsatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],c,d];
splitAnsatzTerms=splitAnsatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b,c//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],d];
splitAnsatzTerms=splitAnsatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b,c,d//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]]];*)
splitAnsatzTerms=(*splitAnsatzTerms*)ansatzTerms/.CircleDot[t1_,t2_,t3_,t4_]:>temp[t1,t4]*CircleDot[t2,t3];
(*expandedMiddlePairs=allowedMiddlePairs/.CircleDot[a_,b_]:>CircleDot[a//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],b];
expandedMiddlePairs=expandedMiddlePairs/.CircleDot[a_,b_]:>CircleDot[a,b//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]]];*)
expandedMiddlePairs=allowedMiddlePairs;
Sort[DeleteCases[expand[ansatzTerms.coeffArrayN[Array[c4dummy,Length[ansatzTerms]]/.Association@@reduce[Sort[DeleteDuplicates[Expand[Flatten[NullSpace[CoefficientArrays[expandedMiddlePairs,termsHead[splitAnsatzTerms]][[2]]].(coeffArray[Array[c4dummy,Length[ansatzTerms]].(coeffArray[splitAnsatzTerms][[2]]),temp][[2]])]]],Length[#1]<Length[#2]&]],c4dummy]],0]]]

weightFourAnsatzX[cl[a__][x__][B_?SquareMatrixQ]]:=weightFourAnsatzX[cl[a][x][B]]=Module[{ansatzTerms,weightTwoAnsatzTerms=coeffArrayN[weightTwoAnsatzX[cl[a][x][B]],c2]},ansatzTerms=Flatten[Table[CircleDot[t1,t2],{t1,weightTwoAnsatzTerms},{t2,weightTwoAnsatzTerms}]];Array[c4,Length[#]].#&@restrictMiddlePairs[ansatzTerms,weightTwoAnsatzTerms]]
weightFourAnsatzXexpanded[cl[a__][x__][B_?SquareMatrixQ]]:=weightFourAnsatzXexpanded[cl[a][x][B]]=weightFourAnsatzX[cl[a][x][B]]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],l2,l3,l4]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1,l2//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],l3,l4]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1,l2,l3//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],l4]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1,l2,l3,l4//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]]]

adjacentWeightFourAnsatzX[generateSeed[dynkinA[1]]]=c4[1] Log[x1]\[CircleDot]Log[x1]\[CircleDot]Log[x1]\[CircleDot]Log[x1];
adjacentWeightFourAnsatzX[cl[a__][x__][B_?SquareMatrixQ]]:=adjacentWeightFourAnsatzX[cl[a][x][B]]=Module[{ansatzTerms,weightTwoAnsatzTerms=coeffArrayN[adjacentWeightTwoAnsatzX[cl[a][x][B]],c2]},ansatzTerms=Flatten[Table[CircleDot[t1,t2],{t1,weightTwoAnsatzTerms},{t2,weightTwoAnsatzTerms}]];Array[c4,Length[#]].#&@restrictMiddlePairs[ansatzTerms,weightTwoAnsatzTerms]]
adjacentWeightFourAnsatzXexpanded[cl[a__][x__][B_?SquareMatrixQ]]:=adjacentWeightFourAnsatzXexpanded[cl[a][x][B]]=adjacentWeightFourAnsatzX[cl[a][x][B]]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],l2,l3,l4]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1,l2//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],l3,l4]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1,l2,l3//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],l4]/.CircleDot[l1_,l2_,l3_,l4_]:>CircleDot[l1,l2,l3,l4//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]]]

(* Evaluate the weight-four cluster-adjacent ansatz on all subalgebras
   of a given type on a larger algebra *)

weightFourSubalgAnsatzX[cl[aAlg__][xAlg__][Balg_?SquareMatrixQ],cl[aSubalg__][xSubalg__][Bsubalg_?SquareMatrixQ]]:=weightFourSubalgAnsatzX[cl[aAlg][xAlg][Balg],cl[aSubalg][xSubalg][Bsubalg]]=Sum[adjacentWeightFourAnsatzX[cl[aSubalg][xSubalg][Bsubalg]]/.AssociationThread[xSubalg,vars]/.c4[ii_]:>c4subalg[ii,vars,Bsubalg],{vars,findSubalgebras[cl[aAlg][xAlg][Balg],cl[aSubalg][xSubalg][Bsubalg]]/.subalg[cl[a_][x_][B_],path[___],coord[__],vars_]:>x}]/.Log[arg_]:>Log[Factor[arg]]
weightFourSubalgAnsatzXexpanded[cl[aAlg__][xAlg__][Balg_?SquareMatrixQ],cl[aSubalg__][xSubalg__][Bsubalg_?SquareMatrixQ]]:=weightFourSubalgAnsatzXexpanded[cl[aAlg][xAlg][Balg],cl[aSubalg][xSubalg][Bsubalg]]=Table[c4subalg[ii,Bsubalg],{ii,Length[#]}].#&@(Normal[CoefficientArrays[#,reduce[coeffArrayN[#]]/.Rule[p_,q_]:>p][[2]]]&@(weightFourSubalgAnsatzX[cl[aAlg][xAlg][Balg],cl[aSubalg][xSubalg][Bsubalg]]/.c4subalg[ii_,vars_,B_]:>c4subalg[ii,Sequence@@vars]/.CircleDot[a_,b_,c_,d_]:>CircleDot[a//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],b,c,d]/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],c,d]/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b,c//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],d]/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b,c,d//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]]]))

(* Generate the space of symbols on a given algebra that are not 
   subalgebra constructible *)

nonSubalgebraConstructibleWeightFourAnsatzX[generateSeed[dynkinA[1]]]=adjacentWeightFourAnsatzX[generateSeed[dynkinA[1]]];
nonSubalgebraConstructibleWeightFourAnsatzX[cl[a__][x__][B_?SquareMatrixQ]]:=nonSubalgebraConstructibleWeightFourAnsatzX[cl[a][x][B]]=Array[c4,Length[#]].#&@coeffArrayN[adjacentWeightFourAnsatzXexpanded[cl[a][x][B]]/.Association@@(Rule[#,0]&/@Complement[termsHead[adjacentWeightFourAnsatzXexpanded[cl[a][x][B]],c4],reduce[NullSpace[CoefficientArrays[coeffArrayN[Sum[weightFourSubalgAnsatzXexpanded[cl[a][x][B],allSubalgebras[cl[a][x][B]][[ss]]]/.c4subalg[tt__]:>c4subalg[ss,tt],{ss,Length[allSubalgebras[cl[a][x][B]]]}],c4subalg],termsHead[adjacentWeightFourAnsatzXexpanded[cl[a][x][B]]]][[2]]].coeffArrayN[adjacentWeightFourAnsatzXexpanded[cl[a][x][B]]]]/.Rule[p_,q_]:>p]),c4]

(* Evaluate the weight-four cluster-adjacent non-subalgebra-constructible 
   ansatz on all subalgebras of a given type on a larger algebra *)

nonSubalgebraConstructibleWeightFourSubalgAnsatzX[cl[aAlg__][xAlg__][Balg_?SquareMatrixQ],cl[aSubalg__][xSubalg__][Bsubalg_?SquareMatrixQ]]:=nonSubalgebraConstructibleWeightFourSubalgAnsatzX[cl[aAlg][xAlg][Balg],cl[aSubalg][xSubalg][Bsubalg]]=Module[{ansatz,ansatzTerms,subalgebras=findSubalgebras[cl[aAlg][xAlg][Balg],cl[aSubalg][xSubalg][Bsubalg]]},If[Length[subalgebras]>0,ansatz=Sum[nonSubalgebraConstructibleWeightFourAnsatzX[cl[aSubalg][xSubalg][Bsubalg]]/.AssociationThread[xSubalg,vars]/.c4[ii_]:>c4subalg[ii,Sequence@@vars],{vars,subalgebras/.subalg[cl[a_][x_][B_],path[___],coord[__],vars_]:>x}]/.Log[arg_]:>Log[Factor[arg]];ansatzTerms=termsHead[ansatz]/.CircleDot[a_,b_,c_,d_]:>CircleDot[a//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],b,c,d];ansatzTerms=ansatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],c,d];ansatzTerms=ansatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b,c//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]],d];ansatzTerms=ansatzTerms/.CircleDot[a_,b_,c_,d_]:>CircleDot[a,b,c,d//.expandLogs/.Pi->0/.Log[arg_]:>Log[Sort[{arg,-arg}][[-1]]]];expand[#/.Association@@Table[tt->0,{tt,termsHead[reduce[coeffArrayN[#]]/.Rule[p_,q_]:>q,c4subalg]}]]&@(coeffArrayN[ansatz].ansatzTerms),0]]

(* The automorphism generators for the finite cluster algebras *)

a2\[Sigma]={x1->(x1 x2)/(1+x1),x2->1/x1};
a2\[Tau]={x1->1/x2,x2->1/x1};

a3\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->(1+x1+x1 x2)/(x1 x2 x3)};
a3\[Tau]={x1->1/x3,x2->1/x2,x3->1/x1};

a4\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->((1+x1+x1 x2) x4)/(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4),x4->(1+x1+x1 x2+x1 x2 x3)/(x1 x2 x3 x4)};
a4\[Tau]={x1->1/x4,x2->1/x3,x3->1/x2,x4->1/x1};

a5\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->((1+x1+x1 x2) x4)/(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4),x4->((1+x1+x1 x2+x1 x2 x3) x5)/(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4+x1 x2 x3 x4 x5),x5->(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4)/(x1 x2 x3 x4 x5)};
a5\[Tau]={x1->1/x5,x2->1/x4,x3->1/x3,x4->1/2,x5->1/x1};

d4\[Sigma]3={x1->1/x3,x2->(x1 x2 (1+x3))/(1+x1),x3->x4,x4->1/x1};
d4\[Tau]3={x1->x1,x2->x2,x3->x4,x4->x3};

d4\[Sigma]4={x1->x2/(1+x1+x1 x2),x2->(x1 (1+x1) x2 x3 x4)/((1+x1+x1 x2+x1 x2 x3) (1+x1+x1 x2+x1 x2 x4)),x3->(1+x1+x1 x2)/(x1 x2 x3),x4->(1+x1+x1 x2)/(x1 x2 x4)};
d4\[Tau]4={x1->x1,x2->(1+x1)/(x1 x2 (1+x3) (1+x4)),x3->x3,x4->x4};

d5\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->(x1 x2 (1+x1+x1 x2) x3 x4 x5)/((1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4) (1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x5)),x4->(1+x1+x1 x2+x1 x2 x3)/(x1 x2 x3 x4),x5->(1+x1+x1 x2+x1 x2 x3)/(x1 x2 x3 x5)};
d5\[Tau]={x1->x1,x2->(1+x1)/(x1 x2 (1+x3+x3 x4+x3 x5+x3 x4 x5)),x3->(x3 x4 x5)/((1+x3+x3 x4) (1+x3+x3 x5)),x4->(1+x3+x3 x4+x3 x5+x3 x4 x5)/x4,x5->(1+x3+x3 x4+x3 x5+x3 x4 x5)/x5};
d5z2={x1->x1,x2->x2,x3->x3,x4->x5,x5->x4}; 

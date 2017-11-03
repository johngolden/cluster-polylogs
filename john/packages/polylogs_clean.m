(* ::Package:: *)

(*assorted minor technical definitions*)

expand[x_]:=If[Length[x]>0,Total[Expand/@(List@@x)],x];
length[x_]:=Length[x];


(*the length of the multiplicative basis of a given set of cross-ratios*)

multLength[x_]:=If[Length[x]<2,Length[x],Module[{Y,M},
Y = PowerExpand[Log /@ x]/.Pi -> 0;
Y = Expand[Y.(y /@ Range[Length[Y]])];
Y = List @@ Collect[Y, Log[__]];
Y = Y/.Log[__]->1;
M = CoefficientArrays[Y, y /@ Range[Length[x]]][[2]];
Return[MatrixRank[M]];]];

(*generate a multiplicative basis for a given set of cross-ratios*)

multBasis[x_]:=multBasis[x]=Module[{array,baseTry,i},
For[i=1;array={};baseTry={},multLength[baseTry]<multLength[x],i++,
baseTry=x[[#]]&/@Append[array,i];
array=If[Length[baseTry]-multLength[baseTry]==0,Append[array,i],array]];
Return[baseTry]];

(*given a multpiplicative basis, solve for R*)
multSolveFull[{basisExp_,basisSym_}][R_]:=Module[{X,Y,Z,sol1,sol},
X=basisExp;
Y=(y/@Range[Length[X]]).Table[Exponent[X[[i]],#]&/@Variables[X],{i,Length[X]}];
Z=Exponent[R,#]&/@Variables[X];
sol=Solve[Y==Z];
If[Length[sol]>0,Return[(((Times@@(((basisSym)^(y/@Range[Length[X]]))//.sol[[1]]))))],Return[0]]];


(*solve*)
solve[eqs_]:=Module[{x,m,b,mat,matSolve,soln,isSolnGood},
x=Variables[eqs];
m=CoefficientArrays[eqs,x][[2]];
b=-eqs/.p[_]:>0;
mat=Transpose[Append[Transpose[m],b]];
matSolve=Rationalize[Chop[DeleteCases[RowReduce[N[mat]],Table[0,{i,Length[mat[[1]]]}]]],.0000001];
isSolnGood=If[Length[matSolve]-Length[matSolve[[1]]]==0,If[Union[Flatten[matSolve-IdentityMatrix[Length[matSolve]]]]=={0},0,1],If[Max[Position[#,1,1,1][[1,1]]&/@matSolve]>Length[x],0,1]];
soln=If[isSolnGood==1, Table[With[{pos=Position[matSolve[[iii]],1,1,1][[1,1]]},x[[pos]]->-(matSolve[[iii,pos+1;;-1]].Append[x,-1][[pos+1;;-1]])],{iii,Length[matSolve]}],{}];
Return[soln];];
genEqs[0]=0;
genEqs[x_]:=With[{vars=DeleteCases[Variables[x],p[__]]},eqs=sortMinus[Normal[CoefficientArrays[x,vars]][[2]]]];
genEqsFull[x_]:=With[{vars=DeleteCases[Variables[x],p[__]]},eqs=Normal[CoefficientArrays[x,vars]][[2]]];
fit[base_]:=Module[{eqs,soln},
eqs=DeleteCases[genEqs[base],0];
soln=If[MemberQ[Union[NumberQ/@ eqs],True],{},solve[eqs]];
Return[soln];];


(*tensor & projector rules*)

Tensor[a___,b_*c_,d___]:=Tensor[a,b,d]+Tensor[a,c,d];
Tensor[a___,b_^c_,d___]:=c Tensor[a,b,d];
Tensor[a___,b_/c_,d___]:=Tensor[a,b,d]-Tensor[a,c,d];
Tensor[a___,b_?NumberQ,c___]:=0;
tensorExpand[x_]:=(x/.tensor:>Tensor)/.Tensor:>tensor;
tensorVars[x_]:=Union[Cases[Variables[x],tensor[__]]/.tensor[a__]:>a];
clean[x_]:=Module[{cleanVar,tmp},cleanVar[xx_]:=cleanVar[xx]=With[{base=Together[xx]},Factor[Numerator[base]]/Factor[Denominator[base]]];cleanVar/@tensorVars[x];tmp=x/.tensor[a__]:>tensor@@(cleanVar/@{a});tmp=tensorExpand[tmp];If[sortMinus[tensorVars[tmp]]!=tensorVars[tmp],tmp=tmp/.tensor[a__]:>tensor@@(Sort[{#,-#}][[-1]]&/@{a})];Return[tmp];];
delta[x_]:=clean[((x/.tensor[a_,b_,c_,d_]:>tensor[a,b,c,d]-tensor[a,b,d,c]-tensor[b,a,c,d]+tensor[b,a,d,c]-tensor[c,d,a,b]+tensor[c,d,b,a]+tensor[d,c,a,b]-tensor[d,c,b,a])//.{wedge[cb2[a_],cb2[b_]]:>-tensor[cb2[a],1+b,b]+tensor[cb2[a],b,1+b]+tensor[cb2[b],1+a,a]-tensor[cb2[b],a,1+a],tensor[cb3[a_],b_]:>tensor[cb2[a],a,b]-tensor[cb2[a],b,a]})//.tensor[cb2[a_],b__]:>tensor[1+a,a,b]-tensor[a,1+a,b]];
b2b2[x_]:=clean[(((x/.tensor[cb3[_],_]:>0)/.tensor[a_,b_,c_,d_]:>tensor[a,b,c,d]-tensor[a,b,d,c]-tensor[b,a,c,d]+tensor[b,a,d,c]-tensor[c,d,a,b]+tensor[c,d,b,a]+tensor[d,c,a,b]-tensor[d,c,b,a])//.wedge[cb2[a_],cb2[b_]]:>tensor[cb2[a],1+b,b]-tensor[cb2[a],b,1+b]-tensor[cb2[b],1+a,a]+tensor[cb2[b],a,1+a])//.tensor[cb2[a_],b__]:>tensor[1+a,a,b]-tensor[a,1+a,b]];
b3c[x_]:=Module[{tmp}, tmp=x/.wedge[__]:>0; tmp=tmp/.tensor[a_,b_,c_,d_]:>(tensor[a,b,c,d]-tensor[b,a,c,d]-tensor[b,c,a,d]+tensor[c,b,a,d]) -(tensor[b,c,d,a]-tensor[c,b,d,a]-tensor[c,d,b,a]+tensor[d,c,b,a]); tmp=tmp/.tensor[cb3[a_],b_]:>tensor[1+a,a,a,b]-tensor[a,1+a,a,b];tmp=clean[tmp];Return[tmp];];
sortInverse[x_]:=Union[Sort[{#,1/#}][[-1]]&/@x];
sortMinus[x_]:=Union[Sort[{#,-#}][[-1]]&/@x];
cleanCoproducts[x_]:=(x//.{cb2[a_]:>cb2[Together[a]],cb3[a_]:>cb3[Together[a]]})//.tensor[a_,b_]:>tensor[a,Together[b]];
cleanWedge[x_]:=(((((x/.cb2[a_]:>cb2[Together[a]])//.wedge[a_,a_]:>0)//.cb2[a_]:>If[Position[Sort[{a,1/a}],a]=={{1}},cb2[a],-cb2[1/a]])//.wedge[a___,-b_,c___]:>-wedge[a,b,c])/.wedge[cb2[a_],cb2[b_]]:>If[Position[Sort[{a,b}],a]=={{1}},wedge[cb2[a],cb2[b]],-wedge[cb2[b],cb2[a]]])/.wedge[cb2[a_],cb2[b_]]:>wedge[cb2[1/a],cb2[1/b]];


(*old bad code:
clean[xx_]:=Expand[tensorExpand[((xx)/.tensor[a__]:>tensor@@(Together/@{a}))/.tensor[a__]:>tensor@@(Factor/@{a})]/.tensor[a__]:>tensor@@(Sort[{#,-#}][[-1]]&/@{a})]
b3c[x_]:=clean[((x/.wedge[__]:>0)//.{tensor[a_,b_,c_,d_]:>Expand[(Tensor[a,b,c,d]-Tensor[b,a,c,d]-Tensor[b,c,a,d]+Tensor[c,b,a,d]) -
	(Tensor[b,c,d,a]-Tensor[c,b,d,a]-Tensor[c,d,b,a]+Tensor[d,c,b,a]) ],tensor[cb3[a_],b_]:>Tensor[1+a,a,a,b]-Tensor[a,1+a,a,b]})//.Tensor:>tensor]
*)

(*symbols*)

li22[x_,y_]:=tensor[-1+y,-1+x,x,y]+tensor[-1+y,-1+x,y,x]+tensor[-1+y,y,-1+x,x]-tensor[-1+x y,-1+x,x,y]-tensor[-1+x y,-1+x,y,x]-tensor[-1+x y,x,-1+x,x]+tensor[-1+x y,x,x,x]+tensor[-1+x y,x,x,y]+tensor[-1+x y,x,-1+y,y]+tensor[-1+x y,x,y,x]+tensor[-1+x y,-1+y,x,y]+tensor[-1+x y,-1+y,y,x]-tensor[-1+x y,y,-1+x,x]+tensor[-1+x y,y,x,x]+tensor[-1+x y,y,-1+y,y];

li4[a_]:=\[Minus]tensor[1\[Minus]a,a,a,a];

li3[a_]:=\[Minus]tensor[1\[Minus]a,a,a];

li2li2[a_,b_]:=tensor[-1+a, a, -1+b, b] + tensor[-1+a, -1+b, a, b] + tensor[-1+a, -1+b, b, a] + tensor[-1+b, -1+a, a, b] + tensor[-1+b, -1+a, b, a] + tensor[-1+b, b, -1+a, a];

li2loglog[a_,b_,c_]:=-(tensor[-1+a,a,b,c]+tensor[-1+a,a,c,b]+tensor[-1+a,b,a,c]+tensor[-1+a,b,c,a]+tensor[-1+a,c,b,a]+tensor[-1+a,c,a,b]+tensor[b,-1+a,a,c]+tensor[b,-1+a,c,a]+tensor[b,c,-1+a,a]+tensor[c,b,-1+a,a]+tensor[c,-1+a,b,a]+tensor[c,-1+a,a,b]);

li3log[a_,b_]:=-(tensor[1-a,a,a,b]+tensor[1-a,a,b,a]+tensor[1-a,b,a,a]+tensor[b,1-a,a,a]);


(*antisym schemes*)

li22Part[x_]:=clean[(x//.tensor[a_,b_,c_,d_]:> ttensor[a,b,c,d]-ttensor[a,b,d,c]-ttensor[b,a,c,d]+ttensor[b,a,d,c]-ttensor[c,d,a,b]+ttensor[c,d,b,a]+ttensor[d,c,a,b]-ttensor[d,c,b,a])//.ttensor:>tensor];
li4Part[x_]:=clean[(x//.tensor[a_,b_,c_,d_]:> ttensor[a,b,c,d]-ttensor[b,a,c,d]-ttensor[b,c,a,d]-ttensor[b,c,d,a]+ttensor[c,b,a,d]+ttensor[c,b,d,a]+ttensor[c,d,b,a]-ttensor[d,c,b,a])//.ttensor:>tensor];
li2li2Part[x_]:=clean[(x//.tensor[a_,b_,c_,d_]:> ttensor[a,b,c,d]-ttensor[a,b,d,c]-ttensor[b,a,c,d]+ttensor[b,a,d,c]+ttensor[c,d,a,b]-ttensor[c,d,b,a]-ttensor[d,c,a,b]+ttensor[d,c,b,a])//.ttensor:>tensor];
li2loglogPart[x_]:=clean[(x//.tensor[a_,b_,c_,d_]:> ttensor[a,b,c,d]-ttensor[a,b,d,c])//.ttensor:>tensor];
li3logPart[x_]:=clean[(x//.tensor[a_,b_,c_,d_]:> ttensor[a,b,c,d]-ttensor[b,a,c,d])//.ttensor:>tensor];
delGood[xx_]:=clean[xx//.tensor[a__]:>If[Length[Complement[{a},{x,y,1+x,1+y}]]>0,tensor[a],0]];


(*functions*)

l22[x_,y_]:=1/2 (li22[x/y,-y]-li22[y/x,-x])
L22[x_,y_]:=1/2 (Li[{2,2},{x/y,-y}]-Li[{2,2},{y/x,-x}]);

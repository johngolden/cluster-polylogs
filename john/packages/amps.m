(* ::Package:: *)

(*load data*)
oldDir=Directory[];
SetDirectory[Directory[]<>"/data"];
<<ratios.mx
<<xcoords.mx
<<symbols.mx
<<schoutens.mx
<<conjRules.mx
<<m1Rules.mx
<<poissonData.m
SetDirectory[oldDir];


(*parse data*)
allRatios[6]=ALLRATIOS[[1]];
allRatios[7]=ALLRATIOS[[2]];
allRatios[8]=ALLRATIOS[[3]];
allRatios[9]=ALLRATIOS[[4]];
Clear[ALLRATIOS];

xcoords[6]=ALLXCOORDS[[1]];
xcoords[7]=ALLXCOORDS[[2]];
xcoords[8]=ALLXCOORDS[[3]];
xcoords[9]=ALLXCOORDS[[4]];
Clear[ALLXCOORDS];

symbol[6]=ALLSYMBOLS[[1]];
symbol[7]=ALLSYMBOLS[[2]];
symbol[8]=ALLSYMBOLS[[3]];
symbol[9]=ALLSYMBOLS[[4]];
Clear[ALLSYMBOLS];

schoutens[6]=ALLSCHOUTENS[[1]];
schoutens[7]=ALLSCHOUTENS[[2]];
schoutens[8]=ALLSCHOUTENS[[3]];
schoutens[9]=ALLSCHOUTENS[[4]];
Clear[ALLSCHOUTENS];

conjRules[6]=ALLCONJRULES[[1]];
conjRules[7]=ALLCONJRULES[[2]];
conjRules[8]=ALLCONJRULES[[3]];
conjRules[9]=ALLCONJRULES[[4]];
Clear[ALLCONJRULES];


(*assorted minor technical definitions*)

expand[x_]:=If[Length[x]>0,Total[Expand/@(List@@x)],x];
length[x_]:=Length[x];

sortInverse[x_]:=Union[Sort[{#,1/#}][[-1]]&/@x];
sortMinus[x_]:=Union[Sort[{#,-#}][[-1]]&/@x];

ratios[n_]:=ratios[n]=sortInverse[xcoords[n]];


(*kinematic points within the positive grassmannian*)

Z[6]={{23946604304851,191062106960,265790520,1795680},{579755633539,4809578002,11043286,79808},{138295215389,1383088902,9674386,79808},{1254243391,13385791,130201,1247},{6023999,113735,3473,43},{124124,2387,77,1}};
Z[7]={{9871978950921479,76070078882946,129810260997,280294425},{46916693010767,394225821633,1477196931,3221775},{2560646257133,22314627072,102644379,224775},{31244262107,299508525,2024775,4995},{1658789072,16897776,140007,555},{103884088,1062424,8921,37},{551736,6984,97,1}};
Z[8]={{7424622550576150,12470314587670,244003563027,1587222000},{2199565010902226,4040896575890,81218762289,529074000},{55155197535086,161177934590,3753868509,25194000},{758459902192,2649263320,64940637,442000},{26393486812,93160900,2290131,15600},{529762832,2686388,73371,520},{4689804,35616,1077,8},{43092,2268,81,1}};
Z[9]={{14564525020574770500,28183970448729395,66235858099132,6503411200},{22046316003912900,54014822331835,192515124836,23909600},{2660999685709260,8849311441315,43203735104,5977400},{5470765303164,28329150755,233242258,41800},{8563911981816,49032605110,443788451,83600},{277755999540,1954468685,20600698,4180},{3630895440,30186908,349513,88},{14448840,169742,2461,22},{382704,4556,67,1}};
Z[10]={{11591015076444701132113,27043375924922929632,176513320714894800,1162931883750000},{46265933111130364201,211949938877484384,1411179768682320,9303455070000},{321670568281293271,1626737294332368,10854426633024,71565039000},{32896379738340211,166830868463808,1113269707504,7340004000},{60484016305151,316891667088,2127942699,14043375},{51518215994396,303264052668,2114891349,14043375},{430023021052,3544600572,28341789,192375},{1877048652,21930972,204671,1425},{27181416,326256,3281,25},{513216,7128,99,1}};
Z[11]={{1093790733529809723255,2563700849570508427,2164079632028000,10103617650000},{403086096446345283765,957768548326433321,809725471222300,3788856618750},{28760098118167393395,68391006975216323,57827012429650,270632615625},{1057829089083655695,2613493412405519,2247816503842,10825304625},{37592598572436525,98822485631309,96837712918,569752875},{961097984309025,2618791475689,2805969578,18379125},{6422355641820,22477235872,38832878,360375},{365175116955,1789273223,6038572,69750},{709245135,9347243,115900,1550},{8583165,126585,1854,25},{123120,2736,72,1}};
Z[12]={{16502634363584862510814,21849312255664579217,57829335947266429,63694856179773},{14277487501628524491742,20308696438368514337,56931140830916989,63694856179773},{384619166123218041814,634467568419582149,2306560564648753,4899604321521},{7975276517544113026,13456528490031263,50656906432531,113944286547},{3533486267789241922,7272390765379103,35763364962451,113944286547},{53013075348564898,120652010838047,657495584899,2325393603},{398483068991210,1278409451059,9231774263,43875351},{1354048324910,4731316993,36071981,180557},{20937273724,126487142,1318159,9503},{237662830,3895721,55567,559},{1969498,61037,1009,13},{17914,689,13,1}};
Z[13]={{970590617355247814259367,17640907117143294479616,28707234623265194136,9801180332768880},{295518012861681067705789,5373330386357177839872,8814546184472854812,3267060110922960},{298852523881418313289,5435973307713699072,8992308595400172,3626037858960},{566646655812876140593,10309005178728528384,17136431309827368,7252075717920},{42546881993783242859,774270528742042752,1298451988448076,604339643160},{587179887299030089,10686011856404352,17950484683572,8511825960},{4270997949669683,77763296406144,136058129004,102552120},{325042407594673,5921248789184,11488928948,17092020},{1740521233741,31764444128,86086280,294690},{171752556158,3134958784,8727691,31020},{355340590,6509360,29813,165},{21611984,399856,3970,33},{446688,8272,88,1}};


sort={br[x___]:> Signature[{x}]br@@Sort[{x}],ccap1[i_,tup[x___],tup[y___],tup[z___]]:> (Times@@(Signature/@{{x},{y},{z}})Signature[Sort/@{{x},{y},{z}}]ccap1[i,#1,#2,#3]&@@(tup@@@Sort[Sort/@{{x},{y},{z}}])),ccap[i_,j_,tup[x___],tup[y___]]:> (Times@@(Signature/@{{i,j},{x},{y}})Signature[Sort/@{{i,j},{x},{y}}]ccap[#1,#2,#3,#4]&@@(Join[Sort[{i,j}],tup@@@Sort[Sort/@{{x},{y}}]]))};

mod[n_][x_] := (x/.{ccap1[a_, tup[b__], tup[c__], tup[d__]] :>ccap1[Mod[a, n, 1], tup @@ Mod[{b}, n, 1], tup @@ Mod[{c}, n, 1], tup @@ Mod[{d}, n, 1]], br[a__] :> br @@ Mod[{a}, n, 1], ccap[a_, b_, tup[c__], tup[d__]] :> ccap[Mod[a, n, 1], Mod[b, n, 1], tup @@ Mod[{c}, n, 1], tup @@ Mod[{d}, n, 1]]});


cap[a_, b_, tup[i_, j_, k_], tup[l_, m_, n_]] := br[a, i, j, k] br[b, l, m, n] - br[a, l, m, n] br[b, i, j, k]//.sort; 
capAlt[i_,ip1_,tup[jm1_,j_,jp1_],tup[km1_,k_,kp1_]]:=br[i,ip1,jm1,j]br[jp1,km1,k,kp1]+br[j,jp1,i,ip1]br[jm1,km1,k,kp1]+br[jp1,i,ip1,jm1]br[j,km1,k,kp1]//.sort;
cap1[a_, tup[i_, j_], tup[k_, l_], tup[m_, n_]]:=br[i, a, k, l] br[j, a, m, n] - br[j, a, k, l] br[i, a, m, n] //. sort;


(* 1+x+m1[x]=0 *)

m1[n_][x_]:=Together[-1-x];


cycle[n_][x___]:=((x/.{br[y___]:> br@@(Mod[{y}+1,n,1]),ccap1[i_,j___]:>ccap1[Mod[i+1,n,1],j],ccap[i_,j_,k___]:>ccap[Mod[i+1,n,1],Mod[j+1,n,1],k]})/.tup[y___]:>tup@@(Mod[{y}+1,n,1]))/.sort;
flip[n_][x___]:=((x/.{br[y___]:> br@@(Mod[n+1-{y},n,1]),ccap1[i_,j___]:>ccap1[Mod[n+1-i,n,1],j],ccap[i_,j_,k___]:>ccap[Mod[n+1-i,n,1],Mod[n+1-j,n,1],k]})/.tup[y___]:>tup@@(Mod[n+1-{y},n,1]))/.sort;
conjugate[n_][x_]:=x/.conjRules[n];

addCyclic[n_][x_]:=Total[NestList[cycle[n],x,n-1]];
addFlip[n_][x_]:=x+flip[n][x];
addConj[n_][x_]:=x+conjugate[n][x];
addDihedral[n_][x_]:=Total[NestList[cycle[n],x+flip[n][x],n-1]];
addSym[n_][x_]:=Total[NestList[cycle[n],x+flip[n][x]+conjugate[n][x]+conjugate[n][flip[n][x]],n-1]]
listSym[n_][x_]:=Flatten[NestList[cycle[n],{x,flip[n][x],conjugate[n][x],conjugate[n][flip[n][x]]},n-1]];


(*entries in the two-loop MHV symbol*)

goodBrs[n_]:=goodBrs[n]=br@@@Select[Subsets[Range[n],{4}],MemberQ[Mod[#-RotateRight[#,1],n],1]&];
goodCap1s[n_]:=goodCap1s[n]=Flatten[NestList[cycle[n],{Table[ccap1[3,tup[1,2],tup[4,5],tup[k-1,k]],{k,7,n}],Table[ccap1[2,tup[1,3],tup[j-1,j],tup[k-1,k]],{j,5,n-2},{k,j+2,n}]},n-1]]//.-a_:>a;
goodCaps[n_]:=goodCaps[n]=Flatten[NestList[cycle[n],Table[ccap[1,2,tup[i-1,i,i+1],tup[j-1,j,j+1]],{i,4,n-4},{j,i+3,n-1}],n-1]]//.-a_:>a;
goodLetters[n_]:=goodLetters[n]=Join[goodBrs[n],goodCap1s[n],goodCaps[n]];


goodFirst[n_]:=Variables[Table[br[i,Mod[i+1,n,1],j,Mod[j+1,n,1]],{i,n},{j,n}]//.sort];
goodLast[n_]:=Variables[Table[br[i,Mod[j-1,n,1],j,Mod[j+1,n,1]],{i,n},{j,n}]//.sort];


(*multiplicative basis for general n*)

SetAttributes[BR, Orderless];
SetAttributes[TUP, Orderless];
SetAttributes[CCAP1, Orderless];
SetAttributes[CCAP, Orderless];

fixCCAP={CCAP1[a_,TUP[b_,c_],TUP[b_,d_],TUP[e__]]:>BR[a,b,c,d]BR[a,b,e],CCAP[a_,b_,TUP[c_,d__],TUP[c_,e__]]:>CCAP1[c,TUP[a,b],TUP[d],TUP[e]],CCAP[a_,b_,TUP[a_,c__],TUP[d__]]:>BR[a,d]BR[b,a,c]};
MOD[n_][x_] := (x/.{CCAP1[a_, TUP[b__], TUP[c__], TUP[d__]] :>CCAP1[Mod[a, n, 1], TUP @@ Mod[{b}, n, 1], TUP @@ Mod[{c}, n, 1], TUP @@ Mod[{d}, n, 1]], BR[a__] :> BR @@ Mod[{a}, n, 1], CCAP[a_, b_, TUP[c__], TUP[d__]] :> CCAP[Mod[a, n, 1], Mod[b, n, 1], TUP @@ Mod[{c}, n, 1], TUP @@ Mod[{d}, n, 1]]})//.fixCCAP;

v[n_][i_,j_,k_]:=v[n][i,j,k]=With[{base=MOD[n][CCAP1[i,TUP[-1+i,1+i],TUP[j,1+j],TUP[k,1+k]]/(BR[-1+i,i,k,1+k] BR[i,1+i,j,1+j])]//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap}},If[n<14,If[num[n][base]>0,base,-base],If[numPG[n][base]>0,base,-base]]];
zp[n_][i_,j_,k_]:=zp[n][i,j,k]=With[{base=MOD[n][CCAP[k,k+1,TUP[j-1,j,j+1],TUP[i-1,i,i+1]]/(BR[i-1,i,i+1,k+1]BR[j-1,j,j+1,k])//.fixCCAP]//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap}},If[n<14,If[num[n][base]>0,base,-base],If[numPG[n][base]>0,base,-base]]];
zm[n_][i_,j_,k_]:=zm[n][i,j,k]=With[{base=MOD[n][CCAP[i,j,TUP[k-1,k,k+1],TUP[k,k+1,k+2]]/(BR[i,k,k+1,k+2]BR[j,k-1,k,k+1])//.fixCCAP]//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap}},If[n<14,If[num[n][base]>0,base,-base],If[numPG[n][base]>0,base,-base]]];
z[n_][i_,j_]:=zm[n][i,i+1,j];
v[n_][i_,j_]:=v[n][i+1,j,j+1];

vP1[n_][i_,j_,k_]:=vP1[n][i,j,k]=With[{base=MOD[n][BR[-1+i,i,j,1+j] BR[i,1+i,k,1+k]/(BR[-1+i,i,k,1+k] BR[i,1+i,j,1+j])]//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap}},If[n<14,If[num[n][base]>0,base,-base],If[numPG[n][base]>0,base,-base]]];
zpP1[n_][i_,j_,k_]:=zpP1[n][i,j,k]=With[{base=MOD[n][BR[i-1,i,i+1,k]BR[j-1,j,j+1,k+1]/(BR[i-1,i,i+1,k+1]BR[j-1,j,j+1,k])]//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap}},If[n<14,If[num[n][base]>0,base,-base],If[numPG[n][base]>0,base,-base]]];
zmP1[n_][i_,j_,k_]:=zmP1[n][i,j,k]=With[{base=MOD[n][BR[i,k-1,k,k+1]BR[j,k,k+1,k+2]/(BR[i,k,k+1,k+2]BR[j,k-1,k,k+1])]//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap}},If[n<14,If[num[n][base]>0,base,-base],If[numPG[n][base]>0,base,-base]]];
zP1[n_][i_,j_]:=zmP1[n][i,i+1,j];
vP1[n_][i_,j_]:=vP1[n][i+1,j,j+1];

v[i_,j_,k_]:=((CCAP1[i,TUP[-1+i,1+i],TUP[j,1+j],TUP[k,1+k]]/(BR[-1+i,i,k,1+k] BR[i,1+i,j,1+j])//.fixCCAP//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap})//.0:>n)//.sort;
zp[i_,j_,k_]:=(((CCAP[k,k+1,TUP[j-1,j,j+1],TUP[i-1,i,i+1]]/(BR[i-1,i,i+1,k+1]BR[j-1,j,j+1,k]))//.fixCCAP//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap})//.0:>n)//.sort;
zm[i_,j_,k_]:=((CCAP[i,j,TUP[k-1,k,k+1],TUP[k,k+1,k+2]]/(BR[i,k,k+1,k+2]BR[j,k-1,k,k+1])//.fixCCAP//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap})//.0:>n)//.sort;
z[i_,j_]:=zm[i,i+1,j];
v[i_,j_]:=v[i+1,j,j+1];

vP1[i_,j_,k_]:=((BR[-1+i,i,j,1+j] BR[i,1+i,k,1+k]/(BR[-1+i,i,k,1+k] BR[i,1+i,j,1+j])//.fixCCAP//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap})//.0:>n)//.sort;
zpP1[i_,j_,k_]:=(((BR[i-1,i,i+1,k]BR[j-1,j,j+1,k+1]/(BR[i-1,i,i+1,k+1]BR[j-1,j,j+1,k]))//.fixCCAP//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap})//.0:>n)//.sort;
zmP1[i_,j_,k_]:=((BR[i,k-1,k,k+1]BR[j,k,k+1,k+2]/(BR[i,k,k+1,k+2]BR[j,k-1,k,k+1])//.fixCCAP//.{BR:>br,TUP:>tup,CCAP1:>ccap1,CCAP:>ccap})//.0:>n)//.sort;
zP1[i_,j_]:=zm[i,i+1,j];
vP1[i_,j_]:=v[i+1,j,j+1];

rangeZ[n_]:=Sort[Flatten[NestList[#/.{i_,j_}:>Mod[{i,j}+1,n,1]&,Table[{1,i},{i,4,n-2}],n-1],1]];
rangeZm[n_]:=Sort[Flatten[NestList[#/.{i_,j_,k_}:>Mod[{i,j,k}+1,n,1]&,Flatten[Table[{i,j,1},{i,4,n-3},{j,i+2,n-1}],1],n-1],1]];
rangeV[n_]:=Join[Select[Subsets[Range[n],{2}],n-2>#[[2]]-#[[1]]>2&],Sort[Flatten[NestList[#/.{i_,j_,k_}:>Mod[{i,j,k}+1,n,1]&,Flatten[Table[{i,j,1},{i,4,n-3},{j,i+2,n-1}],1],n-1],1]]];

vs[n_]:=v[n]@@@rangeV[n];
zs[n_]:=Join[z[n]@@@rangeZ[n],zp[n]@@@rangeZm[n],zm[n]@@@rangeZm[n]];

vP1s[n_]:=vP1[n]@@@rangeV[n];
zP1s[n_]:=Join[zP1[n]@@@rangeZ[n],zpP1[n]@@@rangeZm[n],zmP1[n]@@@rangeZm[n]];

vSym[n_]:=V@@@rangeV[n];
zSym[n_]:=Join[Z@@@rangeZ[n],Zp@@@rangeZm[n],Zm@@@rangeZm[n]];

cBasis[n_]:=cBasis[n]=Join[v[n]@@@rangeV[n],z[n]@@@rangeZ[n],zp[n]@@@rangeZm[n],zm[n]@@@rangeZm[n]];
cBasisSym[n_]:=Join[V@@@rangeV[n],Z@@@rangeZ[n],Zp@@@rangeZm[n],Zm@@@rangeZm[n]];

cExp[n_]:={V:>v[n],Z:>z[n],Zm:>zm[n],Zp:>zp[n]};


(*some functions for numerically evaluating expressions*)

(*at the kinematical points above*)

brNum[n_][i_,j_,k_,l_]:=brNum[n][i,j,k,l]=Det[{Z[n][[i]],Z[n][[j]],Z[n][[k]],Z[n][[l]]}];

num[n_][x_]:=Module[{m,k},
k = (x/.{ccap[y__]:>cap[y],ccap1[j___]:>cap1[j]})//.sort;
Table[br[i,j,k,l]=brNum[n][i,j,k,l],{l,n},{k,l-1},{j,k-1},{i,j-1}];k=k;
Clear[br];
Return[k]];

(*at a random point in the positive grassmannian*)
numPG[n_][x_]:=Module[{a,b,paths,weight,M,Z,ans},
For[a = 1, a < 4, a++, For[b = 1, b <= n, b++,l[a,b] = Random[Integer, {1, 99}];]];For[a = 1, a <= 4, a++, For[b = 1, b < n, b++,d[a,b] = Random[Integer, {1, 99}];]];
paths[a_, b_] := Select[Tuples[Range[0, n], {5-b}], (Total[#] == (n-a+1) && Last[#] > 0) &] /; (0< a <= n && 0 < b <= 4);
weight[path_, a_, b_] := Module[{total, col, row, p},row = a;p = 1;total = 1;
For[col = 4, col >= 5 - Length[path], col--,total *= Product[d[col, i], {i, row, row + path[[p]] - 1}];row += path[[p++]];If[row <= n, total *= l[col-1, row]];];Return[total/.d[_, n] -> 1];];
M[a_,b_] := Total[weight[#,a,b] & /@ paths[a,b]];
Z = Table[M[a,b],{a,1,n},{b,1,4}];
Z = Z/((GCD @@ #) & /@ Z);
ans=x/.{ccap[y__]:>cap[y],ccap1[j___]:>cap1[j]};
Table[br[i,j,k,l]=Det[{Z[[i]],Z[[j]],Z[[k]],Z[[l]]}],{l,n},{k,l-1},{j,k-1},{i,j-1}];
ans=ans;
Clear[br,l];
Return[ans]];

(*at a completely random point*)
numRand[n_][x_]:=Module[{m,k},
k = x/.{ccap[y__]:>cap[y],ccap1[j___]:>cap1[j]};
m = RandomInteger[{0,100},{n,4}];
Table[br[i,j,k,l]=Det[{m[[i]],m[[j]],m[[k]],m[[l]]}],{l,n},{k,l-1},{j,k-1},{i,j-1}];
k=k;
Clear[br];
Return[k]];

numRatios[n_]:=numRatios[n]=allRatios[n]//num[n];
numXcoords[n_]:=numXcoords[n]=xcoords[n]//num[n];


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


multSolveBasis[n_][R_]:=Module[{X,Y,Z,sol1,sol},
X=cBasis[n];
Y=(y/@Range[Length[X]]).Table[Exponent[X[[i]],#]&/@Variables[X],{i,Length[X]}];
Z=Exponent[R,#]&/@Variables[X];
sol=Solve[Y==Z];
If[Length[sol]>0,Return[(((Times@@(((cBasisSym[n])^(y/@Range[Length[X]]))//.sol[[1]]))))],Return[0]]]


(*Tensor expand rules*)

(*slow but works for any length tensor*)

Tensor[a___,b_*c_,d___]:=Tensor[a,b,d]+Tensor[a,c,d];
Tensor[a___,b_^c_,d___]:=c Tensor[a,b,d];
Tensor[a___,b_/c_,d___]:=Tensor[a,b,d]-Tensor[a,c,d];
Tensor[a___,b_?NumberQ,c___]:=0;

tensorExpandFull[x_]:=Expand[(x/.tensor:>Tensor)/.Tensor:>tensor];

(*more cumbersome to code but slightly faster*)

Tensor1[a___,b_*c_]:=Tensor1[a,b]+Tensor1[a,c];
Tensor1[a___,b_^c_]:=c Tensor1[a,b];
Tensor1[a___,b_/c_]:=Tensor1[a,b]-Tensor1[a,c];
Tensor1[a___,b_?NumberQ]:=0;
Tensor2[a___,b_*c_,d_]:=Tensor2[a,b,d]+Tensor2[a,c,d];
Tensor2[a___,b_^c_,d_]:=c Tensor2[a,b,d];
Tensor2[a___,b_/c_,d_]:=Tensor2[a,b,d]-Tensor2[a,c,d];
Tensor2[a___,b_?NumberQ,c_]:=0;
Tensor3[a_,b_*c_,d_,e_]:=Tensor3[a,b,d,e]+Tensor3[a,c,d,e];
Tensor3[a_,b_^c_,d_,e_]:=c Tensor3[a,b,d,e];
Tensor3[a_,b_/c_,d_,e_]:=Tensor3[a,b,d,e]-Tensor3[a,c,d,e];
Tensor3[a_,b_?NumberQ,c_,d_]:=0;
Tensor4[b_*c_,d__]:=Tensor4[b,d]+Tensor4[c,d];
Tensor4[b_^c_,d__]:=c Tensor4[b,d];
Tensor4[b_/c_,d__]:=Tensor4[b,d]-Tensor4[c,d];
Tensor4[b_?NumberQ,c__]:=0;

tensorExpand[x_]:=Expand[Expand[Expand[Expand[x/.tensor:>Tensor1]/.Tensor1:>Tensor2]/.Tensor2:>Tensor3]/.Tensor3:>Tensor4]/.Tensor4:>tensor;

tensorClean[x_]:=Expand[tensorExpand[((x)//.tensor[a__]:>tensor@@(Together/@{a}))//.tensor[a__]:>tensor@@(Factor/@{a})]//.tensor[a__]:>tensor@@(Sort[{#,-#}][[-1]]&/@{a})];

tensorVars[x_]:=Union[Cases[Variables[x],tensor[__]]//.tensor[a__]:>a];


(*motivic projectors*)

cleanCoproducts[x_]:=(x//.{cb2[a_]:>cb2[Together[a]],cb3[a_]:>cb3[Together[a]]})//.tensor[a_,b_]:>tensor[a,Together[b]];
cleanWedge[x_]:=((((cleanCoproducts[x]//.wedge[a_,a_]:>0)//.cb2[a_]:>If[Position[Sort[{a,1/a}],a]=={{1}},cb2[a],-cb2[1/a]])//.wedge[a___,-b_,c___]:>-wedge[a,b,c])/.wedge[cb2[a_],cb2[b_]]:>If[Position[Sort[{a,b}],a]=={{1}},wedge[cb2[a],cb2[b]],-wedge[cb2[b],cb2[a]]])/.wedge[cb2[a_],cb2[b_]]:>wedge[cb2[1/a],cb2[1/b]];

delta[n_][x_]:=tensorExpand[((x/.{tensor[a_,b_,c_,d_]:>tensor[a,b,c,d]-tensor[a,b,d,c]-tensor[b,a,c,d]+tensor[b,a,d,c]-tensor[c,d,a,b]+tensor[c,d,b,a]+tensor[d,c,a,b]-tensor[d,c,b,a]})//.{wedge[cb2[a_],cb2[b_]]:>-tensor[cb2[a],m1[n][b],b]+tensor[cb2[a],b,m1[n][b]]+tensor[cb2[b],m1[n][a],a]-tensor[cb2[b],a,m1[n][a]],tensor[cb3[a_],b_]:>tensor[cb2[a],a,b]-tensor[cb2[a],b,a], tensor[cb2[a_],wedge[b_,c_]]:>tensor[cb2[a],b,c]-tensor[cb2[a],c,b]})//.tensor[cb2[a_],b__]:>tensor[m1[n][a],a,b]-tensor[a,m1[n][a],b]];

b2b2[n_][x_]:=tensorExpand[((x/.tensor[cb3[_],_]:>0)/.wedge[cb2[a_],cb2[b_]]:>tensor[m1[n][a],a,m1[n][b],b])/.{tensor[a_,b_,c_,d_]:>tensor[a,b,c,d]-tensor[a,b,d,c]-tensor[b,a,c,d]+tensor[b,a,d,c]-tensor[c,d,a,b]+tensor[c,d,b,a]+tensor[d,c,a,b]-tensor[d,c,b,a]}];

b3c[n_][x_]:=((x/.wedge[__]:>0)//.{tensor[a_,b_,c_,d_]:>Expand[(Tensor[a,b,c,d]-Tensor[b,a,c,d]-Tensor[b,c,a,d]+Tensor[c,b,a,d]) -
	(Tensor[b,c,d,a]-Tensor[c,b,d,a]-Tensor[c,d,b,a]+Tensor[d,c,b,a]) ],tensor[cb3[a_],b_]:>Tensor[m1[n][a],a,a,b]-Tensor[a,m1[n][a],a,b]})//.Tensor:>tensor;


(*fitting to an additive basis, with p's as the default variable*)

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
fitBlind[base_]:=solveBlind[genEqs[base]];

findMaxSoln[eqs_,sampleSize_,iterations_]:=Module[{solns,solnsReduce},
solns=ParallelTable[solve[RandomSample[eqs,sampleSize]],{iterations}];
solnsReduce=Union[Sort/@DeleteCases[solns,{}]];
Return[solnsReduce];];


(*take symbol*)

(*note that due to sign issues, one should really think of these as the symbols of Li[k,-x]!!*)

symRules[n_]:={Li[1,a_]:>-Log[m1[n][a]],
Li[4,a_]:>-tensor[m1[n][a],a,a,a],Li[3,a_] Log[b_]:>-(tensor[m1[n][a],a,a,b]+tensor[m1[n][a],a,b,a]+tensor[m1[n][a],b,a,a]+tensor[b,m1[n][a],a,a]),Li[2,a_] Li[2,b_]:>tensor[m1[n][a],a,m1[n][b],b]+tensor[m1[n][a],m1[n][b],a,b]+tensor[m1[n][a],m1[n][b],b,a]+tensor[m1[n][b],b,m1[n][a],a]+tensor[m1[n][b],m1[n][a],a,b]+tensor[m1[n][b],m1[n][a],b,a],
Li[2,a_]^2:>2 tensor[m1[n][a],a,m1[n][a],a]+4 tensor[m1[n][a],m1[n][a],a,a],Li[2,a_] Log[b_] Log[c_]:>-(tensor[m1[n][a],a,b,c]+tensor[m1[n][a],a,c,b]+tensor[m1[n][a],b,a,c]+tensor[m1[n][a],b,c,a]+tensor[m1[n][a],c,b,a]+tensor[m1[n][a],c,a,b]+tensor[b,m1[n][a],a,c]+tensor[b,m1[n][a],c,a]+tensor[b,c,m1[n][a],a]+tensor[c,b,m1[n][a],a]+tensor[c,m1[n][a],b,a]+tensor[c,m1[n][a],a,b]),Li[2,a_] Log[a_] Log[b_]:>-tensor[a,b,m1[n][a],a]-tensor[a,m1[n][a],a,b]-tensor[a,m1[n][a],b,a]-tensor[b,a,m1[n][a],a]-2 (tensor[b,m1[n][a],a,a]+tensor[m1[n][a],a,a,b]+tensor[m1[n][a],a,b,a]+tensor[m1[n][a],b,a,a]),Li[2,a_] Log[b_]^2:>-2 (tensor[b,b,m1[n][a],a]+tensor[b,m1[n][a],a,b]+tensor[b,m1[n][a],b,a]+tensor[m1[n][a],a,b,b]+tensor[m1[n][a],b,a,b]+tensor[m1[n][a],b,b,a]),
Li[2,a_]Li[2,b_]:>tensor[m1[n][a], a, m1[n][b], b] + tensor[m1[n][a], m1[n][b], a, b] + tensor[m1[n][a], m1[n][b], b, a] + tensor[m1[n][b], m1[n][a], a, b] + tensor[m1[n][b], m1[n][a], b, a] + tensor[m1[n][b], b, m1[n][a], a],
Li[2,a_] Log[a_]^2:>-2 (tensor[a,a,m1[n][a],a]+2 tensor[a,m1[n][a],a,a]+3 tensor[m1[n][a],a,a,a]),Log[a_] Log[b_] Log[c_] Log[d_]:>tensor[a,b,c,d]+tensor[a,b,d,c]+tensor[a,c,b,d]+tensor[a,c,d,b]+tensor[a,d,b,c]+tensor[a,d,c,b]+tensor[b,a,c,d]+tensor[b,a,d,c]+tensor[b,c,a,d]+tensor[b,c,d,a]+tensor[b,d,a,c]+tensor[b,d,c,a]+tensor[c,a,b,d]+tensor[c,a,d,b]+tensor[c,b,a,d]+tensor[c,b,d,a]+tensor[c,d,a,b]+tensor[c,d,b,a]+tensor[d,a,b,c]+tensor[d,a,c,b]+tensor[d,b,a,c]+tensor[d,b,c,a]+tensor[d,c,a,b]+tensor[d,c,b,a],Log[a_] Log[b_] Log[c_]^2:>2 (tensor[a,b,c,c]+tensor[a,c,b,c]+tensor[a,c,c,b]+tensor[b,a,c,c]+tensor[b,c,a,c]+tensor[b,c,c,a]+tensor[c,a,b,c]+tensor[c,a,c,b]+tensor[c,b,a,c]+tensor[c,b,c,a]+tensor[c,c,a,b]+tensor[c,c,b,a]),Log[a_] Log[b_]^3:>6 (tensor[a,b,b,b]+tensor[b,a,b,b]+tensor[b,b,a,b]+tensor[b,b,b,a]),
Log[a_]^4:>24tensor[a,a,a,a],
Log[a_]^2 Log[b_]^2:>4 tensor[a,a,b,b]+4 tensor[a,b,a,b]+4 tensor[a,b,b,a]+4 tensor[b,a,a,b]+4 tensor[b,a,b,a]+4 tensor[b,b,a,a],
Li[{2,2},{a_,b_}]:> tensor[m1[n][b],m1[n][a],a,b]+tensor[m1[n][b],m1[n][a],b,a]+tensor[m1[n][b],b,m1[n][a],a]-tensor[m1[n][a b],m1[n][a],a,b]-tensor[m1[n][a b],m1[n][a],b,a]-tensor[m1[n][a b],a,m1[n][a],a]+tensor[m1[n][a b],a,a,a]+tensor[m1[n][a b],a,a,b]+tensor[m1[n][a b],a,m1[n][b],b]+tensor[m1[n][a b],a,b,a]+tensor[m1[n][a b],m1[n][b],a,b]+tensor[m1[n][a b],m1[n][b],b,a]-tensor[m1[n][a b],b,m1[n][a],a]+tensor[m1[n][a b],b,a,a]+tensor[m1[n][a b],b,m1[n][b],b],
Li[{3,1},{x_,y_}]:>tensor[m1[n][y], m1[n][x], x, x] + tensor[m1[n][-x y], x, x, x] + 
 tensor[m1[n][-x y], x, x, m1[n][y]] + tensor[m1[n][-x y], x, y, m1[n][y]] + 
 tensor[m1[n][-x y], x, m1[n][y], x] + tensor[m1[n][-x y], y, x, m1[n][y]] + 
 tensor[m1[n][-x y], y, y, m1[n][y]] + tensor[m1[n][-x y], y, m1[n][y], x] - 
 tensor[m1[n][-x y], m1[n][x], x, x] + tensor[m1[n][-x y], m1[n][y], x, x],
Li[{1,3},{x_,y_}]:>tensor[m1[n][y], y, y, m1[n][x]] + tensor[m1[n][y], y, m1[n][x], y] + 
 tensor[m1[n][y], m1[n][x], y, y] + tensor[m1[n][-x y], x, x, x] + 
 tensor[m1[n][-x y], x, x, y] - tensor[m1[n][-x y], x, x, m1[n][x]] + 
 tensor[m1[n][-x y], x, y, x] + tensor[m1[n][-x y], x, y, y] - 
 tensor[m1[n][-x y], x, y, m1[n][x]] - tensor[m1[n][-x y], x, m1[n][x], y] + 
 tensor[m1[n][-x y], y, x, x] + tensor[m1[n][-x y], y, x, y] - 
 tensor[m1[n][-x y], y, x, m1[n][x]] + tensor[m1[n][-x y], y, y, x] - 
 tensor[m1[n][-x y], y, y, m1[n][x]] - tensor[m1[n][-x y], y, m1[n][x], y] - 
 tensor[m1[n][-x y], m1[n][x], y, y] + tensor[m1[n][-x y], m1[n][y], y, y],
tensor[a___]dLog[b_]:>tensor[a,b]};

sym[n_][x_]:=tensorExpand[(x//.Pi:>0)//.symRules[n]];


(*calculate the poisson bracket of two cluster x-coordinate*)

sklyaninZ[n_]:=sklyaninZ[n]=Transpose[RowReduce[Transpose[Z[n]]]];
sklyaninZPartial[n_][i_,j_]:=sklyaninZPartial[n][i,j]=Insert[Delete[sklyaninZ[n],{i,j}],Y[i,j],{i,j}];
brNumPartial[n_][i_,j_][a_,b_,c_,d_]:=brNumPartial[n][i,j][a,b,c,d]=Det[{sklyaninZPartial[n][i,j][[a]],sklyaninZPartial[n][i,j][[b]],sklyaninZPartial[n][i,j][[c]],sklyaninZPartial[n][i,j][[d]]}];
dFast[n_][f_,Y[i_,j_]]:=dFast[n][f,Y[i,j]]=D[(f)//.br:>brNumPartial[n][i,j],Y[i,j]]//.Y[i,j]:>sklyaninZ[n][[i,j]];
sklyanin[n_][F_,G_]:=Module[{f,g,base},
f=(F//.{ccap1:>cap1,ccap:>cap})//.sort;
g=(G//.{ccap1:>cap1,ccap:>cap})//.sort;
base=-Sum[If[Sign[a-i]-Sign[b-j]==0,0,dFast[n][f,Y[i,j]]dFast[n][g,Y[a,b]](Sign[a-i]-Sign[b-j])sklyaninZ[n][[i,b]]sklyaninZ[n][[a,j]]],{i,n},{j,4},{a,n},{b,4}]/(2num[n][F G]);
Return[base]];


(*cluster polylogarithm code*)

(*first, clean up ratios*)

clean[n_][R_]:=clean[n][R]=With[{x=Position[numRatios[n],num[n][R]]},If[Length[x]==1,allRatios[n][[x[[1,1]]]],Print["bad ratio encountered"];Abort[];]];

a2Vars[n_][{x1_,x2_}]:=Module[{x,answer},
x[1]=x1;
x[2]=x2;
x[3]=clean[n][(1+x2)/x1];
x[4]=clean[n][((1+x2+x1 )/(x1 x2))];
x[5]=clean[n][(1+x1)/x2];
Return[x/@Range[5]];];

a2B2b2[n_][{x1_,x2_}]:=Module[{x,answer},
x[1]=x1;
x[2]=x2;
x[3]=clean[n][(1+x2)/x1];
x[4]=clean[n][((1+x2+x1 )/(x1 x2))];
x[5]=clean[n][(1+x1)/x2];
answer=-3 wedge[cb2[x[1]],cb2[x[2]]]-wedge[cb2[x[1]],cb2[x[3]]]+wedge[cb2[x[1]],cb2[x[4]]]+3 wedge[cb2[x[1]],cb2[x[5]]]-3 wedge[cb2[x[2]],cb2[x[3]]]-wedge[cb2[x[2]],cb2[x[4]]]+wedge[cb2[x[2]],cb2[x[5]]]-3 wedge[cb2[x[3]],cb2[x[4]]]-wedge[cb2[x[3]],cb2[x[5]]]-3 wedge[cb2[x[4]],cb2[x[5]]];
Return[answer];];

a2B3c[n_][{x1_,x2_}]:=Module[{x,answer},
x[1]=x1;
x[2]=x2;
x[3]=clean[n][(1+x2)/x1];
x[4]=clean[n][((1+x2+x1 )/(x1 x2))];
x[5]=clean[n][(1+x1)/x2];
x[6]=x[1];
answer=5 Sum[tensor[cb3[x[i+1]],x[i]]-tensor[cb3[x[i]],x[i+1]],{i,5}];
Return[answer];];


(*pretty printer code*)

brPrint/:MakeBoxes[brPrint[i_,j_,k_,l_],fmt_:StandardFormat]:=With[{a=i 1000+j 100+k 10+l},RowBox[{"\[LeftAngleBracket]",MakeBoxes[a,fmt],"\[RightAngleBracket]"}]];
ccap1Print/:MakeBoxes[ccap1Print[i_,tup[j_,k_],tup[l_,m_],tup[n_,o_]],fmt_:StandardFormat]:=RowBox[{"\[LeftAngleBracket]",MakeBoxes[i,fmt],"(",MakeBoxes[j,fmt],"\[InvisibleComma]",MakeBoxes[k,fmt],")","(",MakeBoxes[l,fmt],"\[InvisibleComma]",MakeBoxes[m,fmt],")","(",MakeBoxes[n,fmt],"\[InvisibleComma]",MakeBoxes[o,fmt],")","\[RightAngleBracket]"}];
ccapPrint/:MakeBoxes[ccapPrint[i_,j_,tup[k_,l_,m_],tup[n_,o_,p_]],fmt_:StandardFormat]:=RowBox[{"\[LeftAngleBracket]",MakeBoxes[i,fmt],"\[InvisibleComma]",MakeBoxes[j,fmt],"(",MakeBoxes[k,fmt],"\[InvisibleComma]",MakeBoxes[l,fmt],"\[InvisibleComma]",MakeBoxes[m,fmt],")","\[Intersection]","(",MakeBoxes[n,fmt],"\[InvisibleComma]",MakeBoxes[o,fmt],"\[InvisibleComma]",MakeBoxes[p,fmt],")","\[RightAngleBracket]"}];

print[x_]:=TeXForm[(x//.Li[a_,b_]:>Subscript[Li, a][b])//.{br:>brPrint,ccap1:>ccap1Print,ccap:>ccapPrint,wedge[a_,b_]:>a\[Wedge]b,cb2[a_]:>Subscript[{a}, 2],V[a_?NumberQ,b_]:>Subscript[v, 10a+b],V[a_?NumberQ,b_,c_]:>Subscript[v, 100a+10b+c],Z[a_?NumberQ,b_]:>Subscript[z, 10a+b],Zm[a_?NumberQ,b_,c_]:>Subscript[SuperMinus[z], 100a+10b+c]}];

nice[exprn_]:=exprn/.{br[x___]:>Row[List["\[LeftAngleBracket]",{x}[[1]],"\[ThinSpace]",{x}[[2]],"\[ThinSpace]",{x}[[3]],"\[ThinSpace]",{x}[[4]],"\[RightAngleBracket]"]],ccap[i_,j_,tup[k__],tup[m__]]:>Row[List["\[LeftAngleBracket]",i,j,"(",k,")\[Intersection](",m,")\[RightAngleBracket]"]],ccap1[i_,tup[j__],tup[k__],tup[m__]]:>Row[List["\[LeftAngleBracket]",j,"(",k,i,")\[Intersection](",m,i,")\[RightAngleBracket]"]]}/.{tensor[x___]:>Row[Riffle[{x},Style["\[CircleTimes]",FontSize->16]]]}/.{i-1:>"i-1",i+1:>"i+1",j-1:>"j-1",j+1:>"j+1",k-1:>"k-1",k+1->"k+1",k+2->"k+2"}; 


(* collinear limit -- note that this IGNORES SIGN*)

SetAttributes[Br,Orderless];
SetAttributes[Ccap1,Orderless];
SetAttributes[Ccap,Orderless];
SetAttributes[Tup,Orderless];

collinearLimit[n_]:=collinearLimit[n]={Ccap[ii_,jj_,Tup[ii_,kk_,ll_],Tup[oo_,mm_,nn_]]->Br[ii,oo,mm,nn] Br[ii,jj,kk,ll],Ccap[kk_,ll_,Tup[ii_,jj_,mm_],Tup[ii_,jj_,nn_]]->Br[ii,jj,mm,nn] Br[ii,jj,kk,ll],Ccap[kk_,ll_,Tup[ii_,jj__],Tup[ii_,mm__]]->Ccap1[ii,Tup[kk,ll],Tup[jj],Tup[mm]],Ccap1[ii_,Tup[jj_,kk_],Tup[jj_,ll_],Tup[mm__]]->Br[ii,jj,kk,ll] Br[ii,jj,mm],Br[n-1,n,ii__]->Br[n-1,B,ii],Br[n,ii__]->Br[n-1,ii],Br[n-1,B,n-2,1]->XXX,Br[n-1,B,n-2,ii_]->E1*Br[n-2,n-1,1,ii],Br[n-1,B,1,ii_]->E2*Br[n-2,n-1,1,ii],Ccap[n-1,n,Tup[n-2,1,ii_],Tup[jj__]]->Br[n-2,n-1,1,ii] Br[B,jj],Ccap[n-2,1,Tup[n-1,n,jj_],Tup[kk__]]->Br[n-2,n-1,1,jj] Br[B,kk],Ccap[n-1,n,Tup[ii__],Tup[jj__]]->Ccap[n-1,B,Tup[ii],Tup[jj]],Ccap[ii__,Tup[n-1,n,jj_],Tup[kk__]]->Ccap[ii,Tup[n-1,B,jj],Tup[kk]],Ccap[n,ii__]->Ccap[n-1,ii],Ccap[ii__,Tup[n,jj__],Tup[kk__]]->Ccap[ii,Tup[n-1,jj],Tup[kk]],Ccap1[ii_,Tup[n-1,n],Tup[jj__],Tup[kk__]]->Ccap1[ii,Tup[n-1,B],Tup[jj],Tup[kk]],Ccap1[n-1,Tup[n,ii_],Tup[jj__],Tup[kk__]]->Ccap1[n-1,Tup[B,ii],Tup[jj],Tup[kk]],Ccap1[n,Tup[n-1,ii_],Tup[jj__],Tup[kk__]]->Ccap1[n-1,Tup[B,ii],Tup[jj],Tup[kk]],Ccap1[n,jj__]->Ccap1[n-1,jj],Ccap1[Tup[n,jj_],kk__]->Ccap1[Tup[n-1,jj],kk],Ccap[ii__,Tup[n-2,n-1,B],Tup[jj__]]->E1*Ccap[ii,Tup[n-2,n-1,1],Tup[jj]],Ccap[ii__,Tup[1,n-1,B],Tup[jj__]]->E2*Ccap[ii,Tup[n-2,n-1,1],Tup[jj]],Ccap1[n-2,Tup[n-1,B],Tup[jj__],Tup[kk__]]->E1*Ccap1[n-2,Tup[n-1,1],Tup[jj],Tup[kk]],Ccap1[1,Tup[n-1,B],Tup[jj__],Tup[kk__]]->E2*Ccap1[1,Tup[n-1,n-2],Tup[jj],Tup[kk]],Ccap1[ii_,Tup[jj_,B],Tup[n-2,1],Tup[kk__]]->Br[ii,n-2,1,jj] Br[ii,B,kk],Ccap1[n-1,Tup[B,n-2],Tup[jj__],Tup[kk__]]->E1*Ccap1[n-1,Tup[n-2,1],Tup[jj],Tup[kk]],Ccap1[n-1,Tup[B,1],Tup[jj__],Tup[kk__]]->E2*Ccap1[n-1,Tup[n-2,1],Tup[jj],Tup[kk]],Ccap1[n-1,Tup[B,ii_],Tup[n-2,1],Tup[jj__]]->Br[n-1,n-2,1,ii] Br[n-1,B,jj]};

collinear[n_][x_]:=((x//.{br:>Br,ccap1:>Ccap1,ccap:>Ccap,tup:>Tup})//.collinearLimit[n])//.{Br:>br,Ccap1:>ccap1,Ccap:>ccap,Tup:>tup};


(*assorted*)

(*all values i<j<k mod n*)
ijkVals[n_]:=ijkVals[n]=Union[Flatten[Table[NestList[Mod[#+1,n,1]&/@#&,{i,j,k},n-1],{i,n},{j,i+1,n},{k,j+1,n}],3]];

(*find conformal weight of an object*)
confWeight[a_]:=Sort[Flatten[Variables[a]//.{br[x__]:>{x},ccap[x__]:>{x},ccap1[x_,y__]:>{x,x,y},tup[x__]:>{x}}]];


(*the b2b2 formula*)

b2b2Base[7]=-wedge[cb2[V[1,4]],cb2[Z[1,5]]]-wedge[cb2[V[1,3,5]],cb2[Zm[1,3,5]]]-wedge[cb2[V[1,3,5]],cb2[Zm[6,1,3]]];
b2b2Full[7]=Expand[1/2addSym[7][b2b2Base[7]/.cExp[7]]];


(* load clean tools, without knowledge of amplitudes or Gr(4,n)*)

cleanTools:=Module[{},
Clear[a2B2b2,a2B3c,a2Vars,addConj,addCyclic,addDihedral,addFlip,addSym,allRatios,b2b2,b3c,brNum,brNumPartial,brPrint,cap,cap1,capAlt,cBasis,cBasisSym,ccap1Print,ccapPrint,cExp,clean,cleanWedge,collinear,collinearLimit,confWeight,conjRules,conjugate,cycle,delta,dFast,expand,findMaxSoln,fit,fitBlind,fixCCAP,flip,genEqs,genEqsFull,goodBrs,goodCap1s,goodCaps,goodFirst,goodLast,goodLetters,ijkVals,length,listSym,m1,mod,MOD,multBasis,multLength,multSolveFull,nice,num,numPG,numRand,numRatios,numXcoords,print,rangeV,rangeZ,rangeZm,ratios,schoutens,sklyanin,sklyaninZ,sklyaninZPartial,solve,sort,sortInverse,sortMinus,sym,symbol,symRules,Tensor,Tensor1,Tensor2,Tensor3,Tensor4,tensorClean,tensorExpand,tensorExpandFull,tensorVars,vars,vP1,vP1s,vs,vSym,xcoords,z,Z,zm,Zm,zmP1,zp,Zp,zP1,zP1s,zpP1,zs,zSym];
oldDir=Directory[];
SetDirectory[Directory[]<>"/packages"];
<<polylogs_clean.m;
SetDirectory[oldDir]];


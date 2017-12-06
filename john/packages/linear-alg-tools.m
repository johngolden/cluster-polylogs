(* ::Package:: *)

imposeBase[ansatz_,zero_]:=Module[{soln,tmp},
soln=fit[zero];
If[soln==={0->0},Print["already satisfied"];Return[ansatz]];If[soln==={},Print["no solutions"];Return[{}]];
PrintTemporary["found solution of length "<>ToString[Length[soln]]<>", now imposing"];
tmp=collectPs[ansatz]/.soln;
If[tmp===0,Print["trivial solution"];Return[0]];
PrintTemporary["expanding"];
tmp=collectTensors[tmp]/.a_*tensor[b__]:>Expand[a]tensor[b];
varBase=Variables[tmp];
Print["finished with "<>ToString[Length[Cases[varBase,p[_]]]]<>" variables and "<>ToString[Length[Cases[varBase,tensor[___]]]]<>" tensors"];
Return[tmp];];

imposeSym[ansatz_,{sym_,sign_}]:=Module[{vars,tensorRep,ansatzSym,soln,lenP,lenTensors,tmp,varBase},
varBase=Variables[ansatz];
lenP=Length[Cases[varBase,p[_]]];
lenTensors=Length[Cases[varBase,tensor[___]]];
Print["started with "<>ToString[lenP]<>" variables and "<>ToString[lenTensors]<>" tensors"];
vars=tensorVars[ansatz];
tensorRep=Thread[vars->Together[vars/.sym]]/.(a_->b_):>(tensor[x___,a,y___]:>tensor[x,done[b],y]) ;
ansatzSym=collectTensors[tensorClean[collectTensors[tensorExpand[(ansatz//.tensorRep)//.done[a_]:>a]]]];
Which[sign===P,func=Subtract,sign===M,func=Plus];
answer=imposeBase[ansatz,func[ansatz,ansatzSym]];
Return[answer];];

imposeTarget[ansatz_,{ansatzImg_,target_}]:=Module[{varBase},
varBase=Variables[ansatz];
lenP=Length[Cases[varBase,p[_]]];
lenTensors=Length[Cases[varBase,tensor[___]]];
Print["started with "<>ToString[lenP]<>" variables and "<>ToString[lenTensors]<>" tensors"];Return[imposeBase[ansatz,ansatzImg-target]];];

(* Mathematica for https://arxiv.org/abs/2010.09152   Oliver Knill         *)
G={{1},{2},{3},{4},{1,2},{2,3},{3,4}}; n=Length[G]; e = {x,y,z,a,b,c,d};
L0=Table[If[Equal[Intersection[G[[k]],G[[l]]],{}],0,1],{k,n},{l,n}];
energy[A_]:=If[A=={},0,Sum[e[[Position[G,A[[k]]][[1,1]]]],{k,Length[A]}]];
S=Table[-(-1)^Length[G[[k]]]*If[k ==l,1,0],{k,n},{l,n}];  w[k_]:=S[[k,k]];
star[x_]:=Module[{u={}},Do[v=G[[k]];If[SubsetQ[v,x],u=Append[u,v]],{k,n}];u];
core[x_]:=Module[{u={}},Do[v=G[[k]];If[SubsetQ[x,v],u=Append[u,v]],{k,n}];u];
star1[x_]:=Module[{u={}},Do[v=G[[k]];If[SubsetQ[v,x],u=Append[u,k]],{k,n}];u];
closure[x_]:=Union[Flatten[Table[star1[G[[x[[k]]]]],{k,Length[x]}]]];
Wminus    = Table[Intersection[core[G[[k]]],core[G[[l]]]],{k,n},{l,n}];
Wplus     = Table[Intersection[star[G[[k]]],star[G[[l]]]],{k,n},{l,n}];
Lminus    = Table[energy[Wminus[[k,l]]],    {k,n},{l,n}];  L  =      Lminus;
Lplus     = Table[energy[Wplus[[k,l]]],     {k,n},{l,n}];  g  =   S.Lplus.S;
Fredholm  = Chop[Det[IdentityMatrix[n]+g.g]];
Wu        = Sum[e[[k]] e[[l]] L0[[k,l]],{k,n},{l,n}];
Mo        = Sum[g[[k,l]]^2 w[k] w[l] ,{k,n},{l,n}];
Print["Theorem 1: ",   Simplify[energy[G]-Sum[g[[k,l]],{k,n},{l,n}]]];
Print["Theorem 2: ",Simplify[Wu  - Mo]];
Print["Theorem 3: ",Product[e[[k]],{k,n}] - Det[g]];
Print["Theorem 4: ",MatrixForm[Simplify[g.L]]];


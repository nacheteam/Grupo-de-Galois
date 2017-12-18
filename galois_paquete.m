BeginPackage["GrupoGalois`"]

GrupoGalois2::usage = "Calcula el grupo de Galois de un polinomio de grado 2."

Begin["Privado`"]

GrupoGalois2[f_, k_] := If[IrreduciblePolynomialQ[f, k],
   						Return["El grupo de Galois es S2"],
   						Return["El grupo de Galois es A2"]];

FactoresFormateados[f_,k_]:=Module[{lista,factores},factores=FactorList[f,k];
							lista=List[];
						    Do[AppendTo[lista,Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
                        	SortBy[lista,Exponent[#1,x]<Exponent[#2,x]];
                        	Return[lista]];

ObtenerFactorBueno2[f_,k_]:=Module[{factores,mayor=1},factores=FactorList[f,k];
							Do[If[factores[[i]][[1]]!=1 \[And] factores[[i]][[2]]>factores[[mayor]][[2]],mayor=i],{i,1,Length[factores]}];
							If[factores[[mayor]][[2]]==3,
								Return[factores[[mayor]][[1]]^2],
								Return[factores[[mayor]][[1]]^factores[[mayor]][[2]]]]];

GrupoGalois3[f_,k_,x_]:=If[IrreduciblePolynomialQ[f,k],
						If[IrreduciblePolynomialQ[x^2-Discriminant[f,x],k],
							Return["El grupo de Galois es S3"],
							Return["El grupo de Galois es A3"]],
						Module[{factores},factores=FactorList[f,k];
							GrupoGalois2[ObtenerFactorBueno2[f,k],k]]];

End[]

EndPackage[]

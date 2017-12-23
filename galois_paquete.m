BeginPackage["GrupoGalois`"]

GrupoGalois2General::usage = "Calcula el grupo de Galois de un polinomio de grado 2 en un cuerpo K de característica distinta de 2."
GrupoGalois2::usage = "Calcula el grupo de Galois de un polinomio de grado 2 sobre Q."
FactoresFormateados::usage = "Devuelve la lista de factores ordenados por grado."
ObtenerFactorBueno2General::usage = "Devuelve el factor de grado 2 en el caso general."
ObtenerFactorBueno2::usage = "Devuelve el factor de grado 2 en el caso de Q."
GrupoGalois3General::usage = "Devuelve el grupo de Galois de un polinomio de grado 3 en un cuerpo k de característica distinta de 2."
GrupoGalois3::usage = "Devuelve el grupo de Galois de un polinomio de grado 3 en el caso de Q."
ResolventeCubica::usage = "Devuelve la resolvente cúbica de un polinomio de grado 4 en el caso de Q."
CompruebaDescomposicionGrado3::usage = "Devuelve True si tiene un factor de grado 3 el polinomio dado (debe ser de grado 4)."

Begin["Privado`"]

GrupoGalois2General[f_, k_] := If[IrreduciblePolynomialQ[f, k],
   						           Print["El grupo de Galois es S2"],
   						           Print["El grupo de Galois es A2"]];

GrupoGalois2[f_]:=If[IrreduciblePolynomialQ[f],
                    Print["El grupo de Galois es S2"],
                    Print["El grupo de Galois es A2"]];

FactoresFormateados[f_,k_]:=Module[{lista,factores},factores=FactorList[f,k];
    							lista=List[];
    							Do[AppendTo[lista,Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
    							SortBy[lista,Exponent[#1,x]<Exponent[#2,x]];
    							Return[lista]];

ObtenerFactorBueno2General[f_,k_]:=Module[{factores,mayor=1},factores=FactorList[f,k];
    							     Do[If[factores[[i]][[1]]!=1 \[And] factores[[i]][[2]]>factores[[mayor]][[2]],mayor=i],{i,1,Length[factores]}];
                                     If[factores[[mayor]][[2]]==3,
                                     Return[factores[[mayor]][[1]]^2],
                                     Return[factores[[mayor]][[1]]^factores[[mayor]][[2]]]]];

ObtenerFactorBueno2[f_]:=Module[{factores,mayor=1},factores=FactorList[f];
 							Do[If[factores[[i]][[1]]!=1 \[And] factores[[i]][[2]]>factores[[mayor]][[2]],mayor=i],{i,1,Length[factores]}];
 							If[factores[[mayor]][[2]]==3,
							Return[factores[[mayor]][[1]]^2],
							Return[factores[[mayor]][[1]]^factores[[mayor]][[2]]]]];

GrupoGalois3General[f_,k_,x_]:=If[IrreduciblePolynomialQ[f,k],
        						  If[IrreduciblePolynomialQ[x^2-Discriminant[f,x],k],
                                    Return["El grupo de Galois es S3"],
            					    Return["El grupo de Galois es A3"]],
                				Module[{factores},factores=FactorList[f,k];
                                    GrupoGalois2General[ObtenerFactorBueno2General[f,k],k]]];

GrupoGalois3[f_,x_]:=If[IrreduciblePolynomialQ[f],
						If[IrreduciblePolynomialQ[x^2-Discriminant[f,x]],
							Return["El grupo de Galois es S3"],
							Return["El grupo de Galois es A3"]],
						Module[{factores},factores=FactorList[f];
						GrupoGalois2[ObtenerFactorBueno2[f]]]];


ResolventeCubica[f_,x_]:=Module[{coefs,b0,b1,b2},coefs=CoefficientList[f,x];
							b2=-coefs[[3]];
							b1=coefs[[2]]*coefs[[4]]-4*coefs[[1]];
						    b0=-coefs[[4]]^2*coefs[[1]]+4*coefs[[3]]*coefs[[1]]-coefs[[2]]^2;
                    	    Return[x^3+b2*x^2+b1*x+b0]];

CompruebaDescomposicionGrado3[f_,x_]:=Module[{factores,descomponeBien},factores=FactorList[f];
    										descomponeBien=False;
    										Do[If[Exponent[factores[[i]][[1]]^factores[[i]][[2]],x]==3,descomponeBien=True],{i,1,Length[factores]}];
										    Return[descomponeBien]];

End[]

EndPackage[]

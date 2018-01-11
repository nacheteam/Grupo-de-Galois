(* ::Package:: *)

(* ::Title:: *)
(*C\[AAcute]lculo del Grupo de Galois*)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 2*)


(* ::Input:: *)
(*Off[$RecursionLimit::reclim2]*)


(* ::Text:: *)
(*Calculo del grupo de Galois en cualquier cuerpo.*)


GrupoGalois2General[f_, k_] := If[IrreduciblePolynomialQ[f, k],
   						Print["El grupo de Galois es S2"],
   						Print["El grupo de Galois es A2"]];


(* ::Text:: *)
(*Calculo del grupo de Galois en Q.*)


GrupoGalois2[f_]:=If[IrreduciblePolynomialQ[f],
					Print["El grupo de Galois es S2"],
					Print["El grupo de Galois es A2"]];


(* ::Input:: *)
(*GrupoGalois2[x^2+x+1]*)
(*GrupoGalois2[x^2-1]*)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 3*)


(* ::Text:: *)
(*Devuelve los factores ordenados de menor a mayor por grado.*)


FactoresFormateados[f_,k_]:=Module[{lista,factores},factores=FactorList[f,k];
							lista=List[];
							Do[AppendTo[lista,Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
							SortBy[lista,Exponent[#1,x]<Exponent[#2,x]];
							Return[lista]];


(* ::Text:: *)
(*Devuelve el factor de grado 2 en el caso general.*)


ObtenerFactorBueno2General[f_,k_]:=Module[{factores,mayor=1},factores=FactorList[f,k];
							Do[If[factores[[i]][[1]]!=1 \[And] factores[[i]][[2]]>factores[[mayor]][[2]],mayor=i],{i,1,Length[factores]}];
							If[factores[[mayor]][[2]]==3,
								Return[factores[[mayor]][[1]]^2],
								Return[factores[[mayor]][[1]]^factores[[mayor]][[2]]]]];


(* ::Text:: *)
(*Devuelve el factor de grado 2 en el caso de Q.*)


ObtenerFactorBueno2[f_]:=Module[{factores,mayor=1},factores=FactorList[f];
							Do[If[factores[[i]][[1]]!=1 \[And] factores[[i]][[2]]>factores[[mayor]][[2]],mayor=i],{i,1,Length[factores]}];
							If[factores[[mayor]][[2]]==3,
								Return[factores[[mayor]][[1]]^2],
								Return[factores[[mayor]][[1]]^factores[[mayor]][[2]]]]];


(* ::Text:: *)
(*Calculo del grupo de Galois con un polinomio de grado 3 en el caso general.*)


GrupoGalois3General[f_,k_,x_]:=If[IrreduciblePolynomialQ[f,k],
						If[IrreduciblePolynomialQ[x^2-Discriminant[f,x],k],
							Print["El grupo de Galois es S3"],
							Print["El grupo de Galois es A3"]],
						Module[{factores},factores=FactorList[f,k];
							GrupoGalois2General[ObtenerFactorBueno2General[f,k],k]]];


(* ::Text:: *)
(*Calculo del grupo de Galois con un polinomio de grado 3 en el caso de Q.*)


GrupoGalois3[f_,x_]:=If[IrreduciblePolynomialQ[f],
						If[IrreduciblePolynomialQ[x^2-Discriminant[f,x]],
							Print["El grupo de Galois es S3"],
							Print["El grupo de Galois es A3"]],
						Module[{factores},factores=FactorList[f];
							GrupoGalois2[ObtenerFactorBueno2[f]]]];


(* ::Input:: *)
(*GrupoGalois3[x^3+2x^2+x+1,x]*)
(*GrupoGalois3[2x^3-5x^2-2x-12,x]*)
(*GrupoGalois3[x^3-1,x]*)
(*GrupoGalois3[(2x^2+3x+1)*(2x+1),x]*)
(*GrupoGalois3[x^3-2x^2-x+1,x]*)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 4*)


(* ::Text:: *)
(*Funcion que devuelve la resolvente cubica.*)


ResolventeCubica[f_,x_]:=Module[{coefs,b0,b1,b2},coefs=CoefficientList[f,x];
							b2=-coefs[[3]];
							b1=coefs[[2]]*coefs[[4]]-4*coefs[[1]];
							b0=-coefs[[4]]^2*coefs[[1]]+4*coefs[[3]]*coefs[[1]]-coefs[[2]]^2;
							Return[x^3+b2*x^2+b1*x+b0]];


(* ::Text:: *)
(*Comprueba si tiene una raiz.*)


CompruebaDescomposicionGrado3[f_,x_]:=Module[{factores,descomponeBien},factores=FactorList[f];
										descomponeBien=False;
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,descomponeBien=True],{i,1,Length[factores]}];
										Return[descomponeBien]];


(* ::Text:: *)
(*Devuelve el factor de grado 3.*)


DevuelvePolinomioGrado3[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
										Return[factorBueno]];


(* ::Text:: *)
(*Comprueba si tiene un factor de grado 2.*)


CompruebaDescomposicionGrado2[f_,x_]:=Module[{factores,descomponeBien},factores=FactorList[f];
										descomponeBien=False;
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,descomponeBien=True],{i,1,Length[factores]}];
										Return[descomponeBien]];


(* ::Text:: *)
(*Devuelve el factor de grado 2.*)


DevuelvePolinomioGrado2[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
										Return[factorBueno]];


(* ::Text:: *)
(*Comprueba si Sqrt[Discriminant[g1]*Discriminant[g2]] esta en Q siendo g1 y g2 dos factores de grado 2.*)


CompruebaDiscriminantes[f_,x_]:=Module[{g1,g2},g1=DevuelvePolinomioGrado2[f,x];
										g2=Divide[f,g1];
										If[IrreduciblePolynomialQ[x^2-(Discriminant[g1,x]*Discriminant[g2,x])],Return[False],Return[True]]];


(* ::Text:: *)
(*Comprueba si la resolvente cubica es irreducible.*)


CompruebaResolventeIrreducible[f_,x_]:=Module[{resolvente},resolvente=ResolventeCubica[f,x];
											If[IrreduciblePolynomialQ[resolvente],Return[True],Return[False]]];


(* ::Text:: *)
(*Comprueba si Sqrt[Discriminant[resolvente]] esta en Q.*)


CompruebaDiscriminanteResolvente[f_,x_]:=Module[{resolvente},resolvente=ResolventeCubica[f,x];
												If[IrreduciblePolynomialQ[(x^2-Discriminant[ResolventeCubica[f,x],x])],Return[False],Return[True]]];


(* ::Text:: *)
(*Comprueba si la resolvente tiene tres ra\[IAcute]ces.*)


CompruebaTresRaicesResolvente[f_,x_]:=Module[{resolvente,factores,condicion,suma},resolvente=ResolventeCubica[f,x];
											factores=FactorList[resolvente];
											If[Length[factores]==4,condicion=True,condicion=False];
											Do[suma=suma+factores[[i]][[2]],{i,2,Length[factores]}];
											If[suma==3,condicion=True,condicion=False];
											Return[condicion]];


(* ::Text:: *)
(*Devuelve una lista con los betas necesarios.*)


ObtenerBetasSoluciones[f_,x_]:=Module[{soluciones,solucionesFormateadas,beta1,beta2,beta3},soluciones=Solve[f==0,x];
								solucionesFormateadas=List[];
								Do[AppendTo[solucionesFormateadas,soluciones[[i]][[1]][[2]]],{i,1,Length[soluciones]}];
								beta1=solucionesFormateadas[[1]]*solucionesFormateadas[[2]] + solucionesFormateadas[[3]]*solucionesFormateadas[[4]];
								beta2=solucionesFormateadas[[1]]*solucionesFormateadas[[3]] + solucionesFormateadas[[2]]*solucionesFormateadas[[4]];
								beta3=solucionesFormateadas[[1]]*solucionesFormateadas[[4]] + solucionesFormateadas[[2]]*solucionesFormateadas[[3]];
								Return[List[List[beta1,beta2,beta3],solucionesFormateadas]]];


(* ::Text:: *)
(*Comprueba la condici\[OAcute]n de los alphas y betas con la extensi\[OAcute]n por la raiz del discriminante.*)


ComprobarBetasAlphas[f_,x_]:=Module[{betas,alphas,discriminante,condicion},betas=ObtenerBetasSoluciones[f,x][[1]];
									alphas=ObtenerBetasSoluciones[f,x][[2]];
									discriminante=Discriminant[f,x];
									condicion=True;
									If[IrreduciblePolynomialQ[x-(alphas[[1]]+alphas[[3]]),Extension->Sqrt[discriminante]],condicion=False,];
									If[IrreduciblePolynomialQ[x-(alphas[[1]]*alphas[[3]]),Extension->Sqrt[discriminante]],condicion=False,];
									If[IrreduciblePolynomialQ[x-(alphas[[2]]+alphas[[4]]),Extension->Sqrt[discriminante]],condicion=False,];
									If[IrreduciblePolynomialQ[x-(alphas[[2]]*alphas[[4]]),Extension->Sqrt[discriminante]],condicion=False,];
									Return[condicion]];


(* ::Text:: *)
(*C\[AAcute]lculo del grupo de Galois de un polinomio de grado 4.*)


GrupoGalois4[f_,x_]:=Module[{},If[CompruebaDescomposicionGrado3[f,x],
									GrupoGalois3[DevuelvePolinomioGrado3[f,x]],
									If[CompruebaDescomposicionGrado2[f,x],
										If[CompruebaDiscriminantes[f,x],
											Print["El grupo de Galois es <(12)(34)>"],
											Print["El grupo de Galois es <(12),(34)>"]],
										If[CompruebaResolventeIrreducible[f,x],
											If[!CompruebaDiscriminanteResolvente[f,x],
												Print["El grupo de Galois de f es S4 y el de la resolvente S3"],
												Print["El grupo de Galois de f es A4 y el de la resolvente S4"]],
											If[CompruebaTresRaicesResolvente[f,x],
												Print["El grupo de Galois de f es V = <(12)(34),(13)(24)> y el de la resolvente es el trivial."],
												If[ComprobarBetasAlphas[f,x],
													Print["El grupo de Galois de f es C4 y el de la resolvente es C2"],
													Print["El grupo de Galois de f es D4 y el de la resolvente es C2"]]]]]]];


(* ::Input:: *)
(*GrupoGalois4[x^4-x-1,x]*)
(*GrupoGalois4[x^4+8x+12,x]*)
(*FactorList[x^4+3x+3]*)
(*GrupoGalois4[x^4+3x+3,x]*)
(*GrupoGalois4[x^4-2,x]*)
(*GrupoGalois4[x^4-2x^2-2,x]*)
(*GrupoGalois4[x^4+36x+63,x]*)
(*GrupoGalois4[x^4-3x^2+4,x]*)


(* ::Input:: *)
(**)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 5*)


(* ::Text:: *)
(*Comprueba si el polinomio tiene una ra\[IAcute]z de grado 1.*)


ComprobarUnaRaizGrado5[f_,x_]:=Module[{factores,condicion},factores=FactorList[f];
								condicion=False;
								Do[If[Exponent[factores[[i]][[1]],x]==1,condicion=True,],{i,1,Length[factores]}];
								Return[condicion]];


(* ::Text:: *)
(*Obtiene el factor de grado 4 en el caso de que tenga una ra\[IAcute]z.*)


DevuelveFactorGrado4[f_,x_]:=Module[{factores,factorGrado1,factorGrado4},factores=FactorList[f];
									Do[If[Exponent[factores[[i]][[1]],x]==1,factorGrado1=factores[[i]][[1]],],{i,1,Length[factores]}];
									factorGrado4=Divide[f,factorGrado1];
									Return[factorGrado4]];


(* ::Text:: *)
(*Comprueba si descompone como un polinomio de grado 2 por uno de grado 3.*)


ComprobarDescomposicion2y3[f_,x_]:=Module[{factores,condicion1,condicion2},factores=FactorList[f];
									condicion1=False;
									condicion2=False;
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,condicion1=True,];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,condicion2=True,],{i,1,Length[factores]}];
									Return[condicion1\[And]condicion2]];


(* ::Text:: *)
(*Si descompone como un polinomio de grado 2 por uno de grado 3 devuelve el factor de grado 2.*)


DevuelveFactorGrado2[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]],],{i,1,Length[factores]}];
									Return[factorBueno]];


(* ::Text:: *)
(*Si descompone como un polinomio de grado 2 por uno de grado 3 devuelve el factor de grado 3.*)


DevuelveFactorGrado3[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]],],{i,1,Length[factores]}];
									Return[factorBueno]];


(* ::Text:: *)
(*Comprueba si la ra\[IAcute]z del producto de los discriminantes de grados 2 y 3 est\[AAcute] en Q.*)


ComprobarProductoDiscriminantes[f_,x_]:=Module[{f1,f2},f1=DevuelveFactorGrado2[f,x];
											f2=DevuelveFactorGrado3[f,x];
											Return[!IrreduciblePolynomialQ[x^2-Discriminant[f1,x]*Discriminant[f2,x]]]];


(* ::Text:: *)
(*Comprueba si la ra\[IAcute]a del discriminante del polinomio est\[AAcute] en Q.*)


ComprobarDiscriminante[f_,x_]:=Return[!IrreduciblePolynomialQ[x^2-Discriminant[f,x]]];


(* ::Text:: *)
(*Comprueba si la ra\[IAcute]z del discriminante del factor de grado 3 est\[AAcute] en Q.*)


ComprobarDiscriminantef1[f_,x_]:=Return[!IrreduciblePolynomialQ[x^2-Discriminant[DevuelveFactorGrado3[f,x],x]]];


(* ::Text:: *)
(*Comprueba si tiene una ra\[IAcute]z el polinomio en la extensi\[OAcute]n por la primera ra\[IAcute]z del mismo.*)


ComprobarFactor1alpha1[f_,x_]:=Module[{factores,soluciones,alpha1,condicion},soluciones=Solve[f==0,x];
									alpha1=soluciones[[1]][[1]][[2]];
									condicion=False;
									factores=FactorList[f,Extension->alpha1];
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==4,condicion=True,],{i,1,Length[factores]}];
									Return[condicion]];


(* ::Text:: *)
(*Comprueba si el discriminante del polinomio es una ra\[IAcute]z en la extensi\[OAcute]n dada por dos ra\[IAcute]ces del mismo.*)


ComprobarDiscriminanteAlpha1Alpha2[f_,x_]:=Return[!IrreduciblePolynomialQ[x^2-Discriminant[f,x],Extension->{Solve[f==0,x][[1]][[1]][[2]],Solve[f==0,x][[2]][[1]][[2]]}]];


(* ::Text:: *)
(*Comprueba si descompone como un polinomio de grado 2 por uno de grado 3 en la extensi\[OAcute]n por una de las ra\[IAcute]ces.*)


ComprobarDescomposicion2y3Alpha1[f_,x_]:=Module[{factores,condicion1,condicion2},factores=FactorList[f,Extension->Solve[f==0,x][[1]][[1]][[2]]];
									condicion1=False;
									condicion2=False;
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,condicion1=True,];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,condicion2=True,],{i,1,Length[factores]}];
									Return[condicion1\[And]condicion2]];


(* ::Text:: *)
(*Comprueba si descompone como un polinomio de grado 1 por dos de grado 2 en la extensi\[OAcute]n por una ra\[IAcute]z.*)


ComprobarDescomposicion1y2y2Alpha1[f_,x_]:=Module[{factores,condicion1,condicion2,condicion3,factor2},factores=FactorList[f,Extension->Solve[f==0,x][[1]][[1]][[2]]];
									condicion1=False;
									condicion2=False;
									condicion3=False;
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,condicion1=True;factor2=Expand[factores[[i]][[1]]^factores[[i]][[2]]],];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2\[And]factor2!=Expand[factores[[i]][[1]]^factores[[i]][[2]]],condicion3=True,];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,condicion2=True,],{i,1,Length[factores]}];
									Return[condicion1\[And]condicion2\[And]condicion3]];


(* ::Text:: *)
(*Comprueba si tiene 5 ra\[IAcute]ces en la extensi\[OAcute]n por una de ellas.*)


Comprobar5RaicesAlpha1[f_,x_]:=Module[{factores,condicion},factores=FactorList[f,Extension->Solve[f==0,x][[1]][[1]][[2]]];
									condicion=True;
									Do[If[Exponent[factores[[i]][[1]],x]!=1,condicion=False,],{i,1,Length[factores]}];
									Return[condicion];
									];


CondicionFinal[f_,x_]:=Module[{alpha1,alpha2,factors,grades,aux,grade},alpha1=Root[f,1];
								alpha2 = Root[f,2];
								factors = FactorList[f,Extension->alpha1];
								grades=Array[#-#&,4];
								Do[
								aux=factors[[i]][[1]];
								grade=Exponent[aux,x];
								grades[[grade]]=grades[[grade]]+1,
								{i,1, Length[factors]}
								];
								Return[grades]];


GrupoGalois5[f_,x_]:=Module[{},If[ComprobarUnaRaizGrado5[f,x],
									GrupoGalois4[DevuelveFactorGrado4[f,x],x],
									If[ComprobarDescomposicion2y3[f,x],
										If[ComprobarProductoDiscriminantes[f,x],
											If[ComprobarDiscriminante[f,x],Print["El grupo de Galois de f es S3"],Print["No sabemos nada"]],
											If[ComprobarDiscriminantef1[f,x],Print["El grupo de Galois de f es A3xS2"],Print["El grupo de Galois de f es S3xS2"]]],
										If[ComprobarFactor1alpha1[f,x],
											If[ComprobarDiscriminante[f,x],
												Print["El grupo de Galois de f es A5"],
												If[ComprobarDiscriminanteAlpha1Alpha2[f,x],
													Print["El grupo de Galois de f es F20"],
													Print["El grupo de Galois de f es S5"]]],
											If[ComprobarDescomposicion1y2y2Alpha1[f,x]\[Or]ComprobarDescomposicion2y3Alpha1[f,x]\[Or]CondicionFinal[f,x][[2]]==2\[Or]CondicionFinal[f,x][[2]]==1,
												Print["El grupo de Galois de f es D5"],
												If[!Comprobar5RaicesAlpha1[f,x],
													Print["El grupo de Galois de f es C5"],
													Print["No sabemos nada"]]]]]];
							];


GrupoGalois5s[f_,x_]:=Module[{},If[ComprobarUnaRaizGrado5[f,x],
									GrupoGalois4[DevuelveFactorGrado4[f,x],x],
									If[ComprobarDescomposicion2y3[f,x],
										If[ComprobarProductoDiscriminantes[f,x],
											If[ComprobarDiscriminante[f,x],Print["El grupo de Galois de f es S3"],Print["No sabemos nada"]],
											If[ComprobarDiscriminantef1[f,x],Print["El grupo de Galois de f es A3xS2"],Print["El grupo de Galois de f es S3xS2"]]],
										If[IrreduciblePolynomialQ[f],
											If[CondicionFinal[f,x][[4]]==1,
												If[Element[Sqrt[Discriminant[f,x]],Rationals],
													Print["El grupo de Galois es A5"],
													If[IrreduciblePolynomialQ[x^2-Discriminant[f,x],Extension->{Root[f,1],Root[f,2]}],
														Print["El grupo de Galois es S5"],
														Print["El grupo de Galois es F20"]]],
												If[CondicionFinal[f,x][[2]]==2\[Or]CondicionFinal[f,x][[2]]==1,
													Print["El grupo de Galois es D5"],
													Print["El grupo de Galois es C5"]]]]]]
							];


(* ::Input:: *)
(*GrupoGalois5[x^5+x^2+1,x]*)
(*GrupoGalois5[x^5+20x+16,x]*)
(*GrupoGalois5[x^5+x^4-4x^3-3x^2+3x+1,x]*)
(*GrupoGalois5[x^5-5x+12,x]*)
(*GrupoGalois5[x^5-2,x]*)


Galois[f_,x_]:=If[Exponent[f,x]==2,
					GrupoGalois2[f],
					If[Exponent[f,x]==3,
						GrupoGalois3[f,x],
						If[Exponent[f,x]==4,
							GrupoGalois4[f,x],
							If[Exponent[f,x]==5,
								GrupoGalois5[f,x],
								Print["El grado del polinomio es mayor que 5 o menor que 2."]]]]];


(* ::Input:: *)
(*Galois[x^5+x^2+1,x]*)
(*Galois[x^5+20x+16,x]*)
(*Galois[x^5+x^4-4x^3-3x^2+3x+1,x]*)
(*Galois[x^5-5x+12,x]*)
(*Galois[x^5-2,x]*)


(* ::Input:: *)
(*Galois[x^4-x-1,x]*)
(*Galois[x^4+8x+12,x]*)
(*Galois[x^4+3x+3,x]*)
(*Galois[x^4-2,x]*)
(*Galois[x^4-2x^2-2,x]*)
(*Galois[x^4+36x+63,x]*)
(*Galois[x^4-3x^2+4,x]*)


(* ::Input:: *)
(*Galois[x^3+2x^2+x+1,x]*)
(*Galois[2x^3-5x^2-2x-12,x]*)
(*Galois[x^3-1,x]*)
(*Galois[(2x^2+3x+1)*(2x+1),x]*)
(*Galois[x^3-2x^2-x+1,x]*)


(* ::Input:: *)
(*Galois[x^2+x+1,x]*)
(*Galois[x^2-1,x]*)

(* ::Package:: *)

(* ::Title:: *)
(*C\[AAcute]lculo del Grupo de Galois*)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 2*)


GrupoGalois2General[f_, k_] := If[IrreduciblePolynomialQ[f, k],
   						Print["El grupo de Galois es S2"],
   						Print["El grupo de Galois es A2"]];


GrupoGalois2[f_]:=If[IrreduciblePolynomialQ[f],
					Print["El grupo de Galois es S2"],
					Print["El grupo de Galois es A2"]];


(* ::Input:: *)
(*GrupoGalois2[x^2+x+1]*)
(*GrupoGalois2[x^2-1]*)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 3*)


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
							Print["El grupo de Galois es S3"],
							Print["El grupo de Galois es A3"]],
						Module[{factores},factores=FactorList[f,k];
							GrupoGalois2General[ObtenerFactorBueno2General[f,k],k]]];


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


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 4*)


ResolventeCubica[f_,x_]:=Module[{coefs,b0,b1,b2},coefs=CoefficientList[f,x];
							b2=-coefs[[3]];
							b1=coefs[[2]]*coefs[[4]]-4*coefs[[1]];
							b0=-coefs[[4]]^2*coefs[[1]]+4*coefs[[3]]*coefs[[1]]-coefs[[2]]^2;
							Return[x^3+b2*x^2+b1*x+b0]];


CompruebaDescomposicionGrado3[f_,x_]:=Module[{factores,descomponeBien},factores=FactorList[f];
										descomponeBien=False;
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,descomponeBien=True],{i,1,Length[factores]}];
										Return[descomponeBien]];


DevuelvePolinomioGrado3[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
										Return[factorBueno]];


CompruebaDescomposicionGrado2[f_,x_]:=Module[{factores,descomponeBien},factores=FactorList[f];
										descomponeBien=False;
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,descomponeBien=True],{i,1,Length[factores]}];
										Return[descomponeBien]];


DevuelvePolinomioGrado2[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
										Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]]],{i,1,Length[factores]}];
										Return[factorBueno]];


CompruebaDiscriminantes[f_,x_]:=Module[{g1,g2},g1=DevuelvePolinomioGrado2[f,x];
										g2=Divide[f,g1];
										If[IrreduciblePolynomialQ[x^2-(Discriminant[g1,x]*Discriminant[g2,x])],Return[False],Return[True]]];


CompruebaResolventeIrreducible[f_,x_]:=Module[{resolvente},resolvente=ResolventeCubica[f,x];
											If[IrreduciblePolynomialQ[resolvente],Return[True],Return[False]]];


CompruebaDiscriminanteResolvente[f_,x_]:=Module[{resolvente},resolvente=ResolventeCubica[f,x];
												If[IrreduciblePolynomialQ[(x^2-Discriminant[ResolventeCubica[f,x],x])],Return[False],Return[True]]];


CompruebaTresRaicesResolvente[f_,x_]:=Module[{resolvente,factores},resolvente=ResolventeCubica[f,x];
											factores=FactorList[resolvente];
											If[Length[factores]==4,Return[True],Return[False]]];


ObtenerBetasSoluciones[f_,x_]:=Module[{soluciones,solucionesFormateadas,beta1,beta2,beta3},soluciones=Solve[f==0,x];
								solucionesFormateadas=List[];
								Do[AppendTo[solucionesFormateadas,soluciones[[i]][[1]][[2]]],{i,1,Length[soluciones]}];
								beta1=solucionesFormateadas[[1]]*solucionesFormateadas[[2]] + solucionesFormateadas[[3]]*solucionesFormateadas[[4]];
								beta2=solucionesFormateadas[[1]]*solucionesFormateadas[[3]] + solucionesFormateadas[[2]]*solucionesFormateadas[[4]];
								beta3=solucionesFormateadas[[1]]*solucionesFormateadas[[4]] + solucionesFormateadas[[2]]*solucionesFormateadas[[3]];
								Return[List[List[beta1,beta2,beta3],solucionesFormateadas]]];


ComprobarBetasAlphas[f_,x_]:=Module[{betas,alphas,discriminante,condicion},betas=ObtenerBetasSoluciones[f,x][[1]];
									alphas=ObtenerBetasSoluciones[f,x][[2]];
									discriminante=Discriminant[f,x];
									condicion=True;
									If[IrreduciblePolynomialQ[x-(alphas[[1]]+alphas[[3]]),Extension->Sqrt[discriminante]],,condicion=False];
									If[IrreduciblePolynomialQ[x-(alphas[[1]]*alphas[[3]]),Extension->Sqrt[discriminante]],,condicion=False];
									If[IrreduciblePolynomialQ[x-(alphas[[2]]+alphas[[4]]),Extension->Sqrt[discriminante]],,condicion=False];
									If[IrreduciblePolynomialQ[x-(alphas[[2]]*alphas[[4]]),Extension->Sqrt[discriminante]],,condicion=False];
									Return[condicion]];


GrupoGalois4[f_,x_]:=Module[{},If[CompruebaDescomposicionGrado3[f,x],
									GrupoGalois3[DevuelvePolinomioGrado3[f,x]],
									If[CompruebaDescomposicionGrado2[f,x],
										If[CompruebaDiscriminantes[f,x],
											Print["El grupo de Galois es <(12)(34)>"],
											Print["El grupo de Galois es <(12),(34)>"]],
										If[CompruebaResolventeIrreducible[f,x],
											If[CompruebaDiscriminanteResolvente[f,x],
												Print["El grupo de Galois de f es S4 y el de la resolvente S3"],
												Print["El grupo de Galois de f es A4 y el de la resolvente S4"]],
											If[CompruebaTresRaicesResolvente[f,x],
												Print["El grupo de Galois de f es V = <(12)(34),(13)(24)> y el de la resolvente es el trivial."],
												If[ComprobarBetasAlphas[f,x],
													Print["El grupo de Galois de f es C4 y el de la resolvente es C2"],
													Print["El grupo de Galois de f es D4 y el de la resolvente es C2"]]]]]]];


(* ::Input:: *)
(*GrupoGalois4[Expand[(x-1)^2*(x-Pi)^2,x],x]*)
(*GrupoGalois4[x^4+x^3+x^2+x+1,x]*)


(* ::Section:: *)
(*Grupo de Galois para polinomios de grado 5*)


ComprobarUnaRaizGrado5[f_,x_]:=Module[{factores,condicion},factores=FactorList[f];
								condicion=False;
								Do[If[Exponent[factores[[i]][[1]],x]==1,condicion=True,],{i,1,Length[factores]}];
								Return[condicion]];


DevuelveFactorGrado4[f_,x_]:=Module[{factores,factorGrado1,factorGrado4},factores=FactorList[f];
									Do[If[Exponent[factores[[i]][[1]],x]==1,factorGrado1=factores[[i]][[1]],],{i,1,Length[factores]}];
									factorGrado4=Divide[f,factorGrado1];
									Return[factorGrado4]];


ComprobarDescomposicion2y3[f_,x_]:=Module[{factores,condicion1,condicion2},factores=FactorList[f];
									condicion1=False;
									condicion2=False;
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,condicion1=True,];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,condicion2=True,],{i,1,Length[factores]}];
									Return[condicion1\[And]condicion2]];


DevuelveFactorGrado2[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]],],{i,1,Length[factores]}];
									Return[factorBueno]];


DevuelveFactorGrado3[f_,x_]:=Module[{factores,factorBueno},factores=FactorList[f];
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,factorBueno=Expand[factores[[i]][[1]]^factores[[i]][[2]]],],{i,1,Length[factores]}];
									Return[factorBueno]];


ComprobarProductoDiscriminantes[f_,x_]:=Module[{f1,f2},f1=DevuelveFactorGrado2[f,x];
											f2=DevuelveFactorGrado3[f,x];
											Return[IrreduciblePolynomialQ[x^2-Discriminant[f1,x]*Discriminant[f2,x]]]];


ComprobarDiscriminante[f_,x_]:=Return[IrreduciblePolynomialQ[x^2-Discriminant[f,x]]];


ComprobarDiscriminantef1[f_,x_]:=Return[IrreduciblePolynomialQ[x^2-Discriminant[DevuelveFactorGrado3[f,x],x]]];


ComprobarFactor1alpha1[f_,x_]:=Module[{factores,soluciones,alpha1,condicion},soluciones=Solve[f==0,x];
									alpha1=soluciones[[1]][[1]][[2]];
									condicion=False;
									factores=FactorList[f,Extension->alpha1];
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==4,condicion=True,],{i,1,Length[factores]}];
									Return[condicion]];


ComprobarDiscriminanteAlpha1Alpha2[f_,x_]:=Return[IrreduciblePolynomialQ[x^2-Discriminant[f,x],Extension->{Solve[f==0,x][[1]][[1]][[2]],Solve[f==0,x][[2]][[1]][[2]]}]];


ComprobarDescomposicion2y3Alpha1[f_,x_]:=Module[{factores,condicion1,condicion2},factores=FactorList[f,Extension->Solve[f==0,x][[1]][[1]][[2]]];
									condicion1=False;
									condicion2=False;
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,condicion1=True,];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,condicion2=True,],{i,1,Length[factores]}];
									Return[condicion1\[And]condicion2]];


ComprobarDescomposicion1y2y2Alpha1[f_,x_]:=Module[{factores,condicion1,condicion2,condicion3,factor2},factores=FactorList[f,Extension->Solve[f==0,x][[1]][[1]][[2]]];
									condicion1=False;
									condicion2=False;
									condicion3=False;
									Do[If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2,condicion1=True;factor2=Expand[factores[[i]][[1]]^factores[[i]][[2]]],];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==2\[And]factor2!=Expand[factores[[i]][[1]]^factores[[i]][[2]]],condicion3=True,];
										If[Exponent[Expand[factores[[i]][[1]]^factores[[i]][[2]]],x]==3,condicion2=True,],{i,1,Length[factores]}];
									Return[condicion1\[And]condicion2\[And]condicion3]];


Comprobar5RaicesAlpha1[f_,x_]:=Module[{factores,condicion},factores=FactorList[f,Extension->Solve[f==0,x][[1]][[1]][[2]]];
									condicion=True;
									Do[If[Exponent[factores[[i]][[1]],x]!=1,condicion=False,],{i,1,Length[factores]}];
									Return[condicion];
									];


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
											If[ComprobarDescomposicion1y2y2Alpha1[f,x]\[And]ComprobarDescomposicion2y3Alpha1[f,x],
												Print["El grupo de Galois de f es D5"],
												If[Comprobar5RaicesAlpha1[f,x],
													Print["El grupo de Galois de f es C5"],
													Print["No sabemos nada"]]]]]];
							];


(* ::Input:: *)
(*GrupoGalois5[x^5+x^4+x^3+x^2+x+1,x]*)
(*GrupoGalois5[Expand[(x-Pi)*(x-E)*(x^3+x^2+x+1)],x]*)




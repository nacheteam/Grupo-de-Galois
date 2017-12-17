BeginPackage["GrupoGalois`"]

GrupoGalois2::usage = "Calcula el grupo de Galois de un polinomio de grado 2."

Begin["Privado`"]

GrupoGalois2[f_, k_] := If[IrreduciblePolynomialQ[f, k],
   						Return["El grupo de Galois es S2"],
   						Return["El grupo de Galois es A2"]];

End[]

EndPackage[]

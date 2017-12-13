#!/usr/bin/env sage

import sys
from sage.all import *

def discriminanteGrado2(f):
    coef = f.coefficients(x,sparse=0)
    return coef[1]^2-4*coef[0]

def grupoGaloisOrden2(f,K):
    F.<x>=K[]
    if f.is_irreducible():
        print("El grupo de Galois es S2\n")
    else:
        print("El grupo de Galois es A2\n")

def discriminanteGrado3(f):
    coef = f.coefficients(x)
    return (-4*(coef[2]^3)*coef[0] + (coef[1]^2)*(coef[2]^2) + 18*coef[0]*coef[1]*coef[2] - 4*coef[1]^3 - 27*coef[0]^2)

def grupoGaloisOrden3(f,K):
    F.<x>=K[]
    if f.is_irreducible():
        g = x^2 -discriminanteGrado3(f)
        if g.is_irreducible():
            print("El grupo de Galois es S3")
        else:
            print("El grupo de Galois es A3")
    else:
        F = list(f.factor())
        if len(F)==1:
            print("El grupo de Galois es S2\n")
        else:
            grupoGaloisOrden2(F[1][0],K)

def mainOrden2():
    K = GF(2^3)
    f = x^2+1
    grupoGaloisOrden2(f,K)
    f = x^2+x+1
    grupoGaloisOrden2(f,K)

def mainOrden3():
    K = GF(2^3)
    F.<x>=K[]
    f = x^3+x^2+x+1
    grupoGaloisOrden3(f,K)

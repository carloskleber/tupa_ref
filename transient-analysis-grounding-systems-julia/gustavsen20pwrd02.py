# -*- coding: utf-8 -*-
"""
Created on Sat May  4 09:22:18 2024

@author: pedro + acsl
"""

# -*- coding: utf-8 -*-
"""
Tradução do codigo Mathematica criado por acsl

Conjunto de funções para avaliar as matrizes Z e Y para linhas
de transmissão e cabos subterrâneos.

# TODO documentar funções; usar nomes significativos
"""
import numpy
from scipy.special import kv, iv
from scipy.fftpack import ifft
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
from mpl_toolkits.mplot3d import Axes3D

# %% Constantes e funções
Pi = numpy.pi
I = 1j
mu0 = 4e-7*Pi
eps0 = 8.854e-12
BesselK = kv
BesselI = iv


### impedância interna de condutores tubulares
def ZintTubo(Omega, Rhoc, rf, rint, Mur=1, Mu=mu0):
    Etac = numpy.sqrt( (I*Omega*Mur*Mu)/Rhoc )
    ri = rint + 1e-6
    cf = Etac*rf
    ci = Etac*ri
    Den = BesselK(1, ci)*BesselI(1, cf) - BesselK(1, cf)*BesselI(1, ci)
    Num = BesselK(1, ci)*BesselI(0, cf) + BesselK(0, cf)*BesselI(1, ci)
    return Rhoc*Etac*Num/(2*Pi*rf*Den)

# impedância interna de condutor sem alma de aço
# impedância interna de condutores cilíndricos
# Mur=90 para cabos de aço
def Zin(Omega, Rhopr, rpr, Mur=1, Mu=mu0):
    Etapr = numpy.sqrt( I*Omega*Mu*Mur/Rhopr )
    cr = Etapr*rpr
    civ0 = BesselI(0, cr)
    civ1 = BesselI(1, cr)
    return Etapr*Rhopr*civ0/(2*Pi*rpr*civ1)

def Z2(omega, rcond, rins1, Mur=1, Mu=mu0):
    return numpy.complex128((I*omega*Mur*Mu/(2*Pi))*numpy.log(rins1/rcond))

def Z3(Omega, rins1, rsheath, Rhosheath, Mur=1, Mu=mu0):
    Etasheath = numpy.sqrt( I*Omega*Mur*Mu/Rhosheath )
    csheath = Etasheath*rsheath
    cins = Etasheath*rins1
    Den = BesselI(1, csheath)*BesselK(1, cins) - BesselI(1, cins)*BesselK(1, csheath)
    c4 = (Rhosheath*Etasheath)/(2*Pi*rins1*Den)
    return c4*(BesselI(0, cins)*BesselK(1, csheath) + BesselK(0, cins)*BesselI(1, csheath))

def Z4(Omega, rins1, rsheath, Rhosheath, Mur=1, Mu=mu0):
    Etasheath = numpy.sqrt( (I*Omega*Mur*Mu)/Rhosheath )
    c1 = Etasheath*rins1
    c2 = Etasheath*rsheath
    Den = BesselI(1, c2)*BesselK(1, c1) - BesselI(1, c1)*BesselK(1, c2)
    return (Rhosheath/(2*Pi*rins1*rsheath*Den))

def Z6(omega, rsheath, rins2, Mur=1, Mu=mu0):
    return numpy.complex128(I*omega*Mur*Mu/(2*Pi)*numpy.log(rins2/rsheath))

def ZSolo(omega, r, h1, h2, sigma_solo, mu=mu0):
    eta_solo = numpy.sqrt(I*omega*mu*sigma_solo)
    c1 = I*omega*mu/2/Pi
    c2 = (h1 + h2)**2 - r**2
    c3 = r**2 + (h1 + h2)**2
    c4 = eta_solo**2*(r**2 + (h1 + h2)**2)
    c5 = 1 + (h1 + h2)*eta_solo
    c6 = numpy.exp(-eta_solo*(h1 + h2))
    c7 = BesselK(0, eta_solo*numpy.sqrt(r**2 + (h1 - h2)**2) )
    c8 = BesselK(2, eta_solo*numpy.sqrt(r**2 + (h1 + h2)**2) )
    return c1*(c7 + c2/c3*(c8 - 2*(c6*c5)/c4))
    
def Yci(Omega, rcond, rins, epsrins, eps0=eps0):
    return numpy.complex128(I*2*Pi*Omega*(epsrins*eps0)/numpy.log(rins/rcond))

### rotinas para copiar o linspace e o logspace do matlab
#logspac
#lispac
"""
Cabos para-raios não são explicitamente considerados, então nós incluímos o
efeito deles via redução de Kron.

Matrizes devem estar no formato de impedância.
"""
def eliprc(m, nc, np):
    i = nc-np
    return numpy.linalg.inv(m)[:i, :i]
    
"""
Reduz o feixe para condutores de fase equivalentes.

Matrizes devem estar no formato de admitância.
"""
def elibndc(aux, nb, nf):
    table = numpy.array([])
    for m in range(nf):
        for n in range(nf):
            v = 0
            for i in range(nb*m - (nb - 2), nb*m + 2):
                for j in range(nb*n - (nb - 2), nb*n + 2):
                    v += aux[i, j]
            table = numpy.append(table, v)
    
    return table

### internal impedance of cylindrical conductors
def zintc(omega, Rhoc, rf, ri, Mur):
    return ZintTubo(omega, Rhoc, rf, ri, Mur)

def zic(omega, rhoc, rf, mur):
    return Zin(omega, rhoc, rf, mur)

### External impedance of overhead lines
# With ground wires
def zextc(omega, sigma, npr, x, y, rf, rpr):
    Mu = mu0
    nc = x.size
    p = numpy.sqrt(1.0/(I*omega*Mu*sigma))
    table = numpy.array([])
    for i in range(nc):
        for j in range(nc):
            if i != j:
                Deltaxij = x[i] - x[j]
                yij = y[i] + y[j]
                Deltayij = y[i] - y[j]
                novovalor = 0.5*numpy.log( (Deltaxij**2 + (2*p + yij)**2)/(Deltaxij**2 + (Deltayij)**2) )
            elif i <= nc - npr - 1:
                novovalor = numpy.log( 2.0*(y[i] + p)/rf )
            else:
                novovalor = numpy.log( 2.0*(y[i] + p)/rpr )
                
            table = numpy.append(table, novovalor)
    table = table.reshape(nc,nc)
    return (I*omega*Mu/2/Pi*table).reshape(nc, nc)

"""
evaluate impedance and admittannce matrices per unit of length 
using complex ground plane as overloading in Mma does not work
with compiled functions using uncompiled version
"""
# case 1: with ground wires and bundled conductors
# TODO testar
def cZYlt1(omega, x, y, sigmas, rdc, rf, rint, npr, rdcpr, rpr, nb):
    mu = mu0
    eps = eps0
    nc = x.size
    nf = int( (nc - npr)/nb )
    rhoc = rdc*Pi*(rf**2 - rint**2)
    rhopr = rdcpr*Pi*rpr**2
    v1 = zintc(omega, rhoc, rf, rint, 1)*numpy.ones(nc - npr)
    v2 = zic(omega, rhopr, rpr, 90)*numpy.ones(npr)
    v = numpy.append(v1, v2)
    zin = numpy.diag(v)
    p = numpy.sqrt( 1/(I*omega*mu*sigmas) )
    table = numpy.array([])
    for i in range(nc):
        for j in range(nc):
            if i != j:
                Deltaxij = x[i] - x[j]
                yij = y[i] + y[j]
                Deltayij = y[i] - y[j]
                k1 = (Deltaxij**2 + (2*p + yij)**2)
                k2 = (Deltaxij**2 + (Deltayij)**2)
                novovalor = 0.5*numpy.log( k1/k2 )
            elif i <= nc - npr - 1:
                novovalor = numpy.log( 2.0*(y[i] + p)/rf )
            else:
                novovalor = numpy.log( 2.0*(y[i] + p)/rpr )
                
            table = numpy.append(table, novovalor)
            
    ze = I*omega*mu/2/Pi*table
    ze = ze.reshape(zin.shape)
    co1 = elibndc(eliprc(zin+ze, nc, npr), nb, nf) # FIXME erro
    Z1 = numpy.linalg.inv(co1)
    mp = table
    # inclusao de condutancia shunt para evitar alguns problemas
    # numericos
    co2 = elibndc(eliprc(mp, nc, npr), nb, nf)
    Y1 = 3e-12*numpy.diag( numpy.ones(nf) ) + I*omega*2*Pi*eps*co2
    return Z1, Y1

# case  2: ground wires and unbundled conductors
def cZYlt2(omega, x, y, sigmas, rdc, rf, rint, npr, rdcpr, rpr):
    mu = mu0
    eps = eps0
    nc = x.size
    nf = int(nc - npr)
    rhoc = rdc*Pi*(rf**2 - rint**2)
    rhopr = rdcpr*Pi*rpr**2
    if rint != 0:
        v1 = zintc(omega, rhoc, rf, rint, 1)*numpy.ones(nc - npr)
        v2 = zic(omega, rhopr, rpr, 1)*numpy.ones(npr)
        v = numpy.append(v1, v2)
        zin = numpy.diag(v)
    else:
        v1 = zic(omega, rhoc, rf, 1)*numpy.ones(nc - npr)
        v2 = zic(omega, rhopr, rpr, 1)*numpy.ones(npr)
        v = numpy.append(v1, v2)
        zin = numpy.diag(v)
        
    p = numpy.sqrt( 1/(I*omega*mu*sigmas) )
    table = numpy.array([])
    for i in range(nc):
        for j in range(nc):
            if i != j:
                novovalor = (numpy.log(
                        ((x[i] - x[j])**2 + (2*p + y[i] + y[j])**2)/(
                                (x[i] - x[j])**2 +(y[i] - y[j])**2)))/(2)
            elif i <= nc - npr - 1:
                novovalor = numpy.log( 2.0*(y[i] + p)/rf )
            else:
                novovalor = numpy.log( 2.0*(y[i] + p)/rpr )
                
            table = numpy.append(table, novovalor)
        
    ze = I*omega*mu/2/Pi*table
    ze = ze.reshape(zin.shape)
    Z1 = numpy.linalg.inv(eliprc(zin+ze, nc, npr))
    mp = numpy.array([])
    for i in range(nc):
        for j in range(nc):
            if i != j:
                novovalor = (1/2)*numpy.log(
                        ((x[i] - x[j])**2 + (y[i] + y[j])**2)/(
                                (x[i] - x[j])**2 + (y[i] - y[j])**2))
            elif i <= nc - npr - 1:
                novovalor = numpy.log( (2*y[i])/rf )
            else:
                novovalor = numpy.log( (2*y[i])/rpr )
                
            mp = numpy.append(mp, novovalor)
        
    mp = mp.reshape(zin.shape)
    co2 = eliprc(mp, nc, npr)
    Y1 = 3e-12*numpy.diag( numpy.ones(nf) ) + I*omega*2*Pi*eps*co2
    return Z1, Y1

# case 3: no ground wires or bundled conductors
# TODO testar
def cZYlt3(omega, x, y, sigmas, rdc, rf, rint):
    mu = mu0
    eps = eps0
    nc = x.size
    nf = nc
    rhoc = rdc*Pi*(rf**2 - rint**2)
    
    if rint != 0:
        zin = numpy.diag( zintc(omega, rhoc, rf, rint, 1)
                         *numpy.ones(nc) )
    else:
        zin = numpy.diag( zic(omega, rhoc, rf, 1)
                     *numpy.ones(nc) )
        
    p = numpy.sqrt( 1/(I*omega*mu*sigmas) )
    table = numpy.array([])
    for i in range(nc):
        for j in range(nc):
            if i != j:
                Deltaxij = x[i] - x[j]
                yij = y[i] + y[j]
                Deltayij = y[i] - y[j]
                k1 = (Deltaxij**2 + (2*p + yij)**2)
                k2 = (Deltaxij**2 + (Deltayij)**2)
                novovalor = 0.5*numpy.log( k1/k2 )
            else:
                novovalor = numpy.log( 2.0*(y[i] + p)/rf )
                
            table = numpy.append(table, novovalor)
            
    ze = I*omega*mu/2/Pi*table
    ze = ze.reshape(zin.shape)
    Z1 = zin + ze
    mp = table
    Y1 = 3e-12*numpy.diag( numpy.ones(nf) ) + (I*omega*2*Pi*eps
                         *numpy.linalg.inv(mp))
    return Z1, Y1

def cZYlt(omega, x, y, sigmas, rdc, rf, rint, npr=None,
          rdcpr=None, rpr=None, nb=None):
    """Wrapper para o overloading de cZYlt."""
    if npr is None:
        return cZYlt3(omega, x, y, sigmas, rdc, rf, rint)
    elif nb is None:
        return cZYlt2(omega, x, y, sigmas, rdc, rf, rint, npr,
                      rdcpr, rpr)
    else:
        return cZYlt1(omega, x, y, sigmas, rdc, rf, rint, npr,
                      rdcpr, rpr, nb)

# caso: configuração flat condutor + isolante
#teste=

### Cabo blindado
def cZYsc_c1(omega, h, r, rhoc, eps1, rhob, eps2):
    z1 = Zin(omega, rhoc, r[0], 1)
    z2 = Z2(omega, r[0], r[1], 1, mu0)
    z3 = Z3(omega, r[1], r[2], rhob, 1, mu0)
    z4 = Z4(omega, r[1], r[2], rhob, 1, mu0)
    z5 = ZintTubo(omega, rhob, r[2], r[1], 1, mu0)
    z6 = Z6(omega, r[2], r[3], 1, mu0)
    l1 = [z1 + z2 + z3 - 2*z4 + z5 + z6, z5 + z6 - z4]
    l2 = [z5 + z6 - z4, z5 + z6]
    Zp = numpy.array( [ l1, l2] )
    y1 = Yci(omega, r[0], r[1], eps1, eps0)
    y2 = Yci(omega, r[2], r[3], eps2, eps0)
    Yp = numpy.array( [ [y1, -y1], [-y1, y1+y2] ] )
    return Zp, Yp

### COM ARMADURA
# TODO testar
def cZYsc_c2(omega, h, r, rhoc, eps1, rhob, eps2, rhoa, eps3):
    z1 = Zin(omega, rhoc, r[0], 1)
    z2 = Z2(omega, r[0], r[1], 1, mu0)
    z3 = Z3(omega, r[1], r[2], rhob, 1, mu0)
    z4 = Z4(omega, r[1], r[2], rhob, 1, mu0)
    z5 = ZintTubo(omega, rhob, r[2], r[1], 1, mu0)
    z6 = Z6(omega, r[2], r[3], 1, mu0)
    z7 = Z3(omega, r[3], r[4], rhoa, 90, mu0)
    z8 = Z4(omega, r[3], r[4], rhoa, 90, mu0)
    z9 = ZintTubo(omega, rhoa, r[4], r[3], 90, mu0)
    z10 = Z6(omega, r[4], r[5], 1, mu0)
    
    zc = z1 + z2 + z3 + z5 - 2*z4 + z6 + z7 + z9 - 2*z8 + z10
    zb = z5 + z6 + z7 + z9 - 2*z8 + z10
    za = z9 + z10
    zcb = z5 - z4 + z6 + z7 + z9 - 2*z8 + z10
    zca = z9 - z8 + z10
    zba = zca
    
    Zp = numpy.array([[zc, zcb, zca], [zcb, zb, zba], [zca, zba, za]])
    y1 = Yci(omega, r[0], r[1], eps1, eps0)
    y2 = Yci(omega, r[2], r[3], eps2, eps0)
    y3 = Yci(omega, r[4], r[5], eps3, eps0)
    Yp = numpy.array([[y1, -y1, 0],[-y1, y1+y2, -y2],[0, -y2, y2+y3]])
    
    return Zp, Yp

# TODO testar
def cZYsc(omega, h, r, rhoc, eps1, rhob, eps2, rhoa=None,
             eps3=None):
    """Wrapper para o overloading de cZYsc."""
    if rhoa is None:
        return cZYsc_c1(omega, h, r, rhoc, eps1, rhob, eps2)
    else:
        return cZYsc_c2(omega, h, r, rhoc, eps1, rhob, eps2,
                        rhoa, eps3)

# TODO testar
def cZYsc2(omega, h, nCond, r, sigmas, rhoc, eps1, rhob, eps2,
           rhoa, eps3):
    murc = 1 # core
    murb = 1 # sheath
    mura = 90 # armour
    
    # check for tubular core
    if r[0] < 1e-12:
        # solid core
        z1 = Zin(omega, rhoc, r[1], murc) # core self
    else:
        z1 = ZintTubo(omega, rhoc, r[0], r[1], murc) # core self
        
    if nCond >= 2:
        z2 = Z2(omega, r[1], r[2])
        z3 = Z3(omega, r[2], r[3], rhob)
        z4 = Z4(omega, r[2], r[3], rhob)
        z5 = ZintTubo(omega, rhob, r[3], r[2], murb) # sheath self
        z6 = Z6(omega, r[3], r[4])
        withSheath = 1
    else:
        withSheath = 0
        
    if nCond >= 3: # armour
        z7 = Z3(omega, r[4], r[5], rhoa, mura)
        z8 = Z4(omega, r[4], r[5], rhoa, mura)
        z9 = ZintTubo(omega, rhoa, r[5], r[4], mura) # armour self
        # armor insulation layer due to field var
        z10 = Z6(omega, r[5], r[6])
        withArmour = 1
    else:
        withArmour = 0
        
    # flags
    zc = z1 + z2 + withSheath*(z3 + z5 - 2*z4 + z6)
    zc += withArmour*(z7 + z9 - 2*z8 + z10)
    zb = withSheath*(z5 + z6) + withArmour*(z7 + z9 - 2*z8 + z10)
    za = withArmour*(z9 + z10)
    zcb = withSheath*(z5 - z4 + z6) + withArmour*(z7 + z9 - 2*z8 + z10)
    zca = withArmour*(z9 - z8 + z10)
    zba = zca
    
    y1 = Yci(omega, r[1], r[2], eps1, eps0)
    
    # cases:
        # core only: 1
        # core + sheath: 2
        # core + sheath + armour: 3
        
    if nCond == 1: # core only
        Zp = zc
        Yp = y1
    elif nCond == 2: # core + sheath
        Zp = numpy.array([[zc, zcb], [zcb, zb]])
        y2 = Yci(omega, r[3], r[4], eps2, eps0)
        Yp = numpy.array([[y1, -y1], [-y1, y1+y2]])
    elif nCond == 3: # core + sheath  armour
        Zp = [[zc,zcb,zca],[zcb,zb,zba],[zca,zba,za]]
        
        y2 = Yci(omega, r[3], r[4], eps2, eps0)
        y3 = Yci(omega, r[5], r[6], eps3, eps0)
        Yp = [[y1, -y1, 0],[-y1, y1+y2, -y2],[0, -y2, y2+y3]]
        
    return Zp, Yp

# TODO testar
def montaZY(omega, h, r, x, sigmas, Zp, Yp):
    dim = r.size
    ncabos = x.size # número de fases, no caso, o número de cabos
    k = numpy.floor(dim/2)*ncabos
    # impedancia do solo. Usei a identidade para criar a matrix.
    # As respectivas posições serão substituidas pelos valores 
    # corretos
    fd2 = numpy.floor(dim/2)
    k = fd2*ncabos
    aux = numpy.ones( [fd2, fd2] )
    Zcp = numpy.diag( numpy.ones(k)*Zp )
    
    rext = r[dim-1]
    spem = numpy.array([])
    for i in range(ncabos):
        for j in range(ncabos):
            if i == j:
                zz = ZSolo(omega, rext, h[i], h[j], sigmas, mu0)
            else:
                zz = ZSolo(omega, abs(x[i] - x[j]), h[i], h[j], sigmas, mu0)
            
            spem = numpy.append(spem, zz)
    
    solo = numpy.array([])
    for i in range(ncabos):
        for j in range(ncabos):
            solo = numpy.append(solo, spem[i,j]*aux)
    
    Z = Zcp + solo
    # FIXME se Zp, Yp forem matrizes, vai dar ruim
    Y = numpy.diag( numpy.ones(k)*Yp )
    
    return Z, Y

### Line nodal admittance assembly
# Calculo da matriz de admitancia nodal a partir de parametros
# unitarios e comprimento do circuito

def ynLT(Z, Y, length):
    Z1 = Z
    Y1 = Y
    eigval, eigvect = numpy.linalg.eig( numpy.dot(Z1, Y1) )
    d = numpy.sqrt(eigval)
    Tv = eigvect
    Tvi = numpy.linalg.inv(Tv)
    hm = numpy.exp(-d*length)
    Am = d*(1 + hm**2)/(1 - hm**2)
    Bm = -2.0*d*hm/(1 - hm**2)
    Z1_invTv = numpy.dot( numpy.linalg.inv(Z1), Tv )
    
    y11 = numpy.dot(Z1_invTv, numpy.diag(Am))
    y11 = numpy.dot(y11, Tvi)
    y12 = numpy.dot(Z1_invTv, numpy.diag(Bm))
    y12 = numpy.dot(y12, Tvi)
    return y11, y12


# %%  PROGRAMA DE REPRODUÇÃO DO ARTIGO DO GUSTAVSEN
# CASO DA LINHA DE TRANSMISSÃO COM 3 FASES E 2 PARARRAIOS - CIRCUITO ABERTO
# 20pwrd02gustavsen

### Geometria do problema
xc = numpy.array([-4.5, 0.0, 4.5, -2.25, 2.25])
yc = numpy.array([11.0, 11.0, 11.0, 14.8, 14.8])
r1 = 21.66e-3/2
r0 = 0
Rdc = 0.121e-3
rpr = 12.33e-3/2
Rdcpr = 0.359e-3
l = 25e3
rho = 100.0
npr = 2
rfonte = 0.01
gf = 1/rfonte

fig_geometria = plt.figure()
ax_geometria = fig_geometria.add_subplot(111)
ax_geometria.plot(xc, yc, 'o')
ax_geometria.set_ylim([0, 17])

### Dados de amostragem
n = 4*1024
T = 8e-3
dt = T/n
t = dt*numpy.arange(0, n)
c = -numpy.log(0.001)/T
kk = numpy.arange(0, n/2 + 1)
dw = 2*Pi/n/dt

def sigma(jota, alpha=0.53836):
    return alpha + (1 - alpha)*numpy.cos(2*Pi*jota/n)

sk = -1j*c + dw*kk
nf = sk.size
print("T/n =", T/n)

### Solução no domínio da frequência
#v1out = numpy.zeros(nf)
v1out = numpy.array([])
for nm in range(nf):
    omega = sk[nm]
    Z, Y = cZYlt(omega, xc, yc, 1/rho, Rdc, r1, 0., npr, Rdcpr, rpr)
    A, B = ynLT(Z, Y, l)
    term1 = numpy.diag([gf, gf, gf])
    term2 = numpy.diag([0., 0., 0.])
    Ynodal = numpy.bmat([ [A + term1, B], [B, A + term2] ])
    exci = numpy.append(100/(omega*1j), numpy.zeros(5))
    #v1out[nm] = numpy.linalg.solve(Ynodal, exci)
    val = numpy.linalg.solve(Ynodal, exci)
    try:
        v1out = numpy.vstack( (v1out, val) )
    except ValueError:
        v1out = val
    
### Domínio do tempo com NILT
def montaLap(m, nn):
    outlow = m
    #lowerhalf = numpy.delete(outlow, nf)
    lowerhalf = numpy.vstack( (outlow[:nn-1], outlow[nn:]) )
    upperhalf = numpy.flip(numpy.conjugate(outlow), 0)
    upperhalf = numpy.vstack( (upperhalf[:nn-1], upperhalf[nn:]) )
    return numpy.vstack((lowerhalf, upperhalf))

def nILT(F, tau, dt):
    nc = F.shape[1]
    v = []
    for i in range(nc):
        inv_fourier = ifft(F[:,i])
        v += [numpy.array(numpy.exp(c*tau))/dt*inv_fourier.real]
    return numpy.array(v)

sigmakk = sigma(kk)
outlow = v1out[0] * sigmakk[0]
for i in range(1, sigma(kk).shape[0]):
    outlow = numpy.vstack( (outlow, v1out[i] * sigmakk[i]) )
    
F = montaLap(outlow, nf)
sai = nILT(F, t, dt)

#### resultados da simulação extraídos do artigo
### Tempos
tempo = numpy.array([0,
                   0.0744048,
                   0.0744048,
                   0.0744048,
                   0.0818452,
                   0.0892857,
                   0.111607,
                   0.133929,
                   0.193452,
                   0.245536,
                   0.245536,
                   0.245536,
                   0.267857,
                   0.275298,
                   0.282738,
                   0.297619,
                   0.3125,
                   0.342262,
                   0.401786,
                   0.409226,
                   0.416667,
                   0.424107,
                   0.438988,
                   0.446429,
                   0.46875,
                   0.47619,
                   0.498512,
                   0.513393,
                   0.535714,
                   0.580357,
                   0.580357,
                   0.587798,
                   0.610119,
                   0.61756,
                   0.625,
                   0.647321,
                   0.654762,
                   0.684524,
                   0.699405,
                   0.736607,
                   0.736607,
                   0.751488,
                   0.758929,
                   0.78125,
                   0.803571,
                   0.818452,
                   0.833333,
                   0.855655,
                   0.877976,
                   0.900298,
                   0.907738,
                   0.915179,
                   0.922619,
                   0.93006,
                   0.94494,
                   0.974702,
                   0.989583,
                   1.00446,
                   1.01935,
                   1.03423,
                   1.04167,
                   1.07143,
                   1.07887,
                   1.07887,
                   1.08631,
                   1.09375,
                   1.13839,
                   1.16071,
                   1.1756,
                   1.19048,
                   1.2128,
                   1.23512,
                   1.24256,
                   1.25,
                   1.26488,
                   1.27232,
                   1.31696,
                   1.33929,
                   1.34673,
                   1.37649,
                   1.38393,
                   1.40625,
                   1.42113,
                   1.42113,
                   1.42857,
                   1.43601,
                   1.46577,
                   1.51042,
                   1.54018,
                   1.55506,
                   1.57738,
                   1.58482,
                   1.59226,
                   1.59226,
                   1.5997,
                   1.60714,
                   1.62202,
                   1.65923,
                   1.69643,
                   1.71131,
                   1.74107,
                   1.74851,
                   1.76339,
                   1.77083,
                   1.77083,
                   1.81548,
                   1.84524,
                   1.875,
                   1.90476,
                   1.9122,
                   1.91964,
                   1.93452,
                   1.93452,
                   1.94196,
                   1.97173,
                   2.00893,
                   2.03869,
                   2.05357,
                   2.07589,
                   2.09077,
                   2.09077,
                   2.09077,
                   2.09821,
                   2.10565,
                   2.12054,
                   2.1503,
                   2.18006,
                   2.2247,
                   2.24702,
                   2.25446,
                   2.25446,
                   2.27679,
                   2.28423,
                   2.29167,
                   2.30655,
                   2.35119,
                   2.37351,
                   2.39583,
                   2.41815,
                   2.4256,
                   2.4256,
                   2.43304,
                   2.44792,
                   2.45536,
                   2.47024,
                   2.47768,
                   2.5,
                   2.52232,
                   2.5372,
                   2.58185,
                   2.58185,
                   2.58929,
                   2.61161,
                   2.63393,
                   2.64881,
                   2.68601,
                   2.71577,
                   2.74554,
                   2.76786,
                   2.76786,
                   2.79018,
                   2.8125,
                   2.81994,
                   2.86458,
                   2.90179,
                   2.91667,
                   2.92411,
                   2.93155,
                   2.95387,
                   2.97619,
                   2.99851,
                   3.02827,
                   3.0506,
                   3.08036,
                   3.0878,
                   3.09524,
                   3.11756,
                   3.13244,
                   3.14732,
                   3.16964,
                   3.18452,
                   3.20685,
                   3.25149,
                   3.25149,
                   3.25893,
                   3.28869,
                   3.31101,
                   3.34077,
                   3.3631,
                   3.38542,
                   3.41518,
                   3.43006,
                   3.45238,
                   3.48214,
                   3.49702,
                   3.5119,
                   3.53423,
                   3.54167,
                   3.57887,
                   3.59375,
                   3.60119,
                   3.62351,
                   3.64583,
                   3.6756,
                   3.69048,
                   3.72024,
                   3.74256,
                   3.76488,
                   3.77976,
                   3.77976,
                   3.80208,
                   3.81696,
                   3.83929,
                   3.85417,
                   3.87649,
                   3.89137,
                   3.91369,
                   3.93601,
                   3.94345,
                   3.96577,
                   3.99554,
                   4.0253,
                   4.05506,
                   4.06994,
                   4.10714,
                   4.10714,
                   4.12946,
                   4.15923,
                   4.18155,
                   4.18899,
                   4.21131,
                   4.22619,
                   4.26339,
                   4.26339,
                   4.28571,
                   4.30804,
                   4.34524,
                   4.375,
                   4.38988,
                   4.4122,
                   4.43452,
                   4.4494,
                   4.45685,
                   4.47173,
                   4.49405,
                   4.51637,
                   4.53125,
                   4.56101,
                   4.57589,
                   4.59821,
                   4.6131,
                   4.62798,
                   4.6503,
                   4.66518,
                   4.69494,
                   4.7619,
                   4.78423,
                   4.79911,
                   4.82887,
                   4.85119,
                   4.87351,
                   4.89583,
                   4.91815,
                   4.91815,
                   4.93304,
                   4.94048,
                   4.9628,
                   4.99256])
         
### Tensões
tensao = numpy.array([0,
                   0,
                   0.6,
                   1.20784,
                   1.62745,
                   1.86275,
                   1.92549,
                   1.96471,
                   1.98431,
                   1.98431,
                   1.75686,
                   1.11765,
                   0.592157,
                   0.337255,
                   0.223529,
                   0.172549,
                   0.117647,
                   0.0823529,
                   0.0509804,
                   0.0862745,
                   0.682353,
                   1.14118,
                   1.28627,
                   1.47059,
                   1.67451,
                   1.74902,
                   1.81569,
                   1.87059,
                   1.8902,
                   1.92157,
                   1.38824,
                   0.815686,
                   0.764706,
                   0.690196,
                   0.568627,
                   0.419608,
                   0.321569,
                   0.223529,
                   0.180392,
                   0.129412,
                   0.278431,
                   0.980392,
                   1.19216,
                   1.25882,
                   1.33725,
                   1.49412,
                   1.60784,
                   1.69804,
                   1.74902,
                   1.81176,
                   1.62353,
                   1.26667,
                   0.839216,
                   0.780392,
                   0.752941,
                   0.717647,
                   0.623529,
                   0.529412,
                   0.45098,
                   0.376471,
                   0.301961,
                   0.243137,
                   0.333333,
                   0.823529,
                   1.14118,
                   1.20784,
                   1.27451,
                   1.31765,
                   1.38824,
                   1.48627,
                   1.57647,
                   1.67451,
                   1.38039,
                   0.956863,
                   0.745098,
                   0.705882,
                   0.694118,
                   0.67451,
                   0.631373,
                   0.552941,
                   0.482353,
                   0.407843,
                   0.690196,
                   1.05882,
                   1.23529,
                   1.30196,
                   1.32549,
                   1.33333,
                   1.37255,
                   1.43137,
                   1.5098,
                   1.22353,
                   0.980392,
                   0.8,
                   0.705882,
                   0.658824,
                   0.635294,
                   0.654902,
                   0.666667,
                   0.635294,
                   0.568627,
                   0.733333,
                   1.03529,
                   1.30196,
                   1.39608,
                   1.39608,
                   1.38039,
                   1.36471,
                   1.4,
                   1.27843,
                   1.06275,
                   0.713725,
                   0.627451,
                   0.584314,
                   0.556863,
                   0.592157,
                   0.615686,
                   0.627451,
                   0.619608,
                   0.698039,
                   0.964706,
                   1.2,
                   1.35294,
                   1.41569,
                   1.45882,
                   1.46667,
                   1.43137,
                   1.40392,
                   1.38039,
                   1.23922,
                   1.00784,
                   0.705882,
                   0.607843,
                   0.533333,
                   0.505882,
                   0.537255,
                   0.560784,
                   0.576471,
                   0.619608,
                   0.803922,
                   1.05098,
                   1.25098,
                   1.35686,
                   1.42745,
                   1.47451,
                   1.50588,
                   1.51765,
                   1.49412,
                   1.46667,
                   1.43922,
                   1.31373,
                   0.945098,
                   0.666667,
                   0.54902,
                   0.501961,
                   0.478431,
                   0.505882,
                   0.533333,
                   0.839216,
                   1.14118,
                   1.38039,
                   1.45882,
                   1.50588,
                   1.52941,
                   1.51373,
                   1.4549,
                   1.27843,
                   0.945098,
                   0.666667,
                   0.588235,
                   0.52549,
                   0.490196,
                   0.470588,
                   0.498039,
                   0.592157,
                   0.945098,
                   1.15686,
                   1.32157,
                   1.39216,
                   1.45098,
                   1.49804,
                   1.52157,
                   1.52941,
                   1.44314,
                   1.17255,
                   0.792157,
                   0.643137,
                   0.580392,
                   0.509804,
                   0.478431,
                   0.490196,
                   0.690196,
                   1.12941,
                   1.31765,
                   1.38824,
                   1.42745,
                   1.47059,
                   1.50196,
                   1.52157,
                   1.44706,
                   1.15294,
                   0.858824,
                   0.701961,
                   0.631373,
                   0.584314,
                   0.537255,
                   0.505882,
                   0.603922,
                   0.803922,
                   1.00392,
                   1.14902,
                   1.23137,
                   1.32157,
                   1.36863,
                   1.41961,
                   1.45882,
                   1.48235,
                   1.35686,
                   1.03529,
                   0.823529,
                   0.729412,
                   0.623529,
                   0.584314,
                   0.54902,
                   0.639216,
                   0.898039,
                   1.15686,
                   1.21176,
                   1.27843,
                   1.33725,
                   1.37647,
                   1.40784,
                   1.41961,
                   1.2549,
                   0.988235,
                   0.827451,
                   0.709804,
                   0.670588,
                   0.635294,
                   0.6,
                   0.666667,
                   0.823529,
                   1.05882,
                   1.13725,
                   1.21176,
                   1.27451,
                   1.31765,
                   1.34902,
                   1.38039,
                   1.34902,
                   1.16863,
                   0.929412,
                   0.811765,
                   0.74902,
                   0.701961,
                   0.643137,
                   0.827451,
                   1.09804,
                   1.20392,
                   1.27059,
                   1.30588,
                   1.32941,
                   1.35686,
                   1.31765,
                   1.30196,
                   1.16863,
                   0.937255,
                   0.8])

### Comparação Gustavsen
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot( t*1000, sai[3], 'r' )
ax.plot(tempo, tensao, 'b--')
legenda = ["V4 calculado", "Gustavsen"]
ax.legend(legenda)
ax.set_xlim( (0, tempo[-1]) )
ax.set_title("20pwrd0gustavsen_V4")
plt.show()
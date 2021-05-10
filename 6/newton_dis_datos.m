clc
clear all

%niter=... ; % numero maximo de iteraciones
%funcion='f' ; % funcion que define el sistema no lineal
%x0=[...;...; ... ] ; % iterante inicial
%n=... ; % orden del sistema
%h=... ; % paso de discretizacion de la matriz jacobiana
%tol=... ; % valor de tolerancia del test de parada

niter = 100;
funcion='f';
x0=[0; 0];
%x0=[0.5; 0.5];
%x0=[-0.5; -0.5];
%x0=[2; -2];

n = 2;
h=1e-3;
tol=1e-6;
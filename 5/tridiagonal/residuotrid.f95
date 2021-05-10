subroutine residuotrid(n,ad,as,au,b,u,r)
    !*****************************************
    !Calculo del residuo de un sistema R=Au-b
    !*****************************************

    use mod_clreal      !Modulo donde esta la clase definida de reales (clreal)

    implicit none

    !Argumentos
    integer, intent(in)::n        !Orden del S.E.L.
    real(kind=clreal),intent(in)::ad(n)    !Diagonal de la matriz del S.E.L.
    real(kind=clreal),intent(in)::as(n-1)   !Subdiagonal de la matriz del S.E.L.
    real(kind=clreal),intent(in)::au(n-1)   !Superdiagonal de la matriz del S.E.L.
    real(kind=clreal),intent(in)::b(n)      !Termino independiente
    real(kind=clreal),intent(in)::u(n)      !Solucion aproximada
    real(kind=clreal),intent(out)::r(n)     !Residuo

    !Variables locales
    integer::i   !Forma 1
    !integer::j      !Forma 2 y 3
    real(kind=clreal)::aux      !Forma 1
    !real(kind=clreal)::aux(n)    !Forma 2 y 3

    !Forma 1 de calcular el residuo
    aux = 0._clreal  
    aux = aux + ad(1)*u(1) + au(1)*u(2)
    r(1) = aux - b(1)
    do i=2,n-1  
        aux = 0._clreal
        aux = aux + as(i-1)*u(i-1) + ad(i)*u(i) + au(i)*u(i+1)
        r(i) = aux-b(i)
    end do
    aux = aux + as(n-1)*u(n-1) + ad(n)*u(n)
    r(n) = aux - b(n)

end subroutine residuotrid
subroutine residuo(m,n,a,b,u,r)
    !****************************************
    !Subrutina de calculo del residuo de un S.E.L., r=Au-b
    !****************************************

    use mod_clreal
    implicit none

    !Argumentos
    integer, intent(in)::m        !Numero de filas de la matriz
    integer, intent(in)::n        !Numero de columnas de la matriz
    real(kind=clreal),intent(in)::a(m,n)    !Matriz del S.E.L.
    real(kind=clreal),intent(in)::b(m)      !Termino independiente
    real(kind=clreal),intent(in)::u(n)      !Solucion aproximada
    real(kind=clreal),intent(out)::r(m)     !Residuo

    !Variables locales
    real(kind=clreal)::aux(m)
    integer::j

    !Inicializacion de aux
    aux=0._clreal

    !Calculo de aux=Au
    do j=1,n
        aux=aux + a(:,j)*u(j)
    end do

    !Residuo
    r=aux-b

end subroutine residuo
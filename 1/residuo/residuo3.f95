subroutine residuo(m,n,a,b,u)
    !****************************************
    !Subrutina de calculo del residuo de un S.E.L., r=Au-b
    !****************************************
    
    use mod_clreal
    implicit none

    !Argumentos
    integer, intent(in)::m        !Numero de filas de la matriz
    integer, intent(in)::n        !Numero de columnas de la matriz
    real(kind=clreal),intent(in)::a(m,n)    !Matriz del S.E.L.
    real(kind=clreal),intent(inout)::b(m)   !Termino independiente
    real(kind=clreal),intent(in)::u(n)      !Solucion aproximada

    !Variables locales
    integer::j

    !Se guarda el residuo en el propio vector con el termino independiente
    do j=1,n
        b(:)=b(:)-a(:,j)*u(j)
    end do
    b=-b
    
end subroutine residuo
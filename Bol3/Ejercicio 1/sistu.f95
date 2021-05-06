subroutine sistu(n,a,b,u)
    !*************************************
    !Subrutina de resolución de un sistema con matriz triangular superior
    !Esta versión de la subrutina no modifica el término independiente, b
    !*************************************

    use mod_clreal
    implicit none

    !Argumentos
    integer,intent(in)::                             n      !Orden del S.E.L.L.
    real(kind=clreal),intent(in), dimension(n,n)::   a      !Matriz del S.E.L.
    real(kind=clreal),intent(in), dimension(n)::     b      !Término independiente del S.E.L.
    real(kind=clreal),intent(out), dimension(n)::    u      !Solución del S.E.L.

    !Variables locales
    integer::i,j
    real(kind=clreal)::aux

    u(n)=b(n)/a(n,n)
    do i=n-1,1,-1   !Se retrocede desde n-1 hasta 1
        aux=0._clreal
        do j=i+1,n
            aux = aux + a(i,j)*u(j)
        end do
        u(i) = (b(i)-aux)/a(i,i)
    end do

    !No obstante, esta forma no es la óptima, pues se llama a los elementos de la matriz por filas, y Fortran los almacena por columnas.

    !A continuación se muestra una implementación más eficiente en tiempo de cálculo:
    !do i = n, 1, -1
    !   u(i) = b(i)/a(i,i)
        !Se multiplica toda la columna por u(i)
    !   b(1:i-1) = b(1:i-1) - a(1:i-1,i)*u(i)
    !end do

end subroutine sistu
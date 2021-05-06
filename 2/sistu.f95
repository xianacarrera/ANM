subroutine sistu(n,a,b,u)
    !*********************************************************
    !Subrutina de resolucion de un sistema con matriz triangular superior
    !********************************************************

    use mod_clreal

    implicit none

    !Declarar primero argumentos y luego las variables locales
    integer,intent(in)::n
    real(kind=clreal),intent(out)::u(n)
    !real(kind=clreal),intent(in)::a(n,n),b(n)
    real(kind=clreal),intent(in)::a(n,n)
    real(kind=clreal),intent(inout)::b(n)

    integer::i,j
    !real(kind=clreal)::aux

    !u(n)=b(n)/a(n,n)
    !do i=n-1,1,-1   !Se retrocede desde n-1 hasta 1
    !    aux=0.
    !    do j=i+1,n
    !        aux = aux + a(i,j)*u(j)
    !    end do
    !    u(i) = (b(i)-aux)/a(i,i)
    !end do

    ! No obstante, esta forma no es la óptima, pues se llama a los elementos de la matriz por filas, y Fortran los almacena por columnas.

    ! A continuación se muestra una implementación más eficiente en tiempo de cálculo:
    do i = n, 1, -1
       u(i) = b(i)/a(i,i)
       ! Se multiplica toda la columna por u(i)
       b(1:i-1) = b(1:i-1) - a(1:i-1,i)*u(i)
    end do

end subroutine sistu
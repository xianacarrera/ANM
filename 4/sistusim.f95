subroutine sistusim(n,a,b,u)
    !*********************************************************
    !Subrutina de resolucion de un sistema con matriz triangular superior
    !almacenada en la parte triangular inferior de a.
    !********************************************************

    use mod_clreal

    implicit none

    !Declarar primero argumentos y luego las variables locales
    integer,intent(in)::                                    n !orden del S.E.L.
    real(kind=clreal),dimension(n,n),intent(in)::           a !matriz del S.E.L.
    real(kind=clreal),dimension(n),intent(inout)::          b !termino independiente del S.E.L.
    real(kind=clreal),dimension(n),intent(out)::            u !solucion del S.E.L.

    !Variables locales
    integer::i,j
    real(kind=clreal)::aux

    !Resolvemos un sistema triangular superior con la matriz almacenada
    !en la parte triangular inferior de a. Por tanto, necesitamos a(j,i)
    !y no a(i,j).
    u(n)=b(n)/a(n,n)
    do i=n-1,1,-1   !Se retrocede desde n-1 hasta 1
        aux=0._clreal
        do j=i+1,n
            aux = aux + a(j,i)*u(j)
        end do
        u(i) = (b(i)-aux)/a(i,i)
    end do

end subroutine sistusim
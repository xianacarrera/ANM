subroutine sistu_acalfa(n,a,c,b,u)
    !******************************************
    !Subrutina que resueve un sistema con matriz triangular superior
    !resultante de la eliminacion de Gauss sobre una matriz con la estructura
    !de la matriz del ejercicio 2 del boletin 3.
    !******************************************

    use mod_clreal
    implicit none

    !argumentos
    integer, intent(in)::                               n  !orden del S.E.L.
    real(kind=clreal), dimension(n), intent(in)::       a  !vector con la diagonal de la matriz A
    real(kind=clreal), dimension(n), intent(in)::       c  !primera fila de A (exceptuando a__1)
    real(kind=clreal), dimension(n), intent(in)::       b  !termino independiente del S.E.L.
    real(kind=clreal), dimension(n), intent(out)::      u  !solucion del S.E.L.

    !variables locales
    integer::i,j

    !los unicos elementos no nulos en las filas 2,...,n son los de la diagonal
    do i=n,2,-1
        u(i) = b(i)/a(i)
    end do

    !primera fila (completa)
    u(1) = b(1)
    do j=2,n
        u(1) = u(1) - c(j-1)*u(j) 
    end do
    u(1) = u(1)/a(1)

end subroutine sistu_acalfa
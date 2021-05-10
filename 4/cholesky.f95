subroutine cholesky(n, a, deter)
    !****************************
    !Obtencion de la factorizacion de Cholesky A=BB^T
    !****************************

    use mod_clreal
    implicit none

    !Argumentos
    integer, intent(in)::                                   n        !Orden del S.E.L.
    real(kind=clreal), intent(inout), dimension(:,:)::      a(n,n)   !Matriz del S.E.L.
    real(kind=clreal), intent(out)::                        deter  !Determinante de la matriz

    !Variables locales
    integer::i, j

    !Inicializacion del determinante
    deter=1.

    !calculo de la matriz B

    !bucle de columnas
    do j=1,n

        !elemento diagonal de B
        !los elementos de B ya calculados se almacenan en a
        a(j,j) = a(j,j) - sum(a(j,1:j-1)*a(j,1:j-1))

        if (a(j,j) < 1.e-12) then
            print*
            print*, ' ** Radicando ', j, '<= 0 en la matriz B,'
            print*, '    la matriz del sistema no es definida positiva! **'
            stop
        end if

        !sabemos que el radicando es positivo
        a(j,j) = sqrt(a(j,j))

        !bucle de filas
        do i=j+1,n
            a(i,j) = a(i,j) - sum(a(i, 1:j-1)*a(j, 1:j-1))
            a(i,j) = a(i,j)/a(j,j)
        end do

        !actualizacion del determinante
        !el determinante es igual al producto de los elementos diagonales de B al cuadrado
        deter = deter*a(j,j)
    end do

    !fin del calculo del determinante
    deter = deter**2    

end subroutine cholesky
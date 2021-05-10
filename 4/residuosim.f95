subroutine residuosim(n,a,b,u,r)
    !*****************************************
    !Calculo del residuo de un sistema R=Au-b
    !para la factorizacion de Cholesky.
    !En este caso, la matriz A está almacenada en la parte triangular
    !inferior.
    !*****************************************

    use mod_clreal      !Modulo donde esta la clase definida de reales (clreal)
    implicit none

    !Argumentos
    integer,intent(in)::                             n   !Orden del S.E.L.
    real(kind=clreal),dimension(n,n),intent(in)::    a   !Matriz del S.E.L.
    real(kind=clreal),dimension(n),intent(in)::      b   !Termino independiente
    real(kind=clreal),dimension(n),intent(in)::      u   !Solucion aproximada
    real(kind=clreal),dimension(n),intent(out)::     r   !Residuo

    !Variables locales
    integer::i,j
    real(kind=clreal)::aux  

    !Primera version del calculo del residuo
    !El termino independiente no se modifica
    do i=1,n
        aux = 0._clreal
        do j=1,i
            aux = aux + a(i,j)*u(j)
        end do
        do j = i+1, n
            aux = aux + a(j,i)*u(j)
        end do
        r(i) = aux-b(i)
    end do

end subroutine residuosim
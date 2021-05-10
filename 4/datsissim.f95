subroutine datsissim(n, a, b)
    !*******************************************************************
    !subrutina de inicializacion para el metodo de Cholesky
    !que calcula la solucion de un S.E.L., Au=b,
    !A simetrica y definida positiva
    !Almacenada en la parte triangular inferior
    !*******************************************************************

    use mod_clreal      !Modulo donde está la clase definida de reales (clreal)
    implicit none

    integer, intent(in)::                       n                       !Orden del S.E.L.
    real(kind=clreal),dimension(n,n), intent(out)::a(n,n)        !Matriz del S.E.L.
    real(kind=clreal),dimension(n), intent(out)::b(n)          !Termino independiente

    !variables locales
    integer::i
    !character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****Lectura de datos****'
    print*

    !inicializacion de la matriz de coeficientes simetrica del S.E.L.
    print*
    print*, 'La parte triangular inferior de la matriz simetrica A es: '
    do i=1,n 
        read*, a(i, 1:i)
        print*, a(i, 1:i)
    end do

    print*
    print*,'El termino independiente b es:'
    read*, b
    !print formato2, b
    print*, b

end subroutine datsissim
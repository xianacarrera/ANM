subroutine datsis(n, a, b)
    !*******************************************************************
    !Lectura y escritura de:
    !A matriz cuadrada
    !b termino independiente
    !*******************************************************************

    use mod_clreal      !modulo donde esta la clase definida de reales (clreal)
    implicit none

    !argumentos
    integer, intent(in)::n                       !orden del S.E.L.
    real(kind=clreal),intent(out)::a(n,n)        !matriz del S.E.L.
    real(kind=clreal),intent(out)::b(n)          !termino independiente

    !variables locales
    integer::i
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****Lectura de datos****'
    print*

    !Lectura y escritura de la matriz del S.E.L.
    print*
    print*,'La matriz del S.E.L. A es:'
    do i = 1,n
        read*, a(i,:)
        !print formato2, a(i,:)
        print formato2, a(i,:)
    end do

    print*
    print*,'El termino independiente b es:'
    read*, b
    !print formato2, b
    print formato2, b

end subroutine datsis
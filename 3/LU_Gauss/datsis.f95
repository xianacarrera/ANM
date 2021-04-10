subroutine datsis(n, a, b)
    !*******************************************************************
    !Lectura y escritura de:
    !A matriz cuadrada
    !b término independiente
    !*******************************************************************

    use mod_clreal      !Módulo donde está la clase definida de reales (clreal)
    implicit none

    integer, intent(in)::n                       !Orden del S.E.L.
    real(kind=clreal),intent(out)::a(n,n)        !Matriz del S.E.L.
    real(kind=clreal),intent(out)::b(n)          !Término independiente

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
        print formato2, a(i,:)
    end do

    print*
    print*,'El termino independiente b es:'
    read*, b
    print formato2, b

end subroutine datsis
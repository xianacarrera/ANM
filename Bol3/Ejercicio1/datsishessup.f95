subroutine datsishessup(n, a, b)
    !*******************************************************************
    !Lectura y escritura de:
    !A matriz Hessenberg superior
    !b término independiente
    !*******************************************************************

    use mod_clreal      !Módulo donde está la clase definida de reales (clreal)
    implicit none

    !Argumentos
    integer, intent(in)::                              n    !Orden del S.E.L.
    real(kind=clreal),intent(out),dimension(n,n)::     a    !Matriz del S.E.L.
    real(kind=clreal),intent(out), dimension(n)::      b    !Término independiente

    !Variables locales
    integer::i,j
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****Lectura de datos****'
    print*

    !Lectura y escritura de la matriz Hessenberg superior del S.E.L.
    print*
    print*,'Los elementos de A hasta la subdiagonal son:'
    read*, a(1, 1:n)
    print*, a(1, 1:n)
    do i = 2,n
        read*, a(i, i-1:n)
        print formato2, a(i, i-1:n)
    end do

    !Escritura completa de A
    print*
    print*,'La matriz A es:'
    print formato2, a(1, 1:n)
    do i=2,n
        print formato2, (0._clreal, j=1,i-2), a(i, i-1:n)
    end do

    !Lectura y escritura del término independiente del S.E.L., b.
    print*
    print*,'El termino independiente b es:'
    read*, b
    print formato2, b

end subroutine datsishessup
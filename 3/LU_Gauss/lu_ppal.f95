program lu_ppal
    !***********************************************************
    !Programa principal de resolucion de un S.E.L. por factorizacion LU
    !***********************************************************

    use mod_clreal
    implicit none

    integer::n, reserva, i, j
    real(kind=clreal)::deter
    real(kind=clreal), allocatable, dimension(:,:)::a, aa
    real(kind=clreal), allocatable, dimension(:)::b, bb, x, y, r

    !Formato
    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*,'****Resolucion del sistema por factorizacion LU****'

    print*
    print*,'Introducir orden del S.E.L: '
    read*, n
    print formato1, n
    print*

    allocate(a(n,n), aa(n,n), b(n), bb(n), x(n), y(n), r(n), stat = reserva)

    aa = a
    bb = b

    !Lectura de datos
    call datsis(n, a, b)

    print*
    print*,'****Fin de la lectura de datos****'
    print*

    !Transformación del S.E.L. en uno equivalente con matriz triangular superior por factorización LU
    call lu(n, a, deter)

    !Escritura de la matriz triangular superior resultante
    print*
    print*,'La matriz triangular superior U es:'
    do i = 1,n
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print formato2, (0._clreal, j=1,i-1), a(i,i:n)
    end do

    !Escritura de la matriz triangular inferior resultante
    print*
    print*,'La matriz triangular inferior L es:'
    do i = 1, n
        !Imprimo los elementos de la matriz con 1's en la diagonal y 0's sobre 
        !la misma
        print formato2, a(i,1:i-1), 1._clreal
    end do

    !Escritura del determinante
    print*
    print*, 'El determinante de la matriz equivalente es: '
    print formato2, deter
    print*

    !Resolucion del sistema Ly=b
    call sistl1(n, a, b, y)

    !Escritura de la solucion del sistema Ly=b
    print*
    print*,'La solucion del sistema triangular inferior es: '
    print formato2, y

    !Resolución del sistema Ux=y. x es la solucion del sistema original
    call sistu(n, a, y, x)

    !Escritura de la solucion del sistema
    print*
    print*,'La solucion del sistema es: '
    print formato2, x

    !Calculo del residuo r=Au-b
    call residuo(n, n, aa, bb, x, r)

    !Escritura del residuo
    print*
    print*, 'El residuo es: '
    print formato2, r
    print*

    !Liberacion de memoria
    deallocate(a, aa, b, bb, x, y, r)

end program lu_ppal
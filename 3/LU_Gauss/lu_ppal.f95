program lu_ppal
    !Programa principal relativo al método directo de Gauss
    use mod_clreal
    implicit none

    integer::n, reserva, i, j
    real(kind=clreal)::deter
    real(kind=clreal), allocatable, dimension(:,:)::a, aa
    real(kind=clreal), allocatable, dimension(:)::b, bb, x, y, r

    print*,'****Resolución del sistema por factorización LU****'

    print*
    print*,'Introducir orden del S.E.L: '
    read*, n
    print*, n
    print*

    allocate(a(n,n), aa(n,n), b(n), bb(n), x(n), y(n), r(n), stat = reserva)

    aa = a
    bb = b
    !No es necesario hacer una copia de b

    !Lectura de datos
    call datsis(n, a, b)

    !Transformación del S.E.L. en uno equivalente con matriz triangular superior por factorización LU
    call lu(n, a, deter)

    print*
    print*,'La matriz triangular superior U es:'
    do i = 1,n
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print*, (0._clreal, j=1,i-1), a(i,i:n)
    end do

    print*
    print*,'La matriz triangular inferior L es:'
    do i = 1, n
        !Imprimo los elementos de la matriz con 1's en la diagonal y 0's sobre la misma
        print*, a(i,1:i-1), 1
    end do

    print*
    print*, 'El determinante de la matriz equivalente es: '
    print*, deter
    print*

    !Resolución del sistema Ly=b
    call sistl1(n, a, b, y)

    !Escritura de la solución del sistema Ly=b
    print*
    print*,'La solución del sistema trianguar inferior es: '
    print*, y

    !Resolución del sistema Ux=y. x es la solución del sistema original
    call sistu(n, a, y, x)

    !Escritura de la solución del sistema
    print*
    print*,'La solución del sistema es: '
    print*, x

    !Cálculo del residuo r=Au-b
    call residuo(n, n, aa, bb, x, r)

    print*
    print*, 'El residuo es: '
    print*, r
    print*

    !Liberación de memoria
    deallocate(a, aa, b, bb, x, y, r)

end program lu_ppal
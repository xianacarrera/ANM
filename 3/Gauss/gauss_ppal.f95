program gauss_ppal
    !Programa principal relativo al método directo de Gauss
    use mod_clreal
    implicit none

    integer::n, reserva, i, j
    real(kind=clreal)::deter
    real(kind=clreal), allocatable, dimension(:,:)::a, aa
    real(kind=clreal), allocatable, dimension(:)::b, bb, u, r

    !Formatos
    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*,'****Resolución del sistema mediante el método de Gauss****'

    print*
    print*,'Introducir orden del S.E.L: '
    print*
    read*, n
    print*, n
    print*

    allocate(a(n,n), aa(n,n), b(n), bb(n), u(n), r(n), stat = reserva)

    aa = a
    bb = b

    !Lectura de datos
    call datsis(n, a, b)

    print*
    print*,'******Fin de la lectura de datos*********'
    print*

    !Transformación del S.E.L. en uno equivalente con matriz triangular superior por Gauss
    call gauss(n, a, b, deter)

    print*
    print*,'La matriz triangular superior U es:'
    print*
    do i = 1,n
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print formato2, (0._clreal, j=1,i-1), a(i,i:n)
    end do

    print*
    print*, 'El término independiente del sistema equivalente b es: '
    print*
    print formato2, b
    print*

    print*
    print*, 'El determinante de la matriz equivalente es: '
    print*
    print formato2, deter
    print*

    !Resolución del sistema con matriz triangular superior
    call sistu(n, a, b, u)

    print*
    print*, 'La solución del S.E.L. es: '
    print*
    print formato2, u
    print*

    !Residuo del cálculo de la solución
    call residuo(n, n, aa, bb, u, r)

    print*
    print*, 'El residuo es: '
    print*
    print formato2, r
    print*

    deallocate(a, aa, b, bb, u, r)

end program gauss_ppal
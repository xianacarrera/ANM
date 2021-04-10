program lupp_ppal
    !Programa principal relativo al método directo de Gauss
    use mod_clreal
    implicit none

    !argumentos
    real(kind=clreal)::deter
    real(kind=clreal), allocatable, dimension(:,:)::a, aa
    real(kind=clreal), allocatable, dimension(:)::b, bb, x, y, r
    integer, allocatable, dimension(:)::    ip

    integer::                               n !orden del S.E.L.

    !variables locales
    integer::i,j

    print*,'****FACTORIZACION PA=LU****'

    print*,'Resolucion del sistmea'

    !lectura del orden del sistema
    print*
    print*,'Introducir orden del S.E.L: '
    read*, n
    print*, n
    print*

    !asignacion de memoria dinamica
    allocate(a(n,n), aa(n,n), b(n), bb(n), x(n), y(n), r(n), ip(n))

    !Lectura de datos
    call datsis(n, a, b)

    aa=a

    !calculo de la factorizacion PA=LU
    call lupp(n, a, ip, deter)

    !escritura de la factorizacion PA=LU
    print*
    print*,' La matriz triangular inferior L es:'
    do i = 1,n
        print*, a(ip(i),1:i-1),1.
    end do

    print*
    print*,'La matriz triangular superior U es:'
    do i = 1, n
        print*, (0._clreal, j=1,i-1), a(ip(i),i:n)
    end do

    print*
    print*, 'El determinante de la matriz equivalente es: '
    print*, deter
    print*

    !calculo de la solucion del S.E.L. triangular inferior Ly=b
    call sistlpf1(n, a, b, y, ip)

    !escritura de la solución del sistema Ly=b
    print*
    print*,'La solución del sistema trianguar inferior Lv=Pb es: '
    print*, y

    !calculo de la solucion del S.E.L. trinagular superior Ux=y
    call sistupf2(n, a, y, x, ip)

    !escritura de la solucion
    print*
    print*,'La solución del sistema es: '
    print*, x

    !calculo del residuo r=Ax-b
    call residuo(n, n, aa, b, x, r)

    !escritura del residuo r=Ax-b
    print*
    print*, 'El residuo r=Ax-b es: '
    print*, r

    !Liberación de memoria
    deallocate(a, aa, b, x, y, r, ip)

end program lupp_ppal
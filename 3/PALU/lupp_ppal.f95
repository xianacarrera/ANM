program lupp_ppal
    !Programa principal relativo al método directo de Gauss
    use mod_clreal
    implicit none

    !argumentos
    integer::                                               n !orden del S.E.L.
    real(kind=clreal), allocatable, dimension(:,:)::        a   !matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:,:)::        aa  !copia de la matriz a
    real(kind=clreal), allocatable, dimension(:)::          b   !termino independiente del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::          x   !solucion del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::          y   !solucion del sistema intermedio
    real(kind=clreal), allocatable, dimension(:)::          r   !residuo
    real(kind=clreal)::                                     deter   !determinante de la matriz del S.E.L.
    integer, allocatable, dimension(:)::                    ip  !permutacion de filas

    !variables locales
    integer::i,j

    !formatos
    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'



    print*,'****FACTORIZACION PA=LU****'

    print*,'Resolucion del sistema PA=LU'
    print*,'     A matriz cuadrada'

    !lectura del orden del sistema
    print*
    print*,'Introducir orden del S.E.L: '
    read*, n
    print formato1, n
    print*

    !asignacion de memoria dinamica
    allocate(a(n,n), aa(n,n), b(n), x(n), y(n), r(n), ip(n))

    !lectura de datos
    call datsis(n, a, b)

    !copia de la matriz para el calculo del residuo
    aa=a

    !calculo de la factorizacion PA=LU
    call lupp(n, a, ip, deter)

    !escritura de la matriz P
    print*
    print*,'La matriz de permutacion de filas es:'
    do i=1,n
        print formato2, (0._clreal, j=1,ip(i)-1), 1._clreal, (0._clreal, j=ip(i)+1,n)
    end do

    !escritura de la factorizacion PA=LU
    print*
    print*,' La matriz triangular inferior L es:'
    do i = 1,n
        print formato2, a(ip(i),1:i-1),1.
    end do

    print*
    print*,'La matriz triangular superior U es:'
    do i = 1, n
        print formato2, (0._clreal, j=1,i-1), a(ip(i),i:n)
    end do

    print*
    print*, 'El determinante de la matriz equivalente es: '
    print formato2, deter

    !calculo de la solucion del S.E.L. triangular inferior Ly=b
    call sistlpf1(n, a, b, y, ip)

    !escritura de la solución del sistema Ly=b
    print*
    print*,'La solucion del sistema triangular inferior Lv=Pb es: '
    print formato2, y

    !calculo de la solucion del S.E.L. trinagular superior Ux=y
    call sistupf2(n, a, y, x, ip)

    !escritura de la solucion
    print*
    print*,'La solucion del sistema es: '
    print formato2, x

    !calculo del residuo r=Ax-b
    call residuo(n, n, aa, b, x, r)

    !escritura del residuo r=Ax-b
    print*
    print*, 'El residuo r=Ax-b es: '
    print formato2, r

    !Liberación de memoria
    deallocate(a, aa, b, x, y, r, ip)

end program lupp_ppal
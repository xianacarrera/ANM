program cholesky_ppal
    !***********************************************************
    !Programa principal de resolucion de un S.E.L. por factorizacion de Cholesky
    !A=BB
    !***********************************************************

    use mod_clreal
    implicit none

    integer::                                           n     !orden del S.E.L.
    real(kind=clreal), allocatable, dimension(:,:)::    a     !matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:,:)::    aa    !copia de a
    real(kind=clreal), allocatable, dimension(:)::      b     !termino independiente del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::      bb    !copia de b
    real(kind=clreal), allocatable, dimension(:)::      x     !solucion del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::      y     !solucion del sistema intermedio
    real(kind=clreal), allocatable, dimension(:)::      r     !residuo
    real(kind=clreal)::                                 deter !determinante de la matriz del sistema

    !variables locales
    integer:: i,j

    print*,'****Resolucion del sistema por factorizacion de Cholesky****'

    print*
    print*,'Introducir orden del S.E.L: '
    read*, n
    print*, n
    print*

    !reserva de memoria
    allocate(a(n,n), aa(n,n), b(n), bb(n), x(n), y(n), r(n))

    !lectura de datos
    call datsissim(n, a, b)

    !copia de a para calcular el residuo, una vez hallada la solucion
    !se guarda la copia de a por columnas
    do j=1,n        !se guarda la parte triangular
        aa(j:n, j) = a(j:n, j)
    end do

    !Copia de b
    bb = b

    print*
    print*,'****Fin de la lectura de datos****'
    print*

    !obtencion de la factorizacion A=BB
    call cholesky(n, a, deter)

    print*
    print*,'La matriz B es:'
    do i = 1,n
        print*, a(i, 1:i)
    end do

    print*
    print*, 'El determinante de la matriz del sistema es: '
    print*, deter
    print*

    !calculo de la solucion del S.E.L. triangular superior B*y = b
    call sistl(n,a,b,y)

    !escritura de la solucion del sistema t.i. calculada
    print*
    print*, 'La solucion del sistema t.i. es:'
    print*, y

    !calculo de la solucion del S.E.L. triangular superior Bt*x = y
    call sistusim(n,a,y,x)

    !escritura de la solucion calculada
    print*
    print*, 'La solucion es:'
    print*, x

    !calculo del residuo de la solucion calculada
    call residuosim(n,aa,bb,x,r)

    !escritura del residuo de la solucion calculada
    print*
    print*,'El residuo del sistema, r=Au-b, es:'
    print*,r

    !escritura de la norma 2 del residuo del sistema
    print*
    print*, 'La norma ||r|| es:', sqrt(dot_product(r,r))
    print*, '              2'

    !liberacion de memoria
    deallocate(a, aa, b, bb, x, y, r)

end program cholesky_ppal
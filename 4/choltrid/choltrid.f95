program choltrid
    !***********************************************************
    !Programa que realiza la factorizacion de Cholesky
    !sobre una matriz tridiagonal
    !***********************************************************

    use mod_clreal
    use mod_formatos
    implicit none

    real(kind=clreal)::                             deter   !determinante de la matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::  ad      !diagonal de la matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::  aad     !copia de ad
    real(kind=clreal), allocatable, dimension(:)::  as      !subdiagonal de la matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::  aas     !copia de as
    real(kind=clreal), allocatable, dimension(:)::  b       !termino independiente del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::  x       !solucion del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::  y       !solucion del sistema triangular superior
    real(kind=clreal), allocatable, dimension(:)::  r       !residuo
    integer::                                       n       !orden del S.E.L.
    
    !variables auxiliares
    integer::i, j, j1


    print*,'************ Resolucion del S.E.L. Au=b ************'
    print*
    print*, '         Factorizacion de Cholesky        '
    print*, '             Caso tridiagonal'


    !lectura y escritura del orden del sistema
    print*
    print*,'Introducir orden del S.E.L: '
    read*, n
    print*, n
    print*

    !reserva de memoria
    allocate(ad(n), aad(n), as(n-1), aas(n-1), b(n), x(n), y(n), r(n))

    !lectura de datos
    if (n < 2) then
        print*, 'La matriz ha de tener, al menos, orden 2'
        stop
    end if

    !inicializacion de la matriz de coeficientes simetrica del S.E.L.
    print*
    print*, 'La diagonal principal de A es:'
    read*, ad
    !print decimales, ad
    print*, ad
    print*, 'La subdiagonal de A es:'
    read*, as
    !print decimales, as
    print*, as

    !lectura del termino independiente
    print*
    print*, 'El termino independiente del S.E.L. es:'
    read*, b
    !print decimales, b
    print*, b

    !copias de ad y as para calcular el residuo, una vez hayada la solucion
    aad = ad
    aas = as

    !obtencion de la factorizacion A=BB
    !calculo de la matriz B

    if (ad(1) < 1.e-12) then
        print*
        print*, ' ** Radicando ', j, '<= 0 en la matriz B,'
        print*, '    la matriz del sistema no es definida positiva! **'
        stop
    end if

    !actualizacion del determinante
    deter = ad(1)

    !obtencion del elemento diagonal
    ad(1) = sqrt(ad(1))

    do j=1,n-1
        !obtencion del elemento subdiagonal
        !la factorizacion conserva la estructura en banda. Simplificacion del calculo
        as(j) = as(j)/ad(j)

        j1 = j+1

        !radicando para el calculo del elemento diagonal
        ad(j1) = ad(j1) - as(j)**2

        if (ad(j1) < 1.e-12) then
            print*
            print*, ' ** Radicando ', j, '<= 0 en la matriz B,'
            print*, '    la matriz del sistema no es definida positiva! **'
            stop
        end if

        !actualizacion del determinante
        deter = deter*ad(j1)

        !obtencion del elemento diagonal
        ad(j1)=sqrt(ad(j1))
    
    end do

    print*
    print*, ' En la factorizacion A=BB'
    print*, ' La diagonal principal de B es:'
    !print decimales, ad
    print*, ad
    print*, ' La subdiagonal de B es:'
    !print decimales, as
    print*, as
    print*
    print*, ' El determinante de la matriz del S.E.L. es:'
    print*, deter
    print*

    ! El determinante no se eleva al cuadrado porque ya lo estamos actualizando antes de hacer la raiz

    !calculo de la solucion del S.E.L. triangular
    y(1) = b(1)/ad(1)

    do i = 2, n
        y(i) = (b(i) - as(i-1)*y(i-1))/ad(i)
    end do

    !calculo de la solucion del S.E.L. triangular superior
    x(n) = y(n)/ad(n)

    do i = n-1, 1, -1
        x(i) = (y(i) - as(i)*x(i+1))/ad(i)
    end do

    !escritura de la solucion
    print*
    print*, ' La solucion es:'
    print*, x

    !calculo del residuo de la solucion
    !operamos con las copias de los vectores originales
    r(1) = aad(1)*x(1)+aas(1)*x(2)-b(1)
    do i=2, n-1
        r(i) = aas(i-1)*x(i-1) + aad(i)*x(i) + aas(i)*x(i+1) - b(i)
    end do
    r(n) = aas(n-1)*x(n-1) + aad(n)*x(n)-b(n)

    !escritura del residuo de la solucion
    print*
    print*, ' El residuo del sistema, r=Au-b, es:'
    print*, r

    !escritura de la norma 2 del residuo del sistema
    print*
    print*, ' La norma ||r|| es:', sqrt(dot_product(r,r))
    print*, '               2 '

    !liberacion de memoria
    deallocate(ad, aad, as, aas, b, x, y, r)

end program choltrid
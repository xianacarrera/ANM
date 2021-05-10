subroutine initertrid(n, ad, as, au, b, u, eps, nitmax)
    !*******************************************************************
    !subrutina de lectura de datos de
    !los metodos iterativos de
    !Jacobi, Gauss-Seidel y relajacion
    !Generalizacion para matrices tridiagonales
    !*******************************************************************

    use mod_clreal      !modulo donde está la clase definida de reales (clreal)
    use mod_formatos    !modulo de formatos de impresion
    implicit none

    integer, intent(in)::                        n                              !orden del S.E.L.
    real(kind=clreal), dimension(n),intent(out)::      ad       !diagonal de la matriz del S.E.L.
    real(kind=clreal), dimension(n-1), intent(out)::   as       !subdiagonal de la matriz del S.E.L.
    real(kind=clreal), dimension(n-1), intent(out)::   au       !superdiagonal de la matriz del S.E.L.
    real(kind=clreal), dimension(n), intent(out)::     b        !termino independiente
    real(kind=clreal), dimension(n), intent(out)::     u        !iterante inicial
    real(kind=clreal), intent(out)::                   eps      !parametro para el test de parada:
                                                         !  (k)    (k-1)
                                                         !|u    - u     | < eps
                                                         !               inf
    integer, intent(out)::                           nitmax   !numero maximo de iteraciones permitido

    !variables locales
    integer::i

    !inicializacion de matriz, termino independiente, iterante inicial, eps y nitmax
    print*
    print*,'****Lectura de datos****'
    print*

    !lectura y escritura de la diagonal de la matriz del S.E.L.
    print*
    print*,'La diagonal de la matriz de coeficientes A es:'
    read*, ad
    !print decimales, ad
    print*, ad

    !lectura y escritura de la subdiagonal de la matriz del S.E.L.
    print*
    print*,'La subdiagonal de la matriz de coeficientes A es:'
    read*, as
    !print decimales, au
    print*, as

    !lectura y escritura de la superdiagonal de la matriz del S.E.L.
    print*
    print*,'La superdiagonal de la matriz de coeficientes A es:'
    read*, au
    !print decimales, au
    print*, au

    !lectura y escritura del termino independiente del S.E.L.
    print*
    print*,'El termino independiente b es:'
    read*, b
    !print decimales, b
    print*, b

    !lectura y escritura del iterante inicial
    print*
    print*,'                       (0)'
    print*,' El iterante inicial, u   , es:'
    read*, u
    !print*, decimales, u
    print*, u

    !lectura y escritura de eps
    print*
    print*,' El parametro eps para el test de parada es:'
    read*, eps
    !print decimales, eps
    print*, eps

    !lectura y escritura de nitmax
    print*
    print*,' El numero maximo de iteraciones permitido es:'
    read*, nitmax
    print*, nitmax

end subroutine initertrid
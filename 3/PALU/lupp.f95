subroutine lupp(n, a, ip, deter)
    !****************************************************
    !factorización PA=LU a partir del método de Gauss
    !con estrategia de pivote parcial
    !****************************************************
    !Dpto. de Matematatica Aplicada. Facultad de Matematicas. USC

    use mod_clreal
    implicit none

    !argumentos
    integer, intent(in)::n                     ! Orden del S.E.L.
    real(kind=clreal), dimension(n,n), intent(inout)::a   !Matriz del S.E.L.
    integer, dimension(n), intent(out)::ip      !permutacion de filas
    real(kind=clreal), intent(out)::deter      !Determinante de la matriz

    !variables locales
    integer::i,cont,k,ipiv,ipk,ipi,j
    real(kind=clreal)::piv          !Pivote

    !Inicializacion del determinante
    deter=1._clreal

    !inicialización de la permutacion de filas
    ip=(/(i,i=1,n)/)

    !inicializacion del contador de cambios de filas
    cont=0

    !etapa k-esima de la eliminacion
    do k=1,n-1

        !busqueda del privote y de la fila en que se encuentra
        !ipiv=maxval(maxloc(abs(a(k:n,k))))+k-1: en la version con cambio de filas, sin permutaciones
        piv=a(ip(k),k)
        ipiv=k
        do i=k+1,n
            if (abs(piv) < abs(a(ip(i),k))) then        
                !** Se ha encontrado un elemento de la columna con mayor modulo
                piv=a(ip(i),k)
                ipiv=i
            end if
        end do

        !comprobacion de que el k-pivote no es nulo
        if(abs(piv)<1.e-12) then
            print*
            print*,'Atencion, privote nulo: ',piv,' en la etapa: ',k
            print*,' La matriz del sistema es singular!'
            !** Si piv es 0, entonces todos los elementos de esa columna de la fila k en adelante son 0. Por tanto, el determinante de la matriz es 0 (ya que el determinante de la submatriz es 0) 
            stop
        end if

        !puesta al dia de la permutacion
        !y del contador de cambios de filas
        !si el pivote no esta en la fila k
        if (ipiv/=k) then
            !** Si se ha hecho un intercambio de filas, se actualiza ip (se intercambian los índices correspondientes)
            ipk=ip(ipiv)
            ip(ipiv)=ip(k)
            ip(k)=ipk
            cont=cont+1     !** Contador de cambios de filas (para luego calcular el determinante)
        else 
            ipk=ip(k)
        end if

        print*
        print*,' Permutacion de filas en la etapa: ',k
        print*, ip

        print*
        print*,' Valor de cont en la etapa: ',k
        print*,' cont: ',cont

        !Actualización del determinante
        deter = deter*piv

        !eliminacion
        do i=k+1,n
            ipi=ip(i)
            a(ipi,k)=a(ipi,k)/piv
            do j=k+1,n
                a(ipi,j)=a(ipi,j)-a(ipi,k)*a(ipk,j)
            end do
        end do
    end do

    !Comprobación de que el último pivote no es nulo
    piv=a(ip(n),n)
    if(abs(piv)<1.e-12) then
        print*
        print*,'Atencion, privote nulo: ',piv,' en la etapa: ',k
        print*,' La matriz del sistema es singular!'
        stop
    end if

    !fin del calculo del determinante
    deter=deter*piv*(-1)**cont    

end subroutine lupp
subroutine chol_B(a)
    !*******************
    ! T
    !Obtencion de la factorizacion de Cholesky A=BB
    !B por columnas
    !**********************************************************
    !Dpto. de Matematica Aplicada. Facultad de Matematicas. USC


    use mod_clreal
    use mod_formatos
    implicit none
    
    !argumentos
    real(kind=clreal),dimension(:,:),intent(inout):: a !matriz del S.E.L.
    
    !variables locales
    integer:: n !orden del S.E.L.
    integer:: i,j,k,icount1,icount2,irate
    real(kind=clreal):: det,aux
    n=size(a,1)
    
    
    call system_clock(icount1,irate)
    
    !inicializacion del determinante
    !det=1.
    
    !obtencion de la matriz B
    !bucle de columnas
    do j=1,n
    
        ! bucle de filas

        do i=j,n

            a(i,j)=a(i,j)-sum(a(i,1:j-1)*a(j,1:j-1))

            if(i==j)then
                if(a(j,j)<1.e-12)then
                    print*
                    print formato5,'** Atencion, elto. diag.: ',a(j,j),' en la fila: ',j
                    print formato1,' la matriz del sistema A no es definida positiva **'
                    stop
                else
                    aux=1./sqrt(a(j,j))
                endif
            end if
            a(i,j)=a(i,j)*aux
        end do

        ! actualizacion del determinante
        ! det=det*a(j,j)
    end do

    !escritura del determinante
    !print*
    !print formato5,'El determinante de la matriz del sistema es: ',det**2

    call system_clock(icount2)
    
    print*
    print*,"Tiempo de ejecucion en s, chol_B: ",float(icount2-icount1)/float(irate)
    
    end subroutine chol_B
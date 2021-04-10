program dif_numeros

    use mod_clreal

    implicit none

    print*,'2.-sqrt(2.)**2',2.-sqrt(2.)**2

    print*,'2._clreal-sqrt(2.)**2',2._clreal-sqrt(2.)**2

    print*,'2.-sqrt(2._clreal)**2',2.-sqrt(2._clreal)**2

    print*,'2._clreal-sqrt(2._clreal)**2',2._clreal-sqrt(2._clreal)**2

end program dif_numeros
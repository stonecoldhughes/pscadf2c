	 ! LOCAL <TYPE> <NAME> <Array_Size_1 (# of rows)> <Array_Size_2 (# of cols)>
        #local real A11 6 6
        #local real A12 6 60
        #local real A21 60 6
        #local real A22 60 120

        #local real InvA22 60 60
        #local real A12InvA22 6 60
        #local real A12InvA22A21 6 6

        #local integer row6
        #local integer col6
        #local integer row60
        #local integer col60

        #local integer rowb1
        #local integer colb1
        #local integer rowbn
        #local integer colbn

        #local integer k1
        #local integer k2
        #local integer k3

        #local real b1(6)
        #local real b2(60)
        #local real b3(60)
        #local real b4(60)
        #local real b5(60)
        #local real b6(60)

	 ! Temp. variables for calculation
        #local real k9(3)
        #local real k10(3)

        #local real k11(60)
        #local real k12(60)
        #local real k13(60)
        #local real k14(60)
        #local real k15(60)
        #local real k16(3)
        #local real k17(3)
        #local real k18(3)
        #local real k19(3)
        #local real k20(3)
        #local real k21(3)
        #local real k22(3)
        #local real k23(3)
        #local real k24(3)
        #local real k25(3)
        #local real k26(3)
        #local real k27(3)
        #local real k28(3)
        #local real k29(3)
        #local real k30(3)

        #local real Al 6 6
        #local real Au 6 6         
        #local real y(6)

        #local real TempA1 6 6
        #local real TempA2 6 6
        #local real Tempx1(6)
        #local real Tempx2(60)
        #local real Tempx3(60)
        #local real Tempx4(60)
        #local real Tempx5(60)
        #local real Tempx6(60)

        #local real TempB(6)
        #local real Tempb2(60)
        #local real Tempb3(60)
        #local real Tempb4(60)
        #local real Tempb5(60)
        #local real Tempb6(60)

	 ! Dist. Line. Parameter RLC 3 by 3
        #local real R 3 3
        #local real L 3 3
        #local real C 3 3
	 ! Dist. Line. Parameter with length for main feeder 3 by 3
        #local real R_main 3 3
        #local real L_main 3 3
        #local real C_main 3 3
	 ! Dist. Line. Parameter with length for lateral feeder 3 by 3
        #local real R_lateral 3 3
        #local real L_lateral 3 3
        #local real C_lateral 3 3

	 #local real len_main
	 #local real len_lateral

        #local real h
        #local integer NL
        #local real ratio

	!  Distribution Line Parameters
          R(1,1) = 0.835143985E-03 ! [ohm]
          R(1,2) = 0.131088920E-03
          R(1,3) = 0.132921464E-03
          R(2,1) = 0.131088920E-03
          R(2,2) = 0.827022101E-03
          R(2,3) = 0.128873908E-03
          R(3,1) = 0.132921464E-03
          R(3,2) = 0.128873908E-03
          R(3,3) = 0.830539492E-03

          L(1,1) = 0.219425120E-05 ! [H]
          L(1,2) = 0.954025842E-06
          L(1,3) = 0.828119254E-06
          L(2,1) = 0.954025842E-06
          L(2,2) = 0.223148452E-05
          L(2,3) = 0.758323905E-06
          L(3,1) = 0.828119254E-06
          L(3,2) = 0.758323905E-06
          L(3,3) = 0.221530283E-05

          C(1,1) = 0.297440465E-11 ! [F]
          C(1,2) = 0.102782646E-11
          C(1,3) = 0.597714598E-11
          C(2,1) = 0.102782646E-11
          C(2,2) = 0.318965680E-11
          C(2,3) = 0.292752017E-11
          C(3,1) = 0.597714598E-11
          C(3,2) = 0.292752017E-11
          C(3,3) = 0.347764462E-11

	!  Distribution line lengths
	   len_main = 0.060*1000 ! [m]
	   len_lateral = 0.030*1000

	!  Distribution Line Parameters	
	  do k1 = 1,3,1 	
		do k2 = 1,3,1
			 R_main(k1,k2) = R(k1,k2)*len_main
			 L_main(k1,k2) = L(k1,k2)*len_main

			 R_lateral(k1,k2) = R(k1,k2)*len_lateral
			 L_lateral(k1,k2) = L(k1,k2)*len_lateral

			if(k1.EQ.k2) then
			 	C_main(k1,k2) = (C(k1,1)+C(k1,2)+C(k1,3))*len_main			 
			 	C_lateral(k1,k2) = (C(k1,1)+C(k1,2)+C(k1,3))*len_lateral
			else if(k1.NE.k2) then
			 	C_main(k1,k2) = -C(k1,k2)*len_main		 
			 	C_lateral(k1,k2) = -C(k1,k2)*len_lateral
			endif
	  	end do
      	  end do

      !  Simulation Parameters
	  h = 1e-6
	  NL = 51 ! No. of 3-phase distribution lines

         row6 = 6
         col6 = 6
         row60 = 60
         col60 = 60

         rowb1 = 6
         colb1 = 1
         rowbn = 60
         colbn = 1

       !  Vector [x] Initialization
	  if ($start_in.EQ.0) then
		do k1 = 1,NL,1
			$x_out((k1-1)*6+1) = 0.0 ! ia
			$x_out((k1-1)*6+2) = 0.0 ! ib
			$x_out((k1-1)*6+3) = 0.0 ! ic
			$x_out((k1-1)*6+4) = $vgrid_in(1) ! va
			$x_out((k1-1)*6+5) = $vgrid_in(2) ! vb
			$x_out((k1-1)*6+6) = $vgrid_in(3) ! vc
		end do
	  end if
	  $start_out = 1

	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! MATRIX [A11, A12, A21, A22] initialization
          do k1 = 1, row6, 1 ! 6by6
	  	do k2 = 1, col6, 1
			 A11(k1,k2) = 0.0
	  	end do
      	  end do

          do k1 = 1, row60, 1 ! 60by120 for aug.
	  	do k2 = 1, 2*col60, 1
			 A22(k1,k2) = 0.0
	  	end do
      	  end do

          do k1 = 1, row6, 1 ! 6by60
	  	do k2 = 1, col60, 1
			 A12(k1,k2) = 0.0
	  	end do
      	  end do

          do k1 = 1, row60, 1 ! 60by6
	  	do k2 = 1, col6, 1
			 A21(k1,k2) = 0.0
	  	end do
      	  end do

	! MATRIX [A11] formation - Collector bus
         do k1 = 1, 3, 1
		A11(k1,k1+3) = -1.0
		A11(k1+3,k1) = 1.0
  		do k2 = 1, 3, 1
			A11(k1,k2) = (2*L_main(k1,k2)/h + R_main(k1,k2))
			A11(k1+3,k2+3) = (2*(6*C_main(k1,k2))/h) ! 6 main lines are collected
		end do
	  end do

	 ! MATRIX [A12] formation A12 = A13 = A14 = A15
	  A12(4,1) = -1.0
	  A12(5,2) = -1.0
	  A12(6,3) = -1.0

	 ! MATRIX [A21] formation A21 = A31 = A41 = A51
	  A21(1,4) = 1.0
	  A21(2,5) = 1.0
	  A21(3,6) = 1.0

	! MATRIX [A22] formation - Single feeder A22 = A33 = A44 = A55
         do k1 = 1, 3, 1
		A22(k1+3*0,k1+3) = -1.0
		! 1
		A22(k1+3*1,k1+3*0) = 1.0
		A22(k1+3*1,k1+3*2) = -1.0
		A22(k1+3*1,k1+3*4) = -1.0

		A22(k1+3*2,k1+3*1) = 1.0
		A22(k1+3*2,k1+3*3) = -1.0

		A22(k1+3*3,k1+3*2) = 1.0

		A22(k1+3*4,k1+3*1) = 1.0
		A22(k1+3*4,k1+3*5) = -1.0

		! 2
		A22(k1+3*5,k1+3*4) = 1.0
		A22(k1+3*5,k1+3*6) = -1.0
		A22(k1+3*5,k1+3*8) = -1.0

		A22(k1+3*6,k1+3*5) = 1.0 
		A22(k1+3*6,k1+3*7) = -1.0

		A22(k1+3*7,k1+3*6) = 1.0

		A22(k1+3*8,k1+3*5) = 1.0
		A22(k1+3*8,k1+3*9) = -1.0

		! 3
		A22(k1+3*9,k1+3*8) = 1.0
		A22(k1+3*9,k1+3*10) = -1.0
		A22(k1+3*9,k1+3*12) = -1.0

		A22(k1+3*10,k1+3*9) = 1.0
		A22(k1+3*10,k1+3*11) = -1.0

		A22(k1+3*11,k1+3*10) = 1.0

		A22(k1+3*12,k1+3*9) = 1.0
		A22(k1+3*12,k1+3*13) = -1.0

		! 4
		A22(k1+3*13,k1+3*12) = 1.0
		A22(k1+3*13,k1+3*14) = -1.0
		A22(k1+3*13,k1+3*16) = -1.0

		A22(k1+3*14,k1+3*13) = 1.0
		A22(k1+3*14,k1+3*15) = -1.0

		A22(k1+3*15,k1+3*14) = 1.0

		A22(k1+3*16,k1+3*13) = 1.0
		A22(k1+3*16,k1+3*17) = -1.0

		! 5
		A22(k1+3*17,k1+3*16) = 1.0
		A22(k1+3*17,k1+3*18) = -1.0

		A22(k1+3*18,k1+3*17) = 1.0
		A22(k1+3*18,k1+3*19) = -1.0

		A22(k1+3*19,k1+3*18) = 1.0
  		do k2 = 1, 3, 1
			A22(k1+3*0,k2+3*0) = (2*L_main(k1,k2)/h + R_main(k1,k2))
			! 1
			A22(k1+3*1,k2+3*1) = (2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)
			A22(k1+3*2,k2+3*2) = (2*L_lateral(k1,k2)/h + R_lateral(k1,k2))
			A22(k1+3*3,k2+3*3) = 2*C_lateral(k1,k2)/h
			A22(k1+3*4,k2+3*4) = (2*L_main(k1,k2)/h + R_main(k1,k2))
			! 2
			A22(k1+3*5,k2+3*5) = (2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)
			A22(k1+3*6,k2+3*6) = (2*L_lateral(k1,k2)/h + R_lateral(k1,k2))
			A22(k1+3*7,k2+3*7) = 2*C_lateral(k1,k2)/h
			A22(k1+3*8,k2+3*8) = (2*L_main(k1,k2)/h + R_main(k1,k2))
			! 3
			A22(k1+3*9,k2+3*9) = (2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)
			A22(k1+3*10,k2+3*10) = (2*L_lateral(k1,k2)/h + R_lateral(k1,k2))
			A22(k1+3*11,k2+3*11) = 2*C_lateral(k1,k2)/h
			A22(k1+3*12,k2+3*12) = (2*L_main(k1,k2)/h + R_main(k1,k2))
			! 4
			A22(k1+3*13,k2+3*13) = (2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)
			A22(k1+3*14,k2+3*14) = (2*L_lateral(k1,k2)/h + R_lateral(k1,k2))
			A22(k1+3*15,k2+3*15) = 2*C_lateral(k1,k2)/h
			A22(k1+3*16,k2+3*16) = (2*L_main(k1,k2)/h + R_main(k1,k2))
			! 5
			A22(k1+3*17,k2+3*17) = 2*(C_main(k1,k2) + C_lateral(k1,k2))/h
			A22(k1+3*18,k2+3*18) = (2*L_lateral(k1,k2)/h + R_lateral(k1,k2))
			A22(k1+3*19,k2+3*19) = 2*C_lateral(k1,k2)/h
		end do
	  end do

	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! Inverse A22 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ! Algorithm for matrix inversion by Gauss Jordan
	 ! Augmenting Identity matrix
	 do k1 = 1, row60, 1
	  	do k2 = 1, col60, 1
			If (k1.EQ.k2) then
				A22(k1,k2+row60) = 1
			else
				A22(k1,k2+row60) = 0
			end if
	  	end do
     	 end do

	  ratio = 0
	! Applying Gauss Jordan Elimination
	 do k1 = 1, row60, 1
		do k2 = 1, row60, 1
			if (k1.NE.k2) then
				ratio = A22(k2,k1)/A22(k1,k1)
				do k3 = 1, 2*row60, 1
					A22(k2,k3) = A22(k2,k3) - ratio*A22(k1,k3)
				end do
			end if
		end do
	 end do 

       ! Row Operation to make principal diagonal to 1
	 do k1 = 1, row60, 1
		do k2 = 1+row60, 2*row60, 1
			A22(k1,k2) = A22(k1,k2)/A22(k1,k1)
		end do 
	 end do

	! InvA22 from aug(A22)
        do k1 = 1, row60, 1
		do k2 = 1, col60, 1
			InvA22(k1,k2) = 0 
		end do 
	 end do
        do k1 = 1, row60, 1
		do k2 = 1, col60, 1
			InvA22(k1,k2) = A22(k1,k2+row60)
		end do 
	 end do

       ! A12(6by60)*InvA22(60by60) Multiplication Calculation @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! Matrix initialization 
        do k1 = 1, row6, 1
		do k2 = 1, col60, 1
			A12InvA22(k1,k2) = 0.0
		end do 
	end do
	! Matrix multiplication
	do k1 = 1, row6, 1 ! RowSize of M1
		do k2 = 1, col60, 1  ! ColSize of M2
			do k3 = 1, col60, 1 ! ColSize of M1
				A12InvA22(k1,k2) = A12InvA22(k1,k2) + A12(k1,k3)*InvA22(k3,k2)
			end do
		end do 
	end do

	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! A12*InvA22(6by60)*A21(60by6) Calculation @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! Matrix initialization
        do k1 = 1, row6, 1
		do k2 = 1, col6, 1
			A12InvA22A21(k1,k2) = 0.0
		end do 
	end do
	! Matrix multiplication
	do k1 = 1, row6, 1 ! RowSize of M1
		do k2 = 1, col6, 1  ! ColSize of M2
			do k3 = 1, col60, 1 ! ColSize of M1
				A12InvA22A21(k1,k2) = A12InvA22A21(k1,k2) + A12InvA22(k1,k3)*A21(k3,k2)
			end do
		end do 
	end do

	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ! Vector b1 b2 b3 b4 b5 b6 update 
	! Vector [b1] Update by x_in
         do k1 = 1, 3, 1
		k9(k1) = 0.0 ! I10_abc Collector		
		k10(k1) = 0.0 ! V1_abc Collector
         	do k2 = 1, 3, 1
			k9(k1) = k9(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(k2))
			k10(k1) = k10(k1) + ((2*(6*C_main(k1,k2))/h)*$x_in(k2+3))
      	  	end do
		b1(k1) = k9(k1) + $x_in(k1+3) - 2*$vgrid_in(k1)
		b1(k1+3) = k10(k1) - $x_in(k1) + $x_in(k1+6) + $x_in(k1+66) + $x_in(k1+126) + $x_in(k1+186) + $x_in(k1+246)
      	  end do

       ! Vector [b2] Update by x_in
         do k1 = 1, 3, 1
		k11(k1) = 0.0 ! I21_abc
		k13(k1) = 0.0 ! I32_abc
		k15(k1) = 0.0 ! I42_abc
		k17(k1) = 0.0 ! I54_abc
		k19(k1) = 0.0 ! I64_abc
		k21(k1) = 0.0 ! I76_abc
		k23(k1) = 0.0 ! I86_abc
		k25(k1) = 0.0 ! I98_abc
		k27(k1) = 0.0 ! I10_8_abc
		k29(k1) = 0.0 ! I11_10_abc

		k12(k1) = 0.0 ! V2_abc
		k14(k1) = 0.0 ! V3_abc
		k16(k1) = 0.0 ! V4_abc
		k18(k1) = 0.0 ! V5_abc
		k20(k1) = 0.0 ! V6_abc
		k22(k1) = 0.0 ! V7_abc
		k24(k1) = 0.0 ! V8_abc
		k26(k1) = 0.0 ! V9_abc
		k28(k1) = 0.0 ! V10_abc
		k30(k1) = 0.0 ! V11_abc
         	do k2 = 1, 3, 1
			! feeder 1
			k11(k1) = k11(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(6+k2))
			k13(k1) = k13(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(12+k2))
			k15(k1) = k15(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(18+k2))
			k17(k1) = k17(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(24+k2))
			k19(k1) = k19(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(30+k2))
			k21(k1) = k21(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(36+k2))
			k23(k1) = k23(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(42+k2))
			k25(k1) = k25(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(48+k2))
			k27(k1) = k27(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(54+k2))
			k29(k1) = k29(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(60+k2))

			k12(k1) = k12(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(9+k2))
			k14(k1) = k14(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(15+k2))
			k16(k1) = k16(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(21+k2))
			k18(k1) = k18(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(27+k2))
			k20(k1) = k20(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(33+k2))
			k22(k1) = k22(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(39+k2))
			k24(k1) = k24(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(45+k2))
			k26(k1) = k26(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(51+k2))
			k28(k1) = k28(k1) + ((2*(C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(57+k2))
			k30(k1) = k30(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(63+k2))
      	  	end do
		! feeder 1
		b2(k1+3*0) = k11(k1) + $x_in(9+k1) - $x_in(3+k1)    ! v2 - v1
		b2(k1+3*2) = k13(k1) + $x_in(15+k1) - $x_in(9+k1)   ! v3 - v2
		b2(k1+3*4) = k15(k1) + $x_in(21+k1) - $x_in(9+k1)   ! v4 - v2
		b2(k1+3*6) = k17(k1) + $x_in(27+k1) - $x_in(21+k1)  ! v5 - v4
		b2(k1+3*8) = k19(k1) + $x_in(33+k1) - $x_in(21+k1)  ! v6 - v4 
		b2(k1+3*10) = k21(k1) + $x_in(39+k1) - $x_in(33+k1) ! v7 - v6
		b2(k1+3*12) = k23(k1) + $x_in(45+k1) - $x_in(33+k1) ! v8 - v6
		b2(k1+3*14) = k25(k1) + $x_in(51+k1) - $x_in(45+k1) ! v9 - v8
		b2(k1+3*16) = k27(k1) + $x_in(57+k1) - $x_in(45+k1) ! v10 - v8
		b2(k1+3*18) = k29(k1) + $x_in(63+k1) - $x_in(57+k1) ! v11 - v10

		b2(k1+3*1) = k12(k1) - $x_in(6+k1) + $x_in(12+k1) + $x_in(18+k1)   ! -i21 + i32 + i42
		b2(k1+3*3) = k14(k1) - $x_in(12+k1) + 2*$iload_in(k1)              ! -i32 + 2*iload3
		b2(k1+3*5) = k16(k1) - $x_in(18+k1) + $x_in(24+k1) + $x_in(30+k1)  ! -42 + i54 + i64
		b2(k1+3*7) = k18(k1) - $x_in(24+k1) + 2*$iload_in(3+k1)            ! -i54 + 2*iload5
		b2(k1+3*9) = k20(k1) - $x_in(30+k1) + $x_in(36+k1) + $x_in(42+k1)
		b2(k1+3*11) = k22(k1) - $x_in(36+k1) + 2*$iload_in(6+k1)
		b2(k1+3*13) = k24(k1) - $x_in(42+k1) + $x_in(48+k1) + $x_in(54+k1)
		b2(k1+3*15) = k26(k1) - $x_in(48+k1) + 2*$iload_in(9+k1)
		b2(k1+3*17) = k28(k1) - $x_in(54+k1) + $x_in(60+k1)
		b2(k1+3*19) = k30(k1) - $x_in(60+k1) + 2*$iload_in(12+k1)
      	  end do

       ! Vector [b3] Update by x_in
         do k1 = 1, 3, 1
		k11(k1) = 0.0 ! I21_abc
		k13(k1) = 0.0 ! I32_abc
		k15(k1) = 0.0 ! I42_abc
		k17(k1) = 0.0
		k19(k1) = 0.0
		k21(k1) = 0.0
		k23(k1) = 0.0
		k25(k1) = 0.0
		k27(k1) = 0.0
		k29(k1) = 0.0 ! I11_10_abc

		k12(k1) = 0.0 ! V2_abc
		k14(k1) = 0.0 ! V3_abc
		k16(k1) = 0.0 ! V4_abc
		k18(k1) = 0.0
		k20(k1) = 0.0
		k22(k1) = 0.0
		k24(k1) = 0.0
		k26(k1) = 0.0
		k28(k1) = 0.0
		k30(k1) = 0.0 ! V11_abc
         	do k2 = 1, 3, 1
			! feeder 2
			k11(k1) = k11(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(66+k2))
			k13(k1) = k13(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(72+k2))
			k15(k1) = k15(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(78+k2))
			k17(k1) = k17(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(84+k2))
			k19(k1) = k19(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(90+k2))
			k21(k1) = k21(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(96+k2))
			k23(k1) = k23(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(102+k2))
			k25(k1) = k25(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(108+k2))
			k27(k1) = k27(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(114+k2))
			k29(k1) = k29(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(120+k2))

			k12(k1) = k12(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(69+k2))
			k14(k1) = k14(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(75+k2))
			k16(k1) = k16(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(81+k2))
			k18(k1) = k18(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(87+k2))
			k20(k1) = k20(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(93+k2))
			k22(k1) = k22(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(99+k2))
			k24(k1) = k24(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(105+k2))
			k26(k1) = k26(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(111+k2))
			k28(k1) = k28(k1) + ((2*(C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(117+k2))
			k30(k1) = k30(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(123+k2))
      	  	end do
		! feeder 2
		b3(k1+3*0) = k11(k1) + $x_in(69+k1) - $x_in(3+k1) ! v12 - v1
		b3(k1+3*2) = k13(k1) + $x_in(75+k1) - $x_in(69+k1) ! v13 - v12
		b3(k1+3*4) = k15(k1) + $x_in(81+k1) - $x_in(69+k1) ! v14 - v12
		b3(k1+3*6) = k17(k1) + $x_in(87+k1) - $x_in(81+k1) ! v15 - v14
		b3(k1+3*8) = k19(k1) + $x_in(93+k1) - $x_in(81+k1) ! v16 - v14
		b3(k1+3*10) = k21(k1) + $x_in(99+k1) - $x_in(93+k1) ! v17 - v16
		b3(k1+3*12) = k23(k1) + $x_in(105+k1) - $x_in(93+k1) ! v18 - v16
		b3(k1+3*14) = k25(k1) + $x_in(111+k1) - $x_in(105+k1) ! v19 - v18
		b3(k1+3*16) = k27(k1) + $x_in(117+k1) - $x_in(105+k1) ! v20 - v18
		b3(k1+3*18) = k29(k1) + $x_in(123+k1) - $x_in(117+k1) ! v21 - v20

		b3(k1+3*1) = k12(k1) - $x_in(66+k1) + $x_in(72+k1) + $x_in(78+k1) ! -121 + i1312 + i1412
		b3(k1+3*3) = k14(k1) - $x_in(72+k1) + 2*$iload_in(15+k1)
		b3(k1+3*5) = k16(k1) - $x_in(78+k1) + $x_in(84+k1) + $x_in(90+k1)
		b3(k1+3*7) = k18(k1) - $x_in(84+k1) + 2*$iload_in(18+k1)
		b3(k1+3*9) = k20(k1) - $x_in(90+k1) + $x_in(96+k1) + $x_in(102+k1)
		b3(k1+3*11) = k22(k1) - $x_in(96+k1) + 2*$iload_in(21+k1)
		b3(k1+3*13) = k24(k1) - $x_in(102+k1) + $x_in(108+k1) + $x_in(114+k1)
		b3(k1+3*15) = k26(k1) - $x_in(108+k1) + 2*$iload_in(24+k1)
		b3(k1+3*17) = k28(k1) - $x_in(114+k1) + $x_in(120+k1)
		b3(k1+3*19) = k30(k1) - $x_in(120+k1) + 2*$iload_in(27+k1)
      	  end do

       ! Vector [b4] Update by x_in
         do k1 = 1, 3, 1
		k11(k1) = 0.0 ! I21_abc
		k13(k1) = 0.0 ! I32_abc
		k15(k1) = 0.0 ! I42_abc
		k17(k1) = 0.0
		k19(k1) = 0.0
		k21(k1) = 0.0
		k23(k1) = 0.0
		k25(k1) = 0.0
		k27(k1) = 0.0
		k29(k1) = 0.0 ! I11_10_abc

		k12(k1) = 0.0 ! V2_abc
		k14(k1) = 0.0 ! V3_abc
		k16(k1) = 0.0 ! V4_abc
		k18(k1) = 0.0
		k20(k1) = 0.0
		k22(k1) = 0.0
		k24(k1) = 0.0
		k26(k1) = 0.0
		k28(k1) = 0.0
		k30(k1) = 0.0 ! V11_abc
         	do k2 = 1, 3, 1
			! feeder 3
			k11(k1) = k11(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(126+k2))
			k13(k1) = k13(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(132+k2))
			k15(k1) = k15(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(138+k2))
			k17(k1) = k17(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(144+k2))
			k19(k1) = k19(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(150+k2))
			k21(k1) = k21(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(156+k2))
			k23(k1) = k23(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(162+k2))
			k25(k1) = k25(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(168+k2))
			k27(k1) = k27(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(174+k2))
			k29(k1) = k29(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(180+k2))

			k12(k1) = k12(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(129+k2))
			k14(k1) = k14(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(135+k2))
			k16(k1) = k16(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(141+k2))
			k18(k1) = k18(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(147+k2))
			k20(k1) = k20(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(153+k2))
			k22(k1) = k22(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(159+k2))
			k24(k1) = k24(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(165+k2))
			k26(k1) = k26(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(171+k2))
			k28(k1) = k28(k1) + ((2*(C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(177+k2))
			k30(k1) = k30(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(183+k2))
      	  	end do
		! feeder 3
		b4(k1+3*0) = k11(k1) + $x_in(129+k1) - $x_in(3+k1) ! v22 - v1
		b4(k1+3*2) = k13(k1) + $x_in(135+k1) - $x_in(129+k1) ! v23 - v22
		b4(k1+3*4) = k15(k1) + $x_in(141+k1) - $x_in(129+k1) ! v24 - v22
		b4(k1+3*6) = k17(k1) + $x_in(147+k1) - $x_in(141+k1) ! v25 - v24
		b4(k1+3*8) = k19(k1) + $x_in(153+k1) - $x_in(141+k1) ! v26 - v24
		b4(k1+3*10) = k21(k1) + $x_in(159+k1) - $x_in(153+k1) ! v27 - v26
		b4(k1+3*12) = k23(k1) + $x_in(165+k1) - $x_in(153+k1) ! v28 - v26
		b4(k1+3*14) = k25(k1) + $x_in(171+k1) - $x_in(165+k1) ! v29 - v28
		b4(k1+3*16) = k27(k1) + $x_in(177+k1) - $x_in(165+k1) ! v30 - v28
		b4(k1+3*18) = k29(k1) + $x_in(183+k1) - $x_in(177+k1) ! v31 - v30

		b4(k1+3*1) = k12(k1) - $x_in(126+k1) + $x_in(132+k1) + $x_in(138+k1)
		b4(k1+3*3) = k14(k1) - $x_in(132+k1) + 2*$iload_in(30+k1)
		b4(k1+3*5) = k16(k1) - $x_in(138+k1) + $x_in(144+k1) + $x_in(150+k1)
		b4(k1+3*7) = k18(k1) - $x_in(144+k1) + 2*$iload_in(33+k1)
		b4(k1+3*9) = k20(k1) - $x_in(150+k1) + $x_in(156+k1) + $x_in(162+k1)
		b4(k1+3*11) = k22(k1) - $x_in(156+k1) + 2*$iload_in(36+k1)
		b4(k1+3*13) = k24(k1) - $x_in(162+k1) + $x_in(168+k1) + $x_in(174+k1)
		b4(k1+3*15) = k26(k1) - $x_in(168+k1) + 2*$iload_in(39+k1)
		b4(k1+3*17) = k28(k1) - $x_in(174+k1) + $x_in(180+k1)
		b4(k1+3*19) = k30(k1) - $x_in(180+k1) + 2*$iload_in(42+k1)
      	  end do

       ! Vector [b5] Update by x_in
         do k1 = 1, 3, 1
		k11(k1) = 0.0 ! I21_abc
		k13(k1) = 0.0 ! I32_abc
		k15(k1) = 0.0 ! I42_abc
		k17(k1) = 0.0
		k19(k1) = 0.0
		k21(k1) = 0.0
		k23(k1) = 0.0
		k25(k1) = 0.0
		k27(k1) = 0.0
		k29(k1) = 0.0 ! I11_10_abc

		k12(k1) = 0.0 ! V2_abc
		k14(k1) = 0.0 ! V3_abc
		k16(k1) = 0.0 ! V4_abc
		k18(k1) = 0.0
		k20(k1) = 0.0
		k22(k1) = 0.0
		k24(k1) = 0.0
		k26(k1) = 0.0
		k28(k1) = 0.0
		k30(k1) = 0.0 ! V11_abc
         	do k2 = 1, 3, 1
			! feeder 4
			k11(k1) = k11(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(186+k2))
			k13(k1) = k13(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(192+k2))
			k15(k1) = k15(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(198+k2))
			k17(k1) = k17(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(204+k2))
			k19(k1) = k19(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(210+k2))
			k21(k1) = k21(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(216+k2))
			k23(k1) = k23(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(222+k2))
			k25(k1) = k25(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(228+k2))
			k27(k1) = k27(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(234+k2))
			k29(k1) = k29(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(240+k2))

			k12(k1) = k12(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(189+k2))
			k14(k1) = k14(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(195+k2))
			k16(k1) = k16(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(201+k2))
			k18(k1) = k18(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(207+k2))
			k20(k1) = k20(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(213+k2))
			k22(k1) = k22(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(219+k2))
			k24(k1) = k24(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(225+k2))
			k26(k1) = k26(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(231+k2))
			k28(k1) = k28(k1) + ((2*(C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(237+k2))
			k30(k1) = k30(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(243+k2))
      	  	end do
		! feeder 4
		b5(k1+3*0) = k11(k1) + $x_in(189+k1) - $x_in(3+k1) ! v32 - v1
		b5(k1+3*2) = k13(k1) + $x_in(195+k1) - $x_in(189+k1) ! v33 - v32
		b5(k1+3*4) = k15(k1) + $x_in(201+k1) - $x_in(189+k1) ! v34 - v32
		b5(k1+3*6) = k17(k1) + $x_in(207+k1) - $x_in(201+k1) ! v35 - v34
		b5(k1+3*8) = k19(k1) + $x_in(213+k1) - $x_in(201+k1) ! v36 - v34
		b5(k1+3*10) = k21(k1) + $x_in(219+k1) - $x_in(213+k1) ! v37 - v36
		b5(k1+3*12) = k23(k1) + $x_in(225+k1) - $x_in(213+k1) ! v38 - v36
		b5(k1+3*14) = k25(k1) + $x_in(231+k1) - $x_in(225+k1) ! v39 - v38
		b5(k1+3*16) = k27(k1) + $x_in(237+k1) - $x_in(225+k1) ! v40 - v38
		b5(k1+3*18) = k29(k1) + $x_in(243+k1) - $x_in(237+k1) ! v41 - v40

		b5(k1+3*1) = k12(k1) - $x_in(186+k1) + $x_in(192+k1) + $x_in(198+k1)
		b5(k1+3*3) = k14(k1) - $x_in(192+k1) + 2*$iload_in(45+k1)
		b5(k1+3*5) = k16(k1) - $x_in(198+k1) + $x_in(204+k1) + $x_in(210+k1)
		b5(k1+3*7) = k18(k1) - $x_in(204+k1) + 2*$iload_in(48+k1)
		b5(k1+3*9) = k20(k1) - $x_in(210+k1) + $x_in(216+k1) + $x_in(222+k1)
		b5(k1+3*11) = k22(k1) - $x_in(216+k1) + 2*$iload_in(51+k1)
		b5(k1+3*13) = k24(k1) - $x_in(222+k1) + $x_in(228+k1) + $x_in(234+k1)
		b5(k1+3*15) = k26(k1) - $x_in(228+k1) + 2*$iload_in(54+k1)
		b5(k1+3*17) = k28(k1) - $x_in(234+k1) + $x_in(240+k1)
		b5(k1+3*19) = k30(k1) - $x_in(240+k1) + 2*$iload_in(57+k1)
      	  end do

      ! Vector [b6] Update by x_in
         do k1 = 1, 3, 1
		k11(k1) = 0.0 ! I21_abc
		k13(k1) = 0.0 ! I32_abc
		k15(k1) = 0.0 ! I42_abc
		k17(k1) = 0.0
		k19(k1) = 0.0
		k21(k1) = 0.0
		k23(k1) = 0.0
		k25(k1) = 0.0
		k27(k1) = 0.0
		k29(k1) = 0.0 ! I11_10_abc

		k12(k1) = 0.0 ! V2_abc
		k14(k1) = 0.0 ! V3_abc
		k16(k1) = 0.0 ! V4_abc
		k18(k1) = 0.0
		k20(k1) = 0.0
		k22(k1) = 0.0
		k24(k1) = 0.0
		k26(k1) = 0.0
		k28(k1) = 0.0
		k30(k1) = 0.0 ! V11_abc
         	do k2 = 1, 3, 1
			! feeder 5
			k11(k1) = k11(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(246+k2))
			k13(k1) = k13(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(252+k2))
			k15(k1) = k15(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(258+k2))
			k17(k1) = k17(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(264+k2))
			k19(k1) = k19(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(270+k2))
			k21(k1) = k21(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(276+k2))
			k23(k1) = k23(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(282+k2))
			k25(k1) = k25(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(288+k2))
			k27(k1) = k27(k1) + ((2*L_main(k1,k2)/h - R_main(k1,k2))*$x_in(294+k2))
			k29(k1) = k29(k1) + ((2*L_lateral(k1,k2)/h - R_lateral(k1,k2))*$x_in(300+k2))

			k12(k1) = k12(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(249+k2))
			k14(k1) = k14(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(255+k2))
			k16(k1) = k16(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(261+k2))
			k18(k1) = k18(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(267+k2))
			k20(k1) = k20(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(273+k2))
			k22(k1) = k22(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(279+k2))
			k24(k1) = k24(k1) + ((2*(2*C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(285+k2))
			k26(k1) = k26(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(291+k2))
			k28(k1) = k28(k1) + ((2*(C_main(k1,k2) + C_lateral(k1,k2))/h)*$x_in(297+k2))
			k30(k1) = k30(k1) + ((2*C_lateral(k1,k2)/h)*$x_in(303+k2))
      	  	end do
		! feeder 5
		b6(k1+3*0) = k11(k1) + $x_in(249+k1) - $x_in(3+k1) ! v42 - v1
		b6(k1+3*2) = k13(k1) + $x_in(255+k1) - $x_in(249+k1) ! v43 - v42
		b6(k1+3*4) = k15(k1) + $x_in(261+k1) - $x_in(249+k1) ! v44 - v42
		b6(k1+3*6) = k17(k1) + $x_in(267+k1) - $x_in(261+k1) ! v45 - v44
		b6(k1+3*8) = k19(k1) + $x_in(273+k1) - $x_in(261+k1) ! v46 - v44
		b6(k1+3*10) = k21(k1) + $x_in(279+k1) - $x_in(273+k1) ! v47 - v46
		b6(k1+3*12) = k23(k1) + $x_in(285+k1) - $x_in(273+k1) ! v48 - v46
		b6(k1+3*14) = k25(k1) + $x_in(291+k1) - $x_in(285+k1) ! v49 - v48
		b6(k1+3*16) = k27(k1) + $x_in(297+k1) - $x_in(285+k1) ! v50 - v48
		b6(k1+3*18) = k29(k1) + $x_in(303+k1) - $x_in(297+k1) ! v51 - v50

		b6(k1+3*1) = k12(k1) - $x_in(246+k1) + $x_in(252+k1) + $x_in(258+k1)
		b6(k1+3*3) = k14(k1) - $x_in(252+k1) + 2*$iload_in(60+k1)
		b6(k1+3*5) = k16(k1) - $x_in(258+k1) + $x_in(264+k1) + $x_in(270+k1)
		b6(k1+3*7) = k18(k1) - $x_in(264+k1) + 2*$iload_in(63+k1)
		b6(k1+3*9) = k20(k1) - $x_in(270+k1) + $x_in(276+k1) + $x_in(282+k1)
		b6(k1+3*11) = k22(k1) - $x_in(276+k1) + 2*$iload_in(66+k1)
		b6(k1+3*13) = k24(k1) - $x_in(282+k1) + $x_in(288+k1) + $x_in(294+k1)
		b6(k1+3*15) = k26(k1) - $x_in(288+k1) + 2*$iload_in(69+k1)
		b6(k1+3*17) = k28(k1) - $x_in(294+k1) + $x_in(300+k1)
		b6(k1+3*19) = k30(k1) - $x_in(300+k1) + 2*$iload_in(72+k1)
      	  end do

	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 ! Calculation - b1(6by1) - A12InvA22*b2 - A12InvA22*b3 - A12InvA22*b4 - A12InvA22*b5 - A12InvA22*b6
	! A12InvA22(6by60)*bn(60by1) multiplication
	  do k1 = 1, row6, 1 ! RowSize of M1
		TempB(k1) = 0		
		k11(k1) = 0
		k12(k1) = 0
		k13(k1) = 0
		k14(k1) = 0
		k15(k1) = 0
		do k3 = 1, col60, 1 ! ColSize of M1
		       k11(k1) = k11(k1) + A12InvA22(k1,k3)*b2(k3)
                      k12(k1) = k12(k1) + A12InvA22(k1,k3)*b3(k3)
                      k13(k1) = k13(k1) + A12InvA22(k1,k3)*b4(k3)
                      k14(k1) = k14(k1) + A12InvA22(k1,k3)*b5(k3)
                      k15(k1) = k15(k1) + A12InvA22(k1,k3)*b6(k3)
		end do
		TempB(k1) = b1(k1) - k11(k1) - k12(k1) - k13(k1) - k14(k1) - k15(k1)
	  end do

	 ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 ! Calculation - A11(6by6) - A12InvA22*A21 - A12InvA22*A31 - A12InvA22*A41 - A12InvA22*A51 - A12InvA22*A61 (A11 - 5*A12InvA22*A21)
	  do k1 = 1,row6,1
  		do k2 = 1,col6,1
			TempA1(k1,k2) = 0
		end do
	  end do

	  do k1 = 1,row6,1
  		do k2 = 1,col6,1
			TempA1(k1,k2) = A11(k1,k2) - A12InvA22A21(k1,k2)*5
		end do
	  end do

        ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 ! x1 = A-1*b -> x2, x3, x4, x5, x6 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 ! TempA-1*TempB -> Tempx1  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ! LU Decomposition / TempA=LU, Ax=TempB, LUx=TempB, Ly=TempB, Ux=y
	 ! LU initialization
         do k1 = 1, row6, 1
         	do k2 = 1, col6, 1
         		Al(k1,k2) = 0.0
         		Au(k1,k2) = 0.0
		end do
         	Al(k1,k1) = 1.0
         end do
        ! LU decomposition
         do k1 = 1, row6, 1
         	k2 = k1
		! Au update
	       do while(k2.LE.row6)
         		Au(k1,k2) = TempA1(k1,k2)
         		k3 = 1
         		do while(k3.LE.(k1-1))
         			Au(k1,k2) = Au(k1,k2) - Al(k1,k3)*Au(k3,k2)
         			k3 = k3+1
         		end do
         		k2 = k2+1
         	end do
         	k2 = k1+1
         	do while(k2.LE.row6)
         		Al(k2,k1) = TempA1(k2,k1)
	         	do k3 = 1, k1-1, 1
	         		Al(k2,k1) = Al(k2,k1) - Al(k2,k3)*Au(k3,k1)
	         	end do
	         	Al(k2,k1) = Al(k2,k1)/Au(k1,k1)
	       		k2 = k2+1
	       end do
         end do

	! y update by forward substitution Ly=b
         do k1 = 1, row6, 1
         	y(k1) = TempB(k1)
         	do k3 = 1, k1-1, 1
         		y(k1) = y(k1) - Al(k1,k3)*y(k3)
         	end do
         end do

	! x_out update by back substitution Ux=y
         k1 = row6
         do while (k1.GE.1)
         	Tempx1(k1) = y(k1)
         	if(k1.NE.row6) then
         		k3 = k1+1
         		do while(k3.LE.row6)
         			Tempx1(k1) = Tempx1(k1) - Au(k1,k3)*Tempx1(k3)
         			k3 = k3+1
         		end do
         	endif 
         	Tempx1(k1) = Tempx1(k1)/Au(k1,k1)
         	k1 = k1-1
         end do

	  ! xn(60by1) = A21(60by6)*x1(6by1) -> x2, x3, x4, x5, x6	
	  do k1 = 1, row60, 1 ! RowSize of M1
		k11(k1) = 0.0
		k12(k1) = 0.0
		k13(k1) = 0.0
		k14(k1) = 0.0
		k15(k1) = 0.0
		do k3 = 1, col6, 1 ! ColSize of M1
			k11(k1) = k11(k1) + A21(k1,k3)*Tempx1(k3)
                      k12(k1) = k12(k1) + A21(k1,k3)*Tempx1(k3)
                      k13(k1) = k13(k1) + A21(k1,k3)*Tempx1(k3)
                      k14(k1) = k14(k1) + A21(k1,k3)*Tempx1(k3)
                      k15(k1) = k15(k1) + A21(k1,k3)*Tempx1(k3)
		end do
		Tempb2(k1) = b2(k1) - k11(k1)
		Tempb3(k1) = b3(k1) - k12(k1)
		Tempb4(k1) = b4(k1) - k13(k1)
		Tempb5(k1) = b5(k1) - k14(k1)
		Tempb6(k1) = b6(k1) - k15(k1)
	  end do

	  do k1 = 1, row60, 1 ! RowSize of M1
		k11(k1) = 0.0
		k12(k1) = 0.0
		k13(k1) = 0.0
		k14(k1) = 0.0
		k15(k1) = 0.0
		do k3 = 1, row60, 1 ! ColSize of M1
			k11(k1) = k11(k1) + InvA22(k1,k3)*Tempb2(k3)
                       k12(k1) = k12(k1) + InvA22(k1,k3)*Tempb3(k3)
                       k13(k1) = k13(k1) + InvA22(k1,k3)*Tempb4(k3)
                       k14(k1) = k14(k1) + InvA22(k1,k3)*Tempb5(k3)
                       k15(k1) = k15(k1) + InvA22(k1,k3)*Tempb6(k3)
		end do
		Tempx2(k1) = k11(k1)
		Tempx3(k1) = k12(k1)
		Tempx4(k1) = k13(k1)
		Tempx5(k1) = k14(k1)
		Tempx6(k1) = k15(k1)
	  end do

        ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	 ! X_out update
	 ! x1, x2, x3, x4, x5, x6
	  do k1 = 1, row6, 1 ! RowSize of A11
		$x_out(k1) = Tempx1(k1)
	  end do
	  do k1 = 1, row60, 1 ! RowSize of Ann
		$x_out(k1+6+60*0) = Tempx2(k1)
		$x_out(k1+6+60*1) = Tempx3(k1)
		$x_out(k1+6+60*2) = Tempx4(k1)
		$x_out(k1+6+60*3) = Tempx5(k1)
		$x_out(k1+6+60*4) = Tempx6(k1)
	  end do

	! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	! vload(153) update by vabc(153) in x_out(306)
	do k1 = 1,NL,1
		 $vload_out((k1-1)*3+1) = $x_out((k1-1)*6+4) ! va 4 16 28 40 
		 $vload_out((k1-1)*3+2) = $x_out((k1-1)*6+5) ! vb 5 17 29 41
		 $vload_out((k1-1)*3+3) = $x_out((k1-1)*6+6) ! vc 6 18 30 42
	end do

	! Reactive power measurement
	  $Q_POC = (($vgrid_in(2) - $vgrid_in(3))*$x_out(1) + ($vgrid_in(3) - $vgrid_in(1))*$x_out(2) + ($vgrid_in(1) - $vgrid_in(2))*$x_out(3))/sqrt(3.0)

	! Active power measurement
	  $P_POC = (($vgrid_in(1))*$x_out(1) + ($vgrid_in(2))*$x_out(2) + ($vgrid_in(3))*$x_out(3))

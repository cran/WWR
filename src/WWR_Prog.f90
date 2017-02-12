

SUBROUTINE LOGRANK2(N,Y2,D2,Z,WTY,STAT,VS)
!DEC$ ATTRIBUTES DLLEXPORT,C,REFERENCE,ALIAS:'logrank2_' ::LOGRANK2
   IMPLICIT NONE
   INTEGER,INTENT(IN)::N,WTY
   INTEGER,INTENT(IN)::Y2(N),D2(N),Z(N)
   REAL(8),INTENT(OUT)::STAT,VS

   INTEGER::I
   REAL(8)::R2(N),R21(N),R20(N)

   DO I=1,N,1
      R2(I)=REAL(N-Y2(I)+1,8)
	    R21(I)=REAL(COUNT(Z(Y2(I):N)==1),8)
      R20(I)=REAL(COUNT(Z(Y2(I):N)==0),8)
   END DO

   ! WTY=1 GEHAN; WTY=2 LOGRANK
   IF (WTY==1) THEN
      STAT=SUM(REAL(Z,8)*R2-R21,D2==1)/REAL(N,8)
      VS=SUM(R21*R20/REAL(N,8)/REAL(N,8),D2==1)/REAL(N,8)
   ELSE IF (WTY==2) THEN
      STAT=SUM(REAL(Z,8)-R21/R2,D2==1)
      VS=SUM(R21*R20/R2**2,D2==1)/REAL(N,8)
   END IF

   RETURN
END SUBROUTINE LOGRANK2


SUBROUTINE WWRNULLB(N,Y1,Y2,D1,D2,Z,WTY1,WTY2,WSTAT,VSTAT,LW2,LL2,LW1,LL1)
!DEC$ ATTRIBUTES DLLEXPORT,C,REFERENCE,ALIAS:'wwrnullb_' ::WWRNULLB
   IMPLICIT NONE
   INTEGER,INTENT(IN)::N
   INTEGER,INTENT(IN)::Y1(N),Y2(N),D1(N),D2(N),Z(N),WTY1,WTY2
   REAL(8),INTENT(OUT)::WSTAT(3),VSTAT(3)
   REAL(8),INTENT(OUT)::LW2,LW1,LL2,LL1

   INTEGER::I,J
   INTEGER::N2(N),ITEMP,N1,TI
   REAL(8)::R1(N),RTEMP,R2(N),R3(N),R21(N),R20(N)
   REAL(8)::W2,W1,L2,L1,VW2(N),VL2(N),VW1(N),VL1(N),P1,P2

   W2=0.0;L2=0.0;W1=0.0;L1=0.0
   VW2=0.0;VL2=0.0;VW1=0.0;VL1=0.0

   N1=COUNT(Z==1)

   DO I=1,N,1
      N2(I)=COUNT(Y2<=Y2(I))
      R1(I)=REAL(COUNT(Y1(Y2(I):N)>=Y1(I)),8)/REAL(N,8)
      R2(I)=REAL(N-Y2(I)+1,8)/REAL(N,8)
      R3(I)=REAL(COUNT(Y1>=Y1(I)),8)/REAL(N,8)
      R21(I)=REAL(COUNT(Z(Y2(I):N)==1),8)/REAL(N,8)
      R20(I)=REAL(COUNT(Z(Y2(I):N)==0),8)/REAL(N,8)
   END DO

   SELECT CASE (WTY2)
   CASE (1)
      W2=SUM(R21,Z==0 .AND. D2==1)*REAL(N,8)
      L2=SUM(R20,Z==1 .AND. D2==1)*REAL(N,8)
      DO I=1,N,1
         DO J=1,N2(I),1
            VW2(I)=VW2(I)+REAL((Z(I)-Z(J))*D2(J),8)
            VL2(J)=VL2(J)+REAL((Z(I)-Z(J))*D2(J),8)
         END DO
      END DO
   CASE (2)
      W2=SUM(R21/R2,Z==0 .AND. D2==1)*REAL(N,8)
      L2=SUM(R20/R2,Z==1 .AND. D2==1)*REAL(N,8)
      DO I=1,N,1
         DO J=1,N2(I),1
            VW2(I)=VW2(I)+REAL((Z(I)-Z(J))*D2(J),8)/R2(J)
            VL2(J)=VL2(J)+REAL((Z(I)-Z(J))*D2(J),8)/R2(J)
         END DO
      END DO
   CASE DEFAULT
      W2=SUM(R21/R2,Z==0 .AND. D2==1)*REAL(N,8)
      L2=SUM(R20/R2,Z==1 .AND. D2==1)*REAL(N,8)
      DO I=1,N,1
         DO J=1,N2(I),1
            VW2(I)=VW2(I)+REAL((Z(I)-Z(J))*D2(J),8)/R2(J)
            VL2(J)=VL2(J)+REAL((Z(I)-Z(J))*D2(J),8)/R2(J)
         END DO
      END DO
   END SELECT

   SELECT CASE (WTY1)
   CASE (1)
      DO I=1,N,1
         IF (Y2(I)>1) THEN
            TI=Y2(I)-1
            DO J=1,TI,1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)
               END IF
            END DO
         END IF

         IF (D2(I)==0 .AND. N2(I)<N) THEN
            TI=N2(I)+1
            DO J=TI,N,1
               IF (D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)
               END IF
            END DO
         END IF

         IF (D2(I)==0) THEN
            DO J=Y2(I),N2(I),1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)
               END IF
            END DO
         END IF
      END DO
   CASE (2)
      DO I=1,N,1
         IF (Y2(I)>1) THEN
            TI=Y2(I)-1
            DO J=1,TI,1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R1(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R1(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R1(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R1(J)
               END IF
            END DO
         END IF

         IF (D2(I)==0 .AND. N2(I)<N) THEN
            TI=N2(I)+1
            DO J=TI,N,1
               IF (D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   ITEMP=COUNT(Y1(Y2(I):N)>=Y1(J))
				   RTEMP=REAL(N,8)/REAL(ITEMP,8)
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)*RTEMP
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)*RTEMP
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)*RTEMP
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)*RTEMP
               END IF
            END DO
         END IF

         IF (D2(I)==0) THEN
            DO J=Y2(I),N2(I),1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R1(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R1(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R1(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R1(J)
               END IF
            END DO
         END IF
      END DO
   CASE (3)
      DO I=1,N,1
         IF (Y2(I)>1) THEN
            TI=Y2(I)-1
            DO J=1,TI,1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R2(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R2(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R2(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R2(J)
               END IF
            END DO
         END IF

         IF (D2(I)==0 .AND. N2(I)<N) THEN
            TI=N2(I)+1
            DO J=TI,N,1
               IF (D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R2(I)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R2(I)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R2(I)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R2(I)
               END IF
            END DO
         END IF

         IF (D2(I)==0) THEN
            DO J=Y2(I),N2(I),1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R2(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R2(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R2(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R2(J)
               END IF
            END DO
         END IF
      END DO

   CASE (4)
      DO I=1,N,1
         IF (Y2(I)>1) THEN
            TI=Y2(I)-1
            DO J=1,TI,1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R3(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R3(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R3(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R3(J)
               END IF
            END DO
         END IF

         IF (D2(I)==0 .AND. N2(I)<N) THEN
            TI=N2(I)+1
            DO J=TI,N,1
               IF (D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R3(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R3(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R3(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R3(J)
               END IF
            END DO
         END IF

         IF (D2(I)==0) THEN
            DO J=Y2(I),N2(I),1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)/R3(J)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)/R3(J)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)/R3(J)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)/R3(J)
               END IF
            END DO
         END IF
      END DO

   CASE DEFAULT
      DO I=1,N,1
         IF (Y2(I)>1) THEN
            TI=Y2(I)-1
            DO J=1,TI,1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)
               END IF
            END DO
         END IF

         IF (D2(I)==0 .AND. N2(I)<N) THEN
            TI=N2(I)+1
            DO J=TI,N,1
               IF (D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)
               END IF
            END DO
         END IF

         IF (D2(I)==0) THEN
            DO J=Y2(I),N2(I),1
               IF (D2(J)==0 .AND. D1(J)==1 .AND. Y1(I)>=Y1(J)) THEN
                   W1=W1+REAL(Z(I)*(1-Z(J)),8)
                   L1=L1+REAL(Z(J)*(1-Z(I)),8)
                   VW1(I)=VW1(I)+REAL(Z(I)-Z(J),8)
                   VL1(J)=VL1(J)+REAL(Z(I)-Z(J),8)
               END IF
            END DO
         END IF
      END DO

   END SELECT

   WSTAT(1)=((W2+W1)-(L2+L1))
   WSTAT(2)=(W2+W1)/(L2+L1)
   P2=W2/L2;P1=W1/L1
   WSTAT(3)=P2*P1
   VSTAT(1)=SUM((VW2+VL2+VW1+VL1)**2)/REAL(N,8)/REAL(N**2,8)
   VSTAT(2)=SUM((VW2+VW1+VL2+VL1)**2)*REAL(N,8)/(L2+L1)**2
   VSTAT(3)=SUM(((VW2+VL2)*(P1*P2/L2)+(VW1+VL1)*(P1*P2/L1))**2)*REAL(N,8)
   LW2=W2
   LL2=L2
   LW1=W1
   LL1=L1
   RETURN
END SUBROUTINE WWRNULLB

SUBROUTINE WINRATIO(N,Y1,Y2,LD,Z,DBLBAD,WRATIO,VR,WDIFF,VD,WPROD,VP,LW2,LL2,LW1,LL1)
!DEC$ ATTRIBUTES DLLEXPORT,C,REFERENCE,ALIAS:'winratio_' ::WINRATIO
   IMPLICIT NONE
   INTEGER,INTENT(IN)::N,DBLBAD
   REAL(8),INTENT(IN)::Y1(N),Y2(N)
   INTEGER,INTENT(IN)::LD(N),Z(N)
   REAL(8),INTENT(OUT)::WRATIO,WDIFF,WPROD,VR,VD,VP
   INTEGER::I,J,D(N),H(N)
   REAL(8)::R1(0:1,N),R2(0:1,N),R3(0:1,N),R4(0:1,N),R5(0:1,N),R6(0:1,N)
   REAL(8)::al(N),bl(N),cl(N),dl(N),wl(N),rl(N),ul(N)
   REAL(8)::NL,NW,barw
   REAL(8)::LNA,LNB,LNC,LND
   REAL(8),INTENT(OUT)::LW2,LW1,LL2,LL1

   D=0
   H=0
   DO I=1,N,1
      IF (LD(I)==1 .OR. LD(I)==2) THEN
	       D(I)=1
	    END IF

      IF (LD(I)==1 .OR. LD(I)==4) THEN
	       H(I)=1
	    END IF
   END DO

   DO I=1,N,1
      DO J=0,1,1
	       R1(J,I)=REAL(COUNT(Y2>=Y2(I) .AND. Z==J),8)/REAL(N,8)
         R2(J,I)=REAL(COUNT(Y1>=Y1(I) .AND. Y2>=Y2(I) .AND. Z==J),8)/REAL(N,8)
		     R3(J,I)=REAL(COUNT(Y2<=Y2(I) .AND. D==1 .AND. Z==J),8)/REAL(N,8)
		     R4(J,I)=REAL(COUNT(Y1<=Y1(I) .AND. Y2<=Y2(I) .AND. LD==4 .AND. Z==J),8)/REAL(N,8)
         R5(J,I)=REAL(COUNT(Y1>=Y1(I) .AND. Y2<Y2(I) .AND. D==0 .AND. Z==J),8)/REAL(N,8)
		     R6(J,I)=REAL(COUNT(Y1<=Y1(I) .AND. Y2>Y2(I) .AND. H==1 .AND. Z==J),8)/REAL(N,8)
	    END DO
   END DO
   LNA=SUM(R1(0,:),D==1 .AND. Z==1)*REAL(N,8)
   LNB=SUM(R1(1,:),D==1 .AND. Z==0)*REAL(N,8)
   LNC=SUM(R2(0,:),LD==4 .AND. Z==1)*REAL(N,8)+SUM(R5(0,:),H==1 .AND. Z==1)*REAL(N,8)
   LND=SUM(R2(1,:),LD==4 .AND. Z==0)*REAL(N,8)+SUM(R5(1,:),H==1 .AND. Z==0)*REAL(N,8)

   IF (DBLBAD==1) THEN
      WRATIO=(LNB+LND)/(LNA+LNC);WDIFF=(LNB+LND)-(LNA+LNC);WPROD=(LNB/LNA)*(LND/LNC)
      LW2=LNB;LW1=LND;LL2=LNA;LL1=LNC      
   ELSE
      WRATIO=(LNB+LNC)/(LNA+LND);WDIFF=(LNB+LNC)-(LNA+LND);WPROD=(LNB/LNA)*(LNC/LND)
      LW2=LNB;LW1=LNC;LL2=LNA;LL1=LND
   END IF

   al=0.0;bl=0.0;cl=0.0;dl=0.0
   DO I=1,N,1
      al(I)=REAL(D(I)*Z(I),8)*R1(0,I)+REAL(1-Z(I),8)*R3(1,I)
	    bl(I)=REAL(D(I)*(1-Z(I)),8)*R1(1,I)+REAL(Z(I),8)*R3(0,I)
	    cl(I)=REAL(1-Z(I),8)*R4(1,I)+REAL(H(I)*Z(I),8)*R5(0,I)+REAL((1-D(I))*(1-Z(I)),8)*R6(1,I)
      dl(I)=REAL(Z(I),8)*R4(0,I)+REAL(H(I)*(1-Z(I)),8)*R5(1,I)+REAL((1-D(I))*Z(I),8)*R6(0,I)
	    IF (LD(I)==4) THEN
	       cl(I)=cl(I)+REAL(Z(I),8)*R2(0,I)
		     dl(I)=dl(I)+REAL(1-Z(I),8)*R2(1,I)
	    END IF
   END DO

   IF (DBLBAD==1) THEN
      wl=(bl+dl)-(al+cl)
	    rl=((bl+dl)-WRATIO*(al+cl))/(LNA+LNC)*REAL(N**2,8)
	    ul=(bl-LNB/LNA*al)/LNA*REAL(N**2,8)*(LND/LNC)+(dl-LND/LNC*cl)/LNC*REAL(N**2,8)*(LNB/LNA)
   ELSE
      wl=(bl+cl)-(al+dl)
	    rl=((bl+cl)-WRATIO*(al+dl))/(LNA+LND)*REAL(N**2,8)
	    ul=(bl-LNB/LNA*al)/LNA*REAL(N**2,8)*(LNC/LND)+(cl-LNC/LND*dl)/LND*REAL(N**2,8)*(LNB/LNA)
   END IF
   barw=SUM(wl)/REAL(N,8)
   VD=SUM((wl-barw)**2)/REAL(N,8)
   VR=SUM(rl**2)/REAL(N,8)
   VP=SUM(ul**2)/REAL(N,8)
   RETURN
END SUBROUTINE WINRATIO


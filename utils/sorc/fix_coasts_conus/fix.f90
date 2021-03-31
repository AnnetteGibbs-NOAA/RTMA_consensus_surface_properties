 program fix_coasts

! adjust the terrain provided by raytheon using my "waterfall" logic

 use grib_mod

 implicit none
 
 character*150 :: fngrib

 integer :: i, j, k, imdl, jmdl, lugb, lugi, iret
 integer(kind=1), allocatable :: mask(:,:)
 integer :: jdisc, jgdtn, jpdtn
 integer :: jids(200), jgdt(200), jpdt(200)

 logical :: unpack, done

 real(kind=8),allocatable :: orog(:,:)
 real :: test

 type(gribfield) :: gfld_mask, gfld_orog

! read mask.  note: 0 - water; 1 - land

 lugb = 50
 fngrib = "./fort.50"
 call baopenr(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 1
 end if

 j       = 0      ! search at beginning of file
 jdisc   = 2      ! search for discipline; 2 - land-sfc products
 jpdtn   = -1     ! search for any product definition template number
 jgdtn   = -1     ! search for any grid definition template number
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 unpack  = .true. ! unpack data
 lugi    = 0

 call grib2_null(gfld_mask)

 print*,"- DEGRIB DATA"
 call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
             unpack, k, gfld_mask, iret)

 if (iret /= 0) then
   print*,'- BAD READ, IRET IS ', iret
   stop 2
 end if

 call baclose(lugb, iret)

 if (iret /= 0) then
   print*,'- BAD CLOSE, IRET IS ', iret
   stop 3
 end if

 imdl = gfld_mask%igdtmpl(8)
 jmdl = gfld_mask%igdtmpl(9)

 allocate (mask(imdl,jmdl))
 mask= reshape (nint(gfld_mask%fld) , (/imdl,jmdl/) )

 lugb = 51
 fngrib = "./fort.51"
 call baopenr(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 1
 end if

 j       = 0      ! search at beginning of file
 jdisc   = -1     ! search for any
 jpdtn   = -1     ! search for any product definition template number
 jgdtn   = -1     ! search for any grid definition template number
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 unpack  = .true. ! unpack data
 lugi    = 0

 call grib2_null(gfld_orog)

 print*,"- DEGRIB DATA"
   call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld_orog, iret)

 if (iret /= 0) then
   print*,'- BAD READ, IRET IS ', iret
   stop 21
 end if

 call baclose(lugb, iret)

 if (iret /= 0) then
   print*,'- BAD CLOSE, IRET IS ', iret
   stop 31
 end if

 allocate (orog(imdl,jmdl))
 orog = reshape (gfld_orog%fld, (/imdl,jmdl/) )

 if (imdl == 1649 .and. jmdl == 1105) then
   call qc_data_alaska(mask, orog, imdl, jmdl)
 else
   call qc_data(mask, orog, imdl, jmdl)
 endif

 do k = 1, 5000
   print*,'k is ',k
   done=.true.
   do j=2,jmdl-1
     do i=2,imdl-1
      if (mask(i,j) == 0) then
        if(mask(i,j-1) == 0) then
           if (orog(i,j) > orog(i,j-1)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i,j-1))
        endif
        if(mask(i,j+1) == 0) then
           if (orog(i,j) > orog(i,j+1)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i,j+1))
        endif
        if(mask(i+1,j-1) == 0) then
           if (orog(i,j) > orog(i+1,j-1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i+1,j-1))
        endif
        if(mask(i+1,j) == 0) then
           if (orog(i,j) > orog(i+1,j)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i+1,j))
        endif
        if(mask(i+1,j+1) == 0) then
           if (orog(i,j) > orog(i+1,j+1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i+1,j+1))
        endif
        if(mask(i-1,j+1) == 0)then
           if (orog(i,j) > orog(i-1,j+1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i-1,j+1))
        endif
        if(mask(i-1,j) == 0) then
           if (orog(i,j) > orog(i-1,j)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i-1,j))
        endif
        if(mask(i-1,j-1) == 0.0) then
           if (orog(i,j) > orog(i-1,j-1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i-1,j-1))
        endif
      end if
    enddo
   enddo
  if (done) exit
 enddo

 print*,'- WATERFALLS REMOVED AFTER ', k, ' PASSES.'

 do j = 2, jmdl-1   
   do i = 2, imdl-1 
     if (mask(i,j) == 0) then ! water
       if (mask(i,j-1) == 1) then  ! land
         test = orog(i,j) - orog(i,j-1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i,j-1) = orog(i,j)+1.0
         endif
       endif
       if (mask(i,j+1) == 1) then
         test = orog(i,j) - orog(i,j+1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i,j+1) = orog(i,j)+1.0
         endif
       end if
       if (mask(i+1,j-1) == 1) then
         test = orog(i,j) - orog(i+1,j-1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i+1,j-1) = orog(i,j)+1.0
         endif
       endif
       if (mask(i+1,j+1) == 1) then
         test = orog(i,j) - orog(i+1,j+1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i+1,j+1) = orog(i,j)+1.0
         endif
       endif
       if (mask(i-1,j-1) == 1) then
         test = orog(i,j) - orog(i-1,j-1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i-1,j-1) = orog(i,j)+1.0
         endif
       endif
       if (mask(i-1,j+1) == 1) then
         test = orog(i,j) - orog(i-1,j+1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i-1,j+1) = orog(i,j)+1.0
         endif
       endif
       if (mask(i-1,j) == 1) then
         test = orog(i,j) - orog(i-1,j)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i-1,j) = orog(i,j)+1.0
         endif
       end if
       if (mask(i+1,j) == 1) then
         test = orog(i,j) - orog(i+1,j)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i+1,j) = orog(i,j)+1.0
         endif
       endif
     endif
   enddo
 enddo

 lugb = 48
 fngrib = "./no_waterfalls.gb2"
 call baopenw(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 5
 end if

 gfld_orog%fld=reshape(orog,(/imdl*jmdl/))

 call putgb2(lugb,gfld_orog,iret)

 if (iret /= 0) then
   print*,'- BAD WRITE, IRET IS ', iret
   stop 6
 end if

 call baclose(lugb, iret)

 print*,"NORMAL TERMINATION"

 stop

 end program fix_coasts

 subroutine qc_data_alaska(mask, orog, imdl, jmdl)

 implicit none

 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(inout) :: mask(imdl,jmdl)
 integer :: i, j

 real(kind=8), intent(inout) :: orog(imdl,jmdl)

 print*,"- QC DATA FOR ALASKA."

! Remove bad ocean points and land points that were
! below sea level.

 do j = 1, jmdl
 do i = 1, imdl
   if (orog(i,j) < 0.0) then
     if (mask(i,j) == 0) then
       print*,'- LOW WATER POINT ',i,j,orog(i,j)
       orog(i,j) = 0.0
     else
       print*,'- LOW LAND POINT ',i,j,orog(i,j)
       orog(i,j) = 1.0
     endif
   endif
 enddo
 enddo

! Remove some low (150 m) points in Great Slave lake.
! The wikipedia reported value is 157 m above sea level.

 do j = 750, 762
 do i = 1450, 1465
   if (mask(i,j) == 0 .and. orog(i,j) < 157.0) then
     print*,'- BAD LAKE ',i,j,orog(i,j)
     orog(i,j) = 157.0
   endif
 enddo
 enddo

 end subroutine qc_data_alaska

 subroutine qc_data(mask, orog, imdl, jmdl)

 implicit none

 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(inout) :: mask(imdl,jmdl)
 integer :: i, j

 real(kind=8), intent(inout) :: orog(imdl,jmdl)

 print*,"- QC MASK AND OROG."

! CONUS GRID ONLY!!
! Reset mask to remove a river between Lake Superior and the other
! Great Lakes. This point had a low elevation that was
! influencing the entire Lake Superior.  Also reset a point
! in Georgian Bay with a low elevation that was affecting
! Huron and Michigan.

 if (imdl == 2345 .and. jmdl == 1597) then ! is this the conus grid?
   print*,"- THIS IS A CONUS GRID."
   mask(1630:1636,1078:1086)=1  ! Superior 
   mask(1793:1798,1013:1018)=1  ! Georgian Bay
 endif

! Some ocean points had elevations below sea level which
! caused problems with the 'waterfall' logic.  Reset
! these, but be careful to retain the Salton Sea, which
! is minus 69 meters.

 do j = 1, jmdl
 do i = 1, imdl
   if (mask(i,j)==0 .and. orog(i,j) < 0.0) then
     if (orog(i,j) < -20.) cycle   ! salton sea
     print*,'- LOW WATER ',i,j,orog(i,j)
     orog(i,j)=0.0
   endif
 enddo
 enddo

 end subroutine qc_data

 subroutine grib2_null(gfld)

 use grib_mod

 implicit none

 type(gribfield), intent(inout)           :: gfld

 nullify(gfld%idsect)
 nullify(gfld%local)
 nullify(gfld%list_opt)
 nullify(gfld%igdtmpl)
 nullify(gfld%ipdtmpl)
 nullify(gfld%coord_list)
 nullify(gfld%idrtmpl)
 nullify(gfld%bmap)
 nullify(gfld%fld)

 end subroutine grib2_null

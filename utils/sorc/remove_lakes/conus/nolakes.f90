 program test

!--------------------------------------------------------------
! Read in a land mask in grib 2 format and remove small lakes
! from it. Only works for the Alaska, Hawaii, Puerto Rico
! and CONUS RTMA grids.
!
! The input grib2 file with all lakes is linked to Fortran
! unit number 50.
!--------------------------------------------------------------

 use grib_mod

 implicit none
 
 character*150 :: fngrib

 integer :: i, j, k, imdl, jmdl, lugb, lugi, iret
 integer :: is, ie, js, je
 integer :: jdisc, jgdtn, jpdtn
 integer :: jids(200), jgdt(200), jpdt(200)
 integer(kind=1), allocatable :: flag(:,:)
 integer(kind=1), allocatable :: mask(:,:)

 logical :: unpack

 real(kind=8),allocatable :: dummy(:,:)

 type(gribfield) :: gfld

!--------------------------------------------------------------
! Open and read the grib2 file with all lakes.
!--------------------------------------------------------------

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
 jpdt(1) = 0      ! search for parameter category - veg/biomass
 jpdt(2) = 0      ! search for parameter number - landcover
 unpack  = .true. ! unpack data
 lugi    = 0

 call grib2_null(gfld)

 print*,"- DEGRIB DATA"
   call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

 if (iret /= 0) then
   print*,'- BAD READ, IRET IS ', iret
   stop 2
 end if

 call baclose(lugb, iret)

 if (iret /= 0) then
   print*,'- BAD CLOSE, IRET IS ', iret
   stop 3
 end if

 imdl = gfld%igdtmpl(8)
 jmdl = gfld%igdtmpl(9)

 allocate (mask(imdl,jmdl))
 allocate (flag(imdl,jmdl))
 allocate (dummy(imdl,jmdl))

 dummy = reshape(gfld%fld , (/imdl,jmdl/) )
 mask= reshape (nint(gfld%fld) , (/imdl,jmdl/) )

 is=1
 js=1
 ie=imdl
 je=jmdl

!--------------------------------------------------------------
! The lake removal algorithm works by comparing a flag
! value at a water point with the flag values at 
! all neighboring water points.  If the flag value
! is smaller than one of its neighbors, the flag
! value at that water point is set to the larger
! value of its neighbor.  This process is repeated
! until the flag values are uniform within 
! an enclosed water body.  
!
! The flag values for each grid are set by the following
! routines.
!--------------------------------------------------------------

 if (imdl == 2345 .and. jmdl == 1597) then
   call set_flags_conus(flag, mask, imdl, jmdl)
 elseif (imdl == 1649 .and. jmdl == 1105) then
   call set_flags_alaska(flag, imdl, jmdl)
 elseif (imdl == 321 .and. jmdl == 225) then
   call set_flags_hawaii(flag, imdl, jmdl)
 elseif (imdl == 353 .and. jmdl == 257) then
   call set_flags_prico_1p25(flag, imdl, jmdl)
 elseif (imdl == 177 .and. jmdl == 129) then
   call set_flags_prico_2p5(flag, imdl, jmdl)
 else
   print*,"UNRECOGNIZED GRID. STOP."
   stop 3
 endif

!--------------------------------------------------------------
! Remove small lakes.
!--------------------------------------------------------------

 call lakes(mask,flag,imdl,jmdl,is,ie,js,je)

!--------------------------------------------------------------
! Convert the flag values to landmask values.
! The flag values are: 1-not water, 3-water.
! The landmask values are: 0-water, 1-not water.
!--------------------------------------------------------------

 do j=js,je
 do i=is,ie
   if(flag(i,j) == 3)then
    dummy(i,j) = 0.0
   else
    dummy(i,j) = 1.0
   endif
 enddo
 enddo

!--------------------------------------------------------------
! Write the 'no lake' grib2 file.
!--------------------------------------------------------------

 lugb = 48
 fngrib = "./nolakes.gb2"
 call baopenw(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 5
 end if

 gfld%fld=reshape(dummy,(/imdl*jmdl/))
 call putgb2(lugb,gfld,iret)

 if (iret /= 0) then
   print*,'- BAD WRITE, IRET IS ', iret
   stop 6
 end if

 call baclose(lugb, iret)

 print*,'- NORMAL TERMINATION'

 stop

 end program test

 subroutine set_flags_prico_1p25(flag,imdl,jmdl)

!--------------------------------------------------------------
! Set flag values for the 1.25 km Puerto Rico grid.
!
! Here, set the flag value to one for the entire
! grid. Then, set some seed flag values to three for
! water bodies we want to keep.
!--------------------------------------------------------------

 implicit none
 
 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(out) :: flag(imdl,jmdl)

 print*,"- SET FLAGS FOR 1.25 KM PUERTO RICO GRID."

 flag=1
 flag(20,20)=3  ! Atlantic ocean

 end subroutine set_flags_prico_1p25

 subroutine set_flags_prico_2p5(flag,imdl,jmdl)

!--------------------------------------------------------------
! Set flag values for the 2.5 km Puerto Rico grid.
!
! Here, set the flag value to one for the entire
! grid. Then, set some seed flag values to three for
! water bodies we want to keep.
!--------------------------------------------------------------

 implicit none
 
 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(out) :: flag(imdl,jmdl)

 print*,"- SET FLAGS FOR 2.5 KM PUERTO RICO GRID."

 flag=1
 flag(20,20)=3  ! Atlantic ocean

 end subroutine set_flags_prico_2p5

 subroutine set_flags_hawaii(flag,imdl,jmdl)

!--------------------------------------------------------------
! Set flag values for the Hawaii grid.
!
! Here, set the flag value to one for the entire
! grid. Then, set some seed flag values to three for
! water bodies we want to keep.
!--------------------------------------------------------------

 implicit none
 
 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(out) :: flag(imdl,jmdl)

 print*,"- SET FLAGS FOR HAWAII GRID."

 flag=1
 flag(20,20)=3  ! pac ocean

 end subroutine set_flags_hawaii

 subroutine set_flags_alaska(flag,imdl,jmdl)

!--------------------------------------------------------------
! Set flag values for the Alaska grid.
!
! Here, set the flag value to one for the entire
! grid. Then, set some seed flag values to three for
! water bodies we want to keep.
!--------------------------------------------------------------

 implicit none
 
 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(out) :: flag(imdl,jmdl)

 print*,"- SET FLAGS FOR ALASKA GRID."

 flag=1
 flag(800,100)=3  ! pac ocean
 flag(200,600)=3  ! pac ocean
 flag(800,1000)=3 ! arctic ocean
 flag(50,950) = 3 ! kamchatka

 end subroutine set_flags_alaska

 subroutine set_flags_conus(flag,mask,imdl,jmdl)

!--------------------------------------------------------------
! Set flag values for the CONUS grid.
!
! Here, set the flag value to one for the entire
! grid. Then, set some seed flag values to three for
! water bodies we want to keep.
!
! To get it work, there were situations where the mask
! had to be changed as well, to either add or remove
! connections between water bodies.
!--------------------------------------------------------------

 implicit none
 
 integer, intent(in) :: imdl, jmdl
 integer(kind=1), intent(inout) :: mask(imdl,jmdl)
 integer(kind=1), intent(out) :: flag(imdl,jmdl)

 print*,"- SET FLAGS FOR CONUS GRID."

 flag=1
 flag(200,290)=3  ! pac ocean
 flag(50,1200)=3  ! pac ocean
 flag(600,10)=3   ! pac ocean
 flag(690,855)=3  ! great salt lake - main lake
 flag(701,850)=3   ! great salt lake - small adjacent lake to
                   ! south east
 flag(1300,100)=3  ! gulf of mex
 flag(2000,300)=3  ! atl ocean
 flag(2200,900)=3  ! atl ocean
 flag(2300,1250)=3  ! atl ocean
 flag(1700,1500)=3  ! hudson bay
 flag(1550,1100)=3  ! lake superior
 flag(1550,900)=3  ! lakes michigan and huron
 flag(1750,900)=3  ! lake erie
 flag(1850,965)=3  ! lake ontario
!flag(1705,900)=3  ! lake saint clair
 flag(524,525)=3   ! salton sea
 flag(1840,220)=3  ! lake okeechobee
 flag(1200,1380)=3  ! lake winnipeg
 flag(1800,1)=3  ! cuba

!--------------------------------------------------------------
! Retain Detroit river so lake erie and lake st.
! clair are connected.
!--------------------------------------------------------------

!mask(1695,893) = 0
!mask(1693,887) = 0

!--------------------------------------------------------------
! Create land bridge so logic removes upper portions
! of the Saint Lawrence River. 
!--------------------------------------------------------------

!mask(2150:2220,1300) =  1

 end subroutine set_flags_conus

 subroutine lakes(mask,flag,imdl,jmdl,is,ie,js,je)

!--------------------------------------------------------------
! Remove lakes.
!--------------------------------------------------------------

 implicit none

 integer :: imdl, jmdl, i,j,k,is,ie,js,je
 integer*1 :: mask(imdl,jmdl), flag(imdl,jmdl)

 logical :: done

 do k = 1, 20000
   done=.true.
 do j = js, je
   do i = is, ie
      if (mask(i,j) == 0) then
        if (j > 1) then
          if(mask(i,j-1) == 0 .and. flag(i,j) < flag(i,j-1)) then
             flag(i,j) =  max(flag(i,j),flag(i,j-1))
             done=.false.
          endif
        endif
        if (j < jmdl) then
          if(mask(i,j+1) == 0 .and. flag(i,j) < flag(i,j+1)) then
             flag(i,j) =  max(flag(i,j),flag(i,j+1))
             done=.false.
          endif
        endif 
        if (i < imdl .and. j > 1) then
          if(mask(i+1,j-1) == 0 .and. flag(i,j) < flag(i+1,j-1)) then
             flag(i,j) = max(flag(i,j),flag(i+1,j-1))
             done=.false.
          endif
        endif
        if (i < imdl) then
          if(mask(i+1,j) == 0 .and. flag(i,j) < flag(i+1,j)) then
             flag(i,j) =  max(flag(i,j),flag(i+1,j))
             done=.false.
          endif
        endif
        if (i < imdl .and. j < jmdl) then
          if(mask(i+1,j+1) == 0 .and. flag(i,j) < flag(i+1,j+1)) then
             flag(i,j) = max(flag(i,j),flag(i+1,j+1))
             done=.false.
          endif
        endif
        if (i > 1 .and. j < jmdl) then
          if(mask(i-1,j+1) == 0 .and. flag(i,j) < flag(i-1,j+1)) then
             flag(i,j) = max(flag(i,j),flag(i-1,j+1))
             done=.false.
          endif
        endif
        if (i > 1) then
          if(mask(i-1,j) == 0 .and. flag(i,j) < flag(i-1,j)) then
             flag(i,j) =  max(flag(i,j),flag(i-1,j))
             done=.false.
          endif
        endif
        if (i > 1 .and. j > 1) then
          if(mask(i-1,j-1) == 0 .and. flag(i,j) < flag(i-1,j-1)) then
             flag(i,j) = max(flag(i,j),flag(i-1,j-1))
             done=.false.
          endif
        endif
      end if
   enddo
 enddo
  if (done) then
    print*,'exit loop after ',k, ' iterations.'
    exit
  endif
 enddo

 end subroutine lakes

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

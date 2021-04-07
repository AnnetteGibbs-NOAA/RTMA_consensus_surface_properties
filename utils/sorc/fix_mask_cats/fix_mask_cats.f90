 program fix_mask

! read in raytheon's land mask and convert the categories
! to the standard '0'-water, '1'-land.

 use grib_mod

 implicit none
 
 character*100 :: fngrib

 integer :: i, j, k, imdl, jmdl, lugb, lugi, iret
 integer(kind=1), allocatable :: mask(:,:)
 integer :: jdisc, jgdtn, jpdtn
 integer :: jids(200), jgdt(200), jpdt(200)

 logical :: unpack

 real(kind=8),allocatable :: dummy(:,:)

 type(gribfield) :: gfld

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
 allocate (dummy(imdl,jmdl))

 mask= reshape (nint(gfld%fld) , (/imdl,jmdl/) )

! the raytheon data (from geoff wagner) has '0'- ocean,
! '3' inland water, '9' land.  Convert this to the
! more standard '0' water and '1' land.

 do j=1,jmdl
 do i=1,imdl
   if(mask(i,j)==9) mask(i,j)=1
   if(mask(i,j)==3) mask(i,j)=0
 enddo
 enddo

 lugb = 48
 fngrib = "./mask.correct.cat.gb2"
 call baopenw(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 5
 end if

 dummy=float(mask)
 gfld%fld=reshape(dummy,(/imdl*jmdl/))
 call putgb2(lugb,gfld,iret)

 if (iret /= 0) then
   print*,'- BAD WRITE, IRET IS ', iret
   stop 6
 end if

 call baclose(lugb, iret)

 stop

 end program fix_mask

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

 program fixscan

! Read in a grib file with a scan mode of 80 (alternate
! rows scan in the opposite direction) and convert to
! a scan model of 64.

 use grib_mod

 implicit none
 
 character*100 :: fngrib

 integer :: i, j, k, imdl, jmdl, lugb, lugi, iret
 integer :: jdisc, jgdtn, jpdtn
 integer :: jids(200), jgdt(200), jpdt(200)

 logical :: unpack

 real(kind=8),allocatable :: dummy(:,:), dummy2(:,:)

 type(gribfield) :: gfld

! Read grib2 file with the scan mode of 80.

 lugb = 50
 fngrib = "./fort.50"
 call baopenr(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 1
 end if

 j       = 0      ! search at beginning of file
 jdisc   = -1     ! search for any discipline
 jpdtn   = -1     ! search for any product definition template number
 jgdtn   = -1     ! search for any grid definition template number
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
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

 allocate (dummy2(imdl,jmdl))
 allocate (dummy(imdl,jmdl))

 dummy= reshape (nint(gfld%fld) , (/imdl,jmdl/) )

 if (gfld%igdtnum == 20) then  ! ak grid is polar
   gfld%igdtmpl(18) = 64 ! scan mode
 else
   gfld%igdtmpl(16) = 64 ! scan mode
 endif

 do j = 1, jmdl
  if (mod(j,2) == 0) then
    do i = 1, imdl
      dummy2(i,j) = nint(dummy(imdl-i+1,j))
    enddo
  else
    dummy2(:,j) = nint(dummy(:,j))
  endif
 enddo

 lugb = 48
 fngrib = "./scan64.gb2"
 call baopenw(lugb,fngrib,iret)
 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   stop 5
 end if

 gfld%fld=reshape(dummy2,(/imdl*jmdl/))
 call putgb2(lugb,gfld,iret)

 if (iret /= 0) then
   print*,'- BAD WRITE, IRET IS ', iret
   stop 6
 end if

 call baclose(lugb, iret)

 stop

 end program fixscan

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

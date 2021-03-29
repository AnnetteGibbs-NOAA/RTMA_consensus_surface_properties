 program test

! read in a land mask in grib 2 format and remove small lakes
! from it.  i use a modified version of the 'waterfall'
! code from gridgen_sfc to do this.  

 use grib_mod

 implicit none
 
 character*150 :: fngrib

 integer :: i, j, k, imdl, jmdl, lugb, lugi, iret
 integer :: is, ie, js, je
 integer(kind=1), allocatable :: mask(:,:), flag(:,:)
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
 jdisc   = -1     ! search for any discipline
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

! the algorithm will compare the flag
! value at a water point with the flag values at 
! all neighboring water points.  if the flag value
! is smaller than one of its neighbors, the flag
! value at that water point is set to the larger
! value of its neighbor.  this process is repeated
! until the flag values are uniform within 
! an enclosed water body.  
!
! here, set the flag value to one for the entire
! grid.  set some seed flag values to three for
! water bodies we want to keep.  here, those
! bodies are the ocean.

 flag=1
 flag(20,20)=3  ! pac ocean

 call lakes(mask,flag,imdl,jmdl,is,ie,js,je)

! convert the flag values to landmask values.
! the flag values are: 1-not water, 3-water.
! the landmask values are: 0-water, 1-not water.

 do j=js,je
 do i=is,ie
   if(flag(i,j) == 3)then
    dummy(i,j) = 0.0
   else
    dummy(i,j) = 1.0
   endif
 enddo
 enddo

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

 stop

 end program test

 subroutine lakes(mask,flag,imdl,jmdl,is,ie,js,je)

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

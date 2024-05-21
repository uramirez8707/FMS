program test
	use platform_mod,      only: r4_kind, r8_kind
	use mpp_domains_mod,   only: mpp_define_domains, mpp_define_io_domain, mpp_get_data_domain, &
    	                         mpp_domains_set_stack_size, mpp_get_compute_domain, domain2d
	use mpp_mod,           only: mpp_init, mpp_exit, mpp_pe, mpp_root_pe, mpp_error, FATAL, &
  	                           input_nml_file, mpp_sync, NOTE
	use data_override_mod, only: data_override_init, data_override
	use time_manager_mod,  only: set_calendar_type, time_type, set_date, NOLEAP, set_time
	use diag_manager_mod, only: diag_axis_init, register_diag_field, send_data, diag_manager_end, diag_manager_init, &
  diag_send_complete, diag_manager_set_time_end, operator(+)
	use fms2_io_mod, only: open_file, read_data, close_file, FmsNetcdfFile_t
	use fms_mod, only: fms_init, fms_end

	implicit none

	integer, dimension(2)                      :: layout = (/1,1/) !< Domain layout
	integer                                    :: nlon             !< Number of points in x axis
	integer                                    :: nlat             !< Number of points in y axis
	type(domain2d)                             :: Domain           !< Domain with mask table
	integer                                    :: is               !< Starting x index
	integer                                    :: ie               !< Ending x index
	integer                                    :: js               !< Starting y index
	integer                                    :: je               !< Ending y index
	type(time_type)                            :: Time
	integer                                    :: nhalox=2, nhaloy=2
	real, allocatable :: outdata(:,:)
	logical :: override
	real, dimension(:), allocatable :: x, y
	integer :: id_x, id_y, id_rlds, id_rlds2, id_rlds3
	logical :: used
  integer :: i

	call fms_init()

	call set_calendar_type(NOLEAP)


	nlon = 540
	nlat = 696

	!< Create a domain nlonXnlat with mask
	call mpp_domains_set_stack_size(17280000)
	call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, xhalo=nhalox, yhalo=nhaloy, symmetry=.false., name='test_data_override_emc')
	call mpp_define_io_domain(Domain, (/1,1/))
	call mpp_get_compute_domain(Domain, is, ie, js, je)

	allocate(outdata(is:ie, js:je))

	call data_override_init(ocean_domain_in=Domain)
  call data_override_init(ice_domain_in=Domain)
  call data_override_init(atm_domain_in=Domain)

	call diag_manager_init()
	Time = set_date(1993,1,1,0,0,0)
  allocate(x(540), y(696))
  call get_x_y(x,y)
	id_y  = diag_axis_init('lat',  y,  units='degrees_east', cart_name='y', long_name='point_E', Domain2=Domain)
	id_x  = diag_axis_init('lon',  x,  units='degrees_north', cart_name='x', long_name='point_N', Domain2=Domain)

  id_rlds = register_diag_field  ('OCN', 'rlds', (/id_x,id_y/), Time, missing_value=-999.99)
  id_rlds2 = register_diag_field  ('OCN', 'rlds2', (/id_x,id_y/), Time, missing_value=-999.99)
  id_rlds3 = register_diag_field  ('OCN', 'rlds3', (/id_x,id_y/), Time, missing_value=-999.99)

  Time = set_date(1993,1,1,0,0,0)
  call diag_manager_set_time_end(set_date(1994,1,1,0,0,0))
  do i = 1,31
    Time = Time + set_time (3600*24,0)

  outdata = -999.99
  call data_override('OCN','rlds',outdata, Time, override=override)
	used = send_data(id_rlds, outdata, time )

  call data_override('ICE','rlds2',outdata, Time, override=override)
	used = send_data(id_rlds2, outdata, time )

  call data_override('ATM','rlds3',outdata, Time, override=override)
	used = send_data(id_rlds3, outdata, time )

  call diag_send_complete(set_time (3600*24,0))
enddo

	call diag_manager_end(Time)


	call fms_end()

	contains
	subroutine get_x_y(xin,yin)
		real, intent(inout) :: xin(:)
		real, intent(inout) :: yin(:)

		type(FmsNetcdfFile_t) :: fileobj

		if (open_file(fileobj, "INPUT/ocean_static.nc", "read")) then
			 call read_data(fileobj, "xh", xin)
			 call read_data(fileobj, "yh", yin)
       call close_file(fileobj)
		endif

	end subroutine get_x_y
end program test
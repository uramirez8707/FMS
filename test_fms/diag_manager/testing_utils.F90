module testing_utils
    use platform_mod,      only: r8_kind
    private
  
    public :: allocate_buffer
    contains
  
    function allocate_buffer(is, ie, js, je, k, l) &
      result(buffer)
  
      integer, intent(in) :: is
      integer, intent(in) :: ie
      integer, intent(in) :: js
      integer, intent(in) :: je
      integer, intent(in) :: k
      integer, intent(in) :: l
  
      real(kind=r8_kind), allocatable :: buffer(:,:,:,:)
  
      allocate(buffer(is:ie, js:je, 1:k, 1:l))
      buffer = -999_r8_kind
    end function allocate_buffer
  end module
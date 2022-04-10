extern "C" {
    pub(crate) fn malloc(size: usize) -> *mut u8;

    pub(crate) fn free(ptr: *mut u8);
}

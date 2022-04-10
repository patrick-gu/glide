use std::{
    alloc::{alloc, dealloc, handle_alloc_error, Layout},
    mem::MaybeUninit,
    ptr, slice,
};

use crate::alloc::{free, malloc};

#[derive(Copy, Clone)]
pub(crate) struct ObjectHeader {
    mark: Mark,
    next: *mut Self,
}

impl ObjectHeader {
    pub(crate) unsafe fn should_sweep(ptr: *const Self) -> bool {
        let mark = (*ptr).mark;
        matches!(mark, Mark::White)
    }

    pub(crate) unsafe fn next(ptr: *mut Self) -> *mut Self {
        (*ptr).next
    }

    pub(crate) unsafe fn free(ptr: *mut Self) {
        free(ptr.cast())
    }

    pub(crate) unsafe fn clear_mark(ptr: *mut Self) {
        (*ptr).mark = Mark::White;
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct StringHeader {
    object: ObjectHeader,
    len: usize,
}

impl StringHeader {
    pub(crate) fn alloc(next: *mut ObjectHeader, len: usize) -> (*mut Self, *mut u8) {
        let layout = Layout::new::<Self>()
            .extend(Layout::array::<u8>(len).unwrap())
            .unwrap()
            .0;

        if len > isize::MAX as usize {
            handle_alloc_error(layout)
        }

        // SAFETY: the size of the allocation must be greater than zero
        let ptr = unsafe { malloc(layout.size()) };
        if ptr.is_null() {
            handle_alloc_error(layout);
        }
        let ptr: *mut Self = ptr.cast();
        let header = Self {
            object: ObjectHeader {
                mark: Mark::default(),
                next,
            },
            len,
        };
        // SAFETY: the object was just allocated
        unsafe { ptr::write(ptr, header) };

        let buffer_ptr = unsafe { Self::buffer_ptr(ptr) };

        (ptr, buffer_ptr)
    }

    pub(crate) unsafe fn buffer_ptr(ptr: *mut Self) -> *mut u8 {
        let len = Self::len(ptr);

        let offset = Layout::new::<Self>()
            .extend(Layout::array::<u8>(len).unwrap())
            .unwrap()
            .1;

        offset_bytes_mut(ptr, offset)
    }

    pub(crate) unsafe fn len(ptr: *const Self) -> usize {
        let h = &*ptr;
        h.len
    }

    pub(crate) unsafe fn mark(ptr: *mut Self) {
        (*ptr).object.mark = Mark::Black;
    }
}

#[derive(Copy, Clone)]
enum Mark {
    White,
    Grey,
    Black,
}

impl Default for Mark {
    fn default() -> Self {
        Self::White
    }
}

unsafe fn offset_bytes_mut<T, U>(ptr: *mut T, offset: usize) -> *mut U {
    let ptr: *mut u8 = ptr.cast();
    let ptr = ptr.add(offset);
    ptr.cast()
}

#[cfg(feature = "mimalloc")]
mod mimalloc {
    use libmimalloc_sys::{mi_free, mi_malloc_aligned, mi_zalloc_aligned};
    use std::{alloc::Layout, ffi::c_void};

    pub unsafe fn alloc<T>(layout: Layout, data: T) -> *mut T {
        let ptr = mi_malloc_aligned(layout.size(), layout.align()).cast::<T>();
        ptr.write(data);
        ptr
    }

    pub unsafe fn zalloc<T>(layout: Layout) -> *mut T {
        mi_zalloc_aligned(layout.size(), layout.align()).cast::<T>()
    }

    pub unsafe fn dealloc(ptr: *mut u8) {
        mi_free(ptr as *mut c_void);
    }
}

#[cfg(not(feature = "mimalloc"))]
mod alloc {
    use std::{alloc::Layout, collections::HashMap, sync::RwLock};

    use lazy_static::lazy_static;

    lazy_static! {
        static ref LAYOUT_META: RwLock<HashMap<usize, Layout>> = RwLock::new(HashMap::new());
    }

    macro_rules! lock {
        ($data:expr) => {
            match $data.write() {
                Ok(lock) => lock,
                Err(poisoned) => poisoned.into_inner(),
            }
        };
    }

    pub unsafe fn alloc<T>(layout: Layout, data: T) -> *mut T {
        let ptr = std::alloc::alloc(layout).cast::<T>();
        lock!(LAYOUT_META).insert(ptr as usize, layout);
        ptr.write(data);
        ptr
    }

    pub unsafe fn zalloc<T>(layout: Layout) -> *mut T {
        let ptr = std::alloc::alloc_zeroed(layout).cast::<T>();
        lock!(LAYOUT_META).insert(ptr as usize, layout);
        ptr
    }

    pub unsafe fn dealloc(ptr: *mut u8) {
        let layout = lock!(LAYOUT_META)
            .get(&(ptr as usize))
            .copied()
            .expect("dealloc called on untracked pointer");
        std::alloc::dealloc(ptr, layout);
    }
}

#[cfg(feature = "mimalloc")]
pub use mimalloc::{alloc, dealloc, zalloc};

#[cfg(not(feature = "mimalloc"))]
pub use alloc::{alloc, dealloc, zalloc};

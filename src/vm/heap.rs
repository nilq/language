use hashbrown::{HashMap, HashSet};
use std::{
    hash::{Hash, Hasher},
};

// No GC. Just chaos.
#[derive(Clone)]
pub struct Heap<T> {
    obj_counter: usize,
    objects: HashSet<Handle<T>>,
}

impl<T> Heap<T> {
    pub fn new() -> Self {
        Self {
            obj_counter: 0,
            objects: HashSet::default(),
        }
    }

    fn new_generation(&mut self) -> usize {
        self.obj_counter += 1;
        self.obj_counter
    }

    pub fn insert(&mut self, object: T) -> Handle<T> {
        let ptr = Box::into_raw(Box::new(object));

        let gen = self.new_generation();
        let handle = Handle { gen, ptr };
        self.objects.insert(handle);

        handle
    }

    pub fn len(&self) -> usize {
        self.objects.len()
    }

    pub fn contains(&self, handle: impl AsRef<Handle<T>>) -> bool {
        let handle = handle.as_ref();
        self.objects.contains(&handle)
    }

    pub fn get(&self, handle: impl AsRef<Handle<T>>) -> Option<&T> {
        let handle = handle.as_ref();
        if self.contains(handle) {
            Some(unsafe { &*handle.ptr })
        } else {
            None
        }
    }

    pub unsafe fn get_unchecked(&self, handle: impl AsRef<Handle<T>>) -> &T {
        let handle = handle.as_ref();
        debug_assert!(self.contains(handle));
        &*handle.ptr
    }

    pub fn get_mut_unchecked(&mut self, handle: impl AsRef<Handle<T>>) -> &mut T {
        let handle = handle.as_ref();
        debug_assert!(self.contains(handle));
        unsafe { &mut *handle.ptr }
    }

    pub fn get_mut(&mut self, handle: impl AsRef<Handle<T>>) -> Option<&mut T> {
        let handle = handle.as_ref();
        if self.contains(handle) {
            Some(unsafe { &mut *handle.ptr })
        } else {
            None
        }
    }
}

impl<T> Drop for Heap<T> {
    fn drop(&mut self) {
        for handle in &self.objects {
            drop(unsafe { Box::from_raw(handle.ptr) });
        }
    }
}

#[derive(Debug)]
pub struct Handle<T> {
    gen: usize,
    ptr: *mut T,
}

impl<T> Handle<T> {
    pub unsafe fn get_unchecked(&self) -> &T {
        &*self.ptr
    }

    pub unsafe fn get_mut_unchecked(&self) -> &mut T {
        &mut *self.ptr
    }
}

impl<T> Copy for Handle<T> {}
impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self { gen: self.gen, ptr: self.ptr }
    }
}

impl<T> PartialEq<Self> for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.gen == other.gen && self.ptr == other.ptr
    }
}
impl<T> Eq for Handle<T> {}

impl<T> Hash for Handle<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.gen.hash(state);
        self.ptr.hash(state);
    }
}

impl<T> AsRef<Handle<T>> for Handle<T> {
    fn as_ref(&self) -> &Handle<T> {
        self
    }
}

#[derive(Debug)]
pub struct TaggedHandle<T> {
    handle: Handle<T>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tag<T> {
    Tag(u8),
    Number(f64),
    Handle(Handle<T>),
}

const QNAN: u64 = 0x7ffc000000000000;
const SIGN: u64 = 1 << 63;

impl<T> TaggedHandle<T> {
    pub unsafe fn from_raw(raw: u64) -> Self {
        TaggedHandle {
            handle: Handle {
                gen: 0,
                ptr: raw as *mut T
            },
        }
    }

    pub fn to_raw(&self) -> u64 {
        self.handle.ptr as u64
    }

    pub fn from_handle(handle: Handle<T>) -> Self {
        let u = (handle.ptr as u64) | QNAN | SIGN;
        TaggedHandle{
            handle: Handle {
                gen: handle.gen,
                ptr: u as *mut T,
            }
        }
    }

    pub fn from_float(float: f64) -> Self {
        TaggedHandle {
            handle: Handle {
                gen: 0,
                ptr: unsafe { ::std::mem::transmute(float) },
            },
        }
    }

    pub fn from_tag(tag: u8) -> Self {
        TaggedHandle {
            handle: Handle {
                gen: 0,
                ptr: unsafe { ::std::mem::transmute(QNAN | (tag as u64)) },
            },
        }
    }

    pub fn decode(self) -> Tag<T> {
        let u = self.handle.ptr as u64;
        if u & QNAN != QNAN {
            return Tag::Number(unsafe { ::std::mem::transmute(u) });
        }
        if (u & (QNAN | SIGN)) == (QNAN | SIGN) {
            let ptr = u & (!(QNAN | SIGN)); // only keep lower 51 bits
            return Tag::Handle(Handle {
                gen: self.handle.gen,
                ptr: ptr as *mut T,
            });
        }
        let tag: u8 = (u & 7) as u8;
        Tag::Tag(tag)
    }
}

impl<T> Clone for TaggedHandle<T> {
    fn clone(&self) -> Self {
        TaggedHandle { handle: self.handle }
    }
}
impl<T> Copy for TaggedHandle<T> {}

impl<T> PartialEq<Self> for TaggedHandle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}
impl<T> Eq for TaggedHandle<T> {}

impl<T> From<Handle<T>> for TaggedHandle<T> {
    fn from(handle: Handle<T>) -> Self {
        Self::from_handle(handle)
    }
}

impl<T> From<f64> for TaggedHandle<T> {
    fn from(float: f64) -> Self {
        Self::from_float(float)
    }
}
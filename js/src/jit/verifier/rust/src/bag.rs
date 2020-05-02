use std::fmt;
use std::marker::PhantomData;
use std::mem;

pub unsafe trait BagId: Clone + Copy + Sized {
    const _FITS_IN_USIZE: [(); 0];

    unsafe fn from_usize(n: usize) -> Self;
    fn to_usize(self) -> usize;
}

unsafe impl BagId for u32 {
    const _FITS_IN_USIZE: [(); 0] = [(); (mem::size_of::<u32>() > mem::size_of::<usize>()) as usize];

    #[inline]
    unsafe fn from_usize(n: usize) -> Self {
        n as u32
    }

    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
}

#[derive(Clone)]
pub struct Bag<Id, Item> {
    items: Box<[Option<Item>]>,
    _phantom: PhantomData<Id>,
}

impl<Id, Item> fmt::Debug for Bag<Id, Item>
where
    Id: BagId + fmt::Debug,
    Item: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<Id, Item> Bag<Id, Item> where Id: BagId {
    pub fn new(capacity: Id) -> Self {
        let capacity = capacity.to_usize();

        let mut items = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            items.push(None);
        }

        Bag {
            items: items.into_boxed_slice(),
            _phantom: PhantomData,
        }
    }

    pub fn capacity(&self) -> Id {
        unsafe { Id::from_usize(self.items.len()) }
    }

    pub fn iter(&self) -> impl Iterator<Item=(Id, &Item)> {
        self.items.iter().enumerate().filter_map(|(n, x)| x.as_ref().map(|y| (unsafe { Id::from_usize(n) }, y)))
    }

    pub fn item(&self, id: Id) -> Option<&Item> {
        let id = id.to_usize();

        if self.items.len() <= id {
            None
        } else {
            self.items[id].as_ref()
        }
    }

    pub fn item_mut(&mut self, id: Id) -> Option<&mut Item> {
        let id = id.to_usize();

        if self.items.len() <= id {
            None
        } else {
            self.items[id].as_mut()
        }
    }

    pub fn put(&mut self, id: Id, item: Item) -> bool {
        let id = id.to_usize();

        if self.items.len() <= id {
            // This item doesn't fit in the capacity we allocated.
            return false;
        }

        let item_ref = &mut self.items[id];

        if item_ref.is_some() {
            // An item with this id has already been filled in.
            return false;
        }

        *item_ref = Some(item);
        true
    }
}

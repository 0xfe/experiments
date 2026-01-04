pub struct MoStack<T> {
    items: Vec<T>,
    pos: u32,
}

impl<T> MoStack<T> {
    pub fn new() -> MoStack<T> {
        MoStack {
            items: vec![],
            pos: 0,
        }
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item);
        self.pos += 1
    }

    pub fn pop(&mut self) -> Option<T> {
        self.items.pop()
    }
}

pub struct MeStack<'a, T: Clone> {
    items: &'a mut [T],
    pos: u32,
}

impl<'a, T: Clone> MeStack<'a, T> {
    pub fn new(items: &'a mut [T]) -> MeStack<'a, T> {
        MeStack { items, pos: 0 }
    }

    pub fn push(&mut self, item: T) {
        self.items[self.pos as usize] = item;
        self.pos += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.pos == 0 {
            return None;
        }
        self.pos -= 1;
        Some(self.items[self.pos as usize].clone())
    }
}

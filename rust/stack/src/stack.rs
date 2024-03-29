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

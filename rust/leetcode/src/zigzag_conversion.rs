// https://leetcode.com/problems/zigzag-conversion/

/*

Z   G   A
I  IZ  ZG
G Z A G Z
ZG  GI  I
A   Z   G

--- init:

num_rows = 5
spacing = num_rows - 2
cols_per_block = spacing + 1
chars_per_block = num_rows + spacing

-- solve:
str = zigzagzigzagzigzagzig
len = 21

num_full_blocks = len / chars_per_block
chars_in_remaining_block = len % chars_per_block
num_blocks = num_full_blocks + (1 if chars_in_remaining_block > 0 else 0)

-- final:
num_cols = num_blocks * cols_per_block

--- corner cases

len < num_rows
len == num_rows
len < chars_per_block
*/

#[allow(dead_code)]
#[derive(Debug)]
struct Board {
    s: String,
    num_rows: i32,

    // -- calculated
    len: i32,
    spacing: i32,
    cols_per_block: i32,
    chars_per_block: i32,
    num_full_blocks: i32,
    chars_in_remaining_block: i32,
    num_blocks: i32,
    num_cols: i32,

    // -- state
    rows: Vec<Vec<char>>,
}

#[derive(Debug)]
enum Direction {
    Down,
    Up,
}

impl Board {
    pub fn new(s: String, num_rows: i32) -> Self {
        let spacing = num_rows - 2;
        let cols_per_block = spacing + 1;
        let chars_per_block = num_rows + spacing;
        let len = s.len() as i32;
        let num_full_blocks = len / chars_per_block;
        let chars_in_remaining_block = len % chars_per_block;
        let num_blocks = num_full_blocks + (if chars_in_remaining_block > 0 { 1 } else { 0 });
        let num_cols = num_blocks * cols_per_block;
        let mut rows = Vec::with_capacity(num_rows as usize);
        for _ in 0..num_rows {
            rows.push(vec![0 as char; num_cols as usize]);
        }

        Self {
            s,
            len,
            num_rows,
            spacing,
            cols_per_block,
            chars_per_block,
            num_full_blocks,
            chars_in_remaining_block,
            num_blocks,
            num_cols,
            rows,
        }
    }

    pub fn plot(&mut self, row: i32, col: i32, c: char) {
        println!("plotting {} at ({}, {})", c, row, col);
        self.rows[row as usize][col as usize] = c;
    }

    pub fn solve(&mut self) {
        use Direction::*;

        let mut dir = Down;
        let mut row = 0;
        let mut col = 0;

        for i in 0..self.s.len() {
            let c = self.s.chars().nth(i).unwrap();
            match dir {
                Down => {
                    if row >= self.num_rows {
                        dir = Up;
                        col += 1;
                        row -= 2;
                        self.plot(row, col, c);
                        row -= 1;
                    } else {
                        self.plot(row, col, c);
                        row += 1;
                    }
                }
                Up => {
                    if row < 0 {
                        dir = Down;
                        row = 1;
                        self.plot(row, col, c);
                        row += 1;
                    } else {
                        self.plot(row, col, c);
                        row -= 1;
                        col += 1;
                    }
                }
            }
        }
    }

    pub fn as_zigzag(&self) -> String {
        let mut s = String::with_capacity(self.s.len());
        for row in self.rows.iter() {
            for c in row.iter() {
                if *c != 0 as char {
                    s.push(*c);
                }
            }
        }
        s
    }
}

pub fn convert(s: String, num_rows: i32) -> String {
    if s.len() == 1 || num_rows == 1 {
        return s;
    }

    let mut board = Board::new(s, num_rows);
    board.solve();
    board.as_zigzag()
}

pub fn convert2(s: String, num_rows: i32) -> String {
    if s.len() == 1 || num_rows == 1 {
        return s;
    }

    let mut board = Board::new(s, num_rows);
    board.solve();
    board.as_zigzag()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let actual = convert("A".to_string(), 1);
        assert_eq!(actual, "A");

        let actual = convert("PAYPALISHIRING".to_string(), 3);
        assert_eq!(actual, "PAHNAPLSIIGYIR");

        let actual = convert("PAYPALISHIRING".to_string(), 4);
        assert_eq!(actual, "PINALSIGYAHRPI");

        let actual = convert("ABCD".to_string(), 2);
        assert_eq!(actual, "ACBD");
    }
}

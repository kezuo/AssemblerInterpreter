use std::collections::VecDeque;

struct LimitedQueue<T, const N: usize> {
    queue: VecDeque<T>,
}

impl<T, const N: usize> LimitedQueue<T, N> {
    fn new() -> Self {
        Self {
            queue: VecDeque::<T>::new(),
        }
    }

    fn push(&mut self, v: T) {
        if self.queue.len() == N {
            self.queue.pop_front();
        }
        self.queue.push_back(v);
    }

    fn pop(&mut self) -> Option<T> {
        self.queue.pop_back()
    }
}

#[derive(Debug)]
enum Token<'s> {
    Number(i64),
    String(&'s str),
    Identity(&'s str),
    Comma(),
    Colon(),
}

enum TokenizeState {
    Waiting(),
    InString(usize),
    InNumber(i64, i64),
    InIdentity(usize),
}

impl<'s> Token<'s> {
    fn tokenize(s: &'s str) -> Vec<Token<'s>> {
        let mut ret = Vec::new();
        let mut iter = s.char_indices();
        let mut state = TokenizeState::Waiting();
        let mut iter_queue = LimitedQueue::<_, 3>::new();
        iter_queue.push(iter.clone());
        while let Some((index, c)) = iter.next() {
            match state {
                TokenizeState::Waiting() => match c {
                    '\'' => state = TokenizeState::InString(index + 1),
                    'a'..='z' | 'A'..='Z' | '_' => state = TokenizeState::InIdentity(index),
                    '0'..='9' => state = TokenizeState::InNumber(c.to_digit(10).unwrap() as i64, 1),
                    '-' => state = TokenizeState::InNumber(0, -1),
                    ':' => ret.push(Token::Colon()),
                    ',' => ret.push(Token::Comma()),
                    ' ' => (),
                    _ => panic!(),
                },
                TokenizeState::InString(begin) => match c {
                    '\'' => {
                        ret.push(Token::String(s.get(begin..index).unwrap()));
                        state = TokenizeState::Waiting();
                    }
                    _ => (),
                },
                TokenizeState::InNumber(ref mut num, sign) => match c {
                    '0'..='9' => *num = *num * 10 + c.to_digit(10).unwrap() as i64,
                    _ => {
                        ret.push(Token::Number(*num * sign));
                        state = TokenizeState::Waiting();
                        iter = iter_queue.pop().unwrap();
                    }
                },
                TokenizeState::InIdentity(begin) => match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {}
                    _ => {
                        ret.push(Token::Identity(s.get(begin..index).unwrap()));
                        state = TokenizeState::Waiting();
                        iter = iter_queue.pop().unwrap();
                    }
                },
            };
            iter_queue.push(iter.clone());
        }
        match state {
            TokenizeState::InString(begin) => ret.push(Token::String(s.get(begin..).unwrap())),
            TokenizeState::InNumber(num, sign) => ret.push(Token::Number(num * sign)),
            TokenizeState::InIdentity(begin) => ret.push(Token::Identity(s.get(begin..).unwrap())),
            _ => (),
        }
        ret
    }
}

#[derive(Debug)]
enum MsgArg<'s> {
    String(&'s str),
    Ident(Ident<'s>),
}

#[derive(Debug)]
enum Node<'s> {
    Cmd(Cmd<'s>),
    Msg(Vec<MsgArg<'s>>),
    Tag(&'s str),
}

#[derive(Debug)]
struct Ident<'s> (&'s str);

#[allow(non_camel_case_types)]
#[derive(macros::Parse)]
#[derive(Debug)]
enum Cmd<'s> {
    mov(Ident<'s>, Ident<'s>),
    movi(Ident<'s>, i64),
    inc(Ident<'s>),
    dec(Ident<'s>),
    add(Ident<'s>, Ident<'s>),
    addi(Ident<'s>, i64),
    sub(Ident<'s>, Ident<'s>),
    subi(Ident<'s>, i64),
    mul(Ident<'s>, Ident<'s>),
    div(Ident<'s>, Ident<'s>),
    jmp(Ident<'s>),
    cmp(Ident<'s>, Ident<'s>),
    cmpi(Ident<'s>, i64),
    cmpii(i64, i64),
    cmpi_(i64, Ident<'s>),
    jne(Ident<'s>),
    je(Ident<'s>),
    jge(Ident<'s>),
    jg(Ident<'s>),
    jle(Ident<'s>),
    jl(Ident<'s>),
    call(Ident<'s>),
    ret(),
    end(),
}

impl<'s> Node<'s> {
    fn parse<'t> (tokens: &'t [Token<'s>]) -> (Option<&'t [Token<'s>]>, Vec<Self>) {
        let (left, cmds) = try_parse_cmd(tokens);
        (left, cmds.into_iter().map(|c| Node::Cmd(c)).collect())
    }
}

pub struct AssemblerInterpreter {}

impl AssemblerInterpreter {

    pub fn split(input: &str) -> Vec<&str> {
        let lines = input
            .split('\n')
            .filter_map(|s| {
                s.get(0..(s.chars().position(|c| c == ';')).unwrap_or(s.len()))
            })
            .collect::<Vec<&str>>();
        lines
    }

    pub fn interpret(input: &str) -> Option<String> {
        let lines = Self::split(input);
        let mut nodes = Vec::<Node>::new();
        for line in lines {
            let tokens = Token::tokenize(line);
            if let (None, mut nodes_now) = Node::parse(&tokens) {
                dbg!(&nodes_now);
                nodes.append(&mut nodes_now);
            } else {
                dbg!(&tokens);
            }
        }
        None
    }
}

#[test]
fn test() {
        let input = "\nmov   a, 2            ; value1\nmov   b, 10           ; value2\nmov   c, a            ; temp1\nmov   d, b            ; temp2\ncall  proc_func\ncall  print\nend\n\nproc_func:\n    cmp   d, 1\n    je    continue\n    mul   c, a\n    dec   d\n    call  proc_func\n\ncontinue:\n    ret\n\nprint:\n    msg a, '^', b, ' = ', c\n    ret\n";
    AssemblerInterpreter::interpret(input);
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn print_tokenize(s: &str) {
        println!("{} ==> {:?}", s, Token::tokenize(s));
    }

    #[test]
    fn test_tokenize() {
        print_tokenize("asdf asdfs");
        print_tokenize("100 asdf,asdfs");
        print_tokenize("  100 'sdfas,d;f'  100:,   ");
    }

    #[test]
    fn simple_test() {
        let simple_programs = &[
            "\n; My first program\nmov  a, 5\ninc  a\ncall function\nmsg  '(5+1)/2 = ', a    ; output message\nend\n\nfunction:\n    div  a, 2\n    ret\n",
            "\nmov   a, 5\nmov   b, a\nmov   c, a\ncall  proc_fact\ncall  print\nend\n\nproc_fact:\n    dec   b\n    mul   c, b\n    cmp   b, 1\n    jne   proc_fact\n    ret\n\nprint:\n    msg   a, '! = ', c ; output text\n    ret\n",
            "\nmov   a, 8            ; value\nmov   b, 0            ; next\nmov   c, 0            ; counter\nmov   d, 0            ; first\nmov   e, 1            ; second\ncall  proc_fib\ncall  print\nend\n\nproc_fib:\n    cmp   c, 2\n    jl    func_0\n    mov   b, d\n    add   b, e\n    mov   d, e\n    mov   e, b\n    inc   c\n    cmp   c, a\n    jle   proc_fib\n    ret\n\nfunc_0:\n    mov   b, c\n    inc   c\n    jmp   proc_fib\n\nprint:\n    msg   'Term ', a, ' of Fibonacci series is: ', b        ; output text\n    ret\n",
            "\nmov   a, 11           ; value1\nmov   b, 3            ; value2\ncall  mod_func\nmsg   'mod(', a, ', ', b, ') = ', d        ; output\nend\n\n; Mod function\nmod_func:\n    mov   c, a        ; temp1\n    div   c, b\n    mul   c, b\n    mov   d, a        ; temp2\n    sub   d, c\n    ret\n",
            "\nmov   a, 81         ; value1\nmov   b, 153        ; value2\ncall  init\ncall  proc_gcd\ncall  print\nend\n\nproc_gcd:\n    cmp   c, d\n    jne   loop\n    ret\n\nloop:\n    cmp   c, d\n    jg    a_bigger\n    jmp   b_bigger\n\na_bigger:\n    sub   c, d\n    jmp   proc_gcd\n\nb_bigger:\n    sub   d, c\n    jmp   proc_gcd\n\ninit:\n    cmp   a, 0\n    jl    a_abs\n    cmp   b, 0\n    jl    b_abs\n    mov   c, a            ; temp1\n    mov   d, b            ; temp2\n    ret\n\na_abs:\n    mul   a, -1\n    jmp   init\n\nb_abs:\n    mul   b, -1\n    jmp   init\n\nprint:\n    msg   'gcd(', a, ', ', b, ') = ', c\n    ret\n",
            "\ncall  func1\ncall  print\nend\n\nfunc1:\n    call  func2\n    ret\n\nfunc2:\n    ret\n\nprint:\n    msg 'This program should return null'\n",
            "\nmov   a, 2            ; value1\nmov   b, 10           ; value2\nmov   c, a            ; temp1\nmov   d, b            ; temp2\ncall  proc_func\ncall  print\nend\n\nproc_func:\n    cmp   d, 1\n    je    continue\n    mul   c, a\n    dec   d\n    call  proc_func\n\ncontinue:\n    ret\n\nprint:\n    msg a, '^', b, ' = ', c\n    ret\n"];

        let expected = &[
            Some(String::from("(5+1)/2 = 3")),
            Some(String::from("5! = 120")),
            Some(String::from("Term 8 of Fibonacci series is: 21")),
            Some(String::from("mod(11, 3) = 2")),
            Some(String::from("gcd(81, 153) = 9")),
            None,
            Some(String::from("2^10 = 1024")),
        ];

        for (prg, exp) in simple_programs.iter().zip(expected) {
            let actual = AssemblerInterpreter::interpret(*prg);
            assert_eq!(actual, *exp);
        }
    }
}

fn main() {
}

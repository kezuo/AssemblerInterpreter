use std::collections::{ VecDeque, HashMap };

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
struct Msg<'s>(Vec<MsgArg<'s>>);

#[derive(Debug)]
struct Label<'s>(&'s str);

#[derive(Debug)]
enum Node<'s> {
    Cmd(Cmd<'s>),
    Msg(Msg<'s>),
    Label(Label<'s>),
}

#[derive(Debug)]
struct Ident<'s>(&'s str);

#[allow(non_camel_case_types)]
#[derive(macros::Parse, Debug)]
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
    muli(Ident<'s>, i64),
    div(Ident<'s>, Ident<'s>),
    divi(Ident<'s>, i64),
    jmp(Ident<'s>),
    cmp(Ident<'s>, Ident<'s>),
    cmpi(Ident<'s>, i64),
    cmpii(i64, i64),
    cmpi_(i64, Ident<'s>),
    jne(Ident<'s>),
    je(Ident<'s>),
    jg(Ident<'s>),
    jge(Ident<'s>),
    jl(Ident<'s>),
    jle(Ident<'s>),
    call(Ident<'s>),
    ret(),
    end(),
}

#[derive(Debug)]
enum CmpResult {
    Eq(),
    Greater(),
    Less(),
}

impl<'s> Node<'s> {
    fn try_parse_msg<'t>(tokens: &'t [Token<'s>]) -> (Option<&'t [Token<'s>]>, Option<Msg<'s>>) {
        let mut iter = tokens.iter().enumerate();
        if let Some((_, Token::Identity(s))) = iter.next() {
            if !"msg".starts_with(s) {
                return (Some(tokens), None);
            }
        } else {
            return (Some(tokens), None);
        };
        let mut args = Vec::<MsgArg>::new();
        match iter.next() {
            Some((_, Token::Identity(i))) => args.push(MsgArg::Ident(Ident(*i))),
            Some((_, Token::String(s))) => args.push(MsgArg::String(*s)),
            _ => (),
        }
        loop {
            match iter.next() {
                Some((_, Token::Comma())) => (),
                Some((index, _)) => return (tokens.get(index..), Some(Msg(args))),
                None => return (None, Some(Msg(args))),
            }
            match iter.next() {
                Some((_, Token::Identity(i))) => args.push(MsgArg::Ident(Ident(*i))),
                Some((_, Token::String(s))) => args.push(MsgArg::String(*s)),
                Some((index, _)) => return (tokens.get(index..), Some(Msg(args))),
                _ => panic!(),
            };
        }
    }

    fn try_parse_label<'t>(tokens: &'t [Token<'s>]) -> (Option<&'t [Token<'s>]>, Option<Label<'s>>) {
        let mut iter = tokens.iter().enumerate();
        let name = match iter.next() {
                Some((_, Token::Identity(i))) => *i,
                _ => return (Some(tokens), None),
        };
        match iter.next() {
            Some((_, Token::Colon())) => (),
            _ => return (Some(tokens), None),
        }
        match iter.next() {
            Some((index, _)) => return (tokens.get(index..), Some(Label(name))),
            None => return (None, Some(Label(name))),
        }
    }

    fn parse<'t>(mut tokens: &'t [Token<'s>]) -> (Option<&'t [Token<'s>]>, Vec<Self>) {
        let mut ret = Vec::<_>::new();
        loop {
            let mut matched = false;
            let (left, cmds) = try_parse_cmd(tokens);
            if cmds.len() > 0 {
                matched = true;
                ret.append(&mut cmds.into_iter().map(|c| Node::Cmd(c)).collect::<Vec<_>>());
            }
            if left.is_none() {
                return (None, ret);
            } else {
                tokens = left.unwrap();
            }
            let (left, msg) = Self::try_parse_msg(tokens);
            if let Some(msg) = msg {
                matched = true;
                ret.push(Node::Msg(msg));
            }
            if left.is_none() {
                return (None, ret);
            } else {
                tokens = left.unwrap();
            }
            let (left, label) = Self::try_parse_label(tokens);
            if let Some(label) = label {
                matched = true;
                ret.push(Node::Label(label));
            }
            if left.is_none() {
                return (None, ret);
            } else {
                tokens = left.unwrap();
            }
            if !matched {
                return (Some(tokens), ret);
            }
        }
    }

    fn execute(nodes: Vec<Self>) -> Option<String> {
        let mut jmp_map = HashMap::<&str, _>::new();
        let mut iter = nodes.iter();
        while let Some(node) = iter.next() {
            match node {
                Node::Label(Label(s)) => {
                    match jmp_map.get(*s) {
                        Some(_) => panic!(),
                        None => { jmp_map.insert(*s, iter.clone()); },
                    }
                }
                _ => (),
            }
        }

        let mut ret = String::new();
        let mut cmp_result = Option::<CmpResult>::None;
        let cmp = |x: i64, y: i64| -> CmpResult {
            if x == y {
                CmpResult::Eq()
            } else if x > y {
                CmpResult::Greater()
            } else {
                CmpResult::Less()
            }
        };
        let mut stack = Vec::<_>::new();
        let mut registers = HashMap::<&str, i64>::new();
        let mut iter = nodes.iter();
        while let Some(node) = iter.next() {
            println!("executing {:?}", node);
            match node {
                Node::Cmd(cmd) => {
                    match cmd {
                        Cmd::mov(Ident(d), Ident(s)) => {
                            registers.insert(*d, registers[*s]);
                        }
                        Cmd::movi(Ident(d), n) => {
                            registers.insert(*d, *n);
                        }
                        Cmd::inc(Ident(d)) => {
                            registers.insert(*d, registers[*d] + 1);
                        }
                        Cmd::dec(Ident(d)) => {
                            registers.insert(*d, registers[*d] - 1);
                        }
                        Cmd::add(Ident(d), Ident(s)) => {
                            registers.insert(*d, registers[*d] + registers[*s]);
                        }
                        Cmd::addi(Ident(d), n) => {
                            registers.insert(*d, registers[*d] + n);
                        }
                        Cmd::sub(Ident(d), Ident(s)) => {
                            registers.insert(*d, registers[*d] - registers[*s]);
                        }
                        Cmd::subi(Ident(d), n) => {
                            registers.insert(*d, registers[*d] - n);
                        }
                        Cmd::mul(Ident(d), Ident(s)) => {
                            registers.insert(*d, registers[*d] * registers[*s]);
                        }
                        Cmd::muli(Ident(d), n) => {
                            registers.insert(*d, registers[*d] * n);
                        }
                        Cmd::div(Ident(d), Ident(s)) => {
                            registers.insert(*d, registers[*d] / registers[*s]);
                        }
                        Cmd::divi(Ident(d), n) => {
                            registers.insert(*d, registers[*d] / n);
                        }
                        Cmd::jmp(Ident(s)) => {
                            iter = jmp_map[*s].clone();
                        }
                        Cmd::cmp(Ident(x), Ident(y)) => {
                            cmp_result = Some(cmp(registers[*x], registers[*y]));
                        }
                        Cmd::cmpi(Ident(x), y) => {
                            cmp_result = Some(cmp(registers[*x], *y));
                        }
                        Cmd::cmpii(x, y) => {
                            cmp_result = Some(cmp(*x, *y));
                        }
                        Cmd::cmpi_(x, Ident(y)) => {
                            cmp_result = Some(cmp(*x, registers[*y]));
                        }
                        Cmd::jne(Ident(s)) => {
                            match cmp_result {
                                None => panic!(),
                                Some(CmpResult::Greater()) | Some(CmpResult::Less()) => iter = jmp_map[*s].clone(),
                                _ => (),
                            }
                        }
                        Cmd::je(Ident(s)) => {
                            match cmp_result {
                                None => panic!(),
                                Some(CmpResult::Eq()) => iter = jmp_map[*s].clone(),
                                _ => (),
                            }
                        },
                        Cmd::jge(Ident(s)) => {
                            match cmp_result {
                                None => panic!(),
                                Some(CmpResult::Greater() | CmpResult::Eq()) => iter = jmp_map[*s].clone(),
                                _ => (),
                            }
                        }
                        Cmd::jg(Ident(s)) => {
                            match cmp_result {
                                None => panic!(),
                                Some(CmpResult::Greater()) => iter = jmp_map[*s].clone(),
                                _ => (),
                            }
                        }
                        Cmd::jle(Ident(s)) => {
                            match cmp_result {
                                None => panic!(),
                                Some(CmpResult::Less() | CmpResult::Eq()) => iter = jmp_map[*s].clone(),
                                _ => (),
                            }
                        }
                        Cmd::jl(Ident(s)) => {
                            match cmp_result {
                                None => panic!(),
                                Some(CmpResult::Less()) => iter = jmp_map[*s].clone(),
                                _ => (),
                            }
                        },
                        Cmd::call(Ident(s)) => {
                            stack.push(iter.clone());
                            iter = jmp_map[*s].clone();
                        },
                        Cmd::ret() => {
                            iter = stack.pop().unwrap();
                        },
                        Cmd::end() => {
                            return Some(ret);
                        },
                    }
                }
                Node::Msg(msg) => {
                    for arg in msg.0.iter() {
                        match arg {
                            MsgArg::String(s) => ret.push_str(*s),
                            MsgArg::Ident(Ident(i)) => ret.push_str(&registers.get(*i).unwrap().to_string()),
                        }
                    }
                },
                Node::Label(_) => (),
            }
        }
        None
    }
}

pub struct AssemblerInterpreter {}

impl AssemblerInterpreter {
    pub fn split(input: &str) -> Vec<&str> {
        let lines = input
            .split('\n')
            .filter_map(|s| s.get(0..(s.chars().position(|c| c == ';')).unwrap_or(s.len())))
            .collect::<Vec<&str>>();
        lines
    }

    pub fn interpret(input: &str) -> Option<String> {
        let lines = Self::split(input);
        let mut nodes = Vec::<Node>::new();
        dbg!(&lines);
        for line in lines {
            let tokens = Token::tokenize(line);
            if let (None, mut nodes_now) = Node::parse(&tokens) {
                println!("{:?}", nodes_now);
                nodes.append(&mut nodes_now);
            } else {
                println!("{:?}", tokens);
            }
        }
        Node::execute(nodes)
    }
}

#[test]
fn test() {
    let input = "\nmov   a, 2            ; value1\nmov   b, 10           ; value2\nmov   c, a            ; temp1\nmov   d, b            ; temp2\ncall  proc_func\ncall  print\nend\n\nproc_func:\n    cmp   d, 1\n    je    continue\n    mul   c, a\n    dec   d\n    call  proc_func\n\ncontinue:\n    ret\n\nprint:\n    msg a, '^', b, ' = ', c\n    ret\n";
    println!("{:?}", AssemblerInterpreter::interpret(input));
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
    let input = "\nmov   a, 2            ; value1\nmov   b, 10           ; value2\nmov   c, a            ; temp1\nmov   d, b            ; temp2\ncall  proc_func\ncall  print\nend\n\nproc_func:\n    cmp   d, 1\n    je    continue\n    mul   c, a\n    dec   d\n    call  proc_func\n\ncontinue:\n    ret\n\nprint:\n    msg a, '^', b, ' = ', c\n    ret\n";
    println!("{:?}", AssemblerInterpreter::interpret(input));
}

//! A simple regular expression engine.

use nom::branch::alt;
use nom::character::complete::{char, none_of, one_of};
use nom::combinator::{all_consuming, flat_map, success};
use nom::error::Error as NomError;
use nom::multi::{fold_many0, many0_count, many1, separated_list1};
use nom::sequence::{delimited, preceded};
use nom::{Finish, IResult, Parser};

/// A regular expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Regex {
    /// Matches the empty string.
    Empty,
    /// Matches any one character.
    Dot,
    /// Matches a literal character.
    Literal(char),
    /// Matches zero or more repetitions.
    Star(Box<Regex>),
    /// Matches two patterns in a row.
    Concat(Box<Regex>, Box<Regex>),
    /// Matches either of two patterns.
    Or(Box<Regex>, Box<Regex>),
    /// Matches one or more repetitions.
    Plus(Box<Regex>),
    /// Matches zero or one times.
    Maybe(Box<Regex>),
    /// Matches the opposite of a pattern.
    Not(Box<Regex>),
    /// Matches the intersection of two patterns.
    And(Box<Regex>, Box<Regex>),
}

/// A regular expression matcher.
pub trait Matcher {
    /// Activate this matcher's *start* state.
    ///
    /// Returns whether this matcher is in an accepting state.
    fn start(&mut self) -> bool;

    /// Process a character from the string we're matching against.
    ///
    /// Returns whether this matcher is in an accepting state.
    fn push(&mut self, c: char) -> bool;

    /// Test if a string matches.
    fn matches(&mut self, text: &str) -> bool {
        text.chars().fold(self.start(), |_, c| self.push(c))
    }
}

impl<T: Matcher + ?Sized> Matcher for Box<T> {
    fn start(&mut self) -> bool {
        self.as_mut().start()
    }

    fn push(&mut self, c: char) -> bool {
        self.as_mut().push(c)
    }
}

/// Extension methods for matchers.
pub trait MatcherExt: Matcher + Sized {
    /// Wrap this matcher with the `*` operator.
    fn star(self) -> Star<Self> {
        Star(self)
    }

    /// Concatenate two matchers.
    fn concat<T: Matcher>(self, other: T) -> Concat<Self, T> {
        Concat::new(self, other)
    }

    /// Alternate two matchers.
    fn or<T: Matcher>(self, other: T) -> Or<Self, T> {
        Or(self, other)
    }

    /// Invert a matcher.
    fn not(self) -> Not<Self> {
        Not(self)
    }

    /// Intersect two matchers.
    fn and<T: Matcher>(self, other: T) -> And<Self, T> {
        And(self, other)
    }
}

impl<T: Matcher> MatcherExt for T {}

/// An error parsing a regular expression.
pub type ParseError<'a> = NomError<&'a str>;

/// Helper for applying binary operators.
fn reduce<T>(v: Vec<T>, f: impl FnMut(T, T) -> T) -> T {
    v.into_iter().reduce(f).unwrap()
}

/// Parser implementation.
fn regex(pattern: &str) -> IResult<&str, Regex> {
    // Empty : ``
    let empty = success(Regex::Empty);

    // Group : `(` Regex `)`
    let group = delimited(char('('), regex, char(')'));

    // Dot : `.`
    let dot = char('.').map(|_| Regex::Dot);

    // Meta : `\` | `(` | `)` | ...
    let meta = r"\().*|+?!&";

    // Literal : [^Meta]
    let literal = none_of(meta).map(Regex::Literal);

    // Escape : `\` Meta
    let escape = preceded(char('\\'), one_of(meta))
        .map(Regex::Literal);

    // Atom : Literal | Escape | Dot | Group
    let atom = alt((literal, escape, dot, group));

    // Repeat : Atom
    //        | Repeat `*`
    //        | Repeat `+`
    //        | Repeat `?`
    let repeat = flat_map(atom, |r| fold_many0(
        one_of("*+?"),
        move || r.clone(),
        |r, c| match c {
            '*' => r.star(),
            '+' => r.plus(),
            '?' => r.maybe(),
            _ => unreachable!(),
        },
    ));

    // Word : Repeat | Word Repeat
    let word = many1(repeat)
        .map(|v| reduce(v, Regex::concat));

    // NotWord : Word
    //         | `!` NotWord
    let not_word = many0_count(char('!'))
        .and(word)
        .map(|(n, r)| (0..n).fold(r, |r, _| r.not()));

    // Chunk : NotWord | Empty
    let chunk = alt((not_word, empty));

    // Clause : Chunk
    //        | Alternate `&` Chunk
    let clause = separated_list1(char('&'), chunk)
        .map(|v| reduce(v, Regex::and));

    // Regex : Clause
    //       | Regex `|` Clause
    separated_list1(char('|'), clause)
        .map(|v| reduce(v, Regex::or))
        .parse(pattern)
}

impl Regex {
    /// Parse a regular expression.
    pub fn parse(pattern: &str) -> Result<Self, ParseError<'_>> {
        all_consuming(regex)
            .parse(pattern)
            .finish()
            .map(|(_, r)| r)
    }

    /// Compile a regular expression.
    pub fn matcher(&self) -> Box<dyn Matcher> {
        match self {
            Self::Empty => Box::new(
                Empty::default()
            ),
            Self::Dot => Box::new(
                Dot::default()
            ),
            Self::Literal(c) => Box::new(
                Literal::new(*c)
            ),
            Self::Star(r) => Box::new(
                r.matcher().star()
            ),
            Self::Concat(a, b) => Box::new(
                a.matcher().concat(b.matcher())
            ),
            Self::Or(a, b) => Box::new(
                a.matcher().or(b.matcher())
            ),
            Self::Plus(r) => Box::new(
                r.matcher().concat(r.matcher().star())
            ),
            Self::Maybe(r) => Box::new(
                r.matcher().or(Empty::default())
            ),
            Self::Not(r) => Box::new(
                r.matcher().not()
            ),
            Self::And(a, b) => Box::new(
                a.matcher().and(b.matcher())
            ),
        }
    }

    /// Test if a string matches.
    pub fn matches(&self, text: &str) -> bool {
        self.matcher().matches(text)
    }

    /// Wrap this regex with the `*` operator.
    pub fn star(self) -> Self {
        Self::Star(self.into())
    }

    /// Concatenate two regexes.
    pub fn concat(self, other: Self) -> Self {
        Self::Concat(self.into(), other.into())
    }

    /// Alternate two regexes.
    pub fn or(self, other: Self) -> Self {
        Self::Or(self.into(), other.into())
    }

    /// Wrap this regex with the `+` operator.
    pub fn plus(self) -> Self {
        Self::Plus(self.into())
    }

    /// Wrap this regex with the `?` operator.
    pub fn maybe(self) -> Self {
        Self::Maybe(self.into())
    }

    /// Invert a regex.
    pub fn not(self) -> Self {
        Self::Not(self.into())
    }

    /// Intersect two regexes.
    pub fn and(self, other: Self) -> Self {
        Self::And(self.into(), other.into())
    }
}

/// Matcher for Regex::Empty.
#[derive(Debug, Default)]
pub struct Empty {
    matched: bool,
}

impl Matcher for Empty {
    fn start(&mut self) -> bool {
        self.matched = true;
        true
    }

    fn push(&mut self, _: char) -> bool {
        self.matched = false;
        false
    }
}

/// Matcher for Regex::Dot.
#[derive(Debug, Default)]
pub struct Dot {
    started: bool,
    matched: bool,
}

impl Matcher for Dot {
    fn start(&mut self) -> bool {
        self.started = true;
        self.matched
    }

    fn push(&mut self, _: char) -> bool {
        self.matched = self.started;
        self.started = false;
        self.matched
    }
}

/// Matcher for Regex::Literal.
#[derive(Debug)]
pub struct Literal {
    c: char,
    started: bool,
    matched: bool,
}

impl Literal {
    /// Create a literal matcher.
    fn new(c: char) -> Self {
        Self {
            c,
            started: false,
            matched: false,
        }
    }
}

impl Matcher for Literal {
    fn start(&mut self) -> bool {
        self.started = true;
        self.matched
    }

    fn push(&mut self, c: char) -> bool {
        self.matched = self.started && c == self.c;
        self.started = false;
        self.matched
    }
}

/// Matcher for Regex::Star.
#[derive(Debug)]
pub struct Star<T>(T);

impl<T: Matcher> Matcher for Star<T> {
    fn start(&mut self) -> bool {
        self.0.start() || true
    }

    fn push(&mut self, c: char) -> bool {
        self.0.push(c) && self.start()
    }
}

/// Matcher for Regex::Concat.
#[derive(Debug)]
pub struct Concat<L, R> {
    left: L,
    right: R,
    right_started: bool,
}

impl<L: Matcher, R: Matcher> Concat<L, R> {
    fn new(left: L, right: R) -> Self {
        Self {
            left,
            right,
            right_started: false,
        }
    }

    fn start_right(&mut self) -> bool {
        self.right_started = true;
        self.right.start()
    }
}

impl<L: Matcher, R: Matcher> Matcher for Concat<L, R> {
    fn start(&mut self) -> bool {
        self.left.start() && self.start_right()
    }

    fn push(&mut self, c: char) -> bool {
        (self.right_started && self.right.push(c))
            | (self.left.push(c) && self.start_right())
    }
}

/// Matcher for Regex::Or.
#[derive(Debug)]
pub struct Or<T, U>(T, U);

impl<T: Matcher, U: Matcher> Matcher for Or<T, U> {
    fn start(&mut self) -> bool {
        self.0.start() | self.1.start()
    }

    fn push(&mut self, c: char) -> bool {
        self.0.push(c) | self.1.push(c)
    }
}

/// Matcher for Regex::Not.
#[derive(Debug)]
pub struct Not<T>(T);

impl<T: Matcher> Matcher for Not<T> {
    fn start(&mut self) -> bool {
        !self.0.start()
    }

    fn push(&mut self, c: char) -> bool {
        !self.0.push(c)
    }
}

/// Intersection matcher.
#[derive(Debug)]
pub struct And<T, U>(T, U);

impl<T: Matcher, U: Matcher> Matcher for And<T, U> {
    fn start(&mut self) -> bool {
        self.0.start() & self.1.start()
    }

    fn push(&mut self, c: char) -> bool {
        self.0.push(c) & self.1.push(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse a regex or panic.
    fn parse(pattern: &str) -> Regex {
        Regex::parse(pattern)
            .expect(&format!("Pattern '{pattern}' should parse successfully"))
    }

    /// Check the result of a successful parse.
    fn assert_parse(pattern: &str, regex: Regex) {
        assert_eq!(parse(pattern), regex, "'{pattern}'");
    }

    /// Check that a parse failed.
    fn assert_parse_err(pattern: &str) {
        Regex::parse(pattern)
            .expect_err(&format!("Pattern '{pattern}' should fail to parse"));
    }

    #[test]
    fn parsing() {
        assert_parse(r"", Regex::Empty);
        assert_parse(r"()", Regex::Empty);
        assert_parse(r"(())", Regex::Empty);

        assert_parse(r".", Regex::Dot);
        assert_parse(r"(.)", Regex::Dot);

        assert_parse(r"a", Regex::Literal('a'));
        assert_parse(r"\\", Regex::Literal('\\'));
        assert_parse(r"\(", Regex::Literal('('));
        assert_parse(r"\)", Regex::Literal(')'));
        assert_parse(r"\.", Regex::Literal('.'));
        assert_parse(r"\*", Regex::Literal('*'));
        assert_parse(r"\+", Regex::Literal('+'));
        assert_parse(r"\?", Regex::Literal('?'));
        assert_parse(r"\|", Regex::Literal('|'));
        assert_parse(r"\!", Regex::Literal('!'));
        assert_parse(r"\&", Regex::Literal('&'));

        assert_parse(r"()*", Regex::Empty.star());
        assert_parse(r".*", Regex::Dot.star());
        assert_parse(r"a*", Regex::Literal('a').star());
        assert_parse(r"\**", Regex::Literal('*').star());
        assert_parse(r".**", Regex::Dot.star().star());

        let a = || Regex::Literal('a');
        let b = || Regex::Literal('b');

        assert_parse("ab", a().concat(b()));
        assert_parse("ba", b().concat(a()));
        assert_parse("a*b", a().star().concat(b()));
        assert_parse("ab*", a().concat(b().star()));
        assert_parse("(ab)*", a().concat(b()).star());

        assert_parse("a|b", a().or(b()));
        assert_parse("a|b*", a().or(b().star()));
        assert_parse("ab|b", a().concat(b()).or(b()));
        assert_parse("(ab|b)*", a().concat(b()).or(b()).star());

        assert_parse("a+", a().plus());
        assert_parse("a+b*", a().plus().concat(b().star()));
        assert_parse("a+*", a().plus().star());
        assert_parse("a*+", a().star().plus());

        assert_parse("a?", a().maybe());

        assert_parse("!a", a().not());
        assert_parse("!.*", Regex::Dot.star().not());
        assert_parse("!a|b", a().not().or(b()));

        assert_parse("a&b", a().and(b()));
        assert_parse("a&!b|b", a().and(b().not()).or(b()));
        assert_parse("a&(!b|b)", a().and(b().not().or(b())));

        assert_parse_err(r"\");
        assert_parse_err(r"(");
        assert_parse_err(r")");
    }

    /// Check whether a regex matches a set of strings.
    fn assert_matches(pattern: &str, matches: &[&str], non_matches: &[&str]) {
        let regex = parse(pattern);

        for text in matches {
            assert!(regex.matches(text), "'{pattern}' should match '{text}'");
        }

        for text in non_matches {
            assert!(!regex.matches(text), "'{pattern}' should not match '{text}'");
        }
    }

    #[test]
    fn matching() {
        assert_matches(
            r"",
            &[""],
            &["a", "b", "ab"],
        );

        assert_matches(
            r".",
            &["a", "b"],
            &["", "ab", "abc"],
        );

        assert_matches(
            r"a",
            &["a"],
            &["", "b", "ab"],
        );

        assert_matches(
            r"()*",
            &[""],
            &["a", "b", "ab"],
        );

        assert_matches(
            r".*",
            &["", "a", "b", "ab", "abc", "abcd"],
            &[],
        );

        assert_matches(
            r"a*",
            &["", "a", "aa", "aaa"],
            &["b", "ab", "ba", "aab", "aba", "baa"],
        );

        assert_matches(
            r"a**",
            &["", "a", "aa", "aaa"],
            &["b", "ab", "ba", "aab", "aba", "baa"],
        );

        assert_matches(
            r"a.c",
            &["aac", "abc", "acc", "adc"],
            &["", "a", "c", "ac", "aab", "bbc"],
        );

        assert_matches(
            r"ab",
            &["ab"],
            &["", "a", "b", "ba", "abc"],
        );

        assert_matches(
            r"a*b",
            &["b", "ab", "aab"],
            &["", "a", "ac", "abc"],
        );

        assert_matches(
            r"ab*",
            &["a", "ab", "abb"],
            &["", "b", "ac", "abc"],
        );

        assert_matches(
            r"a|b",
            &["a", "b"],
            &["", "aa", "ab", "bb"],
        );

        assert_matches(
            r"(ab*|c)*",
            &["", "a", "ab", "c", "abc", "abbacca"],
            &["b", "acb"],
        );

        assert_matches(
            r"a+",
            &["a", "aa", "aaa"],
            &["", "ab", "ba"],
        );

        assert_matches(
            r"a?",
            &["", "a"],
            &["b", "aa"],
        );

        assert_matches(
            r"!a",
            &["", "b", "aa", "abc"],
            &["a"],
        );

        assert_matches(
            r"!.*",
            &[],
            &["", "a", "ab", "abc"],
        );

        assert_matches(
            r"!(!(.*hello.*)|!(.*world.*))",
            &["hello world", "world hello"],
            &["", "hello", "world"],
        );

        assert_matches(
            r"a&a",
            &["a"],
            &["", "b", "ab"],
        );

        assert_matches(
            r"a&b",
            &[],
            &["", "a", "b", "ab"],
        );

        assert_matches(
            r"(.*hello.*)&(.*world.*)",
            &["hello world", "world hello"],
            &["", "hello", "world"],
        );
    }
}

//! A simple regular expression engine.

use nom::branch::alt;
use nom::character::complete::{char, none_of, one_of};
use nom::combinator::{all_consuming, flat_map, success};
use nom::error::Error as NomError;
use nom::multi::fold_many0;
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
}

impl<T: Matcher> MatcherExt for T {}

/// An error parsing a regular expression.
pub type ParseError<'a> = NomError<&'a str>;

/// Parser implementation.
fn regex(pattern: &str) -> IResult<&str, Regex> {
    // Empty : ``
    let empty = success(Regex::Empty);

    // Group : `(` Regex `)`
    let group = delimited(char('('), regex, char(')'));

    // Dot : `.`
    let dot = char('.').map(|_| Regex::Dot);

    // Meta : `\` | `(` | `)` | ...
    let meta = r"\().*";

    // Literal : [^Meta]
    let literal = none_of(meta).map(Regex::Literal);

    // Escape : `\` Meta
    let escape = preceded(char('\\'), one_of(meta))
        .map(Regex::Literal);

    // Atom : Literal | Escape | Dot | Group
    let atom = alt((literal, escape, dot, group));

    // Repeat : Atom
    //        | Repeat `*`
    let repeat = flat_map(atom, |r| fold_many0(
        char('*'),
        move || r.clone(),
        |r, _| r.star(),
    ));

    // Regex : Repeat | Empty
    alt((repeat, empty))
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

        assert_parse(r"()*", Regex::Empty.star());
        assert_parse(r".*", Regex::Dot.star());
        assert_parse(r"a*", Regex::Literal('a').star());
        assert_parse(r"\**", Regex::Literal('*').star());
        assert_parse(r".**", Regex::Dot.star().star());

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
    }
}

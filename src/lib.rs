//! A simple regular expression engine.

use nom::branch::alt;
use nom::character::complete::{char, none_of, one_of};
use nom::combinator::{all_consuming, success};
use nom::error::Error as NomError;
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
    let meta = r"\().";

    // Literal : [^Meta]
    let literal = none_of(meta).map(Regex::Literal);

    // Escape : `\` Meta
    let escape = preceded(char('\\'), one_of(meta))
        .map(Regex::Literal);

    // Regex : Literal | Escape | Dot | Group | Empty
    alt((literal, escape, dot, group, empty))
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
        }
    }

    /// Test if a string matches.
    pub fn matches(&self, text: &str) -> bool {
        self.matcher().matches(text)
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
    }
}

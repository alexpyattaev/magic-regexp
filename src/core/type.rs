use crate::{AsRegex, Condition, Grouping, Result};
use regex::Regex;
use std::fmt::{Display, Write};

/// Represents a regex type. This enum is used to create the smallest regex statement.
/// For example, `Type::Digit` will create the regex `\d`.
///
/// # Examples
/// ```
/// use magic_regexp::{OneOrMore, Type::Digit};
///
/// let input = OneOrMore(Digit);
/// assert_eq!(input.to_string(), r"\d+"); // Note that the regex is wrapped in parentheses.
/// ```
pub enum Type<'a> {
    ///0-9
    Digit,
    ///[^0-9]
    NotDigit,
    WordBoundary,
    NotWordBoundary,
    /// contigous sequence of WordChar
    Word,
    /// contigous sequence of characters not in Word class
    NotWord,
    /// Stuff that you find in words (see UTS18)
    WordChar,
    /// Anything not in WordChar
    NotWordChar,
    /// String literal. Will be escaped if necessary.
    Text(&'a str),
    /// Any character from set provided.
    AnyOf(&'a str),
    /// Any character not in set provided.
    NoneOf(&'a str),
    /// Any character
    Char,
    Whitespace,
    NotWhitespace,
    /// Any unicode letter
    Letter,
    /// Any unicode non-letter
    NotLetter,

    LetterLowercase,
    NotLetterLowercase,

    LetterUppercase,
    NotLetterUppercase,
    Tab,
    NotTab,
    Linefeed,
    NotLinefeed,
    CarriageReturn,
    NotCarriageReturn,
    /// Raw input to be passed to regex crate as-is
    Raw(&'a str),
}

impl AsRegex for Type<'_> {}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Raw(r) => r,

            Type::Digit => r"\d",
            Type::NotDigit => r"\D",

            Type::WordBoundary => r"\b",
            Type::NotWordBoundary => r"\B",

            Type::Word => r"\b\w+\b",
            Type::NotWord => r"W+",

            Type::WordChar => r"\w",
            Type::NotWordChar => r"\W",

            Type::Char => r".",
            Type::Text(text) => {
                if text.len() > 1 {
                    //Wrap text into a non-capturing group so it can be combined with +, * and so on.
                    f.write_str("(?:")?;
                }

                for c in text.chars() {
                    if must_escape_text(c) {
                        f.write_char('\\')?;
                    }
                    f.write_char(c)?;
                }

                if text.len() > 1 {
                    f.write_char(')')?;
                }
                return Ok(());
            }
            //There is a hack possible to enable "not text" but it is silly inefficient
            /*Type::NotText(text) => {
                for c in text.chars() {
                    f.write_str("[^")?;
                    // Escape if necessary
                    if must_escape_in_set(c) {
                        f.write_char('\\')?;
                    }
                    f.write_char(c)?;
                    f.write_char(']')?;
                }
                return Ok(());
            } */
            Type::Whitespace => r"\s",
            Type::NotWhitespace => r"\S",

            Type::Letter => r"\p{Letter}",
            Type::NotLetter => r"\P{Letter}",

            Type::LetterLowercase => r"\p{Lowercase Letter}",
            Type::NotLetterLowercase => r"\P{Lowercase Letter}",

            Type::LetterUppercase => r"\p{Uppercase Letter}",
            Type::NotLetterUppercase => r"\P{Uppercase Letter}",

            Type::Tab => r"\t",
            Type::NotTab => r"^\t",

            Type::Linefeed => r"\n",
            Type::NotLinefeed => r"^\n",

            Type::CarriageReturn => r"\r",
            Type::NotCarriageReturn => r"^\r",

            Type::AnyOf(options) => {
                f.write_char('[')?;
                for c in options.chars() {
                    // Escape if necessary
                    if must_escape_in_set(c) {
                        f.write_char('\\')?;
                    }
                    f.write_char(c)?;
                }
                f.write_char(']')?;
                return Ok(());
            }
            Type::NoneOf(options) => {
                f.write_str("[^")?; // negative set
                for c in options.chars() {
                    // Escape if necessary
                    if must_escape_in_set(c) {
                        f.write_char('\\')?;
                    }
                    f.write_char(c)?;
                }
                f.write_char(']')?; // close set
                return Ok(());
            }
        };
        f.write_str(s)
    }
}

/// Returns the opposite of the given type.
/// For example, `Type::Digit` will return `Type::NotDigit`.
/// Returns the same type if it is not a type that can be negated.
///
/// Panics, if the given type is `Type::Options` and the given string is empty.
///
/// # Examples
/// ```
/// use magic_regexp::{OneOrMore, not, AnyOf};
///
/// let input = OneOrMore(not(not(AnyOf("01"))));
/// assert_eq!(input.to_string(), r"[01]+");
/// ```
pub fn not(t: Type) -> Type {
    match t {
        Type::Digit => Type::NotDigit,
        Type::NotDigit => Type::Digit,

        Type::WordBoundary => Type::NotWordBoundary,
        Type::NotWordBoundary => Type::WordBoundary,

        Type::WordChar => Type::NotWordChar,
        Type::NotWordChar => Type::WordChar,

        Type::Whitespace => Type::NotWhitespace,
        Type::NotWhitespace => Type::Whitespace,

        Type::Letter => Type::NotLetter,
        Type::NotLetter => Type::Letter,

        Type::LetterLowercase => Type::NotLetterLowercase,
        Type::NotLetterLowercase => Type::LetterLowercase,

        Type::LetterUppercase => Type::NotLetterUppercase,
        Type::NotLetterUppercase => Type::LetterUppercase,

        Type::Tab => Type::NotTab,
        Type::NotTab => Type::Tab,

        Type::Linefeed => Type::NotLinefeed,
        Type::NotLinefeed => Type::Linefeed,

        Type::CarriageReturn => Type::NotCarriageReturn,
        Type::NotCarriageReturn => Type::CarriageReturn,

        Type::AnyOf(t) => Type::NoneOf(t),
        Type::NoneOf(t) => Type::AnyOf(t),

        Type::Word => Type::NotWord,
        Type::NotWord => Type::Word,

        Type::Text(_) => unreachable!("Negating Text is not something regex can do"),
        Type::Char => unreachable!("Negating Char makes no sense"),
        Type::Raw(_) => unreachable!("Negating Raw makes no sense"),
    }
}

/// This is a regex input that can be used to match a single character or a group of characters.
/// Can be used to create a regex that matches a single character or a group of characters.
/// For example, `Input::Exactly(Type::Digit)` will match a single digit, and nothing else.
///
/// # Example
/// ```
/// use magic_regexp::{create_reg_exp, Input, Type};
///
/// let regex = create_reg_exp(Input::Exactly(Type::Digit)).unwrap();
/// assert!(regex.is_match("1"));
/// assert!(!regex.is_match("12"));
/// assert!(!regex.is_match("1 2"));
/// ```
///
/// # Example
/// ```
/// use magic_regexp::{create_reg_exp, Input, Type};
///
/// let regex = create_reg_exp(Input::OneOrMore(Type::Digit)).unwrap();
/// assert!(regex.is_match("1"));
/// assert!(regex.is_match("12"));
/// assert!(regex.is_match("1 2"));
/// ```
///
/// # Example
/// ```
/// use magic_regexp::{create_reg_exp, Input, Type};
///
/// let regex = create_reg_exp(Input::Maybe(Type::Digit)).unwrap();
/// assert!(regex.is_match("1"));
/// assert!(regex.is_match(""));
/// assert!(regex.is_match("12"));
/// assert!(regex.is_match("a"));
/// assert!(regex.is_match("1 2"));
/// ```
pub enum Input<'a> {
    /// Matches one or more instance of content
    OneOrMore(Type<'a>),
    /// Matches exactly one instance (and NOTHING else), will anchor at start and end of line. Exactly only really makes sense as outer-most tag.
    Exactly(Type<'a>),
    /// Matches if present, ignores if not
    Maybe(Type<'a>),
    /// Matches exactly this many times
    Times(Type<'a>, usize), //TODO: take range to allow for full feature of this pattern
}

///Test if a char needs to be escaped for text literals
fn must_escape_text(c: char) -> bool {
    const ESCAPE_REPLACE_RE: &str = r".*+?^${}()|[]\/";
    ESCAPE_REPLACE_RE.chars().any(|p| p == c)
}

///Test if a char needs to be escaped in a set definition
fn must_escape_in_set(c: char) -> bool {
    r"\-]".chars().any(|p| p == c)
}

impl<'a> Display for Input<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Input::OneOrMore(t) => format!("{t}+"),
            Input::Exactly(t) => match t {
                _ => format!(r"^{t}$"),
            },
            Input::Maybe(t) => format!("(?:{t})?"),
            Input::Times(t, n) => format!("{t}{{{n}}}"),
        };
        write!(f, "{}", str)
    }
}

impl<'a> std::fmt::Debug for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl<'a> std::fmt::Debug for Input<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl AsRegex for Input<'_> {
    fn as_regex(&self) -> Result<Regex> {
        Ok(Regex::new(&self.to_string())?)
    }
}

impl Condition for Input<'_> {}

impl Condition for Type<'_> {}

/// Returns a Regex, which chains the 2 given regexes with an `and` operator.
///
/// # Example
/// ```
/// use magic_regexp::{create_reg_exp, Condition, Digit, LetterLowercase};
///
/// let regex = create_reg_exp(Digit.or(LetterLowercase)).unwrap();
/// assert!(regex.is_match("1"));
/// assert!(regex.is_match("a"));
/// assert!(!regex.is_match("A"));
/// assert!(regex.is_match("12"));
/// assert!(regex.is_match("1a"));
/// assert!(regex.is_match("1 a"));
/// ```

impl Grouping for Input<'_> {}
impl Grouping for Type<'_> {}

impl AsRegex for Regex {}

impl Condition for Regex {}

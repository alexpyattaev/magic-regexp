use crate::{AsRegex, Condition, Result};
use regex::Regex;

pub enum Type {
    Digit,
    NotDigit,
    WordBoundary,
    NotWordBoundary,
    Word,
    WordChar,
    NotWordChar,
    Text(String),
    Options(String),
    Char,
    Whitespace,
    NotWhitespace,
    Letter,
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
}

impl AsRegex for Type {}
impl ToString for Type {
    fn to_string(&self) -> String {
        let txt;
        match self {
            Type::Digit => r"\d",
            Type::NotDigit => r"\D",
            Type::WordBoundary => r"\b",
            Type::NotWordBoundary => r"\B",
            Type::Word => r"\b\w+\b",
            Type::WordChar => r"\w",
            Type::NotWordChar => r"\W",
            Type::Char => r".",
            Type::Text(text) => text,
            Type::Whitespace => r"\s",
            Type::NotWhitespace => r"\S",
            Type::Letter => r"[a-zA-Z]",
            Type::NotLetter => r"[^a-zA-Z]",
            Type::LetterLowercase => r"[a-z]",
            Type::NotLetterLowercase => r"[^a-z]",
            Type::LetterUppercase => r"[A-Z]",
            Type::NotLetterUppercase => r"[^A-Z]",
            Type::Tab => r"\t",
            Type::NotTab => r"^\t",
            Type::Linefeed => r"\n",
            Type::NotLinefeed => r"^\n",
            Type::CarriageReturn => r"\r",
            Type::NotCarriageReturn => r"^\r",
            Type::Options(options) => {
                txt = format!("[{}]", options);
                txt.as_str()
            }
        }
        .to_string()
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
/// use magic_regexp::{OneOrMore, not, Options};
///
/// let input = OneOrMore(not(not(Options("01".to_string()))));
/// assert_eq!(input.to_string(), r"([01]+)");
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
        Type::Text(t) => Type::Text(format!("^{}", t)),
        Type::Options(t) => {
            if let Some(first) = t.chars().next() {
                let opt = if first == '^' {
                    t[1..].to_string()
                } else {
                    format!("^{}", t)
                };
                Type::Options(opt)
            } else {
                panic!("Invalid options: {}", t);
            }
        }
        _ => t,
    }
}

/// This is a regex input that can be used to match a single character or a group of characters.
/// Can be used to create a regex that matches a single character or a group of characters.
/// For example, `Input::Exactly(Type::Digit)` will match a single digit.
///
/// # Example
/// ```
/// use magic_regexp::{create_reg_exp, Input, Type};
///
/// let regex = create_reg_exp(Input::Exactly(Type::Digit)).unwrap();
/// assert!(regex.is_match("1"));
/// assert!(!regex.is_match("12"));
/// assert!(regex.is_match("1 2"));
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
pub enum Input {
    OneOrMore(Type),
    Exactly(Type),
    Maybe(Type),
}

impl ToString for Input {
    /// Returns a string representation of the input.
    /// For example, `Input::Exactly(Type::Digit)` will return `\d`.
    ///
    /// # Example
    /// ```
    /// use magic_regexp::{Exactly, Digit, AsRegex, create_reg_exp};
    ///
    /// let regex = create_reg_exp(Exactly(Digit)).unwrap();
    /// assert!(regex.is_match("1"));
    /// assert!(!regex.is_match("12"));
    /// ```
    ///
    /// # Example
    /// ```
    /// use magic_regexp::{Input, Type};
    ///
    /// let input = Input::Exactly(Type::Text("abc".into()));
    /// assert_eq!(input.to_string(), "abc");
    /// let input = Input::Exactly(Type::Text(".".to_string()));
    /// assert_eq!(input.to_string(), r"\.");
    /// ```
    ///
    /// # Example
    /// ```
    /// use magic_regexp::{not, OneOrMore, Options};
    /// use regex::Regex;
    ///
    /// let input = OneOrMore(not(Options("01".to_string())));
    /// assert_eq!(input.to_string(), r"([^01]+)");
    /// let re = Regex::new(&input.to_string()).unwrap();
    /// assert_eq!(re.replace("1078910", ""), "1010");
    /// ```
    fn to_string(&self) -> String {
        const ESCAPE_REPLACE_RE: &str = r"[.*+?^${}()|[\\]\\/]";

        match self {
            Input::OneOrMore(t) => format!("({}+)", t.to_string()),
            Input::Exactly(t) => match t {
                Type::Text(t) => Regex::new(ESCAPE_REPLACE_RE)
                    .expect("Invalid replace_all regex")
                    .replace_all(t, r"\$0")
                    .to_string(),
                _ => format!(r"\b{}\b", t.to_string()),
            },
            Input::Maybe(t) => format!("({}?)", t.to_string()),
        }
    }
}

impl AsRegex for Input {
    fn as_regex(&self) -> Result<Regex> {
        Ok(Regex::new(&self.to_string())?)
    }
}

/// Returns a Regex, which chains the 2 given regexes with an `and` operator.
///
/// # Example
/// ```
/// use magic_regexp::{create_reg_exp, Condition, Exactly, Digit, LetterLowercase};
///
/// let regex = create_reg_exp(Exactly(Digit).or(Exactly(LetterLowercase))).unwrap();
/// assert!(regex.is_match("1"));
/// assert!(regex.is_match("a"));
/// assert!(!regex.is_match("A"));
/// assert!(!regex.is_match("12"));
/// assert!(!regex.is_match("1a"));
/// assert!(regex.is_match("1 a"));
/// ```
impl Condition for Input {}

impl AsRegex for Regex {}
impl Condition for Regex {}

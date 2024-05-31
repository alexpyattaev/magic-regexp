use regex::Regex;
use thiserror::Error;

#[derive(Error, Debug)]
/// An error, which can occur while using this crate.
/// Mostly used to wrap errors from the Regex crate.
pub enum Error {
    #[error("An regex error occurred")]
    RegexError(#[from] regex::Error),
}

/// A type, which is used to return results from this crate.
/// Mostly used to wrap results from the Regex crate.
pub type Result<T> = std::result::Result<T, Error>;

/// A trait, which allows to convert something to a regex.
/// Mostly needed to work with this lib and Regex crate.
pub trait AsRegex: ToString {
    /// Returns the regex, which represents the wanted statement.
    fn as_regex(&self) -> Result<Regex> {
        let regex = Regex::new(&self.to_string())?;
        Ok(regex)
    }
}

/// A trait, which allows to chain regex statements with conditions.
/// Import this, if you want to use the `and`, `or` and `optionally` methods and chain statements.
pub trait Condition: AsRegex + Sized {
    /// Returns the regex, which chains the two given statements with an `and` condition.
    fn and(self, other: impl AsRegex) -> Regex {
        Regex::new(&format!("{}{}", self.to_string(), other.to_string()))
            .expect("Invalid regex (and)")
    }
    /// Returns the regex, which chains the two given statements with an `or` condition.
    fn or(self, other: impl AsRegex) -> Regex {
        Regex::new(&format!("{}|{}", self.to_string(), other.to_string()))
            .expect("Invalid regex (or)")
    }
    /// Returns the regex, which sets the given statement to optional.
    fn optionally(self) -> Regex {
        Regex::new(&format!("(?:{})?", self.to_string())).expect("Invalid regex (optionally)")
    }
}

pub trait Grouping: AsRegex {
    /// This defines the entire input so far as a named capture group.
    ///
    /// # Example
    /// ```
    /// use magic_regexp::{create_reg_exp, Condition,Grouping, Digit, LetterLowercase, OneOrMore};
    ///
    /// let regex = create_reg_exp(OneOrMore(Digit).grouped_as("digits")).unwrap();
    /// assert_eq!(&regex.captures("1").unwrap()["digits"], "1");
    /// ```
    fn grouped_as(&self, name: &str) -> Regex {
        Regex::new(&format!(r"(?P<{}>{})", name, self.to_string())).expect("Invalid regex")
    }

    /// This defines the entire input so far as a named capture group.
    /// This is an alias for `grouped_as`.
    fn r#as(&self, name: &str) -> Regex {
        self.grouped_as(name)
    }

    /// This defines the entire input so far as an anonymous group.
    ///
    /// # Example
    /// ```
    /// use magic_regexp::{create_reg_exp, Condition, Grouping, Digit, LetterLowercase, OneOrMore, Type, Char, Whitespace, Maybe};
    /// use regex::Regex;
    ///
    /// let regex = create_reg_exp(OneOrMore(Digit).grouped()
    ///     .and(Whitespace)
    ///     .and(OneOrMore(Char).or(Whitespace).optionally())
    ///     .and(OneOrMore(Digit).grouped())
    /// ).unwrap();
    /// assert_eq!(&regex.captures("1 5").unwrap()[1], "1");
    /// assert_eq!(&regex.captures("134 23").unwrap()[1], "134");
    /// // The folloing example is not really useful, because it shows only the first match.
    /// // The second match is not captured. See the next example for a more useful example.
    /// assert_eq!(&regex.captures("this is the 134 test 213").unwrap()[1], "134");
    /// // This is a bit more complex, because we are using anonymous groups in regex.
    /// let cap = &regex
    ///     .find_iter("multiple numbers 134 2123")
    ///     .filter_map(|digits| digits.as_str().parse().ok())
    ///     .collect::<Vec<String>>()
    ///     .join(" ");
    /// let expected = ["134", "2123"].join(" ");
    /// assert_eq!(cap, &expected);
    /// ```
    ///
    fn grouped(&self) -> Regex {
        Regex::new(&format!(r"({})", self.to_string())).expect("Invalid regex")
    }
}

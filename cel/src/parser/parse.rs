use std::iter::Enumerate;
use std::num::ParseIntError;
use std::str::Chars;

/// Error type of [unescape](unescape).
#[derive(Debug, PartialEq)]
pub enum ParseSequenceError {
    // #[error("invalid escape {escape} at {index} in {string}")]
    InvalidEscape {
        escape: String,
        index: usize,
        string: String,
    },
    // #[error("\\u could not be parsed at {index} in {string}: {source}")]
    InvalidUnicode {
        // #[source]
        source: ParseUnicodeError,
        index: usize,
        string: String,
    },
    MissingOpeningQuote,
    MissingClosingQuote,
}

/// Source error type of [ParseError::InvalidUnicode](ParseError::InvalidUnicode).
#[derive(Debug, PartialEq, Clone)]
pub enum ParseUnicodeError {
    // #[error("could not parse {string} as u32 hex: {source}")]
    Hex {
        // #[source]
        source: ParseIntError,
        string: String,
    },
    Oct {
        // #[source]
        source: ParseIntError,
        string: String,
    },
    // #[error("could not parse {value} as a unicode char")]
    Unicode {
        value: u32,
    },
}

pub fn parse_bytes(s: &str) -> Result<Vec<u8>, ParseSequenceError> {
    // Check for raw bytes prefix
    // Case 1: r' or r" or r''' or r""" (quotes included)
    // Case 2: r followed by non-quote char (for pre-stripped triple quotes from parser)
    let has_raw_prefix_with_quotes = s.starts_with("r'") || s.starts_with("r\"")
                                    || s.starts_with("R'") || s.starts_with("R\"")
                                    || s.starts_with("r'''") || s.starts_with("r\"\"\"")
                                    || s.starts_with("R'''") || s.starts_with("R\"\"\"");

    // Check if it's just 'r' or 'R' followed by content (parser already stripped triple quotes)
    let has_raw_prefix_no_quotes = s.len() > 1 &&
                                   (s.starts_with('r') || s.starts_with('R')) &&
                                   !has_raw_prefix_with_quotes;

    let is_raw = has_raw_prefix_with_quotes || has_raw_prefix_no_quotes;

    // For raw bytes, skip the 'r' or 'R' prefix
    let mut content = if is_raw {
        &s[1..]  // Skip 'r' or 'R'
    } else {
        s
    };

    // Check for triple-quoted bytes (''' or """)
    if content.starts_with("'''") || content.starts_with("\"\"\"") {
        // Check if closing quotes are present (they may have been stripped by parser)
        if content.ends_with("'''") || content.ends_with("\"\"\"") {
            // Strip triple quotes from both ends
            if content.len() < 6 {
                return Ok(Vec::new());
            }
            content = &content[3..content.len() - 3];
        } else {
            // Parser already stripped closing quotes, just strip opening
            content = &content[3..];
        }
    } else if content.starts_with('\'') || content.starts_with('"') {
        // Strip single quotes from both ends
        if content.len() < 2 {
            return Ok(Vec::new());
        }
        content = &content[1..content.len() - 1];
    }

    let mut chars = content.chars().enumerate();
    let mut res: Vec<u8> = Vec::with_capacity(content.len());

    while let Some((idx, c)) = chars.next() {
        // In raw bytes mode, backslashes are literal
        if c == '\\' && !is_raw {
            match chars.next() {
                None => {
                    return Err(ParseSequenceError::InvalidEscape {
                        escape: format!("{c}"),
                        index: idx,
                        string: String::from(content),
                    });
                }
                Some((idx, c2)) => {
                    let byte: u8 = match c2 {
                        // Standard escape sequences
                        'a' => 0x07,  // alert/bell
                        'b' => 0x08,  // backspace
                        'v' => 0x0B,  // vertical tab
                        'f' => 0x0C,  // form feed
                        'n' => b'\n', // newline
                        'r' => b'\r', // carriage return
                        't' => b'\t', // tab
                        '\\' => b'\\', // backslash
                        '?' => b'?',  // question mark
                        '\'' => b'\'', // single quote
                        '"' => b'"',  // double quote
                        '`' => b'`',  // backtick
                        // Hex escape sequences
                        'x' | 'X' => {
                            let escape_seq = format!("\\{}", c2);
                            let hex: String = [
                                chars
                                    .next()
                                    .ok_or(ParseSequenceError::InvalidEscape {
                                        escape: escape_seq.clone(),
                                        index: idx,
                                        string: content.to_string(),
                                    })?
                                    .1,
                                chars
                                    .next()
                                    .ok_or(ParseSequenceError::InvalidEscape {
                                        escape: escape_seq,
                                        index: idx,
                                        string: content.to_string(),
                                    })?
                                    .1,
                            ]
                            .iter()
                            .collect();
                            u8::from_str_radix(&hex, 16).map_err(|_| {
                                ParseSequenceError::InvalidEscape {
                                    escape: hex,
                                    index: idx,
                                    string: content.to_string(),
                                }
                            })?
                        }
                        // Octal escape sequences
                        n if ('0'..='3').contains(&n) => {
                            let octal: String = [
                                n,
                                chars
                                    .next()
                                    .ok_or(ParseSequenceError::InvalidEscape {
                                        escape: format!("\\{n}"),
                                        index: idx,
                                        string: content.to_string(),
                                    })?
                                    .1,
                                chars
                                    .next()
                                    .ok_or(ParseSequenceError::InvalidEscape {
                                        escape: format!("\\{n}"),
                                        index: idx,
                                        string: content.to_string(),
                                    })?
                                    .1,
                            ]
                            .iter()
                            .collect();
                            u8::from_str_radix(&octal, 8).map_err(|_| {
                                ParseSequenceError::InvalidEscape {
                                    escape: octal,
                                    index: idx,
                                    string: content.to_string(),
                                }
                            })?
                        }
                        _ => {
                            return Err(ParseSequenceError::InvalidEscape {
                                escape: format!("{c}{c2}"),
                                index: idx,
                                string: String::from(content),
                            });
                        }
                    };

                    res.push(byte);
                    continue;
                }
            };
        }
        let size = c.len_utf8();
        let mut buffer = [0; 4];
        c.encode_utf8(&mut buffer);
        res.extend_from_slice(&buffer[..size]);
    }
    Ok(res)
}

/// Parse the provided quoted string.
/// This function was adopted from [snailquote](https://docs.rs/snailquote/latest/snailquote/).
///
/// # Details
///
/// Parses a single or double quoted string and interprets escape sequences such as
/// '\n', '\r', '\'', etc.
///
/// Supports raw strings prefixed with `r` or `R` in which case all escape sequences are ignored.///
///
/// The full set of supported escapes between quotes may be found below:
///
/// | Escape     | Code       | Description                              |
/// |------------|------------|------------------------------------------|
/// | \a         | 0x07       | Bell                                     |
/// | \b         | 0x08       | Backspace                                |
/// | \v         | 0x0B       | Vertical tab                             |
/// | \f         | 0x0C       | Form feed                                |
/// | \n         | 0x0A       | Newline                                  |
/// | \r         | 0x0D       | Carriage return                          |
/// | \t         | 0x09       | Tab                                      |
/// | \\         | 0x5C       | Backslash                                |
/// | \?         | 0x??       | Question mark                            |
/// | \"         | 0x22       | Double quote                             |
/// | \'         | 0x27       | Single quote                             |
/// | \`         | 0x60       | Backtick                                 |
/// | \xDD       | 0xDD       | Unicode character with hex code DD       |
/// | \uDDDD     | 0xDDDD     | Unicode character with hex code DDDD     |
/// | \UDDDDDDDD | 0xDDDDDDDD | Unicode character with hex code DDDDDDDD |
/// | \DDD       | 0DDD       | Unicode character with octal code DDD    |
///
/// # Errors
///
/// The returned result can display a human readable error if the string cannot be parsed as a
/// valid quoted string.
pub fn parse_string(s: &str) -> Result<String, ParseSequenceError> {
    let mut chars = s.chars().enumerate();
    let res = String::with_capacity(s.len());

    // Check for raw string prefix
    if s.starts_with('r') || s.starts_with('R') {
        chars.next(); // consume the 'r' or 'R'
        return parse_raw_string(&mut chars, res);
    }

    // Check for triple-quoted strings
    if s.starts_with("'''") || s.starts_with("\"\"\"") {
        let delimiter = if s.starts_with("'''") { "'''" } else { "\"\"\"" };
        return parse_triple_quoted_string(s, delimiter);
    }

    // Single-quoted strings
    match chars.next() {
        Some((_, c)) if c == '\'' || c == '"' => parse_quoted_string(s, &mut chars, res, c),
        _ => Err(ParseSequenceError::MissingOpeningQuote),
    }
}

fn parse_raw_string(
    chars: &mut Enumerate<Chars>,
    mut res: String,
) -> Result<String, ParseSequenceError> {
    let mut in_single_quotes = false;
    let mut in_double_quotes = false;

    while let Some((_, c)) = chars.next() {
        let in_quotes = in_single_quotes || in_double_quotes;

        if c == '\\' && in_quotes {
            match chars.next() {
                Some((_, c2)) => {
                    match c2 {
                        '"' => {
                            if in_single_quotes {
                                res.push(c);
                            }
                        }
                        '\'' => {
                            if in_double_quotes {
                                res.push(c);
                            }
                        }
                        _ => {
                            res.push(c);
                        }
                    };
                    res.push(c2);
                    continue;
                }
                _ => {
                    res.push(c);
                    continue;
                }
            };
        } else if c == '\'' {
            if in_double_quotes {
                res.push(c);
                continue;
            }

            in_single_quotes = !in_single_quotes;
            continue;
        } else if c == '"' {
            if in_single_quotes {
                res.push(c);
                continue;
            }

            in_double_quotes = !in_double_quotes;
            continue;
        } else if !in_quotes {
            return Err(ParseSequenceError::MissingOpeningQuote);
        }

        res.push(c);
    }

    Ok(res)
}

fn parse_quoted_string(
    s: &str,
    mut chars: &mut Enumerate<Chars>,
    mut res: String,
    quote: char,
) -> Result<String, ParseSequenceError> {
    let mut in_single_quotes = quote == '\'';
    let mut in_double_quotes = quote == '"';

    while let Some((idx, c)) = chars.next() {
        let in_quotes = in_single_quotes || in_double_quotes;

        if c == '\\' && in_quotes {
            match chars.next() {
                None => {
                    return Err(ParseSequenceError::InvalidEscape {
                        escape: format!("{c}"),
                        index: idx,
                        string: String::from(s),
                    });
                }
                Some((idx, c2)) => {
                    let mut push_escape_character = false;

                    let value = match c2 {
                        'a' => '\u{07}',
                        'b' => '\u{08}',
                        'v' => '\u{0B}',
                        'f' => '\u{0C}',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => c2,
                        '?' => c2,
                        '\'' => {
                            // In double quotes, \' doesn't need escaping, so don't push backslash
                            push_escape_character = false;
                            c2
                        }
                        '"' => {
                            // In single quotes, \" doesn't need escaping, so don't push backslash
                            push_escape_character = false;
                            c2
                        }
                        '`' => c2,
                        'x' | 'X' | 'u' | 'U' => {
                            let length = match c2 {
                                'x' | 'X' => 2,
                                'u' => 4,
                                'U' => 8,
                                _ => unreachable!(),
                            };

                            parse_unicode_hex(length, &mut chars).map_err(|x| {
                                ParseSequenceError::InvalidUnicode {
                                    source: x.clone(),
                                    index: idx,
                                    string: String::from(s),
                                }
                            })?
                        }
                        n if ('0'..='3').contains(&n) => parse_unicode_oct(&n, &mut chars)
                            .map_err(|x| ParseSequenceError::InvalidUnicode {
                                source: x.clone(),
                                index: idx,
                                string: String::from(s),
                            })?,
                        _ => {
                            return Err(ParseSequenceError::InvalidEscape {
                                escape: format!("{c}{c2}"),
                                index: idx,
                                string: String::from(s),
                            });
                        }
                    };

                    if push_escape_character {
                        res.push(c);
                    }

                    res.push(value);

                    continue;
                }
            };
        } else if c == '\'' {
            if in_double_quotes {
                res.push(c);
                continue;
            }

            in_single_quotes = !in_single_quotes;
            continue;
        } else if c == '"' {
            if in_single_quotes {
                res.push(c);
                continue;
            }

            in_double_quotes = !in_double_quotes;
            continue;
        } else if !in_quotes {
            return Err(ParseSequenceError::MissingOpeningQuote);
        }

        res.push(c);
    }

    // Ensure string has a closing quote
    if in_single_quotes || in_double_quotes {
        return Err(ParseSequenceError::MissingClosingQuote);
    }

    Ok(res)
}

fn parse_triple_quoted_string(s: &str, delimiter: &str) -> Result<String, ParseSequenceError> {
    // Verify the string starts and ends with the delimiter
    if !s.starts_with(delimiter) || !s.ends_with(delimiter) || s.len() < 2 * delimiter.len() {
        return Err(ParseSequenceError::MissingClosingQuote);
    }

    // Extract the content between delimiters
    let content = &s[delimiter.len()..s.len() - delimiter.len()];
    let mut result = String::with_capacity(content.len());
    let mut chars = content.chars().enumerate();

    // Process the content character by character
    while let Some((idx, c)) = chars.next() {
        if c == '\\' {
            // Handle escape sequences
            if let Some((_, c2)) = chars.next() {
                match c2 {
                    // Standard escape sequences
                    'a' => result.push('\x07'),
                    'b' => result.push('\x08'),
                    'f' => result.push('\x0c'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    'v' => result.push('\x0b'),
                    '\\' => result.push('\\'),
                    '\'' => result.push('\''),
                    '"' => result.push('"'),
                    '?' => result.push('?'),
                    '`' => result.push('`'),
                    // Hex escapes (\x or \X)
                    'x' | 'X' => {
                        let ch = parse_unicode_hex(2, &mut chars).map_err(|e| {
                            ParseSequenceError::InvalidUnicode {
                                source: e.clone(),
                                index: idx,
                                string: String::from(content),
                            }
                        })?;
                        result.push(ch);
                    }
                    // Unicode escapes
                    'u' => {
                        let ch = parse_unicode_hex(4, &mut chars).map_err(|e| {
                            ParseSequenceError::InvalidUnicode {
                                source: e.clone(),
                                index: idx,
                                string: String::from(content),
                            }
                        })?;
                        result.push(ch);
                    }
                    'U' => {
                        let ch = parse_unicode_hex(8, &mut chars).map_err(|e| {
                            ParseSequenceError::InvalidUnicode {
                                source: e.clone(),
                                index: idx,
                                string: String::from(content),
                            }
                        })?;
                        result.push(ch);
                    }
                    // Octal escapes
                    '0'..='7' => {
                        let mut octal_val = c2.to_digit(8).unwrap();
                        // Try to read up to 2 more octal digits
                        for _ in 0..2 {
                            if let Some((_, next_c)) = chars.clone().next() {
                                if let Some(digit) = next_c.to_digit(8) {
                                    octal_val = octal_val * 8 + digit;
                                    chars.next();
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        if octal_val > 255 {
                            return Err(ParseSequenceError::InvalidEscape {
                                escape: format!("\\{}", c2),
                                index: idx,
                                string: String::from(content),
                            });
                        }
                        result.push(octal_val as u8 as char);
                    }
                    _ => {
                        return Err(ParseSequenceError::InvalidEscape {
                            escape: format!("\\{}", c2),
                            index: idx,
                            string: String::from(content),
                        });
                    }
                }
            }
        } else {
            // Regular character (including newlines, which are allowed in triple-quoted strings)
            result.push(c);
        }
    }

    Ok(result)
}

fn parse_unicode_hex<I>(length: usize, chars: &mut I) -> Result<char, ParseUnicodeError>
where
    I: Iterator<Item = (usize, char)>,
{
    let unicode_seq: String = chars.take(length).map(|(_, c)| c).collect();

    u32::from_str_radix(&unicode_seq, 16)
        .map_err(|e| ParseUnicodeError::Hex {
            source: e,
            string: unicode_seq,
        })
        .and_then(|u| char::from_u32(u).ok_or(ParseUnicodeError::Unicode { value: u }))
}

fn parse_unicode_oct<I>(first_char: &char, chars: &mut I) -> Result<char, ParseUnicodeError>
where
    I: Iterator<Item = (usize, char)>,
{
    let mut unicode_seq: String = String::with_capacity(3);
    unicode_seq.push(*first_char);
    chars.take(2).for_each(|(_, c)| unicode_seq.push(c));

    u32::from_str_radix(&unicode_seq, 8)
        .map_err(|e| ParseUnicodeError::Oct {
            source: e,
            string: unicode_seq,
        })
        .and_then(|u| {
            if u <= 255 {
                char::from_u32(u).ok_or(ParseUnicodeError::Unicode { value: u })
            } else {
                Err(ParseUnicodeError::Unicode { value: u })
            }
        })
}

#[cfg(test)]
mod tests {
    use super::{parse_bytes, parse_string, ParseSequenceError};

    #[test]
    fn single_quotes_interprets_escapes() {
        let tests: Vec<(&str, Result<String, ParseSequenceError>)> = vec![
            ("'Hello \\a'", Ok(String::from("Hello \u{07}"))),
            ("'Hello \\b'", Ok(String::from("Hello \u{08}"))),
            ("'Hello \\v'", Ok(String::from("Hello \u{0b}"))),
            ("'Hello \\f'", Ok(String::from("Hello \u{0c}"))),
            ("'Hello \\n'", Ok(String::from("Hello \u{0a}"))),
            ("'Hello \\r'", Ok(String::from("Hello \u{0d}"))),
            ("'Hello \\t'", Ok(String::from("Hello \u{09}"))),
            ("'Hello \\\\'", Ok(String::from("Hello \\"))),
            ("'Hello \\?'", Ok(String::from("Hello ?"))),
            ("'Hello \"'", Ok(String::from("Hello \""))),
            ("'Hello \\''", Ok(String::from("Hello '"))),
            ("'Hello \\`'", Ok(String::from("Hello `"))),
            ("'Hello \\x20'", Ok(String::from("Hello  "))),
            ("'Hello \\u270c'", Ok(String::from("Hello ✌"))),
            ("'Hello \\U0001f431'", Ok(String::from("Hello 🐱"))),
            ("'Hello \\040'", Ok(String::from("Hello  "))),
            (
                "Missing closing quote'",
                Err(ParseSequenceError::MissingOpeningQuote),
            ),
            (
                "'Missing closing quote",
                Err(ParseSequenceError::MissingClosingQuote),
            ),
            // Testing octal value is out of range
            (
                "'\\440'",
                Err(ParseSequenceError::InvalidEscape {
                    escape: String::from("\\4"),
                    index: 2,
                    string: String::from("'\\440'"),
                }),
            ),
        ];

        for (s, expected) in tests {
            let result = parse_string(s);
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn double_quotes_interprets_escapes() {
        let tests: Vec<(&str, Result<String, ParseSequenceError>)> = vec![
            ("\"Hello \\a\"", Ok(String::from("Hello \u{07}"))),
            ("\"Hello \\b\"", Ok(String::from("Hello \u{08}"))),
            ("\"Hello \\v\"", Ok(String::from("Hello \u{0b}"))),
            ("\"Hello \\f\"", Ok(String::from("Hello \u{0c}"))),
            ("\"Hello \\n\"", Ok(String::from("Hello \u{0a}"))),
            ("\"Hello \\r\"", Ok(String::from("Hello \u{0d}"))),
            ("\"Hello \\t\"", Ok(String::from("Hello \u{09}"))),
            ("\"Hello \\\\\"", Ok(String::from("Hello \\"))),
            ("\"Hello \\?\"", Ok(String::from("Hello ?"))),
            ("\"Hello \\\"\"", Ok(String::from("Hello \""))),
            ("\"Hello \\'\"", Ok(String::from("Hello '"))),
            ("\"Hello \\`\"", Ok(String::from("Hello `"))),
            ("\"Hello \\x20 \"", Ok(String::from("Hello   "))),
            ("\"Hello \\x60\"", Ok(String::from("Hello `"))),
            ("\"Hello \\u270c\"", Ok(String::from("Hello ✌"))),
            ("\"Hello \\U0001f431\"", Ok(String::from("Hello 🐱"))),
            ("\"Hello \\040\"", Ok(String::from("Hello  "))),
            (
                "Missing closing quote\"",
                Err(ParseSequenceError::MissingOpeningQuote),
            ),
            (
                "\"Missing closing quote",
                Err(ParseSequenceError::MissingClosingQuote),
            ),
            // Testing octal value is out of range
            (
                "\"\\440\"",
                Err(ParseSequenceError::InvalidEscape {
                    escape: String::from("\\4"),
                    index: 2,
                    string: String::from("\"\\440\""),
                }),
            ),
        ];

        for (s, expected) in tests {
            let result = parse_string(s);
            assert_eq!(result, expected, "Testing {s}");
        }
    }

    #[test]
    fn raw_string_does_not_interpret_escapes() {
        let tests: Vec<(&str, Result<String, ParseSequenceError>)> = vec![
            // Raw string in double quotes
            // r"Hello \a \" ' \' \U0001f431 " => Hello \a " ' \' \U0001f431
            // R"Hello \a \" ' \' \U0001f431 " => Hello \a " ' \' \U0001f431
            (
                "r\"Hello \\a \\\" ' \\' \\U0001f431 \"",
                Ok(String::from("Hello \\a \" ' \\' \\U0001f431 ")),
            ),
            (
                "R\"Hello \\a \\\" ' \\' \\U0001f431 \"",
                Ok(String::from("Hello \\a \" ' \\' \\U0001f431 ")),
            ),
            // Raw string in single quotes
            // r'Hello \a \" " \' \U0001f431 ' => Hello \a \" " ' \U0001f431
            // R'Hello \a \" " \' \U0001f431 ' => Hello \a \" " ' \U0001f431
            (
                "r'Hello \\a \\\" \" \\' \\U0001f431 '",
                Ok(String::from("Hello \\a \\\" \" ' \\U0001f431 ")),
            ),
            (
                "R'Hello \\a \\\" \" \\' \\U0001f431 '",
                Ok(String::from("Hello \\a \\\" \" ' \\U0001f431 ")),
            ),
        ];

        for (s, expected) in tests {
            let result = parse_string(s);
            assert_eq!(result, expected, "Testing {s}");
        }
    }

    #[test]
    fn parses_bytes() {
        let bytes = parse_bytes("abc💖\\xFF\\376").expect("Must parse!");
        assert_eq!([97, 98, 99, 240, 159, 146, 150, 255, 254], *bytes)
    }
}

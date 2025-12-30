use regex::Regex;

use std::sync::OnceLock;

#[derive(thiserror::Error, Debug)]
pub enum StandardFormatError {
    // Failed to pass standard format regex
    #[error("Failed to interpret {0:?} as standard format.")]
    InvalidFormat(String),
    #[error("`=` align can't be used along with truncate(!).")]
    TruncatedSignFirst,
    // #[error("Align string can't be empty.")]
    #[error("The align char should be one of <, >, =, ^, or `` but received {:?}.", .0.map(|chr| chr.to_string()).unwrap_or("<empty>".to_string()))]
    InvalidAlign(Option<char>),
    // #[error("Sign character can't be empty.")]
    #[error("The sign character should be one of -, +, or space, but received {:?}", .0.map(|chr| chr.to_string()).unwrap_or("<empty>".to_string()))]
    InvalidSign(Option<char>),
    // #[error("Grouping character can't be empty.")]
    #[error("The grouping character must be one of , or _, but received {:?}.", .0.map(|chr| chr.to_string()).unwrap_or("<empty>".to_string()))]
    InvalidGrouping(Option<char>),
    // #[error("The format type character can't be empty.")]
    #[error("The format type character must be one of b, c, d, o, s, x, X, or %, but received {:?}.", .0.map(|chr| chr.to_string()).unwrap_or("<empty>".to_string()))]
    InvalidFormatType(Option<char>),
    // runtime errors
    #[error("Debug trait is not implemented for the type of {0:?} key.")]
    DebugNotImplemented(Option<String>),
    #[error("Display trait is not implemented for the type of {0:?} key.")]
    DisplayNotImplemented(Option<String>),
    #[error("Format type {0:?} is not suitable for string.")]
    InvalidFormatTypeForString(Option<StandardFormatType>),
    #[error("Format type {0:?} is only valid for floats, not integers.")]
    InvalidFormatTypeForInteger(Option<StandardFormatType>),
    #[error("Format type {0:?} is only valid for integers, not floats.")]
    InvalidFormatTypeForFloat(Option<StandardFormatType>),
    #[error("Minimum number {0} cannot be formatted.")]
    MinimumInteger(String),
}

pub(crate) type StandardResult<T> = std::result::Result<T, StandardFormatError>;

fn standard_format() -> &'static Regex {
    static STANDARD_FORMAT: OnceLock<Regex> = OnceLock::new();
    STANDARD_FORMAT.get_or_init(|| {
        // let pattern = r##;
        let pattern = r#"^(?P<options>(?P<filler>((?P<align1>[<>=^])?|(?P<fill>.)?(?P<align2>[<>=^])?)(?P<truncate>!)?)?(?P<sign>[-+ ])?(?P<z_option>z)?(?P<sharp_option>#)?(?P<zero_option>0)?)?(?P<digit>[1-9]\d*)?(?P<grouping>[,_])?(?:\.(?P<precision>\d+))?(?P<type>[bcdeEfFgGnosxX%?])?$"#;
        regex::Regex::new(pattern).unwrap()
    })
}

pub(crate) fn parse_standard_format(format: &str) -> Result<StandardFormat, StandardFormatError> {
    if format.is_empty() {
        return Ok(Default::default());
    }

    // unwrap()로 처리된 구문의 경우 regex에 의해 성공적으로 파싱되었다면 절대 일어날 수 없는 일들이기 때문에 unwrap로 처리함.
    // 만약 unwrap() 구문으로 인해 panic이 발생할 경우 regex 구문이나 코드에 문제가 있다는 의미이니 수정해야 함.
    let captured = standard_format().captures(format).ok_or_else(|| StandardFormatError::InvalidFormat(format.to_string()))?;
    // let captured = dbg!(captured);
    let format_option = captured.name("options").map(|_| {
        let zero_option = captured.name("zero_option").is_some();
        let filler = captured.name("filler").map(|_| {
            // zero_option이 참이면 fill을 0으로, align을 =으로 설정
            // 현재는 다른 fill이나 align이 있다면 그 값이 zero_option의 default 대신 설정되도록 되어 있으나
            // 나중에는 어떨지 모름
            let fill = captured.name("fill").map(|single_str| {
                single_str
                    .as_str()
                    .chars()
                    .next()
                    .expect("Single string should match")
            });
            let fill = if zero_option && fill.is_none() {
                Some('0')
            } else {
                fill
            };
            let align = captured
                .name("align1")
                .or_else(|| captured.name("align2"))
                .map(|value| value.as_str().try_into().unwrap());
            let align = if zero_option && align.is_none() {
                Some(StandardFormatAlign::SignFirst)
            } else {
                align
            };
            StandardFormatOptionFill {
                fill,
                align,
                truncate: captured.name("truncate").is_some(),
            }
        });
        StandardFormatOption {
            filler,
            sign: captured
                .name("sign")
                .map(|matched| matched.as_str().try_into().unwrap()),
            z_option: captured.name("z_option").is_some(),
            sharp_option: captured.name("sharp_option").is_some(),
        }
    });
    let StandardFormatOption {
        filler,
        sign,
        z_option,
        sharp_option,
    } = format_option.unwrap_or_default();
    let digit = captured.name("digit").map(|matched| {
        matched
            .as_str()
            .parse()
            .expect("`digit` pattern is a sequence of digits so it should not fail.")
    });
    let grouping = captured
        .name("grouping")
        .map(|matched| matched.as_str().try_into().unwrap());
    let precision = captured.name("precision").map(|matched| {
        matched
            .as_str()
            .parse()
            .expect("`digit` pattern is a sequence of digits so it should not fail.")
    });
    let format_type = captured
        .name("type")
        .map(|matched| matched.as_str().try_into().unwrap());

    Ok(StandardFormat {
        filler,
        sign,
        z_option,
        sharp_option,
        digit,
        grouping,
        precision,
        format_type,
    })
}

pub(crate) fn fill(
    mut value: String,
    width: Option<usize>,
    align: StandardFormatAlign,
    truncate: bool,
    sign: Option<char>,
    filler: char,
) -> StandardResult<String> {
    if !matches!(align, StandardFormatAlign::SignFirst)
        && let Some(sign) = sign
    {
        value.insert(0, sign);
    }

    let Some(width) = width else {
        return Ok(value);
    };

    let cols = value.chars().count();
    let excess = cols.saturating_sub(width);
    if excess > 0 {
        if !truncate {
            return Ok(value);
        }
        let (start, end) = match align {
            StandardFormatAlign::Left => (0, cols - excess),
            StandardFormatAlign::Right => (excess, cols),
            StandardFormatAlign::SignFirst => {
                return Err(StandardFormatError::TruncatedSignFirst);
            }
            StandardFormatAlign::Center => (excess / 2, cols - excess.saturating_sub(excess / 2)),
        };
        return Ok(value.chars().skip(start).take(end - start).collect::<String>());
    }

    let diff = width.saturating_sub(cols);
    let (left_pad, right_pad) = match align {
        StandardFormatAlign::Left => (0, diff),
        StandardFormatAlign::Right | StandardFormatAlign::SignFirst if sign.is_some() => {
            (diff - 1, 0)
        }
        StandardFormatAlign::Right | StandardFormatAlign::SignFirst => (diff, 0),
        StandardFormatAlign::Center => (diff / 2, diff.saturating_sub(diff / 2)),
    };

    let mut result = String::new();
    if matches!(align, StandardFormatAlign::SignFirst)
        && let Some(sign) = sign
    {
        result.push(sign)
    }
    for _ in 0..left_pad {
        result.push(filler);
    }
    result.push_str(&value);
    for _ in 0..right_pad {
        result.push(filler);
    }
    Ok(result)
}

#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct StandardFormat {
    pub(crate) filler: Option<StandardFormatOptionFill>,
    pub(crate) sign: Option<StandardFormatSign>, // NUMERIC ONLY
    pub(crate) z_option: bool,                   // FLOAT ONLY
    pub(crate) sharp_option: bool,               // NON-FLOAT ONLY
    pub(crate) digit: Option<u32>,
    pub(crate) grouping: Option<StandardFormatGrouping>, // NUMERIC ONLY
    pub(crate) precision: Option<u32>,                   // FLOAT ONLY
    pub(crate) format_type: Option<StandardFormatType>,
}

#[derive(Default)]
pub(crate) struct StandardFormatOption {
    pub(crate) filler: Option<StandardFormatOptionFill>,
    pub(crate) sign: Option<StandardFormatSign>,
    pub(crate) z_option: bool,
    pub(crate) sharp_option: bool,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub(crate) struct StandardFormatOptionFill {
    pub(crate) fill: Option<char>,
    pub(crate) align: Option<StandardFormatAlign>,
    pub(crate) truncate: bool,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub(crate) enum StandardFormatAlign {
    Left,
    Right,
    SignFirst, // NUMERIC ONLY
    Center,
}

impl TryFrom<&str> for StandardFormatAlign {
    type Error = StandardFormatError;

    fn try_from(value: &str) -> StandardResult<Self> {
        Self::try_from(
            value
                .chars()
                .next()
                .ok_or(StandardFormatError::InvalidAlign(None))?,
        )
    }
}

impl TryFrom<char> for StandardFormatAlign {
    type Error = StandardFormatError;

    fn try_from(value: char) -> StandardResult<Self> {
        match value {
            '<' => Ok(Self::Left),
            '>' => Ok(Self::Right),
            '=' => Ok(Self::SignFirst),
            '^' => Ok(Self::Center),
            _ => Err(StandardFormatError::InvalidAlign(Some(value))),
        }
    }
}

impl From<StandardFormatAlign> for char {
    fn from(value: StandardFormatAlign) -> Self {
        match value {
            StandardFormatAlign::Left => '<',
            StandardFormatAlign::Right => '>',
            StandardFormatAlign::SignFirst => '=',
            StandardFormatAlign::Center => '^',
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub(crate) enum StandardFormatSign {
    Minus,
    Plus,
    Space,
}

impl TryFrom<&str> for StandardFormatSign {
    type Error = StandardFormatError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(
            value
                .chars()
                .next()
                .ok_or(StandardFormatError::InvalidSign(None))?,
        )
    }
}

impl TryFrom<char> for StandardFormatSign {
    type Error = StandardFormatError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '-' => Ok(Self::Minus),
            '+' => Ok(Self::Plus),
            ' ' => Ok(Self::Space),
            _ => Err(StandardFormatError::InvalidSign(Some(value)))
        }
    }
}

impl From<StandardFormatSign> for char {
    fn from(value: StandardFormatSign) -> Self {
        match value {
            StandardFormatSign::Minus => '-',
            StandardFormatSign::Plus => '+',
            StandardFormatSign::Space => ' ',
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub(crate) enum StandardFormatGrouping {
    Comma,
    Underscore,
}

impl TryFrom<&str> for StandardFormatGrouping {
    type Error = StandardFormatError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(
            value
                .chars()
                .next()
                .ok_or(StandardFormatError::InvalidGrouping(None))?,
        )
    }
}

impl TryFrom<char> for StandardFormatGrouping {
    type Error = StandardFormatError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            ',' => Ok(Self::Comma),
            '_' => Ok(Self::Underscore),
            _ => Err(StandardFormatError::InvalidGrouping(Some(value))),
        }
    }
}

impl From<StandardFormatGrouping> for char {
    fn from(value: StandardFormatGrouping) -> Self {
        match value {
            StandardFormatGrouping::Comma => ',',
            StandardFormatGrouping::Underscore => '_',
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum StandardFormatType {
    // Display does not have char representation
    Debug,
    // integer conversions
    Octal,
    HexLower,
    HexUpper,
    Binary,
    // Options that exist on Python but not on Rust
    String,
    Percent,
    Decimal,
    FloatLower,
    FloatUpper,
}

impl TryFrom<&str> for StandardFormatType {
    type Error = StandardFormatError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(
            value
                .chars()
                .next()
                .ok_or(StandardFormatError::InvalidFormatType(None))?,
        )
    }
}

impl TryFrom<char> for StandardFormatType {
    type Error = StandardFormatError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '?' => Ok(Self::Debug),
            'o' => Ok(Self::Octal),
            'x' => Ok(Self::HexLower),
            'X' => Ok(Self::HexUpper),
            'b' => Ok(Self::Binary),
            's' => Ok(Self::String),
            '%' => Ok(Self::Percent),
            'd' => Ok(Self::Decimal),
            'f' => Ok(Self::FloatLower),
            'F' => Ok(Self::FloatUpper),
            _ => Err(StandardFormatError::InvalidFormatType(Some(value))),
        }
    }
}

impl From<StandardFormatType> for char {
    fn from(value: StandardFormatType) -> Self {
        match value {
            StandardFormatType::Binary => 'b',
            StandardFormatType::Decimal => 'd',
            StandardFormatType::Octal => 'o',
            StandardFormatType::HexLower => 'x',
            StandardFormatType::HexUpper => 'X',
            StandardFormatType::Percent => '%',
            StandardFormatType::Debug => '?',
            StandardFormatType::FloatLower => 'f',
            StandardFormatType::FloatUpper => 'F',
            StandardFormatType::String => 's',
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Formattable;

    use super::*;

    #[test]
    fn test_parse_standard_format() {
        // Basic test
        let format = "05d";
        let parsed = parse_standard_format(format).unwrap();
        assert_eq!(parsed.digit, Some(5));
        assert_eq!(parsed.format_type, Some(StandardFormatType::Decimal));
        assert_eq!(parsed.filler.unwrap().fill.unwrap(), '0');

        // Alignment and fill
        let format = "*^10";
        let parsed = parse_standard_format(format).unwrap();
        assert_eq!(parsed.digit, Some(10));
        let filler = parsed.filler.unwrap();
        assert_eq!(filler.fill, Some('*'));
        assert_eq!(filler.align, Some(StandardFormatAlign::Center));

        // Complex format
        let format = "#<10.5s";
        let parsed = parse_standard_format(format).unwrap();
        assert_eq!(parsed.digit, Some(10));
        assert_eq!(parsed.precision, Some(5));
        assert_eq!(parsed.format_type, Some(StandardFormatType::String));
        let filler = parsed.filler.unwrap();
        assert_eq!(filler.fill, Some('#'));
        assert_eq!(filler.align, Some(StandardFormatAlign::Left));

        // Empty
        assert_eq!(parse_standard_format("").unwrap(), StandardFormat::default());

        // Digit
        let format = parse_standard_format("10").unwrap();
        assert_eq!(format.digit, Some(10));
        assert_eq!(format.format_type, None);

        // Align and Fill
        let format = parse_standard_format("<10").unwrap();
        assert_eq!(
            format.filler,
            Some(StandardFormatOptionFill {
                fill: None,
                align: Some(StandardFormatAlign::Left),
                truncate: false
            })
        );
        assert_eq!(format.digit, Some(10));

        let format = parse_standard_format("*>10").unwrap();
        assert_eq!(
            format.filler,
            Some(StandardFormatOptionFill {
                fill: Some('*'),
                align: Some(StandardFormatAlign::Right),
                truncate: false
            })
        );

        // Sign
        let format = parse_standard_format("+").unwrap();
        assert_eq!(format.sign, Some(StandardFormatSign::Plus));

        // Type
        let format = parse_standard_format("x").unwrap();
        assert_eq!(format.format_type, Some(StandardFormatType::HexLower));

        // Complex
        let format = parse_standard_format("#010x").unwrap();
        assert!(format.sharp_option);
        // 0 option implies fill='0' and align=SignFirst if not specified
        assert_eq!(
            format.filler,
            Some(StandardFormatOptionFill {
                fill: Some('0'),
                align: Some(StandardFormatAlign::SignFirst),
                truncate: false
            })
        );
        assert_eq!(format.digit, Some(10));
        assert_eq!(format.format_type, Some(StandardFormatType::HexLower));
    }

    #[test]
    fn test_fill() {
        // Basic fill
        let res = fill(
            "foo".to_string(),
            Some(5),
            StandardFormatAlign::Left,
            false,
            None,
            ' ',
        )
        .unwrap();
        assert_eq!(res, "foo  ");

        let res = fill(
            "foo".to_string(),
            Some(5),
            StandardFormatAlign::Right,
            false,
            None,
            ' ',
        )
        .unwrap();
        assert_eq!(res, "  foo");

        let res = fill(
            "foo".to_string(),
            Some(5),
            StandardFormatAlign::Center,
            false,
            None,
            ' ',
        )
        .unwrap();
        assert_eq!(res, " foo ");

        // With fill char
        let res = fill(
            "foo".to_string(),
            Some(5),
            StandardFormatAlign::Right,
            false,
            None,
            '-',
        )
        .unwrap();
        assert_eq!(res, "--foo");

        // Truncate
        let res = fill(
            "foobar".to_string(),
            Some(3),
            StandardFormatAlign::Left,
            true,
            None,
            ' ',
        )
        .unwrap();
        assert_eq!(res, "foo");

        // No truncate (default behavior for excess)
        let res = fill(
            "foobar".to_string(),
            Some(3),
            StandardFormatAlign::Left,
            false,
            None,
            ' ',
        )
        .unwrap();
        assert_eq!(res, "foobar");
    }

    #[test]
    fn test_fill_unicode_truncate_bug() {
        let res = fill(
            "한글".to_string(),
            Some(1),
            StandardFormatAlign::Left,
            true,
            None,
            ' ',
        );
        
        // If it doesn't panic, check the result
        if let Ok(s) = res {
            assert_eq!(s, "한");
        }
    }

    #[test]
    fn test_formattable_i32_edge_cases() {
        let to_format: &dyn Formattable = &0;
        assert_eq!(
            to_format.standard_format(parse_standard_format("d").unwrap()).unwrap(),
            "0"
        );

        let to_format: &dyn Formattable = &i32::MAX;
        assert_eq!(
            to_format.standard_format(parse_standard_format("d").unwrap()).unwrap(),
            "2147483647"
        );

        // 현재는 포매팅 불가
        let to_format: &dyn Formattable = &i32::MIN;
        assert!(to_format.standard_format(parse_standard_format("d").unwrap()).is_err());
        // assert_eq!(
        //     to_format.standard_format(parse_standard_format("d").unwrap()).unwrap(),
        //     "-2147483648"
        // );
    }

    #[test]
    fn test_formattable_string_unicode() {
        let to_format: &dyn Formattable = &"한글".to_string();
        
        // Width 4, align right. "  한글" (2 spaces + 2 chars)
        assert_eq!(
            to_format.standard_format(parse_standard_format(">4s").unwrap()).unwrap(),
            "  한글"
        );
    }
}

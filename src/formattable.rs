use anyhow::bail;

use crate::StandardFormatType;

use super::*;

const WARN_FOR_USELESS_OPTIONS: bool = cfg!(debug_assertions);

pub trait Formattable {
    fn custom_format(&self, _: &str) -> anyhow::Result<String> {
        bail!("This type is not formattable with arbitrary format.")
    }
    fn standard_format(&self, format: StandardFormat) -> anyhow::Result<String>;
}

macro_rules! impl_standard_format {
    (display) => {
        impl_standard_format!(@normal_standard_format, __display_stringfy__);
    };
    (debug) => {
        impl_standard_format!(@normal_standard_format, __debug_stringfy__);
    };
    (__display_stringfy__, $format:ident, $self:ident) => {
        match (
            $format.sharp_option,
            matches!($format.format_type, Some(StandardFormatType::Debug)),
        ) {
            (true, true) => format!("{:#?}", $self),
            (true, false) => format!("{:#}", $self),
            (false, true) => format!("{:?}", $self),
            (false, false) => $self.to_string(),
        }
    };
    (__debug_stringfy__, $format:ident, $self:ident) => {
        match (
            $format.sharp_option,
            matches!($format.format_type, Some(StandardFormatType::Debug)),
        ) {
            (true, true) => format!("{:#?}", $self),
            (true, false) => bail!("Display trait is not implemented."),
            (false, true) => format!("{:?}", $self),
            (false, false) => bail!("Display trait is not implemented."),
        }
    };
    (@normal_standard_format, $stringfy_method:ident) => {
        fn standard_format(&self, format: StandardFormat) -> anyhow::Result<String> {
            if !matches!(format.format_type, None | Some(StandardFormatType::Debug | StandardFormatType::String)) {
                bail!(
                    "Unexpected format type for string: {:?}",
                    format.format_type
                );
            }

            if WARN_FOR_USELESS_OPTIONS {
                if format.z_option {
                    log::warn!("The zero option is only valid for the float type. format: {format:#?}")
                }
                if format.precision.is_some() {
                    log::warn!("The precision is only valid for the float type. format: {format:#?}")
                }
                if format.sign.is_some() {
                    log::warn!("The sign is only valid for the numeric type. format: {format:#?}")
                }
                if format.grouping.is_some() {
                    log::warn!("The grouping is only valid for the numeric type. format: {format:#?}")
                }
            }

            let stringfied = impl_standard_format!($stringfy_method, format, self);

            if let Some(filler) = format.filler {
                let result = fill(
                    stringfied,
                    format.digit.map(|value| value as usize),
                    filler.align.unwrap_or(StandardFormatAlign::Left),
                    filler.truncate,
                    None,
                    filler.fill.unwrap_or(' '),
                )?;
                Ok(result)
            } else {
                Ok(stringfied)
            }
        }
    };
    (signed) => {
        impl_standard_format!(@integer_standard_format, __signed_operation__);
    };
    (unsigned) => {
        impl_standard_format!(@integer_standard_format, __unsigned_operation__);
    };
    (__signed_operation__, $format:ident, $self:ident) => {
        {
            // Self::MIN인 경우 abs() 메서드에서 패닉이 발생함 :/
            if *$self == Self::MIN {
                bail!("Minimum number {} cannot be formatted.", $self);
            }
            let num = $self.abs();
            let format_type = $format.format_type.unwrap_or(StandardFormatType::Decimal);
            let formatted = match format_type {
                StandardFormatType::Octal => if $format.sharp_option { format!("{num:#o}") } else { format!("{num:o}") },
                StandardFormatType::HexLower => if $format.sharp_option { format!("{num:#x}") } else { format!("{num:x}") },
                StandardFormatType::HexUpper => if $format.sharp_option { format!("{num:#X}") } else { format!("{num:X}") },
                StandardFormatType::Binary => if $format.sharp_option { format!("{num:#b}") } else { format!("{num:b}") },
                StandardFormatType::Decimal => format!("{num}"),
                StandardFormatType::Debug | StandardFormatType::String => panic!("Already handled."),
                StandardFormatType::FloatLower
                | StandardFormatType::FloatUpper
                | StandardFormatType::Percent => {
                    bail!(
                        "{:?} format type is only valid for floats, not integer.",
                        $format.format_type
                    )
                }
            };

            let sign = match $self.signum() {
                1 => match $format.sign {
                    None => None,
                    Some(StandardFormatSign::Minus) => None,
                    Some(StandardFormatSign::Plus) => Some('+'),
                    Some(StandardFormatSign::Space) => Some(' '),
                }
                0 => None,
                -1 => Some('-'),
                _ => panic!(),
            };

            (formatted, sign)
        }
    };
    (__unsigned_operation__, $format:ident, $self:ident) => {
        {
            let num = *$self;
            let format_type = $format.format_type.unwrap_or(StandardFormatType::Decimal);
            let formatted = match format_type {
                StandardFormatType::Octal => if $format.sharp_option { format!("{num:#o}") } else { format!("{num:o}") },
                StandardFormatType::HexLower => if $format.sharp_option { format!("{num:#x}") } else { format!("{num:x}") },
                StandardFormatType::HexUpper => if $format.sharp_option { format!("{num:#X}") } else { format!("{num:X}") },
                StandardFormatType::Binary => if $format.sharp_option { format!("{num:#b}") } else { format!("{num:b}") },
                StandardFormatType::Decimal => format!("{num}"),
                StandardFormatType::Debug | StandardFormatType::String => panic!("Already handled."),
                StandardFormatType::FloatLower
                | StandardFormatType::FloatUpper
                | StandardFormatType::Percent => {
                    bail!(
                        "{:?} format type is only valid for floats, not integer.",
                        $format.format_type
                    )
                }
            };

            let sign = match $self {
                0 => None,
                _ => match $format.sign {
                    None => None,
                    Some(StandardFormatSign::Minus) => None,
                    Some(StandardFormatSign::Plus) => Some('+'),
                    Some(StandardFormatSign::Space) => Some(' '),
                }
            };

            (formatted, sign)
        }
    };
    (@integer_standard_format, $signed:ident) => {
        fn standard_format(&self, format: StandardFormat) -> anyhow::Result<String> {
            if matches!(
                format.format_type,
                Some(StandardFormatType::Debug | StandardFormatType::String)
            ) {
                return self.to_string().standard_format(format);
            }

            if WARN_FOR_USELESS_OPTIONS {
                if format.z_option {
                    eprintln!("The zero option is only valid for the float type. format: {format:#?}")
                }
                if format.precision.is_some() {
                    eprintln!("The precision is only valid for the float type. format: {format:#?}")
                }
            }

            if format.grouping.is_some() {
                todo!("The number grouping is not yet implemented.");
            }

            let (mut formatted, sign) = impl_standard_format!($signed, format, self);

            if let Some(filler) = format.filler {
                let result = fill(
                    formatted,
                    format.digit.map(|value| value as usize),
                    filler.align.unwrap_or(StandardFormatAlign::Right),
                    filler.truncate,
                    sign,
                    filler.fill.unwrap_or(' '),
                )?;
                Ok(result)
            } else {
                if let Some(sign) = sign {
                    formatted.insert(0, sign);
                }
                Ok(formatted)
            }
        }
    };
    ($type:ty, $option:ident) => {
        impl Formattable for $type {
            impl_standard_format!($option);
        }
    };
    (float) => {
        fn standard_format(&self, format: StandardFormat) -> anyhow::Result<String> {
            if matches!(
                format.format_type,
                Some(StandardFormatType::Debug | StandardFormatType::String)
            ) {
                return self.to_string().standard_format(format);
            }

            if WARN_FOR_USELESS_OPTIONS {
                // 비록 아무 역할도 없는 것은 맞지만 딱 float만 이 값이 쓸모 없기 때문에 generic함이 부족해서 따로 경고를 내보내지는 않음.
                // if format.sharp_option {
                //     log::warn!("The sharp option is useless for the float type. format: {format:#?}")
                // }
            }

            if format.grouping.is_some() {
                todo!("The number grouping is not yet implemented.");
            }
            if format.z_option {
                todo!("The z option is not yet implemented.");
            }

            // let num = if *self == 0.0 && format.z_option { 0.0 } else { self.abs() };
            let num = self.abs();
            let format_type = format.format_type.unwrap_or(StandardFormatType::Decimal);
            let mut formatted = match format_type {
                StandardFormatType::Octal
                | StandardFormatType::HexLower
                | StandardFormatType::HexUpper
                | StandardFormatType::Binary
                | StandardFormatType::Decimal => {
                    bail!(
                        "{:?} format type is only valid for integer, not floats.",
                        format.format_type
                    )
                }
                StandardFormatType::Debug | StandardFormatType::String => panic!("Already handled."),
                StandardFormatType::FloatLower => {
                    if let Some(precision) = format.precision {
                        format!("{num:.*}", precision as usize)
                    } else {
                        num.to_string()
                    }
                }
                StandardFormatType::FloatUpper => {
                    if let Some(precision) = format.precision {
                        format!("{num:.*}", precision as usize)
                    } else {
                        num.to_string()
                    }.to_ascii_uppercase()
                }
                StandardFormatType::Percent => {
                    if let Some(precision) = format.precision {
                        format!("{:.*}%", precision as usize, num * 100.0)
                    } else {
                        format!("{}%", num * 100.0)
                    }
                }
            };

            let sign = match (self.signum(), *self) {
                (_, Self::INFINITY) => None,
                _ if self.is_nan() => None,
                // (_, 0.0) if format.z_option => None,
                (1.0, _) => match format.sign {
                    None => None,
                    Some(StandardFormatSign::Minus) => None,
                    Some(StandardFormatSign::Plus) => Some('+'),
                    Some(StandardFormatSign::Space) => Some(' '),
                }
                (-1.0, _) => Some('-'),
                _ => panic!(),
            };

            if let Some(filler) = format.filler {
                let result = fill(
                    formatted,
                    format.digit.map(|value| value as usize),
                    filler.align.unwrap_or(StandardFormatAlign::Right),
                    filler.truncate,
                    sign,
                    filler.fill.unwrap_or(' '),
                )?;
                Ok(result)
            } else {
                if let Some(sign) = sign {
                    formatted.insert(0, sign);
                }
                Ok(formatted)
            }
        }
    };
}

impl_standard_format!(String, display);
impl_standard_format!(char, display);
impl_standard_format!(&str, display);
impl_standard_format!(i8, signed);
impl_standard_format!(i16, signed);
impl_standard_format!(i32, signed);
impl_standard_format!(i64, signed);
impl_standard_format!(i128, signed);
impl_standard_format!(isize, signed);
impl_standard_format!(u8, unsigned);
impl_standard_format!(u16, unsigned);
impl_standard_format!(u32, unsigned);
impl_standard_format!(u64, unsigned);
impl_standard_format!(u128, unsigned);
impl_standard_format!(usize, unsigned);
impl_standard_format!(f32, float);
impl_standard_format!(f64, float);

#[cfg(feature = "chrono")]
impl Formattable for chrono::NaiveDateTime {
    impl_standard_format!(debug);
}

#[cfg(feature = "chrono")]
impl<T: chrono::TimeZone> Formattable for chrono::DateTime<T>
    where T::Offset: std::fmt::Display
{
    impl_standard_format!(debug);

    fn custom_format(&self, format: &str) -> anyhow::Result<String> {
        Ok(self.format(format).to_string())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let to_format: &dyn Formattable = &123;
        // Sign
        assert_eq!(
            to_format.standard_format(dbg!(parse_standard_format("+d").unwrap())).unwrap(),
            "+123"
        );
    }

    #[test]
    #[cfg(feature = "chrono")]
    fn test_chrono() {
        use chrono::TimeZone;

        let datetime: &dyn Formattable = &chrono::Utc.with_ymd_and_hms(2014, 11, 28, 12, 0, 9).unwrap();
        let mapping = literal_map!({
            "datetime" => datetime,
        });

        let template: Template = "{datetime|%Y-%m-%d %H:%M:%S}".parse().unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "2014-11-28 12:00:09");
    }

    #[test]
    fn test_formattable_i32() {
        let to_format: &dyn Formattable = &123;

        // Octal
        assert_eq!(
            to_format.standard_format(parse_standard_format("o").unwrap()).unwrap(),
            "173"
        );

        // Decimal
        assert_eq!(
            to_format.standard_format(parse_standard_format("d").unwrap()).unwrap(),
            "123"
        );

        // Binary
        assert_eq!(
            to_format.standard_format(parse_standard_format("b").unwrap()).unwrap(),
            "1111011"
        );

        // Hex
        assert_eq!(
            to_format.standard_format(parse_standard_format("x").unwrap()).unwrap(),
            "7b"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("X").unwrap()).unwrap(),
            "7B"
        );

        // Padding
        assert_eq!(
            to_format.standard_format(parse_standard_format("05d").unwrap()).unwrap(),
            "00123"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("5d").unwrap()).unwrap(),
            "  123"
        );

        // Alignment
        assert_eq!(
            to_format.standard_format(parse_standard_format("<5d").unwrap()).unwrap(),
            "123  "
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format(">5d").unwrap()).unwrap(),
            "  123"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("^5d").unwrap()).unwrap(),
            " 123 "
        );

        // Sign
        assert_eq!(
            to_format.standard_format(parse_standard_format("+d").unwrap()).unwrap(),
            "+123"
        );

        let neg_format: &dyn Formattable = &-123;
        assert_eq!(neg_format.standard_format(parse_standard_format("d").unwrap()).unwrap(), "-123");

        assert_eq!(neg_format.standard_format(parse_standard_format("05d").unwrap()).unwrap(), "-0123");

        let n = 42;

        // Default
        let format = parse_standard_format("").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "42");

        // Binary
        let format = parse_standard_format("b").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "101010");

        // Hex
        let format = parse_standard_format("x").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "2a");
        
        let format = parse_standard_format("#x").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "0x2a");

        // Padding
        let format = parse_standard_format("05").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "00042");
    }

    #[test]
    fn test_formattable_u32() {
        let to_format: &dyn Formattable = &123u32;

        // Octal
        assert_eq!(
            to_format.standard_format(parse_standard_format("o").unwrap()).unwrap(),
            "173"
        );

        // Decimal
        assert_eq!(
            to_format.standard_format(parse_standard_format("d").unwrap()).unwrap(),
            "123"
        );

        // Binary
        assert_eq!(
            to_format.standard_format(parse_standard_format("b").unwrap()).unwrap(),
            "1111011"
        );

        // Hex
        assert_eq!(
            to_format.standard_format(parse_standard_format("x").unwrap()).unwrap(),
            "7b"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("X").unwrap()).unwrap(),
            "7B"
        );

        // Padding
        assert_eq!(
            to_format.standard_format(parse_standard_format("05d").unwrap()).unwrap(),
            "00123"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("5d").unwrap()).unwrap(),
            "  123"
        );

        // Alignment
        assert_eq!(
            to_format.standard_format(parse_standard_format("<5d").unwrap()).unwrap(),
            "123  "
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format(">5d").unwrap()).unwrap(),
            "  123"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("^5d").unwrap()).unwrap(),
            " 123 "
        );

        // Sign
        assert_eq!(
            to_format.standard_format(parse_standard_format("+d").unwrap()).unwrap(),
            "+123"
        );

        let n = 42u32;

        // Default
        let format = parse_standard_format("").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "42");

        // Binary
        let format = parse_standard_format("b").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "101010");

        // Hex
        let format = parse_standard_format("x").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "2a");
        
        let format = parse_standard_format("#x").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "0x2a");

        // Padding
        let format = parse_standard_format("05").unwrap();
        assert_eq!(n.standard_format(format).unwrap(), "00042");
    }

    #[test]
    fn test_formattable_string() {
        let to_format: &dyn Formattable = &"hello".to_string();

        assert_eq!(
            to_format.standard_format(parse_standard_format("s").unwrap()).unwrap(),
            "hello"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("10s").unwrap()).unwrap(),
            "hello     "
        );

        assert_eq!(
            to_format.standard_format(parse_standard_format(">10s").unwrap()).unwrap(),
            "     hello"
        );
        assert_eq!(
            to_format.standard_format(parse_standard_format("^10s").unwrap()).unwrap(),
            "  hello   "
        );

        assert_eq!(
            to_format.standard_format(parse_standard_format("!3s").unwrap()).unwrap(),
            "hel"
        );

        let s = "hello".to_string();
        
        // Default
        let format = parse_standard_format("").unwrap();
        assert_eq!(s.standard_format(format).unwrap(), "hello");

        // Align
        let format = parse_standard_format("10").unwrap();
        assert_eq!(s.standard_format(format).unwrap(), "hello     ");

        let format = parse_standard_format(">10").unwrap();
        assert_eq!(s.standard_format(format).unwrap(), "     hello");

        let format = parse_standard_format("^10").unwrap();
        assert_eq!(s.standard_format(format).unwrap(), "  hello   ");

        // Truncate
        let format = parse_standard_format("!3").unwrap();
        assert_eq!(s.standard_format(format).unwrap(), "hel");

        // Debug
        assert_eq!(
            s.standard_format(parse_standard_format("?").unwrap()).unwrap(),
            "\"hello\""
        );
        assert_eq!(
            s.standard_format(parse_standard_format("#?").unwrap()).unwrap(),
            "\"hello\""
        );
    }

    #[test]
    fn test_formattable_char() {
        let c = 'a';
        assert_eq!(c.standard_format(parse_standard_format("").unwrap()).unwrap(), "a");
        assert_eq!(c.standard_format(parse_standard_format("3").unwrap()).unwrap(), "a  ");
        assert_eq!(c.standard_format(parse_standard_format(">3").unwrap()).unwrap(), "  a");

        // Debug
        assert_eq!(
            c.standard_format(parse_standard_format("?").unwrap()).unwrap(),
            "'a'"
        );
        assert_eq!(
            c.standard_format(parse_standard_format("#?").unwrap()).unwrap(),
            "'a'"
        );
    }

    #[test]
    fn test_formattable_str() {
        let s = "hello";
        assert_eq!(s.standard_format(parse_standard_format("").unwrap()).unwrap(), "hello");
        assert_eq!(s.standard_format(parse_standard_format("10").unwrap()).unwrap(), "hello     ");
        assert_eq!(s.standard_format(parse_standard_format(">10").unwrap()).unwrap(), "     hello");
        assert_eq!(s.standard_format(parse_standard_format("!3").unwrap()).unwrap(), "hel");

        // Debug
        assert_eq!(
            s.standard_format(parse_standard_format("?").unwrap()).unwrap(),
            "\"hello\""
        );
        assert_eq!(
            s.standard_format(parse_standard_format("#?").unwrap()).unwrap(),
            "\"hello\""
        );
    }

    #[test]
    fn test_formattable_dyn_str() {
        let hello: &dyn Formattable = &"hello";
        assert_eq!(hello.standard_format(parse_standard_format("").unwrap()).unwrap(), "hello");
    }

    #[test]
    fn test_formattable_i8() {
        let n = 42i8;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
        assert_eq!((-n).standard_format(parse_standard_format("04").unwrap()).unwrap(), "-042");
    }

    #[test]
    fn test_formattable_i16() {
        let n = 42i16;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
        assert_eq!((-n).standard_format(parse_standard_format("04").unwrap()).unwrap(), "-042");
    }

    #[test]
    fn test_formattable_i64() {
        let n = 42i64;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
        assert_eq!((-n).standard_format(parse_standard_format("04").unwrap()).unwrap(), "-042");
    }

    #[test]
    fn test_formattable_i128() {
        let n = 42i128;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
        assert_eq!((-n).standard_format(parse_standard_format("04").unwrap()).unwrap(), "-042");
    }

    #[test]
    fn test_formattable_u8() {
        let n = 42u8;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
    }

    #[test]
    fn test_formattable_u16() {
        let n = 42u16;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
    }

    #[test]
    fn test_formattable_u64() {
        let n = 42u64;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
    }

    #[test]
    fn test_formattable_u128() {
        let n = 42u128;
        assert_eq!(n.standard_format(parse_standard_format("").unwrap()).unwrap(), "42");
        assert_eq!(n.standard_format(parse_standard_format("04").unwrap()).unwrap(), "0042");
    }

    #[test]
    fn test_formattable_f32() {
        let n = 123.456f32;
        let neg = -123.456f32;
        let zero = 0.0f32;
        let neg_zero = -0.0f32;
        let inf = f32::INFINITY;
        let neg_inf = f32::NEG_INFINITY;
        let nan = f32::NAN;

        // Basic FloatLower
        assert_eq!(n.standard_format(parse_standard_format("f").unwrap()).unwrap(), "123.456");
        assert_eq!(neg.standard_format(parse_standard_format("f").unwrap()).unwrap(), "-123.456");
        assert_eq!(zero.standard_format(parse_standard_format("f").unwrap()).unwrap(), "0");
        
        // Negative zero behavior (without z_option)
        assert_eq!(neg_zero.standard_format(parse_standard_format("f").unwrap()).unwrap(), "-0");

        // Precision
        assert_eq!(n.standard_format(parse_standard_format(".2f").unwrap()).unwrap(), "123.46");
        assert_eq!(n.standard_format(parse_standard_format(".0f").unwrap()).unwrap(), "123");
        assert_eq!(n.standard_format(parse_standard_format(".5f").unwrap()).unwrap(), "123.45600");
        assert_eq!(neg_inf.standard_format(parse_standard_format("f").unwrap()).unwrap(), "-inf");
        assert_eq!(neg_inf.standard_format(parse_standard_format("=5f").unwrap()).unwrap(), "- inf");

        // FloatUpper
        assert_eq!(n.standard_format(parse_standard_format("F").unwrap()).unwrap(), "123.456");
        assert_eq!(inf.standard_format(parse_standard_format("F").unwrap()).unwrap(), "INF");
        assert_eq!(nan.standard_format(parse_standard_format("F").unwrap()).unwrap(), "NAN");

        // Percent
        let p = 0.1234f32;
        assert_eq!(p.standard_format(parse_standard_format("%").unwrap()).unwrap(), "12.34%");
        assert_eq!(p.standard_format(parse_standard_format(".1%").unwrap()).unwrap(), "12.3%");

        // Padding and Alignment
        assert_eq!(n.standard_format(parse_standard_format("10f").unwrap()).unwrap(), "   123.456");
        assert_eq!(n.standard_format(parse_standard_format("<10f").unwrap()).unwrap(), "123.456   ");
        assert_eq!(n.standard_format(parse_standard_format("^10f").unwrap()).unwrap(), " 123.456  ");
        assert_eq!(n.standard_format(parse_standard_format(">10f").unwrap()).unwrap(), "   123.456");

        // Zero Padding
        assert_eq!(n.standard_format(parse_standard_format("010f").unwrap()).unwrap(), "000123.456");
        assert_eq!(neg.standard_format(parse_standard_format("010f").unwrap()).unwrap(), "-00123.456");

        // Sign options
        assert_eq!(n.standard_format(parse_standard_format("+f").unwrap()).unwrap(), "+123.456");
        assert_eq!(neg.standard_format(parse_standard_format("+f").unwrap()).unwrap(), "-123.456");
        assert_eq!(zero.standard_format(parse_standard_format("+f").unwrap()).unwrap(), "+0");
        assert_eq!(n.standard_format(parse_standard_format(" f").unwrap()).unwrap(), " 123.456");
        assert_eq!(neg.standard_format(parse_standard_format(" f").unwrap()).unwrap(), "-123.456");

        // Complex combinations
        assert_eq!(n.standard_format(parse_standard_format("+010.2f").unwrap()).unwrap(), "+000123.46");
        assert_eq!(neg.standard_format(parse_standard_format("+010.2f").unwrap()).unwrap(), "-000123.46");
        
        // Special values with padding
        assert_eq!(inf.standard_format(parse_standard_format("5f").unwrap()).unwrap(), "  inf");
        assert_eq!(nan.standard_format(parse_standard_format("5f").unwrap()).unwrap(), "  NaN");
    }

    #[test]
    fn test_formattable_f64() {
        let n = 123.456f64;
        let neg = -123.456f64;
        let zero = 0.0f64;
        let neg_zero = -0.0f64;
        let inf = f64::INFINITY;
        let neg_inf = f64::NEG_INFINITY;
        let nan = f64::NAN;

        // Basic FloatLower
        assert_eq!(n.standard_format(parse_standard_format("f").unwrap()).unwrap(), "123.456");
        assert_eq!(neg.standard_format(parse_standard_format("f").unwrap()).unwrap(), "-123.456");
        assert_eq!(zero.standard_format(parse_standard_format("f").unwrap()).unwrap(), "0");
        
        // Negative zero behavior (without z_option)
        assert_eq!(neg_zero.standard_format(parse_standard_format("f").unwrap()).unwrap(), "-0");

        // Precision
        assert_eq!(n.standard_format(parse_standard_format(".2f").unwrap()).unwrap(), "123.46");
        assert_eq!(n.standard_format(parse_standard_format(".0f").unwrap()).unwrap(), "123");
        assert_eq!(n.standard_format(parse_standard_format(".5f").unwrap()).unwrap(), "123.45600");
        assert_eq!(neg_inf.standard_format(parse_standard_format("f").unwrap()).unwrap(), "-inf");
        assert_eq!(neg_inf.standard_format(parse_standard_format("=5f").unwrap()).unwrap(), "- inf");

        // FloatUpper
        assert_eq!(n.standard_format(parse_standard_format("F").unwrap()).unwrap(), "123.456");
        assert_eq!(inf.standard_format(parse_standard_format("F").unwrap()).unwrap(), "INF");
        assert_eq!(nan.standard_format(parse_standard_format("F").unwrap()).unwrap(), "NAN");

        // Percent
        let p = 0.1234f32;
        assert_eq!(p.standard_format(parse_standard_format("%").unwrap()).unwrap(), "12.34%");
        assert_eq!(p.standard_format(parse_standard_format(".1%").unwrap()).unwrap(), "12.3%");

        // Padding and Alignment
        assert_eq!(n.standard_format(parse_standard_format("10f").unwrap()).unwrap(), "   123.456");
        assert_eq!(n.standard_format(parse_standard_format("<10f").unwrap()).unwrap(), "123.456   ");
        assert_eq!(n.standard_format(parse_standard_format("^10f").unwrap()).unwrap(), " 123.456  ");
        assert_eq!(n.standard_format(parse_standard_format(">10f").unwrap()).unwrap(), "   123.456");

        // Zero Padding
        assert_eq!(n.standard_format(parse_standard_format("010f").unwrap()).unwrap(), "000123.456");
        assert_eq!(neg.standard_format(parse_standard_format("010f").unwrap()).unwrap(), "-00123.456");

        // Sign options
        assert_eq!(n.standard_format(parse_standard_format("+f").unwrap()).unwrap(), "+123.456");
        assert_eq!(neg.standard_format(parse_standard_format("+f").unwrap()).unwrap(), "-123.456");
        assert_eq!(zero.standard_format(parse_standard_format("+f").unwrap()).unwrap(), "+0");
        assert_eq!(n.standard_format(parse_standard_format(" f").unwrap()).unwrap(), " 123.456");
        assert_eq!(neg.standard_format(parse_standard_format(" f").unwrap()).unwrap(), "-123.456");

        // Complex combinations
        assert_eq!(n.standard_format(parse_standard_format("+010.2f").unwrap()).unwrap(), "+000123.46");
        assert_eq!(neg.standard_format(parse_standard_format("+010.2f").unwrap()).unwrap(), "-000123.46");
        
        // Special values with padding
        assert_eq!(inf.standard_format(parse_standard_format("5f").unwrap()).unwrap(), "  inf");
        assert_eq!(nan.standard_format(parse_standard_format("5f").unwrap()).unwrap(), "  NaN");
    }
}

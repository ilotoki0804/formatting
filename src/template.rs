// indicatif = "0.18.3"에서 가져온 코드 (MIT License)
// use indicatif::ProgressStyle;
use std::{collections::HashMap, fmt, mem, str::FromStr};

use anyhow::{Result, anyhow, bail};

use crate::{StandardFormat, parse_standard_format};

use super::formattable::Formattable;

pub trait Getter {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable>;
}

impl Getter for HashMap<String, &dyn Formattable> {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
        let result = self.get(key)?;
        Some(*result)
    }
}

impl Getter for HashMap<String, Box<dyn Formattable>> {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
        let result = self.get(key);
        result.map(|value| value.as_ref())
    }
}

impl Getter for Vec<&dyn Formattable> {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
        let index: usize = key.parse().ok()?;
        let result = self.get(index)?;
        Some(*result)
    }
}

impl Getter for Vec<Box<dyn Formattable>> {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
        let index: usize = key.parse().ok()?;
        let result = self.get(index)?;
        Some(result.as_ref())
    }
}

impl Getter for &[&dyn Formattable] {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
        let index: usize = key.parse().ok()?;
        let result = self.get(index)?;
        Some(*result)
    }
}

impl Getter for &[Box<dyn Formattable>] {
    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
        let index: usize = key.parse().ok()?;
        let result = self.get(index)?;
        Some(result.as_ref())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum State {
    Literal,
    MaybeOpen,
    DoubleClose,
    Key,
    Format,
    CustomFormat,
}

// #[derive(Hash, Eq, PartialEq)]
// enum KeyOrOrder {
//     Key(String),
//     Order(u32),
// }

// type TemplateMap = HashMap<KeyOrOrder, Box<dyn Formattable>>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum TemplatePart {
    Literal(String),
    Interpolation {
        // key와 custom_format은 Some일 경우 empty가 아닌 것이 확정됨
        key: Option<String>,
        standard_format: Option<StandardFormat>,
        custom_format: Option<String>,
    },
}

#[derive(Clone, Debug)]
pub struct Template {
    parts: Vec<TemplatePart>,
}

impl Template {
    pub fn format(&self, key_getter: &dyn Getter) -> Result<String> {
        self.format_ommitable(key_getter, true)
    }

    fn format_ommitable(&self, mapping: &dyn Getter, no_key_ok: bool) -> Result<String> {
        use TemplatePart::*;
        let mut result = String::new();
        let mut index = 0;
        for part in &self.parts {
            match part {
                Literal(string) => result.push_str(string),
                Interpolation {
                    key,
                    standard_format,
                    custom_format,
                } => {
                    let formattable = if let Some(key) = key.as_ref() {
                        mapping.get_formattable(key)
                    } else {
                        let result = mapping.get_formattable(&index.to_string());
                        index += 1;
                        result
                    };
                    let formatted = if let Some(formattable) = formattable {
                        if let Some(custom_format) = custom_format {
                            let result = formattable.custom_format(custom_format)?;
                            if let Some(standard_format) = standard_format {
                                result.standard_format(standard_format.clone())?
                            } else {
                                result
                            }
                        } else {
                            formattable.standard_format(standard_format.clone().unwrap_or_default())?
                        }
                    } else {
                        if !no_key_ok {
                            bail!(
                                "Key {:?} is not found.",
                                key.as_ref().unwrap_or(&index.to_string())
                            );
                        }
                        String::new()
                    };
                    result.push_str(&formatted);
                }
            }
        }
        Ok(result)
    }
}

impl FromStr for Template {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        use State::*;
        let mut state = Literal;
        let mut parts = vec![];
        let mut buffer = String::new();
        let mut format_buffer = String::new();
        let mut custom_format_buffer = String::new();
        for chr in s.chars() {
            state = match (state, chr) {
                (Literal, '{') => MaybeOpen,
                (Literal, '}') => {
                    buffer.push('}');
                    DoubleClose
                }
                (Literal, chr) => {
                    buffer.push(chr);
                    Literal
                }
                (DoubleClose, '}') => Literal,
                (MaybeOpen, '{') => {
                    buffer.push('{');
                    Literal
                }
                (MaybeOpen, '}') => {
                    parts.push(TemplatePart::Literal(mem::take(&mut buffer)));
                    parts.push(TemplatePart::Interpolation { key: None, standard_format: None, custom_format: None });
                    Literal
                }
                (MaybeOpen, ':') => {
                    parts.push(TemplatePart::Literal(mem::take(&mut buffer)));
                    Format
                }
                (MaybeOpen, '|') => {
                    parts.push(TemplatePart::Literal(mem::take(&mut buffer)));
                    CustomFormat
                }
                (MaybeOpen, chr) if chr != '}' && chr != ':' => {
                    parts.push(TemplatePart::Literal(mem::take(&mut buffer)));
                    buffer.push(chr);
                    Key
                }
                (Key, chr) if chr != '}' && chr != ':' && chr != '|' => {
                    buffer.push(chr);
                    Key
                }
                (Key, ':') => Format,
                (Key, '|') => CustomFormat,
                (Key | Format | CustomFormat, '}') => {
                    let taken = mem::take(&mut format_buffer);
                    let format = if taken.is_empty() {
                        None
                    } else {
                        Some(parse_standard_format(&taken)
                            .ok_or_else(|| anyhow!("Failed to parse as standard format: {taken}"))?)
                    };
                    let custom_format = mem::take(&mut custom_format_buffer);
                    let key = mem::take(&mut buffer);
                    parts.push(TemplatePart::Interpolation {
                        key: if key.is_empty() { None } else { Some(key) },
                        standard_format: format,
                        custom_format: if custom_format.is_empty() {
                            None
                        } else {
                            Some(custom_format)
                        },
                    });
                    Literal
                }
                // Format이 온 뒤 CustomFormat이 붙어서 올 수 있음
                // 단, : 뒤에 바로 |가 온 경우 filler일 수도 있기 때문에 CustomFormat으로 넘어가지 않음
                (Format, '|') if !format_buffer.is_empty() => CustomFormat,
                (Format, chr) => {
                    format_buffer.push(chr);
                    Format
                }
                (CustomFormat, chr) => {
                    custom_format_buffer.push(chr);
                    CustomFormat
                }
                (st, chr) => {
                    return Err(TemplateError {
                        next: Some(chr),
                        state: st,
                    }
                    .into());
                }
            };
        }

        match state {
            Literal | DoubleClose => {
                if !buffer.is_empty() {
                    parts.push(TemplatePart::Literal(buffer))
                }
            }
            _ => return Err(TemplateError { state, next: None }.into()),
        }

        Ok(Self { parts })
    }
}

#[derive(Debug)]
pub struct TemplateError {
    state: State,
    next: Option<char>,
}

impl fmt::Display for TemplateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(next) = self.next {
            write!(
                f,
                "TemplateError: unexpected character {:?} in state {:?}",
                next, self.state
            )
        } else {
            write!(
                f,
                "TemplateError: unexpected termination in state {:?}",
                self.state
            )
        }
    }
}

impl std::error::Error for TemplateError {}

#[cfg(test)]
mod test {
    use crate::literal_map;

    use super::*;

    type TemplateMap = HashMap<String, Box<dyn Formattable>>;
    // type TemplateMap<'a> = HashMap<String, &'a dyn Formattable>;

    #[test]
    fn test_template() {
        // Template::from_str("l{t}");
        // Template::from_str("l{t:f}");

        let template = Template::from_str("literal template").unwrap();
        assert_eq!(
            template.format(&TemplateMap::new()).unwrap(),
            "literal template".to_string()
        );

        // let mut map: HashMap<String, Box<dyn Formattable>> = HashMap::new();
        let mapping: TemplateMap = literal_map!(TemplateMap, {
            "hello" => Box::new("world".to_string()),
        });
        let template = Template::from_str("template {hello}").unwrap();
        assert_eq!(
            template.format(&mapping).unwrap(),
            "template world".to_string()
        );

        let mapping: TemplateMap = literal_map!(TemplateMap, {
            "hello" => Box::new("world".to_string()),
            "another" => Box::new("value".to_string()),
            "123" => Box::new(456),
        });
        let template = Template::from_str("template {hello}? {another}, {123}!").unwrap();
        assert_eq!(
            template.format(&mapping).unwrap(),
            "template world? value, 456!".to_string()
        );

        let template = Template::from_str("template {hello}? {unknown}, {123}!").unwrap();
        assert_eq!(
            template.format(&mapping).unwrap(),
            "template world? , 456!".to_string()
        );

        let template = Template::from_str("/{hello}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/world/".to_string());

        let template = Template::from_str("/{hello:}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/world/".to_string());

        let template = Template::from_str("/{hello:<10}/").unwrap();
        assert_eq!(
            template.format(&mapping).unwrap(),
            "/world     /".to_string()
        );

        let template = Template::from_str("/{hello: <10}/").unwrap();
        assert_eq!(
            template.format(&mapping).unwrap(),
            "/world     /".to_string()
        );

        let template = Template::from_str("/{hello:<<10}/").unwrap();
        assert_eq!(
            template.format(&mapping).unwrap(),
            "/world<<<<</".to_string()
        );

        let template = Template::from_str("/{hello:<!2}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/wo/".to_string());

        let template = Template::from_str("/{hello:>!2}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/ld/".to_string());

        let template = Template::from_str("/{hello:^!3}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/orl/".to_string());

        let template = Template::from_str("/{hello:^3}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/world/".to_string());

        let template = Template::from_str("/{hello:^3}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/world/".to_string());

        let template = Template::from_str("/{123:05d}/").unwrap();
        assert_eq!(template.format(&mapping).unwrap(), "/00456/".to_string());

        let ref_mapping = literal_map!({
            "hello" => mapping["hello"].as_ref(),
            "another" => mapping["another"].as_ref(),
            "123" => mapping["123"].as_ref(),
        });

        let template = Template::from_str("template {hello}? {unknown}, {123}!").unwrap();
        assert_eq!(
            template.format(&ref_mapping).unwrap(),
            "template world? , 456!".to_string()
        );

        let keys: [Box<dyn Formattable>; 3] = [
            Box::new("world".to_string()),
            Box::new("value".to_string()),
            Box::new(456),
        ];

        let template = Template::from_str("/{0} {1} {2}/").unwrap();
        assert_eq!(template.format(&&keys[..]).unwrap(), "/world value 456/");

        let template = Template::from_str("/{1} {0} {2}/").unwrap();
        assert_eq!(template.format(&&keys[..]).unwrap(), "/value world 456/");

        let template = Template::from_str("/{1} {0} {2:05d}/").unwrap();
        assert_eq!(template.format(&&keys[..]).unwrap(), "/value world 00456/");

        let template = Template::from_str("/{1} {0} {2} {1} {2:05d}/").unwrap();
        assert_eq!(
            template.format(&&keys[..]).unwrap(),
            "/value world 456 value 00456/"
        );

        let keys: Vec<Box<dyn Formattable>> = vec![
            Box::new("world".to_string()),
            Box::new("value".to_string()),
            Box::new(456),
        ];

        let template = Template::from_str("/{1} {0} {2} {1} {2:05d}/").unwrap();
        assert_eq!(
            template.format(&keys).unwrap(),
            "/value world 456 value 00456/"
        );

        let keys: Vec<&dyn Formattable> =
            vec![keys[0].as_ref(), keys[1].as_ref(), keys[2].as_ref()];

        let template = Template::from_str("/{1} {0} {2} {1} {2:05d}/").unwrap();
        assert_eq!(
            template.format(&keys).unwrap(),
            "/value world 456 value 00456/"
        );

        let template = Template::from_str("hello {} num {} {:04}").unwrap();
        assert_eq!(
            template.format(&keys).unwrap(),
            "hello world num value 0456"
        );
    }

    #[test]
    fn test_template_escape() {
        let template = Template::from_str("{{}}").unwrap();
        let empty_map: HashMap<String, &dyn Formattable> = HashMap::new();
        assert_eq!(template.format(&empty_map).unwrap(), "{}");
    }

    #[test]
    fn test_template_invalid() {
        assert!(Template::from_str("{").is_err());
        assert!(Template::from_str("{hello").is_err());
        assert!(Template::from_str("{hello:").is_err());
        assert!(Template::from_str("{hello:world").is_err());
        assert!(Template::from_str("}a").is_err());
        assert!(Template::from_str("a}b").is_err());
    }

    #[test]
    fn test_template_format() {
        let template: Template = "Hello, {name}! You are {age} years old.".parse().unwrap();
        let mut map: HashMap<String, Box<dyn Formattable>> = HashMap::new();
        map.insert("name".to_string(), Box::new("Alice".to_string()));
        map.insert("age".to_string(), Box::new(20));

        assert_eq!(template.format(&map).unwrap(), "Hello, Alice! You are 20 years old.");
    }

    #[test]
    fn test_template_format_with_options() {
        let template: Template = "Value: {val:05}".parse().unwrap();
        let mut map: HashMap<String, Box<dyn Formattable>> = HashMap::new();
        map.insert("val".to_string(), Box::new(42));

        assert_eq!(template.format(&map).unwrap(), "Value: 00042");
    }
}
